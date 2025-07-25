// #include "../include/KaleidoscopeJIT.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/PassManager.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/StandardInstrumentations.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Transforms/InstCombine/InstCombine.h"
#include "llvm/Transforms/Scalar.h"
#include "llvm/Transforms/Scalar/GVN.h"
#include "llvm/Transforms/Scalar/Reassociate.h"
#include "llvm/Transforms/Scalar/SimplifyCFG.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <vector>

using namespace llvm;
// using namespace llvm::orc;
// ======== LEXER ========
// The lexer returns token [0-255] if it is an unknown character, otherwise one of these for known things
enum Token
{
    tok_eof = -1,

    // commands
    tok_def = -2,
    tok_extern = -3,

    // primary
    tok_identifier = -4,
    tok_number = -5,
};

static std::string IdentifierStr; // Filled in if tok_identifier
static double NumVal;             // Filled in if tok_number

// gettok - Return the next token from standard input
static int gettok()
{
    static int LastChar = ' ';

    // Skip any whitespace
    while (isspace(LastChar))
        LastChar = getchar();

    // recognize identifiers and specific keywords
    // sets `IdentifierStr` global whenever it lexes an identifier
    if (isalpha(LastChar))
    { // identifier: [a-zA-Z][a-zA-Z0-9]*
        IdentifierStr = LastChar;
        while (isalnum((LastChar = getchar())))
            IdentifierStr += LastChar;

        if (IdentifierStr == "def")
            return tok_def;
        if (IdentifierStr == "extern")
            return tok_extern;
        return tok_identifier;
    }

    // note: this incorrectly reads 1.23.45.67 and handle it as 1.23
    if (isdigit(LastChar) || LastChar == '.')
    { // Number: [0-9.]+
        std::string NumStr;
        do
        {
            NumStr += LastChar;
            LastChar = getchar();
        } while (isdigit(LastChar) || LastChar == '.');

        // convert to c style string and then convert to double-precision floating-point number
        NumVal = strtod(NumStr.c_str(), 0);
        return tok_number;
    }

    // handle comments
    // skipping to the end of the line and return the next token
    if (LastChar == '#')
    {
        // Comment until end of line
        do
            LastChar = getchar();
        while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

        if (LastChar != EOF)
            return gettok();
    }

    // if the input doesn't match one of the above cases, it is either an operator character or the end of the file
    if (LastChar == EOF)
        return tok_eof;

    // return character as its ascii value
    int ThisChar = LastChar;
    LastChar = getchar();
    return ThisChar;
}

// ======== AST ========

// The codegen() method says to emit IR for that AST node along with all the things it depends on, and they all return an LLVM Value object.
// "Value" is the class used to represent a Static Single Assignment (SSA) register or SSA value in LLVM.
// The most distinct aspect of SSA values is that their value is computed as the related instruction executes, and it does not get a new
// value until (and if) the instruction re-executes (i.e. there is no way to "change" and SSA value).

// Note: instead of adding virtual methods to the ExprAST class hierarchy, it could also make sense to use a visitor pattern or some other
// way to model this.

// ExprAST - Base class for all expression nodes
class ExprAST
{
public:
    virtual ~ExprAST() = default;
    virtual Value *codegen() = 0;
};

// NumberExprAST - Experssion class for numberic literals like "1.0"
// NumberExprAST class captures the numeric value of the literal as an instance variable
class NumberExprAST : public ExprAST
{
    double Val;

public:
    NumberExprAST(double Val) : Val(Val) {}
    Value *codegen() override;
};

// VariableExprAST - Expression class for referencing a variable, like "a"
class VariableExprAST : public ExprAST
{
    std::string Name;

public:
    VariableExprAST(const std::string &Name) : Name(Name) {}
    Value *codegen() override;
};

// BinaryExprAST - Expression class for a binary operator
class BinaryExprAST : public ExprAST
{
    char Op;
    std::unique_ptr<ExprAST> LHS, RHS;

public:
    BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RHS) : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
    Value *codegen() override;
};

// ClassExprAST - Expression class for function calls
class CallExprAST : public ExprAST
{
    std::string Callee;
    std::vector<std::unique_ptr<ExprAST>> Args;

public:
    CallExprAST(const std::string &Callee,
                std::vector<std::unique_ptr<ExprAST>> Args)
        : Callee(Callee), Args(std::move(Args)) {}
    Value *codegen() override;
};

// PrototypeAST - This class represents the "prototype" for a function,
// which captures its name, and its argument names (this implicitly the number
// of arguments the function takes)
class PrototypeAST
{
    std::string Name;
    std::vector<std::string> Args;

public:
    PrototypeAST(const std::string &Name, std::vector<std::string> Args)
        : Name(Name), Args(std::move(Args)) {}

    Function *codegen();
    const std::string &getName() const { return Name; }
};

// FunctionAST - This class represents a function definition itself
// In Kaleidoscope, functions are typed with just a count of their arguments.
// Since all values are double precision floating point, the type of each argument doesn’t need to be stored anywhere.
// In a more aggressive and realistic language, the “ExprAST” class would probably have a type field.
class FunctionAST
{
    std::unique_ptr<PrototypeAST> Proto;
    std::unique_ptr<ExprAST> Body;

public:
    FunctionAST(std::unique_ptr<PrototypeAST> Proto,
                std::unique_ptr<ExprAST> Body)
        : Proto(std::move(Proto)), Body(std::move(Body)) {}
    Function *codegen();
};

// ======== Parser ========
// Combination of Recursive Descent Parser and Operator-Precedence Parser

// CurTok/getNextToken - Provide a simple token buffer. CurTok is the current
// token the parser is looking at. getNextToken reads another token from the\
// lexer and updates CurTok with its result.
// This implements a simple token buffer around the lexer. This allows us to look one token ahead
// at what the lexer is returning.
// Every function in the parser will assume that CurTok is the current token that needs to be parsed.
static int CurTok;
static int getNextToken()
{
    return CurTok = gettok();
}

// LogError* - Helper functions for error handling.
std::unique_ptr<ExprAST> LogError(const char *Str)
{
    fprintf(stderr, "Error: %s\n", Str);
    return nullptr;
}

std::unique_ptr<PrototypeAST> LogErrorP(const char *Str)
{
    LogError(Str);
    return nullptr;
}

static std::unique_ptr<ExprAST> ParseExpression();

// For eacg production in the grammar, we'll define a function which parses that production.

// For numberic literals:
// this expexts to be called when the current token is a `tok_number` token.
// it takes the current number value, creates a `NumberExprAST` node, advances the lexer to the next token, and finally returns

// numberexpr ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr()
{
    auto Result = std::make_unique<NumberExprAST>(NumVal);
    getNextToken(); // consume the number
    return std::move(Result);
}

//  Note that parentheses do not cause construction of AST nodes themselves.
// While we could do it this way, the most important role of parentheses are to guide the parser and provide grouping.
// Once the parser constructs the AST, parentheses are not needed.
// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr()
{
    getNextToken(); // eat (
    auto V = ParseExpression();
    if (!V)
        return nullptr;

    if (CurTok != ')')
        return LogError("expected ')'");
    getNextToken(); // eat )
    return V;
}

// identifierexpr
//  ::= identifier
//  ::= identifier '(' expression* ')'
static std::unique_ptr<ExprAST> ParseIdentifierExpr()
{
    std::string IdName = IdentifierStr;

    getNextToken(); // eat identifier

    // using look-ahead to determine if the current identifier is a stand alone variable reference or if it is a function call expr
    if (CurTok != '(') // Simple variable ref.
        return std::make_unique<VariableExprAST>(IdName);

    // Call
    getNextToken(); // eat (
    std::vector<std::unique_ptr<ExprAST>> Args;
    if (CurTok != ')')
    {
        while (true)
        {
            if (auto Arg = ParseExpression())
                Args.push_back(std::move(Arg));
            else
                return nullptr;

            if (CurTok == ')')
                break;

            if (CurTok != ',')
                return LogError("Expected ')' or ',' in argument list");
            getNextToken();
        }
    }

    // Eat the ')'
    getNextToken();

    return std::make_unique<CallExprAST>(IdName, std::move(Args));
}

/// primary
///   ::= identifierexpr
///   ::= numberexpr
///   ::= parenexpr
static std::unique_ptr<ExprAST> ParsePrimary()
{
    switch (CurTok)
    {
    default:
        return LogError("unknown token when expecting an expression");
    case tok_identifier:
        return ParseIdentifierExpr();
    case tok_number:
        return ParseNumberExpr();
    case '(':
        return ParseParenExpr();
    }
}

// Binary Expression Parsing - Operator-Precedence Parsing

// BinopPrecedence - This holds the precedence for each binary operator that is defined
static std::map<char, int> BinopPrecedence;

// GetTokPrecedence - Get the precedence of the pending binary operator token
static int GetTokPrecedence()
{
    if (!isascii(CurTok))
        return -1;

    // make sure it's a declared binop
    int TokPrec = BinopPrecedence[CurTok];
    if (TokPrec <= 0)
        return -1;
    return TokPrec;
}

// `ParseBinOpRHS` is the function that parses the sequence of pairs
// It takes a precedence and a pointer to an expression for the part that has been parsed so far.
// The precedence value passed into `ParseBinOpRHS` indicates the minimal operator precedence that the function is allowed to eat.
// For example, if the current pair stream is [+, x] and `ParseBinOpRHS` is passed in a precedence of 40, it will not consume any tokens because the precedence of '+' is only 20

// binoprhs
//   ::= ('+' primary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS)
{

    // this code gets the precedence of the current token and checks to see if it is too low.
    // Because we defined invalid tokens to have a precedence of -1, this check implicitly knows that the
    // pair-stream ends when the token stream runs out of binary operators.
    // If this check succeeds, we know that the token is a binary operator and that it will be included in this expression

    // if this is a binop, find its precedence
    while (true)
    {
        int TokPrec = GetTokPrecedence();

        // if this is a binop that binds at least as tightly as the current binop,
        // consume it, otherwise we are done.
        if (TokPrec < ExprPrec)
            return LHS;

        // Okay, we know this is a binop.
        int BinOp = CurTok;
        getNextToken(); // eat binop

        // Parse the primary expression after the binary operator.
        auto RHS = ParsePrimary();
        if (!RHS)
            return nullptr;

        // If Binop binds less tightly with RHS than the operator after RHS, let
        // the pending operator take RHS as its LHS.
        // If the precedence of the binop to the right of "RHS" is lower or equal to the precedence of our current operator,
        // then we know that the parantheses associate as “(a+b) binop …”.
        int NextPrec = GetTokPrecedence();
        if (TokPrec < NextPrec)
        {
            RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
            if (!RHS)
                return nullptr;
        }

        // Merge LHS/RHS
        LHS = std::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
    }
}

// an expression is a primary expression potentially followed by a sequence of [binop,primaryexpr] pairs:
// expression
//   ::= primary binoprhs

static std::unique_ptr<ExprAST> ParseExpression()
{
    auto LHS = ParsePrimary();
    if (!LHS)
        return nullptr;

    return ParseBinOpRHS(0, std::move(LHS));
}

// prototype
//   ::= id '(' id* ')'
static std::unique_ptr<PrototypeAST> ParsePrototype()
{
    if (CurTok != tok_identifier)
        return LogErrorP("Expected function name in prototype");

    std::string FnName = IdentifierStr;
    getNextToken();

    if (CurTok != '(')
        return LogErrorP("Expected '(' in prototype");

    // Read the list of argument names.
    std::vector<std::string> ArgNames;
    while (getNextToken() == tok_identifier)
        ArgNames.push_back(IdentifierStr);
    if (CurTok != ')')
        return LogErrorP("Expected ')' in prototype");

    // success
    getNextToken(); // eat ')'

    return std::make_unique<PrototypeAST>(FnName, std::move(ArgNames));
}

// definition ::= 'def' prototype expression
static std::unique_ptr<FunctionAST> ParseDefinition()
{
    getNextToken(); // eat def
    auto Proto = ParsePrototype();
    if (!Proto)
        return nullptr;

    if (auto E = ParseExpression())
        return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    return nullptr;
}

// external ::= 'extern' prototype
static std::unique_ptr<PrototypeAST> ParseExtern()
{
    getNextToken(); // eat extern
    return ParsePrototype();
}

// we’ll also let the user type in arbitrary top-level expressions and evaluate them on the fly.
// We will handle this by defining anonymous nullary (zero argument) functions for them
// toplevelexpr ::= expression
static std::unique_ptr<FunctionAST> ParseTopLevelExpr()
{
    if (auto E = ParseExpression())
    {
        // make an anonymous proto
        auto Proto = std::make_unique<PrototypeAST>("", std::vector<std::string>());
        return std::make_unique<FunctionAST>(std::move(Proto), std::move(E));
    }
    return nullptr;
}

// ======== CODE GENERATION ========

// The static variables will be used during code generation.

// `TheContext` is an opaque object that owns a lot of core LLVM data structures, such as the type and constant value tables.
// LLVM uses contexts to isulate different parts of the compiler (e.g., if we were compiling multiple modules in parallel,
// each would need its own context)
// It ensures that constants and types are unique within a given context
static std::unique_ptr<LLVMContext> TheContext;
// The `Builder` object is a helper object that makes it easy to generate LLVM instructions. Instances of the `IRBuilder` class template
// keep track of the current place to insert instructions and has methods to create new instructions.
// It keeps track of the current insertion point (where the next instruction will go in a function's basic book)
// Provides methods like `CreateAdd()`, `CreateLoad()`, etc., to generate IR instructions without manually constructing them
static std::unique_ptr<IRBuilder<>> Builder;
// `TheModule` is an LLVM construct that contains functions and global variables. In many ways, it is the top-level structure that the
// LLVM IR uses to contain code. It will own the memory for all of the IR that we generate, which is why the `codegen()` method returns
// a raw `Value*`, rather than a `unique_ptr<Value>`
// All the IR we generate lives in a module.
static std::unique_ptr<Module> TheModule;
// The `NamedValues` map keeps track of which values are defined in the current scope and what their LLVM representation is. (In other
// words, it is a symbol table for the code). In this form of Kaleidoscope, the only things that can be referenced are function parameters.
// As such, function parameters will be in this map when generating code for their function body.
// Used to look up values when generating IR for variable references.

// Example:
// When parsing a function like def foo(x), the compiler:
//  Creates a function in TheModule.
//  Uses Builder to generate IR for the function body.
//  Stores the parameter x in NamedValues so it can be referenced later.

// When parsing an expression like x + 1, the compiler:
//  Looks up x in NamedValues.
//  Uses Builder to create an add instruction.

// The resulting IR is part of TheModule.
static std::map<std::string, Value *> NamedValues;

// static std::unique_ptr<KaleidoscopeJIT> TheJIT;
static std::unique_ptr<FunctionPassManager> TheFPM;
static std::unique_ptr<LoopAnalysisManager> TheLAM;
static std::unique_ptr<FunctionAnalysisManager> TheFAM;
static std::unique_ptr<CGSCCAnalysisManager> TheCGAM;
static std::unique_ptr<ModuleAnalysisManager> TheMAM;
static std::unique_ptr<PassInstrumentationCallbacks> ThePIC;
static std::unique_ptr<StandardInstrumentations> TheSI;
static std::map<std::string, std::unique_ptr<PrototypeAST>> FunctionProtos;
static ExitOnError ExitOnErr;

Value *LogErrorV(const char *Str)
{
    LogError(Str);
    return nullptr;
}

// In the LLVM IR, numeric constants are represented with the `ConstantFP` class, which holds the numeric value in an `APFloat` internally
// (`APFloat has the capability of holding floating point constant of Arbitrary Precision).
// This code basically just creates and returns a `ConstantFP`.
// Note that in the LLVM IR that constants are all uniqued together and shared. For this reason, the API uses the `foo::get(...)` idiom
// instead of the `new foo(...)` or `foo::Create(...)`
// Generatic LLVM code for numberic literals
Value *NumberExprAST::codegen()
{
    return ConstantFP::get(*TheContext, APFloat(Val));
}

// In the simple version of Kaleidoscope, we assume that the variable has already been emitted somewhere and its value is available.
// In practice the only values that can be in the `NamedValues` map are function arguments. This code simply checks to see that the
// specified name is in the map and returns a value for it.
Value *VariableExprAST::codegen()
{
    // Look this variable up in the function
    Value *V = NamedValues[Name];
    if (!V)
        LogErrorV("Unknown variable name");
    return V;
}

// We recursively emit code the the left-hand side of the experssion, then the right-hand side, then we compute the result of the binary expression.
// `IRBuilder` knows where to insert a newly created instruction, all we have to do is specify what instruction to create (e.g. with `CreateFAdd`),
// which operands to use (L and R here) and optionally provide a name for the generated instruction
//
// One nice thing about LLVM is that the name is just a hint. For instance, if the code above emits multiple "addtmp" variables, LLVM will automatically provide each one with an increasing,
// unique numeric suffix. Local value names for instructions are purely optional, but it makes it much easier to read the IR dumps.
//
// LLVM instructions are constrained by strict rules: for example, the Left and Right operands of an add instruction must have the same type, and the result type of the add must match
// the operand types.
//
// On the other hand, LLVM specifies that the fcmp instruction always returns an 'i1' value (one bit integer). The problem with this is that Kaleidoscope wants the value to be
// a 0.0 or 1.0 value. In order to get these semantics, we combine the fcmp instruction with a uitofp instruction. This instruction converts its input integer into a floating point
// value. In contracst, if we used the sitofp instrcution, the Kaleidoscope `<` operator would return 0.0 and -1.0, depending on the input value.
Value *BinaryExprAST::codegen()
{
    Value *L = LHS->codegen();
    Value *R = RHS->codegen();
    if (!L || !R)
        return nullptr;

    switch (Op)
    {
    case '+':
        return Builder->CreateFAdd(L, R, "addtmp");
    case '-':
        return Builder->CreateFSub(L, R, "subtmp");
    case '*':
        return Builder->CreateFMul(L, R, "multmp");
    case '<':
        L = Builder->CreateFCmpULT(L, R, "cmptmp");
        // Convert bool 0/1 to double 0.0 or 1.0
        return Builder->CreateUIToFP(L, Type::getDoubleTy(*TheContext), "booltmp");
    default:
        return LogErrorV("invalid binary operator");
    }
}

// This code initially does a function name lookup in the LLVM Module's symbol table. Recall that the LLVM Module is the container that holds the functions we are JIT'ing.
// By giving each function the same name as what the user specifies, we can use the LLVM sumbol table to resolve function names for us.
//
// Once we have the function call, we recursively codegen each argument that is to be passed in, and create an LLVM call instruction. Note that LLVM uses the native
// C calling conventions by default, allowing these calls to also call into standard library functions like "sin" and "cos", with no additional effort.
// This wraps up our handling of the four basic expressions that we have so fair in Kaleidoscope.
Value *CallExprAST::codegen()
{
    // Look up the name in the global module table
    Function *CalleeF = TheModule->getFunction(Callee);
    if (!CalleeF)
        return LogErrorV("Unknown function referenced");

    // If argument mismatch error
    if (CalleeF->arg_size() != Args.size())
        return LogErrorV("Incorrect # arguments passed");

    std::vector<Value *> ArgsV;
    for (unsigned i = 0, e = Args.size(); i != e; ++i)
    {
        ArgsV.push_back(Args[i]->codegen());
        if (!ArgsV.back())
            return nullptr;
    }

    return Builder->CreateCall(CalleeF, ArgsV, "calltmp");
}

// This function returns a "Function*" instead of a "Value*". Beause a "prototype" really talks about the external
// interface for a function and not the value computed by an expression.
//
// The call to `Function::get` creates a `FunctionType` that should be used for a given Prototype. Since all function
// arguments in Kaleidoscope are of type double, the first line creates a vector of "N" LLVM double types. It then
// uses the `FunctionType::get` method to create a function type that takes "N" doubles as arguments, returns one double
// as a result, and that is not vararg (the false parameter indicates this).
// Note that Types in LLBM are uniqued just like Constants are, so you don't "new" a type, you "get" it.
//
// The final line creates the IR Function corresponding to the Prototype. This indicates the type, linkage and name to use,
// as well as which module to insert into. "external linkage" means that the function may be defined outside the current
// module and/or that it is callable by functions outside the module. The Name passed in is the name the user specified:
// since `TheModule` is specified, this name is registered in the `TheModule`'s symbol table.
//
// Finally, we set the name of each of the function's arguments according to the names given in the Prototype. This step
// isn't strictly necessary, but keeping the names consistent makes the IR more readable, and allows subsequent code to
// refer directly to the arguments for their names, rather than having to look them up in the Prototype AST.
Function *PrototypeAST::codegen()
{
    // Make the function type: double(double,double) etc.
    std::vector<Type *> Doubles(Args.size(), Type::getDoubleTy(*TheContext));

    FunctionType *FT = FunctionType::get(Type::getDoubleTy(*TheContext), Doubles, false);

    Function *F = Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());

    unsigned Idx = 0;
    for (auto &Arg : F->args())
        Arg.setName(Args[Idx++]);

    return F;
}

// For function definitions, we start by searching `TheModule`'s symbol table for an existing version of this function,
// in case one has already been created using an 'extern' statement. If `Module::getFunction` returns null then no
// previous version exists, so we'll codegen one from the Prototype. In either case, we want to assert that the function
// is empty (i.e. has no body yet) before we start.
//
// Builder setup: The first line creates a new basic block (named "entry"), which is inserted into `TheFunction`.
// The second line then tells the builder that new instructions should be inserted into the end of the new basic block.
// Basic blocks in LLVM are an important part of functions that define the Control Flow Graph.
//
// Next we add the function arguments to the `NamedValues` map so that they are accessible to `VariableExprAST` nodes.
//
// Once the insertion point has been set up and the `NamedValues` map populated, we call the `codegen()` method for the
// root expression of the function. If no error happens, this emits code to compute the expression into the entry block
// and returns the value that was computed. Assuming no error, we then create an LLVM ret instruction, which completes
// the function. Once the function is built, we call `verifyFunction`, which is provided by LLVM. This function does a
// variety of consistency checks on the generated code, to determine if our compiler is doing everything right. Using
// this is important.
//
// We handle the error case by merely deleting the function we produced with the `eraseFromParent` method. This allows
// the user to redefine a function that they incorrectly typed in before.
Function *FunctionAST::codegen()
{
    // First, check for an existing function from a previous 'extern' declaration
    Function *TheFunction = TheModule->getFunction(Proto->getName());

    if (!TheFunction)
        TheFunction = Proto->codegen();

    if (!TheFunction)
        return nullptr;

    if (!TheFunction->empty())
        return (Function *)LogErrorV("Function cannot be redefined.");

    // Create a new basic block to start insertion into.
    BasicBlock *BB = BasicBlock::Create(*TheContext, "entry", TheFunction);
    Builder->SetInsertPoint(BB);

    // Record the function arguments in the `NamedValues` map.
    NamedValues.clear();
    for (auto &Arg : TheFunction->args())
        NamedValues[std::string(Arg.getName())] = &Arg;

    if (Value *RetVal = Body->codegen())
    {
        // Finish off the function
        Builder->CreateRet(RetVal);

        // Validate the generated code, checking for consistency
        verifyFunction(*TheFunction);

        // Optimize the function
        TheFPM->run(*TheFunction, *TheFAM);

        return TheFunction;
    }

    // Error reading body, remove function
    TheFunction->eraseFromParent();
    return nullptr;
}

// ======== TOP-LEVEL PARSING AND JIT ========

static void InitializeModule()
{
    // Open a new context and module
    TheContext = std::make_unique<LLVMContext>();
    TheModule = std::make_unique<Module>("my cool jit", *TheContext);
    // TheModule->setDataLayout(TheJIT->getDataLAyout());

    // Create a new builder for the module
    Builder = std::make_unique<IRBuilder<>>(*TheContext);

    // Create new pass and analysis managers
    // The four AnalysisManagers allow us to add analysis passes that run accross the four levels of the IR hierarchy
    // PassInstrumentationCallbacks and StandardInstrumentations are required for the pass instrumentation framework,
    // which allows developers to customize what happens between passes.
    TheFPM = std::make_unique<FunctionPassManager>();
    TheLAM = std::make_unique<LoopAnalysisManager>();
    TheFAM = std::make_unique<FunctionAnalysisManager>();
    TheCGAM = std::make_unique<CGSCCAnalysisManager>();
    TheMAM = std::make_unique<ModuleAnalysisManager>();
    ThePIC = std::make_unique<PassInstrumentationCallbacks>();
    TheSI = std::make_unique<StandardInstrumentations>(*TheContext,
                                                       /*DebugLogging*/ true);
    TheSI->registerCallbacks(*ThePIC, TheMAM.get());

    // Add transform passes
    // Do simple "peephole" optimizations and bit-twiddling optimizations (x*2->x<<1, x+0->x, etc.)
    TheFPM->addPass(InstCombinePass());
    // Reassociate expressions
    TheFPM->addPass(ReassociatePass());
    // Eliminate Common Subexpressions (Global Value Numbering)
    TheFPM->addPass(GVNPass());
    // Simplify the control flow graph (deleting unreachable blocks, etc.)
    TheFPM->addPass(SimplifyCFGPass());

    // Register analysis passes used in these transform passes
    PassBuilder PB;
    PB.registerModuleAnalyses(*TheMAM);
    PB.registerFunctionAnalyses(*TheFAM);
    PB.crossRegisterProxies(*TheLAM, *TheFAM, *TheCGAM, *TheMAM);
}

static void HandleDefinition()
{
    // Calls the parser to convert the input token into an AST
    if (auto FnAST = ParseDefinition())
    {
        // Call the code generation method on the parsed AST
        if (auto *FnIR = FnAST->codegen())
        {
            fprintf(stderr, "Read function definition:\n");
            // Print generated LLVM IR (errs() is LLVM's error output stream)
            FnIR->print(errs());
            fprintf(stderr, "\n");
        }
    }
    else
    {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleExtern()
{
    if (auto ProtoAST = ParseExtern())
    {
        if (auto *FnIR = ProtoAST->codegen())
        {
            fprintf(stderr, "Read extern:\n");
            FnIR->print(errs());
            fprintf(stderr, "\n");
        }
    }
    else
    {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleTopLevelExpression()
{
    // Evaluate a top-level expression into an anonymous function.
    if (auto FnAST = ParseTopLevelExpr())
    {
        if (auto *FnIR = FnAST->codegen())
        {
            fprintf(stderr, "Read top-level expression:\n");
            FnIR->print(errs());
            fprintf(stderr, "\n");

            // Remove the anonymous expression.
            FnIR->eraseFromParent();
        }
    }
    else
    {
        // Skip token for error recovery.
        getNextToken();
    }
}

// top ::= definition | external | expression | ';'
static void MainLoop()
{
    while (true)
    {
        fprintf(stderr, "ready> ");
        switch (CurTok)
        {
        case tok_eof:
            return;
        case ';': // ignore top-level semicolons
            getNextToken();
            break;
        case tok_def:
            HandleDefinition();
            break;
        case tok_extern:
            HandleExtern();
            break;
        default:
            HandleTopLevelExpression();
            break;
        }
    }
}

// ======== MAIN DRIVER CODE ========

int main()
{
    // Install standard binary operators.
    // 1 is lowest precedence.
    BinopPrecedence['<'] = 10;
    BinopPrecedence['+'] = 20;
    BinopPrecedence['-'] = 20;
    BinopPrecedence['*'] = 40; // highest.

    // Prime the first token.
    fprintf(stderr, "ready> ");
    getNextToken();

    // Make the module, which holds all the code.
    InitializeModule();

    // Run the main "interpreter loop" now.
    MainLoop();

    // Print out all of the generated code.
    TheModule->print(errs(), nullptr);

    return 0;
}