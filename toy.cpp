#include <string>
#include <vector>
#include <map>
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
// ExprAST - Base class for all expression nodes
class ExprAST
{
public:
    virtual ~ExprAST() = default;
};

// NumberExprAST - Experssion class for numberic literals like "1.0"
// NumberExprAST class captures the numeric value of the literal as an instance variable
class NumberExprAST : public ExprAST
{
    double Val;

public:
    NumberExprAST(double Val) : Val(Val) {}
};

// VariableExprAST - Expression class for referencing a variable, like "a"
class VariableExprAST : public ExprAST
{
    std::string Name;

public:
    VariableExprAST(const std::string &Name) : Name(Name) {}
};

// BinaryExprAST - Expression class for a binary operator
class BinaryExprAST : public ExprAST
{
    char Op;
    std::unique_ptr<ExprAST> LHS, RHS;

public:
    BinaryExprAST(char Op, std::unique_ptr<ExprAST> LHS, std::unique_ptr<ExprAST> RIHS) : Op(Op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
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
static std::map<char, int> BinopPrecendence;

// GetTokPrecedence - Get the precedence of the pending binary operator token
static int GetTokPrecedence()
{
    if (!isascii(CurTok))
        return -1;

    // make sure it's a declared binop
    int TokPrec = BinopPrecendence[CurTok];
    if (TokPrec <= 0)
        return -1;
    return TokPrec;
}

// int main
// {
//     // Install standard binary operators.
//     // 1 is lowest precedence.
//     BinopPrecedence['<'] = 10;
//     BinopPrecedence['+'] = 20;
//     BinopPrecedence['-'] = 20;
//     BinopPrecedence['*'] = 40; // highest.
// }

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