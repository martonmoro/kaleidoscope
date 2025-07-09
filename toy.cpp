#include <string>
#include <vector>
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