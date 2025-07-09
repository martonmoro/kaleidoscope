// ======== LEXER ========
// The lexer returns token [0-255] if it is an unknown character, otherwise one of these for known things
#include <string>
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