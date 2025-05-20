#ifndef MINE_TOKENDEF_H_
#define MINE_TOKENDEF_H_

#include <string>
// Token types
enum TokenType {
    IDENFR, INTCON, STRCON, MAINTK, CONSTTK, INTTK, BREAKTK, CONTINUETK, RETURNTK,
    IFTK, ELSETK, NOT, AND, OR, WHILETK, GETINTTK, PRINTFTK,
    PLUS, MINU, MULT, DIV, MOD, LSS, LEQ, GRE, GEQ, EQL, NEQ,
    ASSIGN, SEMICN, COMMA, LPARENT, RPARENT, LBRACK, RBRACK, LBRACE, RBRACE, VOIDTK,
    TOKEN_EOF // Added for clarity with EOF
};

// Token structure
struct Token {
    TokenType type;
    std::string lexeme;
    int lineNumber = 0;

    // Default constructor (optional, compiler would generate one if no others were defined)
    Token() : type(TOKEN_EOF), lexeme(""), lineNumber(0) {}

    // Constructor for initialization list used in lookahead
    Token(TokenType t, std::string s, int l) : type(t), lexeme(std::move(s)), lineNumber(l) {}
};

#endif // MINE_TOKENDEF_H_ 