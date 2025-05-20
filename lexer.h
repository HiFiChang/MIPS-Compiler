#ifndef MINE_LEXER_H_
#define MINE_LEXER_H_

#include "tokendef.h"
#include <iostream> 
#include <fstream>
#include <string>
#include <vector>
#include <map>
#include <cctype> 

class Lexer {
private:
    std::ifstream inputFile;
    char currentChar;
    int lineNumber = 1;
    std::vector<Token> tokens; 

    std::map<std::string, TokenType> keywords;
    std::map<TokenType, std::string> tokenNames;

public:
    Lexer(const std::string& inputFileName, const std::string& outputFileNameIgnored);
    ~Lexer();
    void getNextChar();
    void skipWhitespace();
    void skipComments();
    Token getNextTokenInternal();
    Token getNextToken(); 
    void processFile(); 
    const std::vector<Token>& getTokens() const;
    const std::map<TokenType, std::string>& getTokenNames() const;
    void semanticError(const std::string& message, int line); 
};

#endif // MINE_LEXER_H_ 