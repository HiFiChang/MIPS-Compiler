#include "lexer.h"
#include <utility> // For std::move

// Lexer class implementation from lab6.cpp
// ... (Lines 502 to 730 from original lab6.cpp, containing Lexer implementation)
Lexer::Lexer(const std::string& inputFileName, const std::string& outputFileNameIgnored) : keywords({
    {"main", MAINTK}, {"const", CONSTTK}, {"int", INTTK}, {"break", BREAKTK},
    {"continue", CONTINUETK}, {"if", IFTK}, {"else", ELSETK}, {"while", WHILETK},
    {"getint", GETINTTK}, {"printf", PRINTFTK}, {"return", RETURNTK}, {"void", VOIDTK}
}), tokenNames({
    {IDENFR, "IDENFR"}, {INTCON, "INTCON"}, {STRCON, "STRCON"}, {MAINTK, "MAINTK"},
    {CONSTTK, "CONSTTK"}, {INTTK, "INTTK"}, {BREAKTK, "BREAKTK"}, {CONTINUETK, "CONTINUETK"},
    {IFTK, "IFTK"}, {ELSETK, "ELSETK"}, {NOT, "NOT"}, {AND, "AND"}, {OR, "OR"},
    {WHILETK, "WHILETK"}, {GETINTTK, "GETINTTK"}, {PRINTFTK, "PRINTFTK"},
    {RETURNTK, "RETURNTK"}, {PLUS, "PLUS"}, {MINU, "MINU"}, {MULT, "MULT"},
    {DIV, "DIV"}, {MOD, "MOD"}, {LSS, "LSS"}, {LEQ, "LEQ"}, {GRE, "GRE"}, {GEQ, "GEQ"},
    {EQL, "EQL"}, {NEQ, "NEQ"}, {ASSIGN, "ASSIGN"}, {SEMICN, "SEMICN"}, {COMMA, "COMMA"},
    {LPARENT, "LPARENT"}, {RPARENT, "RPARENT"}, {LBRACK, "LBRACK"}, {RBRACK, "RBRACK"},
    {LBRACE, "LBRACE"}, {RBRACE, "RBRACE"}, {VOIDTK, "VOIDTK"}, {TOKEN_EOF, "EOF"}
}) {
    inputFile.open(inputFileName);
    if (!inputFile.is_open()) {
        std::cerr << "Failed to open input file: " << inputFileName << std::endl;
        exit(1);
    }
    getNextChar(); // Initialize by reading first character
}

Lexer::~Lexer() {
    if (inputFile.is_open()) {
        inputFile.close();
    }
}
void Lexer::getNextChar() {
    if (inputFile.get(currentChar)) {
        if (currentChar == '\n') { // Fixed: multi-character constant
            lineNumber++;
        }
    } else {
        currentChar = EOF; // Use char EOF, not TokenType
    }
}
void Lexer::skipWhitespace() {
    while (isspace(currentChar) && currentChar != EOF) {
        getNextChar();
    }
}
void Lexer::skipComments() { // Improved comment skipping
    if (currentChar == '/') {
        char peekChar = inputFile.peek(); // Peek next char without consuming
        if (peekChar == '/') {  // Single-line comment
            // Consume the second '/'
            inputFile.get(currentChar); 
            while (currentChar != '\n' && currentChar != EOF) { // Fixed: multi-character constant
                getNextChar();
            }
            // Consume the newline character as well, if it's not EOF
            if (currentChar == '\n') getNextChar(); // Fixed: multi-character constant
        } else if (peekChar == '*') {  // Multi-line comment
            // Consume the '*'
            inputFile.get(currentChar); 
            getNextChar(); // Move to character after /*
            while (true) {
                if (currentChar == EOF) break; // End of file in comment
                if (currentChar == '*') {
                    getNextChar();
                    if (currentChar == '/') {
                        getNextChar(); // consume the / of */
                        break; // End of multi-line comment
                    }
                    // else it was just a random '*'
                } else {
                    getNextChar();
                }
            }
        }
        // If neither // nor /*, it's a division operator, handled by getNextTokenInternal
    }
}

Token Lexer::getNextTokenInternal() {
    // Skip whitespace and comments first
    while (isspace(currentChar) || currentChar == '/') {
         if (isspace(currentChar)) skipWhitespace();
         // Check for comments again after skipping whitespace
         if (currentChar == '/') {
            char peek = inputFile.peek();
            if (peek == '/' || peek == '*') skipComments();
            else break; // It's a DIV op, not a comment
         }
    }
    
    Token token;
    token.lineNumber = lineNumber;

    if (currentChar == EOF) { // Check for actual EOF character
        token.type = TOKEN_EOF; token.lexeme = "EOF"; return token;
    }

    if (isalpha(currentChar) || currentChar == '_') {
        std::string lexeme;
        while ((isalnum(currentChar) || currentChar == '_') && currentChar != EOF) {
            lexeme += currentChar; getNextChar();
        }
        token.lexeme = lexeme;
        if (keywords.count(lexeme)) token.type = keywords[lexeme];
        else token.type = IDENFR;
        return token;
    }
    if (isdigit(currentChar)) {
        std::string lexeme;
        while (isdigit(currentChar) && currentChar != EOF) {
            lexeme += currentChar; getNextChar();
        }
        token.type = INTCON; token.lexeme = lexeme; return token;
    }
    if (currentChar == '"') { // String constants
        std::string lexeme = ""; // Don't include the first quote in lexeme yet
        // lexeme += currentChar; // If you want quotes in lexeme, uncomment. For addStringLiteral, no quotes.
        getNextChar(); // Consume opening quote

        while (currentChar != '"' && currentChar != EOF /*&& currentChar != '\\n'*/) { // TODO: SysY specific string rules
            // SysY format strings: %d, normal chars, \n. No other escapes.
            // Normal chars are d (ASCII 32-126) excluding ", \.
            // For simplicity, take chars literally until next quote or EOF.
            // The addStringLiteral and MIPS generator need to handle \n correctly.
            lexeme += currentChar;
            getNextChar();
        }

        if (currentChar == '"') {
            // lexeme += currentChar; // If you want quotes in lexeme.
            getNextChar(); // Consume closing quote
        } else {
            // Unterminated string error - parser might catch this or lexer flags it
            std::cerr << "Lexical Error (Line " << lineNumber << "): Unterminated string literal." << std::endl;
        }
        token.type = STRCON; token.lexeme = "\"" + lexeme + "\""; /* Store with quotes to be consistent for addStringLiteral */ return token;
    }

    // Operators and delimiters
    token.lexeme = currentChar; // Default for single char tokens
    switch (currentChar) {
        case '+': token.type = PLUS; getNextChar(); return token;
        case '-': token.type = MINU; getNextChar(); return token;
        case '*': token.type = MULT; getNextChar(); return token;
        case '/': token.type = DIV; getNextChar(); return token;
        case '%': token.type = MOD; getNextChar(); return token;
        case '<': getNextChar(); if (currentChar == '=') { token.type = LEQ; token.lexeme += currentChar; getNextChar(); } else token.type = LSS; return token;
        case '>': getNextChar(); if (currentChar == '=') { token.type = GEQ; token.lexeme += currentChar; getNextChar(); } else token.type = GRE; return token;
        case '=': getNextChar(); if (currentChar == '=') { token.type = EQL; token.lexeme += currentChar; getNextChar(); } else token.type = ASSIGN; return token;
        case '!': getNextChar(); if (currentChar == '=') { token.type = NEQ; token.lexeme += currentChar; getNextChar(); } else token.type = NOT; return token;
        case '&': getNextChar(); if (currentChar == '&') { token.type = AND; token.lexeme += currentChar; getNextChar(); } else { /* Error or single &? SysY requires && */ semanticError("Lexical: Expected '&&', found '&'", token.lineNumber); } return token;
        case '|': getNextChar(); if (currentChar == '|') { token.type = OR; token.lexeme += currentChar; getNextChar(); } else { /* Error or single |? SysY requires || */ semanticError("Lexical: Expected '||', found '|'", token.lineNumber); } return token;
        case ';': token.type = SEMICN; getNextChar(); return token;
        case ',': token.type = COMMA; getNextChar(); return token;
        case '(': token.type = LPARENT; getNextChar(); return token;
        case ')': token.type = RPARENT; getNextChar(); return token;
        case '[': token.type = LBRACK; getNextChar(); return token;
        case ']': token.type = RBRACK; getNextChar(); return token;
        case '{': token.type = LBRACE; getNextChar(); return token;
        case '}': token.type = RBRACE; getNextChar(); return token;
        default:
            std::cerr << "Lexical Error at line " << lineNumber << ": Unrecognized character '" << currentChar << "'" << std::endl;
            getNextChar(); // Try to recover by skipping
            return getNextTokenInternal(); // Recursive call to find next valid token
    }
}
Token Lexer::getNextToken() { // Wrapper that stores tokens if needed, but SyntaxAnalyzer will use its own index
    Token token = getNextTokenInternal();
    // Only add if not EOF or if it's the first EOF.
    // SyntaxAnalyzer manages its own token consumption from a pre-filled vector.
    // So, this internal 'tokens' vector in Lexer might become redundant if SyntaxAnalyzer uses getTokens() once.
    if (token.type != TOKEN_EOF || (tokens.empty() || tokens.back().type != TOKEN_EOF) ) {
         tokens.push_back(token);
    }
    return token;
}
void Lexer::processFile() { // Fills the internal tokens vector
    while (currentChar != EOF) { // char EOF
        // getNextToken(); // This would store it.
        // The original code calls getNextToken which stores. Let's keep that for now.
        // If SyntaxAnalyzer pulls tokens one-by-one, this processFile isn't strictly needed by it.
        // But it's good for pre-lexing everything.
        Token t = getNextTokenInternal(); // Get token
        tokens.push_back(t); // Store it
        if(t.type == TOKEN_EOF) break; // Stop if EOF token generated
    }
     // Ensure there's an EOF token at the end of the list if not already there.
     if (tokens.empty() || tokens.back().type != TOKEN_EOF) {
        Token eofToken;
        eofToken.type = TOKEN_EOF;
        eofToken.lexeme = "EOF";
        eofToken.lineNumber = lineNumber; // Line number of EOF
        tokens.push_back(eofToken);
    }
}
const std::vector<Token>& Lexer::getTokens() const { return tokens; }
const std::map<TokenType, std::string>& Lexer::getTokenNames() const { return tokenNames; }

void Lexer::semanticError(const std::string& message, int line) { // Helper for lexer errors
    std::cerr << "Lexical Error (Line " << line << "): " << message << std::endl;
    exit(1);
} 