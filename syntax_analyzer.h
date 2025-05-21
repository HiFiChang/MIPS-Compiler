#ifndef MINE_SYNTAX_ANALYZER_H_
#define MINE_SYNTAX_ANALYZER_H_

#include "lexer.h"        
#include "symbol_table.h"  
#include "ir_generator.h" 

#include <vector>
#include <string>
#include <map>
#include <fstream> 
#include <iostream>
#include <algorithm>
#include <stack>
#include <utility>

class SyntaxAnalyzer {
private:
    Lexer& lexer; 
    std::vector<std::pair<std::string, int>> syntaxOutputsForFile; 
    std::string syntaxOutputFileNameInternal; 
    std::ofstream syntaxOutFileInternal;  

    SymbolTable& symTab;
    IRGenerator& irGen;
    
    Token currentToken;
    int tokenIndex = 0; 
    
    Symbol* currentFunctionSym = nullptr; 
    int loopDepth = 0; 
    std::stack<std::pair<std::string, std::string>> loopLabels; 

    std::ofstream mipsOutFile;
    std::string mipsFileName;

    bool enableErrorRecovery = false;
    bool enableSyntaxOutput = true;
    std::ofstream syntaxOutputFile;

    void getNextTokenFromLexer(); 
    void match(TokenType expectedType);
    void recordSyntaxOutput(const std::string& output); 
    void mergeAndWriteSyntaxOutput(); 
    void semanticError(const std::string& message);
    std::string getExpType(const std::string& expResult);
    std::string getMangledNameForIR(Symbol* sym) const;
    bool tryResolveToConstInt(const std::string& operandStr, int& outValue);

public:
    SyntaxAnalyzer(Lexer& lex, SymbolTable& sTab, IRGenerator& irG,
                   const std::string& primaryOutputFileName,
                   bool enableOldSyntaxOutput = false, 
                   const std::string& secondaryOutputFileName = "output.txt");
    ~SyntaxAnalyzer();
    void analyze();
    
    void parseCompUnit();
    void parseDecl();
    void parseConstDecl();
    std::string parseBType();
    void parseConstDef(const std::string& constBaseType);
    std::string parseConstInitVal(Symbol& constSym, bool parsingArrayElement = false);
    void parseVarDecl();
    void parseVarDef(const std::string& varBaseType);
    std::string parseInitVal(Symbol& varSym);
    void parseFuncDef();
    void parseMainFuncDef();
    std::string parseFuncType();
    void parseFuncFParams(Symbol& funcSymbol);
    void parseFuncFParam(Symbol& funcSymbol, int paramIndex);
    void parseBlock(bool isNewScope = true);
    void parseBlockItem();
    void parseStmt();
    std::string parseExp(const std::string& expectedType = "int");
    std::string parseCond();
    std::string parseLVal(bool& isArrayAccess, std::string& arrayIndexTemp, bool isAssignmentLHS = false);
    std::string parsePrimaryExp();
    std::string parseNumber();
    std::string parseUnaryExp();
    std::string parseUnaryOp();
    void parseFuncRParams(Symbol* funcSym);
    std::string parseMulExp(const std::string& expectedType = "int");
    std::string parseAddExp(const std::string& expectedType = "int");
    std::string parseRelExp();
    std::string parseEqExp();
    std::string parseLAndExp();
    std::string parseLOrExp();
    std::string parseConstExp();

};

#endif // MINE_SYNTAX_ANALYZER_H_ 