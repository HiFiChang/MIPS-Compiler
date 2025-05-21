#ifndef MINE_SYMBOLTABLE_H_
#define MINE_SYMBOLTABLE_H_

#include "symbol_entry.h" 
#include <vector>
#include <map>
#include <string>

class SymbolTable {
public:
    std::vector<std::map<std::string, Symbol>> tableStack;
    std::map<std::string, Symbol> globalLiterals; 
    std::map<std::string, int> functionScopeLevels;
    std::map<std::string, std::vector<Symbol>> functionLocalSymbols;
    std::string activeFunctionName;
    std::vector<Symbol> activeFunctionSymbolsList;
    int currentScopeLevel;
    int currentGlobalDataOffset; 
    int currentFrameOffset;      
    int currentParamOffset;    // For tracking parameter offsets

    SymbolTable();
    void enterScope();
    void exitScope();
    bool addSymbol(Symbol& symbol); 
    bool addParamSymbol(Symbol& paramSymbol, int paramIndex);
    Symbol* lookupSymbol(const std::string& name, bool globalOnly = false, const std::string& functionScopeName = "");
    std::string addStringLiteral(const std::string& strValWithQuotes);
    void resetFrameOffsetForNewFunction();
    int getCurrentFrameSizeForLocals() const; 
    void beginFunctionCompilation(const std::string& funcName);
    void endFunctionCompilation(const std::string& funcName);
};

#endif // MINE_SYMBOLTABLE_H_ 