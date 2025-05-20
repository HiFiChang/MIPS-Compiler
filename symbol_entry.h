#ifndef MINE_SYMBOL_ENTRY_H_
#define MINE_SYMBOL_ENTRY_H_

#include <string>
#include <vector>
#include <map> // For Symbol::params

// Symbol structure for Symbol Table
struct Symbol {
    std::string name;
    std::string type; // "int", "const_int", "void_func", "int_func", "string_literal", "array_int", "array_const_int"
    int scopeLevel;
    int value; // For int constants
    std::string strValue; // For string literals (actual content, MIPS label is 'name')
    bool isConstant;
    bool isInitialized;
    bool isGlobal;
    int offset; // Memory offset (from $fp for locals/params, or conceptual for globals)
    bool isArray;
    std::vector<int> arrayDimensions; // Sizes of dimensions for arrays

    // For functions:
    std::string returnType; // "int" or "void"
    std::vector<std::pair<std::string, std::string>> params; // list of (param_name, param_type_string)
    bool isParam; // Added to track if the symbol is a function parameter

    Symbol() : value(0), offset(0), isConstant(false), isGlobal(false), scopeLevel(0), 
               isArray(false), isInitialized(false), isParam(false) {} // Constructor declaration
};

#endif // MINE_SYMBOL_ENTRY_H_ 