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
    std::vector<int> arrayValues; // 存储数组的初始值，特别是常量数组

    // For functions:
    std::string returnType; // "int" or "void"
    std::vector<std::pair<std::string, std::string>> params; // list of (param_name, param_type_string)
    bool isParam; // Added to track if the symbol is a function parameter

    Symbol() : value(0), offset(0), isConstant(false), isGlobal(false), scopeLevel(0), 
               isArray(false), isInitialized(false), isParam(false) {} // Constructor declaration

    // Helper to get the total number of elements in an array
    int getTotalSize() const {
        if (!isArray || arrayDimensions.empty()) {
            // For non-arrays or arrays with no specified dimensions (e.g. func params), 
            // treat as 1 for initialization purposes if needed, or 0 if more appropriate.
            // For InitVal checking, if it's not an array, this won't be called in that context.
            // If it is an array but dimensions are empty (e.g. `int arr[]`), size is unknown from declaration alone.
            // Let's assume for a declared array, dimensions should not be empty.
            // If an array has dimensions, calculate total size.
            if (isArray && arrayDimensions.empty()) return 0; // Or handle as error, an array should have dimensions if declared with them
            if (!isArray) return 1; // Scalar
        }
        int totalSize = 1;
        for (int dimSize : arrayDimensions) {
            if (dimSize <= 0) return 0; // Invalid dimension implies zero size
            totalSize *= dimSize;
        }
        return totalSize;
    }

    // Helper to get the base type of a symbol (e.g., "int" from "array_int")
    std::string getBaseType() const {
        if (type.rfind("array2d_const_", 0) == 0) return type.substr(14);
        if (type.rfind("array_const_", 0) == 0) return type.substr(12);
        if (type.rfind("array2d_", 0) == 0) return type.substr(8);
        if (type.rfind("array_", 0) == 0) return type.substr(6);
        if (type.rfind("const_", 0) == 0) return type.substr(6);
        // For types like "int", "int_func", "void_func", "int_addr", etc., the type itself is the base.
        // However, for use in parseExp, we usually expect "int" for expressions.
        // This function should primarily strip array/const prefixes for element type determination.
        if (type == "int" || type == "const_int") return "int"; 
        // Fallback for other complex types if not directly an element type we expect for expressions
        // This might need refinement based on how types are used in parseExp for expectedType
        return type; // Default to returning the full type if no prefix matches
    }
};

#endif // MINE_SYMBOL_ENTRY_H_ 