#include "symbol_table.h"
#include <iostream> // For error messages
#include <cstdio>   // For dbg_printf (fopen, fprintf, fflush, stderr)
#include <string>   // For std::string, std::to_string

// #define DEBUG

// Debug macro definition
#ifdef DEBUG
static FILE *debug_log_file = NULL; // File-static pointer for the debug log
#define dbg_printf(...) \
    do { \
        if (debug_log_file == NULL) { \
            debug_log_file = fopen("mm_debug_log.txt", "w"); \
            if (debug_log_file == NULL) { \
                fprintf(stderr, "Error: Could not open debug log file mm_debug_log.txt!\n"); \
            } \
        } \
        if (debug_log_file != NULL) { \
            fprintf(debug_log_file, __VA_ARGS__); \
            fflush(debug_log_file); /* Optional: ensure immediate write, but may affect performance */ \
        } else { \
            fprintf(stderr, __VA_ARGS__); /* Fallback to stderr if file opening failed */ \
        } \
    } while (0)
#else
#define dbg_printf(...) // Defines dbg_printf as a no-op when DEBUG is not defined
#endif

SymbolTable::SymbolTable() : currentScopeLevel(0), currentGlobalDataOffset(0), currentFrameOffset(0) {
    enterScope(); // Initialize global scope
}

void SymbolTable::enterScope() {
    currentScopeLevel++;
    // currentScopeLevel is 1-based (1 for global, 2 for first func, etc.)
    // tableStack is 0-indexed.
    if (currentScopeLevel > tableStack.size()) {
        // This scope level has never been reached before, add a new map for it.
    tableStack.push_back({});
    } else {
        // This scope level has been reached before (e.g., after exiting a function
        // and entering another sibling function). We need to clear the existing map
        // at this level to provide a clean slate for the new scope.
        // The index for the current scope's map is currentScopeLevel - 1.
        if (!tableStack[currentScopeLevel - 1].empty()) {
            tableStack[currentScopeLevel - 1].clear();
        }
    }
    // Note: Resetting currentFrameOffset is typically done when a new function definition begins,
    // often via a call to resetFrameOffsetForNewFunction() from the SyntaxAnalyzer,
    // right after entering the function's main scope but before processing params/locals.
}

void SymbolTable::exitScope() {
    // DO NOT POP. Keep all scopes for MipsGenerator. Just decrement level.
    if (currentScopeLevel > 0) { // Should not be called on global scope, but good practice
        currentScopeLevel--;
    }
}

bool SymbolTable::addSymbol(Symbol& symbol) {
    if (currentScopeLevel == 0) {
        std::cerr << "SymbolTable Error: addSymbol called with currentScopeLevel 0 for symbol '" << symbol.name << "'" << std::endl;
        return false;
    }
    if ((currentScopeLevel - 1) >= tableStack.size()) {
        std::cerr << "SymbolTable Error: currentScopeLevel-1 (" << (currentScopeLevel - 1)
                  << ") is out of bounds for tableStack (size: " << tableStack.size()
                  << ") when adding symbol '" << symbol.name << "'. Ensure enterScope is correctly managed."
                  << std::endl;
        return false;
    }

    // Check for redefinition in the intended scope map
    if (tableStack[currentScopeLevel - 1].count(symbol.name)) {
        // Symbol already exists in the current scope level.
    }

    if (!symbol.isGlobal && !symbol.isParam) { // For local variables and temporaries
        int size = 4; // Default size (e.g., for int, or a temp)
        if (symbol.isArray) {
            size = 4; // Base size of an element
            if (symbol.arrayDimensions.empty()) {
                // This case should ideally not happen if isArray is true and parsed correctly.
            } else {
            for (int dim : symbol.arrayDimensions) {
                    if (dim > 0) {
                size *= dim;
                    } else {
                        std::cerr << "SymbolTable Error: Array '" << symbol.name << "' has non-positive or zero dimension (" << dim << "). Defaulting size for this dimension to 1." << std::endl;
                        // If a dimension is invalid, effectively treat it as 1 for size calculation to avoid zero or negative size.
                        // This might not be ideal but prevents crashes. Proper parsing should ensure valid dimensions.
                    }
                }
            }
        }
        currentFrameOffset -= size;
        symbol.offset = currentFrameOffset;

    } else if (symbol.isGlobal) {
        // Global offset calculation
        int global_size = 4;
        if (symbol.isArray) {
            global_size = 4;
            if (symbol.arrayDimensions.empty()) {
                // Array symbol has no dimensions, assuming size for 1 element.
            } else {
            for (int dim : symbol.arrayDimensions) {
                    if (dim > 0) global_size *= dim;
                    // else: handle non-positive dimension for globals if necessary, similar to locals
                }
            }
        }
        // Assign global offset and increment for next global
        symbol.offset = currentGlobalDataOffset;
        currentGlobalDataOffset += global_size;
    }
    // For parameters (symbol.isParam == true), their offsets are set by addParamSymbol.

    tableStack[currentScopeLevel - 1][symbol.name] = symbol;

    dbg_printf("ADD_SYM_DEBUG: For func '%s', adding sym '%s' (type: %s, offset: %d, sym.scopeLevel: %d) to currentScopeLevel %d. tableStack.size() = %zu\n",
               activeFunctionName.c_str(), symbol.name.c_str(), symbol.type.c_str(), symbol.offset,
               symbol.scopeLevel, currentScopeLevel, tableStack.size());
    if (!tableStack.empty()) {
        dbg_printf("  ADD_SYM_DEBUG: tableStack.back() (idx %zu) now %s '%s' (This map might not be the target scope).\n",
                   tableStack.size()-1, (tableStack.back().count(symbol.name) ? "CONTAINS" : "DOES NOT CONTAIN"),
                   symbol.name.c_str());
    }
    if (currentScopeLevel > 0 && (currentScopeLevel -1) < tableStack.size() ) {
         dbg_printf("  ADD_SYM_DEBUG: tableStack[%d] (target scope) now %s '%s'.\n",
                    currentScopeLevel - 1, (tableStack[currentScopeLevel-1].count(symbol.name) ? "CONTAINS" : "DOES NOT CONTAIN"),
                    symbol.name.c_str());
    } else {
        dbg_printf("  ADD_SYM_DEBUG: (Error state) currentScopeLevel-1 (%d) is out of bounds for tableStack (size %zu).\n",
                   currentScopeLevel-1, tableStack.size());
    }

    if (!activeFunctionName.empty() && !symbol.isGlobal) { // Only add local symbols to the list
        activeFunctionSymbolsList.push_back(symbol);
        dbg_printf("  ADD_SYM_DEBUG: Pushed symbol '%s' to activeFunctionSymbolsList. List size: %zu\n",
                   symbol.name.c_str(), activeFunctionSymbolsList.size());
    }
    return true;
}

bool SymbolTable::addParamSymbol(Symbol& paramSymbol, int paramIndex) {
    if (currentScopeLevel == 0) {
        std::cerr << "SymbolTable Error: addParamSymbol called with currentScopeLevel 0 for param '" << paramSymbol.name << "'" << std::endl;
        return false;
    }
    if ((currentScopeLevel - 1) >= tableStack.size()) {
        std::cerr << "SymbolTable Error: currentScopeLevel-1 (" << (currentScopeLevel - 1)
                  << ") is out of bounds for tableStack (size: " << tableStack.size()
                  << ") when adding param '" << paramSymbol.name << "'. Ensure enterScope is correctly managed."
                  << std::endl;
        return false;
    }

    // Parameters have positive offsets from $fp, set based on their index.
    paramSymbol.offset = (paramIndex + 1) * 4;
    paramSymbol.isParam = true;

    // Check for redefinition in the intended scope map
    if (tableStack[currentScopeLevel - 1].count(paramSymbol.name)) {
        // Parameter already exists in the current scope level.
    }

    tableStack[currentScopeLevel - 1][paramSymbol.name] = paramSymbol; // Use currentScopeLevel-1 as index

    dbg_printf("ADD_PARAM_SYM_DEBUG: For func '%s', adding param '%s' (type: %s, offset: %d, sym.scopeLevel: %d, isParam: %s) to currentScopeLevel %d. tableStack.size() = %zu\n",
               activeFunctionName.c_str(), paramSymbol.name.c_str(), paramSymbol.type.c_str(), paramSymbol.offset,
               paramSymbol.scopeLevel, (paramSymbol.isParam ? "true" : "false"), currentScopeLevel, tableStack.size());
    if (!tableStack.empty()) {
        dbg_printf("  ADD_PARAM_SYM_DEBUG: tableStack.back() (idx %zu) now %s '%s' (This map might not be the target scope).\n",
                   tableStack.size()-1, (tableStack.back().count(paramSymbol.name) ? "CONTAINS" : "DOES NOT CONTAIN"),
                   paramSymbol.name.c_str());
    }
    if (currentScopeLevel > 0 && (currentScopeLevel -1) < tableStack.size() ) {
         dbg_printf("  ADD_PARAM_SYM_DEBUG: tableStack[%d] (target scope) now %s '%s'.\n",
                    currentScopeLevel - 1, (tableStack[currentScopeLevel-1].count(paramSymbol.name) ? "CONTAINS" : "DOES NOT CONTAIN"),
                    paramSymbol.name.c_str());
    } else {
        dbg_printf("  ADD_PARAM_SYM_DEBUG: (Error state) currentScopeLevel-1 (%d) is out of bounds for tableStack (size %zu).\n",
                   currentScopeLevel-1, tableStack.size());
    }

    if (!activeFunctionName.empty()) {
        activeFunctionSymbolsList.push_back(paramSymbol);
        dbg_printf("  ADD_PARAM_SYM_DEBUG: Pushed param symbol '%s' to activeFunctionSymbolsList. List size: %zu\n",
                   paramSymbol.name.c_str(), activeFunctionSymbolsList.size());
    }

    return true;
}

// Overloaded lookupSymbol: functionScopeName takes precedence for params/locals of that func,
// then current scope stack, then global.
// If globalOnly is true, only global (tableStack[0]) is checked.
Symbol* SymbolTable::lookupSymbol(const std::string& name, bool globalOnly, const std::string& functionScopeName) {
    #ifdef LOOKUP_SYM_DEBUG
    dbg_printf("LOOKUP_SYM_DEBUG: For func '%s', looking for sym '%s'. GlobalOnly: %s, FuncScopeHint: '%s'. tableStack.size() = %zu, currentScopeLevel = %d\n",
               (activeFunctionName.empty() ? "GLOBAL" : activeFunctionName.c_str()),
               name.c_str(), (globalOnly ? "true" : "false"), functionScopeName.c_str(),
               tableStack.size(), currentScopeLevel);
    #endif

    // If a specific function scope is provided, check its dedicated local/param symbol collection first.
    // This collection is a std::vector<Symbol>.
    // Search for the symbol with the highest scope level to handle shadowing.
    // This path is important for semantic checks by SyntaxAnalyzer.
    if (!functionScopeName.empty()) {
        auto funcIter = functionLocalSymbols.find(functionScopeName);
        if (funcIter != functionLocalSymbols.end()) {
            // funcIter->second is a std::vector<Symbol>
            Symbol* foundSym = nullptr;
            int maxScopeLevelFound = -1;
            for (const auto& sym : funcIter->second) {
                if (sym.name == name) {
                    if (sym.scopeLevel > maxScopeLevelFound) { // Find the one from the innermost scope (highest level)
                        // This const_cast is generally unsafe.
                        // Consider returning const Symbol* or ensuring MipsGenerator doesn't modify.
                        // Matching existing Symbol* return type for now.
                        foundSym = const_cast<Symbol*>(&sym);
                        maxScopeLevelFound = sym.scopeLevel;
                    }
                }
            }
            if (foundSym) {
                #ifdef LOOKUP_SYM_DEBUG
                dbg_printf("LOOKUP_SYM_DEBUG: Found '%s' in specified function scope (vector) '%s' at scope level %d. Offset: %d, isParam: %s\n",
                           name.c_str(), functionScopeName.c_str(), maxScopeLevelFound,
                           foundSym->offset, (foundSym->isParam ? "true" : "false"));
                #endif
                return foundSym;
            }
        }
    }

    if (globalOnly) {
        if (!tableStack.empty()) { // Check global scope (level 1, index 0)
            auto& globalScope = tableStack[0];
            auto it = globalScope.find(name);
            if (it != globalScope.end()) {
                #ifdef LOOKUP_SYM_DEBUG
                dbg_printf("LOOKUP_SYM_DEBUG: Found '%s' in global scope (globalOnly=true).\n", name.c_str());
                #endif
                return &(it->second);
            }
        }
        #ifdef LOOKUP_SYM_DEBUG
        dbg_printf("LOOKUP_SYM_DEBUG: Did not find '%s' in global scope (globalOnly=true).\n", name.c_str());
        #endif
        return nullptr; // Not found in global, and globalOnly is true
    }

    // Search from current scope outwards to global scope in tableStack
    if (currentScopeLevel > 0 && currentScopeLevel <= tableStack.size()) {
    for (int i = currentScopeLevel - 1; i >= 0; --i) {
            #ifdef LOOKUP_SYM_DEBUG
            dbg_printf("LOOKUP_SYM_DEBUG: Checking scope level %d (index %d) for '%s'.\n", (i + 1), i, name.c_str());
            #endif
            auto it = tableStack[i].find(name);
            if (it != tableStack[i].end()) {
                #ifdef LOOKUP_SYM_DEBUG
                dbg_printf("LOOKUP_SYM_DEBUG: Found '%s' in scope level %d. Offset: %d, isParam: %s\n",
                           name.c_str(), (i + 1), it->second.offset, (it->second.isParam ? "true" : "false"));
                #endif
                return &(it->second);
            }
        }
    }

    // Check global string literals if not found elsewhere and not globalOnly.
    // String literals are globally unique by their label.
    auto lit_it = globalLiterals.find(name);
    if (lit_it != globalLiterals.end()) {
        #ifdef LOOKUP_SYM_DEBUG
        dbg_printf("LOOKUP_SYM_DEBUG: Found '%s' in globalLiterals.\n", name.c_str());
        #endif
        return &(lit_it->second);
    }

    #ifdef LOOKUP_SYM_DEBUG
    dbg_printf("LOOKUP_SYM_DEBUG: Did not find '%s' in any checked scopes (including globalLiterals).\n", name.c_str());
    #endif
    return nullptr;
}

std::string SymbolTable::addStringLiteral(const std::string& strValWithQuotes) {
    // Remove quotes for internal storage.
    std::string actualContent = strValWithQuotes.substr(1, strValWithQuotes.length() - 2);

    // Reuse label if this exact string content already exists.
    for (auto const& pair : globalLiterals) {
        const std::string& key = pair.first;
        const Symbol& val = pair.second;
        if (val.type == "string_literal" && val.strValue == actualContent) {
            return val.name; // Return existing label
        }
    }

    std::string label = "_str_lit_" + std::to_string(globalLiterals.size());
    Symbol strSymbol;
    strSymbol.name = label;
    strSymbol.type = "string_literal";
    strSymbol.strValue = actualContent; // Store without quotes
    strSymbol.isGlobal = true; // String literals are global
    strSymbol.scopeLevel = 1; // Global scope
    globalLiterals[label] = strSymbol;
    return label;
}

void SymbolTable::resetFrameOffsetForNewFunction() {
    // currentFrameOffset tracks the next available slot relative to $fp.
    // Frame layout:
    //   0($fp): saved old $fp
    //  -4($fp): saved $ra
    // First local variable starts at -8($fp).
    // currentFrameOffset is initialized to -4; first local (size 4) will be assigned
    // offset -8, and currentFrameOffset becomes -8.
    this->currentFrameOffset = -4;
    this->currentParamOffset = 4; // First param typically at 4($fp).
                                 // Actual param offsets are set in addParamSymbol based on index.

    // Reset local symbol accumulation for the new function.
    if (!activeFunctionSymbolsList.empty()) {
        activeFunctionSymbolsList.clear();
    }
    // activeFunctionName is set by beginFunctionCompilation and cleared by endFunctionCompilation.
}

int SymbolTable::getCurrentFrameSizeForLocals() const {
    // currentFrameOffset is negative; its absolute value is the total size of locals.
    return -currentFrameOffset;
}

void SymbolTable::beginFunctionCompilation(const std::string& funcName) {
    activeFunctionName = funcName;
    activeFunctionSymbolsList.clear();
    dbg_printf("SYMBOL_TABLE_DEBUG: Beginning compilation for function '%s'. Active local symbols (list) cleared.\n", funcName.c_str());
}

void SymbolTable::endFunctionCompilation(const std::string& funcName) {
    if (funcName == activeFunctionName && !activeFunctionName.empty()) {
        functionLocalSymbols[funcName] = activeFunctionSymbolsList;
        dbg_printf("SYMBOL_TABLE_DEBUG: Ending compilation for function '%s'. Stored %zu local symbols (params, locals, temps) from list.\n",
                   funcName.c_str(), activeFunctionSymbolsList.size());
        for (const Symbol& sym : activeFunctionSymbolsList) {
            dbg_printf("  -> Sym: '%s' (Original Name), Offset: %d, isParam: %s, ScopeLvl: %d\n",
                       sym.name.c_str(), sym.offset, (sym.isParam ? "yes" : "no"), sym.scopeLevel);
        }
        activeFunctionName.clear();
        activeFunctionSymbolsList.clear(); // Clear list after copying
    } else {
        std::cerr << "SYMBOL_TABLE_ERROR: Mismatch or inactive function for endFunctionCompilation. Requested: '"
                  << funcName << "', Active: '" << activeFunctionName << "'" << std::endl;
    }
}

std::vector<Symbol> SymbolTable::getGlobalVariableSymbols() const {
    std::vector<Symbol> globalVars;
    if (!tableStack.empty()) { // Global scope is tableStack[0]
        const auto& globalScopeMap = tableStack[0];
        for (const auto& pair : globalScopeMap) {
            const Symbol& sym = pair.second;
            // Filter out non-variables like functions and string literals (which are handled differently)
            // This condition assumes that typical variable types are "int", "const_int", "array_int", etc.
            // and function types include "_func".
            if (sym.type != "int_func" && sym.type != "void_func" && 
                sym.type.find("func") == std::string::npos && // A more general check for func types
                sym.type != "string_literal") {
                globalVars.push_back(sym);
            }
        }
    }
    return globalVars;
}