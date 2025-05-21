#include "symboltable.h"
#include <iostream> // For error messages

// SymbolTable class implementation from lab6.cpp
// ... (Lines 62 to 149 from original lab6.cpp, containing SymbolTable implementation)
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
    // tableStack.pop_back(); // Original behavior if not keeping scopes.
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
        // std::cout << "SymbolTable Warning: Redeclaring symbol '" << symbol.name << "' in scope level " << currentScopeLevel << "." << std::endl;
    }

    // --->>> NEW OFFSET CALCULATION LOGIC START <<<---
    if (!symbol.isGlobal && !symbol.isParam) { // For local variables and temporaries
        int size = 4; // Default size (e.g., for int, or a temp)
        if (symbol.isArray) {
            size = 4; // Base size of an element
            if (symbol.arrayDimensions.empty()) { 
                // This case should ideally not happen if isArray is true and parsed correctly.
                // std::cerr << "SymbolTable Warning: Array symbol '" << symbol.name << "' has no dimensions. Assuming size for 1 element (4 bytes)." << std::endl;
                // Keep size = 4 for a single element if dimensions are missing for an array.
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
        
        // Optional: Add a specific debug log for offset calculation if needed elsewhere
        // std::cout << "OFFSET_CALC_DEBUG: For local/temp '/" << symbol.name 
        //           << "', size=" << size << ", new currentFrameOffset=" << currentFrameOffset 
        //           << ", symbol.offset set to " << symbol.offset << std::endl;

    } else if (symbol.isGlobal) {
        // Global offset calculation (consistent with original structure if it was present)
        int global_size = 4;
        if (symbol.isArray) {
            global_size = 4;
            if (symbol.arrayDimensions.empty()) {
                // std::cerr << "SymbolTable Warning: Global array symbol '" << symbol.name << "' has no dimensions. Assuming size for 1 element (4 bytes)." << std::endl;
            } else {
            for (int dim : symbol.arrayDimensions) {
                    if (dim > 0) global_size *= dim;
                    // else: handle non-positive dimension for globals if necessary, similar to locals
                }
            }
        }
        // It seems currentGlobalDataOffset was used to assign the *start* offset for a global.
        // The symbol.offset should be this starting offset.
        // Then currentGlobalDataOffset is incremented by the size of this global for the next one.
        symbol.offset = currentGlobalDataOffset; 
        currentGlobalDataOffset += global_size;

        // Optional: Add a specific debug log for global offset calculation
        // std::cout << "OFFSET_CALC_DEBUG: For global '/" << symbol.name 
        //           << "', size=" << global_size << ", currentGlobalDataOffset now " << currentGlobalDataOffset 
        //           << ", symbol.offset set to " << symbol.offset << std::endl;
    }
    // For parameters (symbol.isParam == true), their offsets are set by addParamSymbol.
    // --->>> NEW OFFSET CALCULATION LOGIC END <<<---

    tableStack[currentScopeLevel - 1][symbol.name] = symbol; 

    // ******** DETAILED DEBUGGING START ********
    std::cout << "ADD_SYM_DEBUG: For func '" << activeFunctionName << "', adding sym '" << symbol.name 
              << "' (type: " << symbol.type << ", offset: " << symbol.offset 
              << ", sym.scopeLevel: " << symbol.scopeLevel << ") to currentScopeLevel " << currentScopeLevel 
              << ". tableStack.size() = " << tableStack.size() << std::endl;
    if (!tableStack.empty()) {
        std::cout << "  ADD_SYM_DEBUG: tableStack.back() (idx " << tableStack.size()-1 << ") now " 
                  << (tableStack.back().count(symbol.name) ? "CONTAINS" : "DOES NOT CONTAIN") // This check might be misleading if back() is not currentScopeLevel-1
                  << " '" << symbol.name << "' (This map might not be the target scope)." << std::endl;
    }
    if (currentScopeLevel > 0 && (currentScopeLevel -1) < tableStack.size() ) {
         std::cout << "  ADD_SYM_DEBUG: tableStack[" << currentScopeLevel - 1 << "] (target scope) now "
                  << (tableStack[currentScopeLevel-1].count(symbol.name) ? "CONTAINS" : "DOES NOT CONTAIN")
                  << " '" << symbol.name << "'." << std::endl;
    } else {
        // This case should be caught by the bounds check at the beginning of the function
        std::cout << "  ADD_SYM_DEBUG: (Error state) currentScopeLevel-1 (" << currentScopeLevel-1 << ") is out of bounds for tableStack (size " << tableStack.size() << ")." << std::endl;
    }
    // ******** DETAILED DEBUGGING END ********

    if (!activeFunctionName.empty() && !symbol.isGlobal) { // Only add local symbols to the list
        activeFunctionSymbolsList.push_back(symbol); // New: adding to vector
        // ******** DETAILED DEBUGGING START ********
        std::cout << "  ADD_SYM_DEBUG: Pushed symbol '" << symbol.name << "' to activeFunctionSymbolsList. List size: " 
                  << activeFunctionSymbolsList.size() << std::endl;
        // ******** DETAILED DEBUGGING END ********
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

    // All parameters will have a unique, positive offset from $fp in the callee's frame
    // This offset is where they are expected to be for unified access during function execution.
    paramSymbol.offset = (paramIndex + 1) * 4;
    paramSymbol.isParam = true; 

    // Check for redefinition in the intended scope map
    if (tableStack[currentScopeLevel - 1].count(paramSymbol.name)) {
        // Parameter already exists in the current scope level.
        // std::cout << "SymbolTable Warning: Redeclaring parameter '" << paramSymbol.name << "' in scope level " << currentScopeLevel << "." << std::endl;
    }

    tableStack[currentScopeLevel - 1][paramSymbol.name] = paramSymbol; // Use currentScopeLevel-1 as index

    // ******** DETAILED DEBUGGING START (for addParamSymbol) ********
    std::cout << "ADD_PARAM_SYM_DEBUG: For func '" << activeFunctionName << "', adding param '" << paramSymbol.name 
              << "' (type: " << paramSymbol.type << ", offset: " << paramSymbol.offset 
              << ", sym.scopeLevel: " << paramSymbol.scopeLevel << ", isParam: " << paramSymbol.isParam 
              << ") to currentScopeLevel " << currentScopeLevel 
              << ". tableStack.size() = " << tableStack.size() << std::endl;
    if (!tableStack.empty()) {
        std::cout << "  ADD_PARAM_SYM_DEBUG: tableStack.back() (idx " << tableStack.size()-1 << ") now " 
                  << (tableStack.back().count(paramSymbol.name) ? "CONTAINS" : "DOES NOT CONTAIN")
                  << " '" << paramSymbol.name << "' (This map might not be the target scope)." << std::endl;
    }
    if (currentScopeLevel > 0 && (currentScopeLevel -1) < tableStack.size() ) {
         std::cout << "  ADD_PARAM_SYM_DEBUG: tableStack[" << currentScopeLevel - 1 << "] (target scope) now "
                  << (tableStack[currentScopeLevel-1].count(paramSymbol.name) ? "CONTAINS" : "DOES NOT CONTAIN")
                  << " '" << paramSymbol.name << "'." << std::endl;
    } else {
        std::cout << "  ADD_PARAM_SYM_DEBUG: (Error state) currentScopeLevel-1 (" << currentScopeLevel-1 << ") is out of bounds for tableStack (size " << tableStack.size() << ")." << std::endl;
    }
    // ******** DETAILED DEBUGGING END (for addParamSymbol) ********

    if (!activeFunctionName.empty()) {
        activeFunctionSymbolsList.push_back(paramSymbol); // New: adding to vector
         // ******** DETAILED DEBUGGING START (for addParamSymbol) ********
        std::cout << "  ADD_PARAM_SYM_DEBUG: Pushed param symbol '" << paramSymbol.name << "' to activeFunctionSymbolsList. List size: " 
                  << activeFunctionSymbolsList.size() << std::endl;
        // ******** DETAILED DEBUGGING END (for addParamSymbol) ********
    }

    return true;
}

// Overloaded lookupSymbol: functionScopeName takes precedence for params/locals of that func,
// then current scope stack, then global.
// If globalOnly is true, only global (tableStack[0]) is checked.
Symbol* SymbolTable::lookupSymbol(const std::string& name, bool globalOnly, const std::string& functionScopeName) {
    #ifdef LOOKUP_SYM_DEBUG
    std::cout << "LOOKUP_SYM_DEBUG: For func '" << (activeFunctionName.empty() ? "GLOBAL" : activeFunctionName) 
              << "', looking for sym '" << name << "'. GlobalOnly: " << globalOnly 
              << ", FuncScopeHint: '" << functionScopeName << "'. tableStack.size() = " << tableStack.size() 
              << ", currentScopeLevel = " << currentScopeLevel << std::endl;
    #endif

    // If a specific function scope is provided, check its dedicated local/param symbol COLLECTION first.
    // This collection is now a std::vector<Symbol>.
    // We need to find the one with the highest scope level that matches the name if there are shadowed variables.
    // Or, more simply for now, if MipsGenerator always uses mangled names, this specific lookup path might be less critical for it.
    // However, SyntaxAnalyzer still uses lookupSymbol for semantic checks.
    if (!functionScopeName.empty()) {
        auto funcIter = functionLocalSymbols.find(functionScopeName);
        if (funcIter != functionLocalSymbols.end()) {
            // funcIter->second is a std::vector<Symbol>
            Symbol* foundSym = nullptr;
            int maxScopeLevelFound = -1;
            for (const auto& sym : funcIter->second) { // Iterate vector
                if (sym.name == name) {
                    if (sym.scopeLevel > maxScopeLevelFound) { // Find the one from the innermost scope (highest level)
                        // This const_cast is generally unsafe if the symbols in vector are not meant to be modified.
                        // Consider if SymbolTable should return const Symbol* or if MipsGenerator needs non-const.
                        // For now, to match existing return type Symbol*.
                        foundSym = const_cast<Symbol*>(&sym);
                        maxScopeLevelFound = sym.scopeLevel;
                    }
                }
            }
            if (foundSym) {
                #ifdef LOOKUP_SYM_DEBUG
                std::cout << "LOOKUP_SYM_DEBUG: Found '" << name << "' in specified function scope (vector) '" << functionScopeName 
                          << "' at scope level " << maxScopeLevelFound << "."
                          << " Offset: " << foundSym->offset << ", isParam: " << foundSym->isParam << std::endl;
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
                std::cout << "LOOKUP_SYM_DEBUG: Found '" << name << "' in global scope (globalOnly=true)." << std::endl;
                #endif
                return &(it->second);
            }
        }
        #ifdef LOOKUP_SYM_DEBUG
        std::cout << "LOOKUP_SYM_DEBUG: Did not find '" << name << "' in global scope (globalOnly=true)." << std::endl;
        #endif
        return nullptr; // Not found in global, and globalOnly is true
    }

    // Search from current scope outwards to global scope in tableStack
    if (currentScopeLevel > 0 && currentScopeLevel <= tableStack.size()) {
    for (int i = currentScopeLevel - 1; i >= 0; --i) {
            #ifdef LOOKUP_SYM_DEBUG
            std::cout << "LOOKUP_SYM_DEBUG: Checking scope level " << (i + 1) << " (index " << i << ") for '" << name << "'." << std::endl;
            #endif
            auto it = tableStack[i].find(name);
            if (it != tableStack[i].end()) {
                #ifdef LOOKUP_SYM_DEBUG
                std::cout << "LOOKUP_SYM_DEBUG: Found '" << name << "' in scope level " << (i + 1) << "."
                          << " Offset: " << it->second.offset << ", isParam: " << it->second.isParam << std::endl;
                #endif
                return &(it->second);
            }
        }
    }

    // If not found in scope stack and not globalOnly, check global string literals
    // String literals are a bit special; they are globally unique by their label.
    auto lit_it = globalLiterals.find(name);
    if (lit_it != globalLiterals.end()) {
        #ifdef LOOKUP_SYM_DEBUG
        std::cout << "LOOKUP_SYM_DEBUG: Found '" << name << "' in globalLiterals." << std::endl;
        #endif
        return &(lit_it->second);
    }

    #ifdef LOOKUP_SYM_DEBUG
    std::cout << "LOOKUP_SYM_DEBUG: Did not find '" << name << "' in any checked scopes (including globalLiterals)." << std::endl;
    #endif
    return nullptr;
}

std::string SymbolTable::addStringLiteral(const std::string& strValWithQuotes) {
    // Remove quotes for internal storage of strValue, but label can be based on content.
    std::string actualContent = strValWithQuotes.substr(1, strValWithQuotes.length() - 2);

    // Check if this exact string content already exists to reuse label
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
    // currentFrameOffset should represent the offset of the *next available* slot
    // relative to $fp. If $fp points to the saved old $fp:
    //  0($fp) is saved old $fp
    // -4($fp) is saved $ra
    // So the first local variable should start at -8($fp).
    // If currentFrameOffset is the offset to be assigned to the *current* symbol,
    // and then decremented, it should start such that the first assignment is -8.
    // Let's adjust its usage: currentFrameOffset is the boundary.
    // The first local variable starts at $fp - 8.
    // So, currentFrameOffset will track the upper boundary of allocated local space.
    // When a variable of size S is added, it occupies from currentFrameOffset-S to currentFrameOffset-1.
    // The next variable will start at currentFrameOffset-S.
    // So, if currentFrameOffset starts at -4 (boundary after $ra),
    // first local (size 4) new currentFrameOffset becomes -8, and symbol gets -8. This is correct.
    this->currentFrameOffset = -4; 
    this->currentParamOffset = 4; // First param usually at 4($fp) or 8($fp) depending on $fp definition
                                 // If $fp points to saved old $fp, then 4($fp) is above $fp, in caller's domain or for first stack-passed arg by caller
                                 // Our parseFuncFParam sets param offset as 4 + paramIndex * 4.
                                 // So param 0 is 4($fp), param 1 is 8($fp). This currentParamOffset might not be directly used if offsets are hardcoded in parseFuncFParam.

    // Reset local symbol accumulation for the new function
    if (!activeFunctionSymbolsList.empty()) { // Check if vector is not empty
        activeFunctionSymbolsList.clear();    // Use .clear() for vector
    }
    // Ensure activeFunctionName is also cleared or set appropriately when a function ends.
    // activeFunctionName is set by beginFunctionCompilation.
}

int SymbolTable::getCurrentFrameSizeForLocals() const {
    // currentFrameOffset is negative and its absolute value is the size.
    return -currentFrameOffset; 
}

// NEW implementation
void SymbolTable::beginFunctionCompilation(const std::string& funcName) {
    activeFunctionName = funcName;
    activeFunctionSymbolsList.clear(); // New list
    std::cout << "SYMBOL_TABLE_DEBUG: Beginning compilation for function '" << funcName << "'. Active local symbols (list) cleared." << std::endl;
}

// NEW implementation
void SymbolTable::endFunctionCompilation(const std::string& funcName) {
    if (funcName == activeFunctionName && !activeFunctionName.empty()) {
        functionLocalSymbols[funcName] = activeFunctionSymbolsList; // New: assigning vector
        std::cout << "SYMBOL_TABLE_DEBUG: Ending compilation for function '" << funcName << "'. Stored " 
                  << activeFunctionSymbolsList.size() << " local symbols (params, locals, temps) from list." << std::endl;
        for (const Symbol& sym : activeFunctionSymbolsList) { // New: iterating vector
            std::cout << "  -> Sym: '" << sym.name << "' (Original Name), Offset: " << sym.offset 
                      << ", isParam: " << (sym.isParam ? "yes" : "no") 
                      << ", ScopeLvl: " << sym.scopeLevel << std::endl;
        }
        activeFunctionName.clear();
        activeFunctionSymbolsList.clear(); // Clear list after copying
    } else {
        std::cerr << "SYMBOL_TABLE_ERROR: Mismatch or inactive function for endFunctionCompilation. Requested: '" 
                  << funcName << "', Active: '" << activeFunctionName << "'" << std::endl;
    }
} 