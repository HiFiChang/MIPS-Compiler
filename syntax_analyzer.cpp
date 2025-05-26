#include "syntax_analyzer.h"
#include "mips_generator.h"
#include <algorithm>
#include <cctype>

SyntaxAnalyzer::SyntaxAnalyzer(Lexer& lex, SymbolTable& sTab, IRGenerator& irG,
                             const std::string& primaryOutputFileName,
                             bool enableOldSyntaxOutput,
                             const std::string& secondaryOutputFileName)
    : lexer(lex), symTab(sTab), irGen(irG), mipsFileName(primaryOutputFileName),
      syntaxOutputFileNameInternal(secondaryOutputFileName),
      enableSyntaxOutput(enableOldSyntaxOutput) {

    if (enableSyntaxOutput && !syntaxOutputFileNameInternal.empty()) {
        syntaxOutFileInternal.open(syntaxOutputFileNameInternal);
        if (!syntaxOutFileInternal.is_open()) {
             std::cerr << "Failed to open syntax output file: " << syntaxOutputFileNameInternal << ", disabling it." << std::endl;
             enableSyntaxOutput = false;
        }
    }
    getNextTokenFromLexer(); // Initialize currentToken from the pre-lexed token stream
}

SyntaxAnalyzer::~SyntaxAnalyzer() {
    if (syntaxOutFileInternal.is_open()) syntaxOutFileInternal.close();
}

// Private helper method to get the mangled name for a symbol in IR.
std::string SyntaxAnalyzer::getMangledNameForIR(Symbol* sym) const {
    if (!sym) {
        return "";
    }
    std::string baseName = sym->name;
    bool isIRGenerated = (baseName.length() > 0 && baseName[0] == '_');

    std::string funcNameForMangling;
    if (currentFunctionSym) { // currentFunctionSym is a Symbol* member
        funcNameForMangling = currentFunctionSym->name;
    }

    if (!sym->isGlobal && !isIRGenerated && !funcNameForMangling.empty()) {
        // Appends function name and scope level for local user-defined variables
        return "__" + funcNameForMangling + "_" + baseName + "_s" + std::to_string(sym->scopeLevel);
    }
    return baseName; // Globals or IR-generated names are not mangled further by this scheme
}

void SyntaxAnalyzer::getNextTokenFromLexer() { // Renamed to avoid conflict with Lexer's own getNextToken
    const auto& allTokens = lexer.getTokens();
    if (tokenIndex < allTokens.size()) {
        currentToken = allTokens[tokenIndex++];
    } else {
        // This case should ideally not be reached if lexer guarantees an EOF token at the end.
        currentToken.type = TOKEN_EOF;
        currentToken.lexeme = "EOF";
    }
}

void SyntaxAnalyzer::match(TokenType expectedType) {
    if (currentToken.type == expectedType) {
        getNextTokenFromLexer();
        return;
    }
    std::cerr << "Syntax Error (line " << currentToken.lineNumber
              << "): Expected " << lexer.getTokenNames().at(expectedType)
              << ", got " << currentToken.lexeme << std::endl;
    exit(1);
}

void SyntaxAnalyzer::recordSyntaxOutput(const std::string& output) {
    if (!syntaxOutputFileNameInternal.empty()) {
        syntaxOutputsForFile.push_back({output, currentToken.lineNumber});
    }
}

void SyntaxAnalyzer::mergeAndWriteSyntaxOutput() {
    if (!syntaxOutputFileNameInternal.empty() && syntaxOutFileInternal.is_open()) {
        for (const auto& output : syntaxOutputsForFile) {
            syntaxOutFileInternal << output.first << " " << output.second << std::endl;
        }
    }
}

void SyntaxAnalyzer::semanticError(const std::string& message) {
    std::cerr << "Semantic Error (line " << currentToken.lineNumber << "): " << message << std::endl;
    exit(1);
}

std::string SyntaxAnalyzer::getExpType(const std::string& expResult) {
    if (expResult.empty()) return "unknown";
    if (isdigit(expResult[0]) || (expResult.length() > 1 && expResult[0] == '-' && isdigit(expResult[1]))) return "int";
    // String literals are identified by their labels (e.g., _str_lit_X) in IR
    if (expResult.rfind("_str_lit_", 0) == 0) return "string_literal";

    Symbol* s = symTab.lookupSymbol(expResult);
    if (s) return s->type; // This could be "int", "const_int", "array_int", "int_func", etc.

    if (expResult[0] == '_') { // Temp variables or labels
        if (expResult.length() > 1 && expResult[1] == 't') return "int"; // Assume _tX temps are int for now
        if (expResult.length() > 1 && expResult[1] == 'L') return "label"; // _LX are labels
    }
    // This is a fallback, proper type tracking for temps is needed if they can be other types.
    return "unknown_exp_type(" + expResult + ")";
}

// Static helper function to reorder quadruples, placing the main function first.
static std::vector<Quadruple> reorderQuadsForMainFirst(const std::vector<Quadruple>& originalQuads) {
    std::vector<Quadruple> reorderedQuads;
    std::vector<Quadruple> globalInitQuads;
    std::vector<Quadruple> mainFunctionQuads;
    std::vector<Quadruple> otherFunctionQuads;

    std::string currentProcessingFuncName = "";
    bool mainFound = false;
    bool inAnyFunction = false;

    for (const auto& quad : originalQuads) {
        if (quad.op == "FUNC_BEGIN") {
            inAnyFunction = true;
            currentProcessingFuncName = quad.arg1;
            if (currentProcessingFuncName == "main") {
                mainFound = true;
                mainFunctionQuads.push_back(quad);
            } else {
                otherFunctionQuads.push_back(quad);
            }
        } else if (quad.op == "FUNC_END") {
            if (currentProcessingFuncName == "main" && mainFound) {
                mainFunctionQuads.push_back(quad);
            } else if (!currentProcessingFuncName.empty()) { // End of a non-main function
                otherFunctionQuads.push_back(quad);
            }
            inAnyFunction = false;
            currentProcessingFuncName = "";
        } else {
            if (currentProcessingFuncName == "main" && mainFound) {
                mainFunctionQuads.push_back(quad);
            } else if (!currentProcessingFuncName.empty()) { // Inside a non-main function
                otherFunctionQuads.push_back(quad);
            } else { // Not inside any specific function currently (could be global or between functions)
                 // Heuristic: if not in a function yet, it's likely global init code
                 // Or if a function just ended, and this is not a FUNC_BEGIN
                if (!inAnyFunction) {
                    globalInitQuads.push_back(quad);
                } else {
                    otherFunctionQuads.push_back(quad);
                }
            }
        }
    }

    reorderedQuads.insert(reorderedQuads.end(), globalInitQuads.begin(), globalInitQuads.end());
    if (mainFound) {
        reorderedQuads.insert(reorderedQuads.end(), mainFunctionQuads.begin(), mainFunctionQuads.end());
    }
    reorderedQuads.insert(reorderedQuads.end(), otherFunctionQuads.begin(), otherFunctionQuads.end());

    // If main wasn't found but there were quads, it implies an issue or no main function.
    if (!mainFound && !originalQuads.empty()) {
        if (reorderedQuads.empty() && !originalQuads.empty()) return originalQuads;
    }
     if (reorderedQuads.size() != originalQuads.size()) {
        // This indicates a logic error in quad distribution.
        return originalQuads; // Fallback to prevent code loss
    }

    return reorderedQuads;
}

// CompUnit → {Decl} {FuncDef} MainFuncDef
void SyntaxAnalyzer::parseCompUnit() {
    while (currentToken.type != TOKEN_EOF) {
        if (currentToken.type == CONSTTK) {
            parseDecl();
        } else if (currentToken.type == INTTK || currentToken.type == VOIDTK) {
            // Peek ahead to distinguish between VarDecl, FuncDef, and MainFuncDef
            const auto& allTokens = lexer.getTokens();
            Token token1 = currentToken;
            Token token2 = (tokenIndex < allTokens.size()) ? allTokens[tokenIndex] : Token{TOKEN_EOF,"",0};
            Token token3 = (tokenIndex + 1 < allTokens.size()) ? allTokens[tokenIndex+1] : Token{TOKEN_EOF,"",0};

            if (token1.type == INTTK && token2.type == MAINTK) { // MainFuncDef: int main ()
                parseMainFuncDef();
                // As per many SysY grammars, MainFuncDef is the last part.
                // If your grammar allows declarations/definitions after main, remove this break.
                break;
            } else if (token2.type == IDENFR && token3.type == LPARENT) {
                // FuncDef: (int | void) ident ( ... )
                parseFuncDef();
            } else if (token1.type == INTTK && token2.type == IDENFR) {
                // VarDecl: int ident ... (must not be followed by LPARENT immediately if it's a var)
                // This case is covered if the FuncDef condition (token3 == LPARENT) is false.
                parseDecl();
            } else if (token1.type == VOIDTK && !(token2.type == IDENFR && token3.type == LPARENT)) {
                 // VOIDTK not followed by IDENFR LPARENT is an error at global scope for declarations
                 semanticError("Unexpected token sequence at global scope starting with VOID: " + token1.lexeme + " " + token2.lexeme);
                 break;
            }
             else {
                 if (token1.type == TOKEN_EOF) break;
                semanticError("Unexpected token sequence at global scope starting with: " + token1.lexeme + " " + token2.lexeme);
                break;
            }
        } else {
            if (currentToken.type == TOKEN_EOF) break;
            std::string tokenNameStr = "UNKNOWN_TOKEN_TYPE";
            try {
                tokenNameStr = lexer.getTokenNames().at(currentToken.type);
            } catch (const std::out_of_range& oor) {
                // Handle cases where token type might be out of bounds for the map
            }
            semanticError("Unexpected token at global scope: " + currentToken.lexeme + " of type " + tokenNameStr);
            break;
        }
    }
    recordSyntaxOutput("<CompUnit>");

    // --- MIPS Code Generation Phase (at the end of CompUnit parsing) ---
    const auto& originalQuads = irGen.getQuads();

    if (originalQuads.empty()) {
        std::cerr << "No quadruples generated. Skipping MIPS generation." << std::endl;
        return;
    }

    const std::vector<Quadruple> reorderedQuads = reorderQuadsForMainFirst(originalQuads);

    try {
        MipsGenerator finalMipsGen(this->mipsFileName, symTab, reorderedQuads);
        finalMipsGen.generate();
    } catch (const std::exception& e) {
        std::cerr << "Error generating " << this->mipsFileName << ": " << e.what() << std::endl;
    }
}

// BType → 'int'
std::string SyntaxAnalyzer::parseBType() {
    match(INTTK);
    recordSyntaxOutput("<BType>"); // Old syntax output
    return "int"; // Semantic value
}

// Number → IntConst
std::string SyntaxAnalyzer::parseNumber() {
    std::string numStr = currentToken.lexeme;
    match(INTCON);
    recordSyntaxOutput("<Number>");
    return numStr; // Semantic value (string representation of the number)
}

// ConstDecl → 'const' BType ConstDef { ',' ConstDef } ';'
void SyntaxAnalyzer::parseConstDecl() {
    match(CONSTTK);
    std::string baseType = parseBType();
    parseConstDef(baseType);
    while (currentToken.type == COMMA) {
        match(COMMA);
        parseConstDef(baseType);
    }
    match(SEMICN);
    recordSyntaxOutput("<ConstDecl>");
}

// ConstDef → Ident { '[' ConstExp ']' } '=' ConstInitVal
void SyntaxAnalyzer::parseConstDef(const std::string& constBaseType) {
    std::string name = currentToken.lexeme;
    match(IDENFR);

    Symbol sym;
    sym.name = name;
    sym.isConstant = true;
    sym.isGlobal = (symTab.currentScopeLevel == 1);
    sym.scopeLevel = symTab.currentScopeLevel;
    sym.isInitialized = true;

    bool isArray = false;
    std::vector<int> dimensions;
    if (currentToken.type == LBRACK) {
        isArray = true;
        match(LBRACK);
        std::string dim1Str = parseConstExp();
        try {
            dimensions.push_back(std::stoi(dim1Str));
        }
        catch (...) { semanticError("Array dimension 1 for const '" + name + "' must be a constant integer."); }
        match(RBRACK);

        if (currentToken.type == LBRACK) { // Check for second dimension
            match(LBRACK);
            std::string dim2Str = parseConstExp();
            try {
                dimensions.push_back(std::stoi(dim2Str));
            }
            catch (...) { semanticError("Array dimension 2 for const '" + name + "' must be a constant integer."); }
            match(RBRACK);
        }
    }
    sym.isArray = isArray;
    sym.arrayDimensions = dimensions;
    if (isArray) {
        if (dimensions.size() > 1) {
            sym.type = "array2d_const_" + constBaseType;
        } else {
            sym.type = "array_const_" + constBaseType;
        }
    } else {
        sym.type = "const_" + constBaseType;
    }

    match(ASSIGN);
    // For arrays, parseConstInitVal fills sym.arrayValues. For scalars, it fills sym.value.
    parseConstInitVal(sym, false, 0); 

    if (!symTab.addSymbol(sym)) {
        semanticError("Redefinition of constant symbol '" + sym.name + "'.");
    }

    // After symbol is added (and its offset is determined if local),
    // if it's a local constant, generate IR to initialize its stack memory.
    Symbol* symInTable = symTab.lookupSymbol(name); // Get the symbol with offset info
    if (symInTable && !symInTable->isGlobal) {
        std::string nameForIRTarget = this->getMangledNameForIR(symInTable);
        if (symInTable->isArray) {
            // Initialize local constant array elements on stack
            std::string elementSize = "4"; 
            for (size_t i = 0; i < symInTable->arrayValues.size(); ++i) {
                std::string indexStr = std::to_string(i);
                std::string offsetBytesTemp = irGen.newTemp();
                Symbol tempOffsetSym; tempOffsetSym.name = offsetBytesTemp; tempOffsetSym.type = "int"; 
                tempOffsetSym.scopeLevel = symTab.currentScopeLevel; tempOffsetSym.isGlobal = false;
                symTab.addSymbol(tempOffsetSym); // Add temp to current scope
                irGen.addQuad("MUL", indexStr, elementSize, offsetBytesTemp);

                std::string effectiveAddressTemp = irGen.newTemp();
                Symbol tempAddrSym; tempAddrSym.name = effectiveAddressTemp; tempAddrSym.type = "int_addr"; 
                tempAddrSym.scopeLevel = symTab.currentScopeLevel; tempAddrSym.isGlobal = false;
                symTab.addSymbol(tempAddrSym); // Add temp to current scope
                irGen.addQuad("ADD_OFFSET", nameForIRTarget, offsetBytesTemp, effectiveAddressTemp);
                
                irGen.addQuad("STORE_TO_ADDR", effectiveAddressTemp, "_", std::to_string(symInTable->arrayValues[i]));
            }
        } else {
            // Initialize local constant scalar on stack
            irGen.addQuad("ASSIGN", std::to_string(symInTable->value), "_", nameForIRTarget);
        }
    }

    recordSyntaxOutput("<ConstDef>");
}

// ConstInitVal → ConstExp | '{' [ ConstInitVal { ',' ConstInitVal } ] '}'
// For single const: returns evaluated string. For array: fills sym, generates IR for elements.
std::string SyntaxAnalyzer::parseConstInitVal(Symbol& constSym, bool parsingArrayElement, int currentDimension) {
    if (currentToken.type == LBRACE) {
        if (!constSym.isArray && !parsingArrayElement) { // e.g. const int x = {1}; Error
            semanticError("Aggregate initializer for non-array const '" + constSym.name + "'.");
            // Attempt to consume the erroneous block
        match(LBRACE);
            while(currentToken.type != RBRACE && currentToken.type != TOKEN_EOF) {
                if (currentToken.type == LBRACE) parseConstInitVal(constSym, true, currentDimension +1); 
                else getNextTokenFromLexer();
                if (currentToken.type == COMMA) match(COMMA);
                else break; 
            }
            if (currentToken.type == RBRACE) match(RBRACE);
            return "{error_array_init}";
        }

        if (constSym.isArray && parsingArrayElement && currentDimension >= constSym.arrayDimensions.size()) {
            semanticError("Too many nested initializers for const array '" + constSym.name + "'. Declared dimensions: " + std::to_string(constSym.arrayDimensions.size()) + ", current nesting: " + std::to_string(currentDimension + 1));
            match(LBRACE);
             while(currentToken.type != RBRACE && currentToken.type != TOKEN_EOF) {
                if (currentToken.type == LBRACE) parseConstInitVal(constSym, true, currentDimension +1);
                else getNextTokenFromLexer();
                if (currentToken.type == COMMA) match(COMMA);
                 else break;
            }
            if (currentToken.type == RBRACE) match(RBRACE);
            return "{error_array_init}";
        }

        match(LBRACE);
        // recordSyntaxOutput("{"); // Optional: for detailed syntax trace

        int elementCount = 0;
        if (currentToken.type != RBRACE) {
            parseConstInitVal(constSym, true, currentDimension + 1);
            elementCount++;
            while (currentToken.type == COMMA) {
                match(COMMA);
                // recordSyntaxOutput(","); // Optional
                parseConstInitVal(constSym, true, currentDimension + 1);
                elementCount++;
            }
        }
        match(RBRACE);
        // recordSyntaxOutput("}"); // Optional

        if (constSym.isArray && !constSym.arrayDimensions.empty() && currentDimension < constSym.arrayDimensions.size()) {
             if (constSym.arrayDimensions[currentDimension] > 0 && elementCount != constSym.arrayDimensions[currentDimension]) {
                 // semanticError("Incorrect number of initializers for dimension " + std::to_string(currentDimension + 1) +
                 //               " of const array '" + constSym.name + "'. Expected " +
                 //               std::to_string(constSym.arrayDimensions[currentDimension]) + " got " + std::to_string(elementCount) + ".");
                 // SysY judge might be lenient or expect zero-filling implicitly for consts.
             }
        }

        recordSyntaxOutput("<ConstInitVal>");
        return "{array_init}";
    } else { // This is for ConstExp (a single value)
        if (constSym.isArray && !parsingArrayElement) {
            semanticError("Scalar initializer used for array const '" + constSym.name + "'. Use { } for initialization.");
        }
        if (constSym.isArray && parsingArrayElement && currentDimension != constSym.arrayDimensions.size()) {
             semanticError("Missing braces for initializing const array '" + constSym.name + "'. Expected element at dimension depth " + std::to_string(constSym.arrayDimensions.size()) + ", but at depth " + std::to_string(currentDimension) + ".");
        }

        std::string valStr = parseConstExp();
        try {
            if (constSym.isArray) {
                 if (currentDimension == constSym.arrayDimensions.size()) { 
                    constSym.arrayValues.push_back(std::stoi(valStr));
                 } 
            } else { 
                constSym.value = std::stoi(valStr);
            }
        }
        catch (const std::exception& e) {
            semanticError("Invalid constant value for '" + constSym.name + "': " + valStr + ". Error: " + e.what());
        }

        recordSyntaxOutput("<ConstInitVal>");
        return valStr;
    }
}

// VarDecl → BType VarDef { ',' VarDef } ';'
void SyntaxAnalyzer::parseVarDecl() {
    std::string baseType = parseBType();
    parseVarDef(baseType);
    while (currentToken.type == COMMA) {
        match(COMMA);
        parseVarDef(baseType);
    }
    match(SEMICN);
    recordSyntaxOutput("<VarDecl>");
}

// VarDef → Ident { '[' ConstExp ']' } [ '=' InitVal ]
void SyntaxAnalyzer::parseVarDef(const std::string& varBaseType) {
    std::string name = currentToken.lexeme;
    match(IDENFR);

    Symbol sym;
    sym.name = name;
    sym.isConstant = false;
    sym.isGlobal = (symTab.currentScopeLevel == 1);
    sym.scopeLevel = symTab.currentScopeLevel;
    sym.isInitialized = false;

    bool isArray = false;
    std::vector<int> dimensions;
    if (currentToken.type == LBRACK) {
        isArray = true;
        match(LBRACK);
        std::string dim1Str = parseConstExp();
        try {
            dimensions.push_back(std::stoi(dim1Str));
        }
        catch(...) { semanticError("Array dimension 1 for variable '" + name + "' must be a constant integer."); }
        match(RBRACK);

        if (currentToken.type == LBRACK) {
            match(LBRACK);
            std::string dim2Str = parseConstExp();
            try {
                dimensions.push_back(std::stoi(dim2Str));
            }
            catch(...) { semanticError("Array dimension 2 for variable '" + name + "' must be a constant integer."); }
            match(RBRACK);
        }
    }
    sym.isArray = isArray;
    sym.arrayDimensions = dimensions;
    if (isArray) {
        if (dimensions.size() > 1) {
            sym.type = "array2d_" + varBaseType;
        } else {
            sym.type = "array_" + varBaseType;
        }
    } else {
        sym.type = varBaseType;
    }

    if (!symTab.addSymbol(sym)) {
        semanticError("Redefinition of variable symbol '" + sym.name + "'.");
    }
    Symbol* symInTable = symTab.lookupSymbol(name);
    if (!symInTable) { semanticError("Internal: Failed to find symbol " + name + " after declaration."); return; }


    if (currentToken.type == ASSIGN) {
        match(ASSIGN);
        int elementsInitializedCount = 0; 
        std::string initValResult = parseInitVal(*symInTable, 0, elementsInitializedCount);

        if (!symInTable->isArray) {
            std::string targetNameForAssign = this->getMangledNameForIR(symInTable);
            irGen.addQuad("ASSIGN", initValResult, "_", targetNameForAssign);
        } else {
             // After parseInitVal, elementsInitializedCount holds the number of elements
             // that received a value (either explicit or padded with zero by parseInitVal).
             // Now, zero-fill the rest of the array if it wasn't fully covered.
            int totalArraySize = symInTable->getTotalSize();
            if (elementsInitializedCount < totalArraySize) {
                std::string nameForIRBase = this->getMangledNameForIR(symInTable);
                std::string zeroVal = "0";
                for (int i = elementsInitializedCount; i < totalArraySize; ++i) {
                    std::string indexStr = std::to_string(i);
                    std::string elementSize = "4";
                    std::string offsetBytesTemp = irGen.newTemp();
                    Symbol tempOffsetS; tempOffsetS.name = offsetBytesTemp; tempOffsetS.type = "int"; tempOffsetS.scopeLevel = symTab.currentScopeLevel; tempOffsetS.isGlobal = (symTab.currentScopeLevel == 1); symTab.addSymbol(tempOffsetS);
                    irGen.addQuad("MUL", indexStr, elementSize, offsetBytesTemp);

                    std::string effectiveAddressTemp = irGen.newTemp();
                    Symbol tempAddrS; tempAddrS.name = effectiveAddressTemp; tempAddrS.type = "int_addr"; tempAddrS.scopeLevel = symTab.currentScopeLevel; tempAddrS.isGlobal = (symTab.currentScopeLevel == 1); symTab.addSymbol(tempAddrS);
                    irGen.addQuad("ADD_OFFSET", nameForIRBase, offsetBytesTemp, effectiveAddressTemp);
                    irGen.addQuad("STORE_TO_ADDR", effectiveAddressTemp, "_", zeroVal);
                }
            }
        }
        symInTable->isInitialized = true;
    } else {
        // No initializer assignment.
        // If it's a global array, it's zero-initialized by .data section.
        // If it's a local array and language requires default zeroing for *all* local arrays,
        // then code would be needed here to zero-fill symInTable->getTotalSize() elements.
        // SysY typically doesn't do this; uninitialized local arrays have undefined values.
        // However, a common behavior even for SysY-like subsets is that if *any* part of an array
        // is initialized with an initializer list, the rest of it *is* zero-filled.
        // The logic above under 'ASSIGN' handles this. If there's no '=', no elements are zero-filled here for locals.
    }
    recordSyntaxOutput("<VarDef>");
}

// InitVal → Exp | '{' [ InitVal { ',' InitVal } ] '}'
// Returns:
// - For Exp: a temp variable or constant string from parseExp.
// - For aggregate: "{array_init}" and should ideally generate element-wise IR internally.
std::string SyntaxAnalyzer::parseInitVal(Symbol& varSym, int currentDimension, int& elementsInitializedSoFar) {
    if (currentToken.type == LBRACE) {
        if (!varSym.isArray) {
            semanticError("Aggregate initializer for non-array variable '" + varSym.name + "'.");
            // Attempt to consume the erroneous block to allow parsing to continue for other errors
            match(LBRACE);
            while(currentToken.type != RBRACE && currentToken.type != TOKEN_EOF) {
                if (currentToken.type == LBRACE) parseInitVal(varSym, currentDimension + 1, elementsInitializedSoFar); 
                else getNextTokenFromLexer();
                if (currentToken.type == COMMA) match(COMMA);
                else break;
            }
            if (currentToken.type == RBRACE) match(RBRACE);
            return "{error_array_init}";
        }
        if (varSym.isArray && currentDimension >= varSym.arrayDimensions.size()) {
             if (varSym.arrayDimensions.empty() || currentDimension > varSym.arrayDimensions.size()) { // Corrected logic slightly
                semanticError("Too many nested initializers for array '" + varSym.name + "'. Declared dimensions: " + 
                              std::to_string(varSym.arrayDimensions.size()) + ", current nesting depth attempting: " + std::to_string(currentDimension + 1));
                match(LBRACE);
                while(currentToken.type != RBRACE && currentToken.type != TOKEN_EOF) {
                    if (currentToken.type == LBRACE) parseInitVal(varSym, currentDimension + 1, elementsInitializedSoFar);
                    else getNextTokenFromLexer();
                    if (currentToken.type == COMMA) match(COMMA);
                    else break;
                }
                if (currentToken.type == RBRACE) match(RBRACE);
                return "{error_array_init}";
            }
        }

        match(LBRACE);
        
        int elementsInThisDimensionProvided = 0;
        if (currentToken.type != RBRACE) {
            parseInitVal(varSym, currentDimension + 1, elementsInitializedSoFar);
            elementsInThisDimensionProvided++;
            while (currentToken.type == COMMA) {
                match(COMMA);
                parseInitVal(varSym, currentDimension + 1, elementsInitializedSoFar);
                elementsInThisDimensionProvided++;
            }
        }
        match(RBRACE);

        if (varSym.isArray && currentDimension < varSym.arrayDimensions.size()) {
            int expectedItemsInThisDimension = varSym.arrayDimensions[currentDimension];
            
            // If this {}-block provided fewer items than expected for this dimension (or was empty),
            // we need to "zero-fill" the remaining part of this dimension's span.
            if (elementsInThisDimensionProvided < expectedItemsInThisDimension) {
                int numMissingSubStructures = expectedItemsInThisDimension - elementsInThisDimensionProvided;
                
                int sizeOfOneSubStructure = 1; // Size in flat elements that each sub-structure here represents
                for (size_t k = currentDimension + 1; k < varSym.arrayDimensions.size(); ++k) {
                    sizeOfOneSubStructure *= varSym.arrayDimensions[k];
                }
                
                int numFlatElementsToZeroFill = numMissingSubStructures * sizeOfOneSubStructure;

                std::string nameForIRBase = this->getMangledNameForIR(&varSym);
                std::string zeroVal = "0"; 

                for (int i = 0; i < numFlatElementsToZeroFill; ++i) {
                    if (elementsInitializedSoFar >= varSym.getTotalSize()) {
                        // semanticError("Over-initializing array '" + varSym.name + "' during zero-padding for dimension " + std::to_string(currentDimension) + ".");
                        break; 
                    }
                    
                    std::string indexStr = std::to_string(elementsInitializedSoFar);
                    std::string elementSize = "4"; 
                    std::string offsetBytesTemp = irGen.newTemp();
                    Symbol tempOffsetS; tempOffsetS.name = offsetBytesTemp; tempOffsetS.type = "int"; tempOffsetS.scopeLevel = symTab.currentScopeLevel; tempOffsetS.isGlobal = (symTab.currentScopeLevel == 1); symTab.addSymbol(tempOffsetS);
                    irGen.addQuad("MUL", indexStr, elementSize, offsetBytesTemp);

                    std::string effectiveAddressTemp = irGen.newTemp();
                    Symbol tempAddrS; tempAddrS.name = effectiveAddressTemp; tempAddrS.type = "int_addr"; tempAddrS.scopeLevel = symTab.currentScopeLevel; tempAddrS.isGlobal = (symTab.currentScopeLevel == 1); symTab.addSymbol(tempAddrS);
                    irGen.addQuad("ADD_OFFSET", nameForIRBase, offsetBytesTemp, effectiveAddressTemp);
                    irGen.addQuad("STORE_TO_ADDR", effectiveAddressTemp, "_", zeroVal);
                    
                    elementsInitializedSoFar++;
                }
            }
        }
        recordSyntaxOutput("<InitVal>");
        return "{array_init}";
    }
    // This part handles individual expressions (e.g. int x = 5; or int arr[] = {1,2,3};)
    // or the elements within a deeper level of brace initialization.
    if (varSym.isArray && elementsInitializedSoFar >= varSym.getTotalSize()) {
        semanticError("Too many initializers for array '" + varSym.name + "'. Total capacity: " + std::to_string(varSym.getTotalSize()) + ".");
        parseExp(varSym.getBaseType()); 
        return "{error_too_many_elements}";
    }
    
    std::string expectedElementType = varSym.getBaseType(); 
    std::string elemVal = parseExp(expectedElementType);

    if (varSym.isArray) {
        std::string nameForIRBase = this->getMangledNameForIR(&varSym);
        std::string indexStr = std::to_string(elementsInitializedSoFar);
        std::string elementSize = "4"; 
        std::string offsetBytesTemp = irGen.newTemp();
        Symbol tempOffsetSym; tempOffsetSym.name = offsetBytesTemp; tempOffsetSym.type = "int"; tempOffsetSym.scopeLevel = symTab.currentScopeLevel; tempOffsetSym.isGlobal = (symTab.currentScopeLevel == 1); symTab.addSymbol(tempOffsetSym);
        irGen.addQuad("MUL", indexStr, elementSize, offsetBytesTemp);

        std::string effectiveAddressTemp = irGen.newTemp();
        Symbol tempAddrSym; tempAddrSym.name = effectiveAddressTemp; tempAddrSym.type = "int_addr"; tempAddrSym.scopeLevel = symTab.currentScopeLevel; tempAddrSym.isGlobal = (symTab.currentScopeLevel == 1); symTab.addSymbol(tempAddrSym);
        irGen.addQuad("ADD_OFFSET", nameForIRBase, offsetBytesTemp, effectiveAddressTemp);
        irGen.addQuad("STORE_TO_ADDR", effectiveAddressTemp, "_", elemVal);
        elementsInitializedSoFar++;
    }
    recordSyntaxOutput("<InitVal>");
    return elemVal; 
}

// FuncDef → FuncType Ident '(' [FuncFParams] ')' Block
void SyntaxAnalyzer::parseFuncDef() {
    std::string funcRetType = parseFuncType();
    std::string funcName = currentToken.lexeme;
    match(IDENFR);

    Symbol funcSymbolEntry;
    funcSymbolEntry.name = funcName;
    funcSymbolEntry.returnType = funcRetType;
    funcSymbolEntry.type = (funcRetType == "int" ? "int_func" : "void_func");
    funcSymbolEntry.isGlobal = true;
    funcSymbolEntry.scopeLevel = symTab.currentScopeLevel; // Should be global scope (1)
    if(symTab.currentScopeLevel != 1) semanticError("Function '" + funcName + "' not defined at global scope.");

    match(LPARENT);

    Symbol funcShell = funcSymbolEntry; // Create a shell for early addition to symbol table (for recursion)
    funcShell.params.clear(); // Params will be added by parseFuncFParams

    if (symTab.tableStack.front().count(funcName)) { // Check global scope
        semanticError("Redefinition of function symbol '" + funcName + "'.");
    }
    symTab.tableStack.front()[funcName] = funcShell; // Add shell to global scope

    currentFunctionSym = symTab.lookupSymbol(funcName); // Get the persistent symbol from table
    if (!currentFunctionSym) {
        semanticError("Internal: Failed to retrieve function shell '" + funcName + "' from global symbol table.");
    }

    symTab.enterScope(); // Enter function's parameter and body scope
    symTab.resetFrameOffsetForNewFunction(); // Reset for local variables and params of this function

    symTab.beginFunctionCompilation(funcName); // Start accumulating symbols for this function

    irGen.addQuad("FUNC_BEGIN", funcName, "PLACEHOLDER_STACK_SIZE", "_"); // Stack size updated later

    if (currentToken.type != RPARENT) {
        parseFuncFParams(*currentFunctionSym); // Pass the symbol from table to populate its params
    }
    match(RPARENT);

    parseBlock(false); // Function block does not create its own new scope on top of param scope

    int localFrameSize = symTab.getCurrentFrameSizeForLocals();
    bool foundFuncBegin = false;
    for(auto& quad : irGen.quadruples) { // Update placeholder stack size
        if(quad.op == "FUNC_BEGIN" && quad.arg1 == funcName) {
            quad.arg2 = std::to_string(localFrameSize);
            foundFuncBegin = true;
            break;
        }
    }
    if (!foundFuncBegin) semanticError("Internal: FUNC_BEGIN quad not found for " + funcName);

    symTab.endFunctionCompilation(funcName); // Finalize symbol accumulation

    symTab.exitScope(); // Exit function scope
    irGen.addQuad("FUNC_END", funcName, "_", "_");
    currentFunctionSym = nullptr; // Clear current function context
    recordSyntaxOutput("<FuncDef>");
}

// MainFuncDef → 'int' 'main' '(' ')' Block
void SyntaxAnalyzer::parseMainFuncDef() {
    match(INTTK);
    match(MAINTK);

    Symbol mainFuncSymbol;
    mainFuncSymbol.name = "main";
    mainFuncSymbol.type = "int_func";
    mainFuncSymbol.returnType = "int";
    mainFuncSymbol.isGlobal = true;
    mainFuncSymbol.scopeLevel = symTab.currentScopeLevel; // Should be global scope (1)
    if(symTab.currentScopeLevel != 1) semanticError("'main' function not defined at global scope.");

    // Add to global symbol table immediately
    if (symTab.tableStack.front().count("main")) {
        semanticError("Redefinition of 'main' function.");
    }
    symTab.tableStack.front()["main"] = mainFuncSymbol; // Add to global scope
    currentFunctionSym = symTab.lookupSymbol("main"); // Get the persistent symbol
     if (!currentFunctionSym) semanticError("Internal: Failed to add/find main function symbol.");

    symTab.enterScope(); // Enter main's body scope
    symTab.resetFrameOffsetForNewFunction();

    symTab.beginFunctionCompilation("main"); // Start accumulating for main

    irGen.addQuad("FUNC_BEGIN", "main", "PLACEHOLDER_STACK_SIZE", "_"); // Stack size updated later

    match(LPARENT);
    match(RPARENT);
    parseBlock(false); // Main's block

    int localFrameSize = symTab.getCurrentFrameSizeForLocals();
    bool foundFuncBegin = false;
    for(auto& quad : irGen.quadruples) { // Update placeholder
        if(quad.op == "FUNC_BEGIN" && quad.arg1 == "main") {
            quad.arg2 = std::to_string(localFrameSize);
            foundFuncBegin = true;
            break;
        }
    }
    if (!foundFuncBegin) semanticError("Internal: FUNC_BEGIN quad not found for main");

    symTab.endFunctionCompilation("main"); // Finalize symbol accumulation

    symTab.exitScope(); // Exit main's scope
    irGen.addQuad("FUNC_END", "main", "_", "_");
    currentFunctionSym = nullptr; // Clear current function context
    recordSyntaxOutput("<MainFuncDef>");
}

// FuncType → 'void' | 'int'
std::string SyntaxAnalyzer::parseFuncType() {
    std::string typeStr;
    if (currentToken.type == VOIDTK) {
        match(VOIDTK); typeStr = "void";
    } else if (currentToken.type == INTTK) {
        match(INTTK); typeStr = "int";
    } else {
        semanticError("Invalid function return type. Expected 'void' or 'int'.");
    }
    recordSyntaxOutput("<FuncType>");
    return typeStr;
}

// FuncFParams → FuncFParam { ',' FuncFParam }
void SyntaxAnalyzer::parseFuncFParams(Symbol& funcSymbol) { // Modifies funcSymbol.params
    int paramIndex = 0; // For assigning offsets and tracking for $a0-$a3 vs stack
    parseFuncFParam(funcSymbol, paramIndex++);
    while (currentToken.type == COMMA) {
        match(COMMA);
        parseFuncFParam(funcSymbol, paramIndex++);
    }
    recordSyntaxOutput("<FuncFParams>");
}

// FuncFParam → BType Ident [ '[' ']' { '[' ConstExp ']' } ]
// According to user's rule: FuncFParam → BType Ident (only normal variables)
void SyntaxAnalyzer::parseFuncFParam(Symbol& funcSymbol, int paramIndex) {
    std::string paramBaseType = parseBType();
    std::string paramName = currentToken.lexeme;
    match(IDENFR);

    Symbol paramSymbolEntry;
    paramSymbolEntry.name = paramName;
    paramSymbolEntry.scopeLevel = symTab.currentScopeLevel; // Current scope is function's scope
    paramSymbolEntry.isGlobal = false;
    paramSymbolEntry.isConstant = false; // Parameters are not const by default in SysY
    paramSymbolEntry.type = paramBaseType;
    paramSymbolEntry.isArray = false; // Strictly per user rule: FuncFParam -> BType Ident

    // Add to function's own signature list (stored in its Symbol object)
    funcSymbol.params.push_back({paramName, paramSymbolEntry.type});
    // Add parameter to current (function's) symbol table scope
    // The offset for parameters passed in registers will be managed by saving them to stack in FUNC_BEGIN if needed for memory access.
    // The offset here is for accessing them as if they are on stack relative to $fp.
    // $fp points to saved old $fp. Saved $ra is at $fp-4. First stack-passed arg would be $fp+4.
    paramSymbolEntry.offset = 4 + paramIndex * 4; // Adjusted offset

    if (!symTab.addParamSymbol(paramSymbolEntry, paramIndex)) {
        semanticError("Redefinition of parameter '" + paramName + "' in function '" + funcSymbol.name + "'.");
    }
    recordSyntaxOutput("<FuncFParam>");
}

// Block → '{' { BlockItem } '}'
void SyntaxAnalyzer::parseBlock(bool isNewScope) {
    match(LBRACE);
    if (isNewScope) symTab.enterScope();

    while (currentToken.type != RBRACE) {
        parseBlockItem();
    }
    match(RBRACE);
    if (isNewScope) symTab.exitScope(); // Exit the scope if it was entered by this block
    recordSyntaxOutput("<Block>");
}

// BlockItem → Decl | Stmt
void SyntaxAnalyzer::parseBlockItem() {
    if (currentToken.type == CONSTTK || currentToken.type == INTTK) {
        // Note: FuncDef is not allowed inside a BlockItem by typical SysY grammar.
        // If INTTK is seen, it must be a VarDecl.
        parseDecl();
    } else {
        parseStmt();
    }
    recordSyntaxOutput("<BlockItem>");
}

// Stmt → LVal '=' Exp ';' | [Exp] ';' | Block | 'if' '(' Cond ')' Stmt [ 'else' Stmt ] |
// 'while' '(' Cond ')' Stmt | 'break' ';' | 'continue' ';' | 'return' [Exp] ';' |
// LVal '=' 'getint' '(' ')' ';' | 'printf' '(' FormatString {',' Exp} ')' ';'
void SyntaxAnalyzer::parseStmt() {
    if (currentToken.type == IFTK) {
        match(IFTK);
        match(LPARENT);
        std::string condResult = parseCond();
        match(RPARENT);

        std::string elseLabel = irGen.newLabel();
        std::string endIfLabel = irGen.newLabel();

        irGen.addQuad("IF_FALSE_GOTO", condResult, "_", elseLabel); // Jump if condition is false

        parseStmt(); // Then-branch

        if (currentToken.type == ELSETK) {
            match(ELSETK);
            irGen.addQuad("GOTO", "_", "_", endIfLabel); // Skip else-branch if then-branch executed

            irGen.addQuad(elseLabel + ":", "_", "_", "_"); // Label for else-branch
            parseStmt(); // Else-branch

            irGen.addQuad(endIfLabel + ":", "_", "_", "_"); // Label after if-else
        } else {
            irGen.addQuad(elseLabel + ":", "_", "_", "_"); // Label after if (no else)
        }
        recordSyntaxOutput("<Stmt_If>");
    } else if (currentToken.type == WHILETK) {
        std::string startLoopLabel = irGen.newLabel(); // Label for condition check
        std::string endLoopLabel = irGen.newLabel();   // Label after loop

        loopLabels.push({startLoopLabel, endLoopLabel}); // For break/continue

        irGen.addQuad(startLoopLabel + ":", "_", "_", "_"); // Mark start of loop (for condition)

        match(WHILETK);
        match(LPARENT);
        std::string condResult = parseCond();
        match(RPARENT);

        irGen.addQuad("IF_FALSE_GOTO", condResult, "_", endLoopLabel); // Exit loop if condition false

        parseStmt(); // Loop body

        irGen.addQuad("GOTO", "_", "_", startLoopLabel); // Jump back to condition check

        irGen.addQuad(endLoopLabel + ":", "_", "_", "_"); // Mark end of loop

        loopLabels.pop();
        recordSyntaxOutput("<Stmt_While>");
    } else if (currentToken.type == BREAKTK) {
        match(BREAKTK);
        if (loopLabels.empty()) {
            semanticError("Break statement outside of a loop.");
        } else {
            irGen.addQuad("GOTO", "_", "_", loopLabels.top().second); // Jump to endLoopLabel
        }
        match(SEMICN);
        recordSyntaxOutput("<Stmt_Break>");
    } else if (currentToken.type == CONTINUETK) {
        match(CONTINUETK);
        if (loopLabels.empty()) {
            semanticError("Continue statement outside of a loop.");
        } else {
            irGen.addQuad("GOTO", "_", "_", loopLabels.top().first); // Jump to startLoopLabel (condition)
        }
        match(SEMICN);
        recordSyntaxOutput("<Stmt_Continue>");
    } else if (currentToken.type == RETURNTK) {
        match(RETURNTK);
        std::string returnValue = "_"; // Default for void or if no expression

        if (!currentFunctionSym) {
            semanticError("Return statement outside of a function.");
        }

        if (currentToken.type != SEMICN) { // If there is an expression
            std::string expectedReturnType = "int";
            if (currentFunctionSym && !currentFunctionSym->returnType.empty()) {
                expectedReturnType = currentFunctionSym->returnType;
            }

            if (currentFunctionSym && currentFunctionSym->returnType == "void") {
                semanticError("Return with a value in a void function \'" + currentFunctionSym->name + "\'.");
                parseExp(expectedReturnType);
            } else {
                returnValue = parseExp(expectedReturnType);
            }
        } else { // No expression
            if (currentFunctionSym && currentFunctionSym->returnType != "void") {
                semanticError("Return without a value in non-void function \'" + currentFunctionSym->name + "\'. Expected " + currentFunctionSym->returnType);
            }
        }

        if (currentFunctionSym && currentFunctionSym->returnType != "void") {
            irGen.addQuad("RETURN_VAL", returnValue, "_", "_");
        } else {
            // Also covers cases where currentFunctionSym might be null due to prior error,
            // but we still need a return quad if syntax allows.
            irGen.addQuad("RETURN_VOID", "_", "_", "_");
        }
        match(SEMICN);
        recordSyntaxOutput("<Stmt_Return>");
    } else if (currentToken.type == PRINTFTK) {
        match(PRINTFTK);
        match(LPARENT);

        std::string fmtStrLexemeWithQuotes = currentToken.lexeme;
        match(STRCON);

        std::vector<std::string> argTemps;
        while (currentToken.type == COMMA) {
            match(COMMA);
            argTemps.push_back(parseExp("int")); // Assuming printf args are int
        }

        std::string formatStringNoQuotes = fmtStrLexemeWithQuotes.substr(1, fmtStrLexemeWithQuotes.length() - 2);

        size_t currentPos = 0;
        int argIdx = 0;

        while (currentPos < formatStringNoQuotes.length()) {
            size_t nextPercentD = formatStringNoQuotes.find("%d", currentPos);

            if (nextPercentD != std::string::npos) {
                if (nextPercentD > currentPos) {
                    std::string textSegment = formatStringNoQuotes.substr(currentPos, nextPercentD - currentPos);
                    irGen.addQuad("PRINT_STR", symTab.addStringLiteral("\"" + textSegment + "\""), "_", "_");
                }

                if (argIdx < argTemps.size()) {
                    irGen.addQuad("PRINT_INT", argTemps[argIdx], "_", "_");
                    argIdx++;
                } else {
                    // Error: Not enough arguments for %d specifiers.
                    // Print "%d" literally if no argument is left.
                    irGen.addQuad("PRINT_STR", symTab.addStringLiteral("\"%d\""), "_", "_");
                }
                currentPos = nextPercentD + 2;
            } else {
                if (currentPos < formatStringNoQuotes.length()) {
                    std::string remainingSegment = formatStringNoQuotes.substr(currentPos);
                    irGen.addQuad("PRINT_STR", symTab.addStringLiteral("\"" + remainingSegment + "\""), "_", "_");
                }
                break;
            }
        }
        // Check if there are more arguments supplied than %d specifiers (usually ignored by C printf)
        if (argIdx < argTemps.size()) {
            // Potentially issue a warning about unused arguments
        }

        match(RPARENT);
        match(SEMICN);
        recordSyntaxOutput("<Stmt_Printf>");
    } else if (currentToken.type == LBRACE) {
        parseBlock(true);
    } else if (currentToken.type == SEMICN) {
        match(SEMICN);
    } else if (currentToken.type == IDENFR) {
        const auto& allTokens = lexer.getTokens();
        Token peekNext = (tokenIndex < allTokens.size()) ? allTokens[tokenIndex] : Token{TOKEN_EOF, "", 0};

        if (peekNext.type == LPARENT) { // Likely a function call
            // Parse as an expression. parseUnaryExp will handle the function call.
            // The result of the expression (if any) is discarded for a statement.
            parseExp("int"); // Or "void" if function calls can be expressions of type void
            match(SEMICN);
        } else {
            // Could be LVal = Exp; or LVal = getint();
            // Fall back to LVal-first speculative parsing.
            int preLValPos = tokenIndex -1;

            bool isArrayAccessLVal;
            std::string arrayIndexValueTemp_unused;

            std::string lvalTarget = parseLVal(isArrayAccessLVal, arrayIndexValueTemp_unused, true /*isAssignmentLHS=true*/);

            if (currentToken.type == ASSIGN) {
                match(ASSIGN);
                if (currentToken.type == GETINTTK) {
                    match(GETINTTK);
                    match(LPARENT);
                    match(RPARENT);

                    std::string getintResultTemp = irGen.newTemp();
                    Symbol tempSymbolGetint;
                    tempSymbolGetint.name = getintResultTemp;
                    tempSymbolGetint.type = "int";
                    tempSymbolGetint.isConstant = false;
                    tempSymbolGetint.isArray = false;
                    tempSymbolGetint.scopeLevel = symTab.currentScopeLevel;
                    tempSymbolGetint.isGlobal = (symTab.currentScopeLevel == 1);
                    if (!symTab.addSymbol(tempSymbolGetint)) {
                         semanticError("Failed to add temporary variable " + getintResultTemp + " for getint result to symbol table.");
                    }
                    irGen.addQuad("GET_INT", "_", "_", getintResultTemp);

                    if(isArrayAccessLVal) { // Assignment to array element
                        irGen.addQuad("STORE_TO_ADDR", lvalTarget /*address_temp*/, "_", getintResultTemp /*value_temp*/);
                    } else { // Assignment to scalar
                        irGen.addQuad("ASSIGN", getintResultTemp, "_", lvalTarget);
                    }
                } else { // Regular Exp assignment: LVal = Exp;
                    std::string rhsVal = parseExp("int"); // Assuming RHS is int
                    if (isArrayAccessLVal) { // Assignment to array element
                        irGen.addQuad("STORE_TO_ADDR", lvalTarget /*address_temp*/, "_", rhsVal /*value_temp*/);
                    } else { // Assignment to scalar
                        irGen.addQuad("ASSIGN", rhsVal, "_", lvalTarget);
                    }
                }
                match(SEMICN);
            } else { // Not an assignment, so it must have been an Exp (which might start with an LVal)
                // Reset token stream to before LVal was parsed and parse as full Exp.
                tokenIndex = preLValPos;
                getNextTokenFromLexer(); // Re-fetch the IDENFR as currentToken

                if (currentToken.type != SEMICN) {
                     parseExp("int"); // Parse the expression, result is discarded
                }
                match(SEMICN);
            }
        }
    } else {
             // Could be an expression starting with '(', or a number, etc. or just ';'
        if (currentToken.type != SEMICN) {
             parseExp("int"); // Result is unused for statement Exp.
        }
        match(SEMICN); // All valid statements (or expression statements) end with ';'
    }
    recordSyntaxOutput("<Stmt>"); // Generic Stmt tag
}

// Exp → AddExp
std::string SyntaxAnalyzer::parseExp(const std::string& expectedType) {
    std::string result = parseAddExp(expectedType);
    recordSyntaxOutput("<Exp>");
    return result;
}

// AddExp → MulExp { ('+' | '-') MulExp }
std::string SyntaxAnalyzer::parseAddExp(const std::string& expectedType) {
    std::string leftOperandStr = parseMulExp(expectedType);

    while (currentToken.type == PLUS || currentToken.type == MINU) {
        TokenType opToken = currentToken.type;
        match(opToken);
        std::string rightOperandStr = parseMulExp(expectedType);

        int constLeftVal, constRightVal;
        bool isConstLeft = tryResolveToConstInt(leftOperandStr, constLeftVal);
        bool isConstRight = tryResolveToConstInt(rightOperandStr, constRightVal);

        if (isConstLeft && isConstRight) {
            if (opToken == PLUS) {
                leftOperandStr = std::to_string(constLeftVal + constRightVal);
            } else { // MINU
                leftOperandStr = std::to_string(constLeftVal - constRightVal);
            }
            recordSyntaxOutput("<AddExp>");
            continue;
        }

        std::string resultTempName = irGen.newTemp();
        Symbol tempSymbol;
        tempSymbol.name = resultTempName;
        tempSymbol.type = "int";
        tempSymbol.isConstant = false;
        tempSymbol.isArray = false;
        tempSymbol.scopeLevel = symTab.currentScopeLevel;
        tempSymbol.isGlobal = (symTab.currentScopeLevel == 1);
        if (!symTab.addSymbol(tempSymbol)) {
            semanticError("Failed to add temporary variable " + resultTempName + " to symbol table.");
        }

        irGen.addQuad(opToken == PLUS ? "ADD" : "SUB", leftOperandStr, rightOperandStr, resultTempName);
        leftOperandStr = resultTempName;

        recordSyntaxOutput("<AddExp>");
    }
    return leftOperandStr;
}

// MulExp → UnaryExp { ('*' | '/' | '%') UnaryExp }
std::string SyntaxAnalyzer::parseMulExp(const std::string& expectedType) {
    std::string leftOperandStr = parseUnaryExp();

    while (currentToken.type == MULT || currentToken.type == DIV || currentToken.type == MOD) {
        TokenType opToken = currentToken.type;
        match(opToken);
        std::string rightOperandStr = parseUnaryExp();

        int constLeftVal, constRightVal;
        bool isConstLeft = tryResolveToConstInt(leftOperandStr, constLeftVal);
        bool isConstRight = tryResolveToConstInt(rightOperandStr, constRightVal);

        if (isConstLeft && isConstRight) {
            if (opToken == MULT) {
                leftOperandStr = std::to_string(constLeftVal * constRightVal);
            } else if (opToken == DIV) {
                if (constRightVal == 0) semanticError("Division by zero in constant expression.");
                leftOperandStr = std::to_string(constLeftVal / constRightVal);
            } else { // MOD
                if (constRightVal == 0) semanticError("Modulo by zero in constant expression.");
                leftOperandStr = std::to_string(constLeftVal % constRightVal);
            }
            recordSyntaxOutput("<MulExp>");
            continue;
        }

        std::string resultTempName = irGen.newTemp();
        Symbol tempSymbol;
        tempSymbol.name = resultTempName;
        tempSymbol.type = "int";
        tempSymbol.isConstant = false;
        tempSymbol.isArray = false;
        tempSymbol.scopeLevel = symTab.currentScopeLevel;
        tempSymbol.isGlobal = (symTab.currentScopeLevel == 1);
        if (!symTab.addSymbol(tempSymbol)) {
            semanticError("Failed to add temporary variable " + resultTempName + " to symbol table.");
        }

        std::string opStr;
        if (opToken == MULT) opStr = "MUL";
        else if (opToken == DIV) opStr = "DIV";
        else opStr = "MOD";
        irGen.addQuad(opStr, leftOperandStr, rightOperandStr, resultTempName);
        leftOperandStr = resultTempName;

        recordSyntaxOutput("<MulExp>");
    }
    return leftOperandStr;
}

// UnaryExp → PrimaryExp | Ident '(' [FuncRParams] ')' | UnaryOp UnaryExp
std::string SyntaxAnalyzer::parseUnaryExp() {
    if (currentToken.type == PLUS || currentToken.type == MINU || currentToken.type == NOT) {
        std::string opIrName = parseUnaryOp();
        std::string operandStr = parseUnaryExp();

        if (opIrName == "POS") { 
            recordSyntaxOutput("<UnaryExp>");
            return operandStr;
        }

        int constOperandVal;
        bool isConstOperand = tryResolveToConstInt(operandStr, constOperandVal);

        if (isConstOperand) {
            if (opIrName == "NEG") {
                recordSyntaxOutput("<UnaryExp>");
                return std::to_string(-constOperandVal);
            } else if (opIrName == "NOT_OP") {
                recordSyntaxOutput("<UnaryExp>");
                return std::to_string(constOperandVal == 0 ? 1 : 0);
            }
        }

        std::string resultTempName = irGen.newTemp();
        Symbol tempSymbolUnary;
        tempSymbolUnary.name = resultTempName;
        tempSymbolUnary.type = "int";
        tempSymbolUnary.isConstant = false;
        tempSymbolUnary.isArray = false;
        tempSymbolUnary.scopeLevel = symTab.currentScopeLevel;
        tempSymbolUnary.isGlobal = (symTab.currentScopeLevel == 1);
        if (!symTab.addSymbol(tempSymbolUnary)) {
            semanticError("Failed to add temporary variable " + resultTempName + " to symbol table in UnaryOp.");
        }
        irGen.addQuad(opIrName, operandStr, "_", resultTempName);
        recordSyntaxOutput("<UnaryExp>");
        return resultTempName;
    } 
    // ADDED: Handle getint() as an expression
    else if (currentToken.type == GETINTTK) {
        match(GETINTTK);
        match(LPARENT);
        match(RPARENT);

        std::string resultTempName = irGen.newTemp();
        Symbol tempSymGetint;
        tempSymGetint.name = resultTempName;
        tempSymGetint.type = "int";
        tempSymGetint.isConstant = false;
        tempSymGetint.isArray = false;
        tempSymGetint.scopeLevel = symTab.currentScopeLevel;
        tempSymGetint.isGlobal = (symTab.currentScopeLevel == 1);
        if (!symTab.addSymbol(tempSymGetint)) {
            semanticError("Failed to add temporary variable " + resultTempName + " for getint result in UnaryExp.");
        }
        irGen.addQuad("GET_INT", "_", "_", resultTempName);
        recordSyntaxOutput("<UnaryExp>"); 
        return resultTempName;
    } 
    // Existing logic for IDENFR-based function calls or PrimaryExp
    // This part needs to correctly handle IDENFR LPARENT for function calls, 
    // and fall back to parsePrimaryExp otherwise.
    else if (currentToken.type == IDENFR) { 
        const auto& allTokens = lexer.getTokens(); 
        Token peekNext = Token{TOKEN_EOF,"",0}; 
        // Ensure tokenIndex is valid before accessing allTokens[tokenIndex]
        // The tokenIndex in SyntaxAnalyzer is the index of the *next* token to be consumed.
        // So currentToken is allTokens[tokenIndex-1]. The token to peek is allTokens[tokenIndex].
        if (tokenIndex < allTokens.size()) { 
             peekNext = allTokens[tokenIndex];
        }

        if (peekNext.type == LPARENT) { // Function call: IDENFR LPARENT ...
            std::string funcName = currentToken.lexeme;
            match(IDENFR); // Consume IDENFR

            Symbol* funcSym = symTab.lookupSymbol(funcName);
            if (!funcSym || (funcSym->type != "int_func" && funcSym->type != "void_func")) {
                semanticError("Identifier '" + funcName + "' is not a function or not declared.");
                return ""; 
            }

            match(LPARENT); // Consume LPARENT
            if (currentToken.type != RPARENT) {
                parseFuncRParams(funcSym);
            }
            match(RPARENT); // Consume RPARENT

            std::string funcCallResultTempName = ""; 
            if (funcSym->returnType == "int") { 
                funcCallResultTempName = irGen.newTemp();
                Symbol tempSymbolFuncRet;
                tempSymbolFuncRet.name = funcCallResultTempName;
                tempSymbolFuncRet.type = "int";
                tempSymbolFuncRet.isConstant = false;
                tempSymbolFuncRet.isArray = false;
                tempSymbolFuncRet.scopeLevel = symTab.currentScopeLevel;
                tempSymbolFuncRet.isGlobal = (symTab.currentScopeLevel == 1);
                if (!symTab.addSymbol(tempSymbolFuncRet)) {
                    semanticError("Failed to add temporary variable " + funcCallResultTempName + " for function return to symbol table.");
                }
            }
            irGen.addQuad("CALL", funcName, std::to_string(funcSym->params.size()), funcCallResultTempName);
            recordSyntaxOutput("<UnaryExp>"); 
            return funcCallResultTempName;
        } else { // Not a function call starting with IDENFR, so it must be a PrimaryExp (which could be an LVal/IDENFR itself)
            return parsePrimaryExp();
        }
    } else { // Not UnaryOp, not GETINTTK, not IDENFR -> must be other PrimaryExp types like ( Exp ) or Number
        return parsePrimaryExp();
    }
}

// PrimaryExp → '(' Exp ')' | LVal | Number
std::string SyntaxAnalyzer::parsePrimaryExp() {
    std::string result;
    if (currentToken.type == LPARENT) {
        match(LPARENT);
        result = parseExp("int");
        match(RPARENT);
    } else if (currentToken.type == IDENFR) {
        // Save original token for potential symbol lookup if not an array access that returns an address temp
        std::string originalIdentName = currentToken.lexeme;
        int originalIdentLine = currentToken.lineNumber;

        bool isArrayAccess = false;
        std::string arrayIndexValueTemp; // Stores the *value* of index from parseLVal

        // parseLVal returns:
        // - Mangled name for scalar variable
        // - Temp name holding address for array element
        std::string lvalResultName = parseLVal(isArrayAccess, arrayIndexValueTemp, false /*isAssignmentLHS=false*/);

        if (isArrayAccess) {
            // lvalResultName is a temporary variable holding the *address* of the array element.
            // We need to load the value from this address.
            std::string valueLoadedTemp = irGen.newTemp();
            Symbol tempSymbolVal;
            tempSymbolVal.name = valueLoadedTemp;
            tempSymbolVal.type = "int";
            tempSymbolVal.isConstant = false;
            tempSymbolVal.isArray = false;
            tempSymbolVal.scopeLevel = symTab.currentScopeLevel;
            tempSymbolVal.isGlobal = (symTab.currentScopeLevel == 1);
            if (!symTab.addSymbol(tempSymbolVal)) {
                semanticError("Failed to add temporary variable " + valueLoadedTemp + " for LOAD_FROM_ADDR to symbol table.");
            }

            irGen.addQuad("LOAD_FROM_ADDR", lvalResultName /*address*/, "_", valueLoadedTemp /*value*/);
            result = valueLoadedTemp; // The result of the primary expression is the loaded value
        } else {
            // lvalResultName is the (potentially mangled) name of a scalar variable or a global array base.
            // Need to look up the original symbol to check if it's a simple const scalar or an array base.
            Symbol* s = symTab.lookupSymbol(originalIdentName); // Use original name for symtab lookup
            if (!s) {
                // This should ideally be caught by parseLVal, but as a safeguard:
                semanticError("Undeclared identifier '" + originalIdentName + "' in expression (line " + std::to_string(originalIdentLine) + ").");
                 return "";
            }

            if (s->isConstant && !s->isArray) { // Constant scalar
                result = std::to_string(s->value);
            } else if (s->isArray) {
                // Using an array name directly in an expression (not as LVal for element access).
                // This typically means its base address. For IR, parseLVal already returned its (mangled) name.
                // MipsGenerator's getMipsOperand will handle `la` for global arrays or `addiu` for local array stack address.
                result = lvalResultName; // lvalResultName is already the correct (mangled) base name for IR.
            } else { // It's a simple (non-array, non-const) scalar variable
                result = lvalResultName; // lvalResultName is the (mangled) name for IR.
            }
        }
    } else if (currentToken.type == INTCON) {
        result = parseNumber();
    } else {
        semanticError("Invalid token at start of PrimaryExp: " + currentToken.lexeme);
    }
    recordSyntaxOutput("<PrimaryExp>");
    // The <UnaryExp> tag should be output by parseUnaryExp if PrimaryExp is its sole child.
    return result;
}

// UnaryOp → '+' | '-' | '!'
std::string SyntaxAnalyzer::parseUnaryOp() {
    std::string opIrName = "POS"; // Default for unary '+'
    if (currentToken.type == PLUS) {
        match(PLUS);
        opIrName = "POS";
    } else if (currentToken.type == MINU) {
        match(MINU);
        opIrName = "NEG";
    } else if (currentToken.type == NOT) {
        match(NOT);
        opIrName = "NOT_OP";
    } else {
        semanticError("Syntax error in UnaryOp: Expected '+', '-', or '!' got " + currentToken.lexeme);
    }
    recordSyntaxOutput("<UnaryOp>");
    return opIrName; // Return IR op name
}

// FuncRParams → Exp { ',' Exp }
void SyntaxAnalyzer::parseFuncRParams(Symbol* funcSym) {
    // Here, we need to generate PARAM quads and type check arguments.
    int paramCount = 0;
    if (!funcSym) { semanticError("Internal: funcSym is null in parseFuncRParams"); }

    std::string argResult = parseExp("int");
    // Type checking (example for int params)
    if (paramCount < funcSym->params.size()) {
        const auto& expectedParam = funcSym->params[paramCount];
        std::string argType = getExpType(argResult);
        // SysY is weakly typed for params often, but good to have a placeholder
        if ((expectedParam.second == "int" || expectedParam.second == "array_int" /* passed as pointer */) &&
            !(argType == "int" || argType == "const_int" || (argType.rfind("array_",0)==0 && expectedParam.second.rfind("array_",0)==0 ))) {
           // Type mismatch error could be reported here
        }
    } else if (!funcSym->params.empty()){ // check only if funcSym expects params
         semanticError("Too many arguments for function '" + funcSym->name + "'.");
    }
    irGen.addQuad("PARAM", argResult, "_", std::to_string(paramCount++));

    while (currentToken.type == COMMA) {
        match(COMMA);
        argResult = parseExp("int");

        if (paramCount < funcSym->params.size()) {
            const auto& expectedParam = funcSym->params[paramCount];
            std::string argType = getExpType(argResult);
            if ((expectedParam.second == "int" || expectedParam.second == "array_int") &&
                !(argType == "int" || argType == "const_int" || (argType.rfind("array_",0)==0 && expectedParam.second.rfind("array_",0)==0 ))) {
                 // Type mismatch error could be reported here
            }
        } else {
             semanticError("Too many arguments for function '" + funcSym->name + "'.");
        }
        irGen.addQuad("PARAM", argResult, "_", std::to_string(paramCount++));
    }

    if (paramCount != funcSym->params.size()) {
        semanticError("Incorrect number of arguments for function '" + funcSym->name +
                      "'. Expected " + std::to_string(funcSym->params.size()) + " got " + std::to_string(paramCount));
    }

    recordSyntaxOutput("<FuncRParams>");
}

// RelExp → AddExp { ('<' | '>' | '<=' | '>=') AddExp }
std::string SyntaxAnalyzer::parseRelExp() {
    std::string leftOperand = parseAddExp("int");
    recordSyntaxOutput("<RelExp>"); // For the initial AddExp part

    while (currentToken.type == LSS || currentToken.type == GRE ||
           currentToken.type == LEQ || currentToken.type == GEQ) {
        TokenType opToken = currentToken.type;
        match(opToken);
        std::string rightOperand = parseAddExp("int");

        std::string resultTempName = irGen.newTemp();
        Symbol tempSymbol;
        tempSymbol.name = resultTempName;
        tempSymbol.type = "int";
        tempSymbol.isConstant = false;
        tempSymbol.isArray = false;
        tempSymbol.scopeLevel = symTab.currentScopeLevel;
        tempSymbol.isGlobal = (symTab.currentScopeLevel == 1);
        if (!symTab.addSymbol(tempSymbol)) {
            semanticError("Failed to add temporary variable " + resultTempName + " to symbol table in RelExp.");
        }

        std::string opStr;
        if (opToken == LSS) opStr = "LSS";
        else if (opToken == GRE) opStr = "GRE";
        else if (opToken == LEQ) opStr = "LEQ";
        else opStr = "GEQ";
        irGen.addQuad(opStr, leftOperand, rightOperand, resultTempName);
        leftOperand = resultTempName;
        recordSyntaxOutput("<RelExp>"); // For each operation
    }
    return leftOperand;
}

// EqExp → RelExp { ('==' | '!=') RelExp }
std::string SyntaxAnalyzer::parseEqExp() {
    std::string leftOperand = parseRelExp();
    recordSyntaxOutput("<EqExp>"); // For the initial RelExp part

    while (currentToken.type == EQL || currentToken.type == NEQ) {
        TokenType opToken = currentToken.type;
        match(opToken);
        std::string rightOperand = parseRelExp();

        std::string resultTempName = irGen.newTemp();
        Symbol tempSymbol;
        tempSymbol.name = resultTempName;
        tempSymbol.type = "int";
        tempSymbol.isConstant = false;
        tempSymbol.isArray = false;
        tempSymbol.scopeLevel = symTab.currentScopeLevel;
        tempSymbol.isGlobal = (symTab.currentScopeLevel == 1);
        if (!symTab.addSymbol(tempSymbol)) {
            semanticError("Failed to add temporary variable " + resultTempName + " to symbol table in EqExp.");
        }

        std::string opStr = (opToken == EQL ? "EQL" : "NEQ");
        irGen.addQuad(opStr, leftOperand, rightOperand, resultTempName);
        leftOperand = resultTempName;
        recordSyntaxOutput("<EqExp>"); // For each operation
    }
    return leftOperand;
}

// LAndExp → EqExp { '&&' EqExp }
std::string SyntaxAnalyzer::parseLAndExp() {
    std::string leftOperand = parseEqExp();
    recordSyntaxOutput("<LAndExp>"); // For the initial EqExp part

    while (currentToken.type == AND) {
        match(AND);
        // Short-circuiting for && (A && B):
        // result = 0 (default)
        // if A is false, goto end_and (result is 0)
        // ; A is true
        // if B is false, goto end_and (result is 0)
        // ; B is true (so A && B is true)
        // result = 1
        // end_and:
        std::string resultTempName = irGen.newTemp();
        Symbol tempSymbol;
        tempSymbol.name = resultTempName;
        tempSymbol.type = "int";
        tempSymbol.isConstant = false;
        tempSymbol.isArray = false;
        tempSymbol.scopeLevel = symTab.currentScopeLevel;
        tempSymbol.isGlobal = (symTab.currentScopeLevel == 1);
        if (!symTab.addSymbol(tempSymbol)) {
            semanticError("Failed to add temporary variable " + resultTempName + " to symbol table in LAndExp.");
        }
        std::string endAndLabel = irGen.newLabel();

        irGen.addQuad("ASSIGN", "0", "_", resultTempName); // Default to false
        irGen.addQuad("IF_FALSE_GOTO", leftOperand, "_", endAndLabel);
        // Left is true, evaluate right
        std::string rightOperand = parseEqExp();
        irGen.addQuad("IF_FALSE_GOTO", rightOperand, "_", endAndLabel);
        // Both left and right are true
        irGen.addQuad("ASSIGN", "1", "_", resultTempName);
        irGen.addQuad(endAndLabel + ":", "_", "_", "_");

        leftOperand = resultTempName;
        recordSyntaxOutput("<LAndExp>"); // For each operation
    }
    return leftOperand;
}

// LOrExp → LAndExp { '||' LAndExp }
std::string SyntaxAnalyzer::parseLOrExp() {
    std::string leftOperand = parseLAndExp();
    recordSyntaxOutput("<LOrExp>"); // For the initial LAndExp part

    while (currentToken.type == OR) {
        match(OR);
        // Short-circuiting for || (A || B):
        // result = 1 (default)
        // if A is true, goto end_or (result is 1)
        // ; A is false
        // if B is true, goto end_or (result is 1)
        // ; B is false (so A || B is false)
        // result = 0
        // end_or:
        std::string resultTempName = irGen.newTemp();
        Symbol tempSymbol;
        tempSymbol.name = resultTempName;
        tempSymbol.type = "int";
        tempSymbol.isConstant = false;
        tempSymbol.isArray = false;
        tempSymbol.scopeLevel = symTab.currentScopeLevel;
        tempSymbol.isGlobal = (symTab.currentScopeLevel == 1);
        if (!symTab.addSymbol(tempSymbol)) {
            semanticError("Failed to add temporary variable " + resultTempName + " to symbol table in LOrExp.");
        }
        std::string endOrLabel = irGen.newLabel();

        irGen.addQuad("ASSIGN", "1", "_", resultTempName); // Default to true
        irGen.addQuad("IF_TRUE_GOTO", leftOperand, "_", endOrLabel);
        // Left is false, evaluate right
        std::string rightOperand = parseLAndExp();
        irGen.addQuad("IF_TRUE_GOTO", rightOperand, "_", endOrLabel);
        // Both left and right are false
        irGen.addQuad("ASSIGN", "0", "_", resultTempName);
        irGen.addQuad(endOrLabel + ":", "_", "_", "_");

        leftOperand = resultTempName;
        recordSyntaxOutput("<LOrExp>"); // For each operation
    }
    return leftOperand;
}

// ConstExp → AddExp
std::string SyntaxAnalyzer::parseConstExp() {
    std::string result = parseAddExp("int"); // Expect int for const expressions usually

    // After constant folding attempts in AddExp/MulExp/UnaryExp,
    // result should be a numeric string if it truly resolved to a constant.
    int constValue;
    if (!tryResolveToConstInt(result, constValue)) {
        // If it couldn't be resolved to a constant integer now (e.g., it's a temp var like '_t1')
        // then it wasn't a valid compile-time constant expression.
        semanticError("Expression in constant context ('" + result + "') did not evaluate to a compile-time integer constant.");
    }

    // If tryResolveToConstInt succeeded, 'result' might still be an identifier that resolved to a const.
    // For consistency, and to ensure parseConstInitVal receives a pure number string:
    recordSyntaxOutput("<ConstExp>");
    return std::to_string(constValue); // Return the actual folded integer value as a string
}

// Cond → LOrExp
std::string SyntaxAnalyzer::parseCond() {
    std::string result = parseLOrExp();
    recordSyntaxOutput("<Cond>");
    return result;
}

// Decl → ConstDecl | VarDecl
void SyntaxAnalyzer::parseDecl() {
    if (currentToken.type == CONSTTK) {
        parseConstDecl();
    } else if (currentToken.type == INTTK) {
        parseVarDecl();
    } else {
        semanticError("Syntax error in Decl: Expected CONSTTK or INTTK, got " + currentToken.lexeme);
    }
    // recordSyntaxOutput("<Decl>"); // Optional, as ConstDecl/VarDecl record themselves
}

// LVal → Ident {'[' Exp ']'}
// Returns:
// - For scalar: (potentially mangled) name of the variable.
// - For array element: name of a temporary variable holding the *address* of the element.
std::string SyntaxAnalyzer::parseLVal(bool& isArrayAccess, std::string& arrayIndexResultTemp_out, bool isAssignmentLHS) {
    recordSyntaxOutput("<LVal>");
    if (currentToken.type != IDENFR) {
        semanticError("Expected identifier for LVal, got " + currentToken.lexeme);
        return "";
    }
    std::string identName = currentToken.lexeme;
    int identLine = currentToken.lineNumber;
    match(IDENFR);

    Symbol* sym = symTab.lookupSymbol(identName);
    if (!sym) {
        semanticError("Identifier \"" + identName + "\" not declared (line " + std::to_string(identLine) + ").");
        return "";
    }
    if (sym->type == "int_func" || sym->type == "void_func") {
         semanticError("Function name '" + identName + "' cannot be used as an LVal (line " + std::to_string(identLine) + ").");
         return "";
    }

    std::string nameForIRBase = this->getMangledNameForIR(sym);

    isArrayAccess = false;
    arrayIndexResultTemp_out = ""; // This was for 1D index, might need rethink for 2D or remove if not used by caller

    if (currentToken.type == LBRACK) { // Array element access
        if (!sym->isArray) {
            semanticError("Attempting array access on non-array variable '" + identName + "' (line " + std::to_string(identLine) + ").");
        }
        isArrayAccess = true;
        match(LBRACK);
        std::string index1ExpVal = parseExp("int");
        // arrayIndexResultTemp_out = index1ExpVal; // If caller needs first index string for some reason
        match(RBRACK);
        recordSyntaxOutput("["); recordSyntaxOutput("<Exp>"); recordSyntaxOutput("]");

        std::string finalOffsetBytesTemp;

        if (sym->arrayDimensions.size() == 1) { // 1D Array
            if (currentToken.type == LBRACK) {
                 semanticError("Too many dimensions for 1D array '" + identName + "' (line " + std::to_string(identLine) + ").");
                 // Consume extra bracket and expression if any to allow parsing to continue
                 match(LBRACK); parseExp("int"); match(RBRACK);
            }
            std::string elementSize = "4";
            finalOffsetBytesTemp = irGen.newTemp();
            Symbol tempOffset1D; tempOffset1D.name = finalOffsetBytesTemp; tempOffset1D.type = "int"; 
            tempOffset1D.scopeLevel = symTab.currentScopeLevel; tempOffset1D.isGlobal = (symTab.currentScopeLevel == 1);
            symTab.addSymbol(tempOffset1D);
            irGen.addQuad("MUL", index1ExpVal, elementSize, finalOffsetBytesTemp);
        } else if (sym->arrayDimensions.size() == 2) { // 2D Array
            if (currentToken.type != LBRACK) {
                semanticError("Missing second dimension for 2D array '" + identName + "' (line " + std::to_string(identLine) + ").");
                // Gracefully return or attempt recovery. For now, proceed as if it might be an error in usage (e.g. passing 2D array as pointer)
                // However, for LVal access, both dimensions are typically required.
                // This path might be problematic if a 2D array base address is expected (not element)
                // For now, assume if one LBRACK is seen for 2D array, a second one for element access must follow.
                 return nameForIRBase; // Or some error indicator, or try to compute address of row? 
                                     // For now, if used as LVal and it's 2D, both indices needed.
            }
            match(LBRACK);
            std::string index2ExpVal = parseExp("int");
            match(RBRACK);
            recordSyntaxOutput("["); recordSyntaxOutput("<Exp>"); recordSyntaxOutput("]");

            if (sym->arrayDimensions[1] <= 0) { // Check if second dimension size is valid
                semanticError("Array '" + identName + "' has invalid second dimension size (" + std::to_string(sym->arrayDimensions[1]) + ") for address calculation.");
                return ""; // Cannot compute offset
            }

            // Offset = (index1 * dim2_size + index2) * element_size
            // Temp for index1 * dim2_size
            std::string dim2SizeStr = std::to_string(sym->arrayDimensions[1]);
            std::string temp1 = irGen.newTemp();
            Symbol tempSym1; tempSym1.name = temp1; tempSym1.type = "int"; symTab.addSymbol(tempSym1);
            irGen.addQuad("MUL", index1ExpVal, dim2SizeStr, temp1);

            // Temp for (index1 * dim2_size + index2)
            std::string temp2 = irGen.newTemp();
            Symbol tempSym2; tempSym2.name = temp2; tempSym2.type = "int"; symTab.addSymbol(tempSym2);
            irGen.addQuad("ADD", temp1, index2ExpVal, temp2);

            // Final offset in bytes: temp2 * element_size
            std::string elementSize = "4";
            finalOffsetBytesTemp = irGen.newTemp();
            Symbol tempSymFinalOff; tempSymFinalOff.name = finalOffsetBytesTemp; tempSymFinalOff.type = "int"; symTab.addSymbol(tempSymFinalOff);
            irGen.addQuad("MUL", temp2, elementSize, finalOffsetBytesTemp);
        } else {
            semanticError("Array '" + identName + "' has unsupported number of dimensions: " + std::to_string(sym->arrayDimensions.size()) + " (line " + std::to_string(identLine) + "). Only 1D or 2D supported for LVal element access.");
            return ""; // Or a mangled base name if that's a valid interpretation
        }

        if (isAssignmentLHS && sym->isConstant) {
             semanticError("Cannot assign to element of constant array \"" + sym->name + "\" (line " + std::to_string(identLine) + ").");
        }

        std::string effectiveAddressTemp = irGen.newTemp();
        Symbol tempSymbolAddr; tempSymbolAddr.name = effectiveAddressTemp; tempSymbolAddr.type = "int_addr";
        tempSymbolAddr.scopeLevel = symTab.currentScopeLevel; tempSymbolAddr.isGlobal = (symTab.currentScopeLevel == 1);
        if (!symTab.addSymbol(tempSymbolAddr)) {
            semanticError("Failed to add temporary variable " + effectiveAddressTemp + " to symbol table in LVal.");
        }
        irGen.addQuad("ADD_OFFSET", nameForIRBase, finalOffsetBytesTemp, effectiveAddressTemp);
        return effectiveAddressTemp; 
    } else { // Scalar variable or array name used as address (e.g. passed to function)
        if (isAssignmentLHS && sym->isConstant) {
            semanticError("Cannot assign to constant identifier \"" + sym->name + "\" (line " + std::to_string(identLine) + ").");
        }
        // If sym is an array but no brackets follow, it means using the array name, which decays to its base address.
        // The mangled name (nameForIRBase) is appropriate for this in IR.
        return nameForIRBase; 
    }
}


void SyntaxAnalyzer::analyze() {
    parseCompUnit();

    if (enableSyntaxOutput) {
        mergeAndWriteSyntaxOutput();
    }
}

bool SyntaxAnalyzer::tryResolveToConstInt(const std::string& operandStr, int& outValue) {
    if (operandStr.empty()) {
        return false;
    }

    bool isNumeric = true;
    size_t k_idx = 0;
    if (operandStr[0] == '-') {
        if (operandStr.length() == 1) isNumeric = false; // Just "-"
        k_idx = 1;
    }
    for (; k_idx < operandStr.length(); ++k_idx) {
        if (!isdigit(operandStr[k_idx])) {
            isNumeric = false;
            break;
        }
    }

    if (isNumeric) {
        try {
            outValue = std::stoi(operandStr);
            return true;
        } catch (const std::invalid_argument& ia) {
            // Not a valid integer string for stoi
        } catch (const std::out_of_range& oor) {
            // Value out of range for stoi
        }
    }

    Symbol* s = this->symTab.lookupSymbol(operandStr);
    if (s && s->isConstant && !s->isArray && s->isInitialized) {
        // We are interested in simple integer constants here.
        // For constant folding, as long as it has a valid integer s->value.
        outValue = s->value;
        return true;
    }

    return false;
}
