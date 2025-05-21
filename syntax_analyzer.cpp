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
        std::string dimStr = parseConstExp();
        try {
            if (!dimensions.empty()) dimensions.clear();
            dimensions.push_back(std::stoi(dimStr));
        }
        catch (...) { semanticError("Array dimension for const '" + name + "' must be a constant integer."); }
        match(RBRACK);
    }
    sym.isArray = isArray;
    sym.arrayDimensions = dimensions;
    sym.type = (isArray ? "array_const_" : "const_") + constBaseType;


    match(ASSIGN);
    parseConstInitVal(sym);

    if (!symTab.addSymbol(sym)) {
        semanticError("Redefinition of constant symbol '" + sym.name + "'.");
    }
    recordSyntaxOutput("<ConstDef>");
}

// ConstInitVal → ConstExp | '{' [ ConstInitVal { ',' ConstInitVal } ] '}'
// For single const: returns evaluated string. For array: fills sym, generates IR for elements.
std::string SyntaxAnalyzer::parseConstInitVal(Symbol& constSym, bool parsingArrayElement) {
    if (currentToken.type == LBRACE) {
        if (!constSym.isArray && !parsingArrayElement) semanticError("Aggregate initializer for non-array const '" + constSym.name + "'.");
        match(LBRACE);

        // For MIPS, global const arrays will be initialized in .data section.
        // Local const arrays are more complex, often disallowed or unrolled.
        // For now, we'll parse, and MipsGenerator will rely on values stored in Symbol object for globals.
        int elementCount = 0;
        if (currentToken.type != RBRACE) {
            parseConstInitVal(constSym, true);
            elementCount++;
            while (currentToken.type == COMMA) {
                match(COMMA);
                parseConstInitVal(constSym, true);
                elementCount++;
            }
        }
        match(RBRACE);
        recordSyntaxOutput("<ConstInitVal>");
        return "{array_init}"; // Placeholder indicating array init was parsed
    } else {
        if (constSym.isArray && !parsingArrayElement) semanticError("Scalar initializer for array const '" + constSym.name + "'.");
        std::string valStr = parseConstExp();
        try {
            if (parsingArrayElement && constSym.isArray) {
                try {
                    int elementValue = std::stoi(valStr);
                    constSym.arrayValues.push_back(elementValue);
                } catch (...) {
                    semanticError("Invalid array element value for '" + constSym.name + "'. Expected integer.");
                }
            } else if (!constSym.isArray) {
                constSym.value = std::stoi(valStr);
            }
            constSym.isInitialized = true;
        }
        catch (...) { semanticError("Invalid constant value for '" + constSym.name + "'. Expected integer string from ConstExp."); }

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
    sym.isInitialized = false; // Will be true if '=' InitVal is present

    bool isArray = false;
    std::vector<int> dimensions;
    if (currentToken.type == LBRACK) {
        isArray = true;
        match(LBRACK);
        std::string dimStr = parseConstExp(); // Dimensions must be constant expressions
        try {
            if (!dimensions.empty()) dimensions.clear();
            dimensions.push_back(std::stoi(dimStr));
        }
        catch(...) { semanticError("Array dimension for variable '" + name + "' must be a constant integer."); }
        match(RBRACK);
    }
    sym.isArray = isArray;
    sym.arrayDimensions = dimensions;
    sym.type = (isArray ? "array_" : "") + varBaseType;

    // Add symbol to table *before* parsing InitVal, especially for offset calculation for locals.
    if (!symTab.addSymbol(sym)) {
        semanticError("Redefinition of variable symbol '" + sym.name + "'.");
    }
    // Get the symbol from table to ensure we have the one with correct offset for IR gen
    Symbol* symInTable = symTab.lookupSymbol(name);
    if (!symInTable) { semanticError("Internal: Failed to find symbol " + name + " after declaration."); }


    if (currentToken.type == ASSIGN) {
        match(ASSIGN);
        std::string initValResult = parseInitVal(*symInTable);

        if (!symInTable->isArray) {
            std::string targetNameForAssign = this->getMangledNameForIR(symInTable);
            irGen.addQuad("ASSIGN", initValResult, "_", targetNameForAssign);
        } else {
            if (initValResult != "{array_init}") { // Defensive check, should be {array_init}
                  semanticError("Internal: Array initialization did not return {array_init} sentinel.");
            }
        }
        symInTable->isInitialized = true;
            } else {
        // No initializer.
        // Global variables: MIPS .data section will typically initialize them to 0.
        // Local variables: They are uninitialized on the stack. No IR needed for non-initialization.
        // If the language required zero-initialization for locals, IR would be needed here.
    }
    recordSyntaxOutput("<VarDef>");
}

// InitVal → Exp | '{' [ InitVal { ',' InitVal } ] '}'
// Returns:
// - For Exp: a temp variable or constant string from parseExp.
// - For aggregate: "{array_init}" and should ideally generate element-wise IR internally.
std::string SyntaxAnalyzer::parseInitVal(Symbol& varSym) {
    if (currentToken.type == LBRACE) {
        if (!varSym.isArray) {
            semanticError("Aggregate initializer for non-array variable '" + varSym.name + "'.");
            // Attempt to consume the erroneous block to allow parsing to continue for other errors
            match(LBRACE);
            while(currentToken.type != RBRACE && currentToken.type != TOKEN_EOF) {
                if (currentToken.type == LBRACE) parseInitVal(varSym); // Eat nested block
                else getNextTokenFromLexer();
                if (currentToken.type == COMMA) match(COMMA);
                else break;
            }
            if (currentToken.type == RBRACE) match(RBRACE);
            return "{error_array_init}";
        }
        match(LBRACE);

        // Determine the base name for IR (potentially mangled for local arrays)
        std::string nameForIRBase = this->getMangledNameForIR(&varSym);

        int elementIndex = 0;
        int totalElementsExpected = 1;
        if (!varSym.arrayDimensions.empty()) {
            for(int dim : varSym.arrayDimensions) totalElementsExpected *= std::max(1, dim);
        } else if (varSym.isArray) {
            semanticError("Array symbol '" + varSym.name + "' has no dimensions specified for initialization.");
            if (currentToken.type == RBRACE) { match(RBRACE); return "{error_array_init}"; }
        }

        if (currentToken.type != RBRACE) {
            // For array elements, we expect expressions that evaluate to the element type (e.g., int)
            std::string elemVal = parseExp(varSym.type.rfind("array_", 0) == 0 ? varSym.type.substr(6) : varSym.type);

            if (elementIndex < totalElementsExpected) {
                std::string indexStr = std::to_string(elementIndex);
                std::string elementSize = "4";
                std::string offsetBytesTemp = irGen.newTemp();

                Symbol tempSymbolOffset_init;
                tempSymbolOffset_init.name = offsetBytesTemp;
                tempSymbolOffset_init.type = "int";
                tempSymbolOffset_init.isConstant = false;
                tempSymbolOffset_init.isArray = false;
                tempSymbolOffset_init.scopeLevel = symTab.currentScopeLevel;
                tempSymbolOffset_init.isGlobal = (symTab.currentScopeLevel == 1);
                if (!symTab.addSymbol(tempSymbolOffset_init)) {
                    semanticError("Failed to add temporary variable " + offsetBytesTemp + " to symbol table in InitVal.");
                }

                irGen.addQuad("MUL", indexStr, elementSize, offsetBytesTemp);

                std::string effectiveAddressTemp = irGen.newTemp();

                Symbol tempSymbolAddr_init;
                tempSymbolAddr_init.name = effectiveAddressTemp;
                tempSymbolAddr_init.type = "int_addr"; // Indicate it holds an address
                tempSymbolAddr_init.isConstant = false;
                tempSymbolAddr_init.isArray = false;
                tempSymbolAddr_init.scopeLevel = symTab.currentScopeLevel;
                tempSymbolAddr_init.isGlobal = (symTab.currentScopeLevel == 1);
                if (!symTab.addSymbol(tempSymbolAddr_init)) {
                    semanticError("Failed to add temporary variable " + effectiveAddressTemp + " to symbol table in InitVal.");
                }

                irGen.addQuad("ADD_OFFSET", nameForIRBase, offsetBytesTemp, effectiveAddressTemp);
                irGen.addQuad("STORE_TO_ADDR", effectiveAddressTemp, "_", elemVal);
            } else {
                semanticError("Too many initializers for array '" + varSym.name + "'.");
            }
            elementIndex++;

            while (currentToken.type == COMMA) {
                match(COMMA);
                elemVal = parseExp(varSym.type.rfind("array_", 0) == 0 ? varSym.type.substr(6) : varSym.type);
                if (elementIndex < totalElementsExpected) {
                    std::string indexStr = std::to_string(elementIndex);
                    std::string elementSize = "4";
                    std::string offsetBytesTemp = irGen.newTemp();

                    Symbol tempSymbolOffset_init2;
                    tempSymbolOffset_init2.name = offsetBytesTemp;
                    tempSymbolOffset_init2.type = "int";
                    tempSymbolOffset_init2.isConstant = false;
                    tempSymbolOffset_init2.isArray = false;
                    tempSymbolOffset_init2.scopeLevel = symTab.currentScopeLevel;
                    tempSymbolOffset_init2.isGlobal = (symTab.currentScopeLevel == 1);
                    if (!symTab.addSymbol(tempSymbolOffset_init2)) {
                        semanticError("Failed to add temporary variable " + offsetBytesTemp + " to symbol table in InitVal (loop).");
                    }

                    irGen.addQuad("MUL", indexStr, elementSize, offsetBytesTemp);

                    std::string effectiveAddressTemp = irGen.newTemp();

                    Symbol tempSymbolAddr_init2;
                    tempSymbolAddr_init2.name = effectiveAddressTemp;
                    tempSymbolAddr_init2.type = "int_addr";
                    tempSymbolAddr_init2.isConstant = false;
                    tempSymbolAddr_init2.isArray = false;
                    tempSymbolAddr_init2.scopeLevel = symTab.currentScopeLevel;
                    tempSymbolAddr_init2.isGlobal = (symTab.currentScopeLevel == 1);
                    if (!symTab.addSymbol(tempSymbolAddr_init2)) {
                        semanticError("Failed to add temporary variable " + effectiveAddressTemp + " to symbol table in InitVal (loop).");
                    }

                    irGen.addQuad("ADD_OFFSET", nameForIRBase, offsetBytesTemp, effectiveAddressTemp);
                    irGen.addQuad("STORE_TO_ADDR", effectiveAddressTemp, "_", elemVal);
                } else {
                     semanticError("Too many initializers for array '" + varSym.name + "'.");
                }
                elementIndex++;
            }
        }
        // SysY might allow partial initialization (rest are 0).
        // If zero-padding is required and elementIndex < totalElementsExpected,
        // you would add loops here to STORE_TO_ADDR 0 for remaining elements.
        // For simplicity, this example assumes SysY does not require explicit zero-padding for uninitialized elements in IR for locals,
        // and globals are zeroed by .space or default .word 0 in MIPS gen.

        match(RBRACE);
        recordSyntaxOutput("<InitVal>");
        return "{array_init}";
    } else {
        // If varSym is an array, standard SysY/C usually doesn't allow direct `arr = scalar_exp;`
        // except for `char arr[] = "string";` which is special.
        // Our grammar is `InitVal -> Exp`. If `varSym` is array, this `Exp` better be an error or handled carefully.
        // For now, assume if `varSym` is scalar, this Exp is its value.
        if (varSym.isArray) {
            // This path (scalar Exp for array InitVal) is usually an error unless it's a string literal for char array.
            // Not handled here for general arrays.
            semanticError("Cannot initialize array '" + varSym.name + "' with a single scalar expression directly in InitVal. Use {}.");
        }
        std::string expectedElementType = varSym.type;
        if (varSym.isArray) {
            if (varSym.type.rfind("array_", 0) == 0) expectedElementType = varSym.type.substr(6);
        }
        std::string expRes = parseExp(expectedElementType);
        recordSyntaxOutput("<InitVal>");
        return expRes;
    }
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

        if (opIrName == "POS") { // Unary plus, effectively a no-op for value
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
                // Logical NOT: !0 is 1, !non-zero is 0
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
    } else if (currentToken.type == IDENFR) {
        const auto& allTokens = lexer.getTokens();
        Token peekNext = (tokenIndex < allTokens.size()) ? allTokens[tokenIndex] : Token{TOKEN_EOF, "", 0};

        if (peekNext.type == LPARENT) { // Function call
            std::string funcName = currentToken.lexeme;
            match(IDENFR);

            Symbol* funcSym = symTab.lookupSymbol(funcName);
            if (!funcSym || (funcSym->type != "int_func" && funcSym->type != "void_func")) {
                semanticError("Identifier '" + funcName + "' is not a function or not declared.");
            }

            match(LPARENT);
            if (currentToken.type != RPARENT) {
                parseFuncRParams(funcSym);
            }
            match(RPARENT);

            std::string resultTempName = "";
            if (funcSym->returnType == "int") {
                resultTempName = irGen.newTemp();
                Symbol tempSymbolFuncRet;
                tempSymbolFuncRet.name = resultTempName;
                tempSymbolFuncRet.type = "int";
                tempSymbolFuncRet.isConstant = false;
                tempSymbolFuncRet.isArray = false;
                tempSymbolFuncRet.scopeLevel = symTab.currentScopeLevel;
                tempSymbolFuncRet.isGlobal = (symTab.currentScopeLevel == 1);
                if (!symTab.addSymbol(tempSymbolFuncRet)) {
                    semanticError("Failed to add temporary variable " + resultTempName + " for function return to symbol table.");
                }
            }

            irGen.addQuad("CALL", funcName, std::to_string(funcSym->params.size()), resultTempName);
            recordSyntaxOutput("<UnaryExp>"); // This UnaryExp is the function call itself
            return resultTempName;
        } else {
            // If not a function call, it must be a PrimaryExp which starts with LVal (IDENFR).
            // parsePrimaryExp will handle it.
            // No recordSyntaxOutput("<UnaryExp>") here, let PrimaryExp path handle its own output.
            return parsePrimaryExp();
        }
    } else {
        // Must be other PrimaryExp types: ( Exp ) or Number
        // No recordSyntaxOutput("<UnaryExp>") here, let PrimaryExp path handle its own output.
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
    arrayIndexResultTemp_out = ""; // This will hold the *value* of the index expression

    if (currentToken.type == LBRACK) { // Array element access
        if (!sym->isArray) {
            semanticError("Attempting array access on non-array variable '" + identName + "' (line " + std::to_string(identLine) + ").");
        }
        isArrayAccess = true;
        match(LBRACK);
        std::string indexExpVal = parseExp("int");
        arrayIndexResultTemp_out = indexExpVal;
        match(RBRACK);
        recordSyntaxOutput("[");
        recordSyntaxOutput("<Exp>");
        recordSyntaxOutput("]");

        if (isAssignmentLHS && sym->isConstant) {
             semanticError("Cannot assign to element of constant array \"" + sym->name + "\" (line " + std::to_string(identLine) + ").");
        }

        // IR for array element address calculation:
        // effective_addr_temp = ADD_OFFSET base_name_for_IR, (index_exp_val * element_size)
        std::string elementSize = "4"; // Assuming int arrays (word size)
        std::string offsetBytesTemp = irGen.newTemp();
        Symbol tempSymbolOffset;
        tempSymbolOffset.name = offsetBytesTemp;
        tempSymbolOffset.type = "int";
        tempSymbolOffset.isConstant = false;
        tempSymbolOffset.isArray = false;
        tempSymbolOffset.scopeLevel = symTab.currentScopeLevel;
        tempSymbolOffset.isGlobal = (symTab.currentScopeLevel == 1);
        if (!symTab.addSymbol(tempSymbolOffset)) {
            semanticError("Failed to add temporary variable " + offsetBytesTemp + " to symbol table in LVal.");
        }

        irGen.addQuad("MUL", indexExpVal, elementSize, offsetBytesTemp);

        std::string effectiveAddressTemp = irGen.newTemp();
        Symbol tempSymbolAddr;
        tempSymbolAddr.name = effectiveAddressTemp;
        tempSymbolAddr.type = "int_addr"; // Indicate it holds an address
        tempSymbolAddr.isConstant = false;
        tempSymbolAddr.isArray = false;
        tempSymbolAddr.scopeLevel = symTab.currentScopeLevel;
        tempSymbolAddr.isGlobal = (symTab.currentScopeLevel == 1);
        if (!symTab.addSymbol(tempSymbolAddr)) {
            semanticError("Failed to add temporary variable " + effectiveAddressTemp + " to symbol table in LVal.");
        }

        // nameForIRBase is the (potentially mangled) base name of the array.
        irGen.addQuad("ADD_OFFSET", nameForIRBase, offsetBytesTemp, effectiveAddressTemp);
        return effectiveAddressTemp; // Return the temp holding the calculated *address* of the element
    } else { // Scalar variable
        if (isAssignmentLHS && sym->isConstant) {
            semanticError("Cannot assign to constant identifier \"" + sym->name + "\" (line " + std::to_string(identLine) + ").");
        }
        return nameForIRBase; // Return the (potentially mangled) name of the scalar variable itself
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
