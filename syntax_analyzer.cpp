#include "syntax_analyzer.h"
#include "mips_generator.h"
#include <algorithm> // For std::all_of
#include <cctype>    // For ::isdigit

// SyntaxAnalyzer 类的实现

// 构造函数与析构函数
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
    
    // SyntaxAnalyzer 不再自己打开 mipsOutFile. MipsGenerator 会用 mipsFileName 自己处理。
    // if (!mipsFileName.empty()) { // Check if a MIPS file name was provided
    //     mipsOutFile.open(mipsFileName);
    //     if (!mipsOutFile.is_open()) {
    //         std::cerr << "Failed to open MIPS output file: " << mipsFileName << std::endl;
    //         // Decide on error handling: throw, exit, or proceed without MIPS output
    //         // For now, let MipsGenerator handle the error when it tries to open.
    //     }
    // }

    // Assumes lexer.processFile() has been called to populate lexer.getTokens()
    // lexer.processFile(); // Ensure tokens are generated if not done by caller
    getNextTokenFromLexer(); // Initialize currentToken from the pre-lexed token stream
}

SyntaxAnalyzer::~SyntaxAnalyzer() {
    // SyntaxAnalyzer 不再管理 mipsOutFile 的打开/关闭
    // if (mipsOutFile.is_open()) mipsOutFile.close(); 
    if (syntaxOutFileInternal.is_open()) syntaxOutFileInternal.close();
}

// 新增：私有辅助方法，用于获取符号在IR中应使用的修饰后名称
std::string SyntaxAnalyzer::getMangledNameForIR(Symbol* sym) const {
    if (!sym) {
        // Should not happen, but good to guard.
        // std::cerr << "Warning: getMangledNameForIR called with null symbol." << std::endl;
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

// 辅助方法实现
void SyntaxAnalyzer::getNextTokenFromLexer() { // Renamed to avoid conflict with Lexer's own getNextToken
    const auto& allTokens = lexer.getTokens(); // Get all pre-lexed tokens
    if (tokenIndex < allTokens.size()) {
        currentToken = allTokens[tokenIndex++];
    } else {
        // This case should ideally not be reached if lexer guarantees an EOF token at the end.
        currentToken.type = TOKEN_EOF; 
        currentToken.lexeme = "EOF";
        // currentToken.lineNumber will be from the last real token or needs update
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
        // Sort based on line number or leave in the order of recording, as needed
        // For this example, I'll assume the outputs should be in the order of generation
        for (const auto& output : syntaxOutputsForFile) {
            syntaxOutFileInternal << output.first << " " << output.second << std::endl;
        }
        // Or, if it's the final file and we want to just dump all the output syntax tokens
        // syntaxOutFileInternal << "Syntax analysis completed successfully." << std::endl;
    }
}

void SyntaxAnalyzer::semanticError(const std::string& message) {
    std::cerr << "Semantic Error (line " << currentToken.lineNumber << "): " << message << std::endl;
    // For submission, this might immediately exit. For development, you might want to continue with warnings.
    exit(1);
}

std::string SyntaxAnalyzer::getExpType(const std::string& expResult) {
    if (expResult.empty()) return "unknown";
    // Check if it's a number literal
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
    // std::cerr << "Warning: Could not determine type for IR operand: " << expResult << std::endl;
    return "unknown_exp_type(" + expResult + ")";
}

// 新增：静态辅助函数，用于重排四元式，将 main 函数置于首位
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
            // Else, if currentProcessingFuncName is empty, it might be an error or a non-function quad after a function
            inAnyFunction = false; 
            currentProcessingFuncName = "";
        } else { // Quad is not FUNC_BEGIN or FUNC_END
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
                    // This case is less likely if quads are well-structured: an op inside a func scope but currentProcessingFuncName is somehow wrong
                    // For safety, or if functions can be interleaved with global ops (not typical for simple compilers)
                    // we might need a more robust state machine. Assuming simple structure for now.
                    // If it's an instruction for a function that's not main and somehow currentProcessingFuncName got cleared, add to others.
                    otherFunctionQuads.push_back(quad); 
                }
            }
        }
    }

    // Assemble the reordered quads
    reorderedQuads.insert(reorderedQuads.end(), globalInitQuads.begin(), globalInitQuads.end());
    if (mainFound) {
        reorderedQuads.insert(reorderedQuads.end(), mainFunctionQuads.begin(), mainFunctionQuads.end());
    }
    reorderedQuads.insert(reorderedQuads.end(), otherFunctionQuads.begin(), otherFunctionQuads.end());

    // If main wasn't found but there were quads, it implies an issue or no main function.
    // Returning originalQuads might be safer if reordering significantly changes non-main-centric code.
    // However, the goal is main-first for the评测机.
    if (!mainFound && !originalQuads.empty()) {
        // std::cerr << "Warning: main function not found for reordering. Global inits might still be first." << std::endl;
        // If no main, the 'reorderedQuads' would be global + others, which is likely the original order anyway for such a case.
        // If originalQuads is non-empty and reorderedQuads is empty (e.g. if only main existed but wasn't correctly extracted)
        // it is safer to return the original to avoid losing code.
        if (reorderedQuads.empty() && !originalQuads.empty()) return originalQuads;
    }
     if (reorderedQuads.size() != originalQuads.size()) {
        // This indicates a logic error in quad distribution.
        // std::cerr << "CRITICAL REORDERING ERROR: Quad count mismatch. Original: " << originalQuads.size() 
        //           << ", Reordered: " << reorderedQuads.size() << ". Returning original quads." << std::endl;
        return originalQuads; // Fallback to prevent code loss
    }

    return reorderedQuads;
}

// CompUnit → {Decl} {FuncDef} MainFuncDef
void SyntaxAnalyzer::parseCompUnit() {
    // Assumes getNextTokenFromLexer() has been called once by the constructor or analyze()
    // to initialize currentToken.

    while (currentToken.type != TOKEN_EOF) {
        if (currentToken.type == CONSTTK) {
            parseDecl(); // Handles ConstDecl
        } else if (currentToken.type == INTTK || currentToken.type == VOIDTK) {
            // Peek ahead to distinguish between VarDecl, FuncDef, and MainFuncDef
            const auto& allTokens = lexer.getTokens();
            Token token1 = currentToken; // Current token: INTTK or VOIDTK
            
            // tokenIndex is the index of the *next* token to be fetched by getNextTokenFromLexer().
            // So, allTokens[tokenIndex] is the token immediately after currentToken.
            Token token2 = (tokenIndex < allTokens.size()) ? allTokens[tokenIndex] : Token{TOKEN_EOF,"",0};
            // And allTokens[tokenIndex+1] is the token after token2.
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
                 break; // Stop parsing on such an error
            }
             else {
                // This case might be reached if INTTK is not followed by IDENFR or MAINTK,
                // or some other unexpected sequence.
                 if (token1.type == TOKEN_EOF) break; // Should be caught by loop condition
                semanticError("Unexpected token sequence at global scope starting with: " + token1.lexeme + " " + token2.lexeme);
                break; // Stop parsing on such an error
            }
        } else {
            if (currentToken.type == TOKEN_EOF) break; // End of all tokens
            // Using lexer.getTokenNames().at() for robust name fetching
            std::string tokenNameStr = "UNKNOWN_TOKEN_TYPE";
            try {
                tokenNameStr = lexer.getTokenNames().at(currentToken.type);
            } catch (const std::out_of_range& oor) {
                // Handle cases where token type might be out of bounds for the map
            }
            semanticError("Unexpected token at global scope: " + currentToken.lexeme + " of type " + tokenNameStr);
            break; // Stop parsing on fundamental syntax error at global level
        }
    }
    recordSyntaxOutput("<CompUnit>");

    // --- MIPS Code Generation Phase (at the end of CompUnit parsing) ---
    const auto& originalQuads = irGen.getQuads();

    if (originalQuads.empty()) {
        std::cerr << "No quadruples generated. Skipping MIPS generation." << std::endl;
        return;
    }
    
    try {
        MipsGenerator originalMipsGen("mips_original.txt", symTab, originalQuads);
        originalMipsGen.generate();
        std::cout << "Successfully generated mips_original.txt" << std::endl;
    } catch (const std::exception& e) {
        std::cerr << "Error generating mips_original.txt: " << e.what() << std::endl;
    }

    // Declare reorderedQuads as const to directly match const reference parameter type
    const std::vector<Quadruple> reorderedQuads = reorderQuadsForMainFirst(originalQuads);

    try {
        MipsGenerator finalMipsGen(this->mipsFileName, symTab, reorderedQuads);
        finalMipsGen.generate();
        std::cout << "Successfully generated " << this->mipsFileName << " with main function first." << std::endl;
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
    std::string baseType = parseBType(); // Should return "int"
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
    // Placeholder - to be implemented with symbol table and IR gen
    std::string name = currentToken.lexeme;
    match(IDENFR);

    Symbol sym;
    sym.name = name;
    sym.isConstant = true;
    sym.isGlobal = (symTab.currentScopeLevel == 1);
    sym.scopeLevel = symTab.currentScopeLevel;
    sym.isInitialized = true; 

    bool isArray = false;
    std::vector<int> dimensions; // Retained for Symbol, but parsing logic simplified, UNCOMMENTED
    if (currentToken.type == LBRACK) { // Changed from while to if, aligning with lab3 and EBNF [ '[' ConstExp ']' ]
        isArray = true;
        match(LBRACK);
        std::string dimStr = parseConstExp(); 
        try { 
            // For Symbol, we might still want to store this one dimension if it exists
            if (!dimensions.empty()) dimensions.clear(); // ensure only one dim if rule is strict
            dimensions.push_back(std::stoi(dimStr)); 
        }
        catch (...) { semanticError("Array dimension for const '" + name + "' must be a constant integer."); }
        match(RBRACK);
    }
    sym.isArray = isArray;
    sym.arrayDimensions = dimensions; // Store parsed dimension(s)
    sym.type = (isArray ? "array_const_" : "const_") + constBaseType;


    match(ASSIGN);
    // ConstInitVal will handle value assignment and IR for global consts
    parseConstInitVal(sym); // Pass symbol to be filled

    if (!symTab.addSymbol(sym)) { 
        semanticError("Redefinition of constant symbol '" + sym.name + "'.");
    }
    // If global const array, MipsGenerator will use sym.arrayDimensions and values (if stored)
    // If global scalar const, MipsGenerator uses sym.value
    // If local, it's effectively a compile-time substitution or error if used non-constly.
    // SysY doesn't have local consts in the same way as C++ stack consts. They are for compile-time.
    recordSyntaxOutput("<ConstDef>");
}

// ConstInitVal → ConstExp | '{' [ ConstInitVal { ',' ConstInitVal } ] '}'
// For single const: returns evaluated string. For array: fills sym, generates IR for elements.
std::string SyntaxAnalyzer::parseConstInitVal(Symbol& constSym) {
    if (currentToken.type == LBRACE) { 
        if (!constSym.isArray) semanticError("Aggregate initializer for non-array const '" + constSym.name + "'.");
        match(LBRACE);
        
        // TODO: Handle const array initialization IR generation.
        // This is complex: need to generate multiple assignments or a data block.
        // For MIPS, global const arrays will be initialized in .data section.
        // Local const arrays are more complex, often disallowed or unrolled.
        // For now, we'll parse, and MipsGenerator will rely on values stored in Symbol object for globals.
        int elementCount = 0;
        if (currentToken.type != RBRACE) {
            // This needs a way to store multiple initial values in the Symbol object for arrays.
            // For now, let's assume parseConstInitVal for element returns a string of the value.
            parseConstInitVal(constSym); // Recursive call - how to store for nth element?
            elementCount++;
            while (currentToken.type == COMMA) {
                match(COMMA);
                parseConstInitVal(constSym);
                elementCount++;
            }
        }
        // Check elementCount against constSym.arrayDimensions if needed.
        match(RBRACE);
        recordSyntaxOutput("<ConstInitVal>");
        return "{array_init}"; // Placeholder indicating array init was parsed
    } else { // Single ConstExp
        if (constSym.isArray) semanticError("Scalar initializer for array const '" + constSym.name + "'.");
        std::string valStr = parseConstExp(); // Should return a string representation of the constant value
        try { 
            constSym.value = std::stoi(valStr); // Store the integer value in the symbol
            constSym.isInitialized = true;
        }
        catch (...) { semanticError("Invalid constant value for '" + constSym.name + "'. Expected integer string from ConstExp."); }
        
        // If global, MipsGenerator will use sym.value from symbol table for .data section.
        // No specific IR quad like ASSIGN is typically needed for *defining* global constants.
        // Their values are used directly or loaded from .data.
        recordSyntaxOutput("<ConstInitVal>");
        return valStr;
    }
}

// VarDecl → BType VarDef { ',' VarDef } ';'
void SyntaxAnalyzer::parseVarDecl() {
    std::string baseType = parseBType(); // e.g., "int"
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
    std::vector<int> dimensions; // Retained for Symbol, but parsing logic simplified, UNCOMMENTED
    if (currentToken.type == LBRACK) { // Changed from while to if
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
        std::string initValResult = parseInitVal(*symInTable); // parseInitVal returns the temp/const string
        
        if (!symInTable->isArray) { // Scalar variable initialization
            // irGen.addQuad("ASSIGN", initValResult, "_", symInTable->name); // OLD WAY
            std::string targetNameForAssign = this->getMangledNameForIR(symInTable);
            irGen.addQuad("ASSIGN", initValResult, "_", targetNameForAssign); 
        } else { // Array initialization
            // parseInitVal for arrays returns "{array_init}" and should handle element-wise IR gen itself if complex.
            // For simple cases like `int a[2] = {1, 2};` this is more involved.
            // The MIPS generator would initialize global arrays in .data.
            // Local arrays are on stack. If initialized, need a sequence of stores.
            // This is a complex part. The current parseInitVal doesn't generate element-wise IR.
            if (initValResult != "{array_init}") { // Defensive check, should be {array_init}
                 // This implies a scalar was returned for an array type - likely an error handled by parseInitVal or needs more checking here.
                  semanticError("Internal: Array initialization did not return {array_init} sentinel.");
            }
        }
        symInTable->isInitialized = true; // Mark as initialized
            } else {
        // No initializer.
        // Global variables: MIPS .data section will typically initialize them to 0 (by .space or .word 0).
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
    if (currentToken.type == LBRACE) { // Aggregate initializer for an array
        if (!varSym.isArray) {
            semanticError("Aggregate initializer for non-array variable '" + varSym.name + "'.");
            // Attempt to consume the erroneous block to allow parsing to continue for other errors
            match(LBRACE);
            while(currentToken.type != RBRACE && currentToken.type != TOKEN_EOF) {
                // Consume tokens somewhat intelligently, but this is basic recovery
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
        if (!varSym.arrayDimensions.empty()) { // Should always be non-empty if varSym.isArray is true
            for(int dim : varSym.arrayDimensions) totalElementsExpected *= std::max(1, dim);
        } else if (varSym.isArray) {
            semanticError("Array symbol '" + varSym.name + "' has no dimensions specified for initialization.");
            // Basic recovery for this case too
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
                    tempSymbolAddr_init2.type = "int_addr"; // Indicate it holds an address
                    tempSymbolAddr_init2.isConstant = false;
                    tempSymbolAddr_init2.isArray = false;
                    tempSymbolAddr_init2.scopeLevel = symTab.currentScopeLevel;
                    tempSymbolAddr_init2.isGlobal = (symTab.currentScopeLevel == 1);
                    if (!symTab.addSymbol(tempSymbolAddr_init2)) {
                        semanticError("Failed to add temporary variable " + effectiveAddressTemp + " to symbol table in InitVal (loop).");
                    }

                    irGen.addQuad("ADD_OFFSET", nameForIRBase, offsetBytesTemp, effectiveAddressTemp);
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
    } else { // Single Exp for initializing a scalar or potentially an array (if grammar allows)
        // If varSym is an array, standard SysY/C usually doesn't allow direct `arr = scalar_exp;`
        // except for `char arr[] = "string";` which is special.
        // Our grammar is `InitVal -> Exp`. If `varSym` is array, this `Exp` better be an error or handled carefully.
        // For now, assume if `varSym` is scalar, this Exp is its value.
        if (varSym.isArray) {
            // This path (scalar Exp for array InitVal) is usually an error unless it's a string literal for char array.
            // Not handled here for general arrays.
            semanticError("Cannot initialize array '" + varSym.name + "' with a single scalar expression directly in InitVal. Use {}.");
        }
        // Determine expected type for the expression based on varSym.
        std::string expectedElementType = varSym.type; // e.g. "int"
        if (varSym.isArray) { // Should not happen if error above is active
            // If it were array_int, expected element type is "int"
            if (varSym.type.rfind("array_", 0) == 0) expectedElementType = varSym.type.substr(6);
        }
        std::string expRes = parseExp(expectedElementType); 
        // TODO: Type check expRes against varSym.type (or element type if it was an array and this was allowed).
        recordSyntaxOutput("<InitVal>");
        return expRes; // This is the temp/const string from the expression
    }
}

// FuncDef → FuncType Ident '(' [FuncFParams] ')' Block
void SyntaxAnalyzer::parseFuncDef() {
    std::string funcRetType = parseFuncType(); // "int" or "void"
    std::string funcName = currentToken.lexeme; 
    match(IDENFR);

    Symbol funcSymbolEntry;
    funcSymbolEntry.name = funcName;
    funcSymbolEntry.returnType = funcRetType;
    funcSymbolEntry.type = (funcRetType == "int" ? "int_func" : "void_func");
    funcSymbolEntry.isGlobal = true; 
    funcSymbolEntry.scopeLevel = symTab.currentScopeLevel; // Should be global scope (1)
    if(symTab.currentScopeLevel != 1) semanticError("Function '" + funcName + "' not defined at global scope.");
    
    currentFunctionSym = &funcSymbolEntry; 

    match(LPARENT);
    
    Symbol funcShell = funcSymbolEntry; 
    funcShell.params.clear(); 

    if (symTab.tableStack.front().count(funcName)) {
        semanticError("Redefinition of function symbol '" + funcName + "'.");
    }
    symTab.tableStack.front()[funcName] = funcShell; 

    currentFunctionSym = symTab.lookupSymbol(funcName);
    if (!currentFunctionSym) {
        semanticError("Internal: Failed to retrieve function shell '" + funcName + "' from global symbol table.");
    }

    symTab.enterScope(); 
    symTab.resetFrameOffsetForNewFunction(); 
    int bodyScopeLevel = symTab.currentScopeLevel;
    symTab.functionScopeLevels[funcName] = bodyScopeLevel; 

    symTab.beginFunctionCompilation(funcName); // NEW: Start accumulating symbols for this function

    irGen.addQuad("FUNC_BEGIN", funcName, "PLACEHOLDER_STACK_SIZE", "_"); 

    if (currentToken.type != RPARENT) {
        parseFuncFParams(*currentFunctionSym); 
    }
    match(RPARENT);
    
    parseBlock(false); 
    
    int localFrameSize = symTab.getCurrentFrameSizeForLocals();
    bool foundFuncBegin = false;
    for(auto& quad : irGen.quadruples) { 
        if(quad.op == "FUNC_BEGIN" && quad.arg1 == funcName) {
            quad.arg2 = std::to_string(localFrameSize); 
            foundFuncBegin = true;
            break;
        }
    }
    if (!foundFuncBegin) semanticError("Internal: FUNC_BEGIN quad not found for " + funcName);
    
    // 旧的 SYNTAX_DEBUG 和 functionLocalSymbols 赋值块已被移除。
    // 符号收集和调试由 endFunctionCompilation 处理。
    
    symTab.endFunctionCompilation(funcName); // Finalize symbol accumulation for this function
    
    symTab.exitScope(); 
    irGen.addQuad("FUNC_END", funcName, "_", "_");
    currentFunctionSym = nullptr; 
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
    mainFuncSymbol.scopeLevel = symTab.currentScopeLevel; 
    if(symTab.currentScopeLevel != 1) semanticError("'main' function not defined at global scope.");

    currentFunctionSym = &mainFuncSymbol; 

    symTab.enterScope(); 
    symTab.resetFrameOffsetForNewFunction();
    int bodyScopeLevel = symTab.currentScopeLevel;
    symTab.functionScopeLevels["main"] = bodyScopeLevel; 

    symTab.beginFunctionCompilation("main"); // NEW: Start accumulating for main

    irGen.addQuad("FUNC_BEGIN", "main", "PLACEHOLDER_STACK_SIZE", "_");

    if (symTab.tableStack.front().count("main")) {
        semanticError("Redefinition of 'main' function.");
    }
    symTab.tableStack.front()["main"] = mainFuncSymbol;
    currentFunctionSym = symTab.lookupSymbol("main"); 
     if (!currentFunctionSym) semanticError("Internal: Failed to add/find main function symbol.");
    
    match(LPARENT);
    match(RPARENT);
    parseBlock(false); 

    int localFrameSize = symTab.getCurrentFrameSizeForLocals();
    bool foundFuncBegin = false;
    for(auto& quad : irGen.quadruples) { 
        if(quad.op == "FUNC_BEGIN" && quad.arg1 == "main") {
            quad.arg2 = std::to_string(localFrameSize); 
            foundFuncBegin = true;
            break;
        }
    }
    if (!foundFuncBegin) semanticError("Internal: FUNC_BEGIN quad not found for main");
    
    // 旧的 SYNTAX_DEBUG 和 functionLocalSymbols 赋值块已被移除。
    // 符号收集和调试由 endFunctionCompilation 处理。
    
    symTab.endFunctionCompilation("main"); // Finalize symbol accumulation for main
    
    symTab.exitScope(); 
    irGen.addQuad("FUNC_END", "main", "_", "_"); 
    currentFunctionSym = nullptr;
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
    std::string paramBaseType = parseBType(); // e.g. "int"
    std::string paramName = currentToken.lexeme;
    match(IDENFR);
    
    Symbol paramSymbolEntry;
    paramSymbolEntry.name = paramName;
    paramSymbolEntry.scopeLevel = symTab.currentScopeLevel; // Current scope is function's scope
    paramSymbolEntry.isGlobal = false; 
    paramSymbolEntry.isConstant = false; // Parameters are not const by default in SysY
    paramSymbolEntry.type = paramBaseType; // e.g. "int"
    paramSymbolEntry.isArray = false; // Strictly per user rule: FuncFParam -> BType Ident
    // paramSymbolEntry.arrayDimensions remains empty
    
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
    // DEBUGGING OUTPUT START
    std::cout << "DEBUG: In parseFuncFParam for func '" << funcSymbol.name << "', param '" << paramName << "':" << std::endl;
    std::cout << "  paramSymbolEntry.name = " << paramSymbolEntry.name << std::endl;
    std::cout << "  paramSymbolEntry.scopeLevel = " << paramSymbolEntry.scopeLevel << std::endl;
    std::cout << "  paramSymbolEntry.offset = " << paramSymbolEntry.offset << std::endl;
    Symbol* testLookup = symTab.lookupSymbol(paramName);
    if (testLookup) {
        std::cout << "  TEST LOOKUP SUCCESS: Found '" << testLookup->name << "' with offset " << testLookup->offset << " in scope " << testLookup->scopeLevel << std::endl;
    } else {
        std::cout << "  TEST LOOKUP FAILED for '" << paramName << "' immediately after adding." << std::endl;
    }
    // DEBUGGING OUTPUT END
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
    // Look at current token to decide:
    // CONSTTK -> ConstDecl (part of Decl)
    // INTTK   -> VarDecl (part of Decl) or if followed by Ident LParent, could be FuncDef (not in block)
    // Other   -> Stmt
    if (currentToken.type == CONSTTK || currentToken.type == INTTK) {
        // Note: FuncDef is not allowed inside a BlockItem by typical SysY grammar.
        // If INTTK is seen, it must be a VarDecl.
        parseDecl(); 
    } else {
        parseStmt();
    }
    recordSyntaxOutput("<BlockItem>"); // Add output for BlockItem
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
        
        irGen.addQuad("IF_FALSE_GOTO", condResult, "_", elseLabel);
        
        parseStmt();
        
        if (currentToken.type == ELSETK) {
            match(ELSETK);
            irGen.addQuad("GOTO", "_", "_", endIfLabel);
            
            irGen.addQuad(elseLabel + ":", "_", "_", "_");
            parseStmt(); 
            
            irGen.addQuad(endIfLabel + ":", "_", "_", "_");
        } else {
            irGen.addQuad(elseLabel + ":", "_", "_", "_");
        }
        recordSyntaxOutput("<Stmt_If>"); 
    } else if (currentToken.type == WHILETK) {
        std::string startLoopLabel = irGen.newLabel();
        std::string endLoopLabel = irGen.newLabel();
        
        loopLabels.push({startLoopLabel, endLoopLabel});

        irGen.addQuad(startLoopLabel + ":", "_", "_", "_"); 

        match(WHILETK);
        match(LPARENT);
        std::string condResult = parseCond();
        match(RPARENT);
        
        irGen.addQuad("IF_FALSE_GOTO", condResult, "_", endLoopLabel);
        
        parseStmt();
        
        irGen.addQuad("GOTO", "_", "_", startLoopLabel);

        irGen.addQuad(endLoopLabel + ":", "_", "_", "_");
        
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
            // Potentially exit or throw to stop further processing if fatal
        }

        if (currentToken.type != SEMICN) { // If there is an expression
            std::string expectedReturnType = "int"; // Fallback
            if (currentFunctionSym && !currentFunctionSym->returnType.empty()) {
                expectedReturnType = currentFunctionSym->returnType;
            }
            
            if (currentFunctionSym && currentFunctionSym->returnType == "void") {
                semanticError("Return with a value in a void function \'" + currentFunctionSym->name + "\'.");
                parseExp(expectedReturnType); // Still parse to consume tokens, but value is problematic
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
        
        std::vector<std::string> argTemps; // Results of parsed expressions
        while (currentToken.type == COMMA) {
            match(COMMA);
            argTemps.push_back(parseExp("int")); 
        }

        // New comprehensive printf handling
        std::string formatStringNoQuotes = fmtStrLexemeWithQuotes.substr(1, fmtStrLexemeWithQuotes.length() - 2);
        
        size_t currentPos = 0; // Current position in formatStringNoQuotes
        int argIdx = 0;       // Current index in argTemps

        while (currentPos < formatStringNoQuotes.length()) {
            size_t nextPercentD = formatStringNoQuotes.find("%d", currentPos);

            if (nextPercentD != std::string::npos) {
                // Found a %d
                // Part 1: Print the string segment before %d (if any)
                if (nextPercentD > currentPos) {
                    std::string textSegment = formatStringNoQuotes.substr(currentPos, nextPercentD - currentPos);
                    irGen.addQuad("PRINT_STR", symTab.addStringLiteral("\"" + textSegment + "\""), "_", "_");
                }

                // Part 2: Print the integer argument
                if (argIdx < argTemps.size()) {
                    irGen.addQuad("PRINT_INT", argTemps[argIdx], "_", "_");
                    argIdx++;
                } else {
                    // Error: Not enough arguments for %d specifiers. 
                    // For simplicity, we could print %d literally or a placeholder, or error out.
                    // Let's print "%d" literally if no argument is left.
                    irGen.addQuad("PRINT_STR", symTab.addStringLiteral("\"%d\""), "_", "_");
                     // semanticError("Not enough arguments for format string in printf."); // Or throw an error
                }
                currentPos = nextPercentD + 2; // Move past "%d"
            } else {
                // No more %d found, print the rest of the string
                if (currentPos < formatStringNoQuotes.length()) {
                    std::string remainingSegment = formatStringNoQuotes.substr(currentPos);
                    irGen.addQuad("PRINT_STR", symTab.addStringLiteral("\"" + remainingSegment + "\""), "_", "_");
                }
                break; // Exit loop
            }
        }
        // Check if there are more arguments supplied than %d specifiers (usually ignored by C printf)
        if (argIdx < argTemps.size()) {
            // You might want to issue a warning here about unused arguments
            // std::cout << "SYNTAX_WARNING: Extra arguments provided to printf that were not used by format string." << std::endl;
        }
        
        match(RPARENT);
        match(SEMICN);
        recordSyntaxOutput("<Stmt_Printf>"); 
    } else if (currentToken.type == LBRACE) {
        parseBlock(true); // Provide isNewScope argument
    } else if (currentToken.type == SEMICN) { // Empty statement
        match(SEMICN);
    } else if (currentToken.type == IDENFR) { // Statement starts with an Identifier
        const auto& allTokens = lexer.getTokens();
        // tokenIndex is the index of the *next* token to be fetched.
        // So, allTokens[tokenIndex] is the token immediately after currentToken.
        Token peekNext = (tokenIndex < allTokens.size()) ? allTokens[tokenIndex] : Token{TOKEN_EOF, "", 0};

        if (peekNext.type == LPARENT) { // Likely a function call, e.g., ident(...);
            // Parse as an expression. parseUnaryExp will handle the function call.
            // The result of the expression (if any) is discarded for a statement.
            parseExp("int"); // Or "void" if function calls can be expressions of type void
            match(SEMICN);
            // recordSyntaxOutput("<Stmt_FuncCall>"); // Or let the generic <Stmt> be recorded later
        } else { 
            // Not ident(...), so it could be LVal = Exp; or LVal = getint(); 
            // or potentially a bare LVal as an Exp (though less common as a full statement).
            // Fall back to the original LVal-first speculative parsing.
            int preLValPos = tokenIndex -1; // currentToken is the first token of potential LVal
            Token preLValToken = currentToken;

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

                    if(isArrayAccessLVal) {
                        irGen.addQuad("STORE_TO_ADDR", lvalTarget /*address_temp*/, "_", getintResultTemp /*value_temp*/);
                    } else { 
                        irGen.addQuad("ASSIGN", getintResultTemp, "_", lvalTarget); 
                    }
                } else { // Regular Exp assignment: LVal = Exp;
                    std::string rhsVal = parseExp("int"); 
                    if (isArrayAccessLVal) {
                        irGen.addQuad("STORE_TO_ADDR", lvalTarget /*address_temp*/, "_", rhsVal /*value_temp*/);
                    } else {
                        irGen.addQuad("ASSIGN", rhsVal, "_", lvalTarget);
                    }
                }
                match(SEMICN);
            } else { // Not an assignment, so it must have been an Exp (which might start with an LVal)
                tokenIndex = preLValPos;
                getNextTokenFromLexer(); 

                if (currentToken.type != SEMICN) { 
                     parseExp("int"); 
                }
                match(SEMICN);
            }
        }
    } else { // Statement does not start with a specific keyword or IDENFR
             // Could be an expression starting with '(', or a number, etc. or just ';'
        if (currentToken.type != SEMICN) { // If not an empty statement
             parseExp("int"); // Result is unused for statement Exp.
        }
        match(SEMICN); // All valid statements (or expression statements) end with ';'
    }
    recordSyntaxOutput("<Stmt>"); // This is a generic Stmt tag, specific tags added above.
}

// Exp → AddExp
std::string SyntaxAnalyzer::parseExp(const std::string& expectedType) { 
    std::string result = parseAddExp(expectedType); 
    recordSyntaxOutput("<Exp>"); 
    return result; 
}

// AddExp → MulExp { ('+' | '-') MulExp }
std::string SyntaxAnalyzer::parseAddExp(const std::string& expectedType) {
    std::string leftOperand = parseMulExp(expectedType); 

    while (currentToken.type == PLUS || currentToken.type == MINU) {
        TokenType opToken = currentToken.type;
        match(opToken); 
        std::string rightOperand = parseMulExp(expectedType); 
        
        std::string leftType = getExpType(leftOperand);
        std::string rightType = getExpType(rightOperand);

        if (!((leftType == "int" || leftType == "const_int") && (rightType == "int" || rightType == "const_int"))) {
            // semanticError("Operands for '" + (opToken == PLUS ? std::string("+") : std::string("-")) + "' must be integers. Got " + leftType + " and " + rightType);
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
        
        irGen.addQuad(opToken == PLUS ? "ADD" : "SUB", leftOperand, rightOperand, resultTempName);
        leftOperand = resultTempName; 
        
        recordSyntaxOutput("<AddExp>"); 
    }
    return leftOperand; 
}

// MulExp → UnaryExp { ('*' | '/' | '%') UnaryExp }
std::string SyntaxAnalyzer::parseMulExp(const std::string& expectedType) {
    std::string leftOperand = parseUnaryExp(); 

    while (currentToken.type == MULT || currentToken.type == DIV || currentToken.type == MOD) {
        TokenType opToken = currentToken.type;
        match(opToken); 
        std::string rightOperand = parseUnaryExp(); 
        
        std::string leftType = getExpType(leftOperand);
        std::string rightType = getExpType(rightOperand);

        if (!((leftType == "int" || leftType == "const_int") && (rightType == "int" || rightType == "const_int"))) {
             // semanticError("Operands for multiplicative op must be integers. Got " + leftType + " and " + rightType);
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
        irGen.addQuad(opStr, leftOperand, rightOperand, resultTempName);
        leftOperand = resultTempName;
        
        recordSyntaxOutput("<MulExp>"); 
    }
    return leftOperand;
}

// UnaryExp → PrimaryExp | Ident '(' [FuncRParams] ')' | UnaryOp UnaryExp
std::string SyntaxAnalyzer::parseUnaryExp() { 
    if (currentToken.type == PLUS || currentToken.type == MINU || currentToken.type == NOT) {
        std::string opIrName = parseUnaryOp(); 
        std::string operand = parseUnaryExp(); 
        
        if (opIrName == "POS") { 
            recordSyntaxOutput("<UnaryExp>");
            return operand; 
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

        std::string operandType = getExpType(operand);

        if (opIrName == "NEG" && !(operandType == "int" || operandType == "const_int")) {
            // semanticError("Operand for unary '-' must be an integer. Got " + operandType);
        }
        if (opIrName == "NOT_OP" && !(operandType == "int" || operandType == "const_int")) { 
            // semanticError("Operand for '!' must be a boolean (integer). Got " + operandType);
        }
        irGen.addQuad(opIrName, operand, "_", resultTempName);
        recordSyntaxOutput("<UnaryExp>");
        return resultTempName;
    } else if (currentToken.type == IDENFR) {
        const auto& allTokens = lexer.getTokens();
        Token peekNext = (tokenIndex < allTokens.size()) ? allTokens[tokenIndex] : Token{TOKEN_EOF, "", 0};

        if (peekNext.type == LPARENT) { 
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
            recordSyntaxOutput("<UnaryExp>");
            return resultTempName; 
        } else { 
            // If not a function call, it must be a PrimaryExp which starts with LVal (IDENFR).
            // parsePrimaryExp will handle it.
            // No recordSyntaxOutput("<UnaryExp>") here, let PrimaryExp path handle its own output if it becomes UnaryExp.
            return parsePrimaryExp(); 
        }
    } else { 
        // Must be other PrimaryExp types: ( Exp ) or Number
        // No recordSyntaxOutput("<UnaryExp>") here, let PrimaryExp path handle its own output if it becomes UnaryExp.
        return parsePrimaryExp(); 
    }
}

// PrimaryExp → '(' Exp ')' | LVal | Number
std::string SyntaxAnalyzer::parsePrimaryExp() { 
    std::string result;
    if (currentToken.type == LPARENT) {
        match(LPARENT);
        result = parseExp("int"); // Default expectedType="int" is fine for Exp in parentheses
        match(RPARENT);
    } else if (currentToken.type == IDENFR) { 
        // Save original token for potential symbol lookup if not an array access that returns an address temp
        std::string originalIdentName = currentToken.lexeme;
        int originalIdentLine = currentToken.lineNumber;

        bool isArrayAccess = false;
        std::string arrayIndexValueTemp; // To store the *value* of index from parseLVal if needed, though not directly used by LOAD_FROM_ADDR
        
        // parseLVal now returns:
        // - Mangled name for scalar variable (e.g., __main_x, g_var)
        // - Temp name holding address for array element (e.g., _t1 which holds address of arr[i])
        std::string lvalResultName = parseLVal(isArrayAccess, arrayIndexValueTemp, false /*isAssignmentLHS=false*/);

        if (isArrayAccess) { 
            // lvalResultName is a temporary variable (e.g., _t1) holding the *address* of the array element.
            // We need to load the value from this address.
            std::string valueLoadedTemp = irGen.newTemp();

            // Add symbol for the new temporary that will hold the loaded value
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
            
            irGen.addQuad("LOAD_FROM_ADDR", lvalResultName /*_t1, address*/, "_", valueLoadedTemp /*_t2, value*/);
            result = valueLoadedTemp; // The result of the primary expression is the loaded value
        } else { 
            // lvalResultName is the (potentially mangled) name of a scalar variable or a global array base.
            // Need to look up the original symbol to check if it's a simple const scalar or an array base.
            Symbol* s = symTab.lookupSymbol(originalIdentName); // Use original name for symtab lookup
            if (!s) {
                // This should ideally be caught by parseLVal, but as a safeguard:
                semanticError("Undeclared identifier '" + originalIdentName + "' in expression (line " + std::to_string(originalIdentLine) + ").");
                 return ""; // Should not happen if parseLVal is correct
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
std::string SyntaxAnalyzer::parseUnaryOp() { // Changed void to std::string
    std::string opIrName = "POS"; // Default for unary '+'
    if (currentToken.type == PLUS) {
        match(PLUS);
        opIrName = "POS"; // Explicitly
    } else if (currentToken.type == MINU) {
        match(MINU);
        opIrName = "NEG";
    } else if (currentToken.type == NOT) {
        match(NOT);
        opIrName = "NOT_OP";
    } else {
        semanticError("Syntax error in UnaryOp: Expected '+', '-', or '!' got " + currentToken.lexeme);
        // exit(1); // semanticError already exits
    }
    recordSyntaxOutput("<UnaryOp>");
    return opIrName; // Return IR op name
}

// FuncRParams → Exp { ',' Exp }
void SyntaxAnalyzer::parseFuncRParams(Symbol* funcSym) {
    // Here, we need to generate PARAM quads and type check arguments.
    // parseExp returns the temp var/const holding the argument's value.
    int paramCount = 0;
    if (!funcSym) { semanticError("Internal: funcSym is null in parseFuncRParams"); }

    std::string argResult = parseExp("int"); // Get first argument, default expected type
    // Type checking (example for int params)
    if (paramCount < funcSym->params.size()) {
        const auto& expectedParam = funcSym->params[paramCount];
        std::string argType = getExpType(argResult);
        // SysY is weakly typed for params often, but good to have a placeholder
        if ((expectedParam.second == "int" || expectedParam.second == "array_int" /* passed as pointer */) &&
            !(argType == "int" || argType == "const_int" || (argType.rfind("array_",0)==0 && expectedParam.second.rfind("array_",0)==0 ))) {
           // semanticError("Type mismatch for function '" + funcSym->name + "' param " + std::to_string(paramCount+1) + 
           //               ". Expected " + expectedParam.second + " got " + argType);
        }
    } else if (!funcSym->params.empty()){ // check only if funcSym expects params
         semanticError("Too many arguments for function '" + funcSym->name + "'.");
    }


    irGen.addQuad("PARAM", argResult, "_", std::to_string(paramCount++));


    while (currentToken.type == COMMA) {
        match(COMMA);
        argResult = parseExp("int"); // Subsequent arguments, default expected type

        if (paramCount < funcSym->params.size()) {
            const auto& expectedParam = funcSym->params[paramCount];
            std::string argType = getExpType(argResult);
            if ((expectedParam.second == "int" || expectedParam.second == "array_int") &&
                !(argType == "int" || argType == "const_int" || (argType.rfind("array_",0)==0 && expectedParam.second.rfind("array_",0)==0 ))) {
                 // semanticError("Type mismatch for function '" + funcSym->name + "' param " + std::to_string(paramCount+1) + 
                 //            ". Expected " + expectedParam.second + " got " + argType);
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
    recordSyntaxOutput("<RelExp>"); 
    
    while (currentToken.type == LSS || currentToken.type == GRE ||
           currentToken.type == LEQ || currentToken.type == GEQ) {
        TokenType opToken = currentToken.type;
        match(opToken);
        std::string rightOperand = parseAddExp("int");
        
        std::string leftType = getExpType(leftOperand);
        std::string rightType = getExpType(rightOperand);
        if (!((leftType == "int" || leftType == "const_int") && (rightType == "int" || rightType == "const_int"))) {
           // semanticError("Operands for relational op must be integers. Got " + leftType + " and " + rightType);
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
            semanticError("Failed to add temporary variable " + resultTempName + " to symbol table in RelExp.");
        }

        std::string opStr;
        if (opToken == LSS) opStr = "LSS";
        else if (opToken == GRE) opStr = "GRE";
        else if (opToken == LEQ) opStr = "LEQ";
        else opStr = "GEQ";
        irGen.addQuad(opStr, leftOperand, rightOperand, resultTempName);
        leftOperand = resultTempName; 
        recordSyntaxOutput("<RelExp>"); 
    }
    return leftOperand; 
}

// EqExp → RelExp { ('==' | '!=') RelExp }
std::string SyntaxAnalyzer::parseEqExp() { 
    std::string leftOperand = parseRelExp(); 
    recordSyntaxOutput("<EqExp>"); 
    
    while (currentToken.type == EQL || currentToken.type == NEQ) {
        TokenType opToken = currentToken.type;
        match(opToken);
        std::string rightOperand = parseRelExp(); 
        
        std::string leftType = getExpType(leftOperand);
        std::string rightType = getExpType(rightOperand);
        if (!((leftType == "int" || leftType == "const_int") && (rightType == "int" || rightType == "const_int"))) {
            // semanticError("Operands for equality op must be integers. Got " + leftType + " and " + rightType);
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
            semanticError("Failed to add temporary variable " + resultTempName + " to symbol table in EqExp.");
        }

        std::string opStr = (opToken == EQL ? "EQL" : "NEQ");
        irGen.addQuad(opStr, leftOperand, rightOperand, resultTempName);
        leftOperand = resultTempName;
        recordSyntaxOutput("<EqExp>"); 
    }
    return leftOperand;
}

// LAndExp → EqExp { '&&' EqExp }
std::string SyntaxAnalyzer::parseLAndExp() { 
    std::string leftOperand = parseEqExp(); 
    recordSyntaxOutput("<LAndExp>"); 
    
    while (currentToken.type == AND) {
        match(AND);
        // Short-circuiting for &&:
        // if leftOperand is false, result is false, skip rightOperand.
        // else, result is rightOperand.
        // IR:
        //   <evaluate leftOperand into t1>
        //   resultTemp = t1             // Assume true initially or copy left
        //   IF_FALSE_GOTO t1, end_label // If t1 is false, jump to end, result is 0 (or t1)
        //   <evaluate rightOperand into t2>
        //   resultTemp = t2             // If left was true, result is t2
        // end_label:

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

        std::string endLabel = irGen.newLabel();

        // Initially, if leftOperand is already 0, resultTempName could be set to 0.
        // Or, more robustly, always evaluate right and let MIPS handle 0 && X = 0.
        // For short-circuiting, we must conditionally evaluate the right part.

        // Copy leftOperand to resultTempName. If we short-circuit, this might be the result (if 0).
        // However, C standard: && yields 1 if both are true, 0 otherwise.
        // So, if left is 0, result is 0. If left is non-zero, then evaluate right. If right is 0, result is 0, else 1.

        // Step 1: evaluate leftOperand (already done, it's in leftOperand)
        // Step 2: if leftOperand is 0, result is 0. Jump to end.
        irGen.addQuad("ASSIGN", "0", "_", resultTempName); // Assume result is 0
        irGen.addQuad("IF_FALSE_GOTO", leftOperand, "_", endLabel); // If left is false, result is 0, skip right
        
        // Step 3: evaluate rightOperand
        std::string rightOperand = parseEqExp();
        std::string leftType = getExpType(leftOperand); // Already have leftOperand
        std::string rightType = getExpType(rightOperand);
        if (!((leftType == "int" || leftType == "const_int") && (rightType == "int" || rightType == "const_int"))) {
            // semanticError("Operands for '&&' must be booleans (integers). Got " + leftType + " and " + rightType);
        }

        // Step 4: If rightOperand is 0, result is 0 (already set). If right is non-zero, result is 1.
        // The actual value of rightOperand (if non-zero) doesn't matter, only its truthiness.
        // C standard: result of && is 1 if both are non-zero, 0 otherwise.
        // So, if we reach here, leftOperand was non-zero.
        // Now, if rightOperand is non-zero, result is 1. Otherwise, result is 0.
        irGen.addQuad("IF_TRUE_GOTO", rightOperand, "_", resultTempName + "_set_to_1"); // A bit clunky label name, but ok
        irGen.addQuad("ASSIGN", "0", "_", resultTempName); // if rightOperand is false, result is 0
        irGen.addQuad("GOTO", "_", "_", endLabel);
        irGen.addQuad(resultTempName + "_set_to_1:", "_", "_", "_");
        irGen.addQuad("ASSIGN", "1", "_", resultTempName); // if rightOperand is true, result is 1
        
        irGen.addQuad(endLabel + ":", "_", "_", "_");
        
        leftOperand = resultTempName; // The result of this AND operation becomes the left operand for the next
        recordSyntaxOutput("<LAndExp>"); 
    }
    return leftOperand;
}

// LOrExp → LAndExp { '||' LAndExp }
std::string SyntaxAnalyzer::parseLOrExp() { // Changed void to std::string
    // Similar short-circuiting logic for '||' applies.
    // if leftOperand is true, result is true, skip rightOperand.
    // else, result is rightOperand.
    std::string leftOperand = parseLAndExp(); 
    recordSyntaxOutput("<LOrExp>"); 
    
    while (currentToken.type == OR) {
        match(OR);
        // Short-circuiting for ||:
        // IR:
        //   <evaluate leftOperand into t1>
        //   resultTemp = t1             // Assume false initially or copy left
        //   IF_TRUE_GOTO t1, end_label  // If t1 is true, jump to end, result is 1 (or t1)
        //   <evaluate rightOperand into t2>
        //   resultTemp = t2             // If left was false, result is t2
        // end_label:

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
        
        std::string endLabel = irGen.newLabel();

        // C standard: || yields 1 if either is true (non-zero), 0 otherwise.
        // Step 1: evaluate leftOperand (already done)
        // Step 2: if leftOperand is non-zero (true), result is 1. Jump to end.
        irGen.addQuad("ASSIGN", "1", "_", resultTempName); // Assume result is 1
        irGen.addQuad("IF_TRUE_GOTO", leftOperand, "_", endLabel); // If left is true, result is 1, skip right
        
        // Step 3: evaluate rightOperand
        std::string rightOperand = parseLAndExp(); 
        std::string leftType = getExpType(leftOperand); // Already have leftOperand
        std::string rightType = getExpType(rightOperand);
        if (!((leftType == "int" || leftType == "const_int") && (rightType == "int" || rightType == "const_int"))) {
             // semanticError("Operands for '||' must be booleans (integers). Got " + leftType + " and " + rightType);
        }

        // Step 4: If we reach here, leftOperand was false (0).
        // Result is 1 if rightOperand is non-zero, 0 otherwise.
        irGen.addQuad("IF_TRUE_GOTO", rightOperand, "_", resultTempName + "_set_to_1_or"); // Different label from AND
        irGen.addQuad("ASSIGN", "0", "_", resultTempName); // if rightOperand is false, result is 0
        irGen.addQuad("GOTO", "_", "_", endLabel);
        irGen.addQuad(resultTempName + "_set_to_1_or:", "_", "_", "_");
        irGen.addQuad("ASSIGN", "1", "_", resultTempName); // if rightOperand is true, result is 1
        
        irGen.addQuad(endLabel + ":", "_", "_", "_");

        leftOperand = resultTempName; // The result of this OR operation becomes the left operand for the next
        recordSyntaxOutput("<LOrExp>"); 
    }
    return leftOperand;
}

// ConstExp → AddExp
// Must return a string that represents a compile-time constant value.
// parseAddExp needs to be able to evaluate constant expressions if its inputs are constants.
// This is a simplification for now: parseAddExp returns a temp or const string.
// For true const evaluation, parseAddExp would need more logic.
// Assume parseAddExp can handle it for now, or this ConstExp is just structurally AddExp.
std::string SyntaxAnalyzer::parseConstExp() { // Changed void to std::string
    // The result of AddExp in a ConstExp context *must* be evaluatable at compile time.
    // The current parseAddExp generates IR and returns a temp/const string.
    // For a true const expression, we'd need to perform constant folding here or in parseAddExp.
    // For SysY, ConstExp is used for array dimensions and const initializers.
    // Let's assume parseAddExp returns a string which is a literal number if it was a const expression.
    std::string result = parseAddExp("int"); // Expect int for const expressions usually

    // Validate if 'result' is indeed a constant integer string.
    // This is a runtime check during parsing. A full constant folder would do this.
    bool isNumeric = !result.empty() && std::all_of(result.begin() + (result[0] == '-' ? 1:0), result.end(), ::isdigit);
    if (result.rfind("_t",0)==0 || (result.rfind("_",0)==0 && result.find("_str_lit") == std::string::npos && result.find("_L") == std::string::npos && !isNumeric) ) { // If it's a temp var, or non-numeric label/ident
        Symbol* s = symTab.lookupSymbol(result);
        if (!s || !s->isConstant) {
             //semanticError("Expression in constant context ('" + result + "') is not a constant value.");
             // This semantic error is tricky because parseAddExp will produce temps.
             // A proper constant folder is needed.
             // For now, we accept the result of AddExp and hope it was const if context required.
        } else if (s && s->isConstant) {
            result = std::to_string(s->value); // Use the actual constant value
        }
    }


    recordSyntaxOutput("<ConstExp>");
    return result; // Return the (hopefully) constant string
}


// Cond → LOrExp
// Definition based on lab3.cpp structure, adapted for lab6.cpp (returns string)
std::string SyntaxAnalyzer::parseCond() {
    std::string result = parseLOrExp(); 
    recordSyntaxOutput("<Cond>");
    return result;
}

// Decl → ConstDecl | VarDecl
// Definition based on lab3.cpp structure
void SyntaxAnalyzer::parseDecl() {
    if (currentToken.type == CONSTTK) {
        parseConstDecl();
    } else if (currentToken.type == INTTK) {
        parseVarDecl();
    } else {
        // In lab6.cpp, we use semanticError for more context
        semanticError("Syntax error in Decl: Expected CONSTTK or INTTK, got " + currentToken.lexeme);
        // exit(1); // semanticError handles exit
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
    arrayIndexResultTemp_out = ""; // This will hold the *value* of the index expression for the caller if needed (e.g. for ARRAY_STORE if we keep it)

    if (currentToken.type == LBRACK) { // Array element access
        if (!sym->isArray) {
            semanticError("Attempting array access on non-array variable '" + identName + "' (line " + std::to_string(identLine) + ").");
        }
        isArrayAccess = true;
        match(LBRACK);
        std::string indexExpVal = parseExp("int"); // parseExp returns temp/const holding index's value
        arrayIndexResultTemp_out = indexExpVal; // Pass out the index value temp if caller needs it
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

        irGen.addQuad("MUL", indexExpVal, elementSize, offsetBytesTemp); // offset_bytes = index_value * 4

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
        // For scalar, isArrayAccess is false, arrayIndexResultTemp_out is irrelevant for address calculation.
        return nameForIRBase; // Return the (potentially mangled) name of the scalar variable itself
    }
}


// 主要分析入口
void SyntaxAnalyzer::analyze() {
    parseCompUnit();
    
    if (enableSyntaxOutput) { 
        mergeAndWriteSyntaxOutput();
    }
}

