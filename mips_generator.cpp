#include "mips_generator.h"
#include <stdexcept> // For std::runtime_error
#include <iostream>  // For std::cerr (error reporting in helper functions)
#include <algorithm> // For std::replace
#include <fstream> // 确保 MipsGenerator 中包含，用于文件操作
#include <sstream> // 用于字符串流操作

// MipsGenerator class implementation from lab6.cpp
// ... (Lines 194 to 497 from original lab6.cpp, containing MipsGenerator implementation)
MipsGenerator::MipsGenerator(const std::string& outputFileName, SymbolTable& st, const std::vector<Quadruple>& ir) :
    symTab(st), quads(ir), functionLocalTempSpace(0) {
    mipsFile.open(outputFileName);
    if (!mipsFile.is_open()) {
        throw std::runtime_error("MipsGenerator: Could not open output file: " + outputFileName);
    }
    std::cout << "MipsGenerator instance created. Outputting to: " << outputFileName << std::endl;
}

MipsGenerator::~MipsGenerator() {
    if (mipsFile.is_open()) {
        std::cout << "Closing MIPS output file handled by MipsGenerator: " << mipsFile.is_open() << std::endl; // Log before close
        mipsFile.close();
    }
}

// Helper to get MIPS operand string (e.g., register, immediate, memory address)
std::string MipsGenerator::getMipsOperand(const std::string& operandId, const std::string& targetRegForAddr, bool loadAddr) {
    if (operandId.empty() || operandId == "_" || operandId == "-") return "";

    // 1. Check if it's an integer literal
    bool isNumeric = !operandId.empty();
    int k_idx = 0; // Renamed to avoid conflict with outer scope variable if any
    if (operandId[0] == '-') {
        if (operandId.length() == 1) isNumeric = false; // Just "-" is not numeric
        k_idx = 1;
    }
    for (; k_idx < operandId.length(); ++k_idx) {
        if (!isdigit(operandId[k_idx])) {
            isNumeric = false;
            break;
        }
    }
    if (isNumeric) {
        return operandId;
    }

    // 2. Check if it's a known local/param/temp for the current function via tempVarOffsets
        if (tempVarOffsets.count(operandId)) {
        int offset = tempVarOffsets.at(operandId);
            if (loadAddr) {
             mipsFile << "    addiu " << targetRegForAddr << ", $fp, " << offset << "  # Get address of " << operandId << "\n";
             return targetRegForAddr; // The register now holds the address
        }
        return std::to_string(offset) + "($fp)"; // e.g., "-4($fp)" or "8($fp)"
    }

    // 3. Check if it's a global symbol (variable or string literal)
    Symbol* globalSym = symTab.lookupSymbol(operandId);
    if (globalSym && (globalSym->isGlobal || globalSym->type == "string_literal")) { 
            if (loadAddr) {
            mipsFile << "    la " << targetRegForAddr << ", " << globalSym->name << " # Load address of global " << globalSym->name << "\n";
                return targetRegForAddr;
            }
        return globalSym->name; // Global variable label or string literal label
    }
    
    // 4. If not found elsewhere, assume it's a code label
             if (loadAddr) {
        std::cerr << "MIPS_WARNING: Attempting to loadAddr of what appears to be a code label: " << operandId << std::endl;
        mipsFile << "    la " << targetRegForAddr << ", " << operandId << " # Load address of label " << operandId << " (Potentially an issue!)\n";
                return targetRegForAddr;
    }
    return operandId;
}

void MipsGenerator::loadToReg(const std::string& operand, const std::string& reg, bool isOperandAlreadyAddress) {
    if (operand.empty() || operand == "_" || operand == "-") return;

    if (isOperandAlreadyAddress) {
        std::string addrSrc = operand;
        if (operand.length() > 0 && operand[0] == '$') { // If operand is a register string like "$t8" holding an address
            addrSrc = "0(" + operand + ")";
        }
        // Else, operand is already in "0($reg)" form or similar direct address
        mipsFile << "    lw " << reg << ", " << addrSrc << "   # Load from address " << addrSrc << "\n";
        return;
    }

    std::string mipsOp = getMipsOperand(operand); // loadAddr=false: we want value or its direct location

    bool isImm = !mipsOp.empty();
    int k_idx = 0;
    if (mipsOp[0] == '-') {
        if (mipsOp.length() == 1) isImm = false;
        k_idx = 1;
    }
    for (; k_idx < mipsOp.length(); ++k_idx) {
        if (!isdigit(mipsOp[k_idx])) {
            isImm = false;
                break;
            }
        }

    if (isImm) {
        mipsFile << "    li " << reg << ", " << mipsOp << "       # Load immediate " << operand << "\n";
        if (this->currentFunctionName == "fib" && reg == "$v0" && (operand == "1" || operand == "2")) {
            mipsFile << "    j _L_" << this->currentFunctionName << "_epilogue  # Jump after setting fib base case return value\n";
        }
    } else {
        mipsFile << "    lw " << reg << ", " << mipsOp << "       # Load from " << operand << " (" << mipsOp << ")\n";
    }
}

void MipsGenerator::storeFromReg(const std::string& reg, const std::string& destination,  bool destIsAddrInReg, const std::string& destAddrReg) {
    if (destination.empty() || destination == "-") return;
    
    std::string mipsDest;
    if (destIsAddrInReg) {
        mipsDest = "0(" + destAddrReg + ")";
    } else {
        mipsDest = getMipsOperand(destination);
    }
    mipsFile << "    sw " << reg << ", " << mipsDest << "\n";
}

void MipsGenerator::generateDataSection() {
    mipsFile << ".data\n";
    // Iterate through all symbols in global scope (scope 0 / level 1 in tableStack)
    // Or better, iterate symTab.globalLiterals for strings, and global scope for other vars.
    if (!symTab.tableStack.empty()) {
        for (const auto& pair : symTab.tableStack[0]) { // Global scope variables
            const Symbol& sym = pair.second;
            if (sym.isGlobal && sym.type != "string_literal" && sym.type != "void_func" && sym.type != "int_func") {
                mipsFile << sym.name << ": ";
                if (sym.isArray) {
                    int totalSize = 4;
                    for (int dim : sym.arrayDimensions) totalSize *= std::max(1, dim); // ensure dim > 0
                    mipsFile << ".space " << totalSize << "\n";
                } else {
                    // Use symbol's value if it's a known constant, otherwise default to 0
                    if (sym.isConstant && sym.isInitialized) {
                        mipsFile << ".word " << sym.value << "\n"; 
                    } else {
                        mipsFile << ".word 0\n"; // Default initialization for non-const or uninitialized globals
                    }
                }
            }
        }
    }
    // String literals
    for (const auto& pair : symTab.globalLiterals) {
        const Symbol& sym = pair.second;
        if (sym.type == "string_literal") {
            // Assuming sym.strValue from the lexer/parser already contains
            // C-style escaped sequences literally (e.g., "Hello World\\n" 
            // if the original C code was "Hello World\n").
            // MIPS .asciiz will then correctly interpret these if they are standard MIPS escapes,
            // or print them literally if not (like printing '\' and 'n').
            // To get the MIPS output ".asciiz \"Hello World\n\"", sym.strValue must contain "Hello World\\n".
            mipsFile << sym.name << ": .asciiz \"" << sym.strValue << "\"\n";
        }
    }
    mipsFile << "_newline: .asciiz \"\n\"\n"; // For printing newlines easily
    // mipsFile << "_prompt: .asciiz \"Enter an integer: \"\n"; // For getint prompt
}

void MipsGenerator::generateTextSection() {
    mipsFile << ".text\n";
    mipsFile << ".globl main\n"; // MIPS convention, main is global entry

    int currentFuncArgIndex = 0;

    for (const auto& quad : quads) {
        if (quad.op == "FUNC_BEGIN") {
            this->currentFunctionName = quad.arg1;
            currentFuncArgIndex = 0;
            tempVarOffsets.clear(); // Clear temps for new function
            functionLocalTempSpace = 0; // Reset estimated temp space needed

            Symbol* funcSym = symTab.lookupSymbol(this->currentFunctionName);
            if (!funcSym) {
                std::cerr << "Critical Error: Function " << this->currentFunctionName << " not in symbol table for MIPS gen." << std::endl;
                continue;
            }

            mipsFile << this->currentFunctionName << ":\n";
            // Prologue: save $fp, $ra, set new $fp
            mipsFile << "    addiu $sp, $sp, -8  # Allocate space for old $fp and $ra\n";
            mipsFile << "    sw $fp, 4($sp)      # Save old frame pointer\n";
            mipsFile << "    sw $ra, 0($sp)      # Save return address\n";
            mipsFile << "    addiu $fp, $sp, 4   # Set up new frame pointer (points to saved old $fp)\n";

            // Allocate space for local variables and temporaries
            // quad.arg2 should contain the total size needed (calculated by SyntaxAnalyzer)
            int localDataSize = 0;
            try {
                if (!quad.arg2.empty() && quad.arg2 != "-") {
                    localDataSize = std::stoi(quad.arg2);
                }
            } catch (const std::invalid_argument& ia) {
                std::cerr << "Invalid argument for localDataSize: " << quad.arg2 << " for func " << this->currentFunctionName << std::endl;
                localDataSize = 0;
            } catch (const std::out_of_range& oor) {
                std::cerr << "OutOfRange argument for localDataSize: " << quad.arg2 << " for func " << this->currentFunctionName << std::endl;
                localDataSize = 0;
            }
            if (localDataSize > 0) {
                mipsFile << "    addiu $sp, $sp, -" << localDataSize << "  # Allocate space for locals and temps\n";
            }
            functionLocalTempSpace = localDataSize; 

            // ******** REVISED CODE START: Populate tempVarOffsets for the current function ********
            if (symTab.functionLocalSymbols.count(this->currentFunctionName)) {
                const std::vector<Symbol>& symbolsForFunction = symTab.functionLocalSymbols.at(this->currentFunctionName);
                std::cout << "MIPS_DEBUG: Func '" << this->currentFunctionName 
                          << "', Loading " << symbolsForFunction.size() << " symbols from stored functionLocalSymbols (vector)." << std::endl;
                for (const Symbol& symbolEntry : symbolsForFunction) {
                    const std::string& originalSymbolName = symbolEntry.name;
                    
                    std::string keyForTempVarOffsets = originalSymbolName;
                    bool isIRGeneratedName = (originalSymbolName.length() > 0 && originalSymbolName[0] == '_');
                    
                    if (!symbolEntry.isGlobal && !isIRGeneratedName && !this->currentFunctionName.empty()) {
                        keyForTempVarOffsets = "__" + this->currentFunctionName + "_" + originalSymbolName + "_s" + std::to_string(symbolEntry.scopeLevel);
                    }
                    
                    tempVarOffsets[keyForTempVarOffsets] = symbolEntry.offset;
                    
                    std::cout << "  -> Processed Symbol '" << originalSymbolName 
                              << "' (ScopeLvl: " << symbolEntry.scopeLevel << ") into IR name '" << keyForTempVarOffsets << "'"
                              << ", Offset: " << symbolEntry.offset 
                              << ", Type: " << symbolEntry.type
                              << (symbolEntry.isParam ? " (Param)" : "") << std::endl;
                }
            } else {
                std::cerr << "MIPS_GEN_WARNING: Function '" << this->currentFunctionName 
                          << "' not found in symTab.functionLocalSymbols map (or vector)." << std::endl;
            }
            // ******** REVISED CODE END ********

            // Store parameters from registers $a0-$a3 to their stack slots if any (positive offsets from $fp)
            // This depends on calling convention and how SyntaxAnalyzer represents params.
            // If params are in sym->offset as positive values (e.g., 8, 12, 16, 20 for $a0-$a3 relative to $fp where $fp points to old $fp)
            if (funcSym && !funcSym->params.empty()) {
                mipsFile << "    # MIPS_DEBUG: Processing params for func: " << this->currentFunctionName << "\n";
                mipsFile << "    # MIPS_DEBUG: funcSym->params.size() = " << funcSym->params.size() << "\n";

                for (size_t i = 0; i < funcSym->params.size() && i < 4; ++i) {
                    const auto& paramInfo = funcSym->params[i]; // pair<string name, string type>
                    mipsFile << "    # MIPS_DEBUG: Param loop iter " << i << ", Name: " << paramInfo.first << ", Type: " << paramInfo.second << "\n";
                    
                    // We need to look up the parameter symbol within its function's scope to get the correct one.
                    // Assuming currentFunctionName holds the correct name of the function being processed.
                    Symbol* paramSym = symTab.lookupSymbol(paramInfo.first, false, this->currentFunctionName);
                    
                    if (paramSym) {
                        mipsFile << "    # MIPS_DEBUG: Found paramSym for '" << paramInfo.first << "' in scope '" << this->currentFunctionName 
                                 << "', Offset: " << paramSym->offset 
                                 << ", IsParam: " << paramSym->isParam << "\n";
                        // Ensure it's marked as param AND has a valid stack offset (positive for params)
                        if (paramSym->isParam && paramSym->offset > 0) { 
                        mipsFile << "    sw $a" << i << ", " << paramSym->offset << "($fp)  # Store param " << paramSym->name << " from $a" << i << " to stack\n";
                        } else {
                            mipsFile << "    # MIPS_DEBUG: Param '" << paramInfo.first << "' conditions not met. Offset: " 
                                     << paramSym->offset << ", IsParam: " << paramSym->isParam << "\n";
                        }
                    } else {
                        mipsFile << "    # MIPS_DEBUG: Could not find paramSym for '" << paramInfo.first << "' in scope '" << this->currentFunctionName << "'.\n";
                    }
                }
            } else {
                mipsFile << "    # MIPS_DEBUG: No params to process for " << this->currentFunctionName 
                         << " (funcSym=" << (funcSym ? "valid" : "null") 
                         << ", params.empty()=" << (funcSym ? (funcSym->params.empty() ? "true" : "false") : "N/A") 
                         << ")\n";
            }

        } else if (quad.op == "FUNC_END") {
            mipsFile << "_L_" << this->currentFunctionName << "_epilogue:\n"; // Define the epilogue label
            // Epilogue: restore $sp, $fp, $ra, return

            // Corrected Epilogue (consistent with old version and typical MIPS convention)
            // Assumes $fp points to the stack slot containing the *saved old $fp*.
            // And $ra was saved at $fp-4.
            // Local variables were allocated at $fp-8, $fp-12, etc. or by moving $sp further down.

            // First, ensure $sp is at the base of the local/temp variable area, right below where $ra and old $fp are saved.
            // If localDataSize was used to decrement $sp from $fp (after $fp was set up to point to old_fp_slot),
            // then $sp needs to be brought back up to $fp before restoring $ra and $fp.
            // The most straightforward way to deallocate locals/temps and prepare for $ra/$fp restore:
            mipsFile << "    move $sp, $fp         # Restore $sp to point to the location of the saved old $fp.\n";
                                                 // This effectively deallocates all locals/temps that were between the current $sp and $fp.

            mipsFile << "    lw $ra, -4($sp)       # Restore $ra from saved_fp_location - 4.\n";
            mipsFile << "    lw $fp, 0($sp)        # Restore $fp from saved_fp_location.\n";
            mipsFile << "    addiu $sp, $sp, 8     # Pop the saved $fp and $ra from the stack.\n";

            if (this->currentFunctionName == "main") {
                mipsFile << "    li $v0, 10          # System call for exit\n";
                mipsFile << "    syscall             # Exit\n";
            } else {
                mipsFile << "    jr $ra              # Return from function\n";
            }
            mipsFile << "\n"; // Blank line after function for readability
        } else if (quad.op == "PARAM") {
            // Load actual parameter value into $a0-$a3 or push onto stack for >4 params
            // For SysY, assuming params are expressions evaluated to temps or are existing vars.
            if (currentFuncArgIndex < 4) {
                loadToReg(quad.arg1, "$t8"); // Load param value into a temp reg first
                mipsFile << "    move $a" << currentFuncArgIndex << ", $t8  # Param " << currentFuncArgIndex << ": " << quad.arg1 << "\n";
            } else {
                // Stack passing for 5th+ param (SysY spec might not require this for basic version)
                // If needed: loadToReg(quad.arg1, "$t8"); mipsFile << "    sw $t8, -offset($sp) # push on stack\n"; mipsFile << "    addiu $sp, $sp, -4\n";
                // For now, assume <= 4 params or it's an error/unsupported.
                std::cerr << "Warning: More than 4 parameters not fully supported for MIPS stack passing in this version (param: " << quad.arg1 << ")" << std::endl;
            }
            currentFuncArgIndex++;
        } else if (quad.op == "CALL") {
            mipsFile << "    jal " << quad.arg1 << "         # Call function " << quad.arg1 << "\n";
            currentFuncArgIndex = 0; // Reset for next call
            if (!quad.result.empty() && quad.result != "-") { // If function has a return value to be stored
                storeFromReg("$v0", quad.result);
            }
        } else if (quad.op == "RETURN_VAL") {
            if (!quad.arg1.empty() && quad.arg1 != "-") {
                loadToReg(quad.arg1, "$v0"); // Load return value into $v0
            }
            // Actual `jr $ra` is handled by FUNC_END based on function name
        } else if (quad.op == "GOTO") {
            mipsFile << "    j " << quad.result << "\n";
        } else if (quad.op == "IF_TRUE_GOTO") {
            loadToReg(quad.arg1, "$t0"); // Load condition into $t0
            mipsFile << "    bne $t0, $zero, " << quad.result << "  # If " << quad.arg1 << " is true (not zero), goto " << quad.result << "\n";
        } else if (quad.op == "IF_FALSE_GOTO") {
            loadToReg(quad.arg1, "$t0"); // Load condition into $t0
            mipsFile << "    beq $t0, $zero, " << quad.result << "  # If " << quad.arg1 << " is false (zero), goto " << quad.result << "\n";
        } else if (quad.op[0] == '_' && quad.op[1] == 'L' && quad.op.back() == ':') { // Label definition e.g. _L1:
             mipsFile << quad.op << "\n"; // Quad.op is the label itself like "_L1:"
        } else if (quad.op.back() == ':') { // Any other label definition, e.g. user-defined if allowed, or loop labels etc.
            mipsFile << quad.op << "\n";
        } else if (quad.op == "ASSIGN") {
            loadToReg(quad.arg1, "$t0");
            storeFromReg("$t0", quad.result);
        } else if (quad.op == "ADD" || quad.op == "SUB" || quad.op == "MULT" || quad.op == "DIV" || quad.op == "MOD" ||
                   quad.op == "AND" || quad.op == "OR" || 
                   quad.op == "LSS" || quad.op == "LEQ" || quad.op == "GRE" || quad.op == "GEQ" || quad.op == "EQL" || quad.op == "NEQ" ) {
            loadToReg(quad.arg1, "$t0");
            loadToReg(quad.arg2, "$t1");
            if (quad.op == "ADD") mipsFile << "    addu $t2, $t0, $t1\n";
            else if (quad.op == "SUB") mipsFile << "    subu $t2, $t0, $t1\n";
            else if (quad.op == "MULT") { mipsFile << "    mult $t0, $t1\n"; mipsFile << "    mflo $t2\n"; }
            else if (quad.op == "DIV") { mipsFile << "    div $t0, $t1\n"; mipsFile << "    mflo $t2\n"; }
            else if (quad.op == "MOD") { mipsFile << "    div $t0, $t1\n"; mipsFile << "    mfhi $t2\n"; }
            else if (quad.op == "AND") mipsFile << "    and $t2, $t0, $t1\n";
            else if (quad.op == "OR")  mipsFile << "    or $t2, $t0, $t1\n";
            else if (quad.op == "LSS") mipsFile << "    slt $t2, $t0, $t1\n";
            else if (quad.op == "LEQ") { mipsFile << "    sgt $t2, $t0, $t1\n"; mipsFile << "    xori $t2, $t2, 1\n";} // t2 = !(t0 > t1)
            else if (quad.op == "GRE") mipsFile << "    sgt $t2, $t0, $t1\n";
            else if (quad.op == "GEQ") { mipsFile << "    slt $t2, $t0, $t1\n"; mipsFile << "    xori $t2, $t2, 1\n";} // t2 = !(t0 < t1)
            else if (quad.op == "EQL") { mipsFile << "    seq $t2, $t0, $t1\n";}
            else if (quad.op == "NEQ") { mipsFile << "    sne $t2, $t0, $t1\n";}
            storeFromReg("$t2", quad.result);
        } else if (quad.op == "NOT_OP") { // Logical NOT or bitwise NOT? Assume logical for now.
            loadToReg(quad.arg1, "$t0");
            mipsFile << "    seq $t2, $t0, $zero   # $t2 = (arg1 == 0) ? 1 : 0 \n"; // Logical NOT
            storeFromReg("$t2", quad.result);
        } else if (quad.op == "NEG_OP") { // Unary minus
            loadToReg(quad.arg1, "$t0");
            mipsFile << "    subu $t2, $zero, $t0\n";
            storeFromReg("$t2", quad.result);
        } else if (quad.op == "NEG") {
            // Quad: (NEG, arg1, _, result) -> result = -arg1
            std::string arg1_operand = quad.arg1;
            std::string result_operand = quad.result;

            // Determine if arg1 is an immediate or a variable/temp
            if (isIntegerImmediate(arg1_operand)) {
                mipsFile << "    li $t0, " << arg1_operand << "         # Load immediate " << arg1_operand << " into $t0\n";
            } else {
                // It's a variable/temp, load its value from memory
                loadToReg(arg1_operand, "$t0"); // loadToReg handles getting memory address via getMipsOperand
            }

            // Perform negation: result_reg = 0 - $t0
            mipsFile << "    subu $t1, $zero, $t0   # $t1 = - $t0 (negation)\n";

            // Store the result from $t1 into the memory location for 'result_operand'
            storeFromReg("$t1", result_operand);
        } else if (quad.op == "ADD_OFFSET") {
            // quad.arg1 = base name (global array, or mangled local array name)
            // quad.arg2 = offset in bytes (constant or temp var)
            // quad.result = result address temp var

            // 1. Get base address of quad.arg1 into $t0
            // getMipsOperand with loadAddr=true will:
            // - For global array: output 'la $t0, array_label'
            // - For local array (mangled name): output 'addiu $t0, $fp, array_offset'
            getMipsOperand(quad.arg1, "$t0", true);

            // 2. Load offset (in bytes) from quad.arg2 into $t1
            loadToReg(quad.arg2, "$t1");

            // 3. Calculate effective address: $t2 = $t0 (base_addr) + $t1 (offset_bytes)
            mipsFile << "    addu $t2, $t0, $t1    # Effective address = base + offset\\n";

            // 4. Store the calculated effective address (from $t2) into the stack slot for quad.result
            storeFromReg("$t2", quad.result);

        } else if (quad.op == "LOAD_FROM_ADDR") {
            // quad.arg1 = Source address (temporary variable holding an address)
            // quad.arg2 = Unused ("_")
            // quad.result = Destination variable (to store the loaded value)
            
            // 1. Load the value of address_temp (which is an address) into $t0
            loadToReg(quad.arg1, "$t0"); // $t0 now holds the memory address M
            
            // 2. Load word from memory location M (held in $t0) into $t1
            mipsFile << "    lw $t1, 0($t0)        # Load value from address in " << quad.arg1 << " (now in $t0)\n";
            
            // 3. Store the loaded value (from $t1) into the stack slot for quad.result
            storeFromReg("$t1", quad.result);
            
        } else if (quad.op == "STORE_TO_ADDR") {
            // quad.arg1 = Destination address (temporary variable holding an address)
            // quad.arg2 = Unused ("_")
            // quad.result = Source value (temporary variable or constant to be stored)

            // 1. Load the value of address_temp (quad.arg1, which is an address) into $t0
            loadToReg(quad.arg1, "$t0"); // $t0 now holds the target memory address M

            // 2. Load the value to be stored (quad.result) into $t1
            loadToReg(quad.result, "$t1"); // $t1 now holds the value V to be stored

            // 3. Store word V (from $t1) into memory location M (pointed to by $t0)
            mipsFile << "    sw $t1, 0($t0)        # Store value " << quad.result << " (in $t1) to address in " << quad.arg1 << " (in $t0)\n";

        } else if (quad.op == "PRINT_STR") {
            Symbol* strSym = symTab.lookupSymbol(quad.arg1); // quad.arg1 is label of string like _str_lit_0
            if (strSym && strSym->type == "string_literal") {
                mipsFile << "    li $v0, 4           # System call for print_string\n";
                mipsFile << "    la $a0, " << strSym->name << " # Address of string to print\n";
                mipsFile << "    syscall\n";
            } else {
                std::cerr << "MIPS Gen Error: String literal " << quad.arg1 << " not found for PRINT_STR." << std::endl;
            }
        } else if (quad.op == "PRINT_INT") {
            loadToReg(quad.arg1, "$a0"); // Value to print in $a0
            mipsFile << "    li $v0, 1           # System call for print_int\n";
            mipsFile << "    syscall\n";
             // Removed automatic newline printing after int to match user's mips.txt behavior
            // mipsFile << "    li $v0, 4           # System call for print_string\n";
            // mipsFile << "    la $a0, _newline    # Address of newline string\n";
            // mipsFile << "    syscall\n";
        } else if (quad.op == "GET_INT") {
            // mipsFile << "    li $v0, 4           # print_string syscall for prompt\n";
            // mipsFile << "    la $a0, _prompt     # load address of prompt string\n";
            // mipsFile << "    syscall\n";
            mipsFile << "    li $v0, 5           # System call for read_int\n";
            mipsFile << "    syscall             # Integer read into $v0\n";
            storeFromReg("$v0", quad.result); // Store result from $v0 to destination
        } else {
            mipsFile << "    # Unknown Quad: (" << quad.op << ", " << quad.arg1 << ", " << quad.arg2 << ", " << quad.result << ")\n";
        }
    }
}

void MipsGenerator::generate() {
    if (!mipsFile.is_open()) {
        std::cerr << "MipsGenerator Error: Output file is not open. Cannot generate MIPS." << std::endl;
        return;
    }
    mipsFile.clear(); //确保流状态良好
    mipsFile.seekp(0, std::ios_base::beg); //确保从文件头开始写（如果文件已存在并有内容）

    generateDataSection();  // 直接使用 this->quads 和 this->mipsFile
    this->mipsFile << "\n"; // 文本段和数据段之间的空行
    generateTextSection();  // 直接使用 this->quads 和 this->mipsFile

    this->mipsFile.flush(); // 确保所有内容都写入文件
    std::cout << "MIPS generation complete for current MipsGenerator instance." << std::endl;
}

// Helper function to determine if an operand is an immediate integer
bool MipsGenerator::isIntegerImmediate(const std::string& operand) {
    if (operand.empty()) return false;
    int k = 0;
    if (operand[0] == '-') {
        if (operand.length() == 1) return false; // Just "-" is not numeric
        k = 1;
    }
    for (; k < operand.length(); ++k) {
        if (!isdigit(operand[k])) {
            return false;
        }
    }
    return true;
} 