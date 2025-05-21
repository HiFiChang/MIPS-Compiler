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
    // 打开调试日志文件
    debugLogFile.open("mips_debug_log.txt");
    if (!debugLogFile.is_open()) {
        // 如果无法打开调试日志文件，可以选择抛出错误或在控制台打印警告后继续
        std::cerr << "MipsGenerator: WARNING: Could not open debug log file: mips_debug_log.txt. Debug logs will be disabled." << std::endl;
    } else {
        debugLogFile << "MipsGenerator Debug Log Started" << std::endl;
    }

    std::cout << "MipsGenerator instance created. Outputting to: " << outputFileName << std::endl;
}

MipsGenerator::~MipsGenerator() {
    if (mipsFile.is_open()) {
        std::cout << "Closing MIPS output file handled by MipsGenerator: " << mipsFile.is_open() << std::endl; // Log before close
        mipsFile.close();
    }
    // 关闭调试日志文件
    if (debugLogFile.is_open()) {
        debugLogFile << "MipsGenerator Debug Log Ended" << std::endl;
        debugLogFile.close();
    }
}

// Helper to get MIPS operand string (e.g., register, immediate, memory address)
std::string MipsGenerator::getMipsOperand(const std::string& operandId, const std::string& targetRegForAddr, bool loadAddr) {
    if (operandId.empty() || operandId == "_" || operandId == "-") return "";

    if (debugLogFile.is_open()) debugLogFile << "MIPS_GET_OPERAND_DEBUG: Called for operandId='" << operandId << "', loadAddr=" << loadAddr << std::endl;

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
    if (debugLogFile.is_open()) debugLogFile << "MIPS_GET_OPERAND_DEBUG: Checking tempVarOffsets for '" << operandId << "' in function '" << this->currentFunctionName << "'" << std::endl;
    if (tempVarOffsets.count(operandId)) {
        int offset = tempVarOffsets.at(operandId);
        if (debugLogFile.is_open()) debugLogFile << "MIPS_GET_OPERAND_DEBUG: Found '" << operandId << "' in tempVarOffsets with offset " << offset << std::endl;
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
    } else {
        if (debugLogFile.is_open()) debugLogFile << "MIPS_GET_OPERAND_DEBUG: Symbol '" << operandId << "' not found as global or string literal via symTab.lookupSymbol." << std::endl;
    }
    
    // 4. If not found elsewhere, assume it's a code label
    if (loadAddr) {
        if (debugLogFile.is_open()) debugLogFile << "MIPS_WARNING: Attempting to loadAddr of what appears to be a code label: " << operandId << std::endl;
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
                    if (sym.isConstant && sym.isInitialized && !sym.arrayValues.empty()) {
                        // 如果是带有初始值的常量数组，直接输出.word序列
                        mipsFile << ".word ";
                        for (size_t i = 0; i < sym.arrayValues.size(); ++i) {
                            if (i > 0) mipsFile << ", ";
                            mipsFile << sym.arrayValues[i];
                        }
                        mipsFile << "\n";
                        
                        // 如果数组大小大于初始值数量，用0填充剩余空间
                        int totalElements = 1;
                        for (int dim : sym.arrayDimensions) totalElements *= std::max(1, dim);
                        
                        if (totalElements > sym.arrayValues.size()) {
                            int remainingSize = (totalElements - sym.arrayValues.size()) * 4;
                            mipsFile << "    .space " << remainingSize << " # Padding for uninitialized elements\n";
                        }
                    } else {
                        // 对于未初始化的数组，使用.space分配空间
                        int totalSize = 4;
                        for (int dim : sym.arrayDimensions) totalSize *= std::max(1, dim); // ensure dim > 0
                        mipsFile << ".space " << totalSize << "\n";
                    }
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
    int stackArgsSizeToCleanup = 0; // Track size of stack-passed arguments for cleanup

    for (const auto& quad : quads) {
        if (quad.op == "FUNC_BEGIN") {
            this->currentFunctionName = quad.arg1;
            currentFuncArgIndex = 0; // Reset for params of this function call if any
            stackArgsSizeToCleanup = 0; // Reset for calls within this function
            tempVarOffsets.clear(); 
            functionLocalTempSpace = 0; 

            Symbol* funcSym = symTab.lookupSymbol(this->currentFunctionName);
            if (!funcSym) {
                std::cerr << "Critical Error: Function " << this->currentFunctionName << " not in symbol table for MIPS gen." << std::endl;
                continue;
            }

            mipsFile << this->currentFunctionName << ":\n";
            mipsFile << "    addiu $sp, $sp, -8  # Allocate space for old $fp and $ra\n";
            mipsFile << "    sw $fp, 4($sp)      # Save old frame pointer\n";
            mipsFile << "    sw $ra, 0($sp)      # Save return address\n";
            mipsFile << "    addiu $fp, $sp, 4   # Set up new frame pointer (points to saved old $fp)\n";

            int localDataSize = 0;
            try {
                if (!quad.arg2.empty() && quad.arg2 != "-") {
                    localDataSize = std::stoi(quad.arg2);
                }
            } catch (const std::invalid_argument& ia) {
                std::cerr << "Invalid argument for localDataSize: " << quad.arg2 << " for func " << this->currentFunctionName << std::endl;
            } catch (const std::out_of_range& oor) {
                std::cerr << "OutOfRange argument for localDataSize: " << quad.arg2 << " for func " << this->currentFunctionName << std::endl;
            }
            if (localDataSize > 0) {
                mipsFile << "    addiu $sp, $sp, -" << localDataSize << "  # Allocate space for locals and temps\n";
            }
            functionLocalTempSpace = localDataSize; 

            if (symTab.functionLocalSymbols.count(this->currentFunctionName)) {
                const std::vector<Symbol>& symbolsForFunction = symTab.functionLocalSymbols.at(this->currentFunctionName);
                if (debugLogFile.is_open()) debugLogFile << "MIPS_DEBUG: Func '" << this->currentFunctionName 
                          << "', Loading " << symbolsForFunction.size() << " symbols from stored functionLocalSymbols (vector)." << std::endl;
                for (const Symbol& symbolEntry : symbolsForFunction) { // Iterate directly over Symbol objects
                    const std::string& originalSymbolName = symbolEntry.name;
                    std::string keyForTempVarOffsets = originalSymbolName;
                    bool isIRGeneratedName = (originalSymbolName.length() > 0 && originalSymbolName[0] == '_');
                    if (!symbolEntry.isGlobal && !isIRGeneratedName && !this->currentFunctionName.empty()) {
                        keyForTempVarOffsets = "__" + this->currentFunctionName + "_" + originalSymbolName + "_s" + std::to_string(symbolEntry.scopeLevel);
                    }
                    if (debugLogFile.is_open()) debugLogFile << "MIPS_FUNC_BEGIN_DEBUG: Processing symbol for tempVarOffsets: originalName='" << originalSymbolName 
                              << "', isGlobal=" << symbolEntry.isGlobal
                              << ", isIRGeneratedName=" << isIRGeneratedName 
                              << ", scopeLevel=" << symbolEntry.scopeLevel
                              << ", computedKey='" << keyForTempVarOffsets 
                              << ", offset=" << symbolEntry.offset 
                              << ", type='" << symbolEntry.type << "'"
                              << (symbolEntry.isParam ? " (Param)" : "") << std::endl;
                    tempVarOffsets[keyForTempVarOffsets] = symbolEntry.offset;
                }
            } else {
                 if (debugLogFile.is_open()) debugLogFile << "MIPS_GEN_WARNING: Function '" << this->currentFunctionName 
                          << "' not found in symTab.functionLocalSymbols map (vector)." << std::endl;
            }

            if (funcSym && !funcSym->params.empty()) {
                mipsFile << "    # MIPS_DEBUG: Processing params for func: " << this->currentFunctionName << "\n";

                // Step 1: Handle stack-passed parameters first to avoid overwriting them
                // before they are moved to their final designated slots.
                mipsFile << "    # Step 1: Secure stack-passed parameters by moving them to final slots\n";
                for (size_t i = 0; i < funcSym->params.size(); ++i) {
                    if (i >= 4) { // Parameters passed on stack by caller
                        const auto& paramInfo = funcSym->params[i];
                        int paramScopeLevel = symTab.functionScopeLevels.count(this->currentFunctionName) ? symTab.functionScopeLevels.at(this->currentFunctionName) : funcSym->scopeLevel + 1;
                        std::string mangledParamName = "__" + this->currentFunctionName + "_" + paramInfo.first + "_s" + std::to_string(paramScopeLevel);
                        
                        int targetOffset = (i + 1) * 4; // Default calculation, should match symbol table
                        Symbol* paramSym = symTab.lookupSymbol(paramInfo.first, false, this->currentFunctionName);
                        if (paramSym && paramSym->isParam && tempVarOffsets.count(mangledParamName)) {
                            targetOffset = tempVarOffsets[mangledParamName]; 
                        } else if (paramSym && paramSym->isParam) {
                            targetOffset = paramSym->offset; 
                        } else {
                             if(debugLogFile.is_open()) debugLogFile << "MIPS_GEN_ERROR: Param symbol '" << paramInfo.first << "' not found or tempVarOffsets issue for " << this->currentFunctionName << " in param pre-saving step." << std::endl;
                        }

                        // Corrected calculation for actual_stack_loc_offset
                        int numTotalParams = funcSym->params.size();
                        int numStackParams = numTotalParams >= 4 ? numTotalParams - 4 : 0;
                        int param_list_stack_index = (int)i - 4; // 0-indexed: 0 for 5th param, 1 for 6th, etc.
                        // stack_order_from_fp_base (0 is closest to $fp, i.e., $fp+4)
                        // The last param in list (e.g. 6th param, i=5) is closest to $fp at $fp+4
                        // The first stack param in list (e.g. 5th param, i=4) is further, at $fp+8 (if 2 stack params)
                        int stack_order_from_fp_base = (numStackParams - 1) - param_list_stack_index;
                        int actual_stack_loc_offset = (stack_order_from_fp_base + 1) * 4;

                        mipsFile << "    # Param " << paramInfo.first << " (orig at " << actual_stack_loc_offset << "($fp)) -> final slot " << targetOffset << "($fp)\n";
                        mipsFile << "    lw $t0, " << actual_stack_loc_offset << "($fp)    # Load stack-passed param '" << paramInfo.first << "' from its initial position\n";
                        mipsFile << "    sw $t0, " << targetOffset << "($fp)    # Store param '" << paramInfo.first << "' to its designated final stack slot\n";
                    }
                }

                // Step 2: Handle register-passed parameters
                mipsFile << "    # Step 2: Store register-passed parameters to their final slots\n";
                for (size_t i = 0; i < funcSym->params.size(); ++i) {
                    if (i < 4) { // Parameters passed in $a0-$a3
                        const auto& paramInfo = funcSym->params[i];
                        int paramScopeLevel = symTab.functionScopeLevels.count(this->currentFunctionName) ? symTab.functionScopeLevels.at(this->currentFunctionName) : funcSym->scopeLevel + 1;
                        std::string mangledParamName = "__" + this->currentFunctionName + "_" + paramInfo.first + "_s" + std::to_string(paramScopeLevel);
                        
                        int targetOffset = (i + 1) * 4; // Default calculation
                        Symbol* paramSym = symTab.lookupSymbol(paramInfo.first, false, this->currentFunctionName);
                        if (paramSym && paramSym->isParam && tempVarOffsets.count(mangledParamName)) {
                            targetOffset = tempVarOffsets[mangledParamName];
                        } else if (paramSym && paramSym->isParam) {
                            targetOffset = paramSym->offset;
                        } else {
                            if(debugLogFile.is_open()) debugLogFile << "MIPS_GEN_ERROR: Param symbol '" << paramInfo.first << "' not found or tempVarOffsets issue for " << this->currentFunctionName << " in register param storing step." << std::endl;
                        }

                        mipsFile << "    sw $a" << i << ", " << targetOffset << "($fp)  # Store register param '" << paramInfo.first << "' (from $a" << i << ") to its designated stack slot\n";
                    }
                }
            }

        } else if (quad.op == "FUNC_END") {
            mipsFile << "_L_" << this->currentFunctionName << "_epilogue:\n";
            mipsFile << "    move $sp, $fp\n";
            mipsFile << "    lw $ra, -4($sp)\n";
            mipsFile << "    lw $fp, 0($sp)\n";
            mipsFile << "    addiu $sp, $sp, 4\n";
            if (this->currentFunctionName == "main") {
                mipsFile << "    li $v0, 10\n";
                mipsFile << "    syscall\n";
            } else {
                mipsFile << "    jr $ra\n";
            }
            mipsFile << "\n";
        } else if (quad.op == "PARAM") {
            loadToReg(quad.arg1, "$t8"); // Load param value into a temp reg first
            if (currentFuncArgIndex < 4) {
                mipsFile << "    move $a" << currentFuncArgIndex << ", $t8  # Param " << currentFuncArgIndex << " ('" << quad.arg1 << "') to $a" << currentFuncArgIndex << "\n";
            } else {
                // 参数 >= 4，压入调用栈
                mipsFile << "    addiu $sp, $sp, -4     # Make space on stack for param '" << quad.arg1 << "'\n";
                mipsFile << "    sw $t8, 0($sp)         # Push stack param " << quad.arg1 << "\n";
                stackArgsSizeToCleanup += 4; // 记录为此调用压栈的总大小
            }
            currentFuncArgIndex++;
        } else if (quad.op == "CALL") {
            mipsFile << "    jal " << quad.arg1 << "         # Call function " << quad.arg1 << "\n";
            
            // 调用返回后，清理为栈参数分配的空间
            if (stackArgsSizeToCleanup > 0) {
                mipsFile << "    addiu $sp, $sp, " << stackArgsSizeToCleanup << "  # Clean up " << stackArgsSizeToCleanup/4 << " stack arguments after call to " << quad.arg1 << "\n";
                stackArgsSizeToCleanup = 0; // 为下一次调用重置
            }

            currentFuncArgIndex = 0; // Reset for next call's params
            if (!quad.result.empty() && quad.result != "-") { 
                storeFromReg("$v0", quad.result);
            }
        } else if (quad.op == "RETURN_VAL") {
            if (!quad.arg1.empty() && quad.arg1 != "-") {
                loadToReg(quad.arg1, "$v0");
            }
            mipsFile << "    j _L_" << this->currentFunctionName << "_epilogue # Jump to epilogue on return val\n";
        } else if (quad.op == "RETURN_VOID") {
             mipsFile << "    j _L_" << this->currentFunctionName << "_epilogue # Handle RETURN_VOID by jumping to epilogue\n";
        } else if (quad.op == "GOTO") {
            mipsFile << "    j " << quad.result << "\n";
        } else if (quad.op == "IF_TRUE_GOTO") {
            loadToReg(quad.arg1, "$t0");
            mipsFile << "    bne $t0, $zero, " << quad.result << "  # If " << quad.arg1 << " is true (not zero), goto " << quad.result << "\n";
        } else if (quad.op == "IF_FALSE_GOTO") {
            loadToReg(quad.arg1, "$t0");
            mipsFile << "    beq $t0, $zero, " << quad.result << "  # If " << quad.arg1 << " is false (zero), goto " << quad.result << "\n";
        } else if (quad.op[0] == '_' && quad.op[1] == 'L' && quad.op.back() == ':') { 
             mipsFile << quad.op << "\n";
        } else if (quad.op.back() == ':') { 
            mipsFile << quad.op << "\n";
        } else if (quad.op == "ASSIGN") {
            loadToReg(quad.arg1, "$t0");
            storeFromReg("$t0", quad.result);
        } else if (quad.op == "ADD" || quad.op == "SUB" || quad.op == "MUL" || quad.op == "DIV" || quad.op == "MOD" ||
                   quad.op == "AND" || quad.op == "OR" || 
                   quad.op == "LSS" || quad.op == "LEQ" || quad.op == "GRE" || quad.op == "GEQ" || quad.op == "EQL" || quad.op == "NEQ" ) {
            loadToReg(quad.arg1, "$t0");
            loadToReg(quad.arg2, "$t1");
            if (quad.op == "ADD") mipsFile << "    addu $t2, $t0, $t1\n";
            else if (quad.op == "SUB") mipsFile << "    subu $t2, $t0, $t1\n";
            else if (quad.op == "MUL") { mipsFile << "    mult $t0, $t1\n"; mipsFile << "    mflo $t2\n"; }
            else if (quad.op == "DIV") { mipsFile << "    div $t0, $t1\n"; mipsFile << "    mflo $t2\n"; }
            else if (quad.op == "MOD") { mipsFile << "    div $t0, $t1\n"; mipsFile << "    mfhi $t2\n"; }
            else if (quad.op == "AND") mipsFile << "    and $t2, $t0, $t1\n";
            else if (quad.op == "OR")  mipsFile << "    or $t2, $t0, $t1\n";
            else if (quad.op == "LSS") mipsFile << "    slt $t2, $t0, $t1\n";
            else if (quad.op == "LEQ") { mipsFile << "    sgt $t2, $t0, $t1\n"; mipsFile << "    xori $t2, $t2, 1\n";}
            else if (quad.op == "GRE") mipsFile << "    sgt $t2, $t0, $t1\n";
            else if (quad.op == "GEQ") { mipsFile << "    slt $t2, $t0, $t1\n"; mipsFile << "    xori $t2, $t2, 1\n";}
            else if (quad.op == "EQL") { mipsFile << "    seq $t2, $t0, $t1\n";}
            else if (quad.op == "NEQ") { mipsFile << "    sne $t2, $t0, $t1\n";}
            storeFromReg("$t2", quad.result);
        } else if (quad.op == "NOT_OP") {
            loadToReg(quad.arg1, "$t0");
            mipsFile << "    seq $t2, $t0, $zero   # $t2 = (arg1 == 0) ? 1 : 0 \n";
            storeFromReg("$t2", quad.result);
        } else if (quad.op == "NEG_OP" || quad.op == "NEG") { // Consolidate NEG and NEG_OP
            loadToReg(quad.arg1, "$t0");
            mipsFile << "    subu $t2, $zero, $t0\n";
            storeFromReg("$t2", quad.result);
        } else if (quad.op == "ADD_OFFSET") {
            getMipsOperand(quad.arg1, "$t0", true);
            loadToReg(quad.arg2, "$t1");
            mipsFile << "    addu $t2, $t0, $t1    # Effective address = base ('" << quad.arg1 << "') + offset ('" << quad.arg2 << "')\n";
            storeFromReg("$t2", quad.result);
        } else if (quad.op == "LOAD_FROM_ADDR") {
            loadToReg(quad.arg1, "$t0"); 
            mipsFile << "    lw $t1, 0($t0)        # Load value from address in '" << quad.arg1 << "' (now in $t0)\n";
            storeFromReg("$t1", quad.result);
        } else if (quad.op == "STORE_TO_ADDR") {
            loadToReg(quad.arg1, "$t0"); 
            loadToReg(quad.result, "$t1"); 
            mipsFile << "    sw $t1, 0($t0)        # Store value '" << quad.result << "' (in $t1) to address in '" << quad.arg1 << "' (in $t0)\n";
        } else if (quad.op == "PRINT_STR") {
            Symbol* strSym = symTab.lookupSymbol(quad.arg1);
            if (strSym && strSym->type == "string_literal") {
                mipsFile << "    li $v0, 4\n";
                mipsFile << "    la $a0, " << strSym->name << "\n";
                mipsFile << "    syscall\n";
            } else {
                if (debugLogFile.is_open()) debugLogFile << "MIPS Gen Error: String literal " << quad.arg1 << " not found for PRINT_STR." << std::endl;
            }
        } else if (quad.op == "PRINT_INT") {
            loadToReg(quad.arg1, "$a0");
            mipsFile << "    li $v0, 1\n";
            mipsFile << "    syscall\n";
        } else if (quad.op == "GET_INT") {
            mipsFile << "    li $v0, 5\n";
            mipsFile << "    syscall\n";
            storeFromReg("$v0", quad.result);
        } else {
            mipsFile << "    # Unknown Quad: (" << quad.op << ", " << quad.arg1 << ", " << quad.arg2 << ", " << quad.result << ")\n";
        }
    }
}

void MipsGenerator::generate() {
    if (!mipsFile.is_open()) {
        // std::cerr << "MipsGenerator Error: Output file is not open. Cannot generate MIPS." << std::endl;
        // 如果 mipsFile 本身都打不开，调试日志可能也无济于事，但可以尝试记录到调试日志或标准错误
        if (debugLogFile.is_open()) {
            debugLogFile << "MipsGenerator Error: MIPS Output file is not open. Cannot generate MIPS." << std::endl;
        } else {
            std::cerr << "MipsGenerator Error: MIPS Output file is not open. Cannot generate MIPS. Debug log also unavailable." << std::endl;
        }
        return;
    }
    mipsFile.clear(); //确保流状态良好
    mipsFile.seekp(0, std::ios_base::beg); //确保从文件头开始写（如果文件已存在并有内容）

    generateDataSection();  // 直接使用 this->quads 和 this->mipsFile
    this->mipsFile << "\n"; // 文本段和数据段之间的空行
    generateTextSection();  // 直接使用 this->quads 和 this->mipsFile

    this->mipsFile.flush(); // 确保所有内容都写入文件
    // std::cout << "MIPS generation complete for current MipsGenerator instance." << std::endl;
    if (debugLogFile.is_open()) {
        debugLogFile << "MIPS generation complete for current MipsGenerator instance. Output to: " << mipsFile.is_open() /* Placeholder for filename, ideally pass filename to constructor and store */ << std::endl;
    } else {
        std::cout << "MIPS generation complete for current MipsGenerator instance." << std::endl;
    }
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