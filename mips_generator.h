#ifndef MINE_MIPS_GENERATOR_H_
#define MINE_MIPS_GENERATOR_H_

#include <fstream>
#include <string>
#include <vector>
#include <map>

// Need full definitions for SymbolTable and Quadruple as MipsGenerator uses them directly.
#include "symboltable.h" // Provides Symbol and SymbolTable
#include "ir_generator.h"   // Provides Quadruple (which includes ir_def.h)

class MipsGenerator {
public:
    // Constructor: Takes the output MIPS filename, SymbolTable, and IR
    MipsGenerator(const std::string& outputFileName, SymbolTable& st, const std::vector<Quadruple>& ir);
    // Destructor to ensure file is closed if opened
    ~MipsGenerator();

    void generate(); // Main function to generate MIPS code

    // Helper function to determine if an operand is an immediate integer
    bool isIntegerImmediate(const std::string& operand);

private:
    std::ofstream mipsFile; // MIPS output file stream (now an object)
    SymbolTable& symTab;
    const std::vector<Quadruple>& quads;
    std::map<std::string, int> tempVarOffsets; 
    int functionLocalTempSpace; 
    std::string currentFunctionName;

    void generateDataSection();
    void generateTextSection();
    std::string getMipsOperand(const std::string& operandId, const std::string& targetRegForAddr = "$t9", bool loadAddr = false);
    void loadToReg(const std::string& operand, const std::string& reg, bool isOperandAlreadyAddress = false);
    void storeFromReg(const std::string& reg, const std::string& destination, bool destIsAddrInReg = false, const std::string& destAddrReg = "$t9");
};

#endif // MINE_MIPS_GENERATOR_H_ 