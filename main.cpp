#include "lexer.h"
#include "syntax_analyzer.h"
#include "symboltable.h"
#include "ir_generator.h"
#include "mips_generator.h"
// #include "mips_reorder_utils.h"
#include <iostream>
#include <exception>
#include <fstream>
#include <vector>
#include <string>

// 定义 saveQuadruplesToFile 函数
// 函数接收一个常量引用指向四元式向量，以及一个常量引用指向文件名字符串
// 返回布尔值表示操作是否成功
bool saveQuadruplesToFile(const std::vector<Quadruple>& quads, const std::string& filename) {
    std::ofstream outFile(filename);
    if (!outFile.is_open()) {
        std::cerr << "Error: Could not open file to save quadruples: " << filename << std::endl;
        return false;
    }

    for (const auto& quad : quads) {
        outFile << "(" << quad.op << ", " 
                << (quad.arg1.empty() ? "-" : quad.arg1) << ", " 
                << (quad.arg2.empty() ? "-" : quad.arg2) << ", " 
                << (quad.result.empty() ? "-" : quad.result) << ")\n";
    }

    outFile.close();
    if (!outFile) { // Check for errors after closing (e.g., write errors)
        std::cerr << "Error: An error occurred while writing or closing the quadruples file: " << filename << std::endl;
        return false;
    }
    return true;
}

// main function for compilation, using fixed filenames for评测机
int main() { // 移除 argc, argv
    std::string sourceFile = "testfile.txt"; // 固定输入文件名
    std::string mipsOutFile = "mips.txt";    // 固定输出文件名

    try {
        Lexer lexer(sourceFile, "lexer_debug_output.txt"); 
        lexer.processFile(); 

        SymbolTable symTab;
        IRGenerator irGen;
        
        SyntaxAnalyzer analyzer(lexer, symTab, irGen, mipsOutFile, false );
        analyzer.analyze(); 

        if (saveQuadruplesToFile(irGen.getQuads(), "quads.txt")) {
            std::cout << "Quadruples saved to quads.txt" << std::endl;
        } else {
            std::cerr << "Failed to save quadruples to quads.txt" << std::endl;
        }
        
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << std::endl;
        return 1;
    } catch (...) {
        std::cerr << "An unknown error occurred during compilation." << std::endl;
        return 1;
    }
    return 0;
} 