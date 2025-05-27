#include "lexer.h"
#include "syntax_analyzer.h"
#include "symbol_table.h"
#include "ir_generator.h"
#include "mips_generator.h"
#include "control_flow_graph.h"
#include <iostream>
#include <exception>
#include <fstream>
#include <vector>
#include <string>
#include <algorithm> // For std::sort

// --- Optimization Flags (Uncomment to enable) ---
#define ENABLE_LCSE
// #define ENABLE_CONSTANT_FOLDING // Example for a future optimization
// --- End Optimization Flags ---

// --- Debug Flags ---
// #define PRINT_CFG_DETAILS // Uncomment to print detailed CFG to console
// --- End Debug Flags ---

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

// Helper function to reorder quads, placing the main function first.
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
            } else if (!currentProcessingFuncName.empty()) {
                otherFunctionQuads.push_back(quad);
            }
            inAnyFunction = false;
            currentProcessingFuncName = "";
        } else {
            if (currentProcessingFuncName == "main" && mainFound) {
                mainFunctionQuads.push_back(quad);
            } else if (!currentProcessingFuncName.empty()) {
                otherFunctionQuads.push_back(quad);
            } else {
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
    
    if (reorderedQuads.size() != originalQuads.size() && !originalQuads.empty()) {
        // Fallback if distribution logic had an issue, to prevent quad loss
        // This could happen if e.g. main was found but its quads weren't all collected, or similar logic error.
        // A more robust check would be needed if this fallback is triggered often.
        // For now, if sizes mismatch significantly and quads existed, it implies a potential issue.
        // However, if main isn't found, globalInitQuads + otherFunctionQuads should equal originalQuads.
        bool expectedSizeMatch = true;
        if (mainFound) {
            if (reorderedQuads.size() != originalQuads.size()) expectedSizeMatch = false;
        } else {
            if (globalInitQuads.size() + otherFunctionQuads.size() != originalQuads.size()) expectedSizeMatch = false;
        }
        if (!expectedSizeMatch) {
             // std::cerr << "Warning: reorderQuadsForMainFirst mismatch, falling back to original order." << std::endl;
             return originalQuads;
        }
    }
    return reorderedQuads;
}

int main() {
    std::string sourceFile = "testfile.txt";
    std::string mipsOutFile = "mips.txt";

    try {
        // 1. Frontend Stages
        Lexer lexer(sourceFile, "lexer_debug_output.txt");
        lexer.processFile();

        SymbolTable symTab; 
        IRGenerator irGen;   

        SyntaxAnalyzer analyzer(lexer, symTab, irGen, "" /* mipsFileName not used by analyzer */, false);
        analyzer.analyze(); 

        const auto& original_ir_quads = irGen.getQuads(); 
        if (saveQuadruplesToFile(original_ir_quads, "quads_original.txt")) {
            std::cout << "Original quadruples saved to quads_original.txt" << std::endl;
        } else {
            std::cerr << "Failed to save original quadruples to quads_original.txt" << std::endl;
        }

        // 2. Middle-end: CFG Construction and Optimization
#ifdef PRINT_CFG_DETAILS
        std::cout << "\n--- Building CFGs and Optimizing ---" << std::endl;
#else
        std::cout << "\n--- Processing CFGs and Optimizing ---" << std::endl;
#endif

        std::vector<std::vector<Quadruple>> functions_quads_list;
        std::vector<Quadruple> global_quads_for_mips; 

        if (!original_ir_quads.empty()) {
            std::vector<Quadruple> current_function_q_list;
            bool in_function_scope = false;
            for (const auto& quad : original_ir_quads) {
                if (quad.op == "FUNC_BEGIN") {
                    if(in_function_scope && !current_function_q_list.empty()){ 
                        // This case implies a FUNC_BEGIN without a preceding FUNC_END for the previous function.
                        // Treat accumulated quads as belonging to an implicitly ended function.
                        functions_quads_list.push_back(current_function_q_list); 
                    }
                    current_function_q_list.clear();
                    current_function_q_list.push_back(quad);
                    in_function_scope = true;
                } else if (quad.op == "FUNC_END") {
                    if(in_function_scope){
                        current_function_q_list.push_back(quad);
                        functions_quads_list.push_back(current_function_q_list);
                        current_function_q_list.clear();
                        in_function_scope = false;
                    } else {
                         global_quads_for_mips.push_back(quad); 
                    }
                } else if (in_function_scope) {
                    current_function_q_list.push_back(quad);
                } else {
                    global_quads_for_mips.push_back(quad);
                }
            }
            if(in_function_scope && !current_function_q_list.empty()){ 
                 functions_quads_list.push_back(current_function_q_list);
            }
        }
        
        std::vector<Quadruple> all_optimized_quads_for_mips_gen;
        all_optimized_quads_for_mips_gen.insert(all_optimized_quads_for_mips_gen.end(), 
                                                global_quads_for_mips.begin(), global_quads_for_mips.end());

        int func_idx = 0;
        for (const auto& func_quads_input : functions_quads_list) { 
            if (func_quads_input.empty()) {
                continue;
            }

            std::string func_name_for_cfg = "unknown_function_" + std::to_string(func_idx);
            if (func_quads_input[0].op == "FUNC_BEGIN" && !func_quads_input[0].arg1.empty()) {
                func_name_for_cfg = func_quads_input[0].arg1;
            }

#ifdef PRINT_CFG_DETAILS
            std::cout << "\nBuilding CFG for: " << func_name_for_cfg << "..." << std::endl;
#endif
            ControlFlowGraph cfg; 
            cfg.build(func_quads_input, func_name_for_cfg);

#ifdef ENABLE_LCSE
            std::cout << "Performing LCSE for: " << func_name_for_cfg << "..." << std::endl;
            cfg.performLCSE();
            std::cout << "LCSE complete for: " << func_name_for_cfg << "." << std::endl;
#endif

            std::vector<Quadruple> optimized_quads_this_func;
            std::vector<int> block_ids_sorted;
            for (const auto& block_pair : cfg.blocks) {
                block_ids_sorted.push_back(block_pair.first);
            }
            std::sort(block_ids_sorted.begin(), block_ids_sorted.end());
            for (int block_id : block_ids_sorted) {
                const auto& block = cfg.blocks.at(block_id);
                for (const auto& q_opt : block.quads) {
                    optimized_quads_this_func.push_back(q_opt);
                }
            }
            
            all_optimized_quads_for_mips_gen.insert(all_optimized_quads_for_mips_gen.end(), 
                                                   optimized_quads_this_func.begin(), optimized_quads_this_func.end());

            std::string optimized_quads_filename = "quads_" + func_name_for_cfg + "_optimized.txt";
            if (saveQuadruplesToFile(optimized_quads_this_func, optimized_quads_filename)) {
                std::cout << "Optimized quadruples for " << func_name_for_cfg << " saved to " << optimized_quads_filename << std::endl;
            } else {
                std::cerr << "Failed to save optimized quadruples for " << func_name_for_cfg << " to " << optimized_quads_filename << std::endl;
            }

#ifdef PRINT_CFG_DETAILS
            std::cout << "--- CFG for function: " << func_name_for_cfg << " --- (Reflects Optimizations)" << std::endl;
            cfg.printGraph(std::cout);
            std::string dot_filename = "cfg_" + func_name_for_cfg + ".dot";
            std::ofstream dot_file(dot_filename);
            if (dot_file.is_open()) {
                cfg.printDot(dot_file, "CFG_" + func_name_for_cfg + " (Optimized)");
                dot_file.close();
                std::cout << "Generated DOT file (Optimized): " << dot_filename << std::endl;
            } else {
                std::cerr << "Error: Could not open DOT file " << dot_filename << " for writing." << std::endl;
            }
#endif
            func_idx++;
        }

#ifdef PRINT_CFG_DETAILS
        std::cout << "\n--- CFG Processing and Optimization Complete ---" << std::endl;
#else
        std::cout << "\n--- CFG Processing and Optimization Complete ---" << std::endl;
#endif

        // 3. Backend: MIPS Code Generation
        if (all_optimized_quads_for_mips_gen.empty() && !original_ir_quads.empty() && functions_quads_list.empty()){
             std::cout << "No functions found, but global quads exist. Generating MIPS for global quads only." << std::endl;
             const std::vector<Quadruple> quads_for_mips = reorderQuadsForMainFirst(all_optimized_quads_for_mips_gen); // Should mostly be global_quads_for_mips
             if (saveQuadruplesToFile(quads_for_mips, "quads_final_for_mips.txt")) {
                 std::cout << "Final quadruples for MIPS generation saved to quads_final_for_mips.txt" << std::endl;
             }
             MipsGenerator mipsGen(mipsOutFile, symTab, quads_for_mips );
             mipsGen.generate();
             std::cout << "MIPS code generated to " << mipsOutFile << std::endl;
        } else if (!all_optimized_quads_for_mips_gen.empty()) {
            std::cout << "\n--- Generating MIPS code from optimized IR ---" << std::endl;
            const std::vector<Quadruple> final_quads_for_mips = reorderQuadsForMainFirst(all_optimized_quads_for_mips_gen);
            if (saveQuadruplesToFile(final_quads_for_mips, "quads_final_for_mips.txt")) {
                 std::cout << "Final quadruples for MIPS generation saved to quads_final_for_mips.txt" << std::endl;
            }

            MipsGenerator mipsGen(mipsOutFile, symTab, final_quads_for_mips);
            mipsGen.generate();
            std::cout << "MIPS code generated to " << mipsOutFile << std::endl;
        } else {
            std::cout << "No quads to generate MIPS code from (original was also empty)." << std::endl;
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