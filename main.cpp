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

#define ENABLE_LCSE
#define ENABLE_DCE

// --- Debug Flags ---
#define PRINT_CFG_DETAILS // Uncomment to print detailed CFG to console
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

        // 2. Middle-end: CFG Construction, Optimization, and Analysis
#ifdef PRINT_CFG_DETAILS
        std::cout << "\n--- Building CFGs, Optimizing, and Analyzing ---" << std::endl;
#else
        std::cout << "\n--- Processing CFGs, Optimizing, and Analyzing ---" << std::endl;
#endif

        std::vector<std::vector<Quadruple>> functions_quads_list;
        std::vector<Quadruple> global_quads_for_mips; 

        if (!original_ir_quads.empty()) {
            std::vector<Quadruple> current_function_q_list;
            bool in_function_scope = false;
            for (const auto& quad : original_ir_quads) {
                if (quad.op == "FUNC_BEGIN") {
                    if(in_function_scope && !current_function_q_list.empty()){ 
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

            // Perform Live Variable Analysis
            std::cout << "Performing Live Variable Analysis for: " << func_name_for_cfg << "..." << std::endl;
            cfg.computeLiveVariables(symTab); // Pass the symbol table
            std::cout << "Live Variable Analysis complete for: " << func_name_for_cfg << "." << std::endl;

#ifdef ENABLE_DCE
            // Perform Dead Code Elimination
            std::cout << "Performing Dead Code Elimination for: " << func_name_for_cfg << "..." << std::endl;
            cfg.eliminateDeadCode(symTab);
            std::cout << "Dead Code Elimination complete for: " << func_name_for_cfg << "." << std::endl;
#endif

            // Optionally, print quads again after DCE to see the effect
            // This part needs to be conditional as well if DCE is disabled, or handle it gracefully.
            // For simplicity, we can assume if ENABLE_DCE is off, quads_after_dce will be the same as quads_after_lva (or whatever was before)
            // We'll adjust the print and save logic slightly.

            std::vector<Quadruple> quads_for_collection_after_opts;
            // Reconstruct quads from CFG blocks. If DCE was run, these are post-DCE.
            // If DCE was not run, these are post-LVA (or post-LCSE if LVA doesn't change quads structually).
            std::vector<int> block_ids_for_reconstruction;
            for(const auto& block_pair : cfg.blocks) block_ids_for_reconstruction.push_back(block_pair.first);
            std::sort(block_ids_for_reconstruction.begin(), block_ids_for_reconstruction.end());
            for(int block_id : block_ids_for_reconstruction) {
                if (cfg.blocks.count(block_id)) {
                    const auto& block = cfg.blocks.at(block_id);
                    quads_for_collection_after_opts.insert(quads_for_collection_after_opts.end(), block.quads.begin(), block.quads.end());
                }
            }

            std::string output_quads_filename_suffix = "_post_opts.txt";
#ifdef ENABLE_DCE
            std::cout << "Quadruples after potential Dead Code Elimination for function " << func_name_for_cfg << ":\\n";
            output_quads_filename_suffix = "_post_dce.txt";
#else
            std::cout << "Quadruples after optimizations (DCE disabled) for function " << func_name_for_cfg << ":\\n";
#endif
            
            std::string output_quads_filename_prefix = "quads_";
            std::string output_quads_filename = output_quads_filename_prefix + func_name_for_cfg + output_quads_filename_suffix;
            
            if (saveQuadruplesToFile(quads_for_collection_after_opts, output_quads_filename)) {
                std::cout << "Quads (" << output_quads_filename_suffix.substr(1, output_quads_filename_suffix.find('.')-1) 
                          << ") for " << func_name_for_cfg << " saved to " << output_quads_filename << std::endl;
            } else {
                std::cerr << "Failed to save quads (" << output_quads_filename_suffix.substr(1, output_quads_filename_suffix.find('.')-1) 
                          << ") for " << func_name_for_cfg << " to " << output_quads_filename << std::endl;
            }

            // Append these optimized quads to the list for MIPS generation
            all_optimized_quads_for_mips_gen.insert(all_optimized_quads_for_mips_gen.end(),
                                                    quads_for_collection_after_opts.begin(), quads_for_collection_after_opts.end());

#ifdef PRINT_CFG_DETAILS
            std::cout << "--- CFG for function: " << func_name_for_cfg << " --- (Reflects Optimizations)" << std::endl;
            cfg.printGraph(std::cout); // This will print quads, and then LVA results are printed by computeLiveVariables itself
            
            std::string dot_filename = "cfg_" + func_name_for_cfg + ".dot";
            std::ofstream dot_file(dot_filename);
            if (dot_file.is_open()) {
                cfg.printDot(dot_file, "CFG_" + func_name_for_cfg + " (Optimized)");
                dot_file.close();
                std::cout << "Generated DOT file (Optimized): " << dot_filename << std::endl;
            } else {
                std::cerr << "Error: Could not open DOT file " << dot_filename << " for writing." << std::endl;
            }
            // LVA results are now printed directly from cfg.computeLiveVariables if its internal cout is uncommented
#endif
            func_idx++;
        }

#ifdef PRINT_CFG_DETAILS
        std::cout << "\n--- CFG Processing, Optimization, and Analysis Complete ---" << std::endl;
#else
        std::cout << "\n--- CFG Processing, Optimization, and Analysis Complete ---" << std::endl;
#endif

        // 3. Backend: MIPS Code Generation
        if (all_optimized_quads_for_mips_gen.empty() && !original_ir_quads.empty() && functions_quads_list.empty()){
             std::cout << "No functions found, but global quads exist. Generating MIPS for global quads only." << std::endl;
             // This branch might be less relevant now if all_optimized_quads_for_mips_gen correctly collects function quads.
             // However, if original_ir_quads only had globals and no functions, it would still be valid.
             const std::vector<Quadruple> quads_for_mips = reorderQuadsForMainFirst(all_optimized_quads_for_mips_gen); 
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