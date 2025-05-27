#ifndef CONTROL_FLOW_GRAPH_H
#define CONTROL_FLOW_GRAPH_H

#include "ir_def.h" // For Quadruple
#include <vector>
#include <string>
#include <map>
#include <set>
#include <iostream> // For printGraph ostream
#include "symbol_table.h" // Include SymbolTable for type information

// Forward declaration (if needed by other files including this one early)
// class ControlFlowGraph; 

struct BasicBlock {
    int id;                           // Unique ID for the basic block
    std::vector<Quadruple> quads;     // The quadruples contained in this block
    // Alternatively, to save memory if quads are stored centrally:
    // int start_quad_index_in_global_list;
    // int end_quad_index_in_global_list;

    std::set<int> successors;         // IDs of successor basic blocks
    std::set<int> predecessors;       // IDs of predecessor basic blocks

    std::string comment; // Optional: for debugging, e.g., "Entry", "Exit", "Loop Header"

    BasicBlock(int block_id = -1) : id(block_id) {}

    void addQuad(const Quadruple& quad) {
        quads.push_back(quad);
    }

    const Quadruple& getLastInstruction() const {
        if (quads.empty()) {
            // This case should ideally be handled or prevented
            // Returning a dummy or throwing an exception might be options
            // For now, let's assume blocks are not empty when this is called
            // Or, the caller checks for emptiness.
            // Consider a specific "empty/invalid" Quadruple static instance.
            throw std::runtime_error("Attempted to get last instruction from an empty basic block.");
        }
        return quads.back();
    }
};

class ControlFlowGraph {
public:
    ControlFlowGraph();
    void build(const std::vector<Quadruple>& quads_for_function, const std::string& functionName);
    void printGraph(std::ostream& out) const;
    void printDot(std::ostream& out, const std::string& graphName) const;
    void performLCSE();
    void computeLiveVariables(const SymbolTable& symTab);
    void eliminateDeadCode(const SymbolTable& symTab);
    void reset();

    // --- Members for Live Variable Analysis ---
    std::map<int, BasicBlock> blocks; // Made public for easier access during analysis/generation phases
    int entry_block_id;
    std::set<int> exit_block_ids;

    std::map<int, std::set<std::string>> live_in;  // live_in[block_id] = set of var names
    std::map<int, std::set<std::string>> live_out; // live_out[block_id] = set of var names
    
    // These can be computed transiently or stored if needed for other passes
    // For now, let's compute them within computeLiveVariables
    // std::map<int, std::set<std::string>> def_vars; 
    // std::map<int, std::set<std::string>> use_vars;

    // --- End Members for Live Variable Analysis ---

private:
    int next_block_id_counter;
    std::map<int, int> quad_index_to_block_id_map;
    std::map<std::string, int> label_to_quad_index_map;
    std::map<std::string, int> label_to_block_id_map;
    std::string current_processing_function_name; 

    // Helper predicates
    bool isLabelDefinition(const Quadruple& quad) const;
    bool isUnconditionalJump(const Quadruple& quad) const;
    bool isConditionalJump(const Quadruple& quad) const;
    bool isReturn(const Quadruple& quad) const;
    bool isFunctionCall(const Quadruple& quad) const;
    bool isFunctionBegin(const Quadruple& quad) const;
    bool isFunctionEnd(const Quadruple& quad) const;
    std::string getTargetLabelFromJump(const Quadruple& quad) const;

    std::set<int> identifyLeaders(const std::vector<Quadruple>& quads);
    void partitionIntoBasicBlocks(const std::vector<Quadruple>& quads, const std::set<int>& leaders_set);
    void linkBasicBlocks(const std::vector<Quadruple>& quads);

    // Helper for LCSE
    std::string get_expression_key(const Quadruple& q) const;

    // Helper for Live Variable Analysis
    bool isVariable(const std::string& s, const SymbolTable& symTab) const;

};

#endif // CONTROL_FLOW_GRAPH_H 