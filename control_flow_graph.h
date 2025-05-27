#ifndef CONTROL_FLOW_GRAPH_H
#define CONTROL_FLOW_GRAPH_H

#include "ir_def.h" // For Quadruple
#include <vector>
#include <string>
#include <map>
#include <set>
#include <iostream> // For printGraph ostream

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
    std::map<int, BasicBlock> blocks;       // Map block ID to BasicBlock object
    int entry_block_id;                     // ID of the entry basic block for the function
    std::set<int> exit_block_ids;           // IDs of basic blocks that end with a return/function end

    // Stores the original linear sequence of quads for the current function being processed.
    // This can be useful if BasicBlock stores indices instead of copies of quads.
    // const std::vector<Quadruple>* source_quads_ptr; 

    ControlFlowGraph();

    // Builds the CFG for a single function's quadruples
    // Assumes quads_for_function is a complete sequence for one function,
    // starting with FUNC_BEGIN and ending appropriately (e.g. before next FUNC_BEGIN or EOF).
    void build(const std::vector<Quadruple>& quads_for_function, const std::string& functionName);

    void printGraph(std::ostream& out) const;
    void printDot(std::ostream& out, const std::string& graphName = "CFG") const; // For Graphviz DOT format

    void performLCSE(); // Local Common Subexpression Elimination

private:
    int next_block_id_counter;
    
    // Maps original quadruple index (from input vector) to the ID of the basic block it belongs to.
    std::map<int, int> quad_index_to_block_id_map;
    
    // Maps label strings to the original index of the quadruple that defines this label.
    std::map<std::string, int> label_to_quad_index_map;
    
    // Maps label strings directly to the ID of the basic block that starts with this label.
    // This is populated after blocks are formed.
    std::map<std::string, int> label_to_block_id_map;

    std::string current_processing_function_name;

    void reset(); // Resets internal state for building a new graph

    // Step 1: Identify leader instructions (by their indices in the input quad vector)
    std::set<int> identifyLeaders(const std::vector<Quadruple>& quads);
    
    // Step 2: Partition quadruples into basic blocks based on leaders
    void partitionIntoBasicBlocks(const std::vector<Quadruple>& quads, const std::set<int>& leaders);
    
    // Step 3: Link basic blocks by identifying successors and predecessors
    void linkBasicBlocks(const std::vector<Quadruple>& quads);

    // Helper predicates for quadruple operations
    bool isLabelDefinition(const Quadruple& quad) const;
    bool isUnconditionalJump(const Quadruple& quad) const;
    bool isConditionalJump(const Quadruple& quad) const;
    bool isReturn(const Quadruple& quad) const;
    bool isFunctionCall(const Quadruple& quad) const; // May not directly affect block ends, but good to identify
    bool isFunctionBegin(const Quadruple& quad) const;
    bool isFunctionEnd(const Quadruple& quad) const;
    
    // Helper to get target label from jump instructions
    std::string getTargetLabelFromJump(const Quadruple& quad) const;
};

#endif // CONTROL_FLOW_GRAPH_H 