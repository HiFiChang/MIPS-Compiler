#include "control_flow_graph.h"
#include <stdexcept> // For std::runtime_error in BasicBlock::getLastInstruction
#include <algorithm> // For std::sort in identifyLeaders

ControlFlowGraph::ControlFlowGraph() {
    reset();
}

void ControlFlowGraph::reset() {
    blocks.clear();
    entry_block_id = -1;
    exit_block_ids.clear();
    next_block_id_counter = 0;
    quad_index_to_block_id_map.clear();
    label_to_quad_index_map.clear();
    label_to_block_id_map.clear();
}

// Helper predicates based on mips_generator.cpp analysis

bool ControlFlowGraph::isLabelDefinition(const Quadruple& quad) const {
    // Quad op itself is the label string ending with ':'
    return !quad.op.empty() && quad.op.back() == ':';
}

bool ControlFlowGraph::isUnconditionalJump(const Quadruple& quad) const {
    return quad.op == "GOTO" || quad.op == "RETURN_VAL" || quad.op == "RETURN_VOID";
}

bool ControlFlowGraph::isConditionalJump(const Quadruple& quad) const {
    return quad.op == "IF_TRUE_GOTO" || quad.op == "IF_FALSE_GOTO";
}

bool ControlFlowGraph::isReturn(const Quadruple& quad) const {
    return quad.op == "RETURN_VAL" || quad.op == "RETURN_VOID";
}

bool ControlFlowGraph::isFunctionCall(const Quadruple& quad) const {
    return quad.op == "CALL";
}

bool ControlFlowGraph::isFunctionBegin(const Quadruple& quad) const {
    return quad.op == "FUNC_BEGIN";
}

bool ControlFlowGraph::isFunctionEnd(const Quadruple& quad) const {
    return quad.op == "FUNC_END";
}

std::string ControlFlowGraph::getTargetLabelFromJump(const Quadruple& quad) const {
    if (quad.op == "GOTO" || quad.op == "IF_TRUE_GOTO" || quad.op == "IF_FALSE_GOTO") {
        return quad.result;
    }
    // Use the stored current_processing_function_name for returns
    if (isReturn(quad) && !this->current_processing_function_name.empty()) {
        return "_L_" + this->current_processing_function_name + "_epilogue"; // No colon
    }
    return "";
}

std::set<int> ControlFlowGraph::identifyLeaders(const std::vector<Quadruple>& quads) {
    std::set<int> leaders;
    if (quads.empty()) {
        return leaders;
    }

    // Rule 1: The first quadruple is a leader.
    leaders.insert(0);

    for (size_t i = 0; i < quads.size(); ++i) {
        const auto& quad = quads[i];

        // Rule 2: Any quadruple that is the target of a jump is a leader.
        if (isUnconditionalJump(quad) || isConditionalJump(quad)) {
            std::string target_label_str = getTargetLabelFromJump(quad);
            if (!target_label_str.empty()) {
                // Find the quadruple index for this label
                auto it = label_to_quad_index_map.find(target_label_str);
                if (it != label_to_quad_index_map.end()) {
                    leaders.insert(it->second);
                }
            }
        }
        // For RETURN_VAL and RETURN_VOID, they jump to an epilogue label.
        // The epilogue label itself (e.g., "_L_main_epilogue:") will be identified as a leader by Rule 4.

        // Rule 3: Any quadruple immediately following a jump or return is a leader.
        if (isUnconditionalJump(quad) || isConditionalJump(quad) || isReturn(quad)) {
            if (i + 1 < quads.size()) {
                leaders.insert(i + 1);
            }
        }

        // Rule 4: A label definition is a leader.
        if (isLabelDefinition(quad)) {
            leaders.insert(i);
        }
        // FUNC_BEGIN is already covered by Rule 1 if it's the first, or by label if it is one.
        // No, FUNC_BEGIN should always be a leader if present (might not be 0th quad if processing sub-function quads)
        if (isFunctionBegin(quad)) {
             leaders.insert(i);
        }
    }
    return leaders;
}

void ControlFlowGraph::partitionIntoBasicBlocks(const std::vector<Quadruple>& quads, const std::set<int>& leaders_set) {
    if (quads.empty()) return;

    // Sort leaders to process in order
    std::vector<int> leaders(leaders_set.begin(), leaders_set.end());
    std::sort(leaders.begin(), leaders.end());

    for (size_t i = 0; i < leaders.size(); ++i) {
        int leader_idx = leaders[i];
        int current_block_id = next_block_id_counter++;
        BasicBlock current_block(current_block_id);

        if (i == 0 && isFunctionBegin(quads[leader_idx])) {
            entry_block_id = current_block_id;
        } else if (entry_block_id == -1 && !quads.empty() && leader_idx == 0) {
            // Fallback if no FUNC_BEGIN but we have quads (e.g. global init code CFG)
            entry_block_id = current_block_id;
        }

        int end_idx;
        if (i + 1 < leaders.size()) {
            end_idx = leaders[i+1] -1; // Block ends just before the next leader
        } else {
            end_idx = quads.size() - 1; // Last leader, block goes to the end of quads
        }

        for (int k = leader_idx; k <= end_idx; ++k) {
            current_block.addQuad(quads[k]);
            quad_index_to_block_id_map[k] = current_block_id;
            if (isLabelDefinition(quads[k])) {
                std::string label_name = quads[k].op;
                label_name.pop_back(); // Remove trailing ':'
                label_to_block_id_map[label_name] = current_block_id;
            }
        }
        blocks[current_block_id] = current_block;
    }
}

void ControlFlowGraph::linkBasicBlocks(const std::vector<Quadruple>& quads) {
    for (auto& pair_block : blocks) { // Use a different name to avoid conflict with potential 'block' var from outer scope
        int block_id = pair_block.first;
        BasicBlock& current_block = pair_block.second; // Renamed 'block' to 'current_block'

        if (current_block.quads.empty()) continue;

        const Quadruple& last_quad = current_block.getLastInstruction();
        
        // Simplified way to get original index of the last quad IF BasicBlock stores original indices or quads are not copied
        // For now, we still rely on the less efficient search as per previous logic until BasicBlock is changed.
        int last_quad_original_index = -1;
        int max_idx_for_block = -1;
        for(const auto& q_map_entry : quad_index_to_block_id_map) {
            if (q_map_entry.second == block_id) {
                if (q_map_entry.first > max_idx_for_block) {
                    max_idx_for_block = q_map_entry.first;
                }
            }
        }
        last_quad_original_index = max_idx_for_block;

        if (isUnconditionalJump(last_quad)) {
            std::string target_label = getTargetLabelFromJump(last_quad); // Will get epilogue for returns
            if (!target_label.empty() && label_to_block_id_map.count(target_label)) {
                int target_block_id = label_to_block_id_map.at(target_label);
                current_block.successors.insert(target_block_id);
                blocks[target_block_id].predecessors.insert(block_id);
            }
            if (isReturn(last_quad)) { // Returns are unconditional jumps to epilogue
                exit_block_ids.insert(block_id);
            }
        } else if (isConditionalJump(last_quad)) {
            std::string target_label = getTargetLabelFromJump(last_quad);
            if (!target_label.empty() && label_to_block_id_map.count(target_label)) {
                int target_block_id = label_to_block_id_map.at(target_label);
                current_block.successors.insert(target_block_id);
                blocks[target_block_id].predecessors.insert(block_id);
            }
            // Fall-through path
            if (last_quad_original_index != -1 && static_cast<size_t>(last_quad_original_index) + 1 < quads.size()) {
                if (quad_index_to_block_id_map.count(last_quad_original_index + 1)) {
                    int fallthrough_block_id = quad_index_to_block_id_map.at(last_quad_original_index + 1);
                    // Simplified fall-through logic: if the next instruction exists and is in a block, link to it.
                    // The leader identification ensures it's a new block if it's not part of current block.
                    current_block.successors.insert(fallthrough_block_id);
                    blocks[fallthrough_block_id].predecessors.insert(block_id);
                }
            }
        } else if (isFunctionEnd(last_quad)) {
             exit_block_ids.insert(block_id);
        } else {
            // Sequential flow
            if (last_quad_original_index != -1 && static_cast<size_t>(last_quad_original_index) + 1 < quads.size()) {
                 if (quad_index_to_block_id_map.count(last_quad_original_index + 1)) {
                    int sequential_block_id = quad_index_to_block_id_map.at(last_quad_original_index + 1);
                    if (sequential_block_id != block_id) { 
                        // Check if the next block is not just a FUNC_END block that's already an exit
                        const auto& next_block_iter = blocks.find(sequential_block_id);
                        if (next_block_iter != blocks.end()) {
                            const BasicBlock& next_b = next_block_iter->second;
                            if (!next_b.quads.empty() && !isFunctionEnd(next_b.getLastInstruction())) {
                                 current_block.successors.insert(sequential_block_id);
                                 blocks[sequential_block_id].predecessors.insert(block_id);
                            } else if (!next_b.quads.empty() && isFunctionEnd(next_b.getLastInstruction())) {
                                // If it flows into a FUNC_END block, the current block is effectively an exit path
                                exit_block_ids.insert(block_id);
                            }
                        }
                    }
                }
            } else {
                 exit_block_ids.insert(block_id);
            }
        }
    }
}

void ControlFlowGraph::build(const std::vector<Quadruple>& quads_for_function, const std::string& functionName) {
    reset();
    this->current_processing_function_name = functionName;
    if (quads_for_function.empty()) {
        return;
    }

    for (size_t i = 0; i < quads_for_function.size(); ++i) {
        if (isLabelDefinition(quads_for_function[i])) {
            std::string label_name = quads_for_function[i].op;
            label_name.pop_back(); // Remove trailing ':'
            label_to_quad_index_map[label_name] = i;
        }
        if (isFunctionEnd(quads_for_function[i])) {
            // Ensure current_processing_function_name is used for consistency.
            // The FUNC_END quad's arg1 might also hold the function name, but using the passed one is safer.
            if (!this->current_processing_function_name.empty()) {
                std::string epilogueLabelName = "_L_" + this->current_processing_function_name + "_epilogue"; // No colon
                // This epilogue label logically points to the FUNC_END instruction itself.
                if (label_to_quad_index_map.find(epilogueLabelName) == label_to_quad_index_map.end()) {
                    label_to_quad_index_map[epilogueLabelName] = i; // i is the index of FUNC_END
                }
            }
        }
    }

    std::set<int> leaders = identifyLeaders(quads_for_function);
    partitionIntoBasicBlocks(quads_for_function, leaders);

    // Supplemental step to ensure all labels in label_to_quad_index_map (especially epilogues)
    // are correctly mapped to block IDs in label_to_block_id_map.
    for (const auto& q_pair : label_to_quad_index_map) {
        const std::string& label_name = q_pair.first;
        int quad_idx_for_label = q_pair.second;
        if (label_to_block_id_map.find(label_name) == label_to_block_id_map.end()) {
            if (quad_index_to_block_id_map.count(quad_idx_for_label)) {
                label_to_block_id_map[label_name] = quad_index_to_block_id_map.at(quad_idx_for_label);
            }
        }
    }

    linkBasicBlocks(quads_for_function);
}

void ControlFlowGraph::printGraph(std::ostream& out) const {
    out << "Control Flow Graph:\n";
    out << "Entry Block ID: " << entry_block_id << "\n";
    if (blocks.empty()) {
        out << "  Graph is empty.\n";
        return;
    }

    // Sort block IDs for consistent output order
    std::vector<int> block_ids;
    for (const auto& pair : blocks) {
        block_ids.push_back(pair.first);
    }
    std::sort(block_ids.begin(), block_ids.end());

    for (int id : block_ids) {
        const auto& block_pair = blocks.find(id);
        if (block_pair == blocks.end()) continue;
        const BasicBlock& block = block_pair->second;

        out << "\nBasic Block ID: " << block.id;
        if (id == entry_block_id) out << " (ENTRY)";
        if (exit_block_ids.count(id)) out << " (EXIT)";
        out << "\n";
        
        out << "  Quads:\n";
        for (const auto& quad : block.quads) {
            out << "    (" << quad.op << ", " 
                << (quad.arg1.empty() ? "-" : quad.arg1) << ", " 
                << (quad.arg2.empty() ? "-" : quad.arg2) << ", " 
                << (quad.result.empty() ? "-" : quad.result) << ")\n";
        }
        out << "  Predecessors: ";
        for (int pred_id : block.predecessors) {
            out << pred_id << " ";
        }
        out << "\n";
        out << "  Successors: ";
        for (int succ_id : block.successors) {
            out << succ_id << " ";
        }
        out << "\n";
    }
}

void ControlFlowGraph::printDot(std::ostream& out, const std::string& graphName) const {
    out << "digraph \"" << graphName << "\" {\n";
    out << "  node [shape=record];\n";

    if (blocks.empty()) {
        out << "  empty [label=\"Empty Graph\"];\n";
        out << "}\n";
        return;
    }

    // Sort block IDs for consistent DOT output order
    std::vector<int> block_ids;
    for (const auto& pair : blocks) {
        block_ids.push_back(pair.first);
    }
    std::sort(block_ids.begin(), block_ids.end());

    for (int id : block_ids) {
        const auto& block_pair = blocks.find(id);
        if (block_pair == blocks.end()) continue;
        const BasicBlock& block = block_pair->second;

        out << "  bb" << block.id << " [label=\"{BB" << block.id;
        if (id == entry_block_id) out << " (ENTRY)";
        if (exit_block_ids.count(id)) out << " (EXIT)";
        out << "|";
        for (const auto& quad : block.quads) {
            std::string q_original_str = "(";
            q_original_str += quad.op + ", ";
            q_original_str += (quad.arg1.empty() ? "-" : quad.arg1) + ", ";
            q_original_str += (quad.arg2.empty() ? "-" : quad.arg2) + ", ";
            q_original_str += (quad.result.empty() ? "-" : quad.result) + ")";

            std::string q_escaped_str;
            for (char ch : q_original_str) {
                switch (ch) {
                    case '<': q_escaped_str += "&lt;"; break; // HTML-like entity for DOT
                    case '>': q_escaped_str += "&gt;"; break; // HTML-like entity for DOT
                    case '"': q_escaped_str += "&quot;"; break; // HTML-like entity or \"
                    case '&': q_escaped_str += "&amp;"; break; // Important for HTML-like entities
                    case '|': q_escaped_str += "\\|"; break; // Escape for DOT record
                    case '{': q_escaped_str += "\\{"; break; // Escape for DOT record
                    case '}': q_escaped_str += "\\}"; break; // Escape for DOT record
                    // Add other necessary escapes here if needed e.g. for backslash itself \\
                    default:
                        q_escaped_str += ch;
                        break;
                }
            }
            out << q_escaped_str << "\\l"; // \l for left-justified newlines in DOT record
        }
        out << "}\"];\n";

        for (int succ_id : block.successors) {
            out << "  bb" << block.id << " -> bb" << succ_id << ";\n";
        }
    }
    if (!blocks.count(entry_block_id) && entry_block_id != -1) {
         out << "  // Entry block ID " << entry_block_id << " not found in blocks map.\n";
    }

    out << "}\n";
}

// Local Common Subexpression Elimination Helper: Identifies ops that compute values.
const std::set<std::string> COMPUTATIONAL_OPS = {
    "ADD", "SUB", "MUL", "DIV", "MOD", 
    "LSS", "LEQ", "GRE", "GEQ", "EQL", "NEQ"
};

// Local Common Subexpression Elimination Helper: Generates a canonical key for an expression.
std::string get_expression_key(const Quadruple& q) {
    std::string key = q.op + ":";
    if (q.op == "ADD" || q.op == "MUL" || q.op == "EQL" || q.op == "NEQ") {
        // Commutative operations: sort operands for canonical form
        if (q.arg1 < q.arg2) {
            key += q.arg1 + ":" + q.arg2;
        } else {
            key += q.arg2 + ":" + q.arg1;
        }
    } else {
        // Non-commutative or unary operations: order matters or arg2 might be empty
        key += q.arg1;
        if (!q.arg2.empty()) { // Only add arg2 if it's not empty
            key += ":" + q.arg2;
        }
    }
    return key;
}

void ControlFlowGraph::performLCSE() {
    for (auto& block_pair : blocks) {
        BasicBlock& block = block_pair.second;
        if (block.quads.empty()) continue;

        std::map<std::string, std::string> available_expressions; // expr_key -> result_temp_var
        // Tracks which expressions (keys) depend on a variable, to invalidate them upon redefinition.
        std::map<std::string, std::vector<std::string>> var_to_dependent_expr_keys;

        for (Quadruple& q : block.quads) { // Iterate by reference to allow modification
            // Step 1: Check if current instruction's result redefines any variable that invalidates existing available expressions.
            // This primarily concerns q.result if it was an operand in a previous expression.
            // More robustly: any variable assigned to by this instruction (q.result for most, specific args for others like GET_INT).
            // For simplicity in this first pass, we focus on q.result for computational ops and ASSIGN.
            std::string defined_var_by_current_quad;
            if (COMPUTATIONAL_OPS.count(q.op) || q.op == "ASSIGN" || q.op == "GET_INT") {
                 defined_var_by_current_quad = q.result;
            }
            // Add other ops that define variables in q.result or other args as needed.

            if (!defined_var_by_current_quad.empty() && var_to_dependent_expr_keys.count(defined_var_by_current_quad)) {
                for (const std::string& expr_key_to_remove : var_to_dependent_expr_keys[defined_var_by_current_quad]) {
                    available_expressions.erase(expr_key_to_remove);
                }
                var_to_dependent_expr_keys.erase(defined_var_by_current_quad); // Clear dependencies for this var
            }
            
            // Step 2: Check if this quad computes an expression that can be an LCSE candidate.
            if (COMPUTATIONAL_OPS.count(q.op)) {
                std::string expr_key = get_expression_key(q);

                if (available_expressions.count(expr_key)) {
                    // Common subexpression found!
                    std::string prev_result_var = available_expressions.at(expr_key);
                    
                    // Before changing q.op, if q.result was depended upon, clear those older dependencies.
                    // This is because q.result will now hold the value of prev_result_var, not a new computation.
                    if (var_to_dependent_expr_keys.count(q.result)) {
                         for (const std::string& expr_key_to_remove : var_to_dependent_expr_keys[q.result]) {
                            // Be careful not to remove the current expr_key if q.result was part of itself (e.g. _t0 = _t0 + x)
                            // Though, for LCSE, this typically won't be an issue as we are looking for expr_key in available_expressions.
                            if (expr_key_to_remove != expr_key) { // Avoid self-invalidation if somehow possible
                                available_expressions.erase(expr_key_to_remove);
                            }
                        }
                        var_to_dependent_expr_keys.erase(q.result);
                    }

                    // Replace current quad with an assignment
                    q.op = "ASSIGN";
                    q.arg1 = prev_result_var;
                    q.arg2 = ""; // Or some indicator for unused operand
                    // q.result remains the same

                    // The new ASSIGN q.result = prev_result_var also makes q.result hold the value of expr_key.
                    // So, we update available_expressions to point to q.result for this expr_key if it's different, 
                    // or simply ensure it is still available via q.result.
                    // More importantly, if prev_result_var is different from q.result, then q.result now aliases prev_result_var for this expression.
                    // For simplicity, we can just re-add/update the current q.result as providing the expr_key
                    // available_expressions[expr_key] = q.result; // This line might be contentious / needs careful thought.
                                                        // If q.result is different from prev_result_var, it means we have two vars holding the value.
                                                        // For CSE purpose, it's often better to keep the FIRST one that computed it.
                                                        // However, the current quad IS q.result = prev_result_var. So q.result is now an alias.
                                                        // Let's stick to: the CSE is identified by its *original* result var.
                                                        // The transformation is q.result = prev_result_var.
                                                        // No new entry to available_expressions from this ASSIGN, but q.result is now defined.

                } else {
                    // New expression, add it to available expressions.
                    available_expressions[expr_key] = q.result;
                    // Record dependencies: if arg1/arg2 are variables, this expr_key depends on them.
                    if (!q.arg1.empty() /* && is_variable(q.arg1) */) { // Assuming args are var names or empty
                        var_to_dependent_expr_keys[q.arg1].push_back(expr_key);
                    }
                    if (!q.arg2.empty() /* && is_variable(q.arg2) */) { // And not a constant literal etc.
                        var_to_dependent_expr_keys[q.arg2].push_back(expr_key);
                    }
                }
            }
             // Step 3: After processing the quad (either as computation or after potential modification to ASSIGN),
            // if the quad's original op (or new op if modified) defines q.result, and q.result was an operand
            // in other available expressions, those expressions are now invalid.
            // This is mostly handled by the check at the beginning of the loop for defined_var_by_current_quad.
            // However, if an ASSIGN operation like q.result = some_val occurs, q.result is defined.
            if (q.op == "ASSIGN") { // If it became an assign due to CSE, or was already an assign.
                // The variable q.result is being (re)defined.
                // Invalidate expressions that used the *old* value of q.result as an operand.
                if (var_to_dependent_expr_keys.count(q.result)) {
                    for (const std::string& expr_key_to_remove : var_to_dependent_expr_keys[q.result]) {
                         // If this assign is `q.result = available_expressions[expr_key]`, and `expr_key` itself
                         // used `q.result` as an operand (e.g. `_t0 = _t0 + 1`), this needs careful handling.
                         // However, `available_expressions[expr_key]` would have already been `_t0` in that case.
                         // The current logic should be okay: if _t0 is redefined, any expr using OLD _t0 is removed.
                        available_expressions.erase(expr_key_to_remove);
                    }
                    var_to_dependent_expr_keys.erase(q.result);
                }
                 // If it's an assign like `q.result = q.arg1` (copy propagation candidate)
                // and `q.arg1` corresponds to an available expression (e.g. `q.arg1` is `_t5` which is result of `exprA`)
                // then `q.result` now also makes `exprA` available.
                // This can make available_expressions more dense. For an initial LCSE, this might be too complex.
                // Let's keep it simple: An ASSIGN defines q.result, invalidating things that used its old value.
            }
        }
    }
} 