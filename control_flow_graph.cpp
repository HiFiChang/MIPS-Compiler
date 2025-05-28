#include "control_flow_graph.h"
#include <stdexcept> // For std::runtime_error in BasicBlock::getLastInstruction
#include <algorithm> // For std::sort in identifyLeaders
#include <iostream>  // For std::cout, std::cerr (used in debug and warnings)

// Define CFG_DEBUG through compiler flags (e.g., -DCFG_DEBUG) to enable debug output.
// Or, uncomment the line below to enable debug output directly in code.
// #define CFG_DEBUG

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
std::string ControlFlowGraph::get_expression_key(const Quadruple& q) const {
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

// Helper for Live Variable Analysis
bool ControlFlowGraph::isVariable(const std::string& s, const SymbolTable& symTab) const {
#ifdef CFG_DEBUG
    std::cout << "[isVariable DBG] Checking: '" << s << "' for func '" << this->current_processing_function_name << "'" << std::endl;
#endif

    if (s.empty() || s == "-" || s == "_") { // Added s == "_" as a common placeholder
#ifdef CFG_DEBUG
        if (s == "_") std::cout << "[isVariable DBG] Result for '_': false (placeholder)" << std::endl;
        else if (s.empty()) std::cout << "[isVariable DBG] Result for empty string: false" << std::endl;
        else std::cout << "[isVariable DBG] Result for '-': false (placeholder)" << std::endl;
#endif
        return false;
    }

    // 1. Check if it's a numeric literal (integer or float)
    char* p_end_char_check; 

    std::strtol(s.c_str(), &p_end_char_check, 10); 
    if (*p_end_char_check == '\0') { 
        if (!(s.length() == 1 && (s[0] == '+' || s[0] == '-'))) { // Not just a sign character
#ifdef CFG_DEBUG
             std::cout << "[isVariable DBG] Result for '" << s << "': false (integer literal)" << std::endl;
#endif
            return false; 
        }
    }

    std::strtod(s.c_str(), &p_end_char_check);
    if (*p_end_char_check == '\0') { 
        if (s.find('.') != std::string::npos || s.find('e') != std::string::npos || s.find('E') != std::string::npos) {
#ifdef CFG_DEBUG
            std::cout << "[isVariable DBG] Result for '" << s << "': false (float literal)" << std::endl;
#endif
            return false;
        }
    }

    // 2. Check for known label prefixes (e.g., _L followed by digits, or _str_lit_)
    if (s.length() > 2 && s[0] == '_' && s[1] == 'L') {
        bool all_digits_after_L = true;
        for (size_t i = 2; i < s.length(); ++i) {
            if (!isdigit(s[i])) {
                all_digits_after_L = false;
                break;
            }
        }
        if (all_digits_after_L) {
#ifdef CFG_DEBUG
            std::cout << "[isVariable DBG] Result for '" << s << "': false (label like _L<digits>)" << std::endl;
#endif
            return false;
        }
    }
    if (s.rfind("_str_lit_", 0) == 0) {
#ifdef CFG_DEBUG
        std::cout << "[isVariable DBG] Result for '" << s << "': false (label _str_lit_)" << std::endl;
#endif
        return false;
    }
    
    // 3. Check for typical temporary variable pattern (e.g., _t followed by digits)
    if (s.rfind("_t", 0) == 0) {
        if (s.length() > 1) { 
            if (s[1] == 't' && s.length() > 2) { 
                 bool all_digits_after_t = true;
                 for (size_t i = 2; i < s.length(); ++i) {
                    if (!isdigit(s[i])) {
                        all_digits_after_t = false;
                        break;
                    }
                }
                if (all_digits_after_t) {
#ifdef CFG_DEBUG
                    std::cout << "[isVariable DBG] Result for '" << s << "': true (temporary _t<digits>)" << std::endl;
#endif
                    return true;
                }
            }
        }
    }
    
    // 4. Query the symbol table
#ifdef CFG_DEBUG
    std::cout << "  [isVariable DBG] Querying SymbolTable for '" << s << "' in func '" << this->current_processing_function_name << "'" << std::endl;
#endif
    Symbol* symbol_ptr = const_cast<SymbolTable&>(symTab).lookupSymbol(s, false, this->current_processing_function_name);
#ifdef CFG_DEBUG
    std::cout << "  [isVariable DBG] SymbolTable lookup for '" << s << "' returned: " << (symbol_ptr ? "FOUND" : "NOT FOUND") << std::endl;
#endif

    if (symbol_ptr != nullptr) {
#ifdef CFG_DEBUG
        std::cout << "    [isVariable DBG] Symbol '" << s << "' type: '" << symbol_ptr->type << "', isParam: " << symbol_ptr->isParam << std::endl;
#endif
        const std::string& sym_type = symbol_ptr->type;
        if (sym_type == "int" || sym_type == "const_int" || 
            sym_type.rfind("array", 0) == 0 || 
            symbol_ptr->isParam) {
#ifdef CFG_DEBUG
            std::cout << "[isVariable DBG] Result for '" << s << "': true (symbol table - var/param)" << std::endl;
#endif
             return true;
        }
        if (sym_type == "int_func" || sym_type == "void_func" || sym_type.rfind("func", sym_type.length() - 4) != std::string::npos) {
#ifdef CFG_DEBUG
            std::cout << "[isVariable DBG] Result for '" << s << "': false (symbol table - function)" << std::endl;
#endif
            return false;
        }
#ifdef CFG_DEBUG
        std::cout << "[isVariable DBG] Result for '" << s << "': false (symbol table - other known type)" << std::endl;
#endif
        return false; 
    }

    // 5. Heuristic for other identifiers
    if (!s.empty() && (isalpha(s[0]) || s[0] == '_')) {
        const static std::set<std::string> IR_OPS = {
            "ADD", "SUB", "MUL", "DIV", "MOD", "GOTO", "ASSIGN", 
            "IF_TRUE_GOTO", "IF_FALSE_GOTO", "CALL", "RETURN_VAL",
            "RETURN_VOID", "FUNC_BEGIN", "FUNC_END", "PRINT_INT",
            "PRINT_STR", "GET_INT", "PARAM", "ARG",
            "LSS", "LEQ", "GRE", "GEQ", "EQL", "NEQ"
        };
        if (IR_OPS.count(s)) {
#ifdef CFG_DEBUG
            std::cout << "[isVariable DBG] Result for '" << s << "': false (IR Op)" << std::endl;
#endif
            return false; 
        }
#ifdef CFG_DEBUG
        std::cout << "[isVariable DBG] Result for '" << s << "': true (heuristic - identifier)" << std::endl;
#endif
        return true; 
    }
    
#ifdef CFG_DEBUG
    std::cout << "[isVariable DBG] Result for '" << s << "': false (default fallback)" << std::endl;
#endif
    return false; 
}

void ControlFlowGraph::computeLiveVariables(const SymbolTable& symTab) {
    live_in.clear();
    live_out.clear();

    if (blocks.empty()) {
        return;
    }

    for (const auto& block_pair : blocks) {
        live_in[block_pair.first] = std::set<std::string>();
        live_out[block_pair.first] = std::set<std::string>();
    }

    std::map<int, std::set<std::string>> local_def_vars;
    std::map<int, std::set<std::string>> local_use_vars;

    for (const auto& block_pair : blocks) {
        int block_id = block_pair.first;
        const BasicBlock& block = block_pair.second;
        std::set<std::string> defined_in_block_for_use_calc; 

        for (const Quadruple& q : block.quads) {
            // Handle CALL instruction specifically for global variable side effects
            if (q.op == "CALL") {
                std::vector<Symbol> global_vars = symTab.getGlobalVariableSymbols();
                for (const Symbol& global_sym : global_vars) {
                    if (isVariable(global_sym.name, symTab)) {
                        // Refined logic for adding to local_use_vars:
                        // Add to USE only if not already defined earlier in this block.
                        if (defined_in_block_for_use_calc.find(global_sym.name) == defined_in_block_for_use_calc.end()) {
                            local_use_vars[block_id].insert(global_sym.name);
                        }
                        // Conservatively assume CALL defines all global variables.
                        local_def_vars[block_id].insert(global_sym.name);
                        // And mark it as defined from this point onwards in the block for subsequent quads.
                        defined_in_block_for_use_calc.insert(global_sym.name); 
                    }
                }
                // The result of the CALL is also a definition
                if (!q.result.empty() && isVariable(q.result, symTab)) {
                    local_def_vars[block_id].insert(q.result);
                    defined_in_block_for_use_calc.insert(q.result);
                }
            } else { // Existing logic for non-CALL instructions
                if (!q.arg1.empty() && isVariable(q.arg1, symTab)) {
                    if (defined_in_block_for_use_calc.find(q.arg1) == defined_in_block_for_use_calc.end()) {
                        local_use_vars[block_id].insert(q.arg1);
                    }
                }
                if (!q.arg2.empty() && isVariable(q.arg2, symTab)) {
                    if (defined_in_block_for_use_calc.find(q.arg2) == defined_in_block_for_use_calc.end()) {
                        local_use_vars[block_id].insert(q.arg2);
                    }
                }
                if (!q.result.empty() && isVariable(q.result, symTab)) {
                    local_def_vars[block_id].insert(q.result);
                    defined_in_block_for_use_calc.insert(q.result);
                }
            }
            // PARAM instruction might also need special handling if its argument can be a variable from a prior computation
            // For now, focusing on CALL for global variable effects.
        }
    }

    bool changed = true;
    while (changed) {
        changed = false;
        std::vector<int> block_ids_to_process;
        for(const auto& pair : blocks) block_ids_to_process.push_back(pair.first);
        std::sort(block_ids_to_process.rbegin(), block_ids_to_process.rend());

        for (int block_id : block_ids_to_process) {
            const BasicBlock& current_block = blocks.at(block_id);
            std::set<std::string> new_live_out;
            for (int succ_id : current_block.successors) {
                if (live_in.count(succ_id)) {
                    const auto& succ_live_in = live_in.at(succ_id);
                    new_live_out.insert(succ_live_in.begin(), succ_live_in.end());
                }
            }
            if (live_out[block_id] != new_live_out) {
                live_out[block_id] = new_live_out;
                changed = true;
            }

            std::set<std::string> live_out_minus_def = live_out[block_id];
            if (local_def_vars.count(block_id)){
                for (const std::string& def_var : local_def_vars.at(block_id)) {
                    live_out_minus_def.erase(def_var);
                }
            }
            
            std::set<std::string> new_live_in = local_use_vars[block_id];
            new_live_in.insert(live_out_minus_def.begin(), live_out_minus_def.end());

            if (live_in[block_id] != new_live_in) {
                live_in[block_id] = new_live_in;
                changed = true;
            }
        }
    }
    
#ifdef CFG_DEBUG    
    std::cout << "\n--- Live Variable Analysis Results for Function: " << this->current_processing_function_name << " ---" << std::endl;
    // Sort block IDs for consistent output order
    std::vector<int> sorted_block_ids_for_print;
    for (const auto& block_pair : blocks) {
        sorted_block_ids_for_print.push_back(block_pair.first);
    }
    std::sort(sorted_block_ids_for_print.begin(), sorted_block_ids_for_print.end());

    for (int id : sorted_block_ids_for_print) {
        std::cout << "Block " << id << ":\n";
        // Ensure key exists before accessing, or use .at() inside a try-catch / check with .count()
        std::cout << "  DEF: "; 
        if(local_def_vars.count(id)) for(const auto&v : local_def_vars.at(id)) std::cout << v << " "; 
        std::cout << "\n";
        
        std::cout << "  USE: "; 
        if(local_use_vars.count(id)) for(const auto&v : local_use_vars.at(id)) std::cout << v << " "; 
        std::cout << "\n";
        
        std::cout << "  LiveIN:  "; 
        if(live_in.count(id)) for(const auto&v : live_in.at(id)) std::cout << v << " "; 
        std::cout << "\n";
        
        std::cout << "  LiveOUT: "; 
        if(live_out.count(id)) for(const auto&v : live_out.at(id)) std::cout << v << " "; 
        std::cout << "\n";
    }
#endif    
}

void ControlFlowGraph::eliminateDeadCode(const SymbolTable& symTab) {
    if (blocks.empty() || live_out.empty()) { // Ensure liveness has been computed
        // Optionally, print a warning or throw an error if liveness info is missing
        std::cerr << "Warning: Skipping dead code elimination as liveness information is not available or CFG is empty." << std::endl;
        return;
    }

#ifdef CFG_DEBUG
    std::cout << "\n--- Performing Dead Code Elimination for Function: " << this->current_processing_function_name << " ---" << std::endl;
#endif

    for (auto& block_pair : blocks) {
        BasicBlock& block = block_pair.second;
        if (block.quads.empty()) {
            continue;
        }

        std::set<std::string> live_now = live_out[block.id]; // Initialize with LiveOUT for the current block
        std::vector<Quadruple> new_quads_for_block;

        // Iterate quads in reverse order (from last to first)
        for (auto it = block.quads.rbegin(); it != block.quads.rend(); ++it) {
            const Quadruple& q = *it;
            bool is_this_quad_dead = false;

            // Determine if this instruction is a candidate for DCE
            // Ops that primarily define a variable and have no other essential side effects.
            bool op_can_be_eliminated = 
                (q.op == "ASSIGN" || q.op == "ADD" || q.op == "SUB" || 
                 q.op == "MUL" || q.op == "DIV" || q.op == "MOD" || 
                 q.op == "LSS" || q.op == "LEQ" || q.op == "GRE" || 
                 q.op == "GEQ" || q.op == "EQL" || q.op == "NEQ");
                 // Note: GET_INT also defines a variable but has side effects (I/O).
                 // CALL also has side effects and its result definition is handled by liveness.

            if (op_can_be_eliminated && !q.result.empty() && isVariable(q.result, symTab)) {
                Symbol* result_sym = const_cast<SymbolTable&>(symTab).lookupSymbol(q.result, true, this->current_processing_function_name);
                bool is_assignment_to_global = (q.op == "ASSIGN" && result_sym && result_sym->isGlobal);

                // If the result of this instruction is not in live_now, it's potentially dead.
                // However, an assignment to a global variable is a side effect and should not be eliminated solely based on function-local liveness.
                if (live_now.find(q.result) == live_now.end()) {
                    if (!is_assignment_to_global) {
                        is_this_quad_dead = true;
#ifdef CFG_DEBUG
                        std::cout << "  Dead Code Identified in Block " << block.id << ": (" 
                                  << q.op << ", " << q.arg1 << ", " << q.arg2 << ", " << q.result << ")\n";
#endif
                    } else {
#ifdef CFG_DEBUG
                        std::cout << "  [DCE_INFO] Preserving ASSIGN to global var: (" 
                                  << q.op << ", " << q.arg1 << ", " << q.arg2 << ", " << q.result << ") despite result not in local live_now.\n";
#endif
                    }
                }
            }

            if (!is_this_quad_dead) {
                new_quads_for_block.push_back(q);
            }

            // Update live_now for the state *before* this instruction q was executed.
            // live_now = (live_now - DEF(q)) U USE(q)
            
            // Ops that primarily USE q.arg1 and don't define q.result (or define memory/side-effect)
            if (q.op == "STORE_TO_ADDR") { // Example: (STORE_TO_ADDR, target_addr_temp, _, value_source_in_q_result)
                if (!q.arg1.empty() && isVariable(q.arg1, symTab)) { // target_addr_temp is used
                    live_now.insert(q.arg1);
                }
                if (!q.result.empty() && isVariable(q.result, symTab)) { // value_source_in_q_result is used
                    live_now.insert(q.result);
                }
                // Does not define a temporary in q.result for liveness tracking purposes of erase(q.result).
            } else if (q.op == "PRINT_INT" || q.op == "PRINT_STR" || 
                       q.op == "RETURN_VAL" || q.op == "ARG" ||
                       q.op == "IF_TRUE_GOTO" || q.op == "IF_FALSE_GOTO" ||
                       q.op == "GOTO" /* GOTO uses q.result (label) but not for var liveness */) {
                // These ops primarily USE q.arg1 (and sometimes q.arg2 for IFs if it's a var).
                // q.result for these is often a label or empty, not a defined temporary whose definition would be erased.
                if (!q.arg1.empty() && isVariable(q.arg1, symTab)) {
                    live_now.insert(q.arg1);
                }
                // For IF_XY_GOTO, arg2 is typically the target label, not a variable used in calculation.
                // If your IF ops could use a variable in arg2, you'd add: 
                // if ((q.op == "IF_TRUE_GOTO" || q.op == "IF_FALSE_GOTO") && !q.arg2.empty() && isVariable(q.arg2, symTab)) {
                //    live_now.insert(q.arg2);
                // }
            }
            // CALL instruction is a bit special: it defines q.result, and uses ARGs pushed before it.
            // The current loop processes one instruction at a time, so ARG liveness is handled when ARG is processed.
            // FUNC_BEGIN, FUNC_END, label definitions don't directly participate in this live_now update for vars.
            else { 
                // Default handling for computational/assignment ops (ADD, MUL, ASSIGN, LOAD_FROM_ADDR, GET_INT, CALL etc.)
                // These are assumed to define q.result.
                if (!q.result.empty() && isVariable(q.result, symTab)) {
                    live_now.erase(q.result); // DEF(q) - remove defined variable from live_now
                }

                // Add what q uses to live_now
                if (!q.arg1.empty() && isVariable(q.arg1, symTab)) {
                    live_now.insert(q.arg1); // USE(q)
                }
                if (!q.arg2.empty() && isVariable(q.arg2, symTab)) {
                    live_now.insert(q.arg2); // USE(q)
                }
            }
        }

        // The new_quads_for_block contains non-dead quads in reverse order.
        // Reverse it to get the correct order and update the block's quads.
        std::reverse(new_quads_for_block.begin(), new_quads_for_block.end());
        block.quads = new_quads_for_block;
    }
#ifdef CFG_DEBUG
     std::cout << "--- Dead Code Elimination Completed for Function: " << this->current_processing_function_name << " ---" << std::endl;
#endif
} 