#include "ir_generator.h"
#include <utility> // For std::move

// IRGenerator class implementation from lab6.cpp
// ... (Lines 165 to 187 from original lab6.cpp, containing IRGenerator implementation)
IRGenerator::IRGenerator() : tempVarCount(0), labelCount(0) {}

std::string IRGenerator::newTemp() {
    return "_t" + std::to_string(tempVarCount++);
}

std::string IRGenerator::newLabel() {
    return "_L" + std::to_string(labelCount++);
}

void IRGenerator::addQuad(const std::string& op, const std::string& arg1, const std::string& arg2, const std::string& result) {
    quadruples.emplace_back(op, arg1, arg2, result);
}

const std::vector<Quadruple>& IRGenerator::getQuads() const {
    return quadruples;
}

void IRGenerator::printQuads(std::ostream& out) const {
    for (const auto& q : quadruples) {
        out << "(" << q.op << ", " << q.arg1 << ", " << q.arg2 << ", " << q.result << ")" << std::endl;
    }
} 