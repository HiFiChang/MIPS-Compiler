#ifndef MINE_IR_GENERATOR_H_
#define MINE_IR_GENERATOR_H_

#include "ir_def.h" 
#include <string>
#include <vector>
#include <iostream> 

class IRGenerator {
public:
    std::vector<Quadruple> quadruples;
    int tempVarCount;
    int labelCount;

    IRGenerator();
    std::string newTemp();
    std::string newLabel();
    void addQuad(const std::string& op, const std::string& arg1, const std::string& arg2, const std::string& result);
    const std::vector<Quadruple>& getQuads() const;
    void printQuads(std::ostream& out = std::cout) const;
};

#endif // MINE_IR_GENERATOR_H_ 