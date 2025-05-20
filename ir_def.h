#ifndef MINE_IR_DEF_H_
#define MINE_IR_DEF_H_

#include <string>

// Quadruple structure
struct Quadruple {
    std::string op;
    std::string arg1;
    std::string arg2;
    std::string result;
    Quadruple(std::string o, std::string a1, std::string a2, std::string r); // Constructor declaration
};

#endif // MINE_IR_DEF_H_ 