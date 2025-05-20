#include "ir_def.h"
#include <utility> // For std::move

// Quadruple structure constructor
// ... (Line 157 from original lab6.cpp, containing Quadruple constructor)
Quadruple::Quadruple(std::string o, std::string a1, std::string a2, std::string r) :
    op(std::move(o)), arg1(std::move(a1)), arg2(std::move(a2)), result(std::move(r)) {} 