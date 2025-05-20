#include "symbol_entry.h"

// This file can be used for definitions of Symbol methods if they are not inlined in the header.
// Currently, the Symbol constructor is inlined in symbol_entry.h, so its definition here was removed to prevent redefinition.

// Symbol structure for Symbol Table
// ... (Lines 43-44: Symbol constructor implementation from lab6.cpp)
// Symbol::Symbol() : scopeLevel(0), value(0), isConstant(false), isInitialized(false), isGlobal(false), offset(0), isArray(false) {} 