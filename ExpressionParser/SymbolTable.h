#ifndef SYMBOLTABLE_H
#define SYMBOLTABLE_H

#include "SymbolTableEntry.h"

class SymbolTable {
public:
	virtual SymbolTableEntry* getEntry(string identifierString)=0; 
	virtual ~SymbolTable() {}
};

#endif
