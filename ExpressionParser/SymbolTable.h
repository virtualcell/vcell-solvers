#ifndef SYMBOLTABLE_H
#define SYMBOLTABLE_H

#include <string>
using std::string;

class SymbolTableEntry;

class SymbolTable {
public:
	virtual SymbolTableEntry* getEntry(string identifierString)=0; 
	virtual ~SymbolTable() {}
};

#endif
