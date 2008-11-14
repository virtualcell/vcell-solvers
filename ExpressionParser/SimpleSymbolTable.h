#ifndef SIMPLESYMBOLTABLE_H
#define SIMPLESYMBOLTABLE_H

#include "SymbolTable.h"
#include "SimpleSymbolTableEntry.h"

class SimpleSymbolTable : public SymbolTable
{
public:
	SimpleSymbolTable(string* symbols, int symbolCount, ValueProxy** valueProxies=0);
	//SimpleSymbolTable(string* symbols, NameScope* namescopeVal);
	~SimpleSymbolTable(void);
	SymbolTableEntry* getEntry(string identifier);
	SymbolTableEntry* getLocalEntry(string identifier);

private:
	vector<SimpleSymbolTableEntry*> steArray;
};

#endif
