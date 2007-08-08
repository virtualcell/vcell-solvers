#include "SimpleSymbolTable.h"

SimpleSymbolTable::SimpleSymbolTable(string* symbols, int symbolCount, ValueProxy** valueProxies)
{
	for (int i = 0; i < symbolCount; i ++){
		if (valueProxies == NULL) {
			steArray.push_back(new SimpleSymbolTableEntry(symbols[i],i,0, NULL));
		} else {
			steArray.push_back(new SimpleSymbolTableEntry(symbols[i],i,0, valueProxies[i]));
		}
	}
}

SimpleSymbolTable::~SimpleSymbolTable(void)
{
	for (unsigned int i = 0; i < steArray.size(); i ++) {
		delete steArray[i];
		steArray[i] = 0;
	}
	steArray.clear();
}

SymbolTableEntry* SimpleSymbolTable::getEntry(string identifier)
{
	SymbolTableEntry* ste = getLocalEntry(identifier);
	if (ste!=null){
		return ste;
	}
	/*
	if (getNameScope() != null){
		return getNameScope().getExternalEntry(identifier);
	}
	*/
	return null;
}


SymbolTableEntry* SimpleSymbolTable::getLocalEntry(string identifier)
{
	for (unsigned int i = 0; i < steArray.size(); i++){
		if (steArray[i]->getName() == identifier){
			return steArray[i];
		}
	}
	return null;
}
