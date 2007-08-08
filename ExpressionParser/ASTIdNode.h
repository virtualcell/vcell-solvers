#ifndef ASTIDNODE_H
#define ASTIDNODE_H

#include "SimpleNode.h"

class SymbolTableEntry;

class ASTIdNode : public SimpleNode
{
public:
	ASTIdNode(int i);
	~ASTIdNode();
	string name;
	string infixString(int lang, NameScope* nameScope);
	SymbolTableEntry* symbolTableEntry;
	SymbolTableEntry* getBinding(string symbol);
	void bind(SymbolTable* symbolTable);
	void getStackElements(vector<StackElement>& elements);
	double evaluate(int evalType, double* values=0); 
	void getSymbols(vector<string>& symbols, int language, NameScope* nameScope);
};

#endif
