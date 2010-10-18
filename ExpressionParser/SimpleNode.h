#ifndef SIMPLENODE_H
#define SIMPLENODE_H

#include "Node.h"
#include "stdinc.h"

class ExpressionParser;
class NameScope;

class SimpleNode : public Node
{
public:
	SimpleNode(int i);
	virtual ~SimpleNode(void);

	void jjtOpen();
	void jjtClose();
	void jjtSetParent(Node* n);
	Node* jjtGetParent();
	void jjtAddChild(Node* n, int i);
	Node* jjtGetChild(int i);
	int jjtGetNumChildren();
	void dump(string prefix);
	virtual string infixString(int lang, NameScope* nameScope)=0;
	string toString(string prefix);
	virtual void getSymbols(vector<string>& symbols, int language, NameScope* nameScope);
	virtual SymbolTableEntry* getBinding(string symbol);
	virtual void bind(SymbolTable* symbolTable);
	static string getFunctionDomainError(string problem, double* values, string argumentName1, Node* node1, string argumentName2="", Node* node2=0);
	static string getNodeSummary(double* values, Node* node);
	virtual bool isBoolean();

	void jjtAddChild(Node* n);
	void substitute(Node* origNode, Node* newNode);
	virtual bool equals(Node* node);

protected:
	Node* parent;
	Node** children;
	int id;
	int numChildren;
};
#endif
