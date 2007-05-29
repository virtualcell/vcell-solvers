#ifndef ASTRELATIONALNODE_H
#define ASTRELATIONALNODE_H

#include "SimpleNode.h"

class ASTRelationalNode : public SimpleNode
{
public:
	ASTRelationalNode(int i);
	~ASTRelationalNode();
	void setOperationFromToken(string op);
	string infixString(int lang, NameScope* nameScope);
	void getStackElements(vector<StackElement>& elements);
	double evaluate(int evalType, double* values=0); 
	bool isBoolean();

private:
	int operation;
	string opString;
};

#endif