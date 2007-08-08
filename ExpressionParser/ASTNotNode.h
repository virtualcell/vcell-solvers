#ifndef ASTNOTNODE_H
#define ASTNOTNODE_H

#include "SimpleNode.h"

class ASTNotNode : public SimpleNode
{
public:
	ASTNotNode(int i);
	~ASTNotNode();
	string infixString(int lang, NameScope* nameScope);
	void getStackElements(vector<StackElement>& elements);
	double evaluate(int evalType, double* values=0); 
	bool isBoolean();
};

#endif
