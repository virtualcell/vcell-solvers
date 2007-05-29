#ifndef ASTMULTNODE_H
#define ASTMULTNODE_H

#include "SimpleNode.h"

class ASTMultNode : public SimpleNode
{
public:
	ASTMultNode(int i);
	~ASTMultNode();
	string infixString(int lang, NameScope* nameScope);
	void getStackElements(vector<StackElement>& elements);
	double evaluate(int evalType, double* values=0); 
	bool isBoolean();
};

#endif