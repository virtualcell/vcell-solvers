#ifndef ASTORNODE_H
#define ASTORNODE_H

#include "SimpleNode.h"

class ASTOrNode : public SimpleNode
{
public:
	ASTOrNode(int i);
	~ASTOrNode();
	string infixString(int lang, NameScope* nameScope);
	void getStackElements(vector<StackElement>& elements);
	double evaluate(int evalType, double* values=0); 
	bool isBoolean();
};

#endif
