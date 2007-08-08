#ifndef ASTMINUSTERMNODE_H
#define ASTMINUSTERMNODE_H

#include "SimpleNode.h"

class ASTMinusTermNode : public SimpleNode
{
public:
	ASTMinusTermNode(int i);
	~ASTMinusTermNode();
	string infixString(int lang, NameScope* nameScope);
	void getStackElements(vector<StackElement>& elements);
	double evaluate(int evalType, double* values=0); 
};

#endif
