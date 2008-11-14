#ifndef ASTEXPRESSION_H
#define ASTEXPRESSION_H

#include "SimpleNode.h"

class ASTExpression : public SimpleNode
{
public:
	ASTExpression(int i);
	~ASTExpression();
	string infixString(int lang, NameScope* nameScope);
	void getStackElements(vector<StackElement>& elements);
	double evaluate(int evalType, double* values=0); 
};

#endif
