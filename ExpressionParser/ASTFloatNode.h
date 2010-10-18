#ifndef ASTFLOATNODE_H
#define ASTFLOATNODE_H

#include "SimpleNode.h"

class ASTFloatNode : public SimpleNode
{
public:
	ASTFloatNode(double i);
	ASTFloatNode(int i);
	~ASTFloatNode();
	double value;
	string infixString(int lang, NameScope* nameScope);
	void getStackElements(vector<StackElement>& elements);
	double evaluate(int evalType, double* values=0); 

	Node* copyTree();
	bool equals(Node* node);
};

#endif
