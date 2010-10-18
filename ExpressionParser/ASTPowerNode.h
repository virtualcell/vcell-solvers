#ifndef ASTPOWERNODE_H
#define ASTPOWERNODE_H

#include "SimpleNode.h"

class ASTPowerNode : public SimpleNode
{
public:
	ASTPowerNode(int i);
	~ASTPowerNode();
	string infixString(int lang, NameScope* nameScope);
	void getStackElements(vector<StackElement>& elements);
	double evaluate(int evalType, double* values=0); 

	Node* copyTree();

private:
	ASTPowerNode();
};

#endif
