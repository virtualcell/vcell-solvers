#ifndef ASTANDNODE_H
#define ASTANDNODE_H

#include "SimpleNode.h"

class ASTAndNode : public SimpleNode
{
public:
	ASTAndNode(int i);
	~ASTAndNode();
	string infixString(int lang, NameScope* nameScope);
	void getStackElements(vector<StackElement>& elements);
	double evaluate(int evalType, double* values=0); 
	bool isBoolean();

	Node* copyTree();

private:
	ASTAndNode();
};

#endif
