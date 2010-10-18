#ifndef ASTFUNCNODE_H
#define ASTFUNCNODE_H

#include "SimpleNode.h"

class ASTFuncNode : public SimpleNode
{
public:
	ASTFuncNode(int i);
	~ASTFuncNode();
	void setFunctionFromParserToken(string parserToken);
	string infixString(int lang, NameScope* nameScope);
	void getStackElements(vector<StackElement>& elements);
	double evaluate(int evalType, double* values=0); 

	Node* copyTree();
	bool equals(Node* node);

private:
	int funcType;
	string funcName;

	ASTFuncNode();
};

#endif
