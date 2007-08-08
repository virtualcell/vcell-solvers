#include "ASTMinusTermNode.h"
#include "ExpressionException.h"

ASTMinusTermNode::ASTMinusTermNode(int i) : SimpleNode(i) {
}

ASTMinusTermNode::~ASTMinusTermNode() {
}

string ASTMinusTermNode::infixString(int lang, NameScope* nameScope)
{
	string buffer(" - ");
	buffer += jjtGetChild(0)->infixString(lang,nameScope);
	return buffer;
}


void ASTMinusTermNode::getStackElements(vector<StackElement>& elements) {
	jjtGetChild(0)->getStackElements(elements);
	elements.push_back(StackElement(TYPE_SUB));
}

double ASTMinusTermNode::evaluate(int evalType, double* values) {
	return (- jjtGetChild(0)->evaluate(evalType, values));
}
