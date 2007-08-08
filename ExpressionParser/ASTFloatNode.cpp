#include "ASTFloatNode.h"
#include "RuntimeException.h"
#include "ExpressionParserTreeConstants.h"

ASTFloatNode::ASTFloatNode(double doubleValue) : SimpleNode(JJTFLOATNODE) {
	// is not a number
	if (doubleValue != doubleValue){
		throw RuntimeException("cannot set float node to NaN");
	}
	value = doubleValue;
}

ASTFloatNode::ASTFloatNode(int i) : SimpleNode(i) , value(0)
{
}

ASTFloatNode::~ASTFloatNode() {
}

string ASTFloatNode::infixString(int lang, NameScope* nameScope)
{
	//if (value == null) {
    //    return string("null");
    //} else 
	if (value == 0.0) {
        return string("0.0");
    } else {
		char s[256];		
		sprintf(s, "%.20lg\0", value);
        return string(s);
    }
}

void ASTFloatNode::getStackElements(vector<StackElement>& elements) {
	elements.push_back(StackElement(value));
}

double ASTFloatNode::evaluate(int evalType, double* values) {
	return value;
}
