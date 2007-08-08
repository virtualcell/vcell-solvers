#include "ASTExpression.h"
#include "ExpressionException.h"

ASTExpression::ASTExpression(int i) : SimpleNode(i) {
}

ASTExpression::~ASTExpression() {
}

string ASTExpression::infixString(int lang, NameScope* nameScope)
{
   string buffer;

    for (int i = 0; i < jjtGetNumChildren(); i++) {
        buffer += jjtGetChild(i)->infixString(lang, nameScope);
    }
    return buffer;
}

void ASTExpression::getStackElements(vector<StackElement>& elements) {
    for (int i = 0; i < jjtGetNumChildren(); i++) {
		jjtGetChild(i)->getStackElements(elements);
    }
}

double ASTExpression::evaluate(int evalType, double* values) {
	return jjtGetChild(0)->evaluate(evalType, values);
}
