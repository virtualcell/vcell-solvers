#include "ASTOrNode.h"
#include "ExpressionException.h"
#include "ExpressionParserTreeConstants.h"

ASTOrNode::ASTOrNode() : SimpleNode(JJTORNODE) {
}

ASTOrNode::ASTOrNode(int i) : SimpleNode(i) {
}

ASTOrNode::~ASTOrNode() {
}

bool ASTOrNode::isBoolean() {
	return true;
}

string ASTOrNode::infixString(int lang, NameScope* nameScope)
{
	string buffer("(");

    for (int i = 0; i < jjtGetNumChildren(); i++) {
        if (i > 0)
            buffer += " || ";
        buffer += jjtGetChild(i)->infixString(lang, nameScope);
    }

    buffer += ")";
    return buffer;
}


void ASTOrNode::getStackElements(vector<StackElement>& elements) {
	for (int i=0;i<jjtGetNumChildren();i++){
		jjtGetChild(i)->getStackElements(elements);;
		if (i>0) 
			elements.push_back(StackElement(TYPE_OR));
	}
}

double ASTOrNode::evaluate(int evalType, double* values) {
	Exception* savedException = null;
	for (int i = 0; i < jjtGetNumChildren(); i++) {
		try {
			if (jjtGetChild(i)->evaluate(evalType, values) != 0) {
				return 1;
			}
		} catch (ExpressionException& e) {
			if (evalType == EVALUATE_VECTOR) {
				throw e;
			}
			savedException = new ExpressionException(e.getMessage());
		}
	}
	if (savedException != null) {
		throw (*savedException);
	} 
	return 0.0;
}

Node* ASTOrNode::copyTree() {
	ASTOrNode* node = new ASTOrNode();
	for (int i=0;i<jjtGetNumChildren();i++){
		node->jjtAddChild(jjtGetChild(i)->copyTree());
	}
	return node;	
}
