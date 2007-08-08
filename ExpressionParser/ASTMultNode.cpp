#include "ASTMultNode.h"
#include "ASTInvertTermNode.h"
#include "ExpressionException.h"

ASTMultNode::ASTMultNode(int i) : SimpleNode(i) {
}

ASTMultNode::~ASTMultNode() {
}

bool ASTMultNode::isBoolean() {
	for (int i=0;i<jjtGetNumChildren();i++){
		if (!jjtGetChild(i)->isBoolean()) {
			return false;
		}
	}
	  return true;
}

string ASTMultNode::infixString(int lang, NameScope* nameScope)
{
	string buffer("(");

	for (int i=0;i<jjtGetNumChildren();i++){
		ASTInvertTermNode* pointer = dynamic_cast<ASTInvertTermNode*>(jjtGetChild(i));
		if (pointer){
			buffer += " / ";
			buffer += jjtGetChild(i)->infixString(lang,nameScope);
		}else{
			if (i>0) 
				buffer += " * ";
			buffer += jjtGetChild(i)->infixString(lang,nameScope);
		}
	}

	buffer += ")";
	return buffer;
}

void ASTMultNode::getStackElements(vector<StackElement>& elements) {
	for (int i=0;i<jjtGetNumChildren();i++){
		jjtGetChild(i)->getStackElements(elements);
		if (i > 0)
			elements.push_back(StackElement(TYPE_MULT));
	}
}

double ASTMultNode::evaluate(int evalType, double* values) {
	Exception* childException = 0;

	// evaluate boolean children first for conditional expressions. 
	// if any one of them is false, just return 0;
	// this protects against evaluating the terms outside of
	// domain defined by the boolean conditions.
	for (int i=0;i<jjtGetNumChildren();i++){
		if (jjtGetChild(i)->isBoolean()) {
			try {
				if (jjtGetChild(i)->evaluate(evalType, values) == 0) {
					return 0.0;
				}
			} catch (ExpressionException& e){
				if (evalType == EVALUATE_VECTOR) {
					throw e;
				}
				childException = new ExpressionException(e.getMessage());
			}		
		}
	}
	if (childException != null){
		throw (*childException);
	}	

	double product = 1.0;
	for (int i=0;i<jjtGetNumChildren();i++){
		try {
			double value = jjtGetChild(i)->evaluate(evalType, values);
			product *= value;
		}catch (ExpressionException& e){
			if (evalType == EVALUATE_VECTOR) {
				throw e;
			}
			childException = new ExpressionException(e.getMessage());
		}		
	}
	if (product == -0.0){
		return 0.0;
	}	
	if (childException != null){
		throw (*childException);
	}	
	return product;
}
