#ifndef SIMPLENODE_CPP
#define SIMPLENODE_CPP

#include "SimpleNode.h"
#include "Expression.h"
#include "Exception.h"
#include "ExpressionException.h"

SimpleNode::SimpleNode(int i) {
	id = i;
	numChildren = 0;
	children = 0;
	parent = 0;
}

SimpleNode::~SimpleNode() {
	for (int i = 0; i < numChildren; i ++) {
		delete children[i];
		children[i] = 0;
	}
	delete[] children;
	children = 0;
}

void SimpleNode::jjtOpen() {
}

void SimpleNode::jjtClose() {
}

void SimpleNode::jjtSetParent(Node* n) { 
	parent = n; 
}

Node* SimpleNode::jjtGetParent() { 
	return parent; 
}

void SimpleNode::jjtAddChild(Node* n, int i) {
	if (numChildren == 0) {
		children = new Node*[i + 1];
		memset(children, 0, (i + 1) * sizeof(Node*));
		numChildren = i + 1;
	} else if (i >= numChildren) {
		Node** c = new Node*[i + 1];
		memset(children, 0, (i + 1) * sizeof(Node*));
		memcpy(c, children, numChildren * sizeof(Node*));
		delete[] children;	
		children = c;
		numChildren = i + 1;
	}
	children[i] = n;	
	((SimpleNode*)children[i])->parent = this;
}

Node* SimpleNode::jjtGetChild(int i) {
	return children[i];
}

int SimpleNode::jjtGetNumChildren() {
	return numChildren;
}

void SimpleNode::dump(string prefix) {
	cout << toString(prefix) << endl;
	if (children != 0) {
		for (int i = 0; i < numChildren; ++i) {
			SimpleNode* n = (SimpleNode*)children[i];
			if (n != 0) {
				n->dump(prefix + " ");
			}
		}
	}
}

string SimpleNode::toString(string prefix)
{
	return prefix + infixString(LANGUAGE_DEFAULT, 0); 
}

void SimpleNode::getSymbols(vector<string>& symbols, int language, NameScope* nameScope)
{
	for (int i=0;i<jjtGetNumChildren();i++){
		jjtGetChild(i)->getSymbols(symbols, language, nameScope);
	}	
}

SymbolTableEntry* SimpleNode::getBinding(string symbol)
{
	for (int i=0;i<jjtGetNumChildren();i++){
		SymbolTableEntry* ste = jjtGetChild(i)->getBinding(symbol);
		if (ste != null){
			return ste;
		}
	}		
	return null;
}

void SimpleNode::bind(SymbolTable* symbolTable)
{
	for (int i=0;i<jjtGetNumChildren();i++){
		jjtGetChild(i)->bind(symbolTable);
	}
}

bool SimpleNode::isBoolean() {
	return false;
}

string SimpleNode::getFunctionDomainError(string problem, double* values, string argumentName1, Node* node1, string argumentName2, Node* node2){
	string errorMsg = problem + ": " + argumentName1 + "=" + getNodeSummary(values, node1);
	if (node2 == 0) {
		return errorMsg;
	}
	errorMsg += "\n" + argumentName2 + "=" + getNodeSummary(values, node2);
	return errorMsg;
}

string SimpleNode::getNodeSummary(double* values, Node* node){
	string errorMsg;
    vector<string> symbols;
	node->getSymbols(symbols, LANGUAGE_DEFAULT, 0);
	if (symbols.size() > 0) {
        errorMsg += "\"" + node->infixString(LANGUAGE_DEFAULT, 0) + "\"\n  where:\n";
		SymbolTableEntry* symbolTableEntry = 0;
		for (unsigned int i = 0; i < symbols.size(); i++) {
			symbolTableEntry = node->getBinding(symbols.at(i));
			double value = 0.0;		
            try {				
				if (symbolTableEntry != 0) {				
					if (symbolTableEntry->getExpression() != null) {								
						if (values == 0) {
							value = symbolTableEntry->getExpression()->evaluateConstant();
						} else {
							value = symbolTableEntry->getExpression()->evaluateVector(values);
						}
						char chrs[1000];
						sprintf(chrs, "\t%s = %lf\n\0", symbolTableEntry->getName().c_str(), value);
						errorMsg += chrs;
					} else if (symbolTableEntry->getIndex() > -1) {
						if (values == 0) {
							value = symbolTableEntry->getConstantValue();
						} else {
							value = values[symbolTableEntry->getIndex()];
						}
						char chrs[1000];
						sprintf(chrs, "\t%s = %lf\n\0", symbolTableEntry->getName().c_str(), value);
						errorMsg += chrs;
					} else {
						errorMsg += "\t" + symbols[i] + " = <<<WRONG BINDING>>>\n";
					}
				} else {
					errorMsg += "\t" + symbols[i] + " = <<<UNBOUND IDENTIFIER>>>\n";
				}
            } catch (Exception& e) {
				throw ExpressionException(errorMsg + "\t" + symbols[i] + " = <<<ERROR>>> " + e.getMessage() + "\n");
            }
        }
	} else {
		errorMsg += node->infixString(LANGUAGE_DEFAULT, 0);
	}
	return errorMsg;
}
#endif