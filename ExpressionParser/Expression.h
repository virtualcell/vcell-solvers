#ifndef EXPRESSION_H
#define EXPRESSION_H

#include "SimpleNode.h"

class Expression
{
public:
	Expression(void);
	Expression(string expString);
	~Expression(void);
	double evaluateConstant(void);
	// exercise the old way of evaluating constant and vector by traversing abstract syntax tree
	double evaluateConstantTree();
	double evaluateVectorTree(double* values);
	// exercise the new way of evaluating vector by using stack machine
	double evaluateVector(double* values);

	string infix(void);
	void bindExpression(SymbolTable* symbolTable);
	static string trim(string str);
	void getSymbols(vector<string>& symbols); 

	SymbolTableEntry* getSymbolBinding(string symbol);
	double evaluateProxy();
	
	void showStackInstructions();

private:
	SimpleNode  *rootNode, *pRootNode;

	static long flattenCount;
	static long diffCount;
	static long parseCount;
	static long derivativeCount;
	static long substituteCount;
	static long bindCount;
	void parseExpression(string exp);
	StackMachine* stackMachine;
	inline StackMachine* getStackMachine();
};

#endif
