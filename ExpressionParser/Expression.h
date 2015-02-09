#ifndef VCELL_EXPRESSION_H
#define VCELL_EXPRESSION_H

#include "SimpleNode.h"

class SymbolTable;
class SymbolTableEntry;
class SimpleNode;
class StackMachine;

namespace VCell {

class Expression
{
public:
	Expression(void);
	Expression(string expString);
	Expression(Expression* expression);
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

	string getEvaluationSummary(double* values);

	SymbolTableEntry* getSymbolBinding(string symbol);
	double evaluateProxy();
	
	void showStackInstructions();
	void substituteInPlace(Expression* origExp, Expression* newExp);
	string infix_Visit(void);

private:
	SimpleNode  *rootNode;

	//static long flattenCount;
	//static long diffCount;
	//static long parseCount;
	//static long derivativeCount;
	//static long substituteCount;
	//static long bindCount;
	void parseExpression(string exp);
	StackMachine* stackMachine;
	inline StackMachine* getStackMachine();
};
}
#endif
