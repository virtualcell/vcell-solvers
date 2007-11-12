#include "Expression.h"
#include "ExpressionParser.h"
#include "ASTFloatNode.h"
#include "ParseException.h"
#include "ParserException.h"

long Expression::flattenCount = 0;
long Expression::diffCount = 0;
long Expression::parseCount = 0;
long Expression::derivativeCount = 0;
long Expression::substituteCount = 0;
long Expression::bindCount = 0;

Expression::Expression(void)
{
	rootNode = null;
	parser = null;
	stackMachine = null;
}

Expression::~Expression(void)
{
	delete rootNode;
	delete parser;
	delete stackMachine;
}

Expression::Expression(string expString)
{
	rootNode = 0;
	parser = 0;
	stackMachine = null;

	if (expString.length() == 0) {
		throw ParserException("Empty expression");
	}

	bool bNumber = true;
	for (unsigned int i = 0; i < expString.length(); i ++) {
		if (!isdigit(expString[i]) && expString[i] != '.' && expString[i] != ';') {
			bNumber = false;
			break;
		}
	}
	if (bNumber) {
		double value = 0.0;
		int n = sscanf(expString.c_str(), "%lf", &value); 
		if (n == 1) {
			rootNode = new ASTFloatNode(value);
			return;
		}				
	} 

	string trimstr = trim(expString);
	if (trimstr[trimstr.length() - 1] != ';'){
		trimstr += ";";
	}
	parseExpression(trimstr);
}

void Expression::showStackInstructions(void)
{
	getStackMachine()->showInstructions();
	cout.flush();
}

double Expression::evaluateConstant(void)
{
	return getStackMachine()->evaluate(null);
}

double Expression::evaluateConstantTree()
{
	return rootNode->evaluate(EVALUATE_CONSTANT);
}

double Expression::evaluateVectorTree(double* values)
{
	try {
		return rootNode->evaluate(EVALUATE_VECTOR, values);
	} catch (Exception& ex) {
		Exception::rethrowException(ex, ex.getMessage() + " in " + rootNode->getNodeSummary(values, rootNode));
	}
}

double Expression::evaluateVector(double* values)
{
	try {
		return getStackMachine()->evaluate(values);
	} catch (Exception& ex) {
		Exception::rethrowException(ex, ex.getMessage()+ " in " + rootNode->getNodeSummary(values, rootNode));
	}
}

void Expression::parseExpression(string exp)
{
	parseCount++;
	try {	

		istringstream* iss = new istringstream(exp);
		parser = new ExpressionParser(iss);
		rootNode = parser->Expression();

		if (typeid(*rootNode) == typeid(ASTExpression)){
			if (rootNode->jjtGetNumChildren() == 1){
				rootNode = (SimpleNode*)rootNode->jjtGetChild(0);
				rootNode->jjtSetParent(null);
			}
		}	
		delete iss;
	} catch (Exception& e) {
		throw ParserException("Parse Error while parsing expression " + e.getMessage());
	}
}

string Expression::infix(void)
{
	return rootNode->infixString(LANGUAGE_DEFAULT, 0);
}

void Expression::bindExpression(SymbolTable* symbolTable)
{	
	bindCount++;
	rootNode->bind(symbolTable);
}

string Expression::trim(string str)
{
	int len = str.length();
	int st = 0;
	int off = 0; 
	const char* val = str.c_str();

	while ((st < len) && (val[st] <= ' ')) {
	    st++;
	}
	while ((st < len) && (val[len - 1] <= ' ')) {
	    len--;
	}
	return ((st > 0) || (len < str.length())) ?  str.substr(st, len-st) : str;
}

inline StackMachine* Expression::getStackMachine() {
	if (stackMachine == null) {
		vector<StackElement> elements_vector;
		rootNode->getStackElements(elements_vector);
		StackElement* elements = new StackElement[elements_vector.size()];
		int i = 0;
		for (vector<StackElement>::iterator iter = elements_vector.begin(); iter != elements_vector.end(); iter ++) {
			elements[i ++] = *iter;
		}
		stackMachine = new StackMachine(elements, elements_vector.size());
	}
	return stackMachine;
}

void Expression::getSymbols(vector<string>& symbols) {
	rootNode->getSymbols(symbols, LANGUAGE_DEFAULT, 0);
}

SymbolTableEntry* Expression::getSymbolBinding(string symbol){
	return rootNode->getBinding(symbol);
}

double Expression::evaluateProxy() {
	return evaluateVector(0);
}
