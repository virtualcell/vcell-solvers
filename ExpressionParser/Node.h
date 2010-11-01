#ifndef NODE_H
#define NODE_H

#include <string>
#include <vector>
using std::string;
using std::vector;

#define LANGUAGE_DEFAULT  0
#define LANGUAGE_C 1
#define LANGUAGE_MATLAB  2
#define LANGUAGE_JSCL 3

#define EVALUATE_CONSTANT  111
#define EVALUATE_VECTOR 112
#define EVALUATE_PROXY  113

class NameScope;
class SymbolTableEntry;
struct StackElement;
class SymbolTable;

class Node
{
public:	
	virtual ~Node() { }
	/*
	virtual string code()=0;
	virtual Node copyTreeBinary()=0;
	virtual Node differentiate(string independentVariable)=0;
	
	virtual RealInterval evaluateInterval(RealInterval intervals[])=0;
	virtual  RealInterval getInterval(RealInterval intervals[]) throws ExpressionBindingException;	
	virtual String infixString(int lang, NameScope nameScope);
	*/
	virtual Node* copyTree()=0;
	virtual void jjtAddChild(Node* n)=0; 

	virtual bool isBoolean()=0;
	virtual void bind(SymbolTable* symbolTable)=0;
	virtual SymbolTableEntry* getBinding(string symbol)=0;
	virtual void getSymbols(vector<string>& symbols, int language, NameScope* nameScope)=0;  

	//virtual Node* flatten()=0;
	virtual string infixString(int lang, NameScope* nameScope)=0;  
	virtual void jjtAddChild(Node* n, int i)=0;  
	virtual void jjtClose()=0;  
	virtual Node* jjtGetChild(int i)=0;  
	virtual int jjtGetNumChildren()=0;
	virtual Node* jjtGetParent()=0;  
	virtual void jjtOpen()=0;  
	virtual void jjtSetParent(Node* n)=0;  

	virtual void getStackElements(vector<StackElement>& elements)=0;
	virtual double evaluate(int type, double* values=0)=0;	
	/*
	virtual bool narrow(RealInterval intervals[]) throws ExpressionBindingException;
	virtual void roundToFloat();
	virtual  void setInterval(RealInterval interval, RealInterval intervals[]) throws ExpressionBindingException;
	virtual void substituteBoundSymbols() throws ExpressionException;
	*/
	virtual bool equals(Node* node)=0;
	virtual void substitute(Node* origNode, Node* newNode)=0;
};

#endif
