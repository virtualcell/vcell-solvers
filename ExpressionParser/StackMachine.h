#ifndef STACKMACHINE_H
#define STACKMACHINE_H

enum STACK_ELEMENT_TYPE {TYPE_LT = 1, TYPE_GT, TYPE_LE, TYPE_GE, TYPE_EQ, TYPE_NE, 
	TYPE_AND, TYPE_OR, TYPE_NOT, TYPE_ADD, TYPE_SUB, TYPE_MULT, 
	TYPE_DIV, TYPE_FLOAT, TYPE_IDENTIFIER, TYPE_EXP, TYPE_SQRT, TYPE_ABS, TYPE_POW, 
	TYPE_LOG, TYPE_SIN, TYPE_COS, TYPE_TAN, TYPE_ASIN, TYPE_ACOS, TYPE_ATAN, 
	TYPE_ATAN2, TYPE_MAX, TYPE_MIN, TYPE_CEIL, TYPE_FLOOR, TYPE_CSC, TYPE_COT, 
	TYPE_SEC, TYPE_ACSC, TYPE_ACOT, TYPE_ASEC, TYPE_SINH, TYPE_COSH, TYPE_TANH, 
	TYPE_CSCH, TYPE_COTH, TYPE_SECH, TYPE_ASINH, TYPE_ACOSH, TYPE_ATANH, TYPE_ACSCH, 
	TYPE_ACOTH, TYPE_ASECH, TYPE_FACTORIAL};	// totally 51

class ValueProxy;

struct StackElement {
    int type;
	double value;
	int vectorIndex;
	ValueProxy* valueProxy;

	StackElement() {
		type = 0;
		value = 0;
		valueProxy = 0;
	}

	StackElement(int arg_type) {
		type = arg_type;
		value = 0.0;
		valueProxy = 0;
	}

	StackElement(ValueProxy* arg_vp, int index) {
		type = TYPE_IDENTIFIER;
		value = 0.0;
		valueProxy = arg_vp;
		vectorIndex = index;
	}
	StackElement(double arg_value) {
		type = TYPE_FLOAT;
		value = arg_value;
		valueProxy = 0;
	}
};

class StackMachine {
private:
	//double *workingStack;
	StackElement* elements;
	int elementSize;

public:
	StackMachine(StackElement* arg_elements, int size);
	~StackMachine();
	double evaluate(double* values=0);	
};

	
#endif