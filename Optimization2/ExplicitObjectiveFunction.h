#ifndef ExplicitObjectiveFunction_H
#define ExplicitObjectiveFunction_H

#include "ObjectiveFunction.h"

namespace VCell {
	class Expression;
}

class SymbolTable;
class ParameterDescription;

class ExplicitObjectiveFunction : public ObjectiveFunction {
public:
	ExplicitObjectiveFunction(VCell::Expression* objFuncExpression, ParameterDescription* parameterDescription, 
		SymbolTable* symbolTable, void (*checkStopRequested)(double, long));

	~ExplicitObjectiveFunction();

	virtual void objective(int nparams, double* parameterValues, double* functionValue);

	virtual int getNumObjFuncEvals();
	virtual double getBestObjectiveFunctionValue();
	virtual double* getBestParameterValues();
	void setCheckStopRequested(void (*checkStopRequested)(double, long));

protected:
	double* unscaled_x;
	ParameterDescription* parameterDescription;

	int numObjFuncEvals;
	VCell::Expression* objFuncExpression;
	double bestObjectiveFunctionValue;
	double* bestParameterValues;
	void (*fn_checkStopRequested)(double, long);
};

#endif
