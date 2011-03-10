#ifndef EXPLICITFITOBJECTIVEFUNCTION_H
#define EXPLICITFITOBJECTIVEFUNCTION_H

#include <vector>
using namespace std;

#include "ObjectiveFunction.h"

class OdeResultSetOpt;
namespace VCell {
	class Expression;
}
class SymbolTable;
class ParameterDescription;

class ExplicitFitObjectiveFunction : public ObjectiveFunction {
public:
	ExplicitFitObjectiveFunction(vector<VCell::Expression*>& functionExpressions, int* funcDataColIdx,
		ParameterDescription* parameterDescription, 
		OdeResultSetOpt* arg_referenceData,
		void (*checkStopRequested)(double, long));

	~ExplicitFitObjectiveFunction();

	virtual void objective(int nparams, double* x, double* f);
	virtual int getNumObjFuncEvals();
	virtual double getBestObjectiveFunctionValue();
	virtual double* getBestParameterValues();
	void setCheckStopRequested(void (*checkStopRequested)(double, long));

private:
	double computeL2error(double* paramValues);

	double* unscaled_x;
	vector<VCell::Expression*> functionExpressions;//list of expression
	int* funcDataColIdx;//array of column index of reference data for the expression(at the same index in its list) to fit
	ParameterDescription* parameterDescription;
	SymbolTable* symbolTable;

	double* independentVarArray;
	double* dependentVarArray;
	double* evaluateArray;

	OdeResultSetOpt* referenceData;
	void (*fn_checkStopRequested)(double, long);

};

#endif
