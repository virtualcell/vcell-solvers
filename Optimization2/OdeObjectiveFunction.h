#ifndef ODEOBJECTIVEFUNCTION_H
#define ODEOBJECTIVEFUNCTION_H

#include <vector>
using namespace std;

#include "ObjectiveFunction.h"
#include "Weights.h"
#include "VariableWeights.h"
#include "TimeWeights.h"
#include "ElementWeights.h"

class OdeResultSetOpt;
class VCellSundialsSolver;
class ParameterDescription;

class OdeObjectiveFunction : public ObjectiveFunction {
public:
	OdeObjectiveFunction(ParameterDescription* parameterDescription, 
		OdeResultSetOpt* arg_referenceData, 
		vector<string>& refColumnMappingExpressions, 
		const char* arg_inputChars,
		void (*checkStopRequested)(double, long));

	~OdeObjectiveFunction();

	virtual void objective(int nparams, double* x, double* f);
	virtual int getNumObjFuncEvals();
	virtual double getBestObjectiveFunctionValue();
	virtual double* getBestParameterValues();
	OdeResultSetOpt* getBestResultSet();
	void setCheckStopRequested(void (*checkStopRequested)(double, long));

private:
	VCellSundialsSolver* sundialsSolver;

	double computeL2error(double* paramValues);

	double* unscaled_x;
	ParameterDescription* parameterDescription;

	OdeResultSetOpt* testResultSet;
	OdeResultSetOpt* referenceData;
	OdeResultSetOpt* bestResultSet;

	void (*fn_checkStopRequested)(double, long);

};

#endif
