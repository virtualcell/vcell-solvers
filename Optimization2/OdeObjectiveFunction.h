#ifndef OdeObjectiveFunction_H
#define OdeObjectiveFunction_H

#include <string>
#include <vector>
using namespace std;

#include "ObjectiveFunction.h"

class OdeResultSet;
class VCellSundialsSolver;
class ParameterDescription;

class OdeObjectiveFunction : public ObjectiveFunction {
public:
	OdeObjectiveFunction(ParameterDescription* parameterDescription, 
		OdeResultSet* arg_referenceData, 
		vector<string> refColumnMappingExpressions, 
		const char* arg_inputChars,
		void (*checkStopRequested)(double, long));

	~OdeObjectiveFunction();

	virtual void objective(int nparams, double* x, double* f);
	virtual int getNumObjFuncEvals();
	virtual double getBestObjectiveFunctionValue();
	virtual double* getBestParameterValues();
	OdeResultSet* OdeObjectiveFunction::getBestResultSet();

private:
	VCellSundialsSolver* sundialsSolver;

	double computeL2error(double* paramValues);

	double* unscaled_x;
	ParameterDescription* parameterDescription;

	OdeResultSet* testResultSet;
	OdeResultSet* referenceData;
	OdeResultSet* bestResultSet;

	int numObjFuncEvals;
	double bestObjectiveFunctionValue;
	double* bestParameterValues;
	void (*fn_checkStopRequested)(double, long);

};

#endif
