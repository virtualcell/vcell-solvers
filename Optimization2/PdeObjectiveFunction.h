#ifndef PDEOBJECTIVEFUNCTION_H
#define PDEOBJECTIVEFUNCTION_H

#include "ObjectiveFunction.h"
#include <vector>
using namespace std;

class SpatialReferenceData;
class ParameterDescription;
class FVSolver;

class PdeObjectiveFunction : public ObjectiveFunction {
public:
	PdeObjectiveFunction(ParameterDescription *parameterDescription,
		SpatialReferenceData* refData,
		vector<string>& refColumnMappingExpressions, 
		const char* inputChars, 
		void (*checkStopRequested)(double, long));
	~PdeObjectiveFunction();

	virtual void objective(int nparams, double* parameterValus, double* functionValue);
	virtual int getNumObjFuncEvals();
	virtual double getBestObjectiveFunctionValue();
	virtual double* getBestParameterValues();
	void setCheckStopRequested(void (*checkStopRequested)(double, long));

private:
	//
	// need output time array support in FVSolver or at least integrate until t=T and continue.
	//
	double* unscaled_x;
	FVSolver* fvSolver;
	ParameterDescription* parameterDescription;
	SpatialReferenceData* referenceData;

	int numObjFuncEvals;
	double bestObjectiveFunctionValue;
	double* bestParameterValues;
	double** currentSolution;

	void (*fn_checkStopRequested)(double, long);
};

#endif
