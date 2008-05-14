#ifndef PDEOBJECTIVEFUNCTION_H
#define PDEOBJECTIVEFUNCTION_H

#include "ObjectiveFunction.h"

class SpatialReferenceData;
class ParameterDescription;
class FVSolver;

class PdeObjectiveFunction : public ObjectiveFunction {
public:
	PdeObjectiveFunction(ParameterDescription *parameterDescription,
		SpatialReferenceData* refData,
		char** refColumnMappingExpressions, 
		char* inputChars, 
		void (*checkStopRequested)(double, long));
	~PdeObjectiveFunction();

	virtual void objective(int nparams, double* parameterValus, double* functionValue);
	virtual int getNumObjFuncEvals();
	virtual double getBestObjectiveFunctionValue();
	virtual double* getBestParameterValues();

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
