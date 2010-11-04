#ifndef OBJECTIVEFUNCTION_H
#define OBJECTIVEFUNCTION_H

#include <float.h>

class ObjectiveFunction {
public:
	virtual void objective(int nparams, double* parameterValus, double* functionValue)=0;

	virtual void gradObjective(int nparam, int j, double *x, double *gradfj, void (* dummy)(int,int,double*,double*,void*), void *cd) {}
	virtual bool hasGradObjective() { return false; }

	virtual int getNumObjFuncEvals() = 0;
	virtual double getBestObjectiveFunctionValue() = 0;
	virtual double* getBestParameterValues() = 0;
	virtual void setCheckStopRequested(void (*checkStopRequested)(double, long)) = 0;
	void reset() {
		numObjFuncEvals = 0;
		bestObjectiveFunctionValue = DBL_MAX;
	}
	int getNumObjFunEvals() {
		return numObjFuncEvals;
	}

protected:
	ObjectiveFunction() {
		numObjFuncEvals = 0;
		bestObjectiveFunctionValue = DBL_MAX;
		bestParameterValues = 0;		
	}
	int numObjFuncEvals;
	double bestObjectiveFunctionValue;
	double* bestParameterValues;
};


#endif
