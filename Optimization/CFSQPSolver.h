#ifndef CFSQPOPTSOLVER_H
#define CFSQPOPTSOLVER_H

#include <string>
#include <vector>
using namespace std;

#include "OptSolver.h"

#define NORMAL_TERMINATION				 0
#define NONFEASIBLE_LINEAR				 1
#define NONFEASIBLE_NONLINEAR			 2
#define NOSOLUTION_ITERATIONS			 3
#define NOSOLUTION_MACHINE_PRECISION	 4
#define FAILED_CONSTRUCTING_D0			 5
#define FAILED_CONSTRUCTING_D1			 6
#define FAILED_INCONSISTENT_INPUT		 7
#define FAILED_ITERATES_STALLED			 8
#define FAILED_PENALTY_TOO_LARGE		 9
#define FAILED							 10
#define STOPPED_BY_USER					11


class OptStatusEvent;
class OptStatusListener;
class CFSQPOptJob;
class CFSQPOptSolver;
class ResultSet;
class Constraint;
class SymbolTable;
class OdeResultSet;

class CFSQPOptJob : public OptJob {
public:
	CFSQPOptJob(int arg_numParameters, 
		char** paramNames, double* arg_LB, double* arg_UB, double* arg_initialGuess, double* arg_scaleFactors,
		int arg_numNonLinearInequality, int arg_numLinearInequality, 
		int arg_numNonLinearEquality, int arg_numLinearEquality, char** constraintExpressions, 
		OdeResultSet* arg_referenceData, void (*checkStopRequested)(double, long));

	~CFSQPOptJob();

	virtual int getNumParameters() { return numParameters; }
	int getNumRealParameters() { return numParameters; }

	virtual int getNumNonlinearInequality() { return numNonLinearInequality; }
	virtual int getNumLinearInequality() { return numLinearInequality; }
	virtual int getNumNonlinearEquality() { return numNonLinearEquality; }
	virtual int getNumLinearEquality() { return numLinearEquality; }

	virtual void getLimits(double *lower, double *upper);
	virtual void getInitialGuess(double *x);

	virtual void objective(int nparams, double* x, double* f);
	virtual void constraints(int nparams, int j, double* x, double* gj);
	virtual void gradObjective(int nparam, int j, double *x, double *gradfj, void (* dummy)(int,int,double*,double*,void*), void *cd);
	virtual void gradConstraint(int nparam, int j, double *x, double *gradgj, void (* dummy)(int,int,double*,double*,void*), void *cd);

	virtual int getNumObjFuncEvals() { return numObjFuncEvals; }
	virtual double getBestObjectiveFunctionValue() { return bestObjectiveFunctionValue; }
	virtual double* getBestParameterValues() { return bestParamterValues; }

	virtual OdeResultSet* getResultSet() { return testResultSet; } 
	virtual OdeResultSet* getBestResultSet();

protected:
	int numObjFuncEvals;
	int numParameters;
	int numNonLinearInequality, numLinearInequality;
    int numNonLinearEquality, numLinearEquality;
	const double* LB, *UB, *initGuess;
	char** paramNames;

	double* scaleFactors;
	double* unscaled_x;

	SymbolTable* parameterSymbolTable;
	OdeResultSet* testResultSet;
	OdeResultSet* referenceData;
	OdeResultSet* bestResultSet;

	double bestObjectiveFunctionValue;
	double* bestParamterValues;
	void (*fn_checkStopRequested)(double, long);

	vector<Constraint*> constraintList;

	void createConstraintList(char** constraintExpressions);

	virtual void unscaleX(const double* scaled_x, double* unscaled_x);
	virtual void scaleX(const double* unscaled_x, double* scaled_x);
};

class CFSQPOptSolver : public OptSolver {
public:
	CFSQPOptSolver(CFSQPOptJob *optJob);
	~CFSQPOptSolver();

	OptSolverResultSet* solve();
	void stop();
	void setPrintMode(int newIprint) { iprint = newIprint; }


protected:
	void obj(int nparam, double *x, double *pobj);
	void constr(int nparam, int j, double *x, double *pconstr);
	void gradob(int nparam, int j, double *x, double *gradfj, void (* dummy)(int,int,double*,double*,void*), void *cd);
	void gradcn(int nparam, int j, double *x, double *gradgj, void (* dummy)(int,int,double*,double*,void*), void *cd);

private:
	int iprint;

	static void obj_callback(int nparam, int j, double *x, double *fj, void *solverPointer);
	static void constr_callback(int nparam, int j, double *x, double *gj, void *solverPointer);
	static void gradob_callback(int nparam, int j, double *x, double *gradfj, void (* dummy)(int,int,double*,double*,void*), void *solverPointer);
	static void gradcn_callback(int nparam, int j, double *x, double *gradgj, void (* dummy)(int,int,double*,double*,void*), void *solverPointer);
};

#endif
