#ifndef ODEMULTISHOOTINGOPTJOB_H
#define ODEMULTISHOOTINGOPTJOB_H

#include "Constraint.h"
#include "CFSQPSolver.h"

#include <vector>
using namespace std;

class OdeResultSet;
class VCellCVodeSolver;
class SymbolTable;

/* 
 * for parameter vector
 *		0 ~ NPARAM-1, regular paramters;
 *		NPARAM ~ NEQ * NTIMEPOINTS - 1, state variables for all the times such that x[i][j]
 *         p0, p1, ..., p[NPARAM-1], A0, B0, ..., A1, B1, ..., ..., A[NTIMEPOINTS-1], B[NTIMEPOINTS-1]
 * 
 * for constraints vector
 *      NonLinearInequality, 
 *		LinearInequality, 
 *		NonLinearEquality,
 *		constraint_A1(A1 - A0 - (RHS_A0(t0,A0,B0, p) + RHS_A1(t1,A1,B1, p))/2 * deltaT), 
 *		constraint_B1(B1 - B0 - (RHS_B0(t0,A0,B0, p) + RHS_B1(t1,A1,B1, p))/2 * deltaT),
 *		constraint_A2(A2 - A1 - (RHS_A1(t1,A1,B1, p) + RHS_A2(t2,A2,B2, p))/2 * deltaT),
 *		constraint_B2(B2 - B1 - (RHS_B1(t1,A1,B1, p) + RHS_B2(t2,A2,B2, p))/2 * deltaT),
 *		...
 *		LinearEquality
 *
 * for evaluating RHS
 *		0: t, 
 *		1 ~ NEQ: A, B, C, ..., variable values at t
 *      NEQ+1 ~ NEQ+NPARAM, p0, p1, p2, parameter values
*/			

class OdeMultiShootingOptJob : public CFSQPOptJob {
public:
	OdeMultiShootingOptJob(int arg_numParameters, 
		char** paramNames, double* arg_LB, double* arg_UB, double* arg_initialGuess, double* arg_scaleFactors,
		int arg_numNonLinearInequality, int arg_numLinearInequality, 
		int arg_numNonLinearEquality, int arg_numLinearEquality, 
		char** constraintExpressions, OdeResultSet* arg_referenceData, char** refColumnMappingExpressions, 
		char* arg_inputChars, double arg_maxTimeStep, void (*checkStopRequested)(double, long));
	~OdeMultiShootingOptJob();

	virtual int getNumParameters();

	virtual int getNumNonlinearEquality();

	virtual void getLimits(double *lower, double *upper);
	virtual void getInitialGuess(double *x);

	virtual void objective(int nparams, double* x, double* f);
	virtual void constraints(int nparams, int j, double* x, double* gj);
	virtual void gradObjective(int nparam, int j, double *x, double *gradfj, void (* dummy)(int,int,double*,double*,void*), void *cd);

	static void computeTimePoints(vector<double>& timePoints, double* refTimes, int numRefTimes, double maxTimeStep);

protected:
	virtual void unscaleX(const double* scaled_x, double* unscaled_x);
	virtual void scaleX(const double* unscaled_x, double* scaled_x);

private:
	int numVariables;
	// 0 : t
	// 1 ~ N : variable values
	// N+1 ~ N+NPARAM : parameter values 
	double* allValues;

	double maxTimeStep;
	vector<double> timePoints;
	char* inputSting;
	VCellCVodeSolver* cvodeSolver;
	
	int* gradObjectiveMask;
	double* variableScales;

	void computeTimePoints();
	double computeL2error(double* paramValues);
	void computeGradObjectiveMask();
	void createConstraintList(const char* const* constraintExpressions, const char* const* paramNames);
};

#endif
