#ifndef ODEOPTJOB_H
#define ODEOPTJOB_H

#include "Constraint.h"
#include "CFSQPSolver.h"

#include <vector>
using namespace std;

class OdeResultSet;
class VCellIDASolver;
class SymbolTable;

class OdeOptJob : public CFSQPOptJob {
public:
	OdeOptJob(int arg_numParameters, 
		char** paramNames, double* arg_LB, double* arg_UB, double* arg_initialGuess, double* arg_scaleFactors,
		int arg_numNonLinearInequality, int arg_numLinearInequality, 
		int arg_numNonLinearEquality, int arg_numLinearEquality, 
		char** constraintExpressions, OdeResultSet* arg_referenceData, 
		char** refColumnMappingExpressions, char* arg_inputChars, void (*checkStopRequested)(double, long));
	~OdeOptJob();


	virtual void objective(int nparams, double* x, double* f);
	virtual void constraints(int nparams, int j, double* x, double* gj);	

private:
	char* inputSting;
	VCellIDASolver* idaSolver;

	void createConstraintList(const char* const* constraintExpressions, const char* const* paramNames);
	double computeL2error(double* paramValues);
};

#endif
