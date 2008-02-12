// OptSolverLibrary.cpp : Defines the entry point for the application.
//

#define __STDC__ 1

extern "C" {
#include "cfsqpusr.h"
}
#include "OptSolver.h"
#include "CFSQPSolver.h"
#include "Constraint.h"
#include "SimpleSymbolTable.h"
#include "OdeResultSet.h"
#include "MemoryManager.h"
#include "StoppedByUserException.h"

#include <float.h>
#include <iostream>
using namespace std;

//================================================
//
// class CFSQPOptSolver
//
//================================================
CFSQPOptSolver::CFSQPOptSolver(CFSQPOptJob *argOptJob) : OptSolver(argOptJob) {
	iprint = 0; 
	// 0: no display
	// 1: display objective and constraint values at initial feasible point and at end of execution.
}

CFSQPOptSolver::~CFSQPOptSolver(){
}

OptSolverResultSet* CFSQPOptSolver::solve(){
	MemoryManager::getInstance()->setMaxHeapSize(200 * 1000 * 1000);

	CFSQPOptJob* cfsqpJob = (CFSQPOptJob*)getOptJob();

	int mode=100; // computational mode: (CBA) C=1: last culprit checked first, B=0: FSQP-AL is selected, A=0: (P) is to be solved.
	int miter=5000;  // maximum iterations
	double bigbnd=1.e10;  // plays role of infinity
	double eps=1.e-8; // final norm requirement for the Newton direction, must be bigger than the machine precision (epsmac)
	double epsneq=0.e0; // maximum violation of nonlinear equality constraints allowed by the user at an optimal point. (> epsmac).
	double udelta=0.e0; // the perturbation size the user suggests to use in approximating gradients by finite difference.

	int nparam = cfsqpJob->getNumParameters(); // number of parameters
	int nf = 1; // number of sequentially dependent objective functions, always 1
	int nineqn = cfsqpJob->getNumNonlinearInequality();  // # of nonlinear inequality constraints
	int nineq = nineqn + cfsqpJob->getNumLinearInequality();  // total # of linear inequality constraints
	int neqn = cfsqpJob->getNumNonlinearEquality(); // # of nonlinear equality constraints
	int neq = neqn + cfsqpJob->getNumLinearEquality();  // total # of linear equality constraints

	int ncsrl=0; // number (possibly zero) of  sets of linear sequentially related constraints.
	int ncsrn=0; // Number (possibly zero) of sets of nonlinear sequentially related constraints.
	int nfsr=0; // number (possibly zero) of sets of sequentially related objective functions.
	int mesh_pts[] = { 0 }; // Pointer to array of dimension max(1,nfsr+ncsrn+ncsrl) indicating number of constraints/functions in each set.

	double *bl = new double[nparam]; // lower parameter bounds. to specify nonexistent lower bounds (bl[j] <= -bigbnd).
	double *bu = new double[nparam]; // upper parameter bounds. to specify nonexistent upper bounds (bu[j] >= bigbnd).
	double *x = new double[nparam]; // initial guess, also holds iterate at the end of execution.
	double *f = new double[nf]; // array holding evaluations of objective function(s) ... for sequentially related objectives.
	double *g = new double[nineq+neq]; // array holding valus of constraints at "x" at end of execution
	double *lambda = new double[nineq+neq+nf+nparam]; // values of the lagrange multipliers at "x" at the end of execution.
	   
	cfsqpJob->getLimits(bl,bu);
	cfsqpJob->getInitialGuess(x);

	int inform=-1; // return code from CFSQP()
	try {
		cfsqp(nparam,nf,nfsr,nineqn,nineq,neqn,neq,ncsrl,ncsrn,mesh_pts,
				mode,iprint,miter,&inform,bigbnd,eps,epsneq,udelta,bl,bu,x,
				f,g,lambda,obj_callback,constr_callback,gradob_callback,gradcn_callback,(void*)this);

		MemoryManager::deleteInstance();

		string statusMessage;
		switch (inform) {
			case NORMAL_TERMINATION: {
				statusMessage = "Normal termination: You have obtained a solution !!";
				break;
			}
			case NONFEASIBLE_LINEAR: {
				statusMessage = "initial guess is infeasible for linear constraints and unable to find feasible point.";
				break;
			}
			case NONFEASIBLE_NONLINEAR: {
				statusMessage = "initial guess is infeasible for nonlinear inequality constraints and linear constraints and unable to find feasible point.";
				break;
			}
			case NOSOLUTION_ITERATIONS: {
				statusMessage = "max iterations reached before finding solution";
				break;
			}
			case NOSOLUTION_MACHINE_PRECISION: {
				statusMessage = "line search fails to find new iterate (trial step size smaller than machine precision)";
				break;
			}
			case FAILED_CONSTRUCTING_D0: {
				statusMessage = "Failure of QP solver in attempting to constuct d0.  A more robust QP solver may succeed.";
				break;
			}
			case FAILED_CONSTRUCTING_D1: {
				statusMessage = "Failure of the QP solver in attempting to construct d1.  A more robust QP solver may succeed.";
				break;
			}
			case FAILED_INCONSISTENT_INPUT: {
				statusMessage = "Input data are not consistent (with printout indicating the error when iprint>0)";
				break;
			}
			case FAILED_ITERATES_STALLED: {
				statusMessage = "New iterate is numerically equivalent to the previous iterate, through stopping criterion is not yet satisfied.  Relaxing the stopping criterion should solve this problem.";
				break;
			}
			case FAILED_PENALTY_TOO_LARGE: {
				statusMessage = "One of the penalty parameters exceeded bigbnd.  The algorithm is having trouble satisfying a nonlinear equality constraint.";
				break;
			}
		}
		OptSolverResultSet* optSolverResultSet = new OptSolverResultSet();
		optSolverResultSet->nParams = cfsqpJob->getNumRealParameters();
		optSolverResultSet->status = inform;
		optSolverResultSet->params = new double[cfsqpJob->getNumRealParameters()];
		optSolverResultSet->statusMessage = statusMessage;
		memcpy(optSolverResultSet->params,cfsqpJob->getBestParameterValues(),cfsqpJob->getNumRealParameters()*sizeof(double));
		optSolverResultSet->objectiveFunctionValue = cfsqpJob->getBestObjectiveFunctionValue();
		optSolverResultSet->numObjFuncEvals = cfsqpJob->getNumObjFuncEvals();

		delete[] bl;
		delete[] bu;
		delete[] x;
		delete[] f;
		delete[] g;
		delete[] lambda;

		return optSolverResultSet;
	} catch (StoppedByUserException ex) {
		MemoryManager::deleteInstance();
		delete[] bl;
		delete[] bu;
		delete[] x;
		delete[] f;
		delete[] g;
		delete[] lambda;
		throw ex;
	} catch (Exception ex) {
		MemoryManager::deleteInstance();
		delete[] bl;
		delete[] bu;
		delete[] x;
		delete[] f;
		delete[] g;
		delete[] lambda;
		throw ex;
	} catch (char* errMsg) {
		MemoryManager::deleteInstance();
		delete[] bl;
		delete[] bu;
		delete[] x;
		delete[] f;
		delete[] g;
		delete[] lambda;
		throw errMsg;
	} catch (string& errMsg) {
		MemoryManager::deleteInstance();
		delete[] bl;
		delete[] bu;
		delete[] x;
		delete[] f;
		delete[] g;
		delete[] lambda;
		throw errMsg;
	} catch (...) {
		MemoryManager::deleteInstance();
		delete[] bl;
		delete[] bu;
		delete[] x;
		delete[] f;
		delete[] g;
		delete[] lambda;
		throw "Parameter estimation solver has caught unknown exception. Please contact the Virtual Cell.";
	}
}

void CFSQPOptSolver::stop(){
}

void CFSQPOptSolver::obj(int nparam, double *x, double *pobj){
	CFSQPOptJob* cfsqpJob = (CFSQPOptJob*)getOptJob();
	cfsqpJob->objective(nparam,x,pobj);	
	fireObjectiveFunctionEvalEvent(nparam, cfsqpJob->getBestParameterValues(), cfsqpJob->getBestObjectiveFunctionValue());
}

void CFSQPOptSolver::constr(int nparam, int j, double *x, double *pconstr){
	CFSQPOptJob* cfsqpJob = (CFSQPOptJob*)getOptJob();
	cfsqpJob->constraints(nparam,j,x,pconstr);	
}

void CFSQPOptSolver::gradob(int nparam, int j, double *x, double *gradfj, void (* dummy)(int,int,double*,double*,void*), void *cd){
	CFSQPOptJob* cfsqpJob = (CFSQPOptJob*)getOptJob();
	cfsqpJob->gradObjective(nparam,j,x,gradfj,dummy,cd);	
}

void CFSQPOptSolver::gradcn(int nparam, int j, double *x, double *gradgj, void (* dummy)(int,int,double*,double*,void*), void *cd){
	CFSQPOptJob* cfsqpJob = (CFSQPOptJob*)getOptJob();
	cfsqpJob->gradConstraint(nparam,j,x,gradgj,dummy,cd);	
}



void CFSQPOptSolver::obj_callback(int nparam, int j, double *x, double *fj, void *solverPointer){
	if (j!=1){
		throw ("more than one objective function not supported");
	}
	((CFSQPOptSolver*)solverPointer)->obj(nparam,x,fj);
}

void CFSQPOptSolver::constr_callback(int nparam, int j, double *x, double *gj, void *solverPointer){
	((CFSQPOptSolver*)solverPointer)->constr(nparam,j,x,gj);
}

void CFSQPOptSolver::gradob_callback(int nparam, int j, double *x, double *gradfj, void (* dummy)(int,int,double*,double*,void*), void *solverPointer){
	if (j!=1){
		throw ("more than one objective function not supported");
	}
	((CFSQPOptSolver*)solverPointer)->gradob(nparam,j,x,gradfj, dummy, solverPointer);
}

void CFSQPOptSolver::gradcn_callback(int nparam, int j, double *x, double *gradgj, void (* dummy)(int,int,double*,double*,void*), void *solverPointer){
	((CFSQPOptSolver*)solverPointer)->gradcn(nparam,j,x,gradgj, dummy, solverPointer);
}


//================================================
//
// class CFSQPOptJob
//
//================================================
CFSQPOptJob::CFSQPOptJob(int arg_numParameters, 
		char** arg_paramNames, double* arg_LB, double* arg_UB, double* arg_initialGuess, double* arg_scaleFactors,
		int arg_numNonLinearInequality, int arg_numLinearInequality, 
		int arg_numNonLinearEquality, int arg_numLinearEquality, char** constraintExpressions, 
		OdeResultSet* arg_referenceData, void (*checkStopRequested)(double, long)){
	numParameters = arg_numParameters;
	paramNames = arg_paramNames;
	LB = arg_LB;
	UB = arg_UB;
	initGuess = arg_initialGuess;

	numNonLinearInequality = arg_numNonLinearInequality;
	numLinearInequality = arg_numLinearInequality;
	numNonLinearEquality = arg_numNonLinearEquality;
	numLinearEquality = arg_numLinearEquality;

	referenceData = arg_referenceData;
	fn_checkStopRequested = checkStopRequested;	

	createConstraintList(constraintExpressions);

	numObjFuncEvals = 0;
	bestParamterValues = new double[numParameters];
	memset(bestParamterValues, 0, numParameters * sizeof(double));
	bestObjectiveFunctionValue = DBL_MAX;
	parameterSymbolTable = 0;
	testResultSet = 0;
	bestResultSet = new OdeResultSet();

	scaleFactors = arg_scaleFactors;
	unscaled_x = 0;

}

CFSQPOptJob::~CFSQPOptJob(){
	for (vector<Constraint*>::iterator iter = constraintList.begin(); iter < constraintList.end(); iter ++) {
		delete (*iter);
	}
	constraintList.clear();
	delete[] bestParamterValues;
	delete parameterSymbolTable;
	delete bestResultSet;
	delete[] unscaled_x;
}

void CFSQPOptJob::createConstraintList(char** constraintExpressions) {
	string* paramStringNames = new string[numParameters];
	for (int i = 0; i < numParameters; i ++) {	
		paramStringNames[i] = paramNames[i];
	}
	parameterSymbolTable = new SimpleSymbolTable(paramStringNames, numParameters);
	int count = 0;
	for (int i = 0; i < numNonLinearInequality; i ++) {
		Constraint* constraint = new Constraint(INEQUALITY_NONLINEAR, constraintExpressions[count ++]);
		constraint->bindExpression(parameterSymbolTable);
		constraintList.push_back(constraint);
	}
	for (int i = 0; i < numLinearInequality; i ++) {
		Constraint* constraint = new Constraint(INEQUALITY_LINEAR, constraintExpressions[count ++]);
		constraint->bindExpression(parameterSymbolTable);
		constraintList.push_back(constraint);
	}
	for (int i = 0; i < numNonLinearEquality; i ++) {
		Constraint* constraint = new Constraint(EQUALITY_NONLINEAR, constraintExpressions[count ++]);
		constraint->bindExpression(parameterSymbolTable);
		constraintList.push_back(constraint);
	}
	for (int i = 0; i < numLinearEquality; i ++) {
		Constraint* constraint = new Constraint(EQUALITY_LINEAR, constraintExpressions[count ++]);
		constraint->bindExpression(parameterSymbolTable);
		constraintList.push_back(constraint);
	}	
	delete[] paramStringNames;
}

void CFSQPOptJob::getLimits(double* bl, double* bu) {
	for (int i = 0; i < numParameters; i ++) {
		if (LB[i] > -DBL_MAX/2 && LB[i] < DBL_MAX) {
			bl[i] = LB[i] / scaleFactors[i];
		} else {
			bl[i] = LB[i];
		}
		if (UB[i] > -DBL_MAX/2 && UB[i] < DBL_MAX) {
			bu[i] = UB[i] / scaleFactors[i];
		} else {
			bu[i] = UB[i];
		}
	}
}

void CFSQPOptJob::getInitialGuess(double* x) {	
	scaleX(initGuess, x);
}

void CFSQPOptJob::scaleX(const double* unscaled_x, double* scaled_x) {
	for (int i = 0; i < numParameters; i ++) {
		scaled_x[i]  = unscaled_x[i] / scaleFactors[i];		
	}
}

void CFSQPOptJob::unscaleX(const double* scaled_x, double* unscaled_x) {
	for (int i = 0; i < numParameters; i ++) {
		unscaled_x[i] = scaled_x[i] * scaleFactors[i];
	}
}

void CFSQPOptJob::objective(int nparams, double* x, double* f) {
}

void CFSQPOptJob::constraints(int nparams, int j, double* x, double* gj) {
}

void CFSQPOptJob::gradObjective(int nparam, int j, double *x, double *gradfj, void (* dummy)(int,int,double*,double*,void*), void *cd){
	grobfd(nparam, j, x, gradfj, dummy, cd);
}

void CFSQPOptJob::gradConstraint(int nparam, int j, double *x, double *gradgj, void (* dummy)(int,int,double*,double*,void*), void *cd){
	grcnfd(nparam, j, x, gradgj, dummy, cd);
} 

OdeResultSet* CFSQPOptJob::getBestResultSet() {
	if (bestResultSet->getNumRows() == 0) {
		return testResultSet;
	}

	return bestResultSet;
}
