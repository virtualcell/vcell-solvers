// OptSolverLibrary.cpp : Defines the entry point for the application.
//

#define __STDC__ 1

extern "C" {
#include "cfsqpusr.h"
}
#include "OptResultSet.h"
#include "OptProblemDescription.h"
#include "ParameterDescription.h"
#include "ConstraintDescription.h"
#include "ObjectiveFunction.h"
#include "OptSolverCFSQP.h"
#include "Constraint.h"
#include "SimpleSymbolTable.h"
#include "OdeResultSet.h"
#include "MemoryManager.h"
#include "StoppedByUserException.h"

#include <float.h>
#include <iostream>
using namespace std;

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

OptSolverCFSQP::OptSolverCFSQP(OptProblemDescription *arg_optProblemDescription) : OptSolver2(arg_optProblemDescription) {
	iprint = 0; 
	// 0: no display
	// 1: display objective and constraint values at initial feasible point and at end of execution.
}

OptSolverCFSQP::~OptSolverCFSQP(){
}

OptResultSet* OptSolverCFSQP::solve(){
	MemoryManager::getInstance()->setMaxHeapSize(200 * 1000 * 1000);

	OptProblemDescription* optProb = getOptProblemDescription();

	int mode=100; // computational mode: (CBA) C=1: last culprit checked first, B=0: FSQP-AL is selected, A=0: (P) is to be solved.
	int miter=5000;  // maximum iterations
	double bigbnd=1.e10;  // plays role of infinity
	double eps=1.e-8; // final norm requirement for the Newton direction, must be bigger than the machine precision (epsmac)
	double epsneq=0.e0; // maximum violation of nonlinear equality constraints allowed by the user at an optimal point. (> epsmac).
	double udelta=0.e0; // the perturbation size the user suggests to use in approximating gradients by finite difference.

	int nparam = optProb->getParameterDescription()->getNumParameters(); // number of parameters
	int nf = 1; // number of sequentially dependent objective functions, always 1
	int nineqn = optProb->getConstraintDescription()->getNumNonlinearInequality();  // # of nonlinear inequality constraints
	int nineq = nineqn + optProb->getConstraintDescription()->getNumLinearInequality();  // total # of linear inequality constraints
	int neqn = optProb->getConstraintDescription()->getNumNonlinearEquality(); // # of nonlinear equality constraints
	int neq = neqn + optProb->getConstraintDescription()->getNumLinearEquality();  // total # of linear equality constraints

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
	   
	optProb->getParameterDescription()->getScaledLimits(bl,bu);
	optProb->getParameterDescription()->getScaledInitialGuess(x);

	int inform=-1; // return code from CFSQP()
	try {
		cfsqp(nparam,nf,nfsr,nineqn,nineq,neqn,neq,ncsrl,ncsrn,mesh_pts,
				mode,iprint,miter,&inform,bigbnd,eps,epsneq,udelta,bl,bu,x,
				f,g,lambda,obj_callback,constr_callback,gradob_callback,gradcn_callback,(void*)this);

		MemoryManager::deleteInstance();

		OptSolverStatus status = unknown;
		string statusMessage;
		switch (inform) {
			case NORMAL_TERMINATION: {
				status = normalTermination;
				statusMessage = "Normal termination: You have obtained a solution !!";
				break;
			}
			case NONFEASIBLE_LINEAR: {
				status = nonfeasibleLinear;
				statusMessage = "initial guess is infeasible for linear constraints and unable to find feasible point.";
				break;
			}
			case NONFEASIBLE_NONLINEAR: {
				status = nonfeasibleNonlinear;
				statusMessage = "initial guess is infeasible for nonlinear inequality constraints and linear constraints and unable to find feasible point.";
				break;
			}
			case NOSOLUTION_ITERATIONS: {
				status = noSolutionIterations;
				statusMessage = "max iterations reached before finding solution";
				break;
			}
			case NOSOLUTION_MACHINE_PRECISION: {
				status = noSolutionMachinePrecision;
				statusMessage = "line search fails to find new iterate (trial step size smaller than machine precision)";
				break;
			}
			case FAILED_CONSTRUCTING_D0: {
				status = failedConstructingD0;
				statusMessage = "Failure of QP solver in attempting to constuct d0.  A more robust QP solver may succeed.";
				break;
			}
			case FAILED_CONSTRUCTING_D1: {
				status = failedConstructingD1;
				statusMessage = "Failure of the QP solver in attempting to construct d1.  A more robust QP solver may succeed.";
				break;
			}
			case FAILED_INCONSISTENT_INPUT: {
				statusMessage = "Input data are not consistent (with printout indicating the error when iprint>0)";
				break;
			}
			case FAILED_ITERATES_STALLED: {
				status = failedIteratesStalled;
				statusMessage = "New iterate is numerically equivalent to the previous iterate, though stopping criterion is not yet satisfied.  Relaxing the stopping criterion should solve this problem.";
				break;
			}
			case FAILED_PENALTY_TOO_LARGE: {
				status = failedPenaltyTooLarge;
				statusMessage = "One of the penalty parameters exceeded bigbnd.  The algorithm is having trouble satisfying a nonlinear equality constraint.";
				break;
			}
		}
		OptResultSet* optResultSet = new OptResultSet();
		int numParameters = optProb->getParameterDescription()->getNumParameters();
		optResultSet->status = status;
		double* bestParameterValues = optProb->getObjectiveFunction()->getBestParameterValues();
		for (int i=0;i<numParameters; i++){
			optResultSet->paramNames.push_back(optProb->getParameterDescription()->getParameterName(i));
			optResultSet->paramValues.push_back(bestParameterValues[i]);
		}
		optResultSet->statusMessage = statusMessage;
		optResultSet->objectiveFunctionValue = optProb->getObjectiveFunction()->getBestObjectiveFunctionValue();
		optResultSet->numObjFuncEvals = optProb->getObjectiveFunction()->getNumObjFuncEvals();

		delete[] bl;
		delete[] bu;
		delete[] x;
		delete[] f;
		delete[] g;
		delete[] lambda;

		return optResultSet;
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
	} catch (const char* errMsg) {
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

void OptSolverCFSQP::stop(){
}

void OptSolverCFSQP::setCheckStopRequested(void (*checkStopRequested)(double, long)) {
	getOptProblemDescription()->getObjectiveFunction()->setCheckStopRequested(checkStopRequested);
}

void OptSolverCFSQP::obj(int nparam, double *x, double *pobj){
	ObjectiveFunction* objFunc = getOptProblemDescription()->getObjectiveFunction();
	objFunc->objective(nparam,x,pobj);	
	fireObjectiveFunctionEvalEvent(nparam, objFunc->getBestParameterValues(), objFunc->getBestObjectiveFunctionValue());
}

void OptSolverCFSQP::constr(int nparam, int j, double *x, double *pconstr){
	getOptProblemDescription()->getConstraintDescription()->constraints(nparam,j,x,pconstr);	
}

void OptSolverCFSQP::gradob(int nparam, int j, double *x, double *gradfj, void (* dummy)(int,int,double*,double*,void*), void *cd){
	ObjectiveFunction* objectiveFun = getOptProblemDescription()->getObjectiveFunction();
	if (objectiveFun->hasGradObjective()) {
		objectiveFun->gradObjective(nparam, j, x, gradfj, dummy, cd);
	} else {
		grobfd(nparam, j, x, gradfj, dummy, cd);
	}
}

void OptSolverCFSQP::gradcn(int nparam, int j, double *x, double *gradgj, void (* dummy)(int,int,double*,double*,void*), void *cd){
	ConstraintDescription* constraintDesc = getOptProblemDescription()->getConstraintDescription();
	if (constraintDesc->hasGradConstraint()) {
		constraintDesc->gradConstraint(nparam, j, x, gradgj, dummy, cd);
	} else {
		grcnfd(nparam, j, x, gradgj, dummy, cd);
	}	
}

void OptSolverCFSQP::obj_callback(int nparam, int j, double *x, double *fj, void *solverPointer){
	if (j!=1){
		throw ("more than one objective function not supported");
	}
	((OptSolverCFSQP*)solverPointer)->obj(nparam,x,fj);
}

void OptSolverCFSQP::constr_callback(int nparam, int j, double *x, double *gj, void *solverPointer){
	((OptSolverCFSQP*)solverPointer)->constr(nparam,j,x,gj);
}

void OptSolverCFSQP::gradob_callback(int nparam, int j, double *x, double *gradfj, void (* dummy)(int,int,double*,double*,void*), void *solverPointer){
	if (j!=1){
		throw ("more than one objective function not supported");
	}
	((OptSolverCFSQP*)solverPointer)->gradob(nparam,j,x,gradfj, dummy, solverPointer);
}

void OptSolverCFSQP::gradcn_callback(int nparam, int j, double *x, double *gradgj, void (* dummy)(int,int,double*,double*,void*), void *solverPointer){
	((OptSolverCFSQP*)solverPointer)->gradcn(nparam,j,x,gradgj, dummy, solverPointer);
}
