// OptSolverLibrary.cpp : Defines the entry point for the application.
//
#ifndef __STDC__
#define __STDC__ 1
#endif

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

static const int mode=100; // computational mode: (CBA) C=1: last culprit checked first, B=0: FSQP-AL is selected, A=0: (P) is to be solved.
static const int miter=5000;  // maximum iterations
static const double bigbnd=1.e10;  // plays role of infinity
static const double eps=1.e-8; // final norm requirement for the Newton direction, must be bigger than the machine precision (epsmac)
static const double epsneq=0.e0; // maximum violation of nonlinear equality constraints allowed by the user at an optimal point. (> epsmac).
static const double udelta=0.e0; // the perturbation size the user suggests to use in approximating gradients by finite difference.

OptSolverCFSQP::OptSolverCFSQP(OptProblemDescription *arg_optProblemDescription) : OptSolver2(arg_optProblemDescription) {
	iprint = 0;
	// 0: no display
	// 1: display objective and constraint values at initial feasible point and at end of execution.
	profileDist_obj_x = 0;
}

OptSolverCFSQP::~OptSolverCFSQP(){
	delete[] profileDist_obj_x;
}

OptResultSet* OptSolverCFSQP::solve(){
	MemoryManager::getInstance()->setMaxHeapSize(200 * 1000 * 1000);

	OptProblemDescription* optProb = getOptProblemDescription();

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

	clearFixedParameter();

	OptResultSet* optResultSet = NULL;
	int returnCode = 0;
	string returnErrMsg;
	int inform=-1; // return code from CFSQP()
	try {
		cfsqp(nparam,nf,nfsr,nineqn,nineq,neqn,neq,ncsrl,ncsrn,mesh_pts,
				mode,iprint,miter,&inform,bigbnd,eps,epsneq,udelta,bl,bu,x,
				f,g,lambda,obj_callback,constr_callback,gradob_callback,gradcn_callback,(void*)this);

		OptSolverStatus status = unknown;
		string statusMessage;
		computeCFSQPStatus(inform, status, statusMessage);

		optResultSet = new OptResultSet();
		int numParameters = optProb->getParameterDescription()->getNumParameters();
		optResultSet->bestRunResultSet.status = status;
		double* bestParameterValues = optProb->getObjectiveFunction()->getBestParameterValues();
		for (int i=0;i<numParameters; i++){
			optResultSet->paramNames.push_back(optProb->getParameterDescription()->getParameterName(i));
			optResultSet->bestRunResultSet.paramValues.push_back(bestParameterValues[i]);
		}
		optResultSet->bestRunResultSet.statusMessage = statusMessage;
		optResultSet->bestRunResultSet.objectiveFunctionValue = optProb->getObjectiveFunction()->getBestObjectiveFunctionValue();
		optResultSet->bestRunResultSet.numObjFuncEvals = optProb->getObjectiveFunction()->getNumObjFuncEvals();

		if (optProb->getComputeProfileDistributions() && nparam > 1) {
			computeProfileDistributions(optResultSet);
		}
		returnCode = 0;
	} catch (StoppedByUserException& ex) {
		returnCode = 1;
		returnErrMsg = ex.getMessage();
	} catch (VCell::Exception& ex) {
		returnCode = 2;
		returnErrMsg = ex.getMessage();
	} catch (const char* errMsg) {
		returnCode = 2;
		returnErrMsg = errMsg;
	} catch (string& errMsg) {
		returnCode = 2;
		returnErrMsg = errMsg;
	} catch (...) {
		returnCode = 2;
		returnErrMsg = "Parameter estimation solver has caught unknown exception. Please contact the Virtual Cell.";
	}
	MemoryManager::deleteInstance();
	delete[] bl;
	delete[] bu;
	delete[] x;
	delete[] f;
	delete[] g;
	delete[] lambda;

	if (returnCode == 1) {
		throw StoppedByUserException(returnErrMsg);
	} else if (returnCode == 2) {
		throw VCell::Exception(returnErrMsg);
	}
	return optResultSet;
}

#define MAX_PROFILE_DISTRIBUTIONS 50

void OptSolverCFSQP::computeProfileDistributions(OptResultSet* optResultSet) {
	OptProblemDescription* optProb = getOptProblemDescription();

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

	optProb->getParameterDescription()->getScaledLimits(bl,bu);
	optProb->getParameterDescription()->getScaledInitialGuess(x);

	profileDist_obj_x = new double[nparam];

	int cfsqp_nparam = nparam - 1;
	double *cfsqp_bl = new double[cfsqp_nparam]; // lower parameter bounds. to specify nonexistent lower bounds (bl[j] <= -bigbnd).
	double *cfsqp_bu = new double[cfsqp_nparam]; // upper parameter bounds. to specify nonexistent upper bounds (bu[j] >= bigbnd).
	double *cfsqp_x = new double[cfsqp_nparam]; // initial guess, also holds iterate at the end of execution.
	double *cfsqp_lambda = new double[nineq+neq+nf+cfsqp_nparam]; // values of the lagrange multipliers at "x" at the end of execution.

	int returnCode = 0;
	string returnErrMsg;
	try {
		optResultSet->vectProfileDistribution.resize(nparam);
		for (int np = 0; np < nparam; np ++) {
			for (int i = 0, j = 0; i < nparam; i ++) {
				if (i != np) {
					//cfsqp_x[j] = x[i];
					cfsqp_bl[j] = bl[i];
					cfsqp_bu[j] = bu[i];
					j ++;
				}
			}
			optResultSet->vectProfileDistribution[np].fixedIndex = np;
			optResultSet->vectProfileDistribution[np].fixedParamName = optProb->getParameterDescription()->getParameterName(np);

			double step = (log(bu[np]) - log(bl[np]))/MAX_PROFILE_DISTRIBUTIONS;
			for (int dist = 0; dist <= MAX_PROFILE_DISTRIBUTIONS; dist ++) {
				for (int i = 0, j = 0; i < nparam; i ++) {
					if (i != np) {
						cfsqp_x[j] = optResultSet->bestRunResultSet.paramValues[i];
						j ++;
					}
				}
				optProb->getObjectiveFunction()->reset();

				double fixedValue = exp(log(bl[np]) + dist * step);
				fixParameter(np, fixedValue);

				int inform=-1; // return code from CFSQP()
				cfsqp(cfsqp_nparam,nf,nfsr,nineqn,nineq,neqn,neq,ncsrl,ncsrn,mesh_pts,
						mode,iprint,miter,&inform,bigbnd,eps,epsneq,udelta,cfsqp_bl,cfsqp_bu,cfsqp_x,
						f,g,cfsqp_lambda,obj_callback,constr_callback,gradob_callback,gradcn_callback,(void*)this);

				OptSolverStatus status = unknown;
				string statusMessage;
				computeCFSQPStatus(inform, status, statusMessage);
				
				int numParameters = optProb->getParameterDescription()->getNumParameters();

				OptRunResultSet optRunResultSet;
				optRunResultSet.status = status;
				double* bestParameterValues = optProb->getObjectiveFunction()->getBestParameterValues();
				for (int i=0;i<numParameters; i++){
					optRunResultSet.paramValues.push_back(bestParameterValues[i]);
				}
				optRunResultSet.statusMessage = statusMessage;
				optRunResultSet.objectiveFunctionValue = optProb->getObjectiveFunction()->getBestObjectiveFunctionValue();
				optRunResultSet.numObjFuncEvals = optProb->getObjectiveFunction()->getNumObjFuncEvals();

				optResultSet->vectProfileDistribution[np].optRunResultSets.push_back(optRunResultSet);
			}  // for (int dist = 0; dist <= 100; dist ++) {
		} // for (int np = 0;
		returnCode = 0;
	} catch (StoppedByUserException& ex) {
		returnCode = 1;
		returnErrMsg = ex.getMessage();
	} catch (VCell::Exception& ex) {
		returnCode = 2;
		returnErrMsg = ex.getMessage();
	} catch (const char* errMsg) {
		returnCode = 2;
		returnErrMsg = errMsg;
	} catch (string& errMsg) {
		returnCode = 2;
		returnErrMsg = errMsg;
	} catch (...) {
		returnCode = 2;
		returnErrMsg = "Parameter estimation solver has caught unknown exception. Please contact the Virtual Cell.";
	}
	delete[] cfsqp_bl;
	delete[] cfsqp_bu;
	delete[] cfsqp_x;
	delete[] bl;
	delete[] bu;
	delete[] x;
	delete[] f;
	delete[] g;
	delete[] cfsqp_lambda;

	if (returnCode == 1) {
		throw StoppedByUserException(returnErrMsg);
	} else if (returnCode == 2) {
		throw VCell::Exception(returnErrMsg);
	}
}

void OptSolverCFSQP::computeCFSQPStatus(int inform, OptSolverStatus& status, string& statusMessage) {
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
}

void OptSolverCFSQP::stop(){
}

void OptSolverCFSQP::setCheckStopRequested(void (*checkStopRequested)(double, long)) {
	getOptProblemDescription()->getObjectiveFunction()->setCheckStopRequested(checkStopRequested);
}

void OptSolverCFSQP::obj(int nparam, double *x, double *pobj){
	ObjectiveFunction* objFunc = getOptProblemDescription()->getObjectiveFunction();
	int obj_nparam = nparam;
	double* obj_x = x;
	if (isFixedParameter()) {
		obj_nparam = nparam + 1;
		obj_x = profileDist_obj_x;
		
		for (int i = 0, j = 0; i < obj_nparam; i ++) {
			if (i == getFixedParamterIndex()) {
				obj_x[getFixedParamterIndex()] = getFixedParamterValue();
			} else {
				obj_x[i] = x[j];
				j ++;
			}
		}
	}
	objFunc->objective(obj_nparam,obj_x,pobj);
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
