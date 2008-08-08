#include "VCellCVodeSolver.h"
#include "Expression.h"
#include "SimpleSymbolTable.h"
#include "Exception.h"
#include "OdeResultSet.h"
#include <assert.h>
#include "DivideByZeroException.h"
#include "FunctionDomainException.h"
#include "FunctionRangeException.h"
#include "StoppedByUserException.h"

#include <cvode/cvode.h>             /* prototypes for CVODE fcts. and consts. */
#include <nvector/nvector_serial.h>  /* serial N_Vector types, fcts., and macros */
#include <cvode/cvode_dense.h>       /* prototype for CVDense */
//#include <cvode/cvode_spgmr.h>       /* prototype for CVSPGMR */
#include <sundials/sundials_dense.h> /* definitions DenseMat DENSE_ELEM */
#include <sundials/sundials_types.h> /* definition of type realtype */

void VCellCVodeSolver::throwCVodeErrorMessage(int returnCode) {
	switch (returnCode){
		case CV_SUCCESS: {
			throw "CV_SUCCESS: CVode succeeded and no roots were found.";
		}						 
		case CV_ROOT_RETURN: {
			throw "CV_ROOT_RETURN: CVode succeeded, and found one or more roots. If nrtfn > 1, call CVodeGetRootInfo to see which g_i were found to have a root at (*tret).";
		}
		case CV_TSTOP_RETURN: {
			throw "CV_TSTOP_RETURN: CVode succeeded and returned at tstop.";
		}
		case CV_MEM_NULL:{
			throw "CV_MEM_NULL: mem argument was null";
		}
		case CV_ILL_INPUT:{
			throw "CV_ILL_INPUT: one of the inputs to CVode is illegal";
		}
		case CV_TOO_MUCH_WORK:{
			throw "CV_TOO_MUCH_WORK: took mxstep internal steps but could not reach tout";
		}
		case CV_TOO_MUCH_ACC:{
			throw "CV_TOO_MUCH_ACC: could not satisfy the accuracy demanded by the user for some internal step";
		}
		case CV_ERR_FAILURE:{
			throw "CV_ERR_FAILURE: error test failures occurred too many times during one internal step";
		}
		case CV_CONV_FAILURE:{
			throw "CV_CONV_FAILURE: convergence test failures occurred too many times during one internal step";
		}
		case CV_LINIT_FAIL:{
			throw "CV_LINIT_FAIL: the linear solver's initialization function failed.";
		}
		case CV_LSETUP_FAIL:{
			throw "CV_LSETUP_FAIL: the linear solver's setup routine failed in an unrecoverable manner.";
		}
		case CV_LSOLVE_FAIL:{
			throw "CV_LSOLVE_FAIL: the linear solver's solve routine failed in an unrecoverable manner";
		}
		case CV_REPTD_RHSFUNC_ERR: {
			stringstream ss;
			ss << "repeated recoverable right-hand side function errors : " << recoverableErrMsg;
			throw ss.str();
		}
		case CV_UNREC_RHSFUNC_ERR:{
			stringstream ss;
			ss << "the right-hand side failed in a recoverable manner, but no recovery is possible : " <<  recoverableErrMsg;
			throw ss.str();
		}
		default:
			throw CVodeGetReturnFlagName(returnCode);
	}	
}

void VCellCVodeSolver::checkCVodeFlag(int flag) {
	if (flag != CV_SUCCESS){
		throwCVodeErrorMessage(flag);
	}
}

VCellCVodeSolver::VCellCVodeSolver() : VCellSundialsSolver() {
	rateExpressions = 0;
	rateSymbolTable = 0;
}

VCellCVodeSolver::~VCellCVodeSolver() {
	CVodeFree(&solver);

	for (int i = 0; i < NEQ; i ++) {
		delete rateExpressions[i];
	}
	delete[] rateExpressions;
	delete rateSymbolTable;
}

/*
Input format:
	STARTING_TIME 0.0
	ENDING_TIME 0.1
	RELATIVE_TOLERANCE 1.0E-9
	ABSOLUTE_TOLERANCE 1.0E-9
	MAX_TIME_STEP 1.0
	KEEP_EVERY 1
	DISCONTINUITIES 1
	D_B0 (t > 0.0432); (-0.0432 + t);
	NUM_EQUATIONS 2
	ODE x_i INIT 0.0;
		 RATE ((20.0 * x_o * D_B0) - (50.0 * x_i));
	ODE x_o INIT 0.0;
		 RATE ( - ((20.0 * x_o * D_B0) - (50.0 * x_i)) + (1505000.0 * (3.322259136212625E-4 - (3.322259136212625E-4 * x_o) - (3.322259136212625E-4 * x_i))) - (100.0 * x_o));
*/
void VCellCVodeSolver::readEquations(istream& inputstream) { 
	try {
		string name;
		string exp;
		
		rateExpressions = new Expression*[NEQ];		

		for (int i = 0; i < NEQ; i ++) {
			// ODE
			inputstream >> name >> variableNames[i];			

			// INIT
			inputstream >> name;
			exp = "";
			getline(inputstream, exp);
			trimString(exp);
			if (*(exp.end() - 1) != ';') {
				string msg = "Initial condition expression for [" + variableNames[i] + "]" + BAD_EXPRESSION_MSG;
				throw Exception(msg);
			}
			initialConditionExpressions[i] = new Expression(exp);

			// RATE
			inputstream >> name;
			exp = "";
			getline(inputstream, exp);
			trimString(exp);
			if (*(exp.end() - 1) != ';') {
				string msg = "Rate expression for [" + variableNames[i] + "]" + BAD_EXPRESSION_MSG;
				throw Exception(msg);
			}
			rateExpressions[i] = new Expression(exp);
		}				
	} catch (char* ex) {
		throw Exception(string("VCellCVodeSolver::readInput() : ") + ex);
	} catch (Exception& ex) {
		throw Exception(string("VCellCVodeSolver::readInput() : ") + ex.getMessage());
	} catch (...) {
		throw "VCellCVodeSolver::readInput() : caught unknown exception";
	}
}

void VCellCVodeSolver::initialize() {
	VCellSundialsSolver::initialize();

	// rate can be function of variables, parameters and discontinuities.
	rateSymbolTable = new SimpleSymbolTable(allSymbols, numAllSymbols);
	for (int i = 0; i < NEQ; i ++) {
		rateExpressions[i]->bindExpression(rateSymbolTable);
	}
}

int VCellCVodeSolver::RHS (realtype t, N_Vector y, N_Vector r) {	
	try {
		values[0] = t;
		memcpy(values + 1, NV_DATA_S(y), NEQ * sizeof(realtype));
		double* r_data = NV_DATA_S(r);
		for (int i = 0; i < NEQ; i ++) {
			r_data[i] = rateExpressions[i]->evaluateVector(values);
		}
		recoverableErrMsg = "";
		return 0;
	}catch (DivideByZeroException e){
		cout << "failed to evaluate residual: " << e.getMessage() << endl;
		recoverableErrMsg = e.getMessage();
		return 1;
	}catch (FunctionDomainException e){
		cout << "failed to evaluate residual: " << e.getMessage() << endl;
		recoverableErrMsg = e.getMessage();
		return 1;
	}catch (FunctionRangeException e){
		cout << "failed to evaluate residual: " << e.getMessage() << endl;
		recoverableErrMsg = e.getMessage();
		return 1;
	}
}

double VCellCVodeSolver::RHS (double* allValues, int equationIndex) {	
	return rateExpressions[equationIndex]->evaluateVector(allValues);		
}

int VCellCVodeSolver::RHS_callback(realtype t, N_Vector y, N_Vector r, void *fdata) {
	VCellCVodeSolver* solver = (VCellCVodeSolver*)fdata;
	return solver->RHS(t, y, r);
}

int VCellCVodeSolver::RootFn(realtype t, N_Vector y, realtype *gout) {
	values[0] = t;
	memcpy(values + 1, NV_DATA_S(y), NEQ * sizeof(realtype));
	for (int i = 0; i < numDiscontinuities; i ++) {
		gout[i] = odeDiscontinuities[i]->rootFindingExpression->evaluateVector(values);
	}
	return 0;
}

int VCellCVodeSolver::RootFn_callback(realtype t, N_Vector y, realtype *gout, void *g_data) {
	VCellCVodeSolver* solver = (VCellCVodeSolver*)g_data;
	return solver->RootFn(t, y, gout);
}

void VCellCVodeSolver::solve(double* paramValues, bool bPrintProgress, FILE* outputFile, void (*checkStopRequested)(double, long)) {
	if (checkStopRequested != 0) {
		checkStopRequested(STARTING_TIME, 0);
	}

	writeFileHeader(outputFile);

	// clear data in result set before solving
	odeResultSet->clearData();

	// copy parameter values to the end of values, these will stay the same during solving
	memset(values, 0, (NEQ + 1) * sizeof(double));
	memcpy(values + 1 + NEQ, paramValues, NPARAM * sizeof(double));
	memset(values + 1 + NEQ + NPARAM, 0, numDiscontinuities * sizeof(double));

	initCVode(paramValues);
	cvodeSolve(bPrintProgress, outputFile, checkStopRequested);
}

void VCellCVodeSolver::initCVode(double* paramValues) {
	//Initialize y, variable portion of values
	values[0] = STARTING_TIME;
	for (int i = 0; i < NEQ; i ++) {
		values[1 + i] = initialConditionExpressions[i]->evaluateVector(paramValues);
		NV_Ith_S(y, i) = values[1 + i];	
	}		

	reInit(STARTING_TIME);

	if (numDiscontinuities > 0) {
		int flag = CVodeRootInit(solver, numDiscontinuities, RootFn_callback, this);
		checkCVodeFlag(flag);
		initDiscontinuities();
	}
}

void VCellCVodeSolver::reInit(double t) {
	int flag = 0;
	int ToleranceType = CV_SS;
	if (solver == 0) {
		solver = CVodeCreate(CV_BDF, CV_NEWTON);
		if (solver == 0) {
			throw "VCellCVodeSolver:: Out of memory";
		}
		flag = CVodeMalloc(solver, RHS_callback, t, y, ToleranceType, RelativeTolerance, &AbsoluteTolerance);
		checkCVodeFlag(flag);

		flag = CVodeSetFdata(solver, this);
		checkCVodeFlag(flag);
		flag = CVDense(solver, NEQ);
		//flag = CVSpgmr(solver, PREC_NONE, 0);
		checkCVodeFlag(flag);

		flag = CVodeSetMaxNumSteps(solver, 5000);
	} else {
		flag = CVodeReInit(solver, RHS_callback, t, y, ToleranceType, RelativeTolerance, &AbsoluteTolerance);
	}
	checkCVodeFlag(flag);
}

void VCellCVodeSolver::cvodeSolve(bool bPrintProgress, FILE* outputFile, void (*checkStopRequested)(double, long)) {	
	if (checkStopRequested != 0) {
		checkStopRequested(STARTING_TIME, 0);
	}

	realtype Time = STARTING_TIME;
	double percentile=0.00;
	double increment =0.01;
	long iterationCount=0;
	long saveCount=0;

	// write intial conditions
	writeData(Time, outputFile);
	if (bPrintProgress) {
		printProgress(Time, percentile, increment);
	}

	if (outputTimes.size() == 0) {
		while (Time < ENDING_TIME) {
			if (checkStopRequested != 0) {
				checkStopRequested(Time, iterationCount);
			}								
			
			double tstop = min(ENDING_TIME, Time + 2 * maxTimeStep + (1e-15));
			CVodeSetStopTime(solver, tstop);
			int returnCode = CVode(solver, ENDING_TIME, y, &Time, CV_ONE_STEP_TSTOP);
			iterationCount++;

			// save data if return CV_TSTOP_RETURN (meaning reached end of time or max time step 
			// before one normal step) or CV_SUCCESS (meaning one normal step)
			if (returnCode == CV_TSTOP_RETURN || returnCode == CV_SUCCESS || returnCode == CV_ROOT_RETURN) {						
				if (returnCode == CV_ROOT_RETURN) {
					cout << setprecision(20);
					cout << endl << "found root at " << Time << endl;
					// flip discontinuities				
					int flag = CVodeGetRootInfo(solver, rootsFound);
					checkCVodeFlag(flag);
					updateDiscontinuities();
					reInit(Time);
				} else {
					checkDiscontinuityConsistency(Time, y);
				}
				if (returnCode == CV_ROOT_RETURN || iterationCount % keepEvery == 0 || Time >= ENDING_TIME){
					saveCount++;
					if (((double)saveCount) * (NEQ + 1) * bytesPerSample > (double)MaxFileSizeBytes){ 
						/* if more than one gigabyte, then fail */ 
						char msg[100];
						sprintf(msg, "output exceeded maximum %ld bytes", MaxFileSizeBytes);
						throw Exception(msg);
					}
					writeData(Time, outputFile);
					if (bPrintProgress) {
						printProgress(Time, percentile, increment);
					}
				}
			} else {
				throwCVodeErrorMessage(returnCode);
			}
		}
	} else {
		double sampleTime = 0.0;
		int outputCount = 0;
		assert(outputTimes[0] > STARTING_TIME);
		while (Time < ENDING_TIME && outputCount < (int)outputTimes.size()) {
			if (checkStopRequested != 0) {
				checkStopRequested(Time, iterationCount);
			}

			sampleTime = outputTimes[outputCount];	
			while (Time < sampleTime) {
				if (checkStopRequested != 0) {
					checkStopRequested(Time, iterationCount);
				}

				double tstop = min(sampleTime, Time + 2 * maxTimeStep + (1e-15));
				CVodeSetStopTime(solver, tstop);
				int returnCode = CVode(solver, sampleTime, y, &Time, CV_NORMAL_TSTOP);
				iterationCount++;	

				// if return CV_SUCCESS, this is an intermediate result, continue without saving data.
				if (returnCode == CV_TSTOP_RETURN || returnCode == CV_SUCCESS || returnCode == CV_ROOT_RETURN) {
					if (returnCode == CV_ROOT_RETURN) {
						// flip discontinuities				
						int flag = CVodeGetRootInfo(solver, rootsFound);
						checkCVodeFlag(flag);
						updateDiscontinuities();
						reInit(Time);
					}  else {
						checkDiscontinuityConsistency(Time, y);
					}
					if (Time == sampleTime) {
						writeData(Time, outputFile);
						if (bPrintProgress) {
							printProgress(Time, percentile, increment);
						}
						outputCount ++;
						break;
					}
				} else {
					throwCVodeErrorMessage(returnCode);
				}
			}
		}
	}	
}
