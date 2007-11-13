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
#include <sundials/sundials_dense.h> /* definitions DenseMat DENSE_ELEM */
#include <sundials/sundials_types.h> /* definition of type realtype */

char* getCVodeErrorMessage(int returnCode) {
	switch (returnCode){
		case CV_SUCCESS: {
			return "CV_SUCCESS: CVode succeeded and no roots were found.";
		}						 
		case CV_ROOT_RETURN: {
			return "CV_ROOT_RETURN: CVode succeeded, and found one or more roots. If nrtfn > 1, call CVodeGetRootInfo to see which g_i were found to have a root at (*tret).";
		}   
		case CV_TSTOP_RETURN: {
			return "CV_TSTOP_RETURN: CVode succeeded and returned at tstop.";
		}
		case CV_MEM_NULL:{
			return "CV_MEM_NULL: mem argument was null";
		}
		case CV_ILL_INPUT:{
			return "CV_ILL_INPUT: one of the inputs to CVode is illegal";
		}
		case CV_TOO_MUCH_WORK:{
			return "CV_TOO_MUCH_WORK: took mxstep internal steps but could not reach tout";
		}
		case CV_TOO_MUCH_ACC:{
			return "CV_TOO_MUCH_ACC: could not satisfy the accuracy demanded by the user for some internal step";
		}
		case CV_ERR_FAILURE:{
			return "CV_ERR_FAILURE: error test failures occurred too many times during one internal step";
		}
		case CV_CONV_FAILURE:{
			return "CV_CONV_FAILURE: convergence test failures occurred too many times during one internal step";
		}
		case CV_LINIT_FAIL:{
			return "CV_LINIT_FAIL: the linear solver's initialization function failed.";
		}
		case CV_LSETUP_FAIL:{
			return "CV_LSETUP_FAIL: the linear solver's setup routine failed in an unrecoverable manner.";
		}
		case CV_LSOLVE_FAIL:{
			return "CV_LSOLVE_FAIL: the linear solver's solve routine failed in an unrecoverable manner";
		}
		default:
			return CVodeGetReturnFlagName(returnCode);
	}	
}

void checkCVodeFlag(int flag) {
	if (flag != CV_SUCCESS){
		throw getCVodeErrorMessage(flag);
	}
}

VCellCVodeSolver::VCellCVodeSolver() : VCellSundialsSolver() {
	rateExpressions = 0;
	rateSymbolTable = 0;
}

VCellCVodeSolver::~VCellCVodeSolver() {
	for (int i = 0; i < NEQ; i ++) {
		delete rateExpressions[i];
	}
	delete[] rateExpressions;
	delete rateSymbolTable;
}

/*
Input format: (NUM_EQUATIONS must be the last parameter before VARIABLES)
	STARTING_TIME 0.0
	ENDING_TIME 0.45
	RELATIVE_TOLERANCE 1.0E-12
	ABSOLUTE_TOLERANCE 1.0E-10				
	MAX_TIME_STEP 4.5E-5
	[KEEP_EVERY 1 | OUTPUT_TIME_STEP 0.05 | OUTPUT_TIMES NUM_OUTPUT_TIMES time1 time2 time3 ....]
	NUM_PARAMETERS 3
	k1
	k2
	k3
	NUM_EQUATIONS 1
	ODE S2 INIT 0.0 RATE (1000000.0 * asech((5.0E-7 * (1000000.0 - S2))));
*/
void VCellCVodeSolver::readInput(istream& inputstream) { 
	try {
		string name;
		while (true) {
			inputstream >> name;
			if (name == "STARTING_TIME") {
				inputstream >> STARTING_TIME;
			} else if (name == "ENDING_TIME") {
				inputstream >> ENDING_TIME;
			} else if (name == "RELATIVE_TOLERANCE") {
				inputstream >> RelativeTolerance;
			} else if (name == "ABSOLUTE_TOLERANCE") {
				inputstream >> AbsoluteTolerance;
			} else if (name == "MAX_TIME_STEP") {
				inputstream >> maxTimeStep;
			} else if (name == "KEEP_EVERY") {
				inputstream >> keepEvery;
			} else if (name == "OUTPUT_TIME_STEP") {
				double outputTimeStep = 0.0;
				inputstream >> outputTimeStep;
				double timePoint = 0.0;
				int count = 1;
				while (STARTING_TIME + count * outputTimeStep < ENDING_TIME + 1E-12) {
					timePoint = STARTING_TIME + count * outputTimeStep;
					outputTimes.push_back(timePoint);
					count ++;
				}
				ENDING_TIME = outputTimes[outputTimes.size() - 1];
			} else if (name == "OUTPUT_TIMES") {
				int totalNumTimePoints;	
				double timePoint;
				inputstream >> totalNumTimePoints;
				for (int i = 0; i < totalNumTimePoints; i ++) {
					inputstream >> timePoint;
					if (timePoint > STARTING_TIME && timePoint <= ENDING_TIME) {
						outputTimes.push_back(timePoint);
					}
				}
				if (outputTimes[outputTimes.size() - 1] < ENDING_TIME) {
					outputTimes.push_back(ENDING_TIME);
				}
			} else if (name == "NUM_PARAMETERS") {
				inputstream >> NPARAM;
				for (int i = 0; i < NPARAM; i ++) {
					inputstream >> name;
					paramNames.push_back(name);
				}				
			} else if (name == "NUM_EQUATIONS") {
				inputstream >> NEQ;
				break;
			} else {
				string msg = "Unexpected token \"" + name + "\" in the input file!";
				throw Exception(msg);
			}
		}

		string* VariableNames = new string[NEQ + 1 + NPARAM];
		initialConditionExpressions = new Expression*[NEQ];	
		rateExpressions = new Expression*[NEQ];
		char exp[MAX_EXPRESSION_LENGTH];

		// add "t" first
		string variableName = "t";
		odeResultSet->addColumn(variableName);
		VariableNames[0] = variableName; 

		for (int i = 0; i < NEQ; i ++) {
			// ODE
			inputstream >> name >> variableName;
			odeResultSet->addColumn(variableName);
			// add columns to symbol table
			VariableNames[i + 1] = variableName;

			// INIT
			inputstream >> name;
			memset(exp, 0, MAX_EXPRESSION_LENGTH*sizeof(char));
			inputstream.getline(exp, MAX_EXPRESSION_LENGTH);
			char* trimmedExp = trim(exp);
			if (trimmedExp[strlen(trimmedExp)-1] != ';') {
				string msg = "Initial condition expression for [" + variableName + "]" + BAD_EXPRESSION_MSG;
				throw Exception(msg);
			}
			initialConditionExpressions[i] = new Expression(trimmedExp);
			delete[] trimmedExp;			

			// RATE
			inputstream >> name;

			memset(exp, 0, MAX_EXPRESSION_LENGTH*sizeof(char));
			inputstream.getline(exp, MAX_EXPRESSION_LENGTH);
			trimmedExp = trim(exp);
			if (trimmedExp[strlen(trimmedExp)-1] != ';') {
				string msg = "Rate expression for [" + variableName + "]" + BAD_EXPRESSION_MSG;
				throw Exception(msg);
			}
			rateExpressions[i] = new Expression(trimmedExp);
			delete[] trimmedExp;
		}

		// add parameters to symbol table
		for (int i = 0 ; i < NPARAM; i ++) {
			VariableNames[NEQ + 1 + i] = paramNames.at(i); 
		}
		rateSymbolTable = new SimpleSymbolTable(VariableNames, NEQ + 1 + NPARAM);
		initialConditionSymbolTable = new SimpleSymbolTable(VariableNames + NEQ + 1, NPARAM);
		for (int i = 0; i < NEQ; i ++) {
			rateExpressions[i]->bindExpression(rateSymbolTable);
			initialConditionExpressions[i]->bindExpression(initialConditionSymbolTable);			
		}

		try {
			values = new realtype[NEQ + 1 + NPARAM];
			tempRowData = new realtype[NEQ + 1];
		} catch (...) {
			throw "Out of Memory";
		}

		delete[] VariableNames;

		y = N_VNew_Serial(NEQ);
		if (y == 0) {
			throw "Out of Memory";
		}
	} catch (char* ex) {
		throw Exception(string("VCellCVodeSolver::readInput() : ") + ex);
	} catch (Exception& ex) {
		throw Exception(string("VCellCVodeSolver::readInput() : ") + ex.getMessage());
	} catch (...) {
		throw "VCellCVodeSolver::readInput() : caught unknown exception";
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
		return 0;
	}catch (DivideByZeroException e){
		cout << "failed to evaluate residual: " << e.getMessage() << endl;
		return 1;
	}catch (FunctionDomainException e){
		cout << "failed to evaluate residual: " << e.getMessage() << endl;
		return 1;
	}catch (FunctionRangeException e){
		cout << "failed to evaluate residual: " << e.getMessage() << endl;
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

void VCellCVodeSolver::solve(double* paramValues, bool bPrintProgress, FILE* outputFile, void (*checkStopRequested)(double, long)) {
	if (checkStopRequested != 0) {
		checkStopRequested(STARTING_TIME, 0);
	}

	if (outputFile != 0) {	
		//  Print header...
		for (int i = 0; i < odeResultSet->getNumColumns(); i++) {
			fprintf(outputFile, "%s:", odeResultSet->getColumnName(i).data());
		}
		fprintf(outputFile, "\n");
	}
	// clear data in result set before solving
	odeResultSet->clearData();

	realtype Time = STARTING_TIME;
	// copy parameter values to the end of values, these will stay the same during solving
	memset(values, 0, (NEQ + 1) * sizeof(double));
	memcpy(values + NEQ + 1, paramValues, NPARAM * sizeof(double));

	//Initialize y
	for (int i = 0; i < NEQ; i ++) {
		NV_Ith_S(y, i) = initialConditionExpressions[i]->evaluateVector(paramValues);		
	}		

	int ToleranceType = CV_SS;
	void* cvode_mem = CVodeCreate(CV_BDF, CV_NEWTON);
	if (cvode_mem == 0) {
		throw "Out of memory";
	}
	int flag = CVodeMalloc(cvode_mem, RHS_callback, STARTING_TIME, y, ToleranceType, RelativeTolerance, &AbsoluteTolerance);
	checkCVodeFlag(flag);
	CVodeSetFdata(cvode_mem, this);
	flag = CVDense(cvode_mem, NEQ);
	checkCVodeFlag(flag);

	// write intial conditions
	writeData(Time, y, outputFile);

	double percentile=0.00;
	double increment =0.01;
	long iterationCount=0;
	long saveCount=0;

	if (outputTimes.size() == 0) {
		while (Time < ENDING_TIME) {
			if (checkStopRequested != 0) {
				checkStopRequested(Time, iterationCount);
			}								
			
			double tstop = min(ENDING_TIME, Time + 2 * maxTimeStep + (1e-15));
			CVodeSetStopTime(cvode_mem, tstop);
			int returnCode = CVode(cvode_mem, ENDING_TIME, y, &Time, CV_ONE_STEP_TSTOP);
			iterationCount++;

			// save data if return CV_TSTOP_RETURN (meaning reached end of time or max time step 
			// before one normal step) or CV_SUCCESS (meaning one normal step)
			if (returnCode == CV_TSTOP_RETURN || returnCode == CV_SUCCESS) {						
				if (iterationCount % keepEvery == 0 || Time >= ENDING_TIME){
					saveCount++;
					if (((double)saveCount) * (NEQ + 1) * bytesPerSample > (double)MaxFileSizeBytes){ 
						/* if more than one gigabyte, then fail */ 
						char msg[100];
						sprintf(msg, "output exceeded maximum %ld bytes", MaxFileSizeBytes);
						throw Exception(msg);
					}
					writeData(Time, y, outputFile);
					if (bPrintProgress) {
						printProgress(Time, percentile, increment);
					}
				}
			} else {
				throw getCVodeErrorMessage(returnCode);
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
				CVodeSetStopTime(cvode_mem, tstop);
				int returnCode = CVode(cvode_mem, sampleTime, y, &Time, CV_NORMAL_TSTOP);
				iterationCount++;	

				// if return CV_SUCCESS, this is an intermediate result, continue without saving data.
				if (returnCode == CV_TSTOP_RETURN || returnCode == CV_SUCCESS) {
					if (Time == sampleTime) {
						writeData(Time, y, outputFile);
						if (bPrintProgress) {
							printProgress(Time, percentile, increment);
						}
						outputCount ++;
						break;
					}
				} else {
					throw getCVodeErrorMessage(returnCode);
				}									
			}
		}
	}	
	CVodeFree(&cvode_mem);
}
