#include "VCellIDASolver.h"
#include "Expression.h"
#include "SimpleSymbolTable.h"
#include "VCellIDASolver.h"
#include "OdeResultSet.h"
#include "Exception.h"
#include "OdeResultSet.h"
#include "StoppedByUserException.h"
using namespace VCell;

#include <assert.h>
#include <ida/ida.h>
#include <ida/ida_dense.h>
#include <nvector/nvector_serial.h>

char* getIDAErrorMessage(int Status) {
	char *errMsg = NULL;
	switch (Status) {
		case IDA_SUCCESS: {
			return "IDA_SUCCESS: IDASolve succeeded and no roots were found";
		}
		case IDA_ROOT_RETURN:  {
			return "IDA_ROOT_RETURN: IDASolve succeeded, and found one or more roots. If nrtfn > 1, call IDAGetRootInfo to see which g_i were found to have a root at (*tret).";
		}
		case IDA_TSTOP_RETURN: {
			return "IDA_TSTOP_RETURN: IDASolve returns computed results for the independent variable value tstop. That is, tstop was reached.";
		}
		case IDA_MEM_NULL: {
			return "IDA_MEM_NULL: The IDA_mem argument was NULL";
		}
		case IDA_ILL_INPUT: {
			return "IDA_ILL_INPUT: One of the inputs to IDASolve is illegal";
		}
		case IDA_TOO_MUCH_WORK: {
			return "IDA_TOO_MUCH_WORK: The solver took mxstep internal steps but could not reach tout. The default value for mxstep is MXSTEP_DEFAULT = 500";			
		}
		case IDA_TOO_MUCH_ACC: {
			return "IDA_TOO_MUCH_ACC: The solver could not satisfy the accuracy demanded by the user for some internal step.";
		}
		case IDA_ERR_FAIL: {
			return "IDA_ERR_FAIL: Error test failures occurred too many times (=MXETF = 10) during one internal step";
		}
		case IDA_CONV_FAIL: {
			return "IDA_CONV_FAIL: Convergence test failures occurred too many times (= MXNCF = 10) during one internal step.";
		}
		case IDA_LSETUP_FAIL:{
			return "IDA_LSETUP_FAIL: The linear solver's setup routine failed in an unrecoverable manner.";
		}
		case IDA_LSOLVE_FAIL:{
			return "IDA_LSOLVE_FAIL: The linear solver's solve routine failed  in an unrecoverable manner.";
		}
		case IDA_CONSTR_FAIL:{
			return "IDA_CONSTR_FAIL: The inequality constraints were violated, and the solver was unable to recover.";
		}
		case IDA_REP_RES_ERR:{
			return "IDA_REP_RES_ERR: The user's residual function repeatedly returned a recoverable error flag, but the solver was unable to recover.";
		}
		case IDA_RES_FAIL:{
			return "IDA_RES_FAIL: The user's residual function returned a nonrecoverable error flag.";
		}
		default:
			return "IDA: unknown error";
	}
}

VCellIDASolver::VCellIDASolver(istream& inputstream, bool arg_bPrintProgress) : VCellSundialsSolver(inputstream, arg_bPrintProgress) {
	rateExpressions = 0;
	rateSymbolTable = 0;

	yp = 0;

	readInput(inputstream);
}


VCellIDASolver::~VCellIDASolver() {
	N_VDestroy_Serial(yp);

	for (int i = 0; i < NEQ; i ++) {
		delete rateExpressions[i];
	}
	delete[] rateExpressions;
	delete rateSymbolTable;
}

//  Residual
int VCellIDASolver::Residual_callback(realtype t, N_Vector y, N_Vector yp, N_Vector r, void *rdata) {
	VCellIDASolver* solver = (VCellIDASolver*)rdata;
	return solver->Residual(t, y, yp, r);
}

//  Residual
int VCellIDASolver::Residual(realtype t, N_Vector y, N_Vector yp, N_Vector r) {
	values[0] = t;	
	memcpy(values + 1, NV_DATA_S(y), NEQ * sizeof(realtype));
	for (int i = 0; i < NEQ; i ++) {
		NV_Ith_S(r, i) = rateExpressions[i]->evaluateVector(values) - NV_Ith_S(yp, i);
	}
	return 0;
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
void VCellIDASolver::readInput(istream& inputstream) { 
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
				if (outputTimes[outputTimes.size() - 1] < ENDING_TIME) {
					outputTimes.push_back(ENDING_TIME);
				}
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
				ENDING_TIME = outputTimes[outputTimes.size() - 1];
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
			char* pexp = exp;
			pexp = trim(pexp);
			if (pexp[strlen(pexp)-1] != ';') {
				string msg = "Initial condition expression for [" + variableName + "] is not terminated by ';', it is either an invalid expression or longer than MAX_EXPRESSION_LENGTH (40000)";
				throw Exception(msg);
			}
			initialConditionExpressions[i] = new Expression(pexp);
			delete[] pexp;			

			// RATE
			inputstream >> name;

			memset(exp, 0, MAX_EXPRESSION_LENGTH*sizeof(char));
			inputstream.getline(exp, MAX_EXPRESSION_LENGTH);
			pexp = exp;
			pexp = trim(pexp);
			if (pexp[strlen(pexp)-1] != ';') {
				string msg = "Rate expression for [" + variableName + "] is not terminated by ';', it is either an invalid expression or longer than MAX_EXPRESSION_LENGTH (40000)";
				throw Exception(msg);
			}
			rateExpressions[i] = new Expression(pexp);
			delete[] pexp;
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
		check_flag((void *)y, "N_VNew_Serial", 0);
		yp = N_VNew_Serial(NEQ);
		check_flag((void *)yp, "N_VNew_Serial", 0);
	} catch (const char* ex) {
		throw Exception(string("VCellIDASolver::readInput() : ") + ex);
	} catch (Exception& ex) {
		throw Exception(string("VCellIDASolver::readInput() : ") + ex.getMessage());
	} catch (...) {
		throw "VCellIDASolver::readInput() : caught unknown exception";
	}
}

void VCellIDASolver::solve(double* paramValues, FILE* outputFile, void (*checkStopRequested)(double, long)) {
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
	
	// Initialize y and y'.
	for (int i = 0; i < NEQ; i ++) {
		NV_Ith_S(y,i) = initialConditionExpressions[i]->evaluateVector(paramValues);		
	}
	//  This might be replaced by a call to IDACalcIC() or something...later...
	//  For now, this computes y'...later we might want to compute y' AND equilibrate...
	values[0] = Time;
	memcpy(values + 1, NV_DATA_S(y), NEQ * sizeof(realtype));
	for (int i = 0; i < NEQ; i ++) {
		NV_Ith_S(yp, i) = rateExpressions[i]->evaluateVector(values);
	}		
	// Scalar relative tolerance, scalar absolute tolerance...
	// Later turn absolute tolerance into a vector...
	int ToleranceType = IDA_SS;

	void* ida_mem = IDACreate();
	check_flag((void *)ida_mem, "IDACreate", 0);
	// Call IDAMalloc to set up problem memory.
	int flag = IDAMalloc(ida_mem, Residual_callback, STARTING_TIME, y, yp, ToleranceType, RelativeTolerance, &AbsoluteTolerance);
	check_flag(&flag, "IDAMalloc", 1);
	IDASetRdata(ida_mem, this);
	flag = IDADense(ida_mem, NEQ);
	check_flag(&flag, "IDADense", 1);		
	
	// write initial conditions
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
			IDASetStopTime(ida_mem, tstop);
			int returnCode = IDASolve(ida_mem, ENDING_TIME, &Time, y, yp, IDA_ONE_STEP_TSTOP);
			iterationCount++;				

			// save data if return IDA_TSTOP_RETURN (meaning reached end of time or max time step 
			// before one normal step) or IDA_SUCCESS (meaning one normal step)
			if (returnCode == IDA_TSTOP_RETURN || returnCode == IDA_SUCCESS) {					
				if (iterationCount%keepEvery == 0 || Time >= ENDING_TIME){
					saveCount++;
					if (((double)saveCount)*(NEQ + 1) * bytesPerSample > (double)MaxFileSizeBytes){ 
						/* if more than one gigabyte, then fail */ 
						char msg[100];
						sprintf(msg, "output exceeded %ld bytes\n", MaxFileSizeBytes);
						throw Exception(msg);
					}
					writeData(Time, y, outputFile);
					if (bPrintProgress) {
						printProgress(Time, percentile, increment);
					}
				}
			} else {
				throw getIDAErrorMessage(returnCode);
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
				IDASetStopTime(ida_mem, tstop);
				int returnCode = IDASolve(ida_mem, sampleTime, &Time, y, yp, IDA_NORMAL_TSTOP);
				iterationCount++;					
						
				// if return IDA_SUCCESS, this is an intermediate result, continue without saving data.
				if (returnCode == IDA_TSTOP_RETURN || returnCode == IDA_SUCCESS) {
					if (Time == sampleTime) {
						writeData(Time, y, outputFile);
						if (bPrintProgress) {
							printProgress(Time, percentile, increment);
						}
						outputCount ++;
						break;
					}
				} else {
					throw getIDAErrorMessage(returnCode);
				}
			}				
		} 
	}

	IDAFree(&ida_mem);
}
