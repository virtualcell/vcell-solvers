#include "llnlmath.h"
#include "ida.h"
#include "idadense.h"
#include "Expression.h"
#include "SimpleSymbolTable.h"
#include "VCellIDASolver.h"
#include "OdeResultSet.h"
#include "Exception.h"
#include "StoppedByUserException.h"
using namespace VCell;

#include <assert.h>

#define Ith(v,i)    N_VIth(v,i)
#define IJth(A,i,j) DENSE_ELEM(A,i,j)

char* getErrorMessage(integer Status) {
	char *errMsg = NULL;
	switch (Status){
		case NORMAL_RETURN:{
			errMsg = "unexpected return code (succeeded)";
			break;
		}
		case IDA_NO_MEM:{
			errMsg = "mem argument was null";
			break;
		}
		case ILL_INPUT:{
			errMsg = "One of the inputs to IDASolve is illegal";
			break;
		}
		case TOO_MUCH_WORK:{
			errMsg = "took mxstep internal steps but could not reach tout";
			break;
		}
		case TOO_MUCH_ACC:{
			errMsg = "could not satisfy the accuracy demanded by the user for some internal step";
			break;
		}
		case ERR_FAILURE:{
			errMsg = "error test failures occurred too many times during one internal step";
			break;
		}
		case CONV_FAILURE:{
			errMsg = "convergence test failures occurred too many times during one internal step";
			break;
		}
		case SETUP_FAILURE:{
			errMsg = "the linear solver's setup routine failed in an unrecoverable manner";
			break;
		}
		case SOLVE_FAILURE:{
			errMsg = "the linear solver's solve routine failed in an unrecoverable manner";
			break;
		}
		case CONSTR_FAILURE:{
			errMsg = "the inequality constraints were violated, and the solver was unable to recover";
			break;
		}
		case REP_RES_REC_ERR:{
			errMsg = "the user's residual function repeatedly returned a recoverable error flag, but the solver was unable to recover";
			break;
		}
		case RES_NONRECOV_ERR:{
			errMsg = "the user's residual function returned a nonrecoverable error flag";
			break;
		}
	}
	char* err = new char[1024];
	sprintf(err, "IDASolve returned %d, msg='%s'\n", Status, errMsg);
	return err;
}

char* trim(char* str) {	
	int leftIndex, rightIndex;
	int len = strlen(str);
	for (leftIndex = 0; leftIndex < len; leftIndex ++) { // remove leading spaces
		char c = str[leftIndex];
		if (c != ' ' && c != '\n' && c != '\r') {
			break;
		}
	}
	for (rightIndex = len - 1; rightIndex >= 0; rightIndex --) { // remove trailing spaces and new line and carriage return		
		char c = str[rightIndex];
		if (c != ' ' && c != '\n' && c != '\r') {
			break;
		}
	}

	len = rightIndex - leftIndex + 2;
	if (len <= 0) {
		return 0;
	}

	char* newstr = new char[len];
	memset(newstr, 0, len * sizeof(char));
	strncpy(newstr, str + leftIndex, len - 1);

	return newstr;
}

VCellIDASolver::VCellIDASolver(istream& inputstream, bool arg_bPrintProgress) {
	bPrintProgress = arg_bPrintProgress;
	rateSymbolTable = 0;
	initialConditionSymbolTable = 0;

	NEQ = 0;
	NPARAM = 0;
	STARTING_TIME = 0.0;
	ENDING_TIME   = 0.0;
	RelativeTolerance = 0.0;
	AbsoluteTolerance = 0.0;
	keepEvery = 0;
	maxTimeStep = 0.0;		

	rateExpressions = 0;
	initialConditionExpressions = 0;
	values = 0;
	tempRowData = 0;

	odeResultSet = new OdeResultSet();

	readInput(inputstream);
}

VCellIDASolver::~VCellIDASolver() {
	for (int i = 0; i < NEQ; i ++) {
		delete rateExpressions[i];
	}
	delete[] rateExpressions;

	for (int i = 0; i < NEQ; i ++) {
		delete initialConditionExpressions[i];
	}
	delete[] initialConditionExpressions;
	
	delete[] values;
	delete[] tempRowData;
	delete initialConditionSymbolTable;
	delete rateSymbolTable;
	delete odeResultSet;
}

//  Rates
void VCellIDASolver::RHS (integer N, real t, N_Vector y, N_Vector yp) {	
	values[0] = t;
	memcpy(values + 1, y->data, N * sizeof(real));
	for (int i = 0; i < N; i ++) {
		Ith(yp, i) = rateExpressions[i]->evaluateVector(values);
	}	
}

double VCellIDASolver::RHS (double* allValues, int equationIndex) {	
	return rateExpressions[equationIndex]->evaluateVector(allValues);		
}

//  Residual
int VCellIDASolver::Residual_callback(integer N, real t, N_Vector y, N_Vector yp, N_Vector r, void *rdata) {
	VCellIDASolver* solver = (VCellIDASolver*)rdata;
	return solver->Residual(N, t, y, yp, r);
}

//  Residual
int VCellIDASolver::Residual(integer N, real t, N_Vector y, N_Vector yp, N_Vector r) {
	values[0] = t;	
	memcpy(values + 1, y->data, N * sizeof(real));
	for (int i = 0; i < N; i ++) {
		Ith(r, i) = rateExpressions[i]->evaluateVector(values) - Ith(yp, i);
	}
	return (SUCCESS);
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

#define MAX_EXPRESSION_LENGTH 40000

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
			values = new real[NEQ + 1 + NPARAM];
			tempRowData = new real[NEQ + 1];
		} catch (...) {
			throw "Out of Memory";
		}

		delete[] VariableNames;
	} catch (Exception& ex) {
		throw ex;
	} catch (const char* ex) {
		throw Exception(ex);
	} catch (...) {
		throw Exception("VCellIDASolver::readInput() caught unknown exception");
	}
}

void VCellIDASolver::writeData(double currTime, N_Vector y, FILE* outputFile) {
	tempRowData[0] = currTime; 
	for (int i = 0; i < NEQ; i++) { 
		tempRowData[i+1] = Ith(y,i); 
	} 
	odeResultSet->addRow(tempRowData);

	if (outputFile != 0) {
		fprintf(outputFile, "%0.17E", tempRowData[0]); 
		for (int i = 1; i < NEQ+1; i++) { 
			fprintf(outputFile, "\t%0.17E", tempRowData[i]); 			
		} 
		fprintf(outputFile, "\n");
	}
}

void VCellIDASolver::printProgress(double currTime, double& percentile, double increment) {	
	if (!bPrintProgress) {
		return;
	}
	while ((STARTING_TIME + ((percentile + increment) * (ENDING_TIME - STARTING_TIME))) <= currTime) { 
		percentile += increment; 
		printf("[[[progress:%lg%%]]]", percentile*100.0); 
		fflush(stdout); 
	} 
}

OdeResultSet* VCellIDASolver::getResultSet() {
	return odeResultSet;
}

Expression** VCellIDASolver::getInitialConditionExpressions() {
	return initialConditionExpressions;
}

void VCellIDASolver::solve(double* paramValues, FILE* outputFile, void (*checkStopRequested)(double, long)) {
	try {	
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

		// copy parameter values to the end of values, these will stay the same during solving
		memset(values, 0, (NEQ + 1) * sizeof(double));
		memcpy(values + NEQ + 1, paramValues, NPARAM * sizeof(double));

		long int iopt[OPT_SIZE];
		real ropt[OPT_SIZE];

		// Initialize y and y'.
		N_Vector y = N_VNew(NEQ, NULL);
		for (int i = 0; i < NEQ; i ++) {
			Ith(y,i) = initialConditionExpressions[i]->evaluateVector(paramValues);		
		}
		//
		N_Vector yp = N_VNew(NEQ, NULL);
		//  This might be replaced by a call to IDACalcIC() or something...later...
		//  For now, this computes y'...later we might want to compute y' AND equilibrate...
		RHS (NEQ, STARTING_TIME, y, yp);
		// Scalar relative tolerance, scalar absolute tolerance...
		// Later turn absolute tolerance into a vector...
		int ToleranceType = SS;

		// Call IDAMalloc to set up problem memory.
		void *IDAContext = (IDAMem) IDAMalloc(NEQ, Residual_callback, (void*)this, STARTING_TIME, y, yp,
			ToleranceType, &RelativeTolerance, &AbsoluteTolerance, NULL, NULL, NULL,
			FALSE, iopt, ropt,  NULL);
		if (IDAContext == NULL) {
			throw Exception("IDAMalloc failed.");
		}

		// Call IDADense and set up the linear solver package.
		if (IDADense(IDAContext, NULL, NULL) != SUCCESS) {
			throw Exception("IDADense failed.");
		}

		//
		integer Status;
		real Time = STARTING_TIME;
		// write initial conditions
		writeData(Time, y, outputFile);
		//
		double percentile=0.00;
		double increment =0.01;
		long iterationCount=0;
		long saveCount=0;
		const long bytesPerSample=25;
		const long MaxFileSizeBytes=1000*1000*1000; /* 1 gigabyte */	

		if (outputTimes.size() == 0) {
			do {
				if (checkStopRequested != 0) {
					checkStopRequested(Time, iterationCount);
				}
				while ((Status = IDASolve(IDAContext, ENDING_TIME, min(ENDING_TIME,Time+2*maxTimeStep+(1e-15)), &Time, y, yp, ONE_STEP_TSTOP)) == INTERMEDIATE_RETURN) {
					if (checkStopRequested != 0) {
						checkStopRequested(Time, iterationCount);
					}
					iterationCount++;
					if (iterationCount%keepEvery == 0){
						saveCount++;
						/* Check for unbounded simulation results file */
						if (((double)saveCount)*(NEQ+1)*bytesPerSample > (double)MaxFileSizeBytes){ /* if more than one gigabyte, then fail */ 
							char msg[100];
							sprintf(msg, "IDASolve ida file exceeded %ld bytes\n", MaxFileSizeBytes);
							throw Exception(msg);
						}
						writeData(Time, y, outputFile);
						printProgress(Time, percentile, increment);
					}
				}
				if (Status == TSTOP_RETURN) {	
					writeData(Time, y, outputFile);
					printProgress(Time, percentile, increment);					
				} else {
					char* errMsg = getErrorMessage(Status);
					throw Exception(errMsg);
				}
			} while (Time<ENDING_TIME);
		} else {
			double sampleTime = 0.0;
			int numIteration = 0;
			assert(outputTimes[0] > STARTING_TIME);
			do {
				if (checkStopRequested != 0) {
					checkStopRequested(Time, iterationCount);
				}
				sampleTime = outputTimes[numIteration];	
				while (Time < sampleTime) {
					if (checkStopRequested != 0) {
						checkStopRequested(Time, iterationCount);
					}
					Status = IDASolve(IDAContext, sampleTime, min(sampleTime,Time+2*maxTimeStep+(1e-15)), &Time, y, yp, ONE_STEP_TSTOP);
					iterationCount++;					
				}				
				assert(Time == sampleTime);
				if (Status == TSTOP_RETURN) {
					writeData(Time, y, outputFile);
					printProgress(Time, percentile, increment);
				} else {
					char* errMsg = getErrorMessage(Status);
					throw Exception(errMsg);
				}				
				numIteration ++;
			} while (Time < ENDING_TIME && numIteration < (int)outputTimes.size());
		}

		IDAFree(IDAContext);
		N_VFree(y);
		N_VFree(yp);
	} catch (StoppedByUserException& ex) {
		throw ex;
	} catch (Exception& ex) {
		throw ex;
	} catch (const char* ex) {
		throw Exception(ex);
	} catch (...) {
		throw Exception("VCellIDASolver::solve() caught unknown exception\n");
	}
}