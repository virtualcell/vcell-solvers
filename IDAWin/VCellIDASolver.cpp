#include "VCellIDASolver.h"
#include "Expression.h"
#include "SimpleSymbolTable.h"
#include "OdeResultSet.h"
#include "Exception.h"
#include "StoppedByUserException.h"
#include "DivideByZeroException.h"
#include "FunctionDomainException.h"
#include "FunctionRangeException.h"
using namespace VCell;

#include <assert.h>
#include <ida/ida.h>
#include <ida/ida_dense.h>
#include <nvector/nvector_serial.h>

char* VCellIDASolver::getIDAErrorMessage(int returnCode) {
	char *errMsg = NULL;
	switch (returnCode) {
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
		case IDA_NO_MALLOC: {
			return "IDA_NO_MALLOC: The allocation function IDAMalloc has not been called.";
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
		case IDA_LINIT_FAIL: {
			return "IDA_LINIT_FAIL: The linear solver's initialization function failed.";
		}
		case IDA_LSETUP_FAIL:{
			return "IDA_LSETUP_FAIL: The linear solver's setup routine failed in an unrecoverable manner.";
		}
		case IDA_LSOLVE_FAIL:{
			return "IDA_LSOLVE_FAIL: The linear solver's solve routine failed in an unrecoverable manner.";
		}
		case IDA_RES_FAIL:{
			return "IDA_RES_FAIL: The user's residual function returned a nonrecoverable error flag.";
		}
		case IDA_CONSTR_FAIL:{
			return "IDA_CONSTR_FAIL: The inequality constraints were violated, and the solver was unable to recover.";
		}
		case IDA_REP_RES_ERR:{
			char* errMsg = new char[MAX_EXPRESSION_LENGTH + 100];
			sprintf(errMsg, "IDA_REP_RES_ERR: The user's residual function repeatedly returned a recoverable error flag, but the solver was unable to recover.: %s", recoverableErrMsg.c_str());
			return errMsg;
		}
		case IDA_MEM_FAIL:{
			return "IDA_MEM_FAIL: A memory allocation request has failed.";
		}
		case IDA_BAD_T:{
			return "IDA_BAD_T: ";
		}
		case IDA_BAD_EWT:{
			return "IDA_BAD_EWT: Some component of the error weight vector is zero (illegal).";
		}
		case IDA_FIRST_RES_FAIL:{
			return "IDA_FIRST_RES_FAIL: The user's residual function returned a recoverable error flag on the first call, but IDA was unable to recover.";
		}
		case IDA_LINESEARCH_FAIL:{
			return "IDA_LINESEARCH_FAIL: The linesearch algorithm failed to find a solution with a step larger than steptol in weighted RMS norm.";
		}
		case IDA_NO_RECOVERY:{
			return "IDA_NO_RECOVERY: The user's residual function, or the linear solver's setup or solve function had a recoverable error, but IDA was unable to recover.";
		}
		case IDA_RTFUNC_FAIL:{
			return "IDA_RTFUNC_FAIL: The rootfinding function failed.";
		}
		default:
			return IDAGetReturnFlagName(returnCode);
	}
}

void VCellIDASolver::checkIDAFlag(int flag) {
	if (flag != IDA_SUCCESS){
		throw getIDAErrorMessage(flag);
	}
}

VCellIDASolver::VCellIDASolver() : VCellSundialsSolver() {
	rhsExpressions = 0;
	rhsSymbolTable = 0;

	numDifferential = 0;
	numAlgebraic = 0;
	transformMatrix = 0;
	inverseTransformMatrix = 0;
	yp = 0;
	id = 0;
}

VCellIDASolver::~VCellIDASolver() {
	IDAFree(&solver);

	N_VDestroy_Serial(yp);
	N_VDestroy_Serial(id);	

	for (int i = 0; i < NEQ; i ++) {
		delete rhsExpressions[i];
	}
	delete[] rhsExpressions;
	delete rhsSymbolTable;

	for (int i = 0; i < NEQ; i ++) {
		delete transformMatrix[i];
		delete inverseTransformMatrix[i];
	}
	delete[] transformMatrix;
	delete[] inverseTransformMatrix;	
}

//  Residual
int VCellIDASolver::Residual_callback(realtype t, N_Vector y, N_Vector yp, N_Vector residual, void *rdata) {
	VCellIDASolver* solver = (VCellIDASolver*)rdata;
	return solver->Residual(t, y, yp, residual);
}

//  Residual
//
// assume that rate rhsExpressions have been substituted so that they are only functions of state variables
// and that the massMatrix is constant coefficient.
//
// note: we should return 0 for success and 1 for recoverable failure (e.g. y or yp is Inf or NaN)
//
int VCellIDASolver::Residual(realtype t, N_Vector y, N_Vector yp, N_Vector residual) {
	
	try {		
		values[0] = t;	
		for (int i = 0; i < NEQ; i ++) {
			values[i + 1] = 0;
			for (int j = 0; j < NEQ; j ++) {
				values[i + 1] += inverseTransformMatrix[i][j] * NV_Ith_S(y, j);
			}			
		}
		for (int i = 0; i < numDifferential; i ++) {				
			NV_Ith_S(residual, i) = rhsExpressions[i]->evaluateVector(values) - NV_Ith_S(yp, i);
		}
		for (int i = numDifferential; i < numDifferential + numAlgebraic; i ++) {
			NV_Ith_S(residual, i) = rhsExpressions[i]->evaluateVector(values);
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

/**----------------------------------------------------
Input format: 
	STARTING_TIME 0.0
	ENDING_TIME 1.0
	RELATIVE_TOLERANCE 1.0E-9
	ABSOLUTE_TOLERANCE 1.0E-9
	MAX_TIME_STEP 1.0
	OUTPUT_TIME_STEP 0.0020
	NUM_EQUATIONS 3
	VAR rf_nucleus INIT (5.0);
	VAR BS_nucleus INIT (20.0);
	VAR rfB_nucleus INIT (0.0);
	TRANSFORM
	0.2 0.0 0.0
	0.0 0.0 0.2
	0.0 1.0 0.0
	INVERSETRANSFORM
	5.0 0.0 0.0
	0.0 0.0 1.0
	0.0 5.0 0.0
	RHS DIFFERENTIAL 2 ALGEBRAIC 1
	- (0.2 * ((0.02 * BS_nucleus * rf_nucleus) - (0.1 * rfB_nucleus)));
	(0.2 * ((0.02 * BS_nucleus * rf_nucleus) - (0.1 * rfB_nucleus)));
	- ((0.1 * BS_nucleus * (-2.0 - (0.2 * rf_nucleus) + (0.2 * BS_nucleus))) - (0.5 * (4.0 - (0.2 * rfB_nucleus) - (0.2 * BS_nucleus))));
--------------------------------------------------------------*/

void VCellIDASolver::readInput(istream& inputstream) { 
	try {
		if (solver != 0) {
			throw "readInput should only be called once";
		}
		string token;
		while (true) {
			inputstream >> token;
			if (token == "STARTING_TIME") {
				inputstream >> STARTING_TIME;
			} else if (token == "ENDING_TIME") {
				inputstream >> ENDING_TIME;
			} else if (token == "RELATIVE_TOLERANCE") {
				inputstream >> RelativeTolerance;
			} else if (token == "ABSOLUTE_TOLERANCE") {
				inputstream >> AbsoluteTolerance;
			} else if (token == "MAX_TIME_STEP") {
				inputstream >> maxTimeStep;
			} else if (token == "KEEP_EVERY") {
				inputstream >> keepEvery;
			} else if (token == "OUTPUT_TIME_STEP") {
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
			} else if (token == "OUTPUT_TIMES") {
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
			} else if (token == "NUM_PARAMETERS") {
				inputstream >> NPARAM;
				for (int i = 0; i < NPARAM; i ++) {
					inputstream >> token;
					paramNames.push_back(token);
				}				
			} else if (token == "NUM_EQUATIONS") {
				inputstream >> NEQ;
				break;
			} else {
				string msg = "Unexpected token \"" + token + "\" in the input file!";
				throw Exception(msg);
			}
		}

		string* symbolNames = new string[NEQ + 1 + NPARAM];
		initialConditionExpressions = new Expression*[NEQ];	
		char exp[MAX_EXPRESSION_LENGTH];

		// add "t" first
		string variableName = "t";
		odeResultSet->addColumn(variableName);
		symbolNames[0] = variableName; 
		//
		// test whether it is a "VAR" block (i.e. whether VAR)
		//
		inputstream >> token;		
		if (token != "VAR"){
			throw "expecting VAR";
		}
		for (int i = 0; i < NEQ; i ++) {
			// consume "VAR" token, but first line has it's "VAR" already consumed
			if (i > 0){
				inputstream >> token;
			}		
			
			inputstream >> variableName;
			odeResultSet->addColumn(variableName);
			// add vars to symbol table
			symbolNames[i + 1] = variableName;

			// INIT
			inputstream >> token;
			memset(exp, 0, MAX_EXPRESSION_LENGTH*sizeof(char));
			inputstream.getline(exp, MAX_EXPRESSION_LENGTH);
			char* trimmedExp = trim(exp);
			if (trimmedExp[strlen(trimmedExp)-1] != ';') {
				string msg = "Initial condition expression for [" + variableName + "]" + BAD_EXPRESSION_MSG;
				throw Exception(msg);
			}
			initialConditionExpressions[i] = new Expression(trimmedExp);
			delete[] trimmedExp;			
		}

		//TRANSFORM
		transformMatrix = new double*[NEQ];	
		inputstream >> token; 
		if (token != "TRANSFORM") {
			throw "expecting TRANSFORM";
		}
		inputstream.getline(exp, MAX_EXPRESSION_LENGTH); // go to next line
		for (int i = 0; i < NEQ; i ++) {
			transformMatrix[i] = new double[NEQ];
			for (int j = 0; j < NEQ; j ++) {
				inputstream >> transformMatrix[i][j];
			}
		}
		//INVERSETRANSFORM
		inverseTransformMatrix = new double*[NEQ];
		inputstream >> token; 
		if (token != "INVERSETRANSFORM") {
			throw "expecting INVERSETRANSFORM";
		}
		inputstream.getline(exp, MAX_EXPRESSION_LENGTH); // go to next line
		for (int i = 0; i < NEQ; i ++) {
			inverseTransformMatrix[i] = new double[NEQ];
			for (int j = 0; j < NEQ; j ++) {
				inputstream >> inverseTransformMatrix[i][j];
			}
		}

		//RHS DIFFERENTIAL 2 ALGEBRAIC 1
		inputstream >> token; 
		if (token != "RHS") {
			throw "expecting RHS";
		}
		inputstream >> token;
		if (token != "DIFFERENTIAL") {
			throw "expecting DIFFERENTIAL";
		}
		inputstream >> numDifferential;
		inputstream >> token;
		if (token != "ALGEBRAIC") {
			throw "expecting ALGEBRAIC";
		}
		inputstream >> numAlgebraic;
		if (numDifferential + numAlgebraic != NEQ) {
			throw "numDifferential + numAlgebraic != NEQ";
		}
		inputstream.getline(exp, MAX_EXPRESSION_LENGTH); // go to next line

		rhsExpressions = new Expression*[NEQ];
		for (int i = 0; i < NEQ; i ++) {
			memset(exp, 0, MAX_EXPRESSION_LENGTH * sizeof(char));
			inputstream.getline(exp, MAX_EXPRESSION_LENGTH);
			char* trimmedExp = trim(exp);
			if (trimmedExp[strlen(trimmedExp)-1] != ';') {
				stringstream ss;
				ss << "RHS[" << i << "]" << BAD_EXPRESSION_MSG;
				throw Exception(ss.str());
			}
			rhsExpressions[i] = new Expression(trimmedExp);
			delete[] trimmedExp;
		}
		
		// add parameters to symbol table
		for (int i = 0 ; i < NPARAM; i ++) {
			symbolNames[NEQ + 1 + i] = paramNames.at(i); 
		}

		rhsSymbolTable = new SimpleSymbolTable(symbolNames, 1 + NEQ + NPARAM); // has time, vars, parameters
		initialConditionSymbolTable = new SimpleSymbolTable(symbolNames + NEQ + 1, NPARAM); // has parameters
		for (int i = 0; i < NEQ; i ++) {
			rhsExpressions[i]->bindExpression(rhsSymbolTable);
			initialConditionExpressions[i]->bindExpression(initialConditionSymbolTable);			
		}

		try {
			values = new realtype[NEQ + 1 + NPARAM];
			tempRowData = new realtype[NEQ + 1];
		} catch (...) {
			throw "Out of Memory";
		}

		y = N_VNew_Serial(NEQ);
		yp = N_VNew_Serial(NEQ);
		id = N_VNew_Serial(NEQ);

		if (y == 0 || yp == 0 || id == 0) {
			throw "Out of Memory";
		}		
		delete[] symbolNames;
	} catch (const char* ex) {
		throw Exception(string("VCellIDASolver::readInput() : ") + ex);		
	} catch (Exception& ex) {		
		throw Exception(string("VCellIDASolver::readInput() : ") + ex.getMessage());
	} catch (...) {
		throw Exception("VCellIDASolver::readInput() caught unknown exception");
	}
}

void VCellIDASolver::solve(double* paramValues, bool bPrintProgress, FILE* outputFile, void (*checkStopRequested)(double, long)) {
	if (checkStopRequested != 0) {
		checkStopRequested(STARTING_TIME, 0);
	}

	writeFileHeader(outputFile);

	// clear data in result set before solving
	odeResultSet->clearData();

	// copy parameter values to the end of values, these will stay the same during solving
	// values[0] is time, y values will be copied to 1~NEQ of values in residual function
	memset(values, 0, (NEQ + 1 + NPARAM) * sizeof(double));
	memcpy(values + NEQ + 1, paramValues, NPARAM * sizeof(double));	

	initIDA(paramValues);
	idaSolve(bPrintProgress, outputFile, checkStopRequested);	
}

#define DEBUG_PRINT \
	for (int i = 0; i < NEQ; i ++) {\
		cout << "y[" << i << "] = " << NV_Ith_S(y,i) << ",  yp[" << i << "] = " << NV_Ith_S(yp,i) << endl; \
	}

void VCellIDASolver::initIDA(double* paramValues) {
	// must initialize y and yp before call IDAMalloc
	// Initialize y, yp and id.
	for (int i = 0; i < NEQ; i ++) {
		NV_Ith_S(id, i) = i < numDifferential ? RCONST(1) : RCONST(0);
		NV_Ith_S(yp, i) = 0; // Initialize yp  to be 0, they will be reinitialize later.
		NV_Ith_S(y, i) = 0;		
		for (int j = 0; j < NEQ; j ++) {
			NV_Ith_S(y, i) += transformMatrix[i][j] * initialConditionExpressions[j]->evaluateVector(paramValues);
		}
	}

	// Call IDAMalloc to set up problem memory.
	// Scalar relative tolerance, scalar absolute tolerance...
	int flag = 0;
	int ToleranceType = IDA_SS;
	if (solver == 0) {
		solver = IDACreate();
		if (solver == 0) {
			throw "Out of memory";
		}
		flag = IDAMalloc(solver, Residual_callback, STARTING_TIME, y, yp, ToleranceType, RelativeTolerance, &AbsoluteTolerance);
	}  else {
		flag = IDAReInit(solver, Residual_callback, STARTING_TIME, y, yp, ToleranceType, RelativeTolerance, &AbsoluteTolerance);
	}
	checkIDAFlag(flag);

	// set non-default solver options (in this case the "this" pointer to include in callbacks)
	//IDASetErrHandlerFn(ida_mem, IDAErrHandlerFn ehfun, eh_data);
	IDASetRdata(solver, this);
	IDASetMaxStep(solver, maxTimeStep);	

	// choose the linear solver (Dense "direct" matrix LU decomposition solver).
	flag = IDADense(solver, NEQ);
	checkIDAFlag(flag);

	// calculate initial condition for YA and YDP given YD
	// time
	realtype tout1 = min(ENDING_TIME, STARTING_TIME + 2 * maxTimeStep + (1e-15));
	if (outputTimes.size() > 0){
		tout1 = outputTimes[0];
	}

	//cout << "before IDACalcIC" << endl;
	//DEBUG_PRINT

	IDASetId(solver, id); // used with IDACalcIC( IDA_YA_YDP_INIT ).
	flag = IDACalcIC(solver, IDA_YA_YDP_INIT, tout1);
	checkIDAFlag(flag);
	flag = IDAGetConsistentIC(solver, y, yp);
	checkIDAFlag(flag);

	IDASetMaxNumSteps(solver, 5000);

	//cout << "aftere IDACalcIC" << endl;
	//DEBUG_PRINT
}

void VCellIDASolver::idaSolve(bool bPrintProgress, FILE* outputFile, void (*checkStopRequested)(double, long)) {	
	if (checkStopRequested != 0) {
		checkStopRequested(STARTING_TIME, 0);
	}

	realtype Time = STARTING_TIME;

	// write initial conditions
	writeData(Time, outputFile);
	
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
			IDASetStopTime(solver, tstop);
			int returnCode = IDASolve(solver, ENDING_TIME, &Time, y, yp, IDA_ONE_STEP_TSTOP);
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
					writeData(Time, outputFile);
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
				IDASetStopTime(solver, tstop);
				int returnCode = IDASolve(solver, sampleTime, &Time, y, yp, IDA_NORMAL_TSTOP);
				iterationCount++;					
						
				// if return IDA_SUCCESS, this is an intermediate result, continue without saving data.
				if (returnCode == IDA_TSTOP_RETURN || returnCode == IDA_SUCCESS) {
					if (Time == sampleTime) {
						writeData(Time, outputFile);
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
}


// override updateTempRowData, since y values need to be transformed to the original variables.
void VCellIDASolver::updateTempRowData(double currTime) {
	tempRowData[0] = currTime; 
	for (int i = 0; i < NEQ; i ++) {
		tempRowData[i + 1] = 0;
		for (int j = 0; j < NEQ; j ++) {
			tempRowData[i + 1] += inverseTransformMatrix[i][j] * NV_Ith_S(y, j);
		}
	}	
}
