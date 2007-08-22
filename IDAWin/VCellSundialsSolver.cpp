#include "Expression.h"
#include "SimpleSymbolTable.h"
#include "Exception.h"
#include "StoppedByUserException.h"
#include "VCellSundialsSolver.h"
#include "OdeResultSet.h"
#include <assert.h>

char* trim(char* str) {	
	int leftIndex, rightIndex;
	int len = (int)strlen(str);
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

int check_flag(void *flagvalue, char *funcname, int opt) {
	int *errflag;

	/* Check if SUNDIALS function returned NULL pointer - no memory allocated */
	if (opt == 0 && flagvalue == NULL) {
		char errMsg[512];
		sprintf(errMsg, "\nSUNDIALS_ERROR: %s() failed - returned NULL pointer\n\n", funcname);
		throw Exception(errMsg);
	} else if (opt == 1) { 	/* Check if flag < 0 */
		errflag = (int *) flagvalue;
		if (*errflag < 0) {
			char errMsg[512];
			sprintf(errMsg, "\nSUNDIALS_ERROR: %s() failed with flag = %d\n\n", funcname, *errflag);
			throw Exception(errMsg);
		}
	} else if (opt == 2 && flagvalue == NULL) { /* Check if function returned NULL pointer - no memory allocated */
		char errMsg[512];
		sprintf(errMsg, "\nMEMORY_ERROR: %s() failed - returned NULL pointer\n\n", funcname);
		throw Exception(errMsg);
	}

	return(0);
}

VCellSundialsSolver::VCellSundialsSolver(istream& inputstream, bool arg_bPrintProgress) {
	bPrintProgress = arg_bPrintProgress;

	NEQ = 0;
	NPARAM = 0;
	STARTING_TIME = 0.0;
	ENDING_TIME   = 0.0;
	RelativeTolerance = 0.0;
	AbsoluteTolerance = 0.0;
	keepEvery = 0;
	maxTimeStep = 0.0;		

	initialConditionSymbolTable = 0;
	initialConditionExpressions = 0;
	values = 0;
	tempRowData = 0;

	odeResultSet = new OdeResultSet();	
	y = 0;
}

VCellSundialsSolver::~VCellSundialsSolver() {
	N_VDestroy_Serial(y);

	for (int i = 0; i < NEQ; i ++) {
		delete initialConditionExpressions[i];
	}
	delete[] initialConditionExpressions;
	
	delete[] values;
	delete[] tempRowData;
	delete initialConditionSymbolTable;
	delete odeResultSet;
}

void VCellSundialsSolver::writeData(double currTime, N_Vector y, FILE* outputFile) {
	tempRowData[0] = currTime; 
	for (int i = 0; i < NEQ; i++) { 
		tempRowData[i+1] = NV_Ith_S(y,i); 
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

void VCellSundialsSolver::printProgress(double currTime, double& percentile, double increment) {	
	if (!bPrintProgress) {
		return;
	}
	while ((STARTING_TIME + ((percentile + increment) * (ENDING_TIME - STARTING_TIME))) <= currTime) { 
		percentile += increment; 
		printf("[[[progress:%lg%%]]]", percentile*100.0); 
		fflush(stdout); 
	} 
}
