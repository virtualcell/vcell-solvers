// OptSolverLibrary.cpp : Defines the entry point for the application.
//

#include "OdeObjectiveFunction.h"
#include <VCellIDASolver.h>
#include <VCellCVodeSolver.h>
#include "Constraint.h"
#include "ParameterDescription.h"
#include "SimpleSymbolTable.h"
#include "OdeResultSetOpt.h"
#include "MemoryManager.h"
#include <StoppedByUserException.h>

#include <float.h>
#include <iostream>
#include <sstream>
using std::istringstream;

//#define JNI_DEBUG

OdeObjectiveFunction::OdeObjectiveFunction(
	ParameterDescription* arg_parameterDescription,
	OdeResultSetOpt* arg_referenceData, 
	vector<string>& modelMappingExpressions, 
	const char* arg_inputChars,
	void (*checkStopRequested)(double, long))
{
	parameterDescription = arg_parameterDescription;
	fn_checkStopRequested = checkStopRequested;
	int numParameters = parameterDescription->getNumParameters();
	unscaled_x = new double[numParameters];
	memset(unscaled_x, 0, numParameters * sizeof(double));

	istringstream inputStream(arg_inputChars);
	if (strstr(arg_inputChars,"ALGEBRAIC")!=0){
		sundialsSolver = new VCellIDASolver();
	}else{
		sundialsSolver = new VCellCVodeSolver();
	}
	sundialsSolver->readInput(inputStream);

	referenceData = arg_referenceData;
	bestResultSet = new OdeResultSetOpt();
	bestParameterValues = new double[numParameters];
	memset(bestParameterValues,0,numParameters*sizeof(double));

	OdeResultSet* testResultSetTemp = sundialsSolver->getResultSet();
	OdeResultSetOpt* testResultSet = new OdeResultSetOpt(testResultSetTemp);

	for (int i = 1; i < referenceData->getNumColumns(); i ++) { // suppose t is the first column
		string columnName = referenceData->getColumnName(i);
		int index = testResultSet->findColumn(columnName);
		if (index == -1) {
			testResultSet->addFunctionColumn(columnName, modelMappingExpressions[i-1].c_str());
		}
	}
	testResultSet->bindFunctionExpression(sundialsSolver->getSymbolTable());
}

OdeObjectiveFunction::~OdeObjectiveFunction(){
	delete[] bestParameterValues;
	delete bestResultSet;
	delete[] unscaled_x;
	delete sundialsSolver;
}

OdeResultSetOpt* OdeObjectiveFunction::getBestResultSet() {
	if (bestResultSet->getNumRows() == 0) {
		return testResultSet;
	}
	return bestResultSet;
}

void OdeObjectiveFunction::objective(int nparams, double* x, double* f) {
	if (fn_checkStopRequested!=0){
		fn_checkStopRequested(bestObjectiveFunctionValue,numObjFuncEvals);
	}
	numObjFuncEvals ++;
	
	try {
		parameterDescription->unscaleX(x, unscaled_x);

		sundialsSolver->solve(unscaled_x, false, 0, fn_checkStopRequested);
		*f = computeL2error(unscaled_x);
		if (bestObjectiveFunctionValue > *f) {
			bestObjectiveFunctionValue = *f;
			memcpy(bestParameterValues, unscaled_x, parameterDescription->getNumParameters() * sizeof(double));
			testResultSet->copyInto(bestResultSet);
		}
#ifdef JNI_DEBUG
		cout << "objective[" << numObjFuncEvals << "]=" << *f << " best=" << bestObjectiveFunctionValue << " p=[";
		for (int i=0;i<nparams;i++){
			std::cout << unscaled_x[i] << " "; 
		}
		std::cout << "]" << std::endl;
#endif
	} catch (VCell::Exception ex) {
		*f = 1000;
	}
}

double OdeObjectiveFunction::computeL2error(double* paramValues) {
	double L2Error = 0.0;
	int numParameters = parameterDescription->getNumParameters();
	int testNumColumns = testResultSet->getNumColumns();
	int refNumColumns = referenceData->getNumColumns();
	int testNumRows = testResultSet->getNumRows();
	int refNumRows = referenceData->getNumRows();

	int testTimeIndex = testResultSet->findColumn("t");
	if (testTimeIndex == -1) {
		throw VCell::Exception("computeL2error: No time data in the solution");
	}
	double* testTimes = new double[testNumRows];
	testResultSet->getColumnData(testTimeIndex, numParameters, paramValues, testTimes);

	int refTimeIndex = referenceData->findColumn("t");
	if (refTimeIndex == -1) {
		throw VCell::Exception("computeL2error: No time data in the reference data");
	}
	double* refTimes = new double[refNumRows];
	referenceData->getColumnData(refTimeIndex, numParameters, paramValues, refTimes);
	
	double* refData = new double[refNumRows];
	double* testData = new double[testNumRows];
	
	bool bVarWeight = false;
	bool bTimeWeight = false;
	bool bEleWeight = false;
	// get weights base on type
	if(referenceData->getWeights()->getWeightType() == TIMEWEIGHT)
	{
		bTimeWeight = true;
	}
	else if(referenceData->getWeights()->getWeightType() == VARIABLEWEIGHT)
	{
		bVarWeight = true;
	}
	else //elementWeight
	{
		bEleWeight = true;
	};
	double weight = 1;
	for (int i = 0; i < refNumColumns; i++){
		string aColumn = referenceData->getColumnName(i);
		if (aColumn == "t") {
			continue;
		}
		if(bVarWeight)
		{
			weight = ((VariableWeights*)referenceData->getWeights())->getWeightByVarIdx(i-1);//weight at column 0(for "t") is not stored
		}
		int refIndex = i;
		referenceData->getColumnData(refIndex, numParameters, paramValues, refData);
		
		int testIndex = testResultSet->findColumn(aColumn);
		if (testIndex == -1) {
			throw VCell::Exception(string("computeL2error: No data in the solution for variable ") + aColumn);
		}
		testResultSet->getColumnData(testIndex, numParameters, paramValues, testData);

		// Resampling test data
		int k = 0; 	
		for (int j = 0; j < refNumRows; j ++) {
			/*
			 * choose two points (testData[k] and testData[k+1]) in test data for 
			 * interpolation (or extrapolation if outside the data)
			 *
			 * a)  extrapolate backward (in time) for points which are before testData
			 * b)  interpolate the values that fall within the test dataset.
			 * c)  extrapolate forward (in time) for points which are after testData
			*/
			while ((k < testNumRows - 2) && (refTimes[j] >= testTimes[k + 1])) {
				k ++;
			}
			if(bTimeWeight)
			{
				weight = ((TimeWeights*)referenceData->getWeights())->getWeightByTimeIdx(j);
			}
			else if(bEleWeight)//element weight doesn't have weights for "t", elementweights has one col less than data
			{
				int idx = j*(refNumColumns-1)+(i-1);
				weight = ((ElementWeights*)referenceData->getWeights())->getWeight(idx);
			}

			/*
			 * apply first order linear basis for reference data interpolation.
			*/
			double resampledTestData = testData[k] + (testData[k+1] - testData[k]) * (refTimes[j] - testTimes[k])/(testTimes[k+1] - testTimes[k]);
			double diff = resampledTestData - refData[j];
			L2Error += weight * diff * diff;
		}
	}
	
	delete[] refTimes;
	delete[] testTimes;
	delete[] refData;
	delete[] testData;

	return L2Error;
}

int OdeObjectiveFunction::getNumObjFuncEvals() 
{ 
	return numObjFuncEvals; 
}

double OdeObjectiveFunction::getBestObjectiveFunctionValue() 
{ 
	return bestObjectiveFunctionValue; 
}

double* OdeObjectiveFunction::getBestParameterValues() 
{ 
	return bestParameterValues; 
} 

void OdeObjectiveFunction::setCheckStopRequested(void (*checkStopRequested)(double, long)) {
	fn_checkStopRequested = checkStopRequested;
}