#include "OdeOptJob.h"
#include "VCellIDASolver.h"
#include "OdeResultSet.h"
#include "Exception.h"
using namespace VCell;

#include <iostream>
#include <sstream>
#include <string>
using namespace std;

OdeOptJob::OdeOptJob(int arg_numParameters, char** arg_paramNames, double* arg_LB, 
					 double* arg_UB, double* arg_initialGuess, double* arg_scaleFactors,
					 int arg_numNonLinearInequality, int arg_numLinearInequality, 
					 int arg_numNonLinearEquality, int arg_numLinearEquality, char** constraintExpressions, 
					 OdeResultSet* arg_referenceData, char** refColumnMappingExpressions, char* arg_inputChars, 
					 void (*checkStopRequested)(double, long))
: CFSQPOptJob (arg_numParameters, arg_paramNames, arg_LB, arg_UB, arg_initialGuess, arg_scaleFactors, arg_numNonLinearInequality, arg_numLinearInequality,
			   arg_numNonLinearEquality, arg_numLinearEquality, constraintExpressions, arg_referenceData, checkStopRequested) {

	istringstream inputStream(arg_inputChars);
	idaSolver = new VCellIDASolver(inputStream);

	testResultSet = idaSolver->getResultSet();
	for (int i = 1; i < referenceData->getNumColumns(); i ++) { // suppose t is the first column
		string columnName = referenceData->getColumnName(i);
		int index = testResultSet->findColumn(columnName);
		if (index == -1) {
			testResultSet->addFunctionColumn(columnName, refColumnMappingExpressions[i-1]);
		}
	}
	testResultSet->bindFunctionExpression(idaSolver->getSymbolTable());

	unscaled_x = new double[numParameters];
	memset(unscaled_x, 0, numParameters * sizeof(double));
}

OdeOptJob::~OdeOptJob(){
	delete idaSolver;
}

void OdeOptJob::objective(int nparams, double* x, double* f) {
	numObjFuncEvals ++;
	
	try {
		unscaleX(x, unscaled_x);

		idaSolver->solve(unscaled_x, 0, fn_checkStopRequested);
		*f = computeL2error(unscaled_x);
		if (bestObjectiveFunctionValue > *f) {
			bestObjectiveFunctionValue = *f;
			memcpy(bestParamterValues, unscaled_x, numParameters * sizeof(double));
			testResultSet->copyInto(bestResultSet);
		}
	} catch (Exception ex) {
		*f = 1000;
	}
}

void OdeOptJob::constraints(int nparams, int j, double* x, double* gj) {
	unscaleX(x, unscaled_x);
	*gj = constraintList.at(j - 1)->evaluate(unscaled_x);
}

double OdeOptJob::computeL2error(double* paramValues) {
	double L2Error = 0.0;

	int testNumColumns = testResultSet->getNumColumns();
	int refNumColumns = referenceData->getNumColumns();
	int testNumRows = testResultSet->getNumRows();
	int refNumRows = referenceData->getNumRows();

	int testTimeIndex = testResultSet->findColumn("t");
	if (testTimeIndex == -1) {
		throw Exception("computeL2error: No time data in the solution");
	}
	double* testTimes = new double[testNumRows];
	testResultSet->getColumnData(testTimeIndex, numParameters, paramValues, testTimes);

	int refTimeIndex = referenceData->findColumn("t");
	if (refTimeIndex == -1) {
		throw Exception("computeL2error: No time data in the reference data");
	}
	double* refTimes = new double[refNumRows];
	referenceData->getColumnData(refTimeIndex, numParameters, paramValues, refTimes);
	
	double* refData = new double[refNumRows];
	double* testData = new double[testNumRows];

	for (int i = 0; i < refNumColumns; i++){
		string aColumn = referenceData->getColumnName(i);
		if (aColumn == "t") {
			continue;
		}
		double weight = referenceData->getColumnWeight(i);
		int refIndex = i;
		referenceData->getColumnData(refIndex, numParameters, paramValues, refData);
		
		int testIndex = testResultSet->findColumn(aColumn);
		if (testIndex == -1) {
			throw Exception(string("computeL2error: No data in the solution for variable ") + aColumn);
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