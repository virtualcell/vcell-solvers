// OptSolverLibrary.cpp : Defines the entry point for the application.
//
#include "ExplicitFitObjectiveFunction.h"

#include <float.h>
#include <iostream>
#include <sstream>
using namespace std;

#include <SimpleSymbolTable.h>
#include <StoppedByUserException.h>
#include <Expression.h>
using VCell::Expression;

#include <VCellIDASolver.h>
#include <VCellCVodeSolver.h>
#include "OdeResultSetOpt.h"

#include "Constraint.h"
#include "ParameterDescription.h"
#include "MemoryManager.h"

//#define JNI_DEBUG

ExplicitFitObjectiveFunction::ExplicitFitObjectiveFunction(
	Expression* arg_functionExpression,
	ParameterDescription* arg_parameterDescription,
	OdeResultSetOpt* arg_referenceData,
	void (*checkStopRequested)(double, long))
{
	parameterDescription = arg_parameterDescription;
	int numParameters = parameterDescription->getNumParameters();

	unscaled_x = new double[numParameters];
	memset(unscaled_x, 0, numParameters * sizeof(double));

	functionExpression = arg_functionExpression;
	int symbolCount = numParameters+1;
	string* symbols = new string[symbolCount];
	symbols[0] = arg_referenceData->getColumnName(0); // name of independent variable (e.g. "t")
	for (int i=0;i<numParameters;i++){
		symbols[i+1] = parameterDescription->getParameterName(i);
	}
	SimpleSymbolTable* symbolTable = new SimpleSymbolTable(symbols,symbolCount);
	functionExpression->bindExpression(symbolTable);
	evaluateArray = new double[numParameters+1]; // +1 is for independent variable

	referenceData = arg_referenceData;
	// check that there are only two columns in data;
	if (arg_referenceData->getNumColumns() != 2){
		stringstream ss;
		ss << "reference data has " << arg_referenceData->getNumColumns() << " columns, expected 2";
		throw ss.str();
	}
	independentVarArray = new double[referenceData->getNumRows()];
	dependentVarArray = new double[referenceData->getNumRows()];
	// assuming no parameters in function for OdeResultSet functions
	int numParamForOdeResultSetFunctions = 0;
	referenceData->getColumnData(0, numParamForOdeResultSetFunctions, 0, independentVarArray);
	referenceData->getColumnData(1, numParamForOdeResultSetFunctions, 0, dependentVarArray);

	bestParameterValues = new double[numParameters];
	memset(bestParameterValues,0,numParameters*sizeof(double));

	fn_checkStopRequested = checkStopRequested;
}

ExplicitFitObjectiveFunction::~ExplicitFitObjectiveFunction(){
	delete[] bestParameterValues;
	delete[] unscaled_x;
	delete[] independentVarArray;
	delete[] dependentVarArray;
	delete[] evaluateArray;
	delete[] symbolTable;
}

//nparams: number of parameters. double* x: parameter values, double* functionValue: function value
void ExplicitFitObjectiveFunction::objective(int nparams, double* x, double* f) {
	if (fn_checkStopRequested!=0){
		fn_checkStopRequested(bestObjectiveFunctionValue,numObjFuncEvals);
	}
	numObjFuncEvals ++;
	
	try {
		parameterDescription->unscaleX(x, unscaled_x);

		*f = computeL2error(unscaled_x);
		if (bestObjectiveFunctionValue > *f) {
			bestObjectiveFunctionValue = *f;
			memcpy(bestParameterValues, unscaled_x, parameterDescription->getNumParameters() * sizeof(double));
		}
#ifdef JNI_DEBUG
		cout << "objective[" << numObjFuncEvals << "]=" << *f << " best=" << bestObjectiveFunctionValue << " p=[";
		for (int i=0;i<nparams;i++){
			std::cout << unscaled_x[i] << " "; 
		}
		std::cout << "]" << std::endl;
#endif
	} catch (VCell::Exception ex) {
		cout << "ExplicitFitObjectiveFunction::objective " << ex.getMessage() << endl;
		*f = 1000;
	}
}

double ExplicitFitObjectiveFunction::computeL2error(double* paramValues) {
	double L2Error = 0.0;
	int numParameters = parameterDescription->getNumParameters();
	int refNumRows = referenceData->getNumRows();
	if(referenceData->getWeights()->getWeightType() != TIMEWEIGHT)
	{
		throw "Time Weights should be used for computeL2error of ExplicitFitObjectiveFunction";
	}
	
	double * timeWeights = referenceData->getWeights()->getWeightData();
	memcpy(evaluateArray+1,paramValues,sizeof(double) * numParameters);
	for (int j = 0; j < refNumRows; j ++) {
		evaluateArray[0] = independentVarArray[j];
		double value = functionExpression->evaluateVector(evaluateArray);
		double diff = value - dependentVarArray[j];
		L2Error += timeWeights[j]*diff * diff;
	}
	
	return L2Error;
}

int ExplicitFitObjectiveFunction::getNumObjFuncEvals() 
{ 
	return numObjFuncEvals; 
}

double ExplicitFitObjectiveFunction::getBestObjectiveFunctionValue() 
{ 
	return bestObjectiveFunctionValue; 
}

double* ExplicitFitObjectiveFunction::getBestParameterValues() 
{ 
	return bestParameterValues; 
} 

void ExplicitFitObjectiveFunction::setCheckStopRequested(void (*checkStopRequested)(double, long)) {
	fn_checkStopRequested = checkStopRequested;
}