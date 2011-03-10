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
#include "Weights.h"

//#define JNI_DEBUG

ExplicitFitObjectiveFunction::ExplicitFitObjectiveFunction(
	vector<Expression*>& arg_functionExpressions,
	int* arg_funcDataColIdx,
	ParameterDescription* arg_parameterDescription,
	OdeResultSetOpt* arg_referenceData,
	void (*checkStopRequested)(double, long))
{
	parameterDescription = arg_parameterDescription;
	int numParameters = parameterDescription->getNumParameters();

	unscaled_x = new double[numParameters];
	memset(unscaled_x, 0, numParameters * sizeof(double));

	funcDataColIdx = arg_funcDataColIdx;
	int symbolCount = numParameters+1;
	string* symbols = new string[symbolCount];
	symbols[0] = arg_referenceData->getColumnName(0); // name of independent variable (e.g. "t")
	for (int i=0;i<numParameters;i++){
		symbols[i+1] = parameterDescription->getParameterName(i);
	}
	SimpleSymbolTable* symbolTable = new SimpleSymbolTable(symbols,symbolCount);
	for(int i=0; i<arg_functionExpressions.size(); i++)
	{
		functionExpressions.push_back(arg_functionExpressions.at(i));
		functionExpressions[i]->bindExpression(symbolTable);
	}
	evaluateArray = new double[numParameters+1]; // +1 is for independent variable

	referenceData = arg_referenceData;
	// check that there are more than two columns in data,(the first column is assumed to be "t");
	if (arg_referenceData->getNumColumns() < 2){
		stringstream ss;
		ss << "reference data has " << arg_referenceData->getNumColumns() << " columns, expected >= 2";
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
	for(int i=0; i<functionExpressions.size(); i++)
	{
		delete functionExpressions.at(i);
	}
	functionExpressions.clear();
	delete[] funcDataColIdx;
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
	int refNumCols = referenceData->getNumColumns();

	memcpy(evaluateArray+1,paramValues,sizeof(double) * numParameters);
	Weights* weights = referenceData->getWeights();
	bool bVarWeight = false;
	bool bTimeWeight = false;
	bool bEleWeight = false;
	// get weight type
	if(weights->getWeightType() == ELEMENTWEIGHT)
	{
		bEleWeight = true;
	}
	else if(weights->getWeightType() == VARIABLEWEIGHT)
	{
		bVarWeight = true;
	}
	else //timeWeight
	{
		bTimeWeight = true;
	}
	//weight data
	double* weightData = weights->getWeightData();
	double weight = 1; 
	//get weighted squared error (NOTE: weight data is one col less than ref data, time col(in ref data col index 0) is not weighted)
	for(int i=0; i<functionExpressions.size(); i++)//go through each function 
	{
		//get a function in the list
		Expression* tempExp = functionExpressions.at(i);
		int funcDataColumnIdx = funcDataColIdx[i];
		if(bVarWeight)//if it's variable weights
		{
			weight = weightData[funcDataColumnIdx-1];
		}
		// assuming no parameters in function for OdeResultSet functions
		int numParamForOdeResultSetFunctions = 0;
		//get the fit data from referenceData corresponding to the expression
		referenceData->getColumnData(funcDataColumnIdx, numParamForOdeResultSetFunctions, 0, dependentVarArray);
		for (int j = 0; j < refNumRows; j ++) //go through each time point
		{
			if(bEleWeight)
			{
				weight =weightData[j*(refNumCols - 1) + (funcDataColumnIdx-1)];
			}
			else if(bTimeWeight)
			{
				weight = weightData[j];
			}
			
			evaluateArray[0] = independentVarArray[j];
			double value = tempExp->evaluateVector(evaluateArray);//function value
			double diff = value - dependentVarArray[j];
			L2Error += weight * diff * diff;
		}

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