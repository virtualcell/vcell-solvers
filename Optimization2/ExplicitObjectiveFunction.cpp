// OptSolverLibrary.cpp : Defines the entry point for the application.
//

#include "ExplicitObjectiveFunction.h"
#include "ParameterDescription.h"
#include "Expression.h"
#include <float.h>
#include <iostream>
using namespace std;


ExplicitObjectiveFunction::ExplicitObjectiveFunction(Expression* arg_objFuncExpression, ParameterDescription* arg_parameterDescription, SymbolTable* arg_symbolTable, void (*arg_checkStopRequested)(double, long))
{
	parameterDescription = arg_parameterDescription;
	int numParameters = parameterDescription->getNumParameters();
	unscaled_x = new double[numParameters];
	memset(unscaled_x, 0, numParameters * sizeof(double));

	objFuncExpression = arg_objFuncExpression;
	objFuncExpression->bindExpression(arg_symbolTable);
	fn_checkStopRequested = arg_checkStopRequested;
	numObjFuncEvals=0;
	bestObjectiveFunctionValue = DBL_MAX;
	bestParameterValues = new double[numParameters];
	memset(bestParameterValues,0,numParameters*sizeof(double));
}

ExplicitObjectiveFunction::~ExplicitObjectiveFunction()
{
}

void ExplicitObjectiveFunction::objective(int nparams, double* x, double* f)
{
	if (fn_checkStopRequested!=0){
		fn_checkStopRequested(bestObjectiveFunctionValue,numObjFuncEvals);
	}
	parameterDescription->unscaleX(x,unscaled_x);

	*f = objFuncExpression->evaluateVector(unscaled_x);
	numObjFuncEvals++;
	if (bestObjectiveFunctionValue > *f){
		memcpy(bestParameterValues,unscaled_x,nparams*sizeof(double));
		bestObjectiveFunctionValue = *f;
	}
	std::cout << "objective[" << numObjFuncEvals << "]=" << *f << " best=" << bestObjectiveFunctionValue << " p=[";
	for (int i=0;i<nparams;i++){
		std::cout << unscaled_x[i] << " "; 
	}
	std::cout << "]" << std::endl;
}


int ExplicitObjectiveFunction::getNumObjFuncEvals() 
{ 
	return numObjFuncEvals; 
}

double ExplicitObjectiveFunction::getBestObjectiveFunctionValue() 
{ 
	return bestObjectiveFunctionValue; 
}

double* ExplicitObjectiveFunction::getBestParameterValues() 
{ 
	return bestParameterValues; 
} 