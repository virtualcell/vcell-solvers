// OptSolverLibrary.cpp : Defines the entry point for the application.
//

#include <cstring> //memset
#include "SimpleParameterDescription.h"
#include "SimpleSymbolTable.h"

#include <float.h>
#include <iostream>
using namespace std;

SimpleParameterDescription::SimpleParameterDescription(
	int arg_numParameters,	vector<string> arg_paramNames, 
	double* arg_UnscaledLB, double* arg_UnscaledUB, double* arg_UnscaledInitialGuess,
	double* arg_scales)
{
	numParameters = arg_numParameters;
	paramNames = arg_paramNames;
	unscaledLB = arg_UnscaledLB;
	unscaledUB = arg_UnscaledUB;
	unscaledInitGuess = arg_UnscaledInitialGuess;
	scales = arg_scales;

	string* paramStringNames = new string[numParameters];
	for (int i = 0; i < numParameters; i ++) {	
		paramStringNames[i] = paramNames[i];
	}
	parameterSymbolTable = new SimpleSymbolTable(paramStringNames, numParameters);
}

SimpleParameterDescription::~SimpleParameterDescription(){
	delete parameterSymbolTable;
}


void SimpleParameterDescription::getUnscaledInitialGuess(double* x) {	
	memcpy(x, unscaledInitGuess, numParameters * sizeof(double));
}

void SimpleParameterDescription::getScaledInitialGuess(double* arg_scaledInitialGuess) {	
	scaleX(unscaledInitGuess,arg_scaledInitialGuess);
}

void SimpleParameterDescription::getScales(double* x) {	
	memcpy(x, scales, numParameters * sizeof(double));
}

void SimpleParameterDescription::getUnscaledLimits(double* arg_unscaledLB, double* arg_unscaledUB) {
	memcpy(arg_unscaledLB, unscaledLB, numParameters * sizeof(double));
	memcpy(arg_unscaledUB, unscaledUB, numParameters * sizeof(double));
}

void SimpleParameterDescription::getScaledLimits(double* arg_scaledLB, double* arg_scaledUB) {
	scaleX(unscaledLB,arg_scaledLB);
	scaleX(unscaledUB,arg_scaledUB);
}

 void SimpleParameterDescription::scaleX(const double* unscaled_x, double* scaled_x) {
	for (int i = 0; i < numParameters; i ++) {
		scaled_x[i]  = unscaled_x[i] / scales[i];		
	}
}

void SimpleParameterDescription::unscaleX(const double* scaled_x, double* unscaled_x) {
	for (int i = 0; i < numParameters; i ++) {
		unscaled_x[i] = scaled_x[i] * scales[i];
	}
}

