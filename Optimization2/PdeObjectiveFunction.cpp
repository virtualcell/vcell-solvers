#include "PdeObjectiveFunction.h"
#include "SpatialReferenceData.h"
#include "ParameterDescription.h"
#include <VCELL/FVSolver.h>
#include <Exception.h>
using namespace VCell;

#include <iostream>
#include <sstream>
#include <string>
using namespace std;

PdeObjectiveFunction::PdeObjectiveFunction(ParameterDescription *arg_parameterDescription,
	SpatialReferenceData* arg_referenceData,
	vector<string>& arg_refColumnMappingExpressions, 
	const char* arg_inputChars, 
	void (*arg_checkStopRequested)(double, long))
{
	istringstream inputStream(arg_inputChars);
	fvSolver = new FVSolver(inputStream);
	parameterDescription = arg_parameterDescription;

	referenceData = arg_referenceData;
	currentSolution = new double*[referenceData->getNumVariables()];
	for (int i = 0; i < referenceData->getNumVariables(); i ++) {
		currentSolution[i] = fvSolver->getValue(referenceData->getVariable(i),1);
	}
	unscaled_x = new double[arg_parameterDescription->getNumParameters()];
	numObjFuncEvals=0;
	int numParams = arg_parameterDescription->getNumParameters();
	bestObjectiveFunctionValue = DBL_MAX;
	bestParameterValues = new double[numParams];
	memset(bestParameterValues,0,numParams*sizeof(double));
	fn_checkStopRequested = arg_checkStopRequested;
}

PdeObjectiveFunction::~PdeObjectiveFunction()
{
	delete[] unscaled_x;
	delete fvSolver;
}

void PdeObjectiveFunction::objective(int nparams, double* parameterValues, double* functionValue)
{
	numObjFuncEvals ++;
	long sampleCount=0;
	double sumSquaredError = 0;
	double currSolverTime = 0;
	parameterDescription->unscaleX(parameterValues,unscaled_x);
	bool done = false;
	int refDataTimeIndex = 0;
	double currentSolverTime = 0;
//std::cout << "calling objective[" << numObjFuncEvals << "], p=[";
//for (int i=0;i<nparams;i++){
//	std::cout << unscaled_x[i] << " "; 
//}
//std::cout << "]" << std::endl;
	try {
		fvSolver->init(unscaled_x);
		while (!done){

			double currentSolverTime=fvSolver->getCurrentTime();
			double refDataTime = referenceData->getTimePoint(refDataTimeIndex);
			while (currSolverTime<refDataTime){
				parameterDescription->unscaleX(parameterValues,unscaled_x);
				fvSolver->step(unscaled_x);
				currSolverTime = fvSolver->getCurrentTime();
			}
			//
			// compare curr refData against curr sim data
			// (initially at t=0)
			//
			int numRefDataVars = referenceData->getNumVariables();
			int numRefTimePoints = referenceData->getNumTimePoints();
			for (int refDataVarIndex=0; refDataVarIndex<numRefDataVars; refDataVarIndex++){
				const double* refData = referenceData->getData(refDataTimeIndex,refDataVarIndex);
				string& varName = fvSolver->getVariableName(refDataVarIndex);
				double* simData = fvSolver->getValue(varName,1);
				int varLength = fvSolver->getVariableLength(varName);
//std::cout << "t=" << currSolverTime << "/" << currSolverTime << " calcium=[ " << refData[0] << "/" << simData[0] << "]" << std::endl;
				//std::cout << "t=" << currSolverTime << "/" << currSolverTime << " calcium=[ ";
				for (int arrayIndex=0; arrayIndex<varLength; arrayIndex++){
					//std::cout << refData[arrayIndex] << "/" << simData[arrayIndex] << ", ";
					double error = refData[arrayIndex] - simData[arrayIndex];
					//std::cout << "([" << arrayIndex << "]" << refData[arrayIndex] << "-" << simData[arrayIndex] << "=" << error << ") ";
//if (arrayIndex==0 || arrayIndex==1 || arrayIndex == 99){
//	std::cout << "t=(" << currSolverTime << "/" << currSolverTime << ") ";
//	std::cout << "calcium[" << arrayIndex << "]: " << refData[arrayIndex] << " - " << simData[arrayIndex] << " = " << error << ") " << std::endl;
//}
					sumSquaredError += error*error;
					sampleCount++;
				}
				//std::cout << std::endl;
				//std::cout << "]" << std::endl;
			}
			//
			// go to the next refData time point 
			// and step the solver until the simulation time is close??
			//
			// THIS NEEDS MORE WORK ... TIME INTERPOLATION??
			// NOT YET TESTED.
			//
			refDataTimeIndex++;
			if (refDataTimeIndex>=numRefTimePoints){
				done = true;
				break;
			}
		}
		*functionValue = sqrt(sumSquaredError/sampleCount);

	} catch (Exception ex) {
		std::cout << "caught exception while in pdeObjectiveFunction: " << ex.getMessage() << std::endl;
		*functionValue = 1000000;
	}
	if (bestObjectiveFunctionValue > *functionValue) {
		bestObjectiveFunctionValue = *functionValue;
		memcpy(bestParameterValues, unscaled_x, parameterDescription->getNumParameters() * sizeof(double));
	}
	std::cout << "objective[" << numObjFuncEvals << "]=" << *functionValue << " best=" << bestObjectiveFunctionValue << " p=[";
	for (int i=0;i<nparams;i++){
		std::cout << unscaled_x[i] << " "; 
	}
	std::cout << "]" << std::endl;
}

int PdeObjectiveFunction::getNumObjFuncEvals()
{
	return numObjFuncEvals;
}

double PdeObjectiveFunction::getBestObjectiveFunctionValue()
{
	return bestObjectiveFunctionValue;
}

double* PdeObjectiveFunction::getBestParameterValues()
{
	return bestParameterValues;
}

void PdeObjectiveFunction::setCheckStopRequested(void (*checkStopRequested)(double, long)) {
	fn_checkStopRequested = checkStopRequested;
}