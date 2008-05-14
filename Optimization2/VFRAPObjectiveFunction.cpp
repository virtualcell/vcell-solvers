#include "VFRAPObjectiveFunction.h"
#include "SpatialReferenceData.h"
#include "ParameterDescription.h"
#include <VCELL/FVSolver.h>
#include "Exception.h"
using namespace VCell;

#include <iostream>
#include <sstream>
#include <string>
using namespace std;

VFRAPObjectiveFunction::VFRAPObjectiveFunction(ParameterDescription *arg_parameterDescription,
	SpatialReferenceData* arg_referenceData,
	SpatialReferenceData* arg_simulationData,
	void (*arg_checkStopRequested)(double, long))
{
	parameterDescription = arg_parameterDescription;

	referenceData = arg_referenceData;
	simDataD1 = arg_simulationData;

	unscaled_x = new double[arg_parameterDescription->getNumParameters()];
	numObjFuncEvals=0;
	int numParams = arg_parameterDescription->getNumParameters();
	bestObjectiveFunctionValue = DBL_MAX;
	bestParameterValues = new double[numParams];
	memset(bestParameterValues,0,numParams*sizeof(double));
	fn_checkStopRequested = arg_checkStopRequested;
}

VFRAPObjectiveFunction::~VFRAPObjectiveFunction()
{
	delete[] unscaled_x;
}

void VFRAPObjectiveFunction::objective(int nparams, double* parameterValues, double* functionValue)
{
	numObjFuncEvals ++;
	long sampleCount=0;
	double sumSquaredError = 0;
	parameterDescription->unscaleX(parameterValues,unscaled_x);
	int numRefDataTimes = referenceData->getNumTimePoints();
	int numSimDataTimes = simDataD1->getNumTimePoints();
	int lengthSimData = simDataD1->getDataSize();
	int lengthRefData = referenceData->getDataSize();
	double endSimTime = simDataD1->getTimePoint(numSimDataTimes-1);
	// look for tau, alpha, scale1, scale2
	// assuming the error function is [ data(t) - exp(t/tau)*(alpha*sim(scale1*t)+(1-alpha)*sim(scale2*t)) ]^2
	//

std::cout << "calling objective[" << numObjFuncEvals << "], p=[";
for (int i=0;i<nparams;i++){
	std::cout << unscaled_x[i] << " "; 
}
std::cout << "]" << std::endl;
//do {
//	std::cout << "hello" << std::endl;
//} while (true);

	try {
		for (int refDataTimeIndex=0; refDataTimeIndex < numRefDataTimes; refDataTimeIndex++){

			double refDataTime = referenceData->getTimePoint(refDataTimeIndex);

			const int TAU_INDEX = 0;
			const int ALPHA_INDEX = 1;
			const int SCALE1_INDEX = 2;
			const int SEPARATION_INDEX = 3;
			double tau = unscaled_x[TAU_INDEX];
			double alpha = unscaled_x[ALPHA_INDEX];
			double scale1 = unscaled_x[SCALE1_INDEX];
			double separationScale = unscaled_x[SEPARATION_INDEX];
			double scale2 = scale1*separationScale;

			double doubleSimTimeIndex1 = (numSimDataTimes==1)?(0.0):((numSimDataTimes-1) * (refDataTime*scale1) / endSimTime);
			double doubleSimTimeIndex2 = (numSimDataTimes==1)?(0.0):((numSimDataTimes-1) * (refDataTime*scale2) / endSimTime);

			int simTimeIndex1_lo = (int)floor(doubleSimTimeIndex1);
			int simTimeIndex1_hi = (int)ceil(doubleSimTimeIndex1);
			double fractSimTimeIndex1 = doubleSimTimeIndex1 - simTimeIndex1_lo;
			if (simTimeIndex1_lo >= numSimDataTimes){
				simTimeIndex1_lo = numSimDataTimes-1;
				fractSimTimeIndex1 = 0.0;
			}
			if (simTimeIndex1_hi >= numSimDataTimes){
				simTimeIndex1_hi = numSimDataTimes-1;
				fractSimTimeIndex1 = 0.0;
			}

			int simTimeIndex2_lo = (int)floor(doubleSimTimeIndex2);
			int simTimeIndex2_hi = (int)ceil(doubleSimTimeIndex2);
			double fractSimTimeIndex2 = doubleSimTimeIndex2 - simTimeIndex2_lo;
			if (simTimeIndex2_lo >= numSimDataTimes){
				simTimeIndex2_lo = numSimDataTimes-1;
				fractSimTimeIndex2 = 0.0;
			}
			if (simTimeIndex2_hi >= numSimDataTimes){
				simTimeIndex2_hi = numSimDataTimes-1;
				fractSimTimeIndex2 = 0.0;
			}

			const double* refData = referenceData->getData(refDataTimeIndex,0);
			const double* simData_1_lo = simDataD1->getData(simTimeIndex1_lo,0);
			const double* simData_1_hi = simDataD1->getData(simTimeIndex1_hi,0);
			const double* simData_2_lo = simDataD1->getData(simTimeIndex2_lo,0);
			const double* simData_2_hi = simDataD1->getData(simTimeIndex2_hi,0);

			for (int arrayIndex=0; arrayIndex<lengthRefData; arrayIndex++){
				double solution = exp(refDataTime*tau)*(simData_1_lo[arrayIndex]*(1-fractSimTimeIndex1) + simData_1_hi[arrayIndex]*(fractSimTimeIndex1));
				double error = refData[arrayIndex] - solution;
				sumSquaredError += error*error;
				sampleCount++;
			}
			refDataTimeIndex++;
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

int VFRAPObjectiveFunction::getNumObjFuncEvals()
{
	return numObjFuncEvals;
}

double VFRAPObjectiveFunction::getBestObjectiveFunctionValue()
{
	return bestObjectiveFunctionValue;
}

double* VFRAPObjectiveFunction::getBestParameterValues()
{
	return bestParameterValues;
}
