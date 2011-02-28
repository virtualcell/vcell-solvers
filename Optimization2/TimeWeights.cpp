/* 
 * This class inherits the Weights class. Time weights has an array of
 * weights corresponding to the time series. The length is the same as
 * number of time points.
 */

#include "TimeWeights.h"

//constructor
TimeWeights::TimeWeights(double* argWeights, int argNumWeights)
{
	weights = argWeights;
	numWeights = argNumWeights;
}
//destructor
TimeWeights::~TimeWeights()
{
	delete[] weights;
}

int TimeWeights::getNumWeights()
{
	return numWeights;
}
	
double TimeWeights::getWeightByTimeIdx(int timeIdx)
{
	return getWeightData()[timeIdx];
}
	
double* TimeWeights::getWeightData()
{
	return weights;
}

WeightType TimeWeights::getWeightType()
{
	return TIMEWEIGHT;
}