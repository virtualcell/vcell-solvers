/* 
 * This class inherits the Weights class. Varaible weights has an array of
 * weights corresponding to different variables. The length is the same as
 * number of variables.(excluding time variable 't')
 */

#include "VariableWeights.h"

//constructor
VariableWeights::VariableWeights(double* argWeights, int argNumWeights)
{
	weights = argWeights;
	numWeights = argNumWeights;
}
//destructor
VariableWeights::~VariableWeights()
{
	delete[] weights;
}

int VariableWeights::getNumWeights()
{
	return numWeights;
}
	
double VariableWeights::getWeightByVarIdx(int varIdx)
{
	return getWeightData()[varIdx];
}
	
double* VariableWeights::getWeightData()
{
	return weights;
}
	
WeightType VariableWeights::getWeightType()
{
	return VARIABLEWEIGHT;
}
