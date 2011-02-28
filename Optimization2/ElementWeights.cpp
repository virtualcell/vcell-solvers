/* 
 * This class inherits the Weights class. Element weights has a double array of
 * weights. The total number of weights is number of cols *number of rows.
 * Assuming that the first column is for time "t". The data is stored by the order of 
 * each row in the array.
 */

#include "ElementWeights.h"

//constructor
ElementWeights::ElementWeights(double* argWeights, int argNumWeightRows, int argNumWeightCols)
{
	weights = argWeights;
	numWeightRows = argNumWeightRows;
	numWeightCols = argNumWeightCols;
}
//destructor
ElementWeights::~ElementWeights()
{
	delete[] weights;
}

int ElementWeights::getNumWeights()
{
	return numWeightRows*numWeightCols;
}

int ElementWeights::getNumWeightRows()
{
	return numWeightRows;
}

int ElementWeights::getNumWeightColumns()
{
	return numWeightCols;
}

double ElementWeights::getWeight(int index)
{
	return weights[index];
}
	
double* ElementWeights::getWeightData()
{
	return weights;
}

WeightType ElementWeights::getWeightType()
{
	return ELEMENTWEIGHT;
}