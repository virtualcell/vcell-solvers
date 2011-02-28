#ifndef ELEMENTWEIGHTS_H
#define ELEMENTWEIGHTS_H

#include "Weights.h"
/* 
 * This class inherits the Weights class. Element weights has a double array of
 * weights. The total number of weights is number of cols *number of rows.
 * Assuming that the first column is for time "t". The data is stored by the order of 
 * each row in the array.
 */
class ElementWeights : public Weights{
public:
	ElementWeights(double*, int, int);
	~ElementWeights();
	int getNumWeights();
	int getNumWeightRows();
	int getNumWeightColumns();
	double getWeight(int);
	double* getWeightData();
	WeightType getWeightType();
private:
	double* weights;
	int numWeightRows;
	int numWeightCols;
};

#endif