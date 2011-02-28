#ifndef TIMEWEIGHTS_H
#define TIMEWEIGHTS_H

#include "Weights.h"
/* 
 * This class inherits the Weights class. Time weights has an array of
 * weights corresponding to the time series. The length is the same as
 * number of time points.
 */
class TimeWeights : public Weights{
public:
	TimeWeights(double*,int);
	~TimeWeights();
	int getNumWeights();
	WeightType getWeightType();
	double getWeightByTimeIdx(int);
	double* getWeightData();
private:
	double* weights;
	int numWeights;
} ;

#endif
