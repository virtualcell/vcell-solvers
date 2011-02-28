#ifndef VARIABLEWEIGHTS_H
#define VARIABLEWEIGHTS_H

#include "Weights.h"
/* 
 * This class inherits the Weights class. Varaible weights has an array of
 * weights corresponding to different variables. The length is the same as
 * number of variables.(excluding time variable 't').
 */
class VariableWeights : public Weights{
public:
	VariableWeights(double*,int);
	~VariableWeights();
	int getNumWeights();
	WeightType getWeightType();
	double getWeightByVarIdx(int);
	double* getWeightData();
private:
	double* weights;
	int numWeights;
} ;

#endif
