#ifndef WEIGHTS_H
#define WEIGHTS_H

enum WeightType{
	ELEMENTWEIGHT,
	TIMEWEIGHT,
	VARIABLEWEIGHT
};

class Weights
{
public:
	virtual double* getWeightData() = 0;
	virtual WeightType getWeightType() = 0;
	virtual int getNumWeights() = 0;
};

#endif