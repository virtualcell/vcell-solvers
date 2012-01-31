/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef DATA_GENERATOR_H
#define DATA_GENERATOR_H

#include <VCELL/SimTypes.h>
#include <string>
using std::string;

class SimulationExpression;
class Feature;

class DataGenerator
{
public:
	DataGenerator(string& name, Feature* feature);
	virtual ~DataGenerator();

	virtual void resolveReferences(SimulationExpression* sim)=0;
	virtual void computePPData(SimulationExpression* sim)=0;

	string getQualifiedName();
	VariableType getVarType() {
		return VAR_VOLUME;
	}
	long getDataSize() {
		return dataSize;
	}
	double* getData() {
		return data;
	}

	const static double double_max;
	const static double double_min;

protected:
	double* data;
	long dataSize;
	string name;
	Feature* feature;
};

#endif
