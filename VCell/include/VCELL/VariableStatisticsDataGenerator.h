/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef VARIABLE_STATISTICS_DATA_GENERATOR_H
#define VARIABLE_STATISTICS_DATA_GENERATOR_H

#include <VCELL/DataGenerator.h>

class VariableStatisticsDataGenerator : public DataGenerator
{
public:
	VariableStatisticsDataGenerator();
	virtual ~VariableStatisticsDataGenerator();

	void resolveReferences(SimulationExpression* sim);
	void computePPData(SimulationExpression* sim);

private:
	static const string VariableStatistics_Name;
};

#endif
