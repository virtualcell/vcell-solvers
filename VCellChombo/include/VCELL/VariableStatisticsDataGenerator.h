/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef VARIABLE_STATISTICS_DATA_GENERATOR_H
#define VARIABLE_STATISTICS_DATA_GENERATOR_H

#include <VCELL/DataGenerator.h>
#include <vector>
using std::vector;

class VariableStatisticsDataGenerator : public DataGenerator
{
public:
	VariableStatisticsDataGenerator();
	virtual ~VariableStatisticsDataGenerator();

	void resolveReferences(SimulationExpression* sim);
	void computePPData(SimulationExpression* sim);

	const vector<string>& getComponentNames()
	{
		return m_compNames;
	}
	const vector<string>& getComponentUnits()
	{
		return m_compUnits;
	}

private:
	static const string VariableStatistics_Name;
	vector<string> m_compNames;
	vector<string> m_compUnits;
};

#endif
