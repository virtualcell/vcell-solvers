/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/VariableStatisticsDataGenerator.h>
#include <VCELL/Variable.h>
#include <VCELL/Feature.h>
#include <VCELL/Membrane.h>
#include <string.h>
#include <algorithm>
#include <VCELL/SimulationExpression.h>

const string VariableStatisticsDataGenerator::VariableStatistics_Name = "VariableStatistics";

VariableStatisticsDataGenerator::VariableStatisticsDataGenerator() 
	: DataGenerator(VariableStatistics_Name, NULL){
}

VariableStatisticsDataGenerator::~VariableStatisticsDataGenerator() {
}

void VariableStatisticsDataGenerator::resolveReferences(SimulationExpression* sim) {
	int numVar = sim->getNumVariables();

	// compute data size
	dataSize = 0;
	string n, u;
	for (int i = 0; i < numVar; ++ i)
	{
		Variable *var = sim->getVariable(i);
		dataSize += 2;
		// mean
		n = var->getName() + "_mean";
		if (var->getVarType() == VAR_MEMBRANE || var->getVarType() == VAR_MEMBRANE_REGION)
		{
			u = "molecules.um-2";
		} 
		else
		{
			u = "uM";
		}
		m_compNames.push_back(n);
		m_compUnits.push_back(u);

		// total
		n = var->getName() + "_total";
		u = "molecules";
		m_compNames.push_back(n);
		m_compUnits.push_back(u);

#ifndef CH_MPI
		if (var->getExactErrorVariable() != NULL)
		{
			dataSize += 2;
			// l2Error
			n = var->getName() + "_l2Error";
		  u = "molecules";
			m_compNames.push_back(n);
			m_compUnits.push_back(u);

			// maxError
			n = var->getName() + "_maxError";
		  u = "molecules";
			m_compNames.push_back(n);
			m_compUnits.push_back(u);
		}
#endif
	}
	data = new double[dataSize];
	memset(data, 0, dataSize * sizeof(double));
	hdf5Rank = 1;
	hdf5Dims[0] = dataSize;
}

void VariableStatisticsDataGenerator::computePPData(SimulationExpression* sim) {
	int numVar = sim->getNumVariables();

	memset(data, 0, dataSize * sizeof(double));
	int dataCount = 0;
	for (int i = 0; i < numVar; i ++) {
		Variable* var = sim->getVariable(i);
		data[dataCount ++] = var->getMean(); // mean
		data[dataCount ++] = var->getTotal(); // total
#ifndef CH_MPI
		if (var->getExactErrorVariable() != NULL)
		{
			// l2Error, maxError, mean
			data[dataCount ++] = var->getL2Error(); // L2 error
			data[dataCount ++] = var->getMaxError(); // max error
		}
#endif
	}
}