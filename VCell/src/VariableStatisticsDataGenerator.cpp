/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/VariableStatisticsDataGenerator.h>
#include <VCELL/SimulationExpression.h>
#include <VCELL/CartesianMesh.h>
#include <VCELL/Element.h>
#include <VCELL/Variable.h>
#include <VCELL/Feature.h>
#include <VCELL/Membrane.h>
#include <Expression.h>
using VCell::Expression;

#include <algorithm>
using std::max;
using std::min;

VariableStatisticsDataGenerator::VariableStatisticsDataGenerator() 
	: DataGenerator(string("VariableStatistics")){
}

VariableStatisticsDataGenerator::~VariableStatisticsDataGenerator() {
}

void VariableStatisticsDataGenerator::resolveReferences(SimulationExpression* sim) {
	int numVar = sim->getNumVariables();

	// compute data size
	CartesianMesh* mesh = (CartesianMesh*)sim->getMesh();
	dataSize = numVar * 2;
	data = new double[dataSize];
	memset(data, 0, dataSize * sizeof(double));

	hdf5Rank = 1;
	// total and average
	hdf5Dims[0] =  dataSize;
	//hdf5Dims[0] = 1;

	//compType = new H5::CompType(size_t(sizeof(double) * dataSize));
	//for (int i = 0; i < numVar; i ++) {
	//	Variable* var = sim->getVariable(i);

	//	char compName[64];
	//	sprintf(compName, "%s_average", var->getName().c_str());
	//	compType->insertMember(compName, i * 2 * sizeof(double),  H5::PredType::NATIVE_DOUBLE);
	//	
	//	sprintf(compName, "%s_total", var->getName().c_str());
	//	compType->insertMember(compName, (i * 2  + 1) * sizeof(double),  H5::PredType::NATIVE_DOUBLE);
	//}
}

void VariableStatisticsDataGenerator::computePPData(SimulationExpression* sim) {
	int numVar = sim->getNumVariables();

	CartesianMesh* mesh = (CartesianMesh *)sim->getMesh(); 

	int meshX = mesh->getNumVolumeX();
	int meshY = mesh->getNumVolumeY();
	int meshZ = mesh->getNumVolumeZ();

	MembraneElement* membraneElements = mesh->getMembraneElements();
	VolumeElement* volumeElements = mesh->getVolumeElements();

	memset(data, 0, dataSize * sizeof(double));
	for (int i = 0; i < numVar; i ++) {
		Variable* var = sim->getVariable(i);
		double totalVolume = 0;

		double& average = data[i * 2];
		double& total = data[i * 2  + 1];
		bool bVolume = true;
		if (var->getVarType() == VAR_VOLUME) {
			for (int j = 0; j < meshX * meshY * meshZ; j ++) {
				if (var->getStructure() == 0 || var->getStructure() == volumeElements[j].getFeature()) {					
					double volume = mesh->getVolumeOfElement_cu(j);
					totalVolume += volume;

					double curr = var->getCurr()[j] * volume;// num of moles
					average += curr; //num of moles
					total += curr * 602.0; //num of molecules
				}
			}
		} else if (var->getVarType() == VAR_VOLUME_PARTICLE) {
			for (int j = 0; j < meshX * meshY * meshZ; j ++) {
				if (var->getStructure() == 0 || var->getStructure() == volumeElements[j].getFeature()) {					
					double volume = mesh->getVolumeOfElement_cu(j);
					totalVolume += volume;

					double curr = var->getCurr()[j];//num of molecules
					average += curr/602;//num of moles
					total += curr; //num of molecules
				}
			}
		} else if (var->getVarType() == VAR_VOLUME_REGION) {
			for (int j = 0; j < meshX * meshY * meshZ; j ++) {
				if (var->getStructure() == 0 || var->getStructure() == volumeElements[j].getFeature()) {
					int regionIndex = volumeElements[j].region->getIndex();					
					double volume = mesh->getVolumeOfElement_cu(j);
					totalVolume += volume;

					double curr = var->getCurr()[regionIndex] * volume;
					average += curr;
					total += curr * 602.0;
				}
			}		
		} else if (var->getVarType() == VAR_MEMBRANE) {
			bVolume = false;
			for (int j = 0; j < mesh->getNumMembraneElements(); j ++) {
				if (var->getStructure() == 0 || var->getStructure() == membraneElements[j].getMembrane()) {
					double area = membraneElements[j].area;
					totalVolume += area;

					double mols = var->getCurr()[j] * area;
					average += mols;
					total += mols;
				}
			}
		} else if (var->getVarType() == VAR_MEMBRANE_PARTICLE) {
			bVolume = false;
			for (int j = 0; j < mesh->getNumMembraneElements(); j ++) {
				if (var->getStructure() == 0 || var->getStructure() == membraneElements[j].getMembrane()) {
					double area = membraneElements[j].area;
					totalVolume += area;

					double mols = var->getCurr()[j]; //num of molecules
					average += mols; //num of molecules
					total += mols; //num of molecules
				}
			}
		} else if (var->getVarType() == VAR_MEMBRANE_REGION) {
			bVolume = false;
			for (int j = 0; j < mesh->getNumMembraneElements(); j ++) {
				if (var->getStructure() == 0 || var->getStructure() == membraneElements[j].getMembrane()) {
					int regionIndex = membraneElements[j].region->getIndex();
					double area = membraneElements[j].area;
					totalVolume += area;

					double mols = var->getCurr()[regionIndex] * area;
					average += mols;
					total += mols;
				}
			}
		}

		// average
		average /= totalVolume;
	}
}
