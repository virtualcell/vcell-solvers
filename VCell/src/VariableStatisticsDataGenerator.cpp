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
#include <string.h>
#include <Expression.h>
#include <VCELL/ParticleVariable.h>
using VCell::Expression;

#include <algorithm>
using std::max;
using std::min;

const string VariableStatisticsDataGenerator::VariableStatistics_Name = "VariableStatistics";

VariableStatisticsDataGenerator::VariableStatisticsDataGenerator() 
	: DataGenerator(VariableStatistics_Name, NULL){
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
		if (var->getVarType() == VAR_VOLUME) {
			for (int j = 0; j < meshX * meshY * meshZ; j ++) {
				if (var->getStructure() == 0 || var->getStructure() == volumeElements[j].getFeature()) {					
					double volume = mesh->getVolumeOfElement_cu(j);
					totalVolume += volume;

					double curr = var->getCurr()[j] * volume;// num of (1e-21) moles (=uM*um3)
					average += curr; //num of (1e-21) moles (=uM*um3)
					total += curr * 602.0; //num of molecules (602 = (molecules/um3*uM)*6.02e23)
				}
			}
		} else if (var->getVarType() == VAR_VOLUME_PARTICLE) {
			ParticleVariable* particleVar = (ParticleVariable *)var;
			for (int j = 0; j < meshX * meshY * meshZ; j ++) {
				if (var->getStructure() == 0 || var->getStructure() == volumeElements[j].getFeature()) {					
					double volume = mesh->getVolumeOfElement_cu(j);
					totalVolume += volume;

					long count = particleVar->getMoleculeCounts()[j];//num of (1e-21) moles (=uM*um3)
					average += count/602.0;//num of (1e-21) moles (=uM*um3)
					total += count; //num of molecules (602 = (molecules/um3*uM)*6.02e23)
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
			ParticleVariable* particleVar = (ParticleVariable *)var;
			for (int j = 0; j < mesh->getNumMembraneElements(); j ++) {
				if (var->getStructure() == 0 || var->getStructure() == membraneElements[j].getMembrane()) {
					double area = membraneElements[j].area;
					totalVolume += area;

					long count = particleVar->getMoleculeCounts()[j]; //num of molecules
					average += count; //num of molecules
					total += count; //num of molecules
				}
			}
		} else if (var->getVarType() == VAR_MEMBRANE_REGION) {
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
