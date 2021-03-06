/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/DataProcessorRoiTimeSeries.h>
#include <VCELL/Simulation.h>
#include <VCELL/SimTool.h>
#include <VCELL/DataSet.h>
#include <VCELL/FieldData.h>
#include <VCELL/VolumeVariable.h>
#include <VCELL/CartesianMesh.h>
#include <VCELL/FVDataSet.h>
#include <VCELL/Element.h>
#include <VCELL/Feature.h>
#include <VCELL/Membrane.h>
#include <VCELL/FVUtils.h>
#include <OdeResultSet.h>
#include <netcdfcpp.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <string.h>

#include <sstream>
using namespace std;

DataProcessorRoiTimeSeries::DataProcessorRoiTimeSeries(string& name, string& input): DataProcessor(name, input) {
	numImageRegions = 0;
	imageRegionVolumes = 0;
	
	sampleImage = 0;
	odeResultSet = 0;
}

DataProcessorRoiTimeSeries::~DataProcessorRoiTimeSeries() {
	delete sampleImage;
	delete[] imageRegionVolumes;

	for (int i = 0; i < SimTool::getInstance()->getSimulation()->getNumVariables()*2; i ++) {
		delete odeResultSet[i];
	}
	delete[] odeResultSet;	
}

bool DataProcessorRoiTimeSeries::checkComplete(SimTool* simTool) {
	outputFileName = string(simTool->getBaseFileName()) + DATAPROCOUTPUT_EXT;
	struct stat buf;
	return !stat(outputFileName.c_str(), &buf);
}

void DataProcessorRoiTimeSeries::onStart(SimTool* simTool) {
	outputFileName = string(simTool->getBaseFileName()) + DATAPROCOUTPUT_EXT;
	Simulation* sim = simTool->getSimulation();
	int numVar = sim->getNumVariables();
	if (odeResultSet == 0) {
		parseInput(simTool);

		odeResultSet = new OdeResultSet*[numVar*2];
		// for convolved variables
		for (int i = 0; i < numVar; i ++) {
			// average and total count
			odeResultSet[i] = new OdeResultSet();
			odeResultSet[numVar + i] = new OdeResultSet();

			odeResultSet[i]->addColumn(string("tc"));
			odeResultSet[numVar + i]->addColumn(string("avg"));
			Variable* var = sim->getVariable(i);
			if (var->getVarType() == VAR_VOLUME || var->getVarType() == VAR_VOLUME_REGION) {
				for (int j = 0; j < numImageRegions; j ++) {
					char p[30];
					sprintf(p, "tc%d\0", j);
					odeResultSet[numVar + i]->addColumn(string(p));
					sprintf(p, "avg%d\0", j);
					odeResultSet[i]->addColumn(string(p));
				}
			}
		}	
	} else {
		timeArray.clear();
		for (int i = 0; i < numVar*2; i ++) {
			odeResultSet[i]->clearData();
		}
	}
}

void DataProcessorRoiTimeSeries::parseInput(SimTool* simTool) {
	stringstream ss(input);
	string token;
	string fieldname;
	
	while (!ss.eof()) {
		token="";
		ss >> token;
		if (token == "StoreEnabled") {
			ss >> token;
			if (token == "false") {
				simTool->setStoreEnable(false);
			}
			getline(ss, token);
			//loadSampleImage(simTool, vcdataID, varName, time);			
		} else if (token == "SampleImage") {
			ss >> numImageRegions;
			ss >> token;
			getline(ss, fieldname);
			//loadSampleImage(simTool, vcdataID, varName, time);			
		} else if (token == "SampleImageFile") {			
			string varName;
			double time;
			ss >> varName >> time;
			string fieldfilename;
			getline(ss, fieldfilename);
			trimString(fieldfilename);
			sampleImage = new FieldData(0, VAR_VOLUME, "", fieldname, varName, time, fieldfilename);
		}
	}
}

void DataProcessorRoiTimeSeries::onWrite(SimTool* simTool) {
	Simulation* sim = simTool->getSimulation();
	int numVar = sim->getNumVariables();

	timeArray.push_back(sim->getTime_sec());

	CartesianMesh* mesh = (CartesianMesh *)sim->getMesh(); 

	int meshX = mesh->getNumVolumeX();
	int meshY = mesh->getNumVolumeY();
	int meshZ = mesh->getNumVolumeZ();

	int imgX = meshX;
	int imgY = meshY;
	int imgZ = meshZ;

	imageRegionVolumes = 0;
	if (sampleImage != 0 && imageRegionVolumes == 0) {
		imageRegionVolumes = new double[numImageRegions];
		memset(imageRegionVolumes, 0, numImageRegions * sizeof(double));
		for (int j = 0; j < imgX * imgY * imgZ; j ++) {
			int index = (int)sampleImage->getData()[j];
			if ((double)index != sampleImage->getData()[j]) {
				stringstream ss;
				ss << "DataProcessorRoiTimeSeries::onWrite(), index (" << sampleImage->getData()[j] << ") is not an integer. ";
				throw ss.str();
			}
			if (index >= numImageRegions || index < 0) {
				stringstream ss;
				ss << "DataProcessorRoiTimeSeries::onWrite(), index (" << index << ") is out of range. should be [" << 0 << "," << numImageRegions << ").";
				throw ss.str();
			}
			double volume = mesh->getVolumeOfElement_cu(j);
			imageRegionVolumes[index] += volume;
		}
	}

	int numCols = numImageRegions + 1;
	double* concentrations = new double[numCols];
	double* totalMolecules = new double[numCols];			
	MembraneElement* membraneElements = mesh->getMembraneElements();
	VolumeElement* volumeElements = mesh->getVolumeElements();
	for (int i = 0; i < numVar; i ++) {
		Variable* var = sim->getVariable(i);
		memset(concentrations, 0, numCols * sizeof(double));
		memset(totalMolecules, 0, numCols * sizeof(double));
		double totalVolume = 0;

		bool bVolume = true;
		if (var->getVarType() == VAR_VOLUME) {
			for (int j = 0; j < imgX * imgY * imgZ; j ++) {
				if (var->getStructure() == 0 || var->getStructure() == volumeElements[j].getFeature()) {					
					double volume = mesh->getVolumeOfElement_cu(j);
					totalVolume += volume;

					double curr = var->getCurr()[j] * volume;// num of moles
					concentrations[0] += curr; //num of moles
					totalMolecules[0] += curr * 602.0; //num of molecules
					if (sampleImage != 0) {
						int index = (int)sampleImage->getData()[j];
						concentrations[index + 1] += curr;//num of moles
						totalMolecules[index + 1] += curr * 602.0; //num of molecules
					}
				}
			}
		} else if (var->getVarType() == VAR_VOLUME_PARTICLE) {
			for (int j = 0; j < imgX * imgY * imgZ; j ++) {
				if (var->getStructure() == 0 || var->getStructure() == volumeElements[j].getFeature()) {					
					double volume = mesh->getVolumeOfElement_cu(j);
					totalVolume += volume;

					double curr = var->getCurr()[j];//num of molecules
					concentrations[0] += curr/602;//num of moles
					totalMolecules[0] += curr; //num of molecules
					if (sampleImage != 0) {
						int index = (int)sampleImage->getData()[j];
						concentrations[index + 1] += curr/602; //num of moles	
						totalMolecules[index + 1] += curr; //num of molecules
					}
				}
			}
		} else if (var->getVarType() == VAR_VOLUME_REGION) {
			for (int j = 0; j < imgX * imgY * imgZ; j ++) {
				if (var->getStructure() == 0 || var->getStructure() == volumeElements[j].getFeature()) {
					int regionIndex = volumeElements[j].region->getIndex();					
					double volume = mesh->getVolumeOfElement_cu(j);
					totalVolume += volume;

					double curr = var->getCurr()[regionIndex] * volume;
					concentrations[0] += curr;
					totalMolecules[0] += curr * 602.0;
					if (sampleImage != 0) {
						int index = (int)sampleImage->getData()[j];
						concentrations[index + 1] += curr;	
						totalMolecules[index + 1] += curr * 602.0;
					}
				}
			}		
		} else if (var->getVarType() == VAR_MEMBRANE) {
			bVolume = false;
			for (int j = 0; j < mesh->getNumMembraneElements(); j ++) {
				if (var->getStructure() == 0 || var->getStructure() == membraneElements[j].getMembrane()) {
					double area = membraneElements[j].area;
					totalVolume += area;

					double mols = var->getCurr()[j] * area;
					concentrations[0] += mols;
					totalMolecules[0] += mols;
				}
			}
		} else if (var->getVarType() == VAR_MEMBRANE_PARTICLE) {
			bVolume = false;
			for (int j = 0; j < mesh->getNumMembraneElements(); j ++) {
				if (var->getStructure() == 0 || var->getStructure() == membraneElements[j].getMembrane()) {
					double area = membraneElements[j].area;
					totalVolume += area;

					double mols = var->getCurr()[j]; //num of molecules
					concentrations[0] += mols; //num of molecules
					totalMolecules[0] += mols; //num of molecules
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
					concentrations[0] += mols;
					totalMolecules[0] += mols;
				}
			}
		}
		// total
		odeResultSet[i]->addRow(totalMolecules);

		// average
		concentrations[0] /= totalVolume;
		if (bVolume) {
			for (int j = 0; j < numImageRegions; j ++) {
				if (imageRegionVolumes[j] != 0) {			
					concentrations[j + 1] /= imageRegionVolumes[j];
				}
			}
		}
		odeResultSet[numVar + i]->addRow(concentrations);		
	}
	delete[] concentrations;
	delete[] totalMolecules;
}

void DataProcessorRoiTimeSeries::onComplete(SimTool* simTool) {
	if (checkComplete(simTool)) { 
		// this may be called when rerunning a completed simulation.
		// the data won't be in memory.
		cout << endl << "existing final post data processor netcdf file is " << outputFileName << endl;
		return;
	}

	Simulation* sim = simTool->getSimulation();
	int numVar = sim->getNumVariables();

	NcFile outputFile(outputFileName.c_str(), NcFile::Replace);

	int numT = odeResultSet[0]->getNumRows();
	NcDim* tDim = outputFile.add_dim("timeDim", numT);
	//NcDim* vDim = outputFile.add_dim("volumeDim", numVolumePoints);
	//NcDim* mDim = numMembranePoints == 0 ? 0 : outputFile.add_dim("membraneDim", numMembranePoints);
	int numVolVarCols = numImageRegions + 1;
	int numMemVarCols = 1;
	NcDim* volVarDim = outputFile.add_dim("volRegionDim", numVolVarCols);
	NcDim* memVarDim = outputFile.add_dim("memRegionDim", numMemVarCols);

	NcVar *data = outputFile.add_var("t", ncDouble, tDim);
	double* times = new double[timeArray.size()];
	for (int i = 0; i < (int)timeArray.size(); i ++) {
		times[i] = timeArray[i];
	}
	data->put(times, numT);
	delete[] times;
	
	for (int i = 0; i < numVar; i ++) {
		Variable* var = sim->getVariable(i);
		if (var->getVarType() == VAR_VOLUME || var->getVarType() == VAR_VOLUME_REGION || var->getVarType() == VAR_VOLUME_PARTICLE) {
			NcVar *data = outputFile.add_var((var->getName() + "_total_molecule").c_str(), ncDouble, tDim, volVarDim);
			data->put(odeResultSet[i]->getRowData(), numT, numVolVarCols);
		} else if (var->getVarType() == VAR_MEMBRANE || var->getVarType() == VAR_MEMBRANE_REGION || var->getVarType() == VAR_MEMBRANE_PARTICLE) {
			NcVar *data = outputFile.add_var((var->getName() + "_total_molecule").c_str(), ncDouble, tDim, memVarDim);
			data->put(odeResultSet[i]->getRowData(), numT, numMemVarCols);
		}
	}
	for (int i = 0; i < numVar; i ++) {
		Variable* var = sim->getVariable(i);
		if (var->getVarType() == VAR_VOLUME || var->getVarType() == VAR_VOLUME_REGION || var->getVarType() == VAR_VOLUME_PARTICLE) {
			NcVar *data = outputFile.add_var((var->getName() + "_average_uM").c_str(), ncDouble, tDim, volVarDim);
			data->put(odeResultSet[numVar + i]->getRowData(), numT, numVolVarCols);
		} else if (var->getVarType() == VAR_MEMBRANE || var->getVarType() == VAR_MEMBRANE_REGION || var->getVarType() == VAR_MEMBRANE_PARTICLE) {
			NcVar *data = outputFile.add_var((var->getName() + "_average_molecules_per_um2").c_str(), ncDouble, tDim, memVarDim);
			data->put(odeResultSet[numVar + i]->getRowData(), numT, numMemVarCols);
		}
	}
	outputFile.close();	

	cout << endl << "new final post data processor netcdf file is " << outputFileName << endl;
}
