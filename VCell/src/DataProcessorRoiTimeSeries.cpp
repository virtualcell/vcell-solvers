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
#include <OdeResultSet.h>
#include <netcdfcpp.h>

#include <sys/types.h>
#include <sys/stat.h>

#include <sstream>
using namespace std;

DataProcessorRoiTimeSeries::DataProcessorRoiTimeSeries(string& name, string& input): DataProcessor(name, input) {
	numImageRegions = 0;
	
	sampleImage = 0;
	odeResultSet = 0;
}

DataProcessorRoiTimeSeries::~DataProcessorRoiTimeSeries() {
	delete sampleImage;

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
			for (int j = 0; j < numImageRegions; j ++) {
				char p[30];
				sprintf(p, "avg%d\0", j);
				odeResultSet[i]->addColumn(string(p));
				sprintf(p, "tc%d\0", j);
				odeResultSet[numVar + i]->addColumn(string(p));
			}
		}	
	} else {
		timeArray.clear();
		for (int i = 0; i < numVar*2; i ++) {
			odeResultSet[i]->clearData();
		}
	}
}

static void trimString(string& str)
{
	string::size_type pos = str.find_last_not_of(" \r\n");
	if(pos != string::npos) {
		str.erase(pos + 1);
		pos = str.find_first_not_of(" \r\n");
		if(pos != string::npos) {
			str.erase(0, pos);
		}
	}
	else {
		str.erase(str.begin(), str.end());
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

	double* regionVolume = new double[numImageRegions];
	memset(regionVolume, 0, numImageRegions * sizeof(double));
	for (int j = 0; j < imgX * imgY * imgZ; j ++) {
		int index = (int)sampleImage->getData()[j];
		double volume = mesh->getVolumeOfElement_cu(j);
		regionVolume[index] += volume;
	}

	MembraneElement* membraneElements = mesh->getMembraneElements();
	for (int i = 0; i < numVar; i ++) {
		Variable* var = sim->getVariable(i);
		double* values = new double[numImageRegions];			
		double* totalMolecules = new double[numImageRegions];			
		memset(values, 0, numImageRegions * sizeof(double));
		memset(totalMolecules, 0, numImageRegions * sizeof(double));

		if (var->getVarType() == VAR_VOLUME) {
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
				values[index] += var->getCurr()[j] * volume;
	
				totalMolecules[index] += values[index] * 602.0;
			}
		} else if (var->getVarType() == VAR_MEMBRANE) {
			for (int j = 0; j < mesh->getNumMembraneElements(); j ++) {
				{
					int volIndex1 = membraneElements[j].vindexFeatureLo;
					int index1 = (int)sampleImage->getData()[volIndex1];
					double volume = mesh->getVolumeOfElement_cu(volIndex1);
					values[index1] += var->getCurr()[j] * volume/2;
					totalMolecules[index1] += values[index1];
				}
				{
					int volIndex2 = membraneElements[j].vindexFeatureHi;
					int index2 = (int)sampleImage->getData()[volIndex2];
					double volume = mesh->getVolumeOfElement_cu(volIndex2);
					values[index2] += var->getCurr()[j] * volume/2;
					totalMolecules[index2] += values[index2];
				}
			}
		}
		for (int j = 0; j < numImageRegions; j ++) {
			if (regionVolume[j] != 0) {			
				values[j] /= regionVolume[j];
			}
		}
		odeResultSet[i]->addRow(values);
		odeResultSet[numVar + i]->addRow(totalMolecules);
		delete[] values;
	}
	delete[] regionVolume;
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
	NcDim* rDim = outputFile.add_dim("regionDim", numImageRegions);

	NcVar *data = outputFile.add_var("t", ncDouble, tDim);
	double* times = new double[timeArray.size()];
	for (int i = 0; i < (int)timeArray.size(); i ++) {
		times[i] = timeArray[i];
	}
	data->put(times, numT);
	delete[] times;
	
	for (int i = 0; i < numVar; i ++) {
		Variable* var = sim->getVariable(i);
		if (var->getVarType() == VAR_VOLUME || var->getVarType() == VAR_MEMBRANE) {
			NcVar *data = outputFile.add_var((var->getName() + "_average_uM").c_str(), ncDouble, tDim, rDim);
			data->put(odeResultSet[i]->getRowData(), numT, numImageRegions);
		}
	}
	for (int i = 0; i < numVar; i ++) {
		Variable* var = sim->getVariable(i);
		if (var->getVarType() == VAR_VOLUME || var->getVarType() == VAR_MEMBRANE) {
			NcVar *data = outputFile.add_var((var->getName() + "_total_molecule").c_str(), ncDouble, tDim, rDim);
			data->put(odeResultSet[numVar + i]->getRowData(), numT, numImageRegions);
		}
	}
	outputFile.close();	

	cout << endl << "new final post data processor netcdf file is " << outputFileName << endl;
}
