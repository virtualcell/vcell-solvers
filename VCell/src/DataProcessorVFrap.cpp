/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#if ( defined(WIN32) || defined(WIN64) )
#define INTEL
#endif

#include <VCELL/DataProcessorVFrap.h>
#include <VCELL/Simulation.h>
#include <VCELL/SimTool.h>
#include <VCELL/DataSet.h>
#include <VCELL/FieldData.h>
#include <VCELL/VolumeVariable.h>
#include <VCELL/CartesianMesh.h>
#include <OdeResultSet.h>
#include <netcdfcpp.h>

#include <sys/types.h>
#include <sys/stat.h>

#include <sstream>
using namespace std;

#define CONVOLVE_SUFFIX "_Convolved"

DataProcessorVFrap::DataProcessorVFrap(string& name, string& input): DataProcessor(name, input) {
	volumePoints = 0;
	membranePoints = 0;
	numVolumePoints = 0;
	numMembranePoints = 0;
	numImageRegions = 0;
	
	sampleImage = 0;
	odeResultSet = 0;
}

DataProcessorVFrap::~DataProcessorVFrap() {
	//delete outputFile;
	delete[] volumePoints;
	delete[] membranePoints;
	delete[] sampleImage;

	for (int i = 0; i < 2 * SimTool::getInstance()->getSimulation()->getNumVariables(); i ++) {
		delete odeResultSet[i];
	}
	delete[] odeResultSet;
}

bool DataProcessorVFrap::checkComplete(SimTool* simTool) {
	outputFileName = string(simTool->getBaseFileName()) + ".dataProcOutput";
	struct stat buf;
	return !stat(outputFileName.c_str(), &buf);
}

void DataProcessorVFrap::onStart(SimTool* simTool) {
	outputFileName = string(simTool->getBaseFileName()) + ".dataProcOutput";
	Simulation* sim = simTool->getSimulation();
	int numVar = sim->getNumVariables();
	if (odeResultSet == 0) {
		parseInput(simTool);
		odeResultSet = new OdeResultSet*[numVar*2];
		for (int i = 0; i < numVar; i ++) {
			Variable* var = sim->getVariable(i);
			odeResultSet[i] = new OdeResultSet();
			if (var->getVarType() == VAR_VOLUME) {
				for (int j = 0; j < numVolumePoints; j ++) {
					char p[30];
					sprintf(p, "%d\0", volumePoints[j]);
					odeResultSet[i]->addColumn(string(p));
				}
			} else 	if (var->getVarType() == VAR_MEMBRANE && numMembranePoints > 0) {
				for (int j = 0; j < numMembranePoints; j ++) {
					char p[30];
					sprintf(p, "%d\0", membranePoints[j]);
					odeResultSet[i]->addColumn(string(p));
				}
			}		
		}

		// for convolved variables
		for (int i = 0; i < numVar; i ++) {
			Variable* var = sim->getVariable(i);
			odeResultSet[i + numVar] = new OdeResultSet();
			for (int j = 0; j < numImageRegions; j ++) {
				char p[30];
				sprintf(p, "%d\0", j);
				odeResultSet[i + numVar]->addColumn(string(p));
			}
		}	
	} else {
		timeArray.clear();
		for (int i = 0; i < 2 * numVar; i ++) {
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

void DataProcessorVFrap::parseInput(SimTool* simTool) {
	stringstream ss(input);
	string token;
	string fieldname;
	
	while (!ss.eof()) {
		token="";
		ss >> token;
		if (token == "VolumePoints") {
			ss >> numVolumePoints;
			volumePoints = new int[numVolumePoints];
			for (int i = 0; i < numVolumePoints; i ++) {
				ss >> volumePoints[i];
			}
		} else if (token == "MembranePoints") {
			ss >> numMembranePoints;
			membranePoints = new int[numMembranePoints];
			for (int i = 0; i < numMembranePoints; i ++) {
				ss >> membranePoints[i];
			}
		} else if (token == "StoreEnabled") {
			ss >> token;
			if (token == "false") {
				simTool->setStoreEnable(false);
			}
			getline(ss, token);
			//loadSampleImage(simTool, vcdataID, varName, time);			
		} else if (token == "SampleImage") {
			ss >> numImageRegions >> zSlice;
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

void DataProcessorVFrap::onWrite(SimTool* simTool) {
	Simulation* sim = simTool->getSimulation();
	int numVar = sim->getNumVariables();

	timeArray.push_back(sim->getTime_sec());

	// write variables
	for (int i = 0; i < numVar; i ++) {
		Variable* var = sim->getVariable(i);
		if (var->getVarType() == VAR_VOLUME) {
			double* values = new double[numVolumePoints];
			for (int j = 0; j < numVolumePoints; j ++) {
				values[j] = var->getCurr(volumePoints[j]);
			}
			odeResultSet[i]->addRow(values);
			delete[] values;
		} else if (var->getVarType() == VAR_MEMBRANE && numMembranePoints > 0) {
			double* values = new double[numMembranePoints];
			for (int j = 0; j < numMembranePoints; j ++) {
				values[j] = var->getCurr(membranePoints[j]);
			}
			odeResultSet[i]->addRow(values);
			delete[] values;
		}
	}

	CartesianMesh* mesh = (CartesianMesh *)sim->getMesh(); 

	int meshX = mesh->getNumVolumeX();
	int meshY = mesh->getNumVolumeY();
	int meshZ = mesh->getNumVolumeZ();

	int imgX = meshX;
	int imgY = meshY;
	int imgZ = 1;
	
	int zoffset = zSlice * meshX * meshY;

	double* convolved_values = new double[meshX * meshY * meshZ];
	int* count = new int[numImageRegions];

	for (int i = 0; i < numVar; i ++) {
		Variable* var = sim->getVariable(i);
		double* values = new double[numImageRegions];			
		memset(values, 0, numImageRegions * sizeof(double));
		memset(count, 0, numImageRegions * sizeof(int));

		DataSet::convolve(sim, var, convolved_values);

		for (int j = 0; j < imgX * imgY; j ++) {
			int index = (int)sampleImage->getData()[j];
			if ((double)index != sampleImage->getData()[j]) {
				stringstream ss;
				ss << "DataProcessorVFrap::onWrite(), index (" << sampleImage->getData()[j] << ") is not an integer. ";
				throw ss.str();
			}
			if (index >= numImageRegions || index < 0) {
				stringstream ss;
				ss << "DataProcessorVFrap::onWrite(), index (" << index << ") is out of range. should be [" << 0 << "," << numImageRegions << ").";
				throw ss.str();
			}
			values[index] += convolved_values[zoffset + j];
			count[index] ++;			
		}
		for (int j = 0; j < numImageRegions; j ++) {
			if (count[j] != 0) {			
				values[j] /= count[j];
			}
		}
		odeResultSet[i + numVar]->addRow(values);
		delete[] values;
	}
	delete[] convolved_values;
	delete[] count;
}

void DataProcessorVFrap::onComplete(SimTool* simTool) {
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
	NcDim* vDim = outputFile.add_dim("volumeDim", numVolumePoints);
	NcDim* mDim = numMembranePoints == 0 ? 0 : outputFile.add_dim("membraneDim", numMembranePoints);
	NcDim* rDim = outputFile.add_dim("regionDim", numImageRegions);

	NcVar *data = outputFile.add_var("t", ncDouble, tDim);
	double* times = new double[timeArray.size()];
	for (int i = 0; i < (int)timeArray.size(); i ++) {
		times[i] = timeArray[i];
	}
	data->put(times, numT);
	delete[] times;
	
	for (int i = 0; i <numVar; i ++) {
		Variable* var = sim->getVariable(i);
		if (var->getVarType() == VAR_VOLUME) {
			NcVar *data = outputFile.add_var(var->getName().c_str(), ncDouble, tDim, vDim);
			data->put(odeResultSet[i]->getRowData(), numT, numVolumePoints);
		} else if (var->getVarType() == VAR_MEMBRANE && numMembranePoints > 0) {
			NcVar *data = outputFile.add_var(var->getName().c_str(), ncDouble, tDim, mDim);
			data->put(odeResultSet[i]->getRowData(), numT, numMembranePoints);
		}
	}

	// convolved variables
	for (int i = 0; i < numVar; i ++) {
		Variable* var = sim->getVariable(i);
		if (var->getVarType() == VAR_VOLUME || var->getVarType() == VAR_MEMBRANE) {
			NcVar *data = outputFile.add_var((var->getName() + CONVOLVE_SUFFIX).c_str(), ncDouble, tDim, rDim);
			data->put(odeResultSet[i + numVar]->getRowData(), numT, numImageRegions);
		}
	}
	outputFile.close();	

	cout << endl << "new final post data processor netcdf file is " << outputFileName << endl;
}
