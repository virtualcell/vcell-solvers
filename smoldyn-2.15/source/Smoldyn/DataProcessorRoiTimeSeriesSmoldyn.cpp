/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include "DataProcessorRoiTimeSeriesSmoldyn.h"
#include <OdeResultSet.h>
#include <VCELL/FieldData.h>
#include "VCellSmoldynOutput.h"
#include <netcdfcpp.h>

#include <sys/types.h>
#include <sys/stat.h>

#include <sstream>
using namespace std;

DataProcessorRoiTimeSeriesSmoldyn::DataProcessorRoiTimeSeriesSmoldyn(VCellSmoldynOutput* output, string& name, string& input) {
	vcellSmoldynOutput = output;

	numImageRegions = 0;	
	sampleImage = 0;
	odeResultSet = 0;
	bStoreEnabled = true;

	parseInput(input);
}

DataProcessorRoiTimeSeriesSmoldyn::~DataProcessorRoiTimeSeriesSmoldyn() {
	delete sampleImage;

	for (int i = 0; i < vcellSmoldynOutput->numVars*2; i ++) {
		delete odeResultSet[i];
	}
	delete[] odeResultSet;
}

void DataProcessorRoiTimeSeriesSmoldyn::onStart() {
	if (odeResultSet == 0) {
		odeResultSet = new OdeResultSet*[vcellSmoldynOutput->numVars*2];
		for (int i = 0; i < vcellSmoldynOutput->numVars; i ++) {
			odeResultSet[i] = new OdeResultSet();
			odeResultSet[vcellSmoldynOutput->numVars + i] = new OdeResultSet();
			for (int j = 0; j < numImageRegions; j ++) {
				char p[30];
				sprintf(p, "avg%d\0", j);
				odeResultSet[i]->addColumn(string(p));
				sprintf(p, "tc%d\0", j);
				odeResultSet[vcellSmoldynOutput->numVars + i]->addColumn(string(p));
			}
		}	
	} else {
		timeArray.clear();
		for (int i = 0; i < vcellSmoldynOutput->numVars*2; i ++) {
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

void DataProcessorRoiTimeSeriesSmoldyn::parseInput(string& input) {
	molssptr mols = vcellSmoldynOutput->smoldynSim->mols;

	stringstream ss(input);
	string token;
	string fieldname;
	
	while (!ss.eof()) {
		token="";
		ss >> token;
		if (token == "StoreEnabled") {
			ss >> token;
			if (token == "false") {
				bStoreEnabled = false;
			}
			getline(ss, token);
		} else if (token == "SampleImage") {
			ss >> numImageRegions;
			ss >> token;
			getline(ss, fieldname);
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

void DataProcessorRoiTimeSeriesSmoldyn::onWrite() {
	timeArray.push_back(vcellSmoldynOutput->smoldynSim->time);

	int imgX = sampleImage->getSizeX();
	int imgY = sampleImage->getSizeY();
	int imgZ = sampleImage->getSizeZ();

	int* count = new int[numImageRegions];
	memset(count, 0, numImageRegions * sizeof(int));
	for (int j = 0; j < imgX * imgY * imgZ; j ++) {
		int index = (int)sampleImage->getData()[j];
		if ((double)index != sampleImage->getData()[j]) {
			stringstream ss;
			ss << "DataProcessorRoiTimeSeriesSmoldyn::onWrite(), index (" << sampleImage->getData()[j] << ") is not an integer. ";
			throw ss.str();
		}
		if (index >= numImageRegions || index < 0) {
			stringstream ss;
			ss << "DataProcessorRoiTimeSeriesSmoldyn::onWrite(), index (" << index << ") is out of range. should be [" << 0 << "," << numImageRegions << ").";
			throw ss.str();
		}
		count[index] ++;
	}

	double* values = new double[numImageRegions];
	double* totalCount = new double[numImageRegions];

	for (int i = 0; i < vcellSmoldynOutput->numVars; i ++) {
		memset(values, 0, numImageRegions * sizeof(double));
		memset(totalCount, 0, numImageRegions * sizeof(double));

		for (int j = 0; j < imgX * imgY * imgZ; j ++) {
			int index = (int)sampleImage->getData()[j];
			values[index] += vcellSmoldynOutput->outputData[i * vcellSmoldynOutput->varSize + j];
			totalCount[index] = values[index];
		}
		for (int j = 0; j < numImageRegions; j ++) {
			if (count[j] != 0) {			
				values[j] /= count[j];
			}
		}
		odeResultSet[i]->addRow(values);
		odeResultSet[vcellSmoldynOutput->numVars + i]->addRow(totalCount);
	}
	delete[] values;
	delete[] count;
}

void DataProcessorRoiTimeSeriesSmoldyn::onComplete(char* outputFileName) {

	NcFile outputFile(outputFileName, NcFile::Replace);

	int numT = odeResultSet[0]->getNumRows();
	NcDim* tDim = outputFile.add_dim("timeDim", numT);
	NcDim* rDim = outputFile.add_dim("regionDim", numImageRegions);

	NcVar *data = outputFile.add_var("t", ncDouble, tDim);
	double* times = new double[timeArray.size()];
	for (int i = 0; i < (int)timeArray.size(); i ++) {
		times[i] = timeArray[i];
	}
	data->put(times, numT);
	delete[] times;
	
	for (int i = 0; i < vcellSmoldynOutput->numVars; i ++) {
		char name[256];
		sprintf(name, "%s_average_molecule", vcellSmoldynOutput->varNames[i]);
		NcVar *data = outputFile.add_var(name, ncDouble, tDim, rDim);
		data->put(odeResultSet[i]->getRowData(), numT, numImageRegions);
	}
	for (int i = 0; i < vcellSmoldynOutput->numVars; i ++) {
		char name[256];
		sprintf(name, "%s_total_molecule", vcellSmoldynOutput->varNames[i]);
		NcVar *data = outputFile.add_var(name, ncDouble, tDim, rDim);
		data->put(odeResultSet[vcellSmoldynOutput->numVars + i]->getRowData(), numT, numImageRegions);
	}
	outputFile.close();	

	cout << endl << "new final post data processor netcdf file is " << outputFileName << endl;
}
