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

	regionVolumes = 0;

	parseInput(input);
}

DataProcessorRoiTimeSeriesSmoldyn::~DataProcessorRoiTimeSeriesSmoldyn() {
	delete sampleImage;

	unsigned int numVolVars = vcellSmoldynOutput->volVariables.size();
	unsigned int numMemVars = vcellSmoldynOutput->memVariables.size();
	for (unsigned int i = 0; i < numVolVars*2 + numMemVars*2; i ++) {
		delete odeResultSet[i];
	}
	delete[] odeResultSet;
	delete[] regionVolumes;
}

void DataProcessorRoiTimeSeriesSmoldyn::onStart() {
	unsigned int numVolVars = vcellSmoldynOutput->volVariables.size();
	unsigned int numMemVars = vcellSmoldynOutput->memVariables.size();
	if (odeResultSet == 0) {		
		odeResultSet = new OdeResultSet*[numVolVars*2 + numMemVars*2];
		char colname[30];
		for (unsigned int i = 0; i < numVolVars; i ++) {
			const char* varName = vcellSmoldynOutput->volVariables[i]->name.c_str();
			odeResultSet[i] = new OdeResultSet();
			sprintf(colname, "%s_tc\0", varName);
			odeResultSet[i]->addColumn(string(colname));

			odeResultSet[numVolVars + i] = new OdeResultSet();
			sprintf(colname, "%s_avg\0", varName);
			odeResultSet[numVolVars + i]->addColumn(string(colname));

			for (int j = 0; j < numImageRegions; j ++) {
				sprintf(colname, "%s_tc_%d\0", varName, j);
				odeResultSet[i]->addColumn(string(colname));

				sprintf(colname, "%s_avg_%d\0", varName, j);
				odeResultSet[numVolVars + i]->addColumn(string(colname));
			}
		}	
		int offset = numVolVars*2;
		for (unsigned int i = 0; i < numMemVars; i ++) {
			const char* varName = vcellSmoldynOutput->memVariables[i]->name.c_str();
			odeResultSet[offset + i] = new OdeResultSet();
			sprintf(colname, "%s_tc\0", varName);
			odeResultSet[offset + i]->addColumn(string(colname));

			odeResultSet[offset + numMemVars + i] = new OdeResultSet();
			sprintf(colname, "%s_avg\0", varName);
			odeResultSet[offset + numMemVars + i]->addColumn(string(colname));
		}	
	} else {
		timeArray.clear();
		for (unsigned int i = 0; i < numVolVars*2 + numMemVars*2; i ++) {
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
	
	if (regionVolumes == 0 && numImageRegions > 0) {
		int Nx = vcellSmoldynOutput->Nx;
		int Ny = vcellSmoldynOutput->Ny;
		int Nz = vcellSmoldynOutput->Nz;
		double dx = vcellSmoldynOutput->extent[0]/(Nx-1);
		double volume = dx;
		if (vcellSmoldynOutput->dimension > 1) {
			double dy = vcellSmoldynOutput->extent[1]/(Ny-1);
			volume *= dy;
			if (vcellSmoldynOutput->dimension > 2) {
				double dz = vcellSmoldynOutput->extent[2]/(Nz-1);
				volume *= dz;
			}
		}
		regionVolumes = new double[numImageRegions];
		memset(regionVolumes, 0, numImageRegions * sizeof(int));
		for (int k = 0; k < Nz; k ++) {
			for (int j = 0; j < Ny; j ++) {
				for (int i = 0; i < Nx; i ++) {
					int volIndex = k * Nx * Ny + j * Nx + i;
					int index = (int)sampleImage->getData()[volIndex];
					if ((double)index != sampleImage->getData()[volIndex]) {
						stringstream ss;
						ss << "DataProcessorRoiTimeSeriesSmoldyn::onWrite(), index (" << sampleImage->getData()[j] << ") is not an integer. ";
						throw ss.str();
					}
					if (index >= numImageRegions || index < 0) {
						stringstream ss;
						ss << "DataProcessorRoiTimeSeriesSmoldyn::onWrite(), index (" << index << ") is out of range. should be [" << 0 << "," << numImageRegions << ").";
						throw ss.str();
					}
					double v = volume;
					if (i == 0 || i == Nx - 1) {
						v /= 2;
					}
					if (vcellSmoldynOutput->dimension > 1 && (j == 0 || j == Ny - 1)) {
						v /= 2;
					}
					if (vcellSmoldynOutput->dimension > 2 && (k == 0 || k == Nz - 1)) {
						v /= 2;
					}
					regionVolumes[index] += v;
				}
			}
		}
	}

	int numVolVars = vcellSmoldynOutput->volVariables.size();
	int numColumns = numImageRegions + 1;
	double* values = new double[numColumns];
	for (int i = 0; i < numVolVars; i ++) {
		memset(values, 0, numColumns * sizeof(double));

		for (int j = 0; j < vcellSmoldynOutput->numVolumeElements; j ++) {
			int mols = vcellSmoldynOutput->volVarOutputData[i * vcellSmoldynOutput->numVolumeElements + j];
			values[0] += mols;

			if (sampleImage != 0) {
				int index = (int)sampleImage->getData()[j];
				values[index + 1] += mols;
			}
		}
		// total count
		odeResultSet[i]->addRow(values);

		// average
		values[0] /= vcellSmoldynOutput->volVariables[i]->cmpt->volume * 602.0;
		for (int j = 0; j < numImageRegions; j ++) {
			if (regionVolumes[j] != 0) {			
				values[j + 1] /= regionVolumes[j] * 602.0;
			}
		}		
		odeResultSet[numVolVars + i]->addRow(values);
	}
	delete[] values;

	values = new double[1];
	int offset = numVolVars * 2;
	numColumns = 1;
	unsigned int numMemVars = vcellSmoldynOutput->memVariables.size();
	for (int i = 0; i < numMemVars; i ++) {
		memset(values, 0, numColumns * sizeof(double));

		for (int j = 0; j < vcellSmoldynOutput->numMembraneElements; j ++) {
			int mols = vcellSmoldynOutput->memVarOutputData[i * vcellSmoldynOutput->numMembraneElements + j];
			values[0] += mols;
		}
		// total count
		odeResultSet[offset + i]->addRow(values);

		// average
		values[0] /= vcellSmoldynOutput->memVariables[i]->srf->totarea;			
		odeResultSet[offset + numMemVars + i]->addRow(values);
	}
	delete[] values;
}

void DataProcessorRoiTimeSeriesSmoldyn::onComplete(char* outputFileName) {

	NcFile outputFile(outputFileName, NcFile::Replace);

	int numT = odeResultSet[0]->getNumRows();
	int numVolColumns = numImageRegions + 1;
	int numMemColumns = 1;
	NcDim* tDim = outputFile.add_dim("timeDim", numT);
	NcDim* volColumnDim = outputFile.add_dim("volColumnDim", numVolColumns);
	NcDim* memColumnDim = outputFile.add_dim("memColumnDim", numMemColumns);

	NcVar *data = outputFile.add_var("t", ncDouble, tDim);
	double* times = new double[timeArray.size()];
	for (int i = 0; i < (int)timeArray.size(); i ++) {
		times[i] = timeArray[i];
	}
	data->put(times, numT);
	delete[] times;
	
	char name[256];
	unsigned int numVolVars = vcellSmoldynOutput->volVariables.size();
	for (unsigned int i = 0; i < numVolVars; i ++) {
		const char* varName = vcellSmoldynOutput->volVariables[i]->name.c_str();
		sprintf(name, "%s_total_molecule", varName);
		data = outputFile.add_var(name, ncDouble, tDim, volColumnDim);
		data->put(odeResultSet[i]->getRowData(), numT, numVolColumns);

		sprintf(name, "%s_average_uM", varName);
		data = outputFile.add_var(name, ncDouble, tDim, volColumnDim);
		data->put(odeResultSet[numVolVars + i]->getRowData(), numT, numVolColumns);
	}
	unsigned int numMemVars = vcellSmoldynOutput->memVariables.size();
	int offset = numVolVars * 2;
	for (unsigned int i = 0; i < numMemVars; i ++) {
		const char* varName = vcellSmoldynOutput->memVariables[i]->name.c_str();
		sprintf(name, "%s_total_molecule", varName);
		data = outputFile.add_var(name, ncDouble, tDim, memColumnDim);
		data->put(odeResultSet[offset + i]->getRowData(), numT, numMemColumns);

		sprintf(name, "%s_average_mol_per_um2", varName);
		data = outputFile.add_var(name, ncDouble, tDim, memColumnDim);
		data->put(odeResultSet[offset + numMemVars + i]->getRowData(), numT, numMemColumns);
	}
	outputFile.close();	

	cout << endl << "new final post data processor netcdf file is " << outputFileName << endl;
}
