/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef VCELLSMOLDYNOUTPUT_H
#define VCELLSMOLDYNOUTPUT_H

#include "smoldyn.h"
#include <VCELL/DataSet.h>

#include <string>
#include <vector>
using std::string;
using std::vector;

class DataProcessorRoiTimeSeriesSmoldyn;

class VCellSmoldynOutput
{
public:
	VCellSmoldynOutput(simptr sim);
	~VCellSmoldynOutput();

	void write();
	void parseInput(char* input);	
	void parseDataProcessingInput(string& name, string& input);

private:
	
	void clearLog();
	void writeSim(char* simFileName, char* zipFileName);
	void computeOutputData();

	simptr smoldynSim;
	int simFileCount;
	int zipFileCount;
	char baseFileName[256];
	char baseSimName[256];
	int Nx, Ny, Nz;
	int numVolumeElements;
	int numMembraneElements;
	int dimension;	

	double extent[3];
	double origin[3];
	VariableType* varTypes;
	vector<char*> volVarNames;
	vector<char*> memVarNames;
	FileHeader fileHeader;
	DataBlock *dataBlock;
	double *volVarOutputData;
	double *memVarOutputData;
	int* molIdentVarIndexMap;

	DataProcessorRoiTimeSeriesSmoldyn* smoldynDataProcessor;
	bool isInSameCompartment(double *pos1, double* pos2);
	double distance2(double* pos1, double* pos2);

	friend class DataProcessorRoiTimeSeriesSmoldyn;
	
};

#endif
