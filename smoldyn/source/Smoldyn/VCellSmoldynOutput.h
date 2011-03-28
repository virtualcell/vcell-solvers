/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef VCELLSMOLDYNOUTPUT_H
#define VCELLSMOLDYNOUTPUT_H

#include "smoldyn.h"
#include <VCELL/DataSet.h>

#include <string>
using std::string;

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
	int dimension, volRegionSize;	

	double extent[3];
	double origin[3];
	char **varNames;
	FileHeader fileHeader;
	int varSize, numVars, numBlocks;
	DataBlock *dataBlock;
	double *outputData;
	int* totalCounts;
	int outputDataSize;

	DataProcessorRoiTimeSeriesSmoldyn* smoldynDataProcessor;
	bool isInSameCompartment(double *pos1, double* pos2);
	double distance2(double* pos1, double* pos2);

	friend class DataProcessorRoiTimeSeriesSmoldyn;
	
};

#endif
