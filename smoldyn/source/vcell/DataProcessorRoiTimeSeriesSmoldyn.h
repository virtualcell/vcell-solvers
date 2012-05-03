/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef DATAPROCESSORROITIMESERIESSMOLDYN_H
#define DATAPROCESSORROITIMESERIESSMOLDYN_H

#include "smoldyn.h"
#include <vector>
#include <string>
using std::string;
using std::vector;

class OdeResultSet;
class FieldData;
class VCellSmoldynOutput;

class DataProcessorRoiTimeSeriesSmoldyn
{
public:
	DataProcessorRoiTimeSeriesSmoldyn(VCellSmoldynOutput* output, string& name, string& input);
	~DataProcessorRoiTimeSeriesSmoldyn();

	void onStart();
	void onWrite();
	void onComplete(char* outputFileName);

	bool isStoreEnabled() {
		return bStoreEnabled;
	}

private:	
	VCellSmoldynOutput* vcellSmoldynOutput;

	void parseInput(string& input);
	OdeResultSet** odeResultSet;
	vector<double> timeArray;

	bool bStoreEnabled;
	FieldData* sampleImage;
	double* regionVolumes;
	int numImageRegions;

	void loadSampleImage(string& vcdataID, string& varName, double time);
};

#endif
