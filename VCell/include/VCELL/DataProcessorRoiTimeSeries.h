/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef DATAPROCESSORROITIMESERIES_H
#define DATAPROCESSORROITIMESERIES_H

#include <VCELL/DataProcessor.h>
#include <vector>
using std::vector;

class OdeResultSet;
class FieldData;

class DataProcessorRoiTimeSeries : public DataProcessor
{
public:
	DataProcessorRoiTimeSeries(string& name, string& input);
	~DataProcessorRoiTimeSeries();

	void onStart(SimTool* simTool);
	void onWrite(SimTool* simTool);
	void onComplete(SimTool* simTool);

	bool checkComplete(SimTool* simTool);

private:
	string outputFileName;

	void parseInput(SimTool* simTool);
	OdeResultSet** odeResultSet;
	vector<double> timeArray;

	FieldData* sampleImage;
	int numImageRegions;

	void loadSampleImage(SimTool* simTool, string& vcdataID, string& varName, double time);
};

#endif
