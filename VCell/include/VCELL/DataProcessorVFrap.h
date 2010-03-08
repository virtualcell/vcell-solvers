/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef DATAPROCESSORVFRAP_H
#define DATAPROCESSORVFRAP_H

#include <VCELL/DataProcessor.h>
#include <vector>
using std::vector;

class OdeResultSet;
class FieldData;

class DataProcessorVFrap : public DataProcessor
{
public:
	DataProcessorVFrap(string& name, string& input);
	~DataProcessorVFrap();

	void onStart(SimTool* simTool);
	void onWrite(SimTool* simTool);
	void onComplete(SimTool* simTool);

	bool checkComplete(SimTool* simTool);

private:
	string outputFileName;
	int* volumePoints;
	int* membranePoints;
	int numVolumePoints;
	int numMembranePoints;

	void parseInput(SimTool* simTool);
	OdeResultSet** odeResultSet;
	vector<double> timeArray;

	FieldData* sampleImage;
	int numImageRegions;
	int zSlice;

	void loadSampleImage(SimTool* simTool, string& vcdataID, string& varName, double time);
};

#endif
