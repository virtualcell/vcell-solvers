/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
/////////////////////////////////////////////////////////////
// SimTool.h
///////////////////////////////////////////////////////////
#ifndef SIMTOOL_H
#define SIMTOOL_H

#include <VCELL/Simulation.h>
#include <VCELL/SimTypes.h>

class CartesianMesh;
class Simulation;

class SimTool {
public:
	static bool bStopSimulation;
	static bool bSimZip;

	SimTool(char *classname);
	~SimTool();
	virtual void setSimulation(Simulation* sim) { simulation = sim; };
	virtual void start();
	virtual void startSteady(double tolerance, double maxTime);
	virtual void loadFinal();

	void setTimeStep(double period) { if (simulation) simulation->setDT_sec(period); }
	void setEndTimeSec(double timeSec) { simEndTime = timeSec; }
	void setKeepEvery(int ke) { keepEvery = ke; }
	void setBaseFilename(char *fname); 
	void setStoreEnable(bool enable) { bStoreEnable = enable; }
	void setFileCompress(bool compress) { bSimFileCompress = compress; }

private:
	void updateLog(double progress,double time,int iteration);
	void clearLog();
	int	getZipCount(char* zipFileName);

	bool bSimFileCompress;
	double simEndTime;
	int currIteration;
	int keepEvery;
	bool bStoreEnable;
	Simulation  *simulation;
	char* baseFileName;
	int simFileCount;
	char* baseSimName;
	char* baseDirName;
	int zipFileCount;
};

SimTool *getSimTool();

#endif
