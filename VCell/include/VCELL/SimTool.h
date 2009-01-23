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
#include <VCELL/Timer.h>

#ifndef DIRECTORY_SEPARATOR
#if ( defined(WIN32) || defined(WIN64) )
#define DIRECTORY_SEPARATOR '\\'
#else
#define DIRECTORY_SEPARATOR '/'
#endif
#endif

class VCellModel;
class Mesh;
class Simulation;

class SimTool {
public:
	static SimTool* getInstance();
	static void create();
	~SimTool();

	virtual void start();
	virtual void loadFinal();

	void setModel(VCellModel* model);
	void setSimulation(Simulation* sim);
	void setTimeStep(double period);
	void setCheckSpatiallyUniform() {
		bCheckSpatiallyUniform = true;
		cout << endl << "----Stop At Spatially Uniform----" << endl;
	}
	bool isCheckingSpatiallyUniform() { return bCheckSpatiallyUniform; }
	void setEndTimeSec(double timeSec) { simEndTime = timeSec; }
	double getEndTime() { return simEndTime; }

	void setKeepEvery(int ke) { keepEvery = ke; }
	void setBaseFilename(char *fname);
	void setStoreEnable(bool enable) {
		bStoreEnable = enable;
	}
	void setFileCompress(bool compress) {
		bSimFileCompress = compress;
	}
	void requestNoZip();

	Simulation* getSimulation() { return simulation; }
	VCellModel* getModel() { return vcellModel; }
	bool checkStopRequested();
	TimerHandle getTimerHandle(string& timerName);
	void        startTimer(TimerHandle hnd);
	void        stopTimer(TimerHandle hnd);
	double      getElapsedTimeSec(TimerHandle hnd);
	virtual void showSummary(FILE *fp);

	void setSolver(string& s);
	bool isSundialsPdeSolver();

	void setDiscontinuityTimes(int num, double* times) {
		numDiscontinuityTimes = num;
		discontinuityTimes = times;
	}
	int getNumDiscontinuityTimes() { return numDiscontinuityTimes; }
	double* getDiscontinuityTimes() { return discontinuityTimes; }
	
	void setSundialsErrorTolerances(double rtol, double atol) {
		sundialsRelTol = rtol;
		sundialsAbsTol = atol;
	}

	double getSundialsRelativeTolerance() { return sundialsRelTol; }
	double getSundialsAbsoluteTolerance() { return sundialsAbsTol; }

	void setSpatiallyUniformAbsErrorTolerance(double atol) {
		spatiallyUniformAbsTol = atol;
	}

private:
	SimTool();

	bool checkSpatiallyUniform(Variable*);	
	void updateLog(double progress,double time,int iteration);
	void clearLog();
	int	getZipCount(char* zipFileName);

	static SimTool* instance;

	bool bSimZip;
	VCellModel* vcellModel;
	Simulation  *simulation;
	Timer  *_timer;

	bool bSimFileCompress;
	double simEndTime;
	bool bCheckSpatiallyUniform;
	double simDeltaTime;
	int keepEvery;
	bool bStoreEnable;
	char* baseFileName;
	int simFileCount;
	char* baseSimName;
	char* baseDirName;
	int zipFileCount;
	string solver;
	double* discontinuityTimes;
	int numDiscontinuityTimes;


	double sundialsRelTol, sundialsAbsTol;
	double spatiallyUniformAbsTol;
};

#endif
