/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
/////////////////////////////////////////////////////////////
// SimTool.h
///////////////////////////////////////////////////////////
#ifndef SIMTOOL_H
#define SIMTOOL_H

#include <VCELL/Timer.h>
#include <stdio.h>

#ifndef DIRECTORY_SEPARATOR
#if ( defined(WIN32) || defined(WIN64) || defined(CYGWIN) )
#define DIRECTORY_SEPARATOR '\\'
#else
#define DIRECTORY_SEPARATOR '/'
#endif
#endif

class VCellModel;
class SimulationExpression;
class Variable;
class ChomboIF;
class ChomboSpec;

class SimTool {
public:
	static SimTool* getInstance();
	static void create();
	~SimTool();

	virtual void start();
	virtual void loadFinal();

	void setModel(VCellModel* model);
	void setSimulation(SimulationExpression* sim);
	void setTimeStep(double period);

	void setEndTimeSec(double timeSec) { simEndTime = timeSec; }
	double getEndTime() { return simEndTime; }

	void setKeepEvery(int ke) { keepEvery = ke; }
	void setKeepAtMost(int kam) { keepAtMost = kam; }
	void setBaseFilename(char *fname);
	char* getBaseFileName() {
		return baseFileName;
	}
	char* getBaseDirName() {
		return baseDirName;
	}
	void setStoreEnable(bool enable) {
		bStoreEnable = enable;
	}
	void requestNoZip();

	SimulationExpression* getSimulation() { return simulation; }
	VCellModel* getModel() { return vcellModel; }
	bool checkStopRequested();
	TimerHandle getTimerHandle(string& timerName);
	void        startTimer(TimerHandle hnd);
	void        stopTimer(TimerHandle hnd);
	double      getElapsedTimeSec(TimerHandle hnd);

	void setSolver(string& s);
	bool isChomboSemiImplicitSolver();
	bool isChomboSundialsSolver();

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

	void setSundialsMaxStep(double ms) {
		sundialsMaxStep = ms;
	}

	double getSundialsRelativeTolerance() { return sundialsRelTol; }
	double getSundialsAbsoluteTolerance() { return sundialsAbsTol; }
	double getSundialsMaxStep() { return sundialsMaxStep; }

	void setPCGRelativeErrorTolerance(double rtol) {
		pcgRelTol = rtol;
	}
	double getPCGRelativeErrorTolerance() {
		return pcgRelTol;
	}

	double getSimStartTime() { return simStartTime; }
	void setSundialsOneStepOutput() { bSundialsOneStepOutput = true; }
	bool isSundialsOneStepOutput() { return bSundialsOneStepOutput; }

	void setSerialParameterScans(int numScans, double** values);
	void setLoadFinal(bool b) {
		bLoadFinal = b;
	}

private:
	SimTool();

	FILE* lockForReadWrite();

	void updateLog(double progress,double time,int iteration);
	void clearLog();
	int	getZipCount(char* zipFileName);

	static SimTool* instance;

	bool bSimZip;
	VCellModel* vcellModel;
	SimulationExpression  *simulation;
	Timer  *_timer;

	double simEndTime, simStartTime;
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
	bool bLoadFinal;


	double sundialsRelTol, sundialsAbsTol, sundialsMaxStep;
	double pcgRelTol;

	bool bSundialsOneStepOutput;
	int keepAtMost;
};

#endif
