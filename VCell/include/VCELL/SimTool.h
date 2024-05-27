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
#include <VCELL/SundialsSolverOptions.h>

#include <smoldyn.h>

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
class Variable;
class PostProcessingHdf5Writer;

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
	void setSmoldynStepMultiplier(int steps);
	void setCheckSpatiallyUniform();

	bool isCheckingSpatiallyUniform() { return bCheckSpatiallyUniform; }
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
	
	void setSundialsSolverOptions(SundialsSolverOptions& sso) {
		sundialsSolverOptions = sso;
	}

	const SundialsSolverOptions& getSundialsSolverOptions() { return sundialsSolverOptions; }

	void setSpatiallyUniformErrorTolerance(double atol, double rtol) {
		spatiallyUniformAbsTol = atol;
		spatiallyUniformRelTol = rtol;
	}

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
	void checkTaskIdLockFile();

	void setSmoldynInputFile(string& inputfile);

private:
	SimTool();

	FILE* lockForReadWrite();

	bool checkSpatiallyUniform(Variable*);	
	void updateLog(double progress,double time,int iteration);
	void clearLog();
	int	getZipCount(char* zipFileName);
	int	getZipCount(const std::string* zipFileName);
	void start1();
	void copyParticleCountsToConcentration();

	static SimTool* instance;

	bool bSimZip;
	VCellModel* vcellModel;
	Simulation  *simulation;
	Timer  *_timer;

	bool bSimFileCompress;
	double simEndTime;
	double simStartTime;
	bool bCheckSpatiallyUniform;
	double simDeltaTime;
	int smoldynStepMultiplier;
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

	SundialsSolverOptions sundialsSolverOptions;
	double spatiallyUniformAbsTol, spatiallyUniformRelTol;
	double pcgRelTol;

	bool bSundialsOneStepOutput;
	int keepAtMost;

	double** serialScanParameterValues;
	int numSerialParameterScans;

	PostProcessingHdf5Writer* postProcessingHdf5Writer;
	simptr smoldynSim;
	string smoldynInputFile;
};

#endif
