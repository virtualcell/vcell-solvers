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

#ifdef VCELL_HYBRID	
#include <smoldyn.h>
#endif

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
class DataProcessor;
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

	void createDataProcessor(string& name, string& text);
	
	void setSerialParameterScans(int numScans, double** values);
	void setLoadFinal(bool b) {
		bLoadFinal = b;
	}
	
#ifdef VCELL_HYBRID	
	void setSmoldynInputFile(string& inputfile);
#endif

private:
	SimTool();

	FILE* lockForReadWrite();

	bool checkSpatiallyUniform(Variable*);	
	void updateLog(double progress,double time,int iteration);
	void clearLog();
	int	getZipCount(char* zipFileName);
	void start1();

	static SimTool* instance;

	bool bSimZip;
	VCellModel* vcellModel;
	Simulation  *simulation;
	Timer  *_timer;

	bool bSimFileCompress;
	double simEndTime, simStartTime;
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
	bool bLoadFinal;

	SundialsSolverOptions sundialsSolverOptions;
	double spatiallyUniformAbsTol, spatiallyUniformRelTol;
	double pcgRelTol;

	bool bSundialsOneStepOutput;
	int keepAtMost;

	DataProcessor* dataProcessor;
	double** serialScanParameterValues;
	int numSerialParameterScans;

	PostProcessingHdf5Writer* postProcessingHdf5Writer;

#ifdef VCELL_HYBRID	
	simptr smoldynSim;
	string smoldynInputFile;
#endif
};

#endif
