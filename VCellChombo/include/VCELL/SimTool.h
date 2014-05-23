/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
/////////////////////////////////////////////////////////////
// SimTool.h
///////////////////////////////////////////////////////////
#ifndef SIMTOOL_H
#define SIMTOOL_H

#include <stdio.h>
#include <string>
using std::string;

#ifndef DIRECTORY_SEPARATOR
#if ( defined(WIN32) || defined(WIN64) || defined(CH_CYGWIN) )
#define DIRECTORY_SEPARATOR '\\'
#else
#define DIRECTORY_SEPARATOR '/'
#endif
#endif

#define CHOMBO_SEMIIMPLICIT_SOLVER "CHOMBO_SEMIIMPLICIT_SOLVER"

class VCellModel;
class SimulationExpression;
class Variable;
class ChomboIF;
class ChomboSpec;
class PostProcessingHdf5Writer;

class SimTool {
public:
	static SimTool* getInstance();
	static void create();
	~SimTool();

	virtual void start();

	void setModel(VCellModel* model);
	void setSimulation(SimulationExpression* sim);
	void setTimeStep(double period);

	void setEndTimeSec(double timeSec) { simEndTime = timeSec; }
	double getEndTime() { return simEndTime; }

	void setKeepEvery(int ke) { keepEvery = ke; }
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
	
	SimulationExpression* getSimulation() { return simulation; }
	VCellModel* getModel() { return vcellModel; }
	bool checkStopRequested();

	void setSolver(string& s);
	bool isChomboSemiImplicitSolver();
	bool isChomboSundialsSolver();
	
	void setChomboSpec(ChomboSpec* cs)
	{
		chomboSpec = cs;
	}

#ifdef CH_MPI
	
	static int rootRank;

	inline int getMyRank()
	{
		return myRank;
	}

	inline bool isRootRank()
	{
		return myRank == rootRank;
	}

	inline int getCommSize()
	{
		return commSize;
	}
#else
	inline bool isRootRank()
	{
		return true;
	}
#endif
	
//	void setKeepAtMost(int kam) { keepAtMost = kam; }
//	void setDiscontinuityTimes(int num, double* times) {
//		numDiscontinuityTimes = num;
//		discontinuityTimes = times;
//	}
//	int getNumDiscontinuityTimes() { return numDiscontinuityTimes; }
//	double* getDiscontinuityTimes() { return discontinuityTimes; }
//
//	void setSundialsErrorTolerances(double rtol, double atol) {
//		sundialsRelTol = rtol;
//		sundialsAbsTol = atol;
//	}
//
//	void setSundialsMaxStep(double ms) {
//		sundialsMaxStep = ms;
//	}
//
//	double getSundialsRelativeTolerance() { return sundialsRelTol; }
//	double getSundialsAbsoluteTolerance() { return sundialsAbsTol; }
//	double getSundialsMaxStep() { return sundialsMaxStep; }
//
	
//	double getPCGRelativeErrorTolerance() {
//		return pcgRelTol;
//	}
//
//	void setSundialsOneStepOutput() { bSundialsOneStepOutput = true; }
//	bool isSundialsOneStepOutput() { return bSundialsOneStepOutput; }

private:
	SimTool();

	FILE* lockForReadWrite();
	void writeData(double progress,double time,int iteration);
	void cleanupLastRun();

	static SimTool* instance;
	
	VCellModel* vcellModel;
	SimulationExpression  *simulation;
	ChomboSpec* chomboSpec;

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

#ifdef CH_MPI
	int myRank;
	int commSize;
#endif

	PostProcessingHdf5Writer* postProcessingHdf5Writer;

//	double* discontinuityTimes;
//	int numDiscontinuityTimes;
//	double sundialsRelTol, sundialsAbsTol, sundialsMaxStep;
//	double pcgRelTol;
//	bool bSundialsOneStepOutput;
//	int keepAtMost;

};

#endif
