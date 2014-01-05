/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef SIMULATIONEXPRESSION_H
#define SIMULATIONEXPRESSION_H

#include <VCELL/ChomboScheduler.h>

class MembraneVariable;
class MembraneRegionVariable;
class VolumeRegionVariable;
class SymbolTable;
class VolumeVariable;
class PostProcessingBlock;

class SimulationExpression
{
public:
	SimulationExpression();
	virtual ~SimulationExpression();

	void initSimulation();
	ChomboScheduler* getScheduler()
	{
		return _scheduler;
	}
	void setScheduler(ChomboScheduler *scheduler)
	{
		_scheduler = scheduler;
	}
	ChomboGeometry* getChomboGeometry()
	{
		return _scheduler->getChomboGeometry();
	}
	void iterate();          // computes 1 time step

	double getTime_sec();
	void setCurrIteration(int curriter) { 
		currIteration = curriter; 
	}
	int getCurrIteration() { 
		return currIteration; 
	}
	double  getDT_sec() { 
		return _dT_sec; 
	}
	void    setDT_sec(double dT) { 
		_dT_sec = dT; 
	}

	Variable* getVariable(int index);

	Variable *getVariableFromName(string& name);
	Variable *getVariableFromName(char* name);

	int getNumVariables() {
		return (int)varList.size();
	}
	
	void writeData(char* filename);

	SymbolTable* getSymbolTable() { 
		return symbolTable; 
	};
	
	void addVariable(Variable *var);

	double* getDiscontinuityTimes() { return discontinuityTimes; }
	void setDiscontinuityTimes(double* stopTimes) { discontinuityTimes=stopTimes; }

	int getNumVolVariables() { return volVarSize; }
	VolumeVariable* getVolVariable(int i) { return volVarList[i]; }

	int getNumMemVariables() { return memVarSize; }
	MembraneVariable* getMemVariable(int i) { return memVarList[i]; }

	int getNumVolRegionVariables() { return volRegionVarSize; }
	VolumeRegionVariable* getVolRegionVariable(int i) { return volRegionVarList[i]; }

	int getNumMemRegionVariables() { return memRegionVarSize; }
	MembraneRegionVariable* getMemRegionVariable(int i) { return memRegionVarList[i]; }

	int getNumMemPde() { return numMemPde; }
	int getNumVolPde() { return numVolPde; }

	void setHasTimeDependentDiffusionAdvection() { bHasTimeDependentDiffusionAdvection = true; }
	bool hasTimeDependentDiffusionAdvection() { return bHasTimeDependentDiffusionAdvection; }

	int getNumSymbols();

	char** getOutputVarNames();
	int getOutputVarCount();
	int* getOutputVarTypes();

	void setHasElliptic()
	{
		bHasElliptic = true;
	}
	bool hasElliptic()
	{
		return bHasElliptic;
	}
	void setHasParabolic()
	{
		bHasParabolic = true;
	}
	bool hasParabolic()
	{
		return bHasParabolic;
	}

	PostProcessingBlock* createPostProcessingBlock();
	PostProcessingBlock* getPostProcessingBlock();
	
private:
	int currIteration;  // first iteration is currIteration=0
  bool bHasElliptic;
	bool bHasParabolic;

	double          _dT_sec;                  // seconds
	ChomboScheduler  *_scheduler;
	vector<Variable*> varList;
	
	SymbolTable* symbolTable;
	int numSymbols;
	void createSymbolTable();

	double* discontinuityTimes;

	VolumeVariable** volVarList;
	int volVarSize;
	MembraneVariable** memVarList;
	int memVarSize;
	VolumeRegionVariable** volRegionVarList;
	int volRegionVarSize;
	MembraneRegionVariable** memRegionVarList;
	int memRegionVarSize;

	int numVolPde;
	int numMemPde;
	bool bHasTimeDependentDiffusionAdvection;

	char** outputVarNames;
	int outputVarCnt;
	int* outputVarTypes;

	PostProcessingBlock* postProcessingBlock;
};

#endif
