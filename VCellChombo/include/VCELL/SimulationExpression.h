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

class SimulationExpression
{
public:
	SimulationExpression();
	virtual ~SimulationExpression();

	void initSimulation();
	void setScheduler(ChomboScheduler *scheduler)
	{
		_scheduler = scheduler;
	}
	ChomboGeometry* getChomboGeometry()
	{
		return _scheduler->getChomboGeometry();
	}
	void iterate();          // computes 1 time step
	void update();           // copies new to old values 

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

	virtual void readData(char *filename);

	Variable* getVariable(int index);

	Variable *getVariableFromName(string& name);
	Variable *getVariableFromName(char* name);

	int getNumVariables() {
		return (int)varList.size();
	}
	
	void resolveReferences(); // create symbol table

	void writeData(char *filename);

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

private:
	int currIteration;  // first iteration is currIteration=0

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
};

#endif
