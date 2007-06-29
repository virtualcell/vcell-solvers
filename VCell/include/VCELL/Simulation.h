/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef SIMULATION_H
#define SIMULATION_H

#include <stdio.h>
#include <VCELL/SimTypes.h>
#include <vector>
using namespace std;

#ifdef FINITEVOLUME_STANDALONE
#include <SimpleSymbolTable.h>
#include <ScalarValueProxy.h>

enum VAR_INDEX {VAR_VOLUME_INDEX=0, VAR_MEMBRANE_INDEX, VAR_CONTOUR_INDEX, VAR_VOLUME_REGION_INDEX, VAR_MEMBRANE_REGION_INDEX,
    VAR_CONTOUR_REGION_INDEX};

#define NUM_VAR_INDEX (VAR_CONTOUR_REGION_INDEX + 1)

class FieldData;

#endif

#define PARTICLE_ALL -1

class VolumeParticleContext;
class MembraneParticleContext;
class ContourParticleContext;
class Particle;
class VolumeVariable;
class Variable;
class Mesh;
class Solver;
class Scheduler;

class Simulation
{
public:
	Simulation(Mesh *mesh);
	~Simulation();

	bool initSimulation();   // initializes to t=0
	void    setScheduler(Scheduler *scheduler);
	void iterate();          // computes 1 time step
	void    update();           // copies new to old values 
	void    reset();            // initializes to t=last time step
	double  getMaxDifference();

	double  getTime_sec() { return _time_sec; }
	void    setTime_sec(double time) { _time_sec = time; }
	double  getDT_sec() { return _dT_sec; }
	void    setDT_sec(double dT) { _dT_sec = dT; }
	void    advanceTimeOn();
	void    advanceTimeOff();

	virtual bool writeData(char *filename, bool bCompress);
	virtual bool readData(char *filename);
	void    synchronize();

	Variable *getVariableFromName(string& name);
	Variable *getVariableFromName(char* name);
	Solver   *getSolverFromVariable(Variable *var);
	Mesh     *getMesh() { return _mesh; }

	Variable *getNextVariable(Variable *var=NULL);
	Solver   *getNextSolver(Solver *solver=NULL);

	void addParticle(Particle *particle); 
	long  getNumParticles() { return (int)globalParticleList.size(); }
        

	void addVariable(Variable *var);
	void addSolver(Solver *solver);

#ifdef FINITEVOLUME_STANDALONE
	void addFieldData(FieldData* fd) {
		fieldDataList.push_back(fd);
	}

	int getNumFields() { return (int)fieldDataList.size(); }
	string* getFieldSymbols();
	void populateFieldValues(double* darray, int index);

	int* getIndices() { return indices; };
	SimpleSymbolTable* getOldSymbolTable() { return oldSymbolTable; };
	//SimpleSymbolTable* getCurrSymbolTable() { return currSymbolTable; };
	void setCurrentCoordinate(WorldCoord& wc) {
		valueProxyX->setValue(wc.x);
		valueProxyY->setValue(wc.y);
		valueProxyZ->setValue(wc.z);
	}
#endif

protected:
	int numVariables;

#ifdef FINITEVOLUME_STANDALONE
	SimpleSymbolTable* oldSymbolTable;
	//SimpleSymbolTable* currSymbolTable;
	int* indices;
	void createSymbolTable();

	ScalarValueProxy* valueProxyTime;
	ScalarValueProxy* valueProxyX;
	ScalarValueProxy* valueProxyY;
	ScalarValueProxy* valueProxyZ;
	vector<FieldData*> fieldDataList;
#endif

	double          _time_sec;                // seconds
	double          _dT_sec;                  // seconds
	Scheduler      *_scheduler;
	Solver          *_solverList;
	Variable        *_varList;
	vector<Particle*> globalParticleList; 
	Mesh            *_mesh;
	bool          _advanced;
	bool          _initEquations;
#ifdef VCELL_MPI
	int mpiRank;
	int mpiSize;
#endif
};

#endif
