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

	virtual bool initSimulation();   // initializes to t=0
	void    setScheduler(Scheduler *scheduler);
	void iterate();          // computes 1 time step
	virtual void    update();           // copies new to old values 
	void    reset();            // initializes to t=last time step
	double  getMaxDifference();

	double  getTime_sec() { return _advanced ? (currIteration + 1) * _dT_sec : currIteration * _dT_sec; }
	void    setCurrIteration(int curriter) { currIteration = curriter; }
	int getCurrIteration() { return currIteration; }
	double  getDT_sec() { return _dT_sec; }
	void    setDT_sec(double dT) { _dT_sec = dT; }
	virtual void advanceTimeOn();
	virtual void advanceTimeOff();

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

protected:
	int numVariables;
	int numVolVar;

	int currIteration;  // first iteration is currIteration=0

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
