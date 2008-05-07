/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/SimTypes.h>
#include <VCELL/Mesh.h>
#include <VCELL/Element.h>
#include <VCELL/Feature.h>
#include <VCELL/Variable.h>
#include <VCELL/Solver.h>
#include <VCELL/Simulation.h>
#include <VCELL/EqnBuilder.h>
#include <VCELL/DataSet.h>
#include <VCELL/VCellModel.h>
#include <VCELL/ParticleContext.h>
#include <VCELL/SimTool.h>
#include <VCELL/CartesianMesh.h>
#include <VCELL/FVUtils.h>
#include <VCELL/SerialScheduler.h>
#ifdef VCELL_MPI     
#include <VCELL/DomainPDEScheduler.h>
#include "mpi.h"
#endif

Simulation::Simulation(Mesh *mesh) 
{
	ASSERTION(mesh);
	// 
	// initialize size of voxel array to 0
	//
	_dT_sec = 0;   // seconds
	currIteration = 0;
	_mesh = mesh;
	_advanced = false;
	_initEquations = false;
	_scheduler = NULL;
	globalParticleList.clear();
   
#ifdef VCELL_MPI
	MPI_Comm_rank(MPI_COMM_WORLD, &mpiRank);
	MPI_Comm_size(MPI_COMM_WORLD, &mpiSize);
#endif
}

Simulation::~Simulation() 
{
	delete _scheduler;
}

void Simulation::advanceTimeOn() {
	if (_advanced) {
		throw "Simulation::advanceTimeOn() : time has already been advanced on";
	}
	_advanced=true;	
}

void Simulation::advanceTimeOff() {
	if (!_advanced) {
		throw "Simulation::advanceTimeOff() : time has already been advanced off";
	}
	_advanced=false;	 
}
   
void Simulation::iterate()
{
	_scheduler->iterate();
}

double Simulation::getMaxDifference()
{
	double maxDiff = 0;

	for (int i = 0; i < (int)varList.size(); i ++) {
		Variable* var = varList[i];
		double diff = var->getMaxDifference();
		maxDiff = max(maxDiff, diff);
	}

	return maxDiff;
}

void Simulation::update()
{
	for (int i = 0; i < (int)varList.size(); i ++) {
		Variable* var = varList[i];
		var->update();
	}
	//_scheduler->update();
	currIteration ++;
}

void Simulation::reset()
{
	currIteration=0;
	//_scheduler->resetFirstTime();
	_scheduler->initValues();
	//_scheduler->reset();
}

void Simulation::addSolver(Solver *solver)
{
	solverList.push_back(solver);
}

Solver *Simulation::getSolver(int index)
{
	if (index < 0 || index >= (int)solverList.size()) {
		throw "Simulation: getSolver(index) : index out of bounds";
	}
	return solverList.at(index);	
}

void Simulation::addVariable(Variable *var)
{
	varList.push_back(var);
}

Variable* Simulation::getVariable(int index) {
	if (index < 0 || index >= (int)varList.size()) {
		throw "Simulation: getVariable(index) : index out of bounds";
	}
	return varList.at(index);
}

int Simulation::getNumVolumeVariables() {
	int numVolVar = 0;
	for (int i = 0; i < (int)varList.size(); i ++) {
		if (varList[i]->getVarType() == VAR_VOLUME) {
			numVolVar ++;
		}
	}
	return numVolVar;
}


Variable *Simulation::getVariableFromName(string& varName)
{
	ASSERTION(varName);
	for (int i = 0; i < (int)varList.size(); i ++) {
		Variable* var = varList[i];
		if (varName == var->getName()){
			return var;
		}
	}
	return NULL;
}

Variable *Simulation::getVariableFromName(char* varName)
{
	string vn(varName);
	return getVariableFromName(vn);
}

Solver *Simulation::getSolverFromVariable(Variable *var)
{
   ASSERTION(var);
   for (int i = 0; i < (int)solverList.size(); i ++) {
		Solver* solver = solverList[i];
		if (solver->getVar()==var){
			return solver;
		}
   }
   return NULL;
}

//void Simulation::synchronize()
//{
//	//
//	// if applicable, collect results into root process
//	//
//	_scheduler->collectResults(0);
//}

void Simulation::addParticle(Particle *particle)
{ 
	globalParticleList.push_back(particle); 
}

void Simulation::writeData(char *filename, bool bCompress)
{
   
	//synchronize();

#ifdef VCELL_MPI
	//
	// only root process actually writes dataset
	//
	if (mpiRank!=0){
		return true;
	}
#endif
	bool hasParticles = false;
	VCellModel *model = SimTool::getInstance()->getModel();
	for (int i = 0; i < model->getNumFeatures(); i ++) {
		Feature* feature = model->getFeatureFromIndex(i);
		if (feature->getVolumeParticleContext()!=NULL){
			hasParticles = true;
		}
		if (feature->getMembraneParticleContext()!=NULL){
			hasParticles = true;
		}
		if (feature->getContourParticleContext()!=NULL){
			hasParticles = true;
		}
	}

	if (hasParticles){
		FILE *fp = NULL;
		string newFname = string(filename) + ".particle";
		if ((fp = fopen(newFname.c_str(),"w"))==NULL){
			char errmsg[512];
			sprintf(errmsg, "Simulation::writeData(), error opening file %s for writing", newFname.c_str());
			throw errmsg;
		}

		fprintf(fp,"%lg %lg %lg\n", ((CartesianMesh*)_mesh)->getDomainSizeX(), 
						((CartesianMesh*)_mesh)->getDomainSizeY(), 
						((CartesianMesh*)_mesh)->getDomainSizeZ()); 
		fprintf(fp,"%d %d %d\n", ((CartesianMesh*)_mesh)->getNumVolumeX(), 
					((CartesianMesh*)_mesh)->getNumVolumeY(), 
					((CartesianMesh*)_mesh)->getNumVolumeZ());

		//
		//write particle file (if needed)
		//
		vector<Particle*>::iterator particleIterator;
		for (particleIterator = globalParticleList.begin();particleIterator != globalParticleList.end();particleIterator++){
			(*particleIterator)->write(fp);
		}

		fclose(fp);
	}

	DataSet dataset;
	dataset.write(filename, this, bCompress);
}

void Simulation::readData(char *filename)
{
	//
	// all processes read data
	// (in the future, root could read and then distribute)
	//
	DataSet dataset;
	dataset.read(filename, this);
}

void Simulation::setScheduler(Scheduler *scheduler)
{
   _scheduler = scheduler;
}

//-------------------------------------------------------
// determines scheduler and resets _time_sec and initializes var's
//-------------------------------------------------------
void Simulation::initSimulation()
{   
	if (_scheduler != NULL){
		return;
	}
	int odeCount=0;
	int pdeCount=0;
	Solver *solver = NULL;
	for (int i = 0; i < (int)solverList.size(); i ++) {
		solver = solverList[i];
		if (solver->isPDESolver()){
			pdeCount++;
		} else {
			odeCount++;
		}
	}

#ifdef VCELL_MPI     
	int numProcesses;
	int rank;
	MPI_Comm_size(MPI_COMM_WORLD,&numProcesses);
	MPI_Comm_rank(MPI_COMM_WORLD,&rank);

	if (rank==0) printf("pdeCount=%d, odeCount=%d\n", pdeCount, odeCount);
	if (numProcesses==1){
		if (rank==0) printf("using scheduler for serial algorithm\n");
			_scheduler = new SerialScheduler(this);
		}else if (pdeCount==0){
			printf("There are no PDE's, no appropriate parallel scheme is availlable\n");
			return false;
		}else if (numProcesses<=pdeCount){
			if (rank==0) printf("using PDE Scheduler - each process computes 1 (or more) PDEs and all the ODEs\n");
			_scheduler = new PDEScheduler(this);
		}else{
			if (rank==0) printf("using Domain Decomposition Scheduler - one process for each PDE, one process for each sub-domain (all ODEs)\n");
			_scheduler = new DomainPDEScheduler(this);
		}
#else
	printf("pdeCount=%d, odeCount=%d\n", pdeCount, odeCount);
	_scheduler = new SerialScheduler(this);
#endif	

	VCellModel *model = SimTool::getInstance()->getModel();
	model->resolveReferences();
	_scheduler->initValues();

	currIteration = 0;
}
