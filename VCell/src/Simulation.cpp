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
	_time_sec = 0; // seconds
	_mesh = mesh;
	_solverList = NULL;
	_varList = NULL;
	_advanced = false;
	_initEquations = false;
	_scheduler = NULL;
	globalParticleList.clear();
   
	numVariables = 0;

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
	_time_sec+=_dT_sec; 
}

void Simulation::advanceTimeOff() {
	if (!_advanced) {
		throw "Simulation::advanceTimeOff() : time has already been advanced off";
	}
	_advanced=false; 
	_time_sec-=_dT_sec; 
}
   
void Simulation::iterate()
{
	_scheduler->iterate();
}

double Simulation::getMaxDifference()
{
	double maxDiff = 0;

	Variable *var = NULL;
	while (var = getNextVariable(var)){
		double diff = var->getMaxDifference();
		maxDiff = max(maxDiff, diff);
	}

	return maxDiff;
}

void Simulation::update()
{
	_scheduler->update();

	_time_sec += _dT_sec;
}

void Simulation::reset()
{
	_scheduler->reset();
}

void Simulation::addSolver(Solver *solver)
{
   solver->next = _solverList;
   _solverList = solver;
}

Solver *Simulation::getNextSolver(Solver *solver)
{
	if (solver==NULL){
		return _solverList;
	}else{
		return solver->next;
	}
}

void Simulation::addVariable(Variable *var)
{
	var->next = _varList;
	_varList = var;
	numVariables ++;
}

Variable *Simulation::getNextVariable(Variable *var)
{
   if (var==NULL){
      return _varList;
   }else{
      return (Variable *)var->next;
   }
}

Variable *Simulation::getVariableFromName(string& varName)
{
	Variable *var=NULL;
	ASSERTION(varName);
	while (var=getNextVariable(var)){
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
   Solver *solver=NULL;
   ASSERTION(var);
   while (solver=getNextSolver(solver)){
      if (solver->getVar()==var){
         return solver;
      }
   }
   return NULL;
}

void Simulation::synchronize()
{
	//
	// if applicable, collect results into root process
	//
	_scheduler->collectResults(0);
}

void Simulation::addParticle(Particle *particle)
{ 
	globalParticleList.push_back(particle); 
}

bool Simulation::writeData(char *filename, bool bCompress)
{
   
	synchronize();

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
	Feature *feature = NULL;
	while (feature = model->getNextFeature(feature)){
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
			cout << "Simulation::writeData(), error opening file " << newFname << " for writing" << endl;
			return false;
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
	return dataset.write(filename, this, bCompress);
}

bool Simulation::readData(char *filename)
{
	//
	// all processes read data
	// (in the future, root could read and then distribute)
	//
	DataSet dataset;
	return dataset.read(filename, this);
}

void Simulation::setScheduler(Scheduler *scheduler)
{
   _scheduler = scheduler;
}

//-------------------------------------------------------
// determines scheduler and resets _time_sec and initializes var's
//-------------------------------------------------------
bool Simulation::initSimulation()
{   
	if (_scheduler != NULL){
		return true;
	}
	int odeCount=0;
	int pdeCount=0;
	Solver *solver = NULL;
	while (solver = getNextSolver(solver)){
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
	//
	// determine max iteration time
	//
	VCellModel *model = SimTool::getInstance()->getModel();

	if (!model->resolveReferences()){
		printf("Simulation::initSimulation() failed\n");
		return false;
	}
	_scheduler->initValues();
	//
	// Initialize Potential
	//

	_time_sec = 0.0;

	return true;
}
