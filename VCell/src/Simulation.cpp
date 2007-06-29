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

#ifdef FINITEVOLUME_STANDALONE
#include <VCELL/FieldData.h>
#include <ValueProxy.h>


class ValueProxyInside : public ValueProxy
{
public:
	ValueProxyInside(double* arg_values, int* arg_indices, Mesh* arg_mesh) 
		: ValueProxy(arg_values,  -1, arg_indices) {
			mesh = arg_mesh;
	}
	
	double evaluate() {
		MembraneElement* element = mesh->getMembraneElements() + indices[VAR_MEMBRANE_INDEX];
		if (element->insideIndexFar<0){
			return values[element->insideIndexNear];
		}else{
			return 1.5 * values[element->insideIndexNear] -	0.5 * values[element->insideIndexFar];
		}	
	}

private:
	Mesh* mesh;
};

class ValueProxyOutside : public ValueProxy
{
public:
	ValueProxyOutside(double* arg_values, int* arg_indices, Mesh* arg_mesh) 
		: ValueProxy(arg_values,  -1, arg_indices) {
			mesh = arg_mesh;
	}
	
	double evaluate() {
		MembraneElement* element = mesh->getMembraneElements() + indices[VAR_MEMBRANE_INDEX];
		if (element->outsideIndexFar<0){
			return values[element->outsideIndexNear];
		}else{
			return 1.5 * values[element->outsideIndexNear] - 0.5 * values[element->outsideIndexFar];
		}	
	}

private:
	Mesh* mesh;
};
#endif

//----------------------------------------------------------------------------
//
// class Simulation
//
//----------------------------------------------------------------------------
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

#ifdef FINITEVOLUME_STANDALONE
	oldSymbolTable = NULL;	
	//currSymbolTable = NULL;

	indices = new int[NUM_VAR_INDEX];
	for (int i = 0; i < NUM_VAR_INDEX; i ++) {
		indices[i] = -1;
	}	

	valueProxyTime = new ScalarValueProxy();
	valueProxyX = new ScalarValueProxy();
	valueProxyY = new ScalarValueProxy();
	valueProxyZ = new ScalarValueProxy();
#endif

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
#ifdef FINITEVOLUME_STANDALONE
	valueProxyTime->setValue(_time_sec); 
#endif
}

void Simulation::advanceTimeOff() {
	if (!_advanced) {
		throw "Simulation::advanceTimeOff() : time has already been advanced off";
	}
	_advanced=false; 
	_time_sec-=_dT_sec; 
#ifdef FINITEVOLUME_STANDALONE
	valueProxyTime->setValue(_time_sec);
#endif
}

#ifdef FINITEVOLUME_STANDALONE
void Simulation::createSymbolTable() {	
	if (oldSymbolTable != NULL) {
		return;
	}

	Variable* var = NULL;
	string* variableNames = new string[numVariables * 3 + 4 + fieldDataList.size()];	
	ValueProxy** oldValueProxies = new ValueProxy*[numVariables * 3 + 4 + fieldDataList.size()];
	//ValueProxy** currValueProxies = new ValueProxy*[numVariables * 3 + 4 + fieldDataList.size()];

	variableNames[0] = "t";
	oldValueProxies[0] = valueProxyTime;
	//currValueProxies[0] = new ValueProxySimple();

	variableNames[1] = "x";	
	oldValueProxies[1] = valueProxyX;
	//currValueProxies[1] = new ValueProxySimple();

	variableNames[2] = "y";	
	oldValueProxies[2] = valueProxyY;
	//currValueProxies[2] = new ValueProxySimple();

	variableNames[3] = "z";	
	oldValueProxies[3] = valueProxyZ;
	//currValueProxies[3] = new ValueProxySimple();

	int variableIndex = 4;	

	for (int i = 0; i < (int)fieldDataList.size(); i ++) {		
		oldValueProxies[variableIndex] = new ValueProxy(fieldDataList[i]->getData(), VAR_VOLUME_INDEX, indices);
		variableNames[variableIndex ++] = fieldDataList[i]->getID();
	}

	while ( var = getNextVariable(var)) {	
		if (var->getVarType() == VAR_VOLUME) {
			oldValueProxies[variableIndex] = new ValueProxy(var->getOld(), VAR_VOLUME_INDEX, indices);
			//currValueProxies[variableIndex] = new ValueProxy(var->getCurr(), VAR_VOLUME_INDEX, indices);
			variableNames[variableIndex ++] = string(var->getName());

			oldValueProxies[variableIndex] = new ValueProxyInside(var->getOld(), indices, _mesh);
			//currValueProxies[variableIndex] = new ValueProxyInside(var->getCurr(), indices, _mesh);
			variableNames[variableIndex ++] = string(var->getName()) + "_INSIDE";

			oldValueProxies[variableIndex] = new ValueProxyOutside(var->getOld(), indices, _mesh);
			//currValueProxies[variableIndex] = new ValueProxyOutside(var->getCurr(), indices, _mesh);
			variableNames[variableIndex ++] = string(var->getName()) + "_OUTSIDE";	

		} else if (var->getVarType() == VAR_MEMBRANE) {
			oldValueProxies[variableIndex] = new ValueProxy(var->getOld(), VAR_MEMBRANE_INDEX, indices);
			//currValueProxies[variableIndex] = new ValueProxy(var->getCurr(), VAR_MEMBRANE_INDEX, indices);
			variableNames[variableIndex ++] = string(var->getName());

		} else if (var->getVarType() == VAR_CONTOUR) {
			oldValueProxies[variableIndex] = new ValueProxy(var->getOld(), VAR_CONTOUR_INDEX, indices);
			//currValueProxies[variableIndex] = new ValueProxy(var->getCurr(), VAR_CONTOUR_INDEX, indices);
			variableNames[variableIndex ++] = string(var->getName());

		} else if (var->getVarType() == VAR_VOLUME_REGION) {
			oldValueProxies[variableIndex] = new ValueProxy(var->getOld(), VAR_VOLUME_REGION_INDEX, indices);
			//currValueProxies[variableIndex] = new ValueProxy(var->getCurr(), VAR_VOLUME_REGION_INDEX, indices);
			variableNames[variableIndex ++] = string(var->getName());

		} else if (var->getVarType() == VAR_MEMBRANE_REGION) {			
			oldValueProxies[variableIndex] = new ValueProxy(var->getOld(), VAR_MEMBRANE_REGION_INDEX, indices);
			//currValueProxies[variableIndex] = new ValueProxy(var->getCurr(), VAR_MEMBRANE_REGION_INDEX, indices);
			variableNames[variableIndex ++] = string(var->getName());

		} else if (var->getVarType() == VAR_CONTOUR_REGION) {
			oldValueProxies[variableIndex] = new ValueProxy(var->getOld(), VAR_CONTOUR_REGION_INDEX, indices);
			//currValueProxies[variableIndex] = new ValueProxy(var->getCurr(), VAR_CONTOUR_REGION_INDEX, indices);
			variableNames[variableIndex ++] = string(var->getName());
		}
	}	

	oldSymbolTable = new SimpleSymbolTable(variableNames, variableIndex, oldValueProxies);
	//currSymbolTable = new SimpleSymbolTable(variableNames, variableIndex, currValueProxies);
	delete[] variableNames;	
}   

string* Simulation::getFieldSymbols() {
	string* symbols = new string[fieldDataList.size()];
	for (int i = 0; i < (int)fieldDataList.size(); i ++) {
		symbols[i] = fieldDataList[i]->getID();
	}
	return symbols;
}

void Simulation::populateFieldValues(double* darray, int index) {
	for (int i = 0; i < (int)fieldDataList.size(); i ++) {
		darray[i] = fieldDataList[i]->getData()[index];
	}
}

#endif
   
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
	return getVariableFromName(string(varName));
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
#ifdef FINITEVOLUME_STANDALONE
	createSymbolTable();
#endif

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
