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
#include <VCELL/SimTool.h>
#include <VCELL/CartesianMesh.h>
#include <VCELL/SerialScheduler.h>
#include <VCELL/SundialsPdeScheduler.h>

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
	//globalParticleList.clear();
}

Simulation::~Simulation()
{
	for (int i = 0; i < (int)varList.size(); i ++) {
		delete varList[i];
	}
	varList.clear();
	for (int i = 0; i < (int)solverList.size(); i ++) {
		delete solverList[i];
	}
	solverList.clear();
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

void Simulation::update()
{
	for (int i = 0; i < (int)varList.size(); i ++) {
		Variable* var = varList[i];
		var->update();
	}
	//_scheduler->update();
	currIteration ++;
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

//void Simulation::addParticle(Particle *particle)
//{
//	globalParticleList.push_back(particle);
//}

void Simulation::writeData(char *filename, bool bCompress)
{
	//bool hasParticles = false;
	//VCellModel *model = SimTool::getInstance()->getModel();
	//for (int i = 0; i < model->getNumFeatures(); i ++) {
	//	Feature* feature = model->getFeatureFromIndex(i);
	//	if (feature->getVolumeParticleContext()!=NULL){
	//		hasParticles = true;
	//	}
	//	if (feature->getMembraneParticleContext()!=NULL){
	//		hasParticles = true;
	//	}
	//	if (feature->getContourParticleContext()!=NULL){
	//		hasParticles = true;
	//	}
	//}

	//if (hasParticles){
	//	FILE *fp = NULL;
	//	string newFname = string(filename) + ".particle";
	//	if ((fp = fopen(newFname.c_str(),"w"))==NULL){
	//		char errmsg[512];
	//		sprintf(errmsg, "Simulation::writeData(), error opening file %s for writing", newFname.c_str());
	//		throw errmsg;
	//	}

	//	fprintf(fp,"%lg %lg %lg\n", ((CartesianMesh*)_mesh)->getDomainSizeX(),
	//					((CartesianMesh*)_mesh)->getDomainSizeY(),
	//					((CartesianMesh*)_mesh)->getDomainSizeZ());
	//	fprintf(fp,"%d %d %d\n", ((CartesianMesh*)_mesh)->getNumVolumeX(),
	//				((CartesianMesh*)_mesh)->getNumVolumeY(),
	//				((CartesianMesh*)_mesh)->getNumVolumeZ());

	//	//
	//	//write particle file (if needed)
	//	//
	//	vector<Particle*>::iterator particleIterator;
	//	for (particleIterator = globalParticleList.begin();particleIterator != globalParticleList.end();particleIterator++){
	//		(*particleIterator)->write(fp);
	//	}

	//	fclose(fp);
	//}

	DataSet::write(filename, this, bCompress);
}

void Simulation::readData(char *filename)
{
	//
	// all processes read data
	// (in the future, root could read and then distribute)
	//
	DataSet::read(filename, this);
}

void Simulation::setScheduler(Scheduler *scheduler)
{
   _scheduler = scheduler;
}

//-------------------------------------------------------
// determines scheduler and resets _time_sec and initializes var's
//-------------------------------------------------------
void Simulation::resolveReferences() {
	VCellModel *model = SimTool::getInstance()->getModel();
	model->resolveReferences();
}

void Simulation::initSimulation()
{
	if (_scheduler == 0) {
		int odeCount = 0, pdeCount = 0;
		for (int i = 0; i < (int)varList.size(); i ++) {
			Variable* var = varList[i];
			if (var->isPde()){
				pdeCount ++;
			} else {
				odeCount ++;
			}
		}

		printf("pdeCount=%d, odeCount=%d\n", pdeCount, odeCount);
	
		SimTool* simTool = SimTool::getInstance();
		if (simTool->isSundialsPdeSolver()) {
			_scheduler = new SundialsPdeScheduler(this, simTool->getSundialsRelativeTolerance(), simTool->getSundialsAbsoluteTolerance(), simTool->getSundialsMaxStep(), simTool->getNumDiscontinuityTimes(), simTool->getDiscontinuityTimes(), simTool->isSundialsOneStepOutput());
		} else {
			_scheduler = new SerialScheduler(this);
		}
	}

	_scheduler->initValues();
	currIteration = 0;
}

double Simulation::getTime_sec() {
	if (SimTool::getInstance()->isSundialsPdeSolver()) {
		return ((SundialsPdeScheduler*)_scheduler)->getCurrentTime();
	}
	return _advanced ? (currIteration + 1) * _dT_sec : currIteration * _dT_sec;
}

void Simulation::setSimStartTime(double st) {
	if (SimTool::getInstance()->isSundialsPdeSolver()) {
		return ((SundialsPdeScheduler*)_scheduler)->setSimStartTime(st);
	}
}
