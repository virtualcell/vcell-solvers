/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/Variable.h>
#include <VCELL/Solver.h>
#include <VCELL/Simulation.h>
#include <VCELL/SerialScheduler.h>
#include <VCELL/FastSystem.h>
#include <VCELL/SimTool.h>
#include <VCELL/CartesianMesh.h>

SerialScheduler::SerialScheduler(Simulation *Asim)
: Scheduler(Asim)
{
}
   
void SerialScheduler::iterate()
{

	VCellModel *model = SimTool::getInstance()->getModel();
	/*
	Contour *contour = NULL;
	int numContours = model->getNumContours();
	for(int i=0; i<numContours; i++){
		contour = model->getContour(i);
		ContourParticleContext *cpc = NULL;
		while (cpc = contour->getNextContourParticleContext(cpc)){
		cpc->releaseParticles();
		}
	}

	for(i=0; i<numContours; i++){
		contour = model->getContour(i);
		ContourParticleContext *cpc = NULL;
		while (cpc = contour->getNextContourParticleContext(cpc)){
		cpc->captureParticles();
		}
	}
	
	for (int i = 0; i < model->getNumFeatures(); i ++) {
		Feature* feature = model->getFeatureFromIndex(i);
		VolumeParticleContext *vpc = feature->getVolumeParticleContext();
		ContourParticleContext *cpc = feature->getContourParticleContext();
		if(vpc != NULL){
			vpc->react();
		}

		if(cpc != NULL){
			cpc->releaseParticles();
			cpc->captureParticles();
		}

	}
	for (int i = 0; i < model->getNumFeatures(); i ++) {
		Feature* feature = model->getFeatureFromIndex(i);
		VolumeParticleContext *vpc = feature->getVolumeParticleContext();
		ContourParticleContext *cpc = feature->getContourParticleContext();
		if(vpc != NULL){
			vpc->move();
		}
		if(cpc != NULL){
			cpc->move();
		}
	}
	*/

	Solver *solver=NULL;
	int volumeSize = sim->getMesh()->getNumVolumeElements(); 
	int membraneSize = sim->getMesh()->getNumMembraneElements(); 
	for (int i = 0; i < sim->getNumSolvers(); i ++) {
		solver = sim->getSolver(i);
		string timername = solver->getVar()->getName() + " Build";
		TimerHandle tHndBuild = SimTool::getInstance()->getTimerHandle(timername);
		timername = solver->getVar()->getName() + " Solve";
		TimerHandle tHndSolve = SimTool::getInstance()->getTimerHandle(timername);
		//
		// initialize equations first time around
		//
		solver->initEqn(sim->getDT_sec(),0,volumeSize,0,membraneSize, bFirstTime);

		SimTool::getInstance()->startTimer(tHndBuild);
		solver->buildEqn(sim->getDT_sec(),0,volumeSize,0,membraneSize, bFirstTime);
		SimTool::getInstance()->stopTimer(tHndBuild);

		SimTool::getInstance()->startTimer(tHndSolve);
		solver->solveEqn(sim->getDT_sec(),0,volumeSize,0,membraneSize, bFirstTime);
		SimTool::getInstance()->stopTimer(tHndSolve);
	}
	if(hasFastSystem()){
		Mesh *mesh = sim->getMesh();
		solveFastSystem(0, mesh->getNumVolumeElements(), 0, mesh->getNumMembraneElements());
	}
	bFirstTime=false;
}
