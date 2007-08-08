/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/SimTypes.h>
#include <VCELL/Mesh.h>
#include <VCELL/Variable.h>
#include <VCELL/Solver.h>
#include <VCELL/Simulation.h>
#include <VCELL/SerialScheduler.h>
#include <VCELL/Feature.h>
#include <VCELL/ParticleContext.h>
#include <VCELL/VCellModel.h>
#include <VCELL/FastSystem.h>
#include <VCELL/VolumeRegion.h>
#include <VCELL/MembraneRegion.h>
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
	*/
	Feature *feature = NULL;
	while (feature = model->getNextFeature(feature)){
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
	feature = NULL;
	while (feature = model->getNextFeature(feature)){
		VolumeParticleContext *vpc = feature->getVolumeParticleContext();
		ContourParticleContext *cpc = feature->getContourParticleContext();
		if(vpc != NULL){
			vpc->move();
		}
		if(cpc != NULL){
			cpc->move();
		}
	}

	Solver *solver=NULL;
	int volumeSize = sim->getMesh()->getNumVolumeElements(); 
	int membraneSize = sim->getMesh()->getNumMembraneElements(); 
	while (solver=sim->getNextSolver(solver)){
		string timername = solver->getVar()->getName() + " Build";
		TimerHandle tHndBuild = SimTool::getInstance()->getTimerHandle(timername);
		timername = solver->getVar()->getName() + " Solve";
		TimerHandle tHndSolve = SimTool::getInstance()->getTimerHandle(timername);
		//
		// initialize equations first time around
		//
		if (!solver->initEqn(sim->getDT_sec(),0,volumeSize,0,membraneSize, bFirstTime)){
			throw "Simulation::iterate() - error init'ing equation";
		}
		SimTool::getInstance()->startTimer(tHndBuild);
		if (!solver->buildEqn(sim->getDT_sec(),0,volumeSize,0,membraneSize, bFirstTime)){
			throw "Simulation::iterate() - error building equation";
		}
		SimTool::getInstance()->stopTimer(tHndBuild);
		SimTool::getInstance()->startTimer(tHndSolve);
		if (!solver->solveEqn(sim->getDT_sec(),0,volumeSize,0,membraneSize, bFirstTime)){
			throw "Simulation::iterate() - error solving equation";        
		}
		SimTool::getInstance()->stopTimer(tHndSolve);
	}
	if(hasFastSystem()){
		Mesh *mesh = sim->getMesh();
		solveFastSystem(0, mesh->getNumVolumeElements(), 0, mesh->getNumMembraneElements());
	}
	bFirstTime=false;
}
