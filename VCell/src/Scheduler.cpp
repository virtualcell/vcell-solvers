/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/SimTypes.h>
#include <VCELL/Variable.h>
#include <VCELL/Solver.h>
#include <VCELL/Simulation.h>
#include <VCELL/Scheduler.h>
#include <VCELL/Feature.h>
#include <VCELL/ParticleContext.h>
#include <VCELL/VCellModel.h>
#include <VCELL/FastSystem.h>
#include <VCELL/VolumeRegion.h>
#include <VCELL/MembraneRegion.h>
#include <VCELL/SimTool.h>
#include <VCELL/CartesianMesh.h>
#include <string>
#include <sstream>
using namespace std;
//----------------------------------------------------------------------------
//
// class Scheduler
//
//----------------------------------------------------------------------------
Scheduler::Scheduler(Simulation *Asim)
{
	sim = Asim;
	bFirstTime = true;
	bHasFastSystem = false;
}

void Scheduler::update()
{
	Variable *var = NULL;
	while (var = sim->getNextVariable(var)){
		var->update();
	}
}

void Scheduler::reset()
{
	Variable *var = NULL;
	while (var = sim->getNextVariable(var)){
		var->revert();
	}
}

void Scheduler::collectResults(int processRank)
{
	SimTool::getInstance()->loadAllHistograms();
}

bool Scheduler::initValues()
{
	Feature *feature;
	//
	// tell Features to initialize the variables with 
	// feature specific values
	//
	Mesh *mesh = sim->getMesh();
	VolumeElement *pVolumeElement = mesh->getVolumeElements();
	long i=0;
	for (i=0;i<mesh->getNumVolumeElements();i++){
		if (feature = pVolumeElement->feature){
			if (!feature->initVolumeValues(i)){
				cout << "Simulation::initValues() - error init'ing vars in Feature " << feature->getName() << endl;
			}
		}
		pVolumeElement++;
	}

	MembraneElement *pMembraneElement = mesh->getMembraneElements();
	for (i=0;i<mesh->getNumMembraneElements();i++){
		if (feature = pMembraneElement->feature){
			if (!feature->initMembraneValues(pMembraneElement)){
				cout << "Simulation::initValues() - error init'ing vars in Feature " << feature->getName() << endl;
			}
		}
		pMembraneElement++;
	}

	int numVolumeRegions = ((CartesianMesh*)mesh)->getNumVolumeRegions();
	if(numVolumeRegions>0){
		for(int j=0; j<numVolumeRegions; j++){
		VolumeRegion *region = ((CartesianMesh*)mesh)->getVolumeRegion(j);
		feature = region->getFeature();
			if (!feature->initVolumeRegionValues(j)){
				cout << "Simulation::initValues() - error init'ing vars Feature " << feature->getName() << endl;
			}
		}
	}

	int numMembraneRegions = ((CartesianMesh*)mesh)->getNumMembraneRegions();
	if(numMembraneRegions>0){
		for(int j=0; j<numMembraneRegions; j++){
		MembraneRegion *membrane = ((CartesianMesh*)mesh)->getMembraneRegion(j);
		feature = membrane->getRegionInside()->getFeature();
			if (!feature->initMembraneRegionValues(j)){
				cout << "Simulation::initValues() - error init'ing vars Feature " << feature->getName() << endl;
			}
		}
	}

	VCellModel *model = SimTool::getInstance()->getModel();
	feature = NULL;
	bHasFastSystem = false;
	while (feature = model->getNextFeature(feature)){
		if(feature->getFastSystem() || feature->getMembraneFastSystem()){
			bHasFastSystem = true;
			break;
		}
	}
	return true;
}

void Scheduler::solveFastSystem(int volStart, int volSize, int memStart, int memSize)
{
	Feature *feature = NULL;
	FastSystem *fs = NULL;
	Mesh *mesh = sim->getMesh();

	ASSERTION(volSize>=0);
	ASSERTION(memSize>=0);
	ASSERTION(volStart >= 0 && (volStart+volSize) <= mesh->getNumVolumeElements());
	ASSERTION(memStart >= 0 && (memStart+memSize) <= mesh->getNumMembraneElements());
	//
	// perform pseudo-steady approximation on volume elements
	//
	VolumeElement *pVolumeElement = mesh->getVolumeElements();
	long i;
	for (i=volStart;i < volStart+volSize;i++){
		feature = pVolumeElement->feature;
		ASSERTION(feature);
		if (fs = feature->getFastSystem()){
			fs->setCurrIndex(i);
			fs->setCoordinates(sim->getTime_sec(), mesh->getVolumeWorldCoord(i));
			fs->initVars();			
			if(!fs->solveSystem()){
				stringstream ss;
				ss << "Scheduler::solveFastSystem() - error solving FastSystem in " << feature->getName();
				throw ss.str();
			}else{
				fs->updateVars();
			}
		}
		pVolumeElement++;
	}
	//
	// perform pseudo-steady approximation on membrane elements
	//
	MembraneElement *pMembraneElement = mesh->getMembraneElements();
	for (i=memStart;i < memStart+memSize;i++){
		feature = pMembraneElement->feature;
		ASSERTION(feature);
		if (fs = feature->getMembraneFastSystem()){
			fs->setCurrIndex(i);
			fs->setCoordinates(sim->getTime_sec(), mesh->getMembraneWorldCoord(i));
			fs->initVars();
			if(!fs->solveSystem()){
				stringstream ss;
				ss << "Scheduler::solveFastSystem() - error solving FastSystem in " << feature->getName();
				throw ss.str();
			}else{
				fs->updateVars();
			}
		}
		pMembraneElement++;
	}
}
