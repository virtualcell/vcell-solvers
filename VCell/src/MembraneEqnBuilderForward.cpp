/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <stdio.h>
#include <math.h>
#include <VCELL/Simulation.h>
#include <VCELL/ODESolver.h>
#include <VCELL/MembraneVariable.h>
#include <VCELL/Mesh.h>
#include <VCELL/Feature.h>
#include <VCELL/MembraneVarContext.h>
#include <VCELL/MembraneEqnBuilderForward.h>
#include <VCELL/SimTool.h>

MembraneEqnBuilderForward::MembraneEqnBuilderForward(MembraneVariable *Avar, Mesh *Amesh, ODESolver *Asolver) : EqnBuilder(Avar,Amesh) {
	odeSolver = Asolver;
}

bool MembraneEqnBuilderForward::buildEquation(double deltaTime, int volumeIndexStart, int volumeIndexSize, int membraneIndexStart, int membraneIndexSize)
{
	Feature *feature;
	MembraneVarContext *varContext;

	Simulation *sim = SimTool::getInstance()->getSimulation();

	ASSERTION((membraneIndexStart>=0) && ((membraneIndexStart+membraneIndexSize)<=mesh->getNumMembraneElements()));

	MembraneElement *pMembraneElement = mesh->getMembraneElements() + membraneIndexStart;
	double *pRate = odeSolver->getRates() + membraneIndexStart;
	ASSERTION(pMembraneElement);

	for (long memIndex=membraneIndexStart;memIndex<(membraneIndexStart+membraneIndexSize);memIndex++){

		feature = pMembraneElement->feature;
		ASSERTION(feature);

		varContext = feature->getMembraneVarContext((MembraneVariable*)var);

		sim->advanceTimeOn();
		*pRate = varContext->getMembraneReactionRate(pMembraneElement);
		sim->advanceTimeOff();

		pMembraneElement++;
		pRate++;
	} // end memIndex
 
	return true;
}
