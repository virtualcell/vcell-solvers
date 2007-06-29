/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <stdio.h>
#include <VCELL/TriDiagMatrix.h>
#include <VCELL/Variable.h>
#include <VCELL/EqnBuilder.h>
#include <VCELL/Simulation.h>
#include <VCELL/SimTypes.h>
#include <VCELL/Mesh.h>
#include <VCELL/Solver.h>
#include <VCELL/Region.h>
#include <VCELL/CartesianMesh.h>

Solver::Solver(Variable *variable)
{
	var = variable;
	next = NULL;
	eqnBuilder = NULL;
}

bool Solver::initEqn(double deltaTime, int volumeIndexStart, int volumeIndexSize, int membraneIndexStart, int membraneIndexSize, bool bFirstTime)
{
	if (!bFirstTime) {
		return true;
	}

	ASSERTION(eqnBuilder);

	return eqnBuilder->initEquation(deltaTime, volumeIndexStart, volumeIndexSize, membraneIndexStart, membraneIndexSize);
}

bool Solver::buildEqn(double deltaTime, int volumeIndexStart, int volumeIndexSize, int membraneIndexStart, int membraneIndexSize, bool bFirstTime)
{
	ASSERTION(eqnBuilder);

	return eqnBuilder->buildEquation(deltaTime, volumeIndexStart, volumeIndexSize, membraneIndexStart, membraneIndexSize);
}
