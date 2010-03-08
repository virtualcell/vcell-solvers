/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/Variable.h>
#include <VCELL/EqnBuilder.h>
#include <VCELL/Solver.h>

Solver::Solver(Variable *variable)
{
	var = variable;
	eqnBuilder = NULL;
}

void Solver::initEqn(double deltaTime, int volumeIndexStart, int volumeIndexSize, int membraneIndexStart, int membraneIndexSize, bool bFirstTime)
{
	if (!bFirstTime) {
		return;
	}

	ASSERTION(eqnBuilder);

	eqnBuilder->initEquation(deltaTime, volumeIndexStart, volumeIndexSize, membraneIndexStart, membraneIndexSize);
}

void Solver::buildEqn(double deltaTime, int volumeIndexStart, int volumeIndexSize, int membraneIndexStart, int membraneIndexSize, bool bFirstTime)
{
	ASSERTION(eqnBuilder);

	eqnBuilder->buildEquation(deltaTime, volumeIndexStart, volumeIndexSize, membraneIndexStart, membraneIndexSize);
}
