/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef ODESOLVER_H
#define ODESOLVER_H

#include <VCELL/Solver.h>

class ODESolver : public Solver
{
public:
	ODESolver(Variable *var, 
				Mesh *mesh,
				int   numSolveRegions, 
				int   *solveRegions);
	virtual ~ODESolver();

	virtual void solveEqn(double deltaTime, int volumeIndexStart, int volumeIndexSize, int membraneIndexStart, int membraneIndexSize, bool bFirstTime);

	double *getRates() { return rate; }
	long getArraySize() {return arraySize;}
	long getGlobalIndex(long arrayIndex) {return Gridmap[arrayIndex];}
    
protected:
	Mesh         *mesh;
	long         size;
	double       *rate;
	long         arraySize;
	long         *Gridmap;
};

#endif
