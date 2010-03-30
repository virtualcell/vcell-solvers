/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef MEMBRANEEQNBUILDERDIFFUSION_H
#define MEMBRANEEQNBUILDERDIFFUSION_H

#include <VCELL/SparseMatrixEqnBuilder.h>
#include <vector>
using std::vector;
#include <map>
using std::pair;

class Mesh;
class MembraneVariable;


class MembraneEqnBuilderDiffusion : public SparseMatrixEqnBuilder
{
public:
	MembraneEqnBuilderDiffusion(MembraneVariable *species, Mesh *mesh);
	~MembraneEqnBuilderDiffusion();

	void initEquation(double deltaTime, int volumeIndexStart, int volumeIndexSize, int membraneIndexStart, int membraneIndexSize);	
	void buildEquation(double deltaTime, int volumeIndexStart, int volumeIndexSize, int membraneIndexStart, int membraneIndexSize);

	void postProcess();

private:
	vector<pair<int, int> > periodicPairs; // map of minus and plus pairs of periodic boundary points.

	void initEquation_Periodic(double deltaTime, int volumeIndexStart, int volumeIndexSize, int membraneIndexStart, int membraneIndexSize);
	void buildEquation_Periodic(double deltaTime, int volumeIndexStart, int volumeIndexSize, int membraneIndexStart, int membraneIndexSize);

	bool bPreProcessed;
	void preProcess();

private:
	double computeDiffusionConstant(int meIndex, int neighborIndex);
};    

#endif
