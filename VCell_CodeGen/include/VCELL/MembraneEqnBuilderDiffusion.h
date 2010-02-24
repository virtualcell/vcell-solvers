/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef MEMBRANEEQNBUILDERDIFFUSION_H
#define MEMBRANEEQNBUILDERDIFFUSION_H

#include <VCELL/SparseMatrixEqnBuilder.h>

class Mesh;
class MembraneVariable;

class MembraneEqnBuilderDiffusion : public SparseMatrixEqnBuilder
{
public:
	MembraneEqnBuilderDiffusion(MembraneVariable *species, Mesh *mesh);
	~MembraneEqnBuilderDiffusion();

	void initEquation(double deltaTime, int volumeIndexStart, int volumeIndexSize, int membraneIndexStart, int membraneIndexSize);
	void buildEquation(double deltaTime, int volumeIndexStart, int volumeIndexSize, int membraneIndexStart, int membraneIndexSize);
};    

#endif
