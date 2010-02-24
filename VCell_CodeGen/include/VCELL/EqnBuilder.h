/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef EQNBUILDER_H
#define EQNBUILDER_H

class Mesh;
class Variable;

class EqnBuilder
{
public:
	EqnBuilder(Variable *var, Mesh *mesh);

	virtual void initEquation(double deltaTime, int volumeIndexStart, int volumeIndexSize, int membraneIndexStart, int membraneIndexSize)=0;
	virtual void buildEquation(double deltaTime, int volumeIndexStart, int volumeIndexSize, int membraneIndexStart, int membraneIndexSize)=0;

	Mesh* getMesh() { return mesh; };
	virtual bool isElliptic() { return false; }

protected:
	Variable   *var;
	Mesh       *mesh;
};    

#endif
