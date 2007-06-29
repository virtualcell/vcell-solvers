/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef SPARSEVOLUMEEQNBUILDER_H
#define SPARSEVOLUMEEQNBUILDER_H

#include <VCELL/VolumeVarContext.h>
#include <VCELL/SparseMatrixEqnBuilder.h>
#include <vector>
using namespace std;

class CartesianMesh;
class VolumeVariable;

struct CoupledNeighbors {
	int centerIndex;
	int neighborIndex;
	double coeff;
	CoupledNeighbors(int arg_centerIndex, int arg_neighborIndex, double arg_coeff) {
		centerIndex = arg_centerIndex;
		neighborIndex = arg_neighborIndex;
		coeff = arg_coeff;
	}
};

typedef enum {ZM=0, YM, XM, XP, YP, ZP} XYZNeighbor;

struct VolumeNeighbor {
	int index; // neighbor index, might be -1 if it is not valid, meaning there is no such neighbor
	bool bPeriodic;
	
	// for convection only
	int convectionDirection;
	double Vi;
	double Vj;

	VolumeNeighbor() {
		index = -1;
		bPeriodic = false;
		convectionDirection = 0;
		Vi = 0;
		Vj = 0;
	}

	VolumeNeighbor(int idx) {
		index = idx;
		bPeriodic = false;
		convectionDirection = 0;
		Vi = 0;
		Vj = 0;
	}

	void setConvectionCoefficients(int myindex, int mymask, XYZNeighbor whichNeighbor, VolumeVarContext* varContext) {
		if (index < 0) {
			return;
		}
		switch (whichNeighbor) { 
			case XM:
			case XP:
				convectionDirection = (whichNeighbor == XM) ? 1 : -1;
				Vi = varContext->getConvectionVelocity_X(myindex);
				Vj = varContext->getConvectionVelocity_X(index);
				break;
			case YM:
			case YP:
				convectionDirection = (whichNeighbor == YM) ? 1 : -1;
				Vi = varContext->getConvectionVelocity_Y(myindex);
				Vj = varContext->getConvectionVelocity_Y(index);
				break;
			case ZM:
			case ZP:
				convectionDirection = (whichNeighbor == ZM) ? 1 : -1;
				Vi = varContext->getConvectionVelocity_Z(myindex);
				Vj = varContext->getConvectionVelocity_Z(index);
				break;
		}
	}
};

/*
 * We always use symmetric matrix when there is no convection no matter
 * what the boundary conditions are.
 * We always use non-symmetric matrix when there is convection, of course.
 *
 */
class SparseVolumeEqnBuilder : public SparseMatrixEqnBuilder
{
public:
	SparseVolumeEqnBuilder(VolumeVariable *species, CartesianMesh *mesh, bool bNoConvection, int numSolveRegions=0, int* solveRegions=0);
	~SparseVolumeEqnBuilder();

	bool initEquation(double deltaTime, int volumeIndexStart, int volumeIndexSize, 
			int membraneIndexStart, int membraneIndexSize);
	bool buildEquation(double deltaTime, int volumeIndexStart, int volumeIndexSize, 
			int membraneIndexStart, int membraneIndexSize);
	void postProcess();

private:
	bool bSymmetricStorage; // no convection, A would be symmetric
	vector<CoupledNeighbors*> dirichletNeighbors; // Aij, j is dirichlet points, computed in LHS, used it RHS
	vector<CoupledNeighbors*> periodicNeighbors; // Aij * periodicConstant, j is dirichlet points, computed in LHS, used it RHS
	vector<CoupledNeighbors*> periodicCoupledPairs; // list of minus and plus pairs of periodic boundary points, used to update the solutions of these points.	
	bool bPreProcessed;

	int DIM;
	double DELTAX, DELTAY, DELTAZ;
	double AREAX, AREAY, AREAZ;
	double VOLUME;
	int SIZEX, SIZEY, SIZEZ, SIZEXY;	

	int numSolveRegions;
	int* solveRegions; // list of IDs of solve regions
	int* GlobalToLocalMap; // global to local mapping, total number of elements is mesh size
	int* LocalToGlobalMap; // local to global mapping, total number of elements is sum of region sizes.
	int* RegionFirstRow; // list of indexes of first row of each region (cummulative sum of number of nodes);
	bool bSolveWholeMesh;	

	void init();
	void computeLHS(int index, double* lambdas, double& Aii, int& numCols, int* columnIndices, double* columnValues, bool& bSort);
	double computeRHS(int index, double deltaTime, double* lambdas, double bInit);
	void preProcess();
	bool checkPeriodicCoupledPairsInRegions(int indexm, int indexp);
};    

#endif
