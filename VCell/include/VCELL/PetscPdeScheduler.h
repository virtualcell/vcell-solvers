/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef PETSCPDESOLVER_H
#define PETSCPDESOLVER_H

#ifdef VCELL_PETSC

#include <VCELL/Scheduler.h>
#include <petsc.h>
#include <VCELL/SundialsSolverOptions.h>

class Variable;
class CartesianMesh;
class SparseMatrixPCG;
struct MembraneElement;
struct VolumeElement;
class SimulationExpression;
class VarContext;
class Feature;

class PetscPdeScheduler : public Scheduler
{
public:
	PetscPdeScheduler(Simulation *sim);
	~PetscPdeScheduler();

	void iterate();
	double getCurrentTime() { return currentTime; }
	void setSimStartTime(double st) {
		currentTime = st;
	}

private:
	SimulationExpression* simulation;
	TS ts;
	Vec vecY;
	Vec oldY;
	SNES snes;
	Mat Pmat;

	void computeNonZeros(int* nnz);
	void buildJ_Volume(double t, double* yinput, Mat Pmat);
	void buildJ_Membrane(double t, double* yinput, Mat Pmat);

	void buildPmat_Volume(double t, double gamma, double* yinput, Mat Pmat);
	void buildPmat_Membrane(double t, double gamma, double* yinput, Mat Pmat);

	static PetscErrorCode RHS_Function(TS ts, PetscReal t, Vec Y, Vec F, void *ctx);
	PetscErrorCode rhs(PetscReal t, double* yinput, double* rhs);

	PetscErrorCode ts_buildPmat(PetscReal t, Vec Y, Mat Pmat);
	static PetscErrorCode TS_Jacobian_Function(TS ts, PetscReal t, Vec Y, Mat Amat, Mat Pmat, void *ctx);
	PetscErrorCode ts_jacobian(TS ts, PetscReal t, Vec Y, Mat Amat, Mat Pmat);

	static PetscErrorCode SNES_Function(SNES snes, Vec Y, Vec F, void *ctx);
	PetscErrorCode snes_function(SNES snes, Vec Y, Vec F);

	PetscErrorCode snes_buildPmat(Vec Y, Mat Pmat);
	static PetscErrorCode SNES_Jacobian_Function(SNES snes, Vec Y, Mat Amat, Mat Pmat, void *ctx);
	PetscErrorCode snes_jacobian(SNES snes, Vec Y, Mat Amat, Mat Pmat);

	VolumeElement* pVolumeElement;
	MembraneElement* pMembraneElement;

	int numSymbolsPerVolVar; // U, U_Feature1_membrane, U_Feature2_membrane, ...
	long numUnknowns;
	long numVolUnknowns;

	int* global2Local;
	int* local2Global;
	int* regionOffsets;
	int* regionSizes;
	int* regionDefinedVolVariableSizes; // the number of variables defined in each region
	int** regionDefinedVolVariableIndexes;
	bool* bRegionHasTimeDepdentVariables; // used to see if we have to update neighbor state values 
	bool* bRegionHasConstantCoefficients; // call variable operator if false.

	int* volVectorOffsets;
	int memVectorOffset;
	int volRegionVectorOffset;
	int memRegionVectorOffset;

	CartesianMesh* mesh;
	int dimension;
	double VOLUME;
	int Nx, Nxy;

	double oneOverH[3];

	void setupOrderMaps();
	void applyVolumeOperatorOld(double t, double* yinput, double* rhs);
	void applyVolumeOperator(double t, double* yinput, double* rhs);
	void regionApplyVolumeOperatorConstant(int regionID, double t, double* yinput, double* rhs);
	void regionApplyVolumeOperatorVariable(int regionID, double t, double* yinput, double* rhs);
	void applyMembraneDiffusionReactionOperator(double t, double* yinput, double* rhs);
	void applyMembraneFluxOperator(double t, double* yinput, double* rhs);
	void applyVolumeRegionReactionOperator(double t, double* yinput, double* rhs);
	void applyMembraneRegionReactionOperator(double t, double* yinput, double* rhs);
	PetscErrorCode initPetscSolver();
	PetscErrorCode solve();

	double *statePointValues, **neighborStatePointValues;
	void updateVolumeStatePointValues(int volIndex, double t, double* yinput, double* values);
	void updateMembraneStatePointValues(MembraneElement& me, double t, double* yinput, double* values);
	void updateRegionStatePointValues(int regionID, double t, double* yinput, bool bVolumeRegion, double* values);

	void updateSolutions();

	int getVolumeElementVectorOffset(int volIndex, int regionID);
	int getMembraneElementVectorOffset(int meindex);
	int getVolumeRegionVectorOffset(int regionID);
	int getMembraneRegionVectorOffset(int regionID);

	double currentTime;

	bool bHasVariableDiffusionAdvection, bHasAdvection;

	double txyzValues[4];
	void dirichletPointSetup(int volIndex, Feature* feature, VarContext* varContext, int mask, int* volumeNeighbors, double& ypoint);
	double computeNeumannCondition(Feature* feature, VarContext* varContext, int mask, double* scaleSs);

	double* diffCoeffs;
	void precomputeDiffusionCoefficients();

	void computeScaleS(int mask, double* scaleS);
};

#endif

#endif
