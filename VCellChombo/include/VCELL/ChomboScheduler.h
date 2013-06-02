#ifndef CHOMBOSCHEDULER_H
#define CHOMBOSCHEDULER_H

//#include <vector>
#include <iostream>
using namespace std;

#include <VCELL/VarContext.h>
#include <Vector.H>
#include <RealVect.H>
#include <IntVect.H>
#include <EBIndexSpace.H>
#include <BaseIF.H>

class SimulationExpression;
class Membrane;
class Variable;
class Feature;
class ChomboGeometry;
class ChomboSpec;

class ProblemDomain;
class EBCellFAB;
class EBISLayout;
template <class> class RefCountedPtr;
template <class> class LevelData;
class Box;
class EBAMRPoissonOpFactory;
class DisjointBoxLayout;
class EBIndexSpace;
template <class> class BaseIVFAB;

#define NUM_PHASES 2
struct ConnectedComponent;

class ChomboScheduler {

	friend class ChomboDomainBC;
	friend class ChomboEBBC;
	
public:
	ChomboScheduler(SimulationExpression* sim, ChomboSpec* chomboSpec);
	~ChomboScheduler();
	
	void initializeGrids();
	virtual void initValues()=0;

	void writeData();
	int getNumMembranePoints()
	{
		return numMembranePoints;
	}
	ChomboGeometry* getChomboGeometry()
	{
		return chomboGeometry;
	}
	virtual void iterate()=0;
	void writeMembraneFiles();

	const IntVect& getFinestMeshSize()
	{
		return vectNxes[numLevels - 1];
	}
protected:
	SimulationExpression* simulation;

	int numLevels;
	int numUnknowns;
	Vector<int>  vectRefRatios;
	//ProblemDomain coarsestDomain;
	//RealVect  coarsestDx;
	//IntVect   coarsestNx;
	Vector<Vector<Box> > vectRefBoxes;
	
	ChomboSpec* chomboSpec;
	ChomboGeometry* chomboGeometry;
	BaseIF *geoIfs[NUM_PHASES];

	Vector<DisjointBoxLayout>  vectGrids;
	Vector< Vector< Vector<EBISLayout> > > vectEbis;
	Vector<ProblemDomain>  vectDomains;
	Vector<RealVect> vectDxes;
	Vector<IntVect> vectNxes;
	Vector< Vector< Vector< RefCountedPtr< LevelData< BaseIVFAB<int> > > > > > irregularPointMembraneIDs; // here it stores membrane index
	int findLevel(const ProblemDomain& domain);

	Vector< Vector< Vector<LevelData<EBCellFAB>*> > > volSoln;
	Vector< Vector< RefCountedPtr< LevelData< BaseIVFAB<Real> > > > > memSoln;
	Vector< RefCountedPtr< LevelData< BaseIVFAB<int> > > > membranePointIndexes; // here it stores membrane index
	IntVect numGhostSoln;

	Vector<EBAMRPoissonOpFactory *> opFactories;

	double* vectValues; // used to evaluate expressions
	int numSymbols;

	int hdf5FileCount;

	void updateSolution();

	int getChomboBoxLocalIndex(const IntVect& size, int ivar, const IntVect& ijk);
	int getChomboBoxLocalIndex(const IntVect& size, int ivar, D_DECL(int i, int j, int k));

	double getExpressionConstantValue(Variable* var, ExpressionIndex expIndex, Feature* feature);
	
	bool isInNextFinerLevel(int level, const IntVect& gridIndex);

	Vector< Vector<ConnectedComponent*> > phaseVolumeList;
	int numConnectedComponents;
	int numMembranePoints;
	bool computeOneFaceCross(int, int, int, RealVect&, RealVect&, RealVect&, RealVect&, RealVect&);
};

#endif