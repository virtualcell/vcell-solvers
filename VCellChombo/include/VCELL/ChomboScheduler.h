#ifndef CHOMBOSCHEDULER_H
#define CHOMBOSCHEDULER_H

//#include <vector>
#include <iostream>
#include <map>
using namespace std;

#include <VCELL/VarContext.h>
#include <Vector.H>
#include <RealVect.H>
#include <IntVect.H>
#include <BaseIF.H>
#include <Expression.h>
#include <VCELL/ChomboSpec.h>

class SimulationExpression;
class Membrane;
class Variable;
class Feature;
class ChomboGeometry;
class ChomboSpec;
class SymbolTable;
class ProblemDomain;
class EBCellFAB;
class EBISLayout;
template <class> class RefCountedPtr;
template <class> class LevelData;
class Box;
class EBAMRPoissonOpFactory;
class DisjointBoxLayout;
template <class> class BaseIVFAB;

#define NUM_PHASES 2
struct ConnectedComponent;
struct Triangle;

enum MembraneInvalidIndex
{
	MEMBRANE_INDEX_IN_FINER_LEVEL = -1,
};

class ChomboScheduler {

	friend class ChomboDomainBC;
	friend class ChomboEBBC;

public:
	ChomboScheduler(SimulationExpression* sim, ChomboSpec* chomboSpec);
	~ChomboScheduler();
	
	void initializeGrids();
	virtual void initValues()=0;

	void writeData(char* filename);
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

	const IntVect& getViewLevelMeshSize()
	{
		return vectNxes[chomboSpec->getViewLevel()];
	}
protected:
	SimulationExpression* simulation;
	VCell::Expression** refinementRoiExps;
	SymbolTable* refinementRoiSymbolTable;

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

	static void populateBoxDataType(hid_t& boxType);
	static void populateRealVectDataType(hid_t& realVectType);
	static void populateIntVectDataType(hid_t& intVectType);
	static void populateMembraneElementMetricsDataType(hid_t& metricsType);
	static void populateStructureMetricsDataType(hid_t& metricsType);
	static void populateVertexDataType(hid_t& metricsType);
	static void populateSegmentDataType(hid_t& triangleType);
	static void populateSliceViewDataType(hid_t& sliceViewType);
	static void populateTriangleDataType(hid_t& triangleType);
	Vector< map<int, int> > irregVolumeMembraneMap;

	int findNeighborMembraneIndex2D(int iphase, int ilev, const IntVect& gridIndex, int iedge, 
	const RealVect& normalizedCrossPoint, const RealVect& crossPointRealCoords, int& neighborEdge);
	
	static const int phase0;
	static const int phase1;
};

#endif
