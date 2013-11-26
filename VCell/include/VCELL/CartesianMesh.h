/*
* (C) Copyright University of Connecticut Health Center 2001.
* All rights reserved.
*/
#ifndef CARTESIANMESH_H
#define CARTESIANMESH_H

#include <VCELL/Mesh.h>
#include <VCELL/DoubleVector3.h>
#include <vector>
#include <set>
#include <iostream>
#include <cassert>
using std::vector;
using std::istream;

class Geometry;
class VolumeRegion;
class MembraneRegion;
class VolumeVariable;
class Particle;
class SparseMatrixPCG;
template <class> class IncidenceMatrix;
struct VoronoiRidge;

enum NormalDirection {NORMAL_DIRECTION_ERROR = -1, NORMAL_DIRECTION_X = 111, NORMAL_DIRECTION_Y, NORMAL_DIRECTION_Z};
enum CurvePlane {CURVE_PLANE_XY = 222, CURVE_PLANE_XZ, CURVE_PLANE_YZ};
enum BoundaryLocation {BL_Xm = 0, BL_Xp, BL_Ym, BL_Yp, BL_Zm, BL_Zp};


class CartesianMesh : public Mesh
{
public:	   
	CartesianMesh(double captureNeighborhood=0);	
	void initialize(istream& ifs);

	virtual WorldCoord getVolumeWorldCoord(long volumeIndex);
	virtual WorldCoord getMembraneWorldCoord(long membraneIndex);
	virtual WorldCoord getMembraneWorldCoord(MembraneElement *element);
	virtual long getVolumeIndex(WorldCoord coord);

	virtual double getVolumeOfElement_cu(long volumeIndex);

	virtual void showSummary(FILE *fp);
	virtual void write(FILE *fp);
	virtual void writeMeshMetrics(FILE* fp);

	inline double getDomainSizeX() { return domainSizeX; }
	inline double getDomainSizeY() { return domainSizeY; }
	inline double getDomainSizeZ() { return domainSizeZ; }
	inline double getDomainOriginX() { return domainOriginX; }
	inline double getDomainOriginY() { return domainOriginY; }
	inline double getDomainOriginZ() { return domainOriginZ; }

	inline int  getNumVolumeX()    { return numX; }
	inline int  getNumVolumeY()    { return numY; }
	inline int  getNumVolumeZ()    { return numZ; }

	double getXScale_um()     { return scaleX_um; }
	double getYScale_um()     { return scaleY_um; }
	double getZScale_um()     { return scaleZ_um; }

	double getXArea_squm()    { return areaX_squm; }
	double getYArea_squm()    { return areaY_squm; }
	double getZArea_squm()    { return areaZ_squm; }

	double getVolume_cu()     { return volume_cu; }

	VolumeRegion   *getVolumeRegion(int i); 
	MembraneRegion *getMembraneRegion(int i); 
	int getNumVolumeRegions()   { return static_cast<int>(pVolumeRegions.size()); }
	int getNumMembraneRegions() { return static_cast<int>(pMembraneRegions.size()); }


	int getMembraneNeighborMask(long meindex);
	int getMembraneNeighborMask(MembraneElement* element);
	double* getMembraneFluxArea(long index);

	MeshCoord getMeshCoord(long index) {
	void asSigned();
		MeshCoord mc;
		mc.x = index % numX; 
		mc.y = (index / numX) % numY;
		mc.z = index/ numXY;
		return mc;        
	}

private:	
	//void setVolumeLists();
	void readGeometryFile(istream& ifs);
	void setBoundaryConditions();

	void initScale();

	//long getVolumeIndex(MeshCoord);
	void findMembraneNeighbors();
	typedef StatusIndex<long,NeighborType::NeighborStatus> NeighborIndex;
	NeighborIndex orthoIndex(long memIndex, long insideIndex, long outsideIndex, long indexer, int boundMask);
	NeighborIndex getNeighbor(int n,  long index, int neighbor);

	void findMembranePointInCurve(int n,  long index, int neighborDir, int& leftOverN, int& returnNeighbor);

	double domainSizeX;
	double domainSizeY;
	double domainSizeZ;
	double domainOriginX;
	double domainOriginY;
	double domainOriginZ;

	int      numX;
	int      numY;
	int      numZ;
	int      numXY;

	double   scaleX_um;
	double   scaleY_um;
	double   scaleZ_um;

	double   areaX_squm;
	double   areaY_squm;
	double   areaZ_squm;

	double   volume_cu;

	struct {
		int oppositeDirection;
		int nDirections;

	} membraneInfo;
	/**
	* return index of direction opposite "d"; value depends on number of dimensions
	*/
	int oppositeMembraneDirection(int d) const {
		return (d + membraneInfo.oppositeDirection) % membraneInfo.nDirections;
	}

	void writeCartesianMeshHeader(FILE *fp);
	void writeVolumeRegionsMapSubvolume(FILE *fp);
	void writeVolumeElementsMapVolumeRegion(FILE *fp);
	void writeMembraneRegionMapVolumeRegion(FILE *fp);
	void writeMembraneElements_Connectivity_Region(FILE *fp);
	//void writeContourElements(FILE *fp);

	vector<VolumeRegion*>   pVolumeRegions;
	vector<MembraneRegion*> pMembraneRegions;	
	void computeMembraneCoupling(void);

	void computeExactNormals();
	WorldCoord computeExactNormal(long meIndex);

	/**
	* set membrane element unit normal as average, unless too small, then default to feature normal
	* @param meptr to set
	* @param tangentNormals input to average 
	* @param numberOfNormals how many 
	*/
	void computeNormal(MembraneElement& meptr, const UnitVector3* tangentNormals, int numberOfNormals);
	void computeNormalsFromNeighbors(); 
	/**
	* compute specific normals
	* @return false if can't find pair of suitable neighbor tangents
	*/
	bool computeNormalsFromNeighbors(long index); 
	void adjustMembraneAreaFromNormal();

	/**
	* formerly getN
	* return number of steps in membrane direction direction to use to approximate normal;
	* it is a function of the curvature of the membrane 
	*/
	//review
	ArrayHolder<int,4> getNormalApproximationHops(const long index);

	int computeNormalApproximationHops(int startingIndex, CurvePlane curvePlane, vector<double> curvex, vector<double> curvey, int currentMeIndexInVector, bool bClose);
	bool findCurve(int startingIndex, CurvePlane curvePlane, vector<double>& curvex, vector<double>& curvey, int& currentMEInVector);

	IncidenceMatrix<VoronoiRidge>* symmetrize(IncidenceMatrix<VoronoiRidge>* im, long N);
	void getNeighborCandidates (vector<long>& neighborCandidates, DoubleVector3 centralNormal, long index, int hierarchy);

	/**
	* find normal direction between features of specified membrane element
	* @parm index of membrane element
	*/
	//review
	NormalDirection membraneElementFeatureDirection(int index) const ;

	UnitVector3 unitVectorBetween(long volumeIndexFrom, long volumeIndexTo);

};

#endif
