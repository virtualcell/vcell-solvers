/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef CARTESIANMESH_H
#define CARTESIANMESH_H

#include <vector>
#include <iostream>
#include <map>
using namespace std;

#include <VCELL/Element.h>
#include <VCELL/Contour.h>
#include <VCELL/ContourSubdomain.h>
#include <VCELL/Mesh.h>
#include <VCELL/SparseMatrixPCG.h>
#include <VCELL/IncidenceMatrix.h>
#include <VCELL/VoronoiRidge.h>

class Geometry;
class VolumeRegion;
class MembraneRegion;
class VolumeVariable;
class Particle;
class SparseMatrixPCG;

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

	virtual double getInsideOld(VolumeVariable *var, 
								MembraneElement *element);
	virtual double getOutsideOld(VolumeVariable *var, 
									MembraneElement *element);

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
	int getNumVolumeRegions()   { return (int)pVolumeRegions.size(); }
	int getNumMembraneRegions() { return (int)pMembraneRegions.size(); }
	MeshCoord getMeshCoord(long index);
									// regions	
	int getMembraneNeighborMask(long meindex);
	int getMembraneNeighborMask(MembraneElement* element);
	double* getMembraneFluxArea(long index);

private:	
	void setVolumeLists();
	void readGeometryFile(istream& ifs);
	void setBoundaryConditions();

	void initScale();

	long getIndex(MeshCoord);
	void findMembraneNeighbors();
	long orthoIndex(long insideIndex, long outsideIndex, long indexer, int boundMask);
	long getNeighbor(int n,  long index, int neighbor);

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
	    	   
	void writeCartesianMeshHeader(FILE *fp);
	void writeVolumeRegionsMapSubvolume(FILE *fp);
	void writeVolumeElementsMapVolumeRegion(FILE *fp);
	void writeMembraneRegionMapVolumeRegion(FILE *fp);
	void writeMembraneElements_Connectivity_Region(FILE *fp);
	void writeContourElements(FILE *fp);

	vector<VolumeRegion*>   pVolumeRegions;
	vector<MembraneRegion*> pMembraneRegions;	
	void computeMembraneCoupling(void);

	void computeExactNormals();
	WorldCoord computeExactNormal(long meIndex);

	void computeNormal(MembraneElement& meptr, DoubleVector3* normal, int neighborCount);
	void computeNormalsFromNeighbors(); 
	void adjustMembraneAreaFromNormal();

	void getN(long index, int* N);
	int computeN(int startingIndex, CurvePlane curvePlane, vector<double> curvex, vector<double> curvey, int currentMeIndexInVector, bool bClose);
	bool findCurve(int startingIndex, CurvePlane curvePlane, vector<double>& curvex, vector<double>& curvey, int& currentMEInVector);

	IncidenceMatrix<VoronoiRidge>* symmetrize(IncidenceMatrix<VoronoiRidge>* im, long N);
	void getNeighborCandidates (vector<long>& neighborCandidates, DoubleVector3 centralNormal, long index, int hierarchy);
};

#endif
