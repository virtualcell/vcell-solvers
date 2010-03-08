/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef MESH_H
#define MESH_H

#include <VCELL/SimTypes.h>
//#include <VCELL/Contour.h>
//#include <VCELL/ContourSubdomain.h>
#include <map>
using std::pair;
using std::map;

class Geometry;
class VolumeRegion;
class MembraneRegion;
class VolumeVariable;
class Particle;
class SparseMatrixPCG;
struct MembraneElement;
struct VolumeElement;

class Mesh
{
public:
	virtual double getVolumeOfElement_cu(long volumeIndex)=0;

	virtual WorldCoord getVolumeWorldCoord(long volumeIndex)=0;
	virtual WorldCoord getMembraneWorldCoord(long membraneIndex)=0;
	virtual WorldCoord getMembraneWorldCoord(MembraneElement *element)=0;
	//virtual long getVolumeIndex(WorldCoord coord)=0;

	//virtual double getInsideOld(VolumeVariable *var, MembraneElement *element)=0;
	//virtual double getOutsideOld(VolumeVariable *var, MembraneElement *element)=0;

	virtual void showSummary(FILE *fp) { fprintf(fp, "Mesh::showSummary()...\n"); }
	virtual void write(FILE *fp) = 0;
	virtual void writeMeshMetrics(FILE* fp) = 0;
	   
	VolumeElement *getVolumeElements();
	long getNumVolumeElements();
	MembraneElement *getMembraneElements();
	long getNumMembraneElements();
	int getDimension() { return dimension; }
	   
	//long getNumContourElements(){return (int)contourElements.size();}
	//ContourElement *getContourElement(long index);

	//void addElementToVolumeList(long volumeIndex, ContourElement *element);
	//vector<ContourElement*> getContourElementList(long index){return volumeLists[index];}
	//virtual void setVolumeLists()=0;

	//int getNumContourSubdomains(){return (int)pContourSubdomains.size();}
	//ContourSubdomain *getContourSubdomain(int i); //returns the ith
	SparseMatrixPCG* getMembraneCoupling();

	virtual int getMembraneNeighborMask(long meindex) = 0;
	virtual int getMembraneNeighborMask(MembraneElement* element) = 0;	
	virtual double* getMembraneFluxArea(long index) = 0;

protected:
	Mesh(double captureNeighborhood);
	   
	virtual ~Mesh();
	
	//virtual bool sampleContours(); // this should go away just like sampleGeometry()
	//void addContourSubdomain(ContourSubdomain *cs);

	VolumeElement          *pVolumeElement;
	MembraneElement        *pMembraneElement;
	//vector<ContourElement>  contourElements;
	long                    numMembrane;			 
	long                    numVolume;   
	int                     dimension;
	//vector<ContourSubdomain*> pContourSubdomains;
	//vector<ContourElement*>     *volumeLists;
	double                  captureNeighborhood;
	SparseMatrixPCG* membraneElementCoupling;
	virtual void computeMembraneCoupling(void) = 0;

	map<long, double*> membrane_boundary_flux;
};
   
#endif
