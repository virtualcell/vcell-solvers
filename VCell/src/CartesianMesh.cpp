/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <assert.h>
#include <VCELL/CartesianMesh.h>
#include <VCELL/Element.h>
#include <VCELL/Feature.h>
#include <VCELL/FVUtils.h>
#include <VCELL/VolumeRegion.h>
#include <VCELL/MembraneRegion.h>
#include <VCELL/SimTool.h>
#include <VCELL/VCellModel.h>
#include <VCELL/MembraneVariable.h>
#include <VCELL/VolumeVariable.h>
#include <sstream>
#include <iomanip>
using namespace std;

#define MAXNEIGHBOR_2D 3
#define MAXNEIGHBOR_3D 20
#define GOING_OUT_LAYERS 3

#define NORMAL_MINLENGTH_THRESHOLD 1E-2

#define PI 3.1415926535897
//#define COMPUTE_EXACT_NORMALS
//#define SPECIAL_SHAPE

//#define CIRCLE
//#define HALFCIRCLE
//#define QUARTERCIRCLE
//#define ROSE
//#define ELLIPSOID
//#define SPHERE
//#define CYLINDER
//#define HALFCUBE
//#define CUBE_PLANE
//#define QUARTERSPHERE
//#define CYLINDER_IN_CUBE
//#define SQUARE
//#define ELLIPSE
//#define APPLE
//#define PERPENDICULAR_CYLINDER

#define COMPUTE_IMPROVED_NORMALS
#define WITH_PROJECTION_TO_WALL

#ifdef ELLIPSOID
//double a = 2.0, b = sqrt(2.0), c = 1;
double a = 3.0, b = 2.0, c = 1.0;
//double a = 3.0, b = 1.0, c = 1.0/3;

//double a2 = 4.0, b2 = 2.0, c2 = 1.0;
double a2 = 9, b2 = 4, c2 = 1.0;
//double a2 = 9, b2 = 1, c2 = 1.0/9;
#endif

extern "C"
{
#include "zlib.h"
}

CartesianMesh::CartesianMesh(char* arg_geoFile, double AcaptureNeighborhood)
: Mesh(AcaptureNeighborhood)
{
	geoFile = arg_geoFile;
	pVolumeRegions.erase(pVolumeRegions.begin(), pVolumeRegions.end());
	pMembraneRegions.erase(pMembraneRegions.begin(), pMembraneRegions.end());	
	
	if (!resolveFeatureReferences()){
		throw "CartesianMesh::resolveFeatureReferences() failed";
	}	
}

bool CartesianMesh::setVolumeLists()
{
	if(getDimension()==1){
		return true;
	}
	if(!getNumContourElements()){
		cout << "no contour elements exist" << endl;
		return true;
	}
    
	double distance =captureNeighborhood;
	volumeLists = new vector<ContourElement*>[numVolume];

	for(long volumeIndex=0;volumeIndex<numVolume;volumeIndex++){
		volumeLists[volumeIndex].clear();
	}
	for(long index=0;index<getNumContourElements(); index++){
		ContourElement *contourElement = getContourElement(index);
		long volumeIndex = contourElement->getVolumeIndex();
		WorldCoord wc_center = getVolumeWorldCoord(volumeIndex);
		FeatureHandle handle = pVolumeElement[volumeIndex].feature->getHandle();
		WorldCoord wc = wc_center;
		FeatureHandle currHandle;
		if(getDimension()==2){
			double deltaX = getXScale_um();
			double deltaY = getYScale_um();
			int sizeX = (int)ceil(distance/deltaX);
			int sizeY = (int)ceil(distance/deltaY);
			for(int i=-sizeX;i<sizeX+1;i++){
				wc.x = wc_center.x + deltaX*i;
				if((wc.x>=domainOriginX)&&(wc.x<=domainSizeX)){
					for(int j=-sizeY;j<sizeY+1;j++){
						wc.y = wc_center.y + deltaY*j;
						if((wc.y>=domainOriginY)&&(wc.y<=domainSizeY)){
							long currVolumeIndex = getVolumeIndex(wc);
							currHandle = pVolumeElement[currVolumeIndex].feature->getHandle();
							if(currHandle==handle){
								addElementToVolumeList(currVolumeIndex, contourElement);
							}  
						}
					}
				}  
			}
		} else if(getDimension()==3){
			double deltaX = getXScale_um();
			double deltaY = getYScale_um();
			double deltaZ = getZScale_um();
			int sizeX = (int)ceil(distance/deltaX);
			int sizeY = (int)ceil(distance/deltaY);
			int sizeZ = (int)ceil(distance/deltaZ);

			for(int i=-sizeX;i<sizeX+1;i++){
				wc.x = wc_center.x + deltaX*i;
				if((wc.x>=domainOriginX)&&(wc.x<=domainSizeX)){
					for(int j=-sizeY;j<sizeY+1;j++){
						wc.y = wc_center.y + deltaY*j;
						if((wc.y>=domainOriginY)&&(wc.y<=domainSizeY)){
							for(int k=-sizeZ;k<sizeZ+1;k++){
								wc.z = wc_center.z + deltaZ*k;
								if((wc.z>=domainOriginZ)&&(wc.z<=domainSizeZ)){
									long currVolumeIndex = getVolumeIndex(wc);
									currHandle = pVolumeElement[currVolumeIndex].feature->getHandle();
									if(currHandle==handle){
										addElementToVolumeList(currVolumeIndex, contourElement);
									}  
								}  
							}
						}
					}  
				}
			}	
		}
	}

	return true;
}

bool CartesianMesh::resolveFeatureReferences()
{
	if (pVolumeElement!=NULL) return true;

	readGeometryFile();
	initScale();

	if(!sampleContours()){
		printf("error in sampling contours\n");
	}
	if(!setVolumeLists()){
		printf("error in setting volumeLists\n");
	}
	
	printf("numVolume=%d\n",numVolume);

	setBoundaryConditions();

	if (!findMembraneNeighbors()){
		printf("error sorting membrane patches\n");
		return false;
	}

	adjustMembraneAreaFromNormal();

	return true;
}

unsigned char fromHex(unsigned char* src) {
	char chs[5];
	chs[0] = '0';
	chs[1] = 'x';
	chs[2] = src[0];
	chs[3] = src[1];
	chs[4] = 0;
	int v;
	sscanf(chs, "%x", &v);
	return (unsigned char)v;
}

void CartesianMesh::readGeometryFile() {
	if (geoFile == NULL) {
		throw "Geometry file doesn't exist!";
	}

	stringstream ss;
	cout << "Reading mesh from file '" << geoFile << "'" << endl;

	ifstream ifs(geoFile);
	if (!ifs.is_open()) {
		ss << "Can't open geometry file '" <<  geoFile << "'";
		throw ss.str();
	}
	char line[100000];
	string name;
	// name
	ifs.getline(line,100000);
	// dimension
	ifs.getline(line,100000);
	ss.clear();
	ss.str(line);
	ss >> name >> dimension;
	//size
	ifs.getline(line, 100000);
	ss.clear();
	ss.str(line);
	switch (dimension) {
		case 1:			
			ss >> name >> domainSizeX;
			domainSizeY = 1;
			domainSizeZ = 1;
			break;
		case 2:
			ss >> name >>  domainSizeX >> domainSizeY;
			domainSizeZ = 1;
			break;
		case 3:
			ss >> name >> domainSizeX >> domainSizeY >> domainSizeZ;
			break;
	}
	//origin
	ifs.getline(line, 100000);
	ss.clear();
	ss.str(line);
	switch (dimension) {
		case 1:
			ss >> name >> domainOriginX;
			domainOriginY = 0;
			domainOriginZ = 0;
			break;
		case 2:
			ss >> name >> domainOriginX >> domainOriginY;
			domainOriginZ = 0;
			break;
		case 3:
			ss >> name >> domainOriginX >> domainOriginY >> domainOriginZ;
			break;
	}
	//volumeRegions
	ifs.getline(line, 100000);	
	int numVolumeRegions;
	ss.clear();
	ss.str(line);
	ss >> name >> numVolumeRegions;

	for (int i = 0; i < numVolumeRegions; i ++) {		
		int fhi;
		double volume;
		ifs.getline(line, 100000);
		ss.clear();
		ss.str(line);
		ss >> name >> volume >> fhi;
		FeatureHandle fh = (FeatureHandle)fhi;
		VolumeRegion* vr = new VolumeRegion(this);
		vr->setId(i);
		vr->setName(name);		
		vr->setFeature(SimTool::getInstance()->getModel()->getFeature(fh));
		vr->setVolume(volume);				
		pVolumeRegions.push_back(vr);
	}
	//membraneRegions
	ifs.getline(line, 100000);	
	int numMembraneRegions;
	ss.clear();
	ss.str(line);
	ss >> name >> numMembraneRegions;

	for (int i = 0; i < numMembraneRegions; i ++) {		
		int inside, outside;
		double area;
		ifs.getline(line, 100000);
		ss.clear();
		ss.str(line);
		ss >> name >> area >> inside >> outside;
		MembraneRegion* mr = new MembraneRegion(this);	
		mr->setSurface(area);
		mr->setId(i);
		VolumeRegion* regionIn = pVolumeRegions[inside];
		VolumeRegion *regionOut = pVolumeRegions[outside];

		regionIn->addMembrane(mr);
		regionOut->addMembrane(mr);	
		if (regionIn->getFeature()->getPriority() < regionOut->getFeature()->getPriority()) {
			mr->setRegionInside(regionOut);  
			mr->setRegionOutside(regionIn);  
		} else {
			mr->setRegionInside(regionIn);  
			mr->setRegionOutside(regionOut);  
		}
		pMembraneRegions.push_back(mr);
	}

	//volumeSamples
	ifs.getline(line, 100000);
	ss.clear();
	ss.str(line);
	switch (dimension) {
		case 1:
			ss >> name >> numX;
			numY = 1;
			numZ = 1;
			break;
		case 2:
			ss >> name >>  numX >> numY;
			numZ = 1;
			break;
		case 3:
			ss >> name >> numX >> numY >> numZ;
			break;
	}

	numXY = numX * numY;
	numVolume = numXY * numZ;

	//volumeSamples compressed
	unsigned char* bytes_from_compressed = new unsigned char[numVolume+1000];
	memset(bytes_from_compressed, 0, (numVolume+1000) * sizeof(unsigned char));

	unsigned char* compressed_hex = new unsigned char[numVolume+1000];
	memset(compressed_hex, 0, (numVolume + 1000) * sizeof(unsigned char));
	ifs.getline((char*)compressed_hex, numVolume + 1000);	
	long compressed_len = ifs.gcount();

	if (compressed_len <= 1) {
		throw "CartesianMesh::readGeometryFile() : invalid compressed volume";
	}
	
	for (int i = 0, j = 0; i < compressed_len; i += 2, j ++) {
		bytes_from_compressed[j] = fromHex(compressed_hex + i);
	}

	unsigned char* inflated_bytes = new unsigned char[numVolume+1];
	memset(inflated_bytes, 0, (numVolume+1) * sizeof(unsigned char));

	unsigned long inflated_len = numVolume;
	int retVal = uncompress(inflated_bytes, &inflated_len, bytes_from_compressed, compressed_len/2);

	pVolumeElement = new VolumeElement[numVolume];
	for(i = 0; i < numVolume; i ++){
		int regionIndex = (unsigned char)inflated_bytes[i];
		VolumeRegion* vr = pVolumeRegions[regionIndex];
		vr->addIndex(i);
		pVolumeElement[i].feature = vr->getFeature();
		pVolumeElement[i].neighborMask = 0;
		pVolumeElement[i].regionIndex = (vr->getNumElements())-1; 
		pVolumeElement[i].region = vr;
	}	

	delete[] compressed_hex;
	delete[] bytes_from_compressed;
	delete[] inflated_bytes;

	// cells
	ifs.getline(line, 100000);
	ss.clear();
	ss.str(line);
	ss >> name >> numMembrane;
	pMembraneElement = new MembraneElement[numMembrane];

	// cell data
	int numFlipped = 0;

	for (int i = 0; i < numMembrane; i ++) {
		ifs.getline(line, 100000);
		int cellID, vindex1, vindex2;

		MembraneElement& memElement = pMembraneElement[i];
		ss.clear();
		ss.str(line);
		ss >> cellID >> vindex1 >> vindex2 >> memElement.area >> memElement.smoothedCoord.x >> memElement.smoothedCoord.y >> memElement.smoothedCoord.z 
			>> memElement.unitNormal.x >> memElement.unitNormal.y >> memElement.unitNormal.z;

		// inside/outside near
		if (pVolumeElement[vindex1].feature->getPriority() > pVolumeElement[vindex2].feature->getPriority()) {
			memElement.insideIndexNear = vindex1;
			memElement.outsideIndexNear = vindex2;
		} else {
			// flip the direction of the normal if we flip the inside and the outside volume elements
			memElement.insideIndexNear = vindex2;
			memElement.outsideIndexNear = vindex1;
			memElement.unitNormal = memElement.unitNormal.scale(-1);
			numFlipped ++;
		}		

		int iin = memElement.insideIndexNear;
		int oin = memElement.outsideIndexNear;
		memElement.index = i;
		memElement.feature = pVolumeElement[iin].feature;

		// for the purpose of finding neighbors of membrane elements later 
		pVolumeElement[iin].adjacentMembraneIndexes.push_back(memElement.index);
		pVolumeElement[oin].adjacentMembraneIndexes.push_back(memElement.index);
		
		for (int j = 0; j < (int)pMembraneRegions.size(); j ++) {
			if (pVolumeElement[iin].region->getId() == pMembraneRegions[j]->getRegionInside()->getId()
					&& pVolumeElement[oin].region->getId() == pMembraneRegions[j]->getRegionOutside()->getId()) {
				memElement.region = pMembraneRegions.at(j);
				pMembraneRegions[j]->addIndex(memElement.index);
				break;
			}
		}

		// try to figure out inside/outside far and neighbor mask of volume elements
		int possible[3];
		possible[0] = 1;
		possible[1] = numX;
		possible[2] = numXY;
		for (int j = 0; j < dimension; j ++) {
			int incandidate1 = iin + possible[j];
			int outcandidate1 = oin - possible[j];
			int incandidate2 = iin - possible[j];			
			int outcandidate2 = oin + possible[j];
			if (iin - oin == possible[j]) {
				memElement.insideIndexFar = incandidate1 < numVolume && pVolumeElement[incandidate1].feature->getHandle() == pVolumeElement[iin].feature->getHandle() ? incandidate1 : -1;
				memElement.outsideIndexFar = outcandidate1 >= 0 && pVolumeElement[outcandidate1].feature->getHandle() == pVolumeElement[oin].feature->getHandle() ? outcandidate1 : -1;				
				switch (j) {
					case 0:
						pVolumeElement[iin].neighborMask |= NEIGHBOR_XM_MEMBRANE;
						pVolumeElement[oin].neighborMask |= NEIGHBOR_XP_MEMBRANE;
						break;
					case 1:
						pVolumeElement[iin].neighborMask |= NEIGHBOR_YM_MEMBRANE;
						pVolumeElement[oin].neighborMask |= NEIGHBOR_YP_MEMBRANE;
						break;
					case 2:
						pVolumeElement[iin].neighborMask |= NEIGHBOR_ZM_MEMBRANE;
						pVolumeElement[oin].neighborMask |= NEIGHBOR_ZP_MEMBRANE;
						break;
				}
				break;
			} else if (iin - oin == -possible[j]) {
				memElement.insideIndexFar = incandidate2 >= 0  && pVolumeElement[incandidate2].feature->getHandle() == pVolumeElement[iin].feature->getHandle() ? incandidate2 : -1;
				memElement.outsideIndexFar = outcandidate2 < numVolume && pVolumeElement[outcandidate2].feature->getHandle() == pVolumeElement[oin].feature->getHandle() ? outcandidate2 : -1;
				switch (j) {
					case 0:
						pVolumeElement[iin].neighborMask |= NEIGHBOR_XP_MEMBRANE;
						pVolumeElement[oin].neighborMask |= NEIGHBOR_XM_MEMBRANE;
						break;
					case 1:
						pVolumeElement[iin].neighborMask |= NEIGHBOR_YP_MEMBRANE;
						pVolumeElement[oin].neighborMask |= NEIGHBOR_YM_MEMBRANE;
						break;
					case 2:
						pVolumeElement[iin].neighborMask |= NEIGHBOR_ZP_MEMBRANE;
						pVolumeElement[oin].neighborMask |= NEIGHBOR_ZM_MEMBRANE;
						break;
				}
				break;
			}			
		} 
	}
	cout << "------------>Total " << numMembrane << ", Flipped " << numFlipped << endl;
}

bool CartesianMesh::setBoundaryConditions()
{
	switch (dimension){
		case 1:{			
			long index;
			int boundaryType;

			index = 0;
			pVolumeElement[index].neighborMask |= NEIGHBOR_XM_BOUNDARY;
			boundaryType = pVolumeElement[index].feature->getXmBoundaryType();
			if (boundaryType == BOUNDARY_VALUE){
				pVolumeElement[index].neighborMask |= BOUNDARY_TYPE_DIRICHLET;      
			} else if (boundaryType == BOUNDARY_PERIODIC) {
				pVolumeElement[index].neighborMask |= BOUNDARY_TYPE_PERIODIC;
			} else if (boundaryType == BOUNDARY_FLUX) {
				pVolumeElement[index].neighborMask |= BOUNDARY_TYPE_NEUMANN;
			}
			pVolumeElement[index].neighborMask |= VOLUME_HALF;

			index = numX - 1;
			pVolumeElement[index].neighborMask |= NEIGHBOR_XP_BOUNDARY;
			boundaryType = pVolumeElement[index].feature->getXpBoundaryType();
			if (boundaryType == BOUNDARY_VALUE){
				pVolumeElement[index].neighborMask |= BOUNDARY_TYPE_DIRICHLET;      
			} else if (boundaryType == BOUNDARY_PERIODIC) {
				pVolumeElement[index].neighborMask |= BOUNDARY_TYPE_PERIODIC;
			} else if (boundaryType == BOUNDARY_FLUX) {
				pVolumeElement[index].neighborMask |= BOUNDARY_TYPE_NEUMANN;
			}
			pVolumeElement[index].neighborMask |= VOLUME_HALF;
			
			break;
		}
		case 2: {
			int boundaryType;
			long index;
			// set volume mask and boundary type for X boundaries.
			int xBoundary[2];
			xBoundary[0] = 0;
			xBoundary[1] = numX - 1;
			int j = 0;
			for (j = 0; j < numY; j ++){
				for (int i = 0; i < 2; i ++ ){		
					index = j * numX + xBoundary[i];
					if (i == 0) {
						pVolumeElement[index].neighborMask |= NEIGHBOR_XM_BOUNDARY;
						boundaryType = pVolumeElement[index].feature->getXmBoundaryType();
					} else {
						pVolumeElement[index].neighborMask |= NEIGHBOR_XP_BOUNDARY;
						boundaryType = pVolumeElement[index].feature->getXpBoundaryType();
					}
					if (boundaryType == BOUNDARY_VALUE){
						pVolumeElement[index].neighborMask |= BOUNDARY_TYPE_DIRICHLET;      
					} else if (boundaryType == BOUNDARY_PERIODIC) {
						pVolumeElement[index].neighborMask |= BOUNDARY_TYPE_PERIODIC;
					} else if (boundaryType == BOUNDARY_FLUX) {
						pVolumeElement[index].neighborMask |= BOUNDARY_TYPE_NEUMANN;
					}	
					if (j == 0 || j == numY - 1) {
						pVolumeElement[index].neighborMask |= VOLUME_QUARTER;
					} else {
						pVolumeElement[index].neighborMask |= VOLUME_HALF;
					}
				} // end i
			} // end j

			// set volume mask and boundary type for Y boundaries.
			int yBoundary[2];
			yBoundary[0] = 0;
			yBoundary[1] = numY - 1;
			for (j = 0; j < 2; j ++){				
				for (int i = 0 ; i < numX; i ++){
					index = yBoundary[j] * numX + i;
					if (j == 0) {						
						pVolumeElement[index].neighborMask |= NEIGHBOR_YM_BOUNDARY;
						boundaryType = pVolumeElement[index].feature->getYmBoundaryType();
					} else {
						pVolumeElement[index].neighborMask |= NEIGHBOR_YP_BOUNDARY;
						boundaryType = pVolumeElement[index].feature->getYpBoundaryType();
					}
					if (boundaryType == BOUNDARY_VALUE){
						pVolumeElement[index].neighborMask |= BOUNDARY_TYPE_DIRICHLET;      
					} else if (boundaryType == BOUNDARY_PERIODIC) {
						pVolumeElement[index].neighborMask |= BOUNDARY_TYPE_PERIODIC;
					} else if (boundaryType == BOUNDARY_FLUX) {
						pVolumeElement[index].neighborMask |= BOUNDARY_TYPE_NEUMANN;
					}
					if (i == 0 || i == numX - 1) {
						pVolumeElement[index].neighborMask |= VOLUME_QUARTER;
					} else {
						pVolumeElement[index].neighborMask |= VOLUME_HALF;
					}
				}// end i
			}// end j
			break;
		}
		case 3:{
			long index = 0;
			int boundaryCount;
			int boundaryType;

			// X direction
			int xBoundary[2];
			xBoundary[0] = 0;
			xBoundary[1] = numX - 1;
			int k = 0;
			for (k = 0; k < numZ; k ++){
				for (int j = 0; j < numY; j ++){
					for (int i = 0; i < 2; i ++){						
						boundaryCount = 1;
						if (k == 0 || k == numZ - 1) {
							boundaryCount ++;
						}
						if (j == 0 || j == numY - 1) {
							boundaryCount ++;
						}
						index = k * numXY + j * numX + xBoundary[i];
						if (i == 0) {
							pVolumeElement[index].neighborMask |= NEIGHBOR_XM_BOUNDARY;
							boundaryType = pVolumeElement[index].feature->getXmBoundaryType();
						} else {
							pVolumeElement[index].neighborMask |= NEIGHBOR_XP_BOUNDARY;
							boundaryType = pVolumeElement[index].feature->getXpBoundaryType();
						}

						if (boundaryType == BOUNDARY_VALUE){
							pVolumeElement[index].neighborMask |= BOUNDARY_TYPE_DIRICHLET;      
						} else if (boundaryType == BOUNDARY_PERIODIC) {
							pVolumeElement[index].neighborMask |= BOUNDARY_TYPE_PERIODIC;
						} else if (boundaryType == BOUNDARY_FLUX) {
							pVolumeElement[index].neighborMask |= BOUNDARY_TYPE_NEUMANN;
						}
						switch (boundaryCount){
							case 1:
								pVolumeElement[index].neighborMask |= VOLUME_HALF;
								break;
							case 2:
								pVolumeElement[index].neighborMask |= VOLUME_QUARTER;
								break;
							case 3:
								pVolumeElement[index].neighborMask |= VOLUME_EIGHTH;
								break;
						}
					}
				}
			}
			// Y direction
			int yBoundary[2];
			yBoundary[0] = 0;
			yBoundary[1] = numY - 1;
			for (k = 0; k < numZ; k ++){
				for (int j = 0; j < 2; j ++){
					for (int i = 0; i < numX; i ++){						
						boundaryCount = 1;
						if (k == 0 || k == numZ - 1) {
							boundaryCount ++;
						}
						if (i == 0 || i == numX - 1) {
							boundaryCount ++;
						}
						index = k * numXY + yBoundary[j] * numX + i;
						if (j == 0) {
							pVolumeElement[index].neighborMask |= NEIGHBOR_YM_BOUNDARY;
							boundaryType = pVolumeElement[index].feature->getYmBoundaryType();
						} else {
							pVolumeElement[index].neighborMask |= NEIGHBOR_YP_BOUNDARY;
							boundaryType = pVolumeElement[index].feature->getYpBoundaryType();
						}

						if (boundaryType == BOUNDARY_VALUE){
							pVolumeElement[index].neighborMask |= BOUNDARY_TYPE_DIRICHLET;      
						} else if (boundaryType == BOUNDARY_PERIODIC) {
							pVolumeElement[index].neighborMask |= BOUNDARY_TYPE_PERIODIC;
						} else if (boundaryType == BOUNDARY_FLUX) {
							pVolumeElement[index].neighborMask |= BOUNDARY_TYPE_NEUMANN;
						}
						switch (boundaryCount){
							case 1:
								pVolumeElement[index].neighborMask |= VOLUME_HALF;
								break;
							case 2:
								pVolumeElement[index].neighborMask |= VOLUME_QUARTER;
								break;
							case 3:
								pVolumeElement[index].neighborMask |= VOLUME_EIGHTH;
								break;
						}
					}
				}
			}

			// Z direction
			int zBoundary[2];
			zBoundary[0] = 0;
			zBoundary[1] = numZ - 1;
			for (k = 0; k < 2; k ++){
				for (int j = 0; j < numY; j ++){
					for (int i = 0; i < numX; i ++){						
						boundaryCount = 1;
						if (j == 0 || j == numY - 1) {
							boundaryCount ++;
						}
						if (i == 0 || i == numX - 1) {
							boundaryCount ++;
						}
						index = zBoundary[k] * numXY + j * numX + i;
						if (k == 0) {
							pVolumeElement[index].neighborMask |= NEIGHBOR_ZM_BOUNDARY;
							boundaryType = pVolumeElement[index].feature->getZmBoundaryType();
						} else {
							pVolumeElement[index].neighborMask |= NEIGHBOR_ZP_BOUNDARY;
							boundaryType = pVolumeElement[index].feature->getZpBoundaryType();
						}

						if (boundaryType == BOUNDARY_VALUE){
							pVolumeElement[index].neighborMask |= BOUNDARY_TYPE_DIRICHLET;      
						} else if (boundaryType == BOUNDARY_PERIODIC) {
							pVolumeElement[index].neighborMask |= BOUNDARY_TYPE_PERIODIC;
						} else if (boundaryType == BOUNDARY_FLUX) {
							pVolumeElement[index].neighborMask |= BOUNDARY_TYPE_NEUMANN;
						}
						switch (boundaryCount){
							case 1:
								pVolumeElement[index].neighborMask |= VOLUME_HALF;
								break;
							case 2:
								pVolumeElement[index].neighborMask |= VOLUME_QUARTER;
								break;
							case 3:
								pVolumeElement[index].neighborMask |= VOLUME_EIGHTH;
								break;
						}
					}// end i
				}// end j
			}// end k
			break;
		}
	}// end switch (getDimension())

	return true;
}

VolumeRegion* CartesianMesh::getVolumeRegion(int i){
	int numRegions = (int)pVolumeRegions.size();
	if ((i >= numRegions) || (i < 0)) {
		throw "CartesianMesh::getVolumeRegion(), index out of bound";
	}
	return pVolumeRegions[i]; 
}

MembraneRegion* CartesianMesh::getMembraneRegion(int i){
	int numRegions = (int)pMembraneRegions.size();
	if ((i >= numRegions) || (i <0)) {
		throw "CartesianMesh::getMembraneRegion(), index out of bound";
	}
	return pMembraneRegions[i]; 
}

double CartesianMesh::getInsideOld(VolumeVariable *var,
                                   MembraneElement *element)
{
	if (element->insideIndexFar<0){
		return var->getOld(element->insideIndexNear);
	}else{
		return 1.5 * var->getOld(element->insideIndexNear) -
				0.5 * var->getOld(element->insideIndexFar);
	}
}

double CartesianMesh::getOutsideOld(VolumeVariable *var,
                                    MembraneElement *element)
{
   if (element->outsideIndexFar<0){
      return var->getOld(element->outsideIndexNear);
   }else{
      return 1.5 * var->getOld(element->outsideIndexNear) -
             0.5 * var->getOld(element->outsideIndexFar);
   }
}

double CartesianMesh::getVolumeOfElement_cu(long volumeIndex)
{
	int mask = pVolumeElement[volumeIndex].neighborMask;
	double VOLUME = getVolume_cu();
	if(!(mask&VOLUME_MASK)){
		return VOLUME;
	}else{
		return VOLUME/(mask&VOLUME_MASK);
	}
}

void CartesianMesh::initScale()
{
	const double decimeter_per_um=1.0/100000.0;            

	scaleX_um = (numX>1) ? (domainSizeX/(numX-1)) : domainSizeX;
	scaleY_um = (numY>1) ? (domainSizeY/(numY-1)) : domainSizeY;
	scaleZ_um = (numZ>1) ? (domainSizeZ/(numZ-1)) : domainSizeZ;

	areaX_squm = scaleY_um * scaleZ_um;
	areaY_squm = scaleX_um * scaleZ_um;
	areaZ_squm = scaleX_um * scaleY_um;

	volume_cu = (scaleX_um*scaleY_um*scaleZ_um);
}

void CartesianMesh::showSummary(FILE *fp)
{
   fprintf(fp,"Cartesian Mesh: Domain    x:[%lg,%lg] y:[%lg,%lg] z:[%lg,%lg]\n",
		domainOriginX, domainOriginX+ domainSizeX, 
		domainOriginY, domainOriginY+ domainSizeY, 
		domainOriginZ, domainOriginZ+ domainSizeZ);
   fprintf(fp,"                Elements  numX=%ld numY=%ld numZ=%ld\n\n",
        numX,numY,numZ);
}

bool CartesianMesh::write(FILE *fp)
{
	//
	// 'Version 1.1' added membrane connectivity
	// 'Version 1.2' added regions
	//
	fprintf(fp,"Version 1.2\n");
	fprintf(fp,"CartesianMesh {\n");//Begin CartesianMesh
	//
	writeCartesianMeshHeader(fp);
	fprintf(fp,"\n");
	writeVolumeRegionsMapSubvolume(fp);
	fprintf(fp,"\n");
	writeMembraneRegionMapVolumeRegion(fp);
	fprintf(fp,"\n");
	writeVolumeElementsMapVolumeRegion(fp);
	fprintf(fp,"\n");
	writeMembraneElements_Connectivity_Region(fp);
	fprintf(fp,"\n");
	if(!writeContourElements(fp)){
		return false;
	}
	//
	fprintf(fp,"}\n");//End CartesianMesh
	return true;
}

void CartesianMesh::writeVolumeRegionsMapSubvolume(FILE *fp)
{
	int numVolumeRegions = getNumVolumeRegions();
	fprintf(fp,"\tVolumeRegionsMapSubvolume {\n");
	fprintf(fp,"\t%d\n",numVolumeRegions);
	fprintf(fp,"\t//%8s %10s %10s\n","VolRegID","SubvolID","Volume");
	for(int c = 0;c < numVolumeRegions;c+= 1){
		VolumeRegion *volumeRegion = getVolumeRegion(c);
		fprintf(fp,"\t%10ld %10ld %10.17lg //%s\n",
				volumeRegion->getId(),
				volumeRegion->getFeature()->getHandle(),
				volumeRegion->getVolume(),
				volumeRegion->getFeature()->getName().c_str());
	}
	fprintf(fp,"\t}\n");
}
void CartesianMesh::writeVolumeElementsMapVolumeRegion(FILE *fp)
{
	int numVolumeElements = getNumVolumeElements();
	VolumeElement *volumeElements = getVolumeElements();
	fprintf(fp,"\tVolumeElementsMapVolumeRegion {\n");
	if(numVolumeElements < 0){
		fprintf(fp,"\t%d UnCompressed",numVolumeElements);
		for(int c = 0;c < numVolumeElements;c+= 1){
			int volumeRegionId = volumeElements[c].region->getId();
			if(c%numX == 0){
				fprintf(fp,"\n\t");
			}
			fprintf(fp,"%3ld ",volumeRegionId);
		}
	}else{
		fprintf(fp,"\t%d Compressed",numVolumeElements);
		unsigned char *src = new unsigned char[numVolumeElements];
		for(int c = 0;c < numVolumeElements; c++){
			int volumeRegionId = volumeElements[c].region->getId();
			src[c] = (unsigned char)volumeRegionId;
		}
		unsigned long destLen = numVolumeElements*2;
		if (destLen<1000){
			destLen = 1000;  // minimum size compressed data buffer
		}
		unsigned char *dest = new unsigned char[destLen];
		int retVal = compress(dest,&destLen,src,numVolumeElements);
		for(int c = 0; c < (int)destLen; c++){
			if(c%40 == 0){
				fprintf(fp,"\n\t");
			}
			fprintf(fp,"%2.2X",dest[c]);
		}
		delete[] dest;
		delete[] src;

	}
	fprintf(fp,"\n\t}\n");
}

void CartesianMesh::writeMembraneRegionMapVolumeRegion(FILE *fp)
{
	int numMembraneRegions = getNumMembraneRegions();
	fprintf(fp,"\tMembraneRegionsMapVolumeRegion {\n");
	fprintf(fp,"\t%d\n",numMembraneRegions);
	fprintf(fp,"\t//%8s %10s %10s %10s\n","MemRegID","VolRegIn","VolRegOut","Surface");
	for(int c = 0;c < numMembraneRegions;c+= 1){
		MembraneRegion *membraneRegion = getMembraneRegion(c);
		VolumeRegion *vrInside = membraneRegion->getRegionInside();
		VolumeRegion *vrOutside= membraneRegion->getRegionOutside();
		fprintf(fp,"\t%10ld %10ld %10ld %10.17lg\n",
					membraneRegion->getId(),
					vrInside->getId(),
					vrOutside->getId(),
					membraneRegion->getSurface());
	}
	fprintf(fp,"\t}\n");
}

void CartesianMesh::writeCartesianMeshHeader(FILE *fp)
{
	fprintf(fp,"\t%s     %10s %10s %10s\n","//","X","Y","Z");
	fprintf(fp,"\tSize   %10ld %10ld %10ld\n",numX,numY,numZ);
	fprintf(fp,"\tExtent %10.17lg %10.17lg %10.17lg\n",domainSizeX,domainSizeY,domainSizeZ);
	fprintf(fp,"\tOrigin %10.17lg %10.17lg %10.17lg\n",domainOriginX,domainOriginY,domainOriginZ);
}

bool CartesianMesh::writeMeshMetrics(FILE *fp)
{
	fprintf(fp,"MembraneElements {\n");
	fprintf(fp,"%d\n",(int)getNumMembraneElements());
	fprintf(fp,"%5s %11s %17s %25s %25s %25s %25s %25s %25s\n","Index","RegionIndex","X","Y","Z","Area","Nx","Ny","Nz");
	for (int i=0;i<getNumMembraneElements();i++){
		MembraneElement *memEl = pMembraneElement + i;
		WorldCoord wc = getMembraneWorldCoord(i);
		fprintf(fp,"%-5ld %11ld %25.17lg %25.17lg %25.17lg %25.17lg %25.17lg %25.17lg %25.17lg\n",
			memEl->index,
			memEl->region->getId(),
			wc.x,
			wc.y,
			wc.z,
			memEl->area,
			memEl->unitNormal.x,
			memEl->unitNormal.y,
			memEl->unitNormal.z);
	}
	fprintf(fp,"}");
	return true;
}


void CartesianMesh::writeMembraneElements_Connectivity_Region(FILE *fp)
{
	fprintf(fp,"\tMembraneElements {\n");
	fprintf(fp,"\t%d\n",(int)getNumMembraneElements());
	fprintf(fp,"\t//%4s %4s %4s %4s %4s %4s %4s %4s\n","Indx","VIn","VOut","Conn0","Conn1","Conn2","Conn3","MemRegID");
	for (int i=0;i<getNumMembraneElements();i++){
		MembraneElement *memEl = getMembraneElements()+i;
		fprintf(fp,"\t%6ld %4ld %4ld %5ld %5ld %5ld %5ld %8d\n",
			memEl->index,
			memEl->insideIndexNear,
			memEl->outsideIndexNear,
			memEl->neighborMEIndex[0],
			memEl->neighborMEIndex[1],
			memEl->neighborMEIndex[2],
			memEl->neighborMEIndex[3],
			memEl->region->getId());

	}
	fprintf(fp,"    }\n");
}

bool CartesianMesh::writeContourElements(FILE *fp)
{
	//
	// write out contour elements (if present)
	//
	if (getNumContourElements()>0){
		fprintf(fp,"    ContourElements {\n");
		fprintf(fp,"           %d\n",(int)getNumContourElements());
		//
		// index volumeIndex begin.x begin.y begin.z  end.x, end.y end.z neighbor(prev) neighbor(next)
		//
		for (int i = 0; i < getNumContourElements(); i++){
			ContourElement *cEl = getContourElement(i);
			int neighborPrev = -1;
			int neighborNext = -1;
			if (cEl->getBorder() == CONTOUR_BEGIN){
				neighborPrev = -1;
				neighborNext = i+1;
			}else if (cEl->getBorder() == CONTOUR_END){
				neighborPrev = i-1;
				neighborNext = -1;
			}else if (cEl->getBorder() == CONTOUR_INTERIOR){
				neighborPrev = i-1;
				neighborNext = i+1;
			}else{
				printf("Error writing contour mesh, contour element(%ld) has an illegal ContourBorder type = %d\n",i,cEl->getBorder());
				return false;
			}
			fprintf(fp,"           %ld %ld %lg %lg %lg %lg %lg %lg %ld %ld\n",cEl->getElementIndex(),cEl->getVolumeIndex(),
						cEl->getBegin().x,cEl->getBegin().y,cEl->getBegin().z,
						cEl->getEnd().x,  cEl->getEnd().y,  cEl->getEnd().z,
						neighborPrev, neighborNext);
		}
		fprintf(fp,"    }\n");  // end ContourElements
	}
	return true;
}


WorldCoord CartesianMesh::getVolumeWorldCoord(long volumeIndex)
{
	MeshCoord mc = getMeshCoord(volumeIndex);
	WorldCoord unit, wc;

	unit.x = (numX>1)?(((double)(mc.x))/(numX-1)):0.5;
	unit.y = (numY>1)?(((double)(mc.y))/(numY-1)):0.5;
	unit.z = (numZ>1)?(((double)(mc.z))/(numZ-1)):0.5;

	wc.x = domainOriginX + domainSizeX*(unit.x);
	wc.y = domainOriginY + domainSizeY*(unit.y);
	wc.z = domainOriginZ + domainSizeZ*(unit.z);

	return wc; 
}

WorldCoord CartesianMesh::getMembraneWorldCoord(long membraneIndex)
{
	return getMembraneWorldCoord(pMembraneElement + membraneIndex);
}

WorldCoord CartesianMesh::getMembraneWorldCoord(MembraneElement *element)
{
	WorldCoord wc_membrane, wc_inside, wc_outside;

	long insideIndex = element->insideIndexNear;
	long outsideIndex = element->outsideIndexNear;

	wc_inside = getVolumeWorldCoord(insideIndex);
	wc_outside = getVolumeWorldCoord(outsideIndex);

	wc_membrane.x = (wc_inside.x + wc_outside.x)/2.0;
	wc_membrane.y = (wc_inside.y + wc_outside.y)/2.0;
	wc_membrane.z = (wc_inside.z + wc_outside.z)/2.0;

	return wc_membrane; 
}

long CartesianMesh::getVolumeIndex(WorldCoord coord)
{
	MeshCoord meshCoord;

	ASSERTION(coord.x>=domainOriginX);
	ASSERTION(coord.x<=(domainOriginX + domainSizeX));
	ASSERTION(coord.y>=domainOriginY);
	ASSERTION(coord.y<=(domainOriginY + domainSizeY));
	ASSERTION(coord.z>=domainOriginZ);
	ASSERTION(coord.z<=(domainOriginZ + domainSizeZ));

	meshCoord.x = (int)((coord.x-domainOriginX) * (numX-1)/domainSizeX + 0.5);
	meshCoord.y = (int)((coord.y-domainOriginY) * (numY-1)/domainSizeY + 0.5);
	meshCoord.z = (int)((coord.z-domainOriginZ) * (numZ-1)/domainSizeZ + 0.5);

	return getIndex(meshCoord);
}

MeshCoord CartesianMesh::getMeshCoord(long index)
{
	MeshCoord mc;
	mc.x = index%numX; 
	mc.y = (index/numX)%numY;
	mc.z = index/(numXY);
	return mc;        
}

long CartesianMesh::getIndex(MeshCoord coord)
{
	return coord.x + numX*(coord.y + numY*coord.z);
}


double getExactVolume() {
#if (defined(SPHERE))
	return 4 * PI;

#elif (defined(QUARTERSPHERE))
	return PI/2;

#elif (defined(SQUARE))
	return 4;

#elif (defined(ELLIPSE)) 
	return 0;

#elif (defined(ELLIPSOID))
	double m = a2 * (b2 - c2)/(b2 * (a2 - c2)) ;
	double theta = asin(sqrt(1-c2/a2));

	int N = 10000000;

	double sum1 = 0;
	double sum2 = 0;
	for (int i = 0; i <= N; i ++) {
		double phi = (theta * i)/N;
		double sinphi =  sin(phi);
		double d = sqrt(1 - m * sinphi * sinphi);
		sum1 += d;
		sum2 += 1/d;
	}
	double EllipticE = sum1 * theta /(N+1);
	double EllipticF = sum2 * theta /(N+1);	

	double d = sqrt(a2 - c2);
	return 2 * PI * (c2 + b * c2 * EllipticF / d + b * d * EllipticE);

#elif (defined(HALFCUBE)) 
	return 1.0 * 1.0;
#elif (defined(CIRCLE)) 
	return 2 * PI;
#elif (defined(HALFCIRCLE)) 
	return PI;
#elif (defined(QUARTERCIRCLE))
	return PI/2;
#elif (defined(ROSE)) 
	return 19.2680242763864;
#elif (defined(CUBE_PLANE))
	return 0.62360956446232352;
#elif (defined(CYLINDER_IN_CUBE))
	return 8 * sqrt(6.0) * PI;
#elif (defined(PERPENDICULAR_CYLINDER)) 
	return 2 * PI * PI;
#endif
	return 0.0;
}

WorldCoord CartesianMesh::computeExactNormal(long meIndex) {
	MembraneElement& meptr = pMembraneElement[meIndex];
	WorldCoord wc = getMembraneWorldCoord(&meptr);
#if (defined(SPHERE) || defined(QUARTERSPHERE))
	wc.normalize();
	return wc;

#elif (defined(ROSE)) 
	return wc;
#elif (defined(APPLE))
	double R = sqrt(wc.x*wc.x + wc.y*wc.y) + 1E-12;
	double R3  = R*R*R;
	WorldCoord normal(wc.x/R -wc.y*wc.y/R3, wc.y/R + wc.x * wc.y/R3, 0);
	normal.normalize();
	return normal;

#elif ( defined(CIRCLE) || defined(HALFCIRCLE) || defined(QUARTERCIRCLE)) 
	wc.z = 0;
	wc.normalize();
	return wc;
#elif (defined(SQUARE)) 
	WorldCoord normal = getVolumeWorldCoord(meptr.outsideIndexNear) - getVolumeWorldCoord(meptr.insideIndexNear);
	normal.normalize();
	return normal;

#elif (defined(CYLINDER))
	if (meptr.outsideIndexNear - meptr.insideIndexNear == numXY) {
		return WorldCoord(0, 0, 1)
	} else if (meptr.outsideIndexNear - meptr.insideIndexNear == -numXY){
		return WorldCoord(0, 0, -1)
	} else {
		wc.z = 0;
		wc.normalize();
		return wc;
	}
#elif (defined(ELLIPSOID))
	WorldCoord normal(2 * wc.x/a2, 2 * wc.y/b2, 2 * wc.z/c2);
	normal.normalize();
	return normal;
#elif (defined(ELLIPSE)) 
	WorldCoord normal(2 * wc.x/4, 2 * wc.y/1, 0);
	normal.normalize();
	return normal;

#elif (defined(HALFCUBE))
	return WorldCoord(0,0,1);
#elif (defined(CUBE_PLANE))
	double ok = sqrt(14.0);
	return WorldCoord(-2/ok, -1/ok, 3/ok);
#elif (defined(CYLINDER_IN_CUBE))
	double xp = wc.x, yp = wc.y, zp = wc.z;
	double v1 = -2/sqrt(6.0), v2 = 1/sqrt(6.0), v3 = 1/sqrt(6.0);
	double lambdap= (-2*xp+yp+zp)/sqrt(6.0);
	double rp = sqrt(xp*xp+yp*yp+zp*zp - lambdap*lambdap);
	return WorldCoord( (xp-lambdap*v1)/rp , (yp-lambdap*v2)/rp , (zp-lambdap*v3)/rp );
#elif (defined(PERPENDICULAR_CYLINDER)) 
	double d = sqrt(wc.y * wc.y + wc.z * wc.z);
	return WorldCoord(0, wc.y/d, wc.z/d);
#else
	throw "Computing exact normals is not supported for arbitrary geometry!";
#endif	
}

void CartesianMesh::computeExactNormals() {
	cout << "CartesianMesh::computeExactNormals(), compute exact normals for special shape" << endl;
#if (defined(COMPUTE_EXACT_NORMALS) && defined(SPECIAL_SHAPE))
	for (int i = 0; i < numMembrane; i ++) {
		MembraneElement& meptr = pMembraneElement[i];
		meptr.unitNormal = computeExactNormal(i);
	}
#else
	throw "Computing exact normals is not supported for arbitrary geometry!";
#endif
}

void CartesianMesh::computeNormalsFromNeighbors() {
	//
	//averaging over neighbors of arbitrary number  
	//
	if (dimension < 2) {
		return;
	}
	cout << "CartesianMesh::computeNormalsFromNeighbors(), compute normals from neighbors" << endl;

	int numOfNaturalNeighbors = 0;
	if (dimension == 2) {
		numOfNaturalNeighbors = 2;
	} else if (dimension == 3) {
		numOfNaturalNeighbors = 4;
	}

	int* tangentN = new int[numOfNaturalNeighbors];
	int* tangentNeighbors = new int[numOfNaturalNeighbors];
	DoubleVector3* tangentWc = new DoubleVector3[numOfNaturalNeighbors];
	DoubleVector3* tangentNormal = new DoubleVector3[numOfNaturalNeighbors];
	
	for (long index = 0; index < numMembrane; index ++){
		WorldCoord wc = getMembraneWorldCoord(index);
		MembraneElement& meptr = pMembraneElement[index];
		
		getN(index, tangentN);

		while (true) {
			int numOfValidTangentNeighbors = 0;
			for (int i = 0; i < numOfNaturalNeighbors; i ++) {
				tangentNeighbors[i] = getNeighbor(tangentN[i], index, i);
				if (tangentNeighbors[i] >= 0 && tangentNeighbors[i] != index) { 
					numOfValidTangentNeighbors ++;
	 				tangentWc[i] = wc - getMembraneWorldCoord(tangentNeighbors[i]);
				} else {
					tangentNeighbors[i] = -1;
				}
			}

			if (numOfValidTangentNeighbors == 0) {  // if there are no neighbors at all, use face normal
				meptr.unitNormal = getVolumeWorldCoord(meptr.outsideIndexNear) - getVolumeWorldCoord(meptr.insideIndexNear);
				meptr.unitNormal.normalize();
				break;
			}
		
			int tangentNormalCount = 0;
			memset(tangentNormal, 0, numOfNaturalNeighbors * sizeof(DoubleVector3));
	
			// compute tangent normals
			if (dimension == 2) {
				if(tangentNeighbors[0] >= 0){
					tangentNormal[0].x = tangentWc[0].y;
					tangentNormal[0].y = - tangentWc[0].x;
					double length = tangentNormal[0].length();
					if (length > 0) {
						tangentNormal[0].normalize();
						tangentNormalCount ++;	 
					}
				}
				if(tangentNeighbors[1] >= 0){
					tangentNormal[1].x = -tangentWc[1].y;
					tangentNormal[1].y = tangentWc[1].x;
					double length = tangentNormal[1].length();
					if (length > 0) {
						tangentNormal[1].normalize();
						tangentNormalCount ++;	 
					}	 
				}
			} else if (dimension == 3) {
				for (int i = 0; i < numOfNaturalNeighbors; i ++) {
					int nextIndex = (i+1) % numOfNaturalNeighbors;
					if((tangentNeighbors[i] >= 0) && (tangentNeighbors[nextIndex] >= 0)){							
						tangentNormal[i].x = tangentWc[i].y * tangentWc[nextIndex].z - tangentWc[nextIndex].y * tangentWc[i].z;
						tangentNormal[i].y = tangentWc[i].z * tangentWc[nextIndex].x - tangentWc[nextIndex].z * tangentWc[i].x;
						tangentNormal[i].z = tangentWc[i].x * tangentWc[nextIndex].y - tangentWc[nextIndex].x * tangentWc[i].y;
						double length = tangentNormal[i].length();
						if (length > 0){
							tangentNormal[i].normalize();
							tangentNormalCount ++;
						}
					}
				}
			}

			// average tangent normals
			if (tangentNormalCount > 0) {
				computeNormal(meptr, tangentNormal, tangentNormalCount);
				break;
			} else {
				int numChanged = 0;
				for (int i = 0; i < numOfNaturalNeighbors; i ++) {
					if (tangentN[i] > 1) {
						tangentN[i] --;
						numChanged ++;
					}
				}
				if (numChanged == 0) {
					throw "Mesh is too coarse, found isolated membrane element, please try to refine mesh";
				}
			}
		}		
	}

	delete[] tangentN;
	delete[] tangentNeighbors;
	delete[] tangentWc;
	delete[] tangentNormal;	
		//if (tangentNeighborCount == 1) { // there is only one valid neighbor
		//	assert(validNeighborIndex != -1);
		//	NormalDirection nd = getFaceNormalDirection(index);
		//	WorldCoord v1;
		//	if (nd == NORMAL_DIRECTION_X) {
		//		v1.x = 1;
		//	} else if (nd == NORMAL_DIRECTION_Y) {
		//		v1.y = 1;
		//	} else if (nd == NORMAL_DIRECTION_Z) {
		//		v1.z = 1;
		//	}
		//	tangentWc[validNeighborIndex].normalize();
		//	pMembraneElement[index].unitNormal = tangentWc[validNeighborIndex].crossProduct(v1);
		//	continue;
		//} else if (tangentNeighborCount == 2 &&  // there are two neighbors, but they are coplanar;
		//		(tangentNeighbors[0] == -1 && tangentNeighbors[2] == -1 || tangentNeighbors[1] == -1 && tangentNeighbors[3] == -1)) {
		//	NormalDirection nd = getFaceNormalDirection(index);
		//	WorldCoord v1;
		//	if (nd == NORMAL_DIRECTION_X) {
		//		v1.x = 1;
		//	} else if (nd == NORMAL_DIRECTION_Y) {
		//		v1.y = 1;
		//	} else if (nd == NORMAL_DIRECTION_Z) {
		//		v1.z = 1;
		//	}
		//	// make up a third point to 
		//	if (tangentNeighbors[0] == -1 && tangentNeighbors[2] == -1) {
		//		tangentWc[0] = v1;
		//		tangentWc[1].normalize();
		//		tangentWc[3].normalize();
		//	} else {
		//		tangentWc[1] = v1;
		//		tangentWc[0].normalize();
		//		tangentWc[2].normalize();
		//	}
		//}		
}

void CartesianMesh::computeNormal(MembraneElement& meptr, DoubleVector3* normal, int neighborCount) {
	switch (dimension) {
		case 2: {
			// average normal
			int cnt = 2;
			meptr.unitNormal.x = 0;
			for (int i = 0; i < cnt; i ++) {
				meptr.unitNormal.x += normal[i].x;
			} 
			meptr.unitNormal.x /= neighborCount;

			meptr.unitNormal.y = 0;
			for (int i = 0; i < cnt; i ++) {
				meptr.unitNormal.y += normal[i].y;
			} 
			meptr.unitNormal.y /= neighborCount;

			meptr.unitNormal.z = 0; // 2D
			break;
		}
		case 3: {
			// average normal
			int cnt = 4;
			meptr.unitNormal.x = 0;
			for (int i = 0; i < cnt; i ++) {
				meptr.unitNormal.x += normal[i].x;
			} 
			meptr.unitNormal.x /= neighborCount;

			meptr.unitNormal.y = 0;
			for (int i = 0; i < cnt; i ++) {
				meptr.unitNormal.y += normal[i].y;
			} 
			meptr.unitNormal.y /= neighborCount;

			meptr.unitNormal.z = 0;
			for (int i = 0; i < cnt; i ++) {
				meptr.unitNormal.z += normal[i].z;
			} 
			meptr.unitNormal.z /= neighborCount;			

			break;
		}
		default: {
			throw "CartesianMesh::computeNormal(), dimension should be 2 or 3.";
		}
	}

	// overwriting the normal computed by neighborhood if unnormalized length is much smaller than 1.
	// under this condition, the direction is meaningless and we use staircase normal.
	double normalLength = meptr.unitNormal.length();
	if (normalLength <= NORMAL_MINLENGTH_THRESHOLD) {	
		meptr.unitNormal = getVolumeWorldCoord(meptr.outsideIndexNear) - getVolumeWorldCoord(meptr.insideIndexNear);
	} 
	meptr.unitNormal.normalize();
}

void CartesianMesh::adjustMembraneAreaFromNormal(){

#if (defined(COMPUTE_EXACT_NORMALS) && defined(SPECIAL_SHAPE))
	computeExactNormals(); 
#elif (defined(COMPUTE_IMPROVED_NORMALS))
	computeNormalsFromNeighbors();
#endif

	for (int memIndex = 0; memIndex < numMembrane; memIndex ++) {	
		MembraneElement& element = pMembraneElement[memIndex];
		long diff = labs(element.outsideIndexNear - element.insideIndexNear);
		int mask = getMembraneNeighborMask(&element);

		switch (dimension) {
			case 2:
				if (diff==1){
					// membrane element is aligned with the Y axis
					element.area = scaleY_um * fabs(element.unitNormal.x);
				} else if (diff==numX){
					// membrane element is aligned with the X axis
					element.area = scaleX_um * fabs(element.unitNormal.y);
				} else {
					throw "CartesianMesh::adjustMembraneAreaFromNormal(), diff is neither 1 nor numX";
				}	
				// correcting for egde or corner			
				if (mask & NEIGHBOR_X_BOUNDARY_MASK) {
					element.area *= 0.5;
				}
				if (mask & NEIGHBOR_Y_BOUNDARY_MASK) {
					element.area *= 0.5;
				}
				break;
			case 3:
				if (diff==1){
					// membrane element is in YZ plane
					element.area = scaleZ_um * scaleY_um * fabs(element.unitNormal.x);
				} else if (diff==numX){
					// membrane element is in XZ plane
					element.area = scaleX_um * scaleZ_um * fabs(element.unitNormal.y);
				} else if (diff == numXY) {
					// membrane element is in XY plane
					element.area =  scaleX_um * scaleY_um * fabs(element.unitNormal.z);
				} else {
					throw "CartesianMesh::adjustMembraneAreaFromNormal(), diff is neither 1 nor numX nor numXY";
				}	
				if (mask & NEIGHBOR_X_BOUNDARY_MASK) {
					element.area *= 0.5;
				}
				if (mask & NEIGHBOR_Y_BOUNDARY_MASK) {
					element.area *= 0.5;
				}
				if (mask & NEIGHBOR_Z_BOUNDARY_MASK) {
					element.area *= 0.5;
				}
				break;
		}
	}
	if (dimension == 2 || dimension == 3) {
		computeMembraneCoupling();
	}
}

bool CartesianMesh::findMembraneNeighbors()
{
	long mecount = getNumMembraneElements();
	long mex = getNumVolumeX();
	long mey = getNumVolumeY();
	long mez = getNumVolumeZ();
	MembraneElement* meptr = getMembraneElements();
	long meloop;

	for(meloop = 0;meloop < mecount;meloop+= 1)
	{
		meptr[meloop].neighborMEIndex[0] = -1;
		meptr[meloop].neighborMEIndex[1] = -1;
		meptr[meloop].neighborMEIndex[2] = -1;
		meptr[meloop].neighborMEIndex[3] = -1;
	}

	for(meloop = 0;meloop < mecount;meloop+= 1)
	{
		long iinloop,oinloop;

		iinloop = meptr[meloop].insideIndexNear;
		oinloop = meptr[meloop].outsideIndexNear;

		if(getDimension() == 2)
		{
			if(labs(iinloop - oinloop) == 1)
			{
				meptr[meloop].neighborMEIndex[0] = orthoIndex(iinloop,oinloop,mex,NEIGHBOR_YP_BOUNDARY);
				meptr[meloop].neighborMEIndex[1] = orthoIndex(iinloop,oinloop,-mex,NEIGHBOR_YM_BOUNDARY);
			}
			else if (abs(iinloop - oinloop) == mex)
			{
				meptr[meloop].neighborMEIndex[0] = orthoIndex(iinloop,oinloop,1,NEIGHBOR_XP_BOUNDARY);
				meptr[meloop].neighborMEIndex[1] = orthoIndex(iinloop,oinloop,-1,NEIGHBOR_XM_BOUNDARY);
			}
		}
		else if (getDimension() == 3)
		{
			if(labs(iinloop - oinloop) == 1)
			{
				meptr[meloop].neighborMEIndex[0] = orthoIndex(iinloop,oinloop,mex,NEIGHBOR_YP_BOUNDARY);
				meptr[meloop].neighborMEIndex[1] = orthoIndex(iinloop,oinloop,-mex*mey,NEIGHBOR_ZM_BOUNDARY);
				meptr[meloop].neighborMEIndex[2] = orthoIndex(iinloop,oinloop,-mex,NEIGHBOR_YM_BOUNDARY);
				meptr[meloop].neighborMEIndex[3] = orthoIndex(iinloop,oinloop,mex*mey,NEIGHBOR_ZP_BOUNDARY);
			}
			else if (labs(iinloop - oinloop) == mex)
			{
				meptr[meloop].neighborMEIndex[0] = orthoIndex(iinloop,oinloop,1,NEIGHBOR_XP_BOUNDARY);
				meptr[meloop].neighborMEIndex[1] = orthoIndex(iinloop,oinloop,-mex*mey,NEIGHBOR_ZM_BOUNDARY);
				meptr[meloop].neighborMEIndex[2] = orthoIndex(iinloop,oinloop,-1,NEIGHBOR_XM_BOUNDARY);
				meptr[meloop].neighborMEIndex[3] = orthoIndex(iinloop,oinloop,mex*mey,NEIGHBOR_ZP_BOUNDARY);
			}
			else if (labs(iinloop - oinloop) == (mex*mey))
			{
				meptr[meloop].neighborMEIndex[0] = orthoIndex(iinloop,oinloop,mex,NEIGHBOR_YP_BOUNDARY);
				meptr[meloop].neighborMEIndex[1] = orthoIndex(iinloop,oinloop,-1,NEIGHBOR_XM_BOUNDARY);
				meptr[meloop].neighborMEIndex[2] = orthoIndex(iinloop,oinloop,-mex,NEIGHBOR_YM_BOUNDARY);
				meptr[meloop].neighborMEIndex[3] = orthoIndex(iinloop,oinloop,1,NEIGHBOR_XP_BOUNDARY);
			}
		}
		else return true;

	}
	return true;
}

int CartesianMesh::getMembraneNeighborMask(long meindex) {
	return getMembraneNeighborMask(pMembraneElement + meindex);
}

int CartesianMesh::getMembraneNeighborMask(MembraneElement* element) {	
	Feature* feature = pVolumeElement[element->insideIndexNear].feature;
	int A = pVolumeElement[element->insideIndexNear].neighborMask;
	int B = pVolumeElement[element->outsideIndexNear].neighborMask;
	int tentativemask =  (A & B & NEIGHBOR_BOUNDARY_MASK);	
	if (((tentativemask & NEIGHBOR_XM_BOUNDARY) && (feature->getXmBoundaryType() == BOUNDARY_VALUE)) 
			|| ((tentativemask & NEIGHBOR_XP_BOUNDARY) && (feature->getXpBoundaryType() == BOUNDARY_VALUE))
			|| ((tentativemask & NEIGHBOR_YM_BOUNDARY) && (feature->getYmBoundaryType() == BOUNDARY_VALUE))
			|| ((tentativemask & NEIGHBOR_YP_BOUNDARY) && (feature->getYpBoundaryType() == BOUNDARY_VALUE))
			|| ((tentativemask & NEIGHBOR_ZM_BOUNDARY) && (feature->getZmBoundaryType() == BOUNDARY_VALUE)) 
			|| ((tentativemask & NEIGHBOR_ZP_BOUNDARY) && (feature->getZpBoundaryType() == BOUNDARY_VALUE))) {
			tentativemask |= BOUNDARY_TYPE_DIRICHLET;
	} else if (((tentativemask & NEIGHBOR_XM_BOUNDARY) && (feature->getXmBoundaryType() == BOUNDARY_PERIODIC)) 
			|| ((tentativemask & NEIGHBOR_XP_BOUNDARY) && (feature->getXpBoundaryType() == BOUNDARY_PERIODIC))
			|| ((tentativemask & NEIGHBOR_YM_BOUNDARY) && (feature->getYmBoundaryType() == BOUNDARY_PERIODIC))
			|| ((tentativemask & NEIGHBOR_YP_BOUNDARY) && (feature->getYpBoundaryType() == BOUNDARY_PERIODIC))
			|| ((tentativemask & NEIGHBOR_ZM_BOUNDARY) && (feature->getZmBoundaryType() == BOUNDARY_PERIODIC)) 
			|| ((tentativemask & NEIGHBOR_ZP_BOUNDARY) && (feature->getZpBoundaryType() == BOUNDARY_PERIODIC))) {
			tentativemask |= BOUNDARY_TYPE_PERIODIC;
	} else {
		tentativemask |= BOUNDARY_TYPE_NEUMANN;
	}

	return tentativemask;
	
}

long CartesianMesh::orthoIndex(long iinloop,long oinloop,long indexer,int bmask)
{
	long iinsearch,oinsearch;
	MembraneElement* meptr = getMembraneElements();
	const int NO_CONNECTING_NEIGHBOR = -1;
	VolumeElement* veptr = getVolumeElements();

	//If we are at the world boundary, there are no neighbors
	if((getVolumeElements()[iinloop].neighborMask & bmask) != 0){
		return NO_CONNECTING_NEIGHBOR;
	}

	//See if we're at a place where membranes may overlap (not allowed).  More than two feature types abut
	Feature* featureMasterInside = veptr[iinloop].feature;
	Feature* featureMasterOutside = veptr[oinloop].feature;
	Feature* featureCompareInside = veptr[iinloop+indexer].feature;
	Feature* featureCompareOutside = veptr[oinloop+indexer].feature;
	if(((featureCompareInside != featureMasterInside) && (featureCompareInside != featureMasterOutside)) ||
		((featureCompareOutside != featureMasterInside) && (featureCompareOutside != featureMasterOutside))){
		//More than 2 feature types abut here, we do not connect even if there are potential neighbors
		return NO_CONNECTING_NEIGHBOR;
	}

	// two membrane elements share the same inside near
	vector<long>& v1 = pVolumeElement[iinloop].adjacentMembraneIndexes;
	for (vector<long>::iterator iter1 = v1.begin(); iter1 != v1.end(); iter1 ++) {
		long mesearch = *iter1;

		iinsearch = meptr[mesearch].insideIndexNear;
		oinsearch = meptr[mesearch].outsideIndexNear;

		if (iinsearch != iinloop) {
			continue;
		}

		if(oinsearch == (iinloop+indexer)){
			return mesearch;
		}
	}

	// two membrane elements the same outside near
	vector<long>& v2 = pVolumeElement[oinloop].adjacentMembraneIndexes;
	for (vector<long>::iterator iter2 = v2.begin(); iter2 != v2.end(); iter2 ++) {
		long mesearch = *iter2;

		iinsearch = meptr[mesearch].insideIndexNear;
		oinsearch = meptr[mesearch].outsideIndexNear;

		if (oinsearch != oinloop) {
			continue;
		}

		if(iinsearch == (oinloop+indexer)){
			return mesearch;
		}
	}	

	// two membrane elements are connected as a straight line
	if (pVolumeElement[iinloop+indexer].feature->getPriority() != pVolumeElement[oinloop+indexer].feature->getPriority()) {
		vector<long>& v3 = pVolumeElement[iinloop + indexer].adjacentMembraneIndexes;
		vector<long>& v4 = pVolumeElement[oinloop + indexer].adjacentMembraneIndexes;
		for (vector<long>::iterator iter3 = v3.begin(); iter3 != v3.end(); iter3 ++) {
			long mi = *iter3;			
			for (vector<long>::iterator iter4 = v4.begin(); iter4 != v4.end(); iter4 ++) {
				long mj = *iter4;
				if (mi == mj) {
					return mi;
				}
			}
		}
	}
	
	return NO_CONNECTING_NEIGHBOR;
}

long CartesianMesh::getNeighbor(int n,  long index, int neighbor)
{
	int k, new_neighbor;
	if(getDimension() == 2){k=2;}
	if(getDimension() == 3){k=4;}
	n = n - 1;
	if(n < 0){
		return index;
	}else{
		MembraneElement *pElement = getMembraneElements()+index;
		long neighborIndex = pElement->neighborMEIndex[neighbor];
		if(neighborIndex < 0){
			return -1;
		}else{
			new_neighbor = -1;
			MembraneElement *pNeighbor = getMembraneElements()+neighborIndex;
			for(int i=0; i<k; i++){
				if((pNeighbor->neighborMEIndex[i])==index){
					new_neighbor = (i+k/2)%k;
				}
			}
			if (new_neighbor < 0){
				return -1;
			}
		}   
		return getNeighbor(n, neighborIndex, new_neighbor);
	}
}

long* qvoronoi(int dim, int numpoints, double (*points)[2], int focus, vector<VoronoiRidge>& ridges, int artificialOffset);

bool addElementToVector(vector<long>& v, long a) {
	if (v.empty()) {
		v.push_back(a);
		return true;
	}
	for (vector<long>::iterator iter = v.begin(); iter != v.end(); iter ++) {
		if (a == (*iter))  {
			return false;
		}
	}
	v.push_back(a);
	return true;
}

void CartesianMesh::getNeighborCandidates (vector<long>& neighborCandidates, DoubleVector3 centralNormal, long index, int hierarchy) {
	hierarchy = hierarchy - 1;
	if (hierarchy < 0) {
		return;
	}
	long* neighbors = pMembraneElement[index].neighborMEIndex;	
	for (int i = 0; i < 4; i ++) {
		if (neighbors[i] != -1) {
			DoubleVector3 wc1 = getVolumeWorldCoord(pMembraneElement[neighbors[i]].outsideIndexNear);
			DoubleVector3 wc2 = getVolumeWorldCoord(pMembraneElement[neighbors[i]].insideIndexNear);
			DoubleVector3 v2 = wc1 - wc2;
			double d = centralNormal.dotProduct(v2);

			// if they are pointing the opposite direction, terminate this neighbor
			if (d >= 0) {
				addElementToVector(neighborCandidates, neighbors[i]);			
				getNeighborCandidates(neighborCandidates, centralNormal, neighbors[i], hierarchy);
			}		
		}		
	}	
}

bool project(WorldCoord pointWc, WorldCoord e1, WorldCoord e2, double* points) {
	double c1 = pointWc.dotProduct(e1); 
	double c2 = pointWc.dotProduct(e2);
	if ((c1*c1 + c2*c2) < 1e-30){
		return false;
	}else{
		points[0] = c1;
		points[1] = c2;
		return true;
	}
}

void CartesianMesh::computeMembraneCoupling(){	
	if (dimension == 1) {
		throw "No membrane diffusion allowed in 1D applications!";
	}

	cout << setprecision(10);	
	long N = getNumMembraneElements();

	cout << "Membrane Elements -> N=" << N << endl;

	switch (getDimension()) {
		case 2: {
			double totalLenghth = 0;
			long arr[4];			

			SparseMatrixPCG* smat = new SparseMatrixPCG(N, MAXNEIGHBOR_2D * N, MATRIX_GENERAL);	
			for (int i = 0; i < N; i ++) {				
				long currentIndex = pMembraneElement[i].index;				
				ASSERTION(currentIndex==i);
				bool currentOnBoundary = false;
				if (getMembraneNeighborMask(currentIndex) & NEIGHBOR_BOUNDARY_MASK) {
					currentOnBoundary = true;
				}

				//sort the neighbor index as needed by sparse matrix format
				memcpy(arr, pMembraneElement[currentIndex].neighborMEIndex, 4 * sizeof(long));
				for (int j = 0; j < 3; j ++) {
					for (int k = j+1; k < 4; k ++) {						
						if (arr[j] > arr[k]) {
							long a = arr[j];
							arr[j] = arr[k];
							arr[k] = a;
						}
					}
				}
				
				smat->addNewRow();
				double sum_arclength = 0;
				int numNeighbors = 0;
				for (int j = 0; j < 4; j ++) {
					long neighborIndex = arr[j];
					if (neighborIndex == -1) {
						continue;
					}

					numNeighbors ++;
					double arc_length = 0;
					if (getMembraneNeighborMask(neighborIndex) & NEIGHBOR_BOUNDARY_MASK) {
						arc_length += pMembraneElement[neighborIndex].area;
					} else {
						arc_length += pMembraneElement[neighborIndex].area/2;
					}
					if (currentOnBoundary) {
						arc_length += pMembraneElement[currentIndex].area;
					} else {
						arc_length += pMembraneElement[currentIndex].area/2;
					}

					double cross_sectional_area = 1;
					smat->setCol(neighborIndex, cross_sectional_area/arc_length); // off-diagnal element and its column
					sum_arclength += arc_length;
				}
				double volLength = 0;
				if (numNeighbors == 0) {
					volLength = pMembraneElement[currentIndex].area;
				} else {
					if (sum_arclength != 0) {
						volLength = sum_arclength/2;
					} else {
						stringstream ss;
						ss << "Unexpected zero area membrane element [" << currentIndex << + "]"; 
						throw ss.str();
					}
				}
				smat->setDiag(i, volLength); // set diagonal elements		
				totalLenghth += volLength;
			}
			smat->close();
			cout << "Total volume= " << totalLenghth << endl;
#ifdef SPECIAL_SHAPE
			double exactVol = getExactVolume();
			cout << " Exact Volume= " <<  exactVol << endl;
			cout << " Volume absolute error with Exact= " << (exactVol - totalLenghth) << endl;
#endif
			membraneElementCoupling = smat;
			break;
		}

		/*==========================================================
				3D case
			    1. find neighbor candidate
				2. projection
				3. vonoroi
				4. symmetrize (averaging)
				5. set value off diagonal si/di and diagonal vi
		==========================================================*/
		case 3: {	
			cout << "==============================================" << endl;

			long numNeighborsBeforeSymmetrize = 0;
			long numNeighborsAfterSymmetrize = 0;
			long numZeroNeighbor = 0;

			SparseMatrixPCG* smat = new SparseMatrixPCG(N, N * MAXNEIGHBOR_3D, MATRIX_GENERAL);	
			if (N == 0) {
				membraneElementCoupling = smat;
				return;
			}
			IncidenceMatrix<VoronoiRidge>* vrIM = new IncidenceMatrix<VoronoiRidge>(N, (MAXNEIGHBOR_3D + 1) * N, MATRIX_GENERAL, MAXNEIGHBOR_3D);

			vector<long> neighborCandidates;
			neighborCandidates.reserve(30);			

			vector<VoronoiRidge> vrRidges;
			vrRidges.reserve(MAXNEIGHBOR_3D);

			for (long index = 0; index < N; index ++) {	

				neighborCandidates.clear();
				vrRidges.clear();

				long currentIndex = pMembraneElement[index].index;				
				ASSERTION(currentIndex==index);

				WorldCoord currentWC = getMembraneWorldCoord(currentIndex);
				DoubleVector3 currentNormal = pMembraneElement[currentIndex].unitNormal;
				int currentMask = getMembraneNeighborMask(currentIndex);

				// get neigbor candidates
				neighborCandidates.push_back(currentIndex);  // including itself

				DoubleVector3 wc1 = getVolumeWorldCoord(pMembraneElement[index].outsideIndexNear);
				DoubleVector3 wc2 = getVolumeWorldCoord(pMembraneElement[index].insideIndexNear);
				DoubleVector3 centralNormal = wc1 - wc2;

				getNeighborCandidates(neighborCandidates, centralNormal, currentIndex, GOING_OUT_LAYERS);
				assert(neighborCandidates.size() > 1);							

				/*==========================================================
						start projection
				==========================================================*/

				WorldCoord e1, e2;

				//
				// create an ortho-normal basis (e1,e2) for tangent plane of "current" membrane element
				//
				while (true) {
					double r = rand();
					WorldCoord e(rand(), rand(), rand()); // should be independent of "normal"

					double nxDot = e.dotProduct(currentNormal); 

					// find portion of "e" that is perpendicular to "normal" (this direction will be first unit vector "e1")
					e1 = e - currentNormal.scale(nxDot);					

					// as long as e wasn't parallel to "normal" (i.e. "e1" wasn't a zero vector)
					// then compute e2 as unit vector which is perpendicular to both "normal" and "e1"
					if (e1.length() > 0) {
						e1.normalize();
						e2 = currentNormal.crossProduct(e1);
						break;
					}
				}

				double (*points)[2] = new double[neighborCandidates.size() + 8][2]; // room for up to 4 artificial points at boundary.

				// current point projection
				points[0][0] = 0.0;
				points[0][1] = 0.0;

				int numPoints = 1;				
				for (vector<long>::iterator iter = neighborCandidates.begin() + 1; iter != neighborCandidates.end(); ) {
					// point X
					int neighborIndex = *iter;
					assert (neighborIndex != currentIndex);
					WorldCoord candidateX_WC = getMembraneWorldCoord(neighborIndex);						
					// move X to new coordiate system where the current point is the origin
					WorldCoord candidateX_LC = candidateX_WC - currentWC;

					// project X to tangent plane
					// convert to 2D coordinates
					if (!project(candidateX_LC, e1, e2, points[numPoints])){
						iter = neighborCandidates.erase(iter);
					} else {
						iter ++;
						numPoints ++;
					}
				}	/*============= end of projection ========================*/				

				//
				// add artifical points if the "current" membrane element is on the boundary
				//								
				
				if (currentMask & NEIGHBOR_BOUNDARY_MASK) {
					for (int j = 0; j < 4; j ++) {
						int nindex = pMembraneElement[currentIndex].neighborMEIndex[j];
						if (nindex != -1) {
							//
							// for each neighbor that is also on the boundary:
							//    create two artificial points to force boundary to be within Voronoi diagram
							//
							//    1) add "artificial" point near "current point" - outside domain along "boundary normal" <<<NOT MEMBRANE NORMAL>>>
							//    2) project "artificial" point onto tagent plane (transformed into e1,e2 coordinates).
							//    (note: projection can skew orthogonal vectors...so have to re-orthogonalize)
							//    3) subtract portion of projected "artificial" point in the direction of transformed boundary.
							//       a) form unit vector in direction of transformed boundary boundary_new_unit = (neighbor-current)/|neighbor-current|
							//       artificial_new = artificial_old -  (boundary_new_unit .dot. artificial_old)*boundary_new_unit
							//    4) repeat for neighbor point ....
							//
							int mask = getMembraneNeighborMask(nindex);
							int commonMask = mask & currentMask;
							if ( commonMask & NEIGHBOR_BOUNDARY_MASK) {
								WorldCoord candidateX_WC = getMembraneWorldCoord(nindex);	
								WorldCoord lc_neighbor = candidateX_WC - currentWC;

								WorldCoord lc_artificial_offset(0,0,0);
								double artificial_scale = 10000;
								if (commonMask & NEIGHBOR_XM_BOUNDARY) {									
									lc_artificial_offset.x = -scaleX_um /artificial_scale;									
								} else if (commonMask & NEIGHBOR_XP_BOUNDARY) {
									lc_artificial_offset.x = scaleX_um /artificial_scale;		
								} else if (commonMask & NEIGHBOR_YM_BOUNDARY) {
									lc_artificial_offset.y = -scaleY_um /artificial_scale;		
								} else if (commonMask & NEIGHBOR_YP_BOUNDARY) {
									lc_artificial_offset.y = scaleY_um /artificial_scale;		
								} else if (commonMask & NEIGHBOR_ZM_BOUNDARY) {
									lc_artificial_offset.z = -scaleZ_um /artificial_scale;		
								} else if (commonMask & NEIGHBOR_ZP_BOUNDARY) {
									lc_artificial_offset.z = scaleZ_um /artificial_scale;		
								}
								double prj_neighbor[2];
								double prj_artificial_offset[2];
								// current is the origin 
								if (project(lc_artificial_offset, e1, e2, prj_artificial_offset) && project(lc_neighbor, e1, e2, prj_neighbor)) {
									WorldCoord boundary_new_unit(prj_neighbor[0], prj_neighbor[1], 0);
									boundary_new_unit.normalize(); // should we protect against zero vector?

									WorldCoord artificial_old(prj_artificial_offset[0], prj_artificial_offset[1], 0);
									WorldCoord artificial_new = artificial_old - boundary_new_unit.scale(boundary_new_unit.dotProduct(artificial_old));

									points[numPoints][0] = artificial_new.x; // + 0; (current.x)
									points[numPoints][1] = artificial_new.y; // + 0; (current.y)
									numPoints ++;

									points[numPoints][0] = artificial_new.x + prj_neighbor[0];
									points[numPoints][1] = artificial_new.y + prj_neighbor[1];
									numPoints ++;
								}								
							}
						}
					}
				}				

				long *vertices = qvoronoi(2, numPoints, points, 0, vrRidges, (int)neighborCandidates.size());
				int numRealNeighbors = (int)vrRidges.size();				
				
#ifdef WITH_PROJECTION_TO_WALL
				if (currentMask & NEIGHBOR_BOUNDARY_MASK) {
					for (int j = 0; j < numRealNeighbors; j ++) {
						int nindex = neighborCandidates[vertices[j]];
						int mask = getMembraneNeighborMask(nindex);
						int commonMask = mask & currentMask;	
						WorldCoord wall_normal(0,0,0);

						if (commonMask & NEIGHBOR_BOUNDARY_MASK) {
							if (commonMask & NEIGHBOR_XM_BOUNDARY || commonMask & NEIGHBOR_XP_BOUNDARY) {							
								wall_normal.x = 1;
							} else if (commonMask & NEIGHBOR_YM_BOUNDARY || commonMask & NEIGHBOR_YP_BOUNDARY) {
								wall_normal.y = 1;						
							} else if (commonMask & NEIGHBOR_ZM_BOUNDARY || commonMask & NEIGHBOR_ZP_BOUNDARY) {
								wall_normal.z = 1;						
							}							

							if (wall_normal.length() != 0) {
								WorldCoord cp = currentNormal.crossProduct(wall_normal);	

								double pr[2];
								project(cp, e1, e2, pr);
								cp.x = pr[0];
								cp.y = pr[1];
								cp.z = 0;																				
								WorldCoord neighbor(points[vertices[j]][0], points[vertices[j]][1], 0);							

								cp.normalize();							
								vrRidges[j].di = fabs(neighbor.dotProduct(cp));	
							}
						}
					}
				}								
#endif				
				assert(numRealNeighbors < MAXNEIGHBOR_3D);
				vrIM->addNewRow();
				
				if (numRealNeighbors == 0) {
					cout << currentIndex << " has 0 neighbors !" << endl;
					numZeroNeighbor ++;
				} else {
					//sort
					int* indexes = new int[numRealNeighbors];
					for (int j = 0; j < numRealNeighbors; j ++) {
						indexes[j] = j;
						vertices[j] = neighborCandidates[vertices[j]]; // change to membrane index
					}

					for (int j = 0; j < numRealNeighbors - 1; j ++) {
						for (int k = j + 1; k < numRealNeighbors; k ++) {
							if (vertices[indexes[j]] > vertices[indexes[k]]) {
								int a = indexes[j];
								indexes[j] = indexes[k];
								indexes[k] = a;
							}
						}
					}				

					numNeighborsBeforeSymmetrize += numRealNeighbors;	
					for (int j = 0; j < numRealNeighbors; j ++) {
						long neighborIndex = vertices[indexes[j]];
						assert(neighborIndex != currentIndex);
						VoronoiRidge& neighborVoronoi = vrRidges[indexes[j]];
						//
						// si might be very big when one of the voronoi points is close to infinity;
						// it usually happens at the corner. Then we set si to be zero, it is ok because when we 
						// symmtrize, it will get si back.
						//
						if (neighborVoronoi.si > 10 * max(scaleX_um, max(scaleY_um, scaleZ_um))) {
							neighborVoronoi.si = 0;
						}
						vrIM->setCol(neighborIndex, neighborVoronoi); // off-diagnal element and its column					
					}					
					delete[] indexes;
				}
				delete[] vertices;
				delete[] points;
			}
			vrIM->close();
			cout << "--------Num of points that have zero neighbors " << numZeroNeighbor << endl;		

			// symmetrize returns the pointer to the matrix that's passed in so symmIM and vrIM point to the same matrix.
			IncidenceMatrix<VoronoiRidge>* symmIM = symmetrize(vrIM, N);

			cout << setprecision(10);
			double volTotal = 0.0;
			double totalFluxArea = 0.0;
			double boundaryFluxArea[6];
			memset(boundaryFluxArea, 0, 6 * sizeof(double));
			for (int index = 0; index < N; index ++) {
				bool bFlux = false;
				double* fluxArea = new double[6];
				memset(fluxArea, 0, 6 * sizeof (double));				

				int currentmask = getMembraneNeighborMask(index);

				if ((currentmask & BOUNDARY_TYPE_MASK) == BOUNDARY_TYPE_NEUMANN){ // boundary and only neumann
					bFlux = true;		
				}

				smat->addNewRow();
				INT32* columns;
				VoronoiRidge* vrs;				
				int numColumns = symmIM->getColumns(index, columns, vrs);				
				assert(numColumns > 0);

				double vol = 0.0;									
				int numNeighbors = 0;
				for (int j = 0; j < numColumns; j ++) {							
					INT32 neighborIndex = columns[j];
					if (neighborIndex == -1) {
						break;
					}

					numNeighbors ++;
					assert(neighborIndex < N && neighborIndex != index);
					assert(vrs[j].bSymmetrized);

					numNeighborsAfterSymmetrize ++;
					smat->setCol(neighborIndex, vrs[j].si/vrs[j].di);
					vol += vrs[j].si * vrs[j].di/4;
					
					double boundary_arclength = vrs[j].di/2;
					int neighbormask = getMembraneNeighborMask(neighborIndex);					
					if (bFlux && neighbormask & NEIGHBOR_BOUNDARY_MASK){ 
						int commonmask = currentmask & neighbormask; 
						// boundary and neumann
						if (commonmask & NEIGHBOR_XM_BOUNDARY) {
							fluxArea[BL_Xm] += boundary_arclength;
							totalFluxArea += boundary_arclength;
							boundaryFluxArea[BL_Xm] += boundary_arclength;
						} else if (commonmask & NEIGHBOR_XP_BOUNDARY) {
							fluxArea[BL_Xp] += boundary_arclength;
							totalFluxArea += boundary_arclength;
							boundaryFluxArea[BL_Xp] += boundary_arclength;
						} else if (commonmask & NEIGHBOR_YM_BOUNDARY) {
							fluxArea[BL_Ym] += boundary_arclength;
							totalFluxArea += boundary_arclength;
							boundaryFluxArea[BL_Ym] += boundary_arclength;
						} else if (commonmask & NEIGHBOR_YP_BOUNDARY) {
							fluxArea[BL_Yp] += boundary_arclength;
							totalFluxArea += boundary_arclength;
							boundaryFluxArea[BL_Yp] += boundary_arclength;
						} else if (commonmask & NEIGHBOR_ZM_BOUNDARY) {
							fluxArea[BL_Zm] += boundary_arclength;
							totalFluxArea += boundary_arclength;
							boundaryFluxArea[BL_Zm] += boundary_arclength;
						} else if (commonmask & NEIGHBOR_ZP_BOUNDARY) {
							fluxArea[BL_Zp] += boundary_arclength;
							totalFluxArea += boundary_arclength;
							boundaryFluxArea[BL_Zp] += boundary_arclength;
						} 
					}
				}
				if (numNeighbors == 0) {				
					vol = pMembraneElement[index].area;
				} else {
					if (vol == 0) {
						stringstream ss;
						ss << "Unexpected zero area membrane element [" << index << + "]"; 
						throw ss.str();
					}
				}
				smat->setDiag(index, vol);
				if (bFlux) {
					membrane_boundary_flux.insert(pair<long, double*>(index, fluxArea));
				} 
				volTotal += vol;
			}
			smat->close();
			delete vrIM;

			cout << "--------Num Neighbors before symmetrize " << numNeighborsBeforeSymmetrize << endl;
			cout << "--------Num Neighbors after symmetrize " << numNeighborsAfterSymmetrize << endl;			
			cout << "Total volume=" << volTotal << endl;
			cout << "Total FluxArea =" << totalFluxArea << endl;
			cout << "Total FluxAreaXM =" << boundaryFluxArea[BL_Xm] << endl;
			cout << "Total FluxAreaXP =" << boundaryFluxArea[BL_Xp] << endl;
			cout << "Total FluxAreaYM =" << boundaryFluxArea[BL_Ym] << endl;
			cout << "Total FluxAreaYP =" << boundaryFluxArea[BL_Yp] << endl;
			cout << "Total FluxAreaZM =" << boundaryFluxArea[BL_Zm] << endl;
			cout << "Total FluxAreaZP =" << boundaryFluxArea[BL_Zp] << endl;
#ifdef SPECIAL_SHAPE
			double exactVol = getExactVolume();
			cout << " Exact Volume= " <<  exactVol << endl;
			cout << " Volume absolute error with Exact= " << (exactVol - volTotal) << endl;
#endif
			membraneElementCoupling = smat;
			break;
		} // end of 3D case
	} // end of switch (dimension)
	
	for (int index = 0; index < N; index ++) {				
		pMembraneElement[index].area = membraneElementCoupling->getValue(index, index); // diagonal element is the new area
	}	
}

bool is_next_point_on_curve(CurvePlane curvePlane, WorldCoord centerWc, WorldCoord nextWc) {
	if (curvePlane == CURVE_PLANE_XY && nextWc.z == centerWc.z 
			|| curvePlane == CURVE_PLANE_XZ && nextWc.y == centerWc.y 
			|| curvePlane == CURVE_PLANE_YZ && nextWc.x == centerWc.x) {
		return true;
	}
	return false;
}

void getXYCurve(CurvePlane curvePlane, const vector<WorldCoord> curve, vector<double>& curvex, vector<double>& curvey) {
	for (vector<WorldCoord>::const_iterator iter = curve.begin(); iter != curve.end(); iter ++) {
		switch (curvePlane) {
			case CURVE_PLANE_XY:
				curvex.push_back(iter->x);
				curvey.push_back(iter->y);
				break;

			case CURVE_PLANE_XZ:
				curvex.push_back(iter->x);
				curvey.push_back(iter->z);
				break;

			case CURVE_PLANE_YZ:
				curvex.push_back(iter->y);
				curvey.push_back(iter->z);
				break;
		}
	}
}

bool CartesianMesh::findCurve(int startingIndex, CurvePlane curvePlane, vector<double>& curvex, vector<double>& curvey, int& currentMeIndexInVector){	
	vector<long> indexCurve;
	vector<WorldCoord> curve;

	WorldCoord currentWc, neighborWc;
	currentWc = getMembraneWorldCoord(startingIndex);

	// push back the staring index;
	curve.push_back(currentWc);	
	indexCurve.push_back(startingIndex);
	currentMeIndexInVector = 0;

	bool bOpenCurve = false;
	long index = startingIndex;
	long neighborIndex = -1;

	while (true) {
		currentWc = getMembraneWorldCoord(index);
		bool bFound = false;
		for (int i = 0; i < 4; i ++) {
			neighborIndex = pMembraneElement[index].neighborMEIndex[i];
			if (neighborIndex == -1) {
				continue;
			}
			if (!bOpenCurve) {
				if (index != startingIndex && neighborIndex == *(indexCurve.end() - 2)) {// if closed, make sure the next point hasn't been added to curve already 
					continue;
				}
			} else {
				if (neighborIndex == indexCurve.at(1)) { // if open, make sure the next point hasn't been added to curve already
					continue;
				}
			}			
			neighborWc = getMembraneWorldCoord(neighborIndex);
			if (is_next_point_on_curve(curvePlane, currentWc, neighborWc)) {
				bFound = true;
				break;
			}
		}

		if (bFound) { // found the next one
			if (neighborIndex == startingIndex) {
				break;
			}  else {
				if (bOpenCurve) {
					curve.insert(curve.begin(), neighborWc); // push back this new point
					indexCurve.insert(indexCurve.begin(), neighborIndex); 
					currentMeIndexInVector ++;
				} else {
					curve.push_back(neighborWc); // push back this new pooint
					indexCurve.push_back(neighborIndex);
				}
				index = neighborIndex;
			}
		} else {
			if (bOpenCurve) {
				break;
			} else {
				bOpenCurve = true;
				if (indexCurve.size() == 1) {
					break;
				}
				index = startingIndex;
			}		
		}
	}	

	getXYCurve(curvePlane, curve, curvex, curvey);
	indexCurve.clear();
	curve.clear();

	return !bOpenCurve;
}

int CartesianMesh::computeN(int startingIndex, CurvePlane curvePlane, vector<double> curvex, vector<double> curvey, int currentMeIndexInVector, bool bClose) {
	assert(curvex.size() == curvey.size());
	int numPoints = (int)curvex.size();
	if (numPoints == 1) {
		return 0;
	}
	double maxX = 1E-10, maxY = 1E-10;
	double minX = 1E10, minY = 1E10;
	for (int i = 0; i < numPoints; i ++) {
		if (maxX < curvex[i]) {
			maxX = curvex[i];
		}
		if (minX > curvex[i]) {
			minX = curvex[i];
		}
		if (maxY < curvey[i]) {
			maxY = curvey[i];
		}
		if (minY > curvey[i]) {
			minY = curvey[i];
		}
	}
	double Nx = 0, Ny = 0, h = 0;
	switch (curvePlane) {
		case CURVE_PLANE_XY:
			Nx = (maxX - minX)/scaleX_um;
			Ny = (maxY - minY)/scaleY_um;
			h = (scaleX_um + scaleY_um) / 2;
			break;
		case CURVE_PLANE_XZ:
			Nx = (maxX - minX)/scaleX_um;
			Ny = (maxY - minY)/scaleZ_um;
			h = (scaleX_um + scaleZ_um) / 2;
			break;
		case CURVE_PLANE_YZ:
			Nx = (maxX - minX)/scaleY_um;
			Ny = (maxY - minY)/scaleZ_um;
			h = (scaleY_um + scaleZ_um) / 2;
			break;
	}
	int coeff = 2;
	int nwave = (int)min(floor(coeff * sqrt((Nx + Ny)/2)), floor(sqrt((double)numPoints)));
	//
	// find coordinates of nearby points (left,right that are nwave neighbors away) to estimate curvature 
	// from the secants in each direction and measuring the angles between the secants.  These secants are
	// sampled at an appropriate characteristic length to ensure convergence as deltaX --> 0
	//
	WorldCoord left(0,0,0), right(0,0,0);
	if (bClose) {
		if (numPoints == 4) {
			return 1;
		}
		left.x = curvex[(currentMeIndexInVector - nwave + numPoints) % numPoints] - curvex[currentMeIndexInVector];
		left.y = curvey[(currentMeIndexInVector - nwave + numPoints) % numPoints] - curvey[currentMeIndexInVector];
		right.x = curvex[(currentMeIndexInVector + nwave) % numPoints] - curvex[currentMeIndexInVector];
		right.y = curvey[(currentMeIndexInVector + nwave) % numPoints] - curvey[currentMeIndexInVector];
	} else {
		if (currentMeIndexInVector - nwave < 0 && currentMeIndexInVector + nwave >= numPoints) {
			throw "Mesh size is too coarse, please refine mesh!";
		} else if (currentMeIndexInVector - nwave < 0) { // left side doesn't have enough neighbors
			if (currentMeIndexInVector + nwave/2 > currentMeIndexInVector &&
				currentMeIndexInVector + nwave/2 < currentMeIndexInVector + nwave) {
				left.x = curvex[currentMeIndexInVector] - curvex[currentMeIndexInVector + nwave/2];
				left.y = curvey[currentMeIndexInVector] - curvey[currentMeIndexInVector + nwave/2] ;
				right.x = curvex[currentMeIndexInVector + nwave] - curvex[currentMeIndexInVector + nwave/2];
				right.y = curvey[currentMeIndexInVector + nwave] - curvey[currentMeIndexInVector + nwave/2];
			} else if (currentMeIndexInVector + 2 * nwave < numPoints) {				
				left.x = curvex[currentMeIndexInVector] - curvex[currentMeIndexInVector + nwave];
				left.y = curvey[currentMeIndexInVector] - curvey[currentMeIndexInVector + nwave];
				right.x = curvex[currentMeIndexInVector + 2 * nwave] - curvex[currentMeIndexInVector + nwave];
				right.y = curvey[currentMeIndexInVector + 2 * nwave] - curvey[currentMeIndexInVector + nwave];
			} else {
				return 1;
			}
		} else if (currentMeIndexInVector + nwave >= numPoints) { // right side doesn't have enough neighbors
			if (currentMeIndexInVector - nwave/2 < currentMeIndexInVector &&
				currentMeIndexInVector - nwave/2 > currentMeIndexInVector - nwave) {
				left.x = curvex[currentMeIndexInVector - nwave] - curvex[currentMeIndexInVector - nwave/2];
				left.y = curvey[currentMeIndexInVector - nwave ] - curvey[currentMeIndexInVector - nwave/2] ;
				right.x = curvex[currentMeIndexInVector] - curvex[currentMeIndexInVector - nwave/2];
				right.y = curvey[currentMeIndexInVector] - curvey[currentMeIndexInVector - nwave/2];
			} else if (currentMeIndexInVector - 2 * nwave >= 0) {				
				left.x = curvex[currentMeIndexInVector - 2 * nwave] - curvex[currentMeIndexInVector - nwave];
				left.y = curvey[currentMeIndexInVector - 2 * nwave] - curvey[currentMeIndexInVector - nwave];
				right.x = curvex[currentMeIndexInVector] - curvex[currentMeIndexInVector - nwave];
				right.y = curvey[currentMeIndexInVector] - curvey[currentMeIndexInVector - nwave];
			} else {
				return 1;
			}
		} else { // both sides have enough neighbors
			left.x = curvex[currentMeIndexInVector - nwave] - curvex[currentMeIndexInVector];
			left.y = curvey[currentMeIndexInVector - nwave] - curvey[currentMeIndexInVector];
			right.x = curvex[currentMeIndexInVector + nwave] - curvex[currentMeIndexInVector];
			right.y = curvey[currentMeIndexInVector + nwave] - curvey[currentMeIndexInVector];
		}
	}
	double r = (left - right).length()/2;
	left.normalize();
	right.normalize();
	double dotp = max(-1.0, min(1.0, left.dotProduct(right)));
	double phi = acos(dotp);
	if (phi < 0.01) {
		phi = 0.01;
	}
	if (phi > PI - 0.01) {
		phi = PI - 0.01;
	}
	double R = r / fabs(sin(phi));

	if (double_infinity == -R || double_infinity == R) {
		return nwave;
	}

	if (R != R) {
		throw "CartesianMesh::computeN(), Radius of curvature is NAN";
	}	

	double N = min(floor(coeff * sqrt(R/h)), floor(sqrt((double)numPoints)));
	if (N >= numPoints || N <= 0) {
		return nwave;
	}
	return (int)N;
}

//#define N_EQUALS_2
void CartesianMesh::getN(long index, int* N){		
	switch (dimension) {
		case 2: {
#ifdef N_EQUALS_2
			N[0] = 2;
			N[1] = N[0];			
#else
			vector<double> curvex;
			vector<double> curvey;
			memset(N, 0, 2 * sizeof(int));			

			int currentMeInVector = 0;
			bool bClose = findCurve(index, CURVE_PLANE_XY, curvex, curvey, currentMeInVector);
			N[0] = computeN(index, CURVE_PLANE_XY, curvex, curvey, currentMeInVector, bClose);
			N[1] = N[0];
			curvex.clear();
			curvey.clear();
#endif
			break;
		}
		case 3: {
#ifdef N_EQUALS_2
			for (int i = 0; i < 4; i ++) {
				N[i] = 2;				
			}
#else
			int resultN1 = 0, resultN2 = 0;
			vector<double> curve1x, curve1y;
			vector<double> curve2x, curve2y;
			memset(N, 0, 4 * sizeof(int));

			WorldCoord currentWc = getMembraneWorldCoord(index);

			int insideNear = pMembraneElement[index].insideIndexNear;
			int outsideNear = pMembraneElement[index].outsideIndexNear;	

			WorldCoord insideWC = getVolumeWorldCoord(insideNear);
			WorldCoord outsideWC = getVolumeWorldCoord(outsideNear);
			WorldCoord difference = insideWC - outsideWC;
			NormalDirection nd = NORMAL_DIRECTION_ERROR;
			if (difference.x != 0 && difference.y == 0 && difference.z == 0) {
				nd = NORMAL_DIRECTION_X;
			} else if (difference.x == 0 && difference.y != 0 && difference.z == 0) {
				nd = NORMAL_DIRECTION_Y;
			} else if (difference.x == 0 && difference.y == 0 && difference.z != 0) {
				nd = NORMAL_DIRECTION_Z;
			}
			assert(nd != NORMAL_DIRECTION_ERROR);	

			int currentMeInVector1 = 0, currentMeInVector2 = 0;
			bool bClose1 = true, bClose2 = true;

			if (nd == NORMAL_DIRECTION_X) {
				bClose1 = findCurve(index, CURVE_PLANE_XY, curve1x, curve1y, currentMeInVector1);
				resultN1 = computeN(index, CURVE_PLANE_XY, curve1x, curve1y, currentMeInVector1, bClose1);
				bClose2 = findCurve(index, CURVE_PLANE_XZ, curve2x, curve2y, currentMeInVector2);
				resultN2 = computeN(index, CURVE_PLANE_XZ, curve2x, curve2y, currentMeInVector2, bClose2);
				for (int i = 0; i < 4; i ++) {					
					long neighborIndex = pMembraneElement[index].neighborMEIndex[i];
					if (neighborIndex >= 0) {
						WorldCoord wc = getMembraneWorldCoord(neighborIndex);
						if (is_next_point_on_curve(CURVE_PLANE_XY, wc, currentWc)) {
							N[i] = resultN1;
						} else if (is_next_point_on_curve(CURVE_PLANE_XZ, wc, currentWc)) {
							N[i] = resultN2;
						}
					}
				}
			} else if (nd == NORMAL_DIRECTION_Y) {
				bClose1 = findCurve(index, CURVE_PLANE_XY, curve1x, curve1y, currentMeInVector1);
				resultN1 = computeN(index, CURVE_PLANE_XY, curve1x, curve1y, currentMeInVector1, bClose1);
				bClose2 = findCurve(index, CURVE_PLANE_YZ, curve2x, curve2y, currentMeInVector2);
				resultN2 = computeN(index, CURVE_PLANE_YZ, curve2x, curve2y, currentMeInVector2, bClose2);
				for (int i = 0; i < 4; i ++) {					
					long neighborIndex = pMembraneElement[index].neighborMEIndex[i];
					if (neighborIndex >= 0) {
						WorldCoord wc = getMembraneWorldCoord(neighborIndex);
						if (is_next_point_on_curve(CURVE_PLANE_XY, wc, currentWc)) {
							N[i] = resultN1;
						} else if (is_next_point_on_curve(CURVE_PLANE_YZ, wc, currentWc)) {
							N[i] = resultN2;
						}
					}
				}
			} else if (nd == NORMAL_DIRECTION_Z) {
				bClose1 = findCurve(index, CURVE_PLANE_XZ, curve1x, curve1y, currentMeInVector1);
				resultN1 = computeN(index, CURVE_PLANE_XZ, curve1x, curve1y, currentMeInVector1, bClose1);
				bClose2 = findCurve(index, CURVE_PLANE_YZ, curve2x, curve2y, currentMeInVector2);
				resultN2 = computeN(index, CURVE_PLANE_YZ, curve2x, curve2y, currentMeInVector2, bClose2);
				for (int i = 0; i < 4; i ++) {					
					long neighborIndex = pMembraneElement[index].neighborMEIndex[i];
					if (neighborIndex >= 0) {
						WorldCoord wc = getMembraneWorldCoord(neighborIndex);
						if (is_next_point_on_curve(CURVE_PLANE_XZ, wc, currentWc)) {
							N[i] = resultN1;
						} else if (is_next_point_on_curve(CURVE_PLANE_YZ, wc, currentWc)) {
							N[i] = resultN2;
						}
					}
				}
			}
			curve1x.clear();
			curve1y.clear();
			curve2x.clear();
			curve2y.clear();
#endif
			break;
		}
	}	
}

IncidenceMatrix<VoronoiRidge>* CartesianMesh::symmetrize(IncidenceMatrix<VoronoiRidge>* im, long N) {
	IncidenceMatrix<VoronoiRidge>* symmIM = im;
	for (int index = 0; index < N; index ++) {
		INT32* columnIndices = 0;
		VoronoiRidge* columnValues = 0;
		int numColumns = symmIM->getColumns(index, columnIndices, columnValues);

		for (int j = 0; j < numColumns; j ++) {	
			INT32 neighborIndex = columnIndices[j];
			if (neighborIndex == -1) {
				break;
			}

			VoronoiRidge& vrij = columnValues[j];	
			if (vrij.bSymmetrized) {
				continue;
			}
			assert(neighborIndex < N && neighborIndex != index);					
			
			bool bJIExists = false;
			if (index < neighborIndex) {
				VoronoiRidge* vrji = symmIM->getValue(neighborIndex, index);
				if (vrji != 0) {
					vrij = vrji->average(vrij);
					vrij.bSymmetrized = true;
					(*vrji) = vrij;
					bJIExists = true;					
				}
			}

			if (!bJIExists || index > neighborIndex) {				
				vrij.si = vrij.si/2;				
				
				// recover di of vrji whose dji = 0
				WorldCoord wc_center = getMembraneWorldCoord(neighborIndex);
				WorldCoord wc_neighbor = getMembraneWorldCoord(index);

				WorldCoord r = wc_neighbor - wc_center;
				WorldCoord unitNormal_center = pMembraneElement[neighborIndex].unitNormal;
				double dot_rn = r.dotProduct(unitNormal_center);
				double di = (r - unitNormal_center.scale(dot_rn)).length();

#ifdef WITH_PROJECTION_TO_WALL				
				// project the boundary to the wall
				int commonMask = getMembraneNeighborMask(neighborIndex) & getMembraneNeighborMask(index);
				if (commonMask & NEIGHBOR_BOUNDARY_MASK) {
					if (commonMask & NEIGHBOR_XM_BOUNDARY || commonMask & NEIGHBOR_XP_BOUNDARY) {							
						di = fabs(r.y * unitNormal_center.z - r.z*unitNormal_center.y)/sqrt(unitNormal_center.y * unitNormal_center.y + unitNormal_center.z * unitNormal_center.z);
					} else if (commonMask & NEIGHBOR_YM_BOUNDARY || commonMask & NEIGHBOR_YP_BOUNDARY) {						
						di = fabs(r.x * unitNormal_center.z - r.z*unitNormal_center.x)/sqrt(unitNormal_center.x * unitNormal_center.x + unitNormal_center.z * unitNormal_center.z);
					} else if (commonMask & NEIGHBOR_ZM_BOUNDARY || commonMask & NEIGHBOR_ZP_BOUNDARY) {
						di = fabs(r.y * unitNormal_center.x - r.x*unitNormal_center.y)/sqrt(unitNormal_center.x * unitNormal_center.x + unitNormal_center.y * unitNormal_center.y);
					}
				}
#endif							
				vrij.di = (di + vrij.di)/2;	
		
				vrij.bSymmetrized = true;
				symmIM->setValue(neighborIndex, index, vrij);
			}
		}
	}
	return symmIM;
}

double* CartesianMesh::getMembraneFluxArea(long index) {	
	map <long, double*> :: const_iterator iter;
	iter = membrane_boundary_flux.find(index);
	if (iter == membrane_boundary_flux.end()) {
		char errmsg[1000];
		sprintf(errmsg, "Membrane boundary flux information exception: no boundary condition found for membrane element %d", index);
		throw errmsg;
	}
	return iter->second;
}
