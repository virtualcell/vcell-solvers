
#ifndef CHOMBOGEOMETRY_H
#define CHOMBOGEOMETRY_H

#include "BaseIF.H"
#include <assert.h>

#include <string>
#include <vector>
using std::string;
using std::vector;

#include <VCELL/ChomboIF.h>

namespace VCell {
class Expression;
}
class SymbolTable;
class Feature;

class ChomboGeometry
{
public: 
  	ChomboGeometry();
  	ChomboGeometry(const ChomboGeometry*);
  	virtual ~ChomboGeometry();
	
	void addSubdomain(Feature* feature, int pi, VCell::Expression* exp);
	void addSubdomain(Feature* feature, int phaseIndex, string& distanceMapFile);
	Feature* getFeature(const RealVect& a_point) const;

  	void setDomainOrigin(RealVect& rv) {
  		domainOrigin = rv;
  	}
  	void setDomainSize(RealVect& rv) {
  		domainSize = rv;
  	}
  	void setGridSize(IntVect& iv) {
  		gridSize = iv;
  	}
  	void setDimension(int dim) {
  		assert(dim == SpaceDim);
  		geoDim = dim;
  	}
  	int getDimension() {
  		return geoDim;
  	}
  	const RealVect& getDomainOrigin() {
  		return domainOrigin;
  	}
		const RealVect& getDomainSize() {
  		return domainSize;
  	}
  	const IntVect& getGridSize() {
  		return gridSize;
  	}
		const int getNumX()
		{
			return gridSize[0];
		}
		const int getNumY()
		{
			return gridSize[1];
		}
		const int getNumZ()
		{
			return geoDim == 2 ? 1 : gridSize[2];
		}

  	const int getNumSubdomains() {
  		return chomboIFList.size();
  	}

  	ChomboIF* getChomboIF(int index) {
  		return chomboIFList[index];
  	}

  	Feature* getFeature(int index) {
  		return chomboIFList[index]->getFeature();
  	}

  	static SymbolTable* getGeometrySymbolTable();

private:
	vector<ChomboIF*> chomboIFList;
	static SymbolTable* geometrySymbolTable;
	
	RealVect domainOrigin;
	RealVect domainSize;
	IntVect gridSize;
	int geoDim;
};

#endif
