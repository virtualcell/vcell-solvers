#ifndef CHOMBOSPEC_H_
#define CHOMBOSPEC_H_

#include <string>
using std::string;

#include <VCELL/ChomboGeometry.h>

class ChomboSpec
{
public:
	ChomboSpec();
	ChomboSpec(ChomboGeometry* cg, int numLevels, int boxsize, double fillRatio, int* ratios);
	virtual ~ChomboSpec();

	int getNumLevels() {
		return numLevels;
	}
	
	int* getRefRatios() {
		return refRatios;
	}
	
	int getMaxBoxSize()
	{
		return maxBoxSize;
	}
	
	double getFillRatio()
	{
		return fillRatio;
	}
	ChomboGeometry* getChomboGeometry() {
		return chomboGeometry;
	}
	
private:
	ChomboGeometry* chomboGeometry;
	int numLevels;
	int* refRatios;
	int maxBoxSize;
	double fillRatio;
};

#endif /*CHOMBOSPEC_H_*/
