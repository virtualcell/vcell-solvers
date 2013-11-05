#ifndef CHOMBOSPEC_H_
#define CHOMBOSPEC_H_

#include <string>
using std::string;

#include <VCELL/ChomboGeometry.h>

class ChomboSpec
{
public:
	ChomboSpec();
	ChomboSpec(ChomboGeometry* cg, int numLevels, int boxsize, double fillRatio, int viewLevel, string* roi, int* ratios);
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

	const string& getRefinementRoi(int ilev)
	{
		return refinementRois[ilev];
	}

	int getViewLevel()
	{
		return viewLevel;
	}
private:
	ChomboGeometry* chomboGeometry;
	int numLevels;
	int* refRatios;
	int maxBoxSize;
	double fillRatio;
	int viewLevel;
	string* refinementRois;
};

#endif /*CHOMBOSPEC_H_*/
