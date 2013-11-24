#ifndef CHOMBOSPEC_H_
#define CHOMBOSPEC_H_

#include <string>
using std::string;

#include <VCELL/ChomboGeometry.h>

class ChomboSpec
{
public:
	ChomboSpec();
	ChomboSpec(ChomboGeometry* cg, int numLevels, int boxsize, double fillRatio, int viewLevel, bool bSaveVCellOutput, bool bSaveChomboOutput, string* roi, int* ratios);
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
	void setSaveVCellOutput(bool b)
	{
		bSaveVCellOutput = b;
	}
	void setSaveChomboOutput(bool b)
	{
		bSaveChomboOutput = b;
	}
	bool isSaveVCellOutput()
	{
		return bSaveVCellOutput;
	}
	bool isSaveChomboOutput()
	{
		return bSaveChomboOutput;
	}
private:
	ChomboGeometry* chomboGeometry;
	int numLevels;
	int* refRatios;
	int maxBoxSize;
	double fillRatio;
	int viewLevel;
	string* refinementRois;
	bool bSaveVCellOutput;
	bool bSaveChomboOutput;
};

#endif /*CHOMBOSPEC_H_*/
