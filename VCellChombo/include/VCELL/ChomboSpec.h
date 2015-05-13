#ifndef CHOMBOSPEC_H_
#define CHOMBOSPEC_H_

#include <string>
#include <vector>
using std::string;
using std::vector;

#include <VCELL/ChomboGeometry.h>
#include <VCELL/ChomboRefinementRoi.h>

class ChomboSpec
{
public:
	ChomboSpec(ChomboGeometry* chomboGeometry);
	virtual ~ChomboSpec();

	static int defaultTagsGrow;
	
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

	vector<ChomboRefinementRoi*>& getMembraneRefinementRois()
	{
		return membraneRefinementRois;
	}

	vector<ChomboRefinementRoi*>& getVolumeRefinementRois()
	{
		return volumeRefinementRois;
	}

	void addMembraneRefinementRoi(ChomboRefinementRoi* roi)
	{
		membraneRefinementRois.push_back(roi);
	}

	void addVolumeRefinementRoi(ChomboRefinementRoi* roi)
	{
		volumeRefinementRois.push_back(roi);
	}
	
	int getViewLevel()
	{
		return viewLevel;
	}
	void setSaveVCellOutput(bool b)
	{
#ifdef CH_MPI
		bSaveVCellOutput = false;
#else
		bSaveVCellOutput = b;
#endif
	}
	void setSaveChomboOutput(bool b)
	{
#ifdef CH_MPI
		bSaveChomboOutput = true;
#else
		bSaveChomboOutput = b;
#endif
	}
	bool isSaveVCellOutput()
	{
		return bSaveVCellOutput;
	}
	bool isSaveChomboOutput()
	{
		return bSaveChomboOutput;
	}
	double getRelativeTolerance()
	{
		return relTol;
	}
	void setNumLevels(int nl)
	{
		numLevels = nl;
	}

	void setRefRatios(int* r)
	{
		refRatios = r;
	}

	void setMaxBoxSize(int bs)
	{
	  maxBoxSize = bs;
	}

	void setViewLevel(int vl)
	{
		viewLevel = vl;
	}

	void setRelTol(double rt)
	{
		relTol = rt;
	}

	void setFillRatio(double fr)
	{
		fillRatio = fr;
	}

	void printSummary();
private:
	ChomboGeometry* chomboGeometry;
	int numLevels;
	int* refRatios;
	double relTol;
	int maxBoxSize;
	double fillRatio;
	int viewLevel;
	vector<ChomboRefinementRoi*> membraneRefinementRois;
	vector<ChomboRefinementRoi*> volumeRefinementRois;
	bool bSaveVCellOutput;
	bool bSaveChomboOutput;
};

#endif /*CHOMBOSPEC_H_*/
