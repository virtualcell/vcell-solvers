#include <VCELL/ChomboSpec.h>

int ChomboSpec::defaultTagsGrow = 2;

ChomboSpec::ChomboSpec(ChomboGeometry* geometry) : chomboGeometry(geometry)
{
	numLevels = 1;
	refRatios = new int[1];
	refRatios[0] = 2;	
	maxBoxSize = 32;
	fillRatio = 0.9;
	relTol = 1e-9;
#ifdef CH_MPI
	bSaveVCellOutput = false;
	bSaveChomboOutput = true;
#else
	bSaveVCellOutput = true;
	bSaveChomboOutput = false;
#endif
}

ChomboSpec::~ChomboSpec()
{
	delete chomboGeometry;
	delete[] refRatios;
	for (vector<ChomboRefinementRoi*>::iterator it = membraneRefinementRois.begin(); it != membraneRefinementRois.end(); ++ it)
	{
		delete *it;
	}
	for (vector<ChomboRefinementRoi*>::iterator it = volumeRefinementRois.begin(); it != volumeRefinementRois.end(); ++ it)
	{
		delete *it;
	}
}

void ChomboSpec::printSummary()
{
	pout() << "numLevels=" << numLevels << endl;
	pout() << "refRatios";
	for (int i = 0; i < numLevels; ++ i)
	{
		pout() << " " << refRatios[i];
	}
	pout() << endl;
	pout() << "relTol=" << relTol << endl;
	pout() << "maxBoxSize=" << maxBoxSize << endl;
	pout() << "fillRatio=" << fillRatio << endl;
	pout() << "viewLevel=" << viewLevel << endl;
	pout() << "bSaveVCellOutput=" << bSaveVCellOutput << endl;
	pout() << "bSaveChomboOutput=" << bSaveChomboOutput << endl;
	pout() << "Membrane ROIs: " << membraneRefinementRois.size() << endl;
	for (vector<ChomboRefinementRoi*>::iterator it = membraneRefinementRois.begin(); it != membraneRefinementRois.end(); ++ it)
	{
		ChomboRefinementRoi* roi = *it;
		pout() << "\tlevel:" << roi->getLevel() << "; tagsGrow:" << roi->getTagsGrow() << "; expression:" << roi->getRoi()->infix() << endl;
	}
	pout() << "Volume ROIs: " << volumeRefinementRois.size() << endl;
	for (vector<ChomboRefinementRoi*>::iterator it = volumeRefinementRois.begin(); it != volumeRefinementRois.end(); ++ it)
	{
		ChomboRefinementRoi* roi = *it;
		pout() << "\tlevel:" << roi->getLevel() << "; tagsGrow:" << roi->getTagsGrow() << "; expression:" << roi->getRoi()->infix() << endl;
	}
}