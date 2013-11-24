#include <VCELL/ChomboSpec.h>

ChomboSpec::ChomboSpec() {
	chomboGeometry = 0;
	numLevels = 1;
	refRatios = new int[1];
	refRatios[0] = 2;	
	maxBoxSize = 32;
	fillRatio = 0.9;
}

ChomboSpec::ChomboSpec(ChomboGeometry* cg, int nl, int boxsize, double fr, int viewLvl, bool vcellOutput, bool chomboOutput, string* roi, int* ros)
	: chomboGeometry(cg),
		numLevels(nl),
		maxBoxSize(boxsize),
		fillRatio(fr),
		viewLevel(viewLvl),
		bSaveVCellOutput(vcellOutput),
    bSaveChomboOutput(chomboOutput),
		refinementRois(roi),
		refRatios(ros)
{
}

ChomboSpec::~ChomboSpec()
{
	delete chomboGeometry;
	delete[] refRatios;
	delete[] refinementRois;
}
