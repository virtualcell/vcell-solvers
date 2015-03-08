#include <VCELL/ChomboSpec.h>

int ChomboSpec::defaultTagsGrow = 2;

ChomboSpec::ChomboSpec() {
	chomboGeometry = 0;
	numLevels = 1;
	refRatios = new int[1];
	refRatios[0] = 2;	
	maxBoxSize = 32;
	fillRatio = 0.9;
	tagsGrow = NULL;
}

ChomboSpec::ChomboSpec(ChomboGeometry* cg, int nl, double rel_tol, int boxsize, int* growSize, double fr, int viewLvl, bool vcellOutput, bool chomboOutput, string* roi, int* ros)
	: chomboGeometry(cg),
		numLevels(nl),
		relTol(rel_tol),
		maxBoxSize(boxsize),
		tagsGrow(growSize),
		fillRatio(fr),
		viewLevel(viewLvl),
		bSaveVCellOutput(vcellOutput),
    bSaveChomboOutput(chomboOutput),
		refinementRois(roi),
		refRatios(ros)
{
#ifdef CH_MPI
	bSaveVCellOutput = false;
	bSaveChomboOutput = true;
#endif
}

ChomboSpec::~ChomboSpec()
{
	delete chomboGeometry;
	delete[] refRatios;
	delete[] refinementRois;
	delete[] tagsGrow;
}
