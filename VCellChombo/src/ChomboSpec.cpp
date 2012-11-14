#include <VCELL/ChomboSpec.h>

ChomboSpec::ChomboSpec() {
	chomboGeometry = 0;
	numLevels = 1;
	refRatios = new int[1];
	refRatios[0] = 2;	
	maxBoxSize = 32;
	fillRatio = 0.9;
}

ChomboSpec::ChomboSpec(ChomboGeometry* cg, int nl, int boxsize, double fr, int* ros) {
	chomboGeometry = cg;
	numLevels = nl;
	refRatios = ros;	
	maxBoxSize = boxsize;
	fillRatio = fr;
}

ChomboSpec::~ChomboSpec()
{
	delete chomboGeometry;
	delete[] refRatios;
}
