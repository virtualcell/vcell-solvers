/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/VolumeVariable.h>
#include <fstream>
using std::ofstream;
using std::endl;

VolumeVariable::VolumeVariable(long numX, long numY, long numZ, string& nameStr, bool pde, bool advect)
: Variable(numX*numY*numZ, nameStr, pde)
{
	sizeX = numX;
	sizeY = numY;
	sizeZ = numZ;
	bAdvecting = advect;
}

void VolumeVariable::show(ofstream& fp)
{
	double *pCurr=curr;
	fp << getName() << endl;
	int i, j, k;
	for (k=0;k<sizeZ;k++){
		if (sizeZ > 1) 
			fp << "z=" << k << ",";
		for (j = 0; j < sizeY; j ++){
			if (sizeY > 1) 
				fp << "y=" << j << ",";
			for (i = 0; i < sizeX; i ++){
				fp << *pCurr << " ";
				pCurr++;
				if (i % 10 == 9) 
					fp << endl;
			}
			if (i % 10 != 0) 
				fp << endl;
			fp << endl;		
		}
	}
}
