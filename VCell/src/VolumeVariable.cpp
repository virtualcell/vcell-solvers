/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/VolumeVariable.h>
#include <fstream>
using namespace std;

VolumeVariable::VolumeVariable(long numX, long numY, long numZ, string& nameStr, string& Aunits)
: Variable(numX*numY*numZ,nameStr,Aunits)
{
	sizeX = numX;
	sizeY = numY;
	sizeZ = numZ;
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

bool VolumeVariable::getLineX(int y, int z, double vect[], int len)
{
	// 
	// assume vect[] is of length 'len'
	//
	ASSERTION(y>=0 && y<sizeY);
	ASSERTION(z>=0 && z<sizeZ);
	ASSERTION(len==sizeX);
	double *ptr = curr + sizeX*(y + sizeY*z);
	memcpy(vect, ptr, sizeof(double)*len);
	   
	return true;
}

bool VolumeVariable::setLineX(int y, int z, double vect[], int len)
{
	// 
	// assume vect[] is of length 'len'
	//
	ASSERTION(y>=0 && y<sizeY);
	ASSERTION(z>=0 && z<sizeZ);
	ASSERTION(len==sizeX);
	double *ptr = curr + sizeX*(y + sizeY*z);
	memcpy(ptr, vect, sizeof(double)*len);
	   
	return true;
}

bool VolumeVariable::getLineY(int x, int z, double vect[], int len)
{
	printf("VolumeVariable::getLineY(x=%d,z=%d,vect,len=%d\n",x,z,len);
	// 
	// assume vect[] is of length 'len'
	//
	ASSERTION(x>=0 && x<sizeX);
	ASSERTION(z>=0 && z<sizeZ);
	ASSERTION(len==sizeY);
	double *ptr = curr + x + sizeX*sizeY*z;
	register int offset = sizeX;
	for (int y=0;y<len;y++){
		*vect++ = *ptr;
		ptr += offset;
	}
	   
	return true;
}

bool VolumeVariable::setLineY(int x, int z, double vect[], int len)
{
	// 
	// assume vect[] is of length 'len'
	//
	ASSERTION(x>=0 && x<sizeX);
	ASSERTION(z>=0 && z<sizeZ);
	ASSERTION(len==sizeY);
	double *ptr = curr + x + sizeX*sizeY*z;
	register int offset = sizeX;
	for (int y=0;y<len;y++){
		*ptr = *vect++;
		ptr += offset;
	}
	   
	return true;
}

bool VolumeVariable::getLineZ(int x, int y, double vect[], int len)
{
	// 
	// assume vect[] is of length 'len'
	//
	ASSERTION(x>=0 && x<sizeX);
	ASSERTION(y>=0 && y<sizeY);
	ASSERTION(len==sizeZ);
	double *ptr = curr + x + sizeX*y;
	register int offset = sizeX*sizeY;
	for (int z=0;z<len;z++){
		*vect++ = *ptr;
		ptr += offset;
	}
	   
	return true;
}

bool VolumeVariable::setLineZ(int x, int y, double vect[], int len)
{
	// 
	// assume vect[] is of length 'len'
	//
	ASSERTION(x>=0 && x<sizeX);
	ASSERTION(y>=0 && y<sizeY);
	ASSERTION(len==sizeZ);
	double *ptr = curr + x + sizeX*y;
	register int offset = sizeX*sizeY;
	for (int z=0;z<len;z++){
		*ptr = *vect++;
		ptr += offset;
	}
	   
	return true;
}
