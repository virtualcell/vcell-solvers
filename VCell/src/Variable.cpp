/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/SimTypes.h>
#include <VCELL/Variable.h>
#include <VCELL/FVUtils.h>
#include <fstream>
using namespace std;

Variable::Variable(long Asize, string& nameStr, string& Aunits)
{
	size = Asize;
	ASSERTION(size);
	old = new double[size];
	curr = new double[size];
	units = Aunits;
	name = nameStr;
	next = NULL;
	clear();
}

Variable::~Variable()
{
	if (old) delete[] old;
	if (curr) delete[] curr;
}

void Variable::clear()
{
	long i;
	double *ptrCurr = curr;
	double *ptrOld = old;
	for (i=0; i<size; i++){
		*ptrCurr++ = 0.0;
		*ptrOld++ = 0.0;
	}
}

void Variable::show(ofstream& fp)
{
	fp << name << endl;
	long k = 0;
	for (k=0;k<size;k++){
		fp << curr[k] << " ";
		if (k % 10 == 9) 
			fp << endl;
	}
	if (k%10 != 0) 
		fp << endl;
}

double Variable::getOld(long index)
{
	// 
	// if (doesn't exist, return 0.0)
	//
	if (index<0 || index>=size) return 0.0;
	   
	return *(old + index);
}

double Variable::getCurr(long index)
{
	// 
	// if (doesn't exist, return 0.0)
	//
	if (index<0 || index>=size) return 0.0;
	   
	return *(curr + index);
}

void Variable::setOld(long index, double value)
{
	ASSERTION(index>=0 && index<size);
	   
	*(old + index) = value;
}

void Variable::setCurr(long index, double value)
{
	ASSERTION(index>=0 && index<size);
	   
	*(curr + index) = value;
}

double Variable::getMaxDifference()
{
	double maxDiff=0;

	double *pCurr=curr;
	double *pOld=old;
	for (long i=0; i<size; i++){
		maxDiff = max(maxDiff, fabs(*pCurr - *pOld));
		pCurr++;
		pOld++;
	}

	return maxDiff;
}

void Variable::update()
{
	memcpy(old, curr, sizeof(double)*size);
}

void Variable::revert()
{
	memcpy(curr, old, sizeof(double)*size);
}
