/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */

#include <string.h>
#include <algorithm>
#include <VCELL/Variable.h>
#include <VCELL/Structure.h>
#include <VCELL/VarContext.h>
#include <REAL.H>
#include <cmath>

Variable::Variable(string& nameStr, Structure* s, long Asize)
{
	name = nameStr;
	structure = s;
	size = Asize;
	curr = new double[size];
	bDiffusing = false;
	bElliptic = false;
	varContext = 0;
	exactErrorVar = 0;
	relativeErrorVar = 0;
	reset();
}

Variable::~Variable()
{
//	delete[] old;
	delete[] curr;
	delete varContext;
}

void Variable::reset()
{
	for (int i = 0; i < size; ++ i)
	{
		curr[i] = BASEFAB_REAL_SETVAL;
	}
	maxError = 0;
	l2Error = 0;
	l2Exact = 0;
	mean = 0;
//	sumVolFrac = 0;
	if (exactErrorVar != NULL)
	{
		memset(exactErrorVar->getCurr(), 0, exactErrorVar->getSize() * sizeof(double));
		memset(relativeErrorVar->getCurr(), 0, relativeErrorVar->getSize() * sizeof(double));
	}
}

string Variable::getQualifiedName(){
	if (structure != 0){
		return structure->getName() + "::" + name;
	}
	return name;
}

void Variable::addL2Error(double d)
{
	l2Error += d;
}
void Variable::addL2Exact(double d)
{
	l2Exact += d;
}
void Variable::addMean(double d)
{
	mean += d;
}

void Variable::updateMaxError(double d)
{
	maxError = std::max<double>(maxError, d);
}

void Variable::computeFinalL2Error()
{
	if (l2Exact != 0)
	{
		l2Error /= l2Exact;
	}
	l2Error = std::sqrt(l2Error);
}
void Variable::computeFinalMean()
{
	mean /= structure->getSizeFrac();
}