/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */

#include <VCELL/Variable.h>
#include <string.h>
#include <algorithm>
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
	for (int i = 0; i < size; ++ i)
	{
		curr[i] = BASEFAB_REAL_SETVAL;
	}
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
	maxError = 0;
	l2Error = 0;
	l2Exact = 0;
	mean = 0;
	total = 0;

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
void Variable::addTotal(double d)
{
	total += d;
}
void Variable::updateMaxError(double d)
{
	maxError = std::max<double>(maxError, d);
}

void Variable::computeFinalStatistics()
{
	mean = total / structure->getSize();
	if (getVarType() == VAR_VOLUME || getVarType() == VAR_VOLUME_REGION)
	{
		total *= 620;
	}
	if (l2Exact != 0)
	{
		l2Error /= l2Exact;
	}
	l2Error = std::sqrt(l2Error);
}