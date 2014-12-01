/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */

#include <VCELL/Variable.h>
#include <parstream.H>
#include <string.h>
#include <algorithm>
#include <VCELL/Structure.h>
#include <VCELL/VarContext.h>
#include <REAL.H>
#include <cmath>
#include <iostream>

Variable::Variable(string& nameStr, Structure* s, long Asize)
{
	name = nameStr;
	structure = s;
	size = Asize;
	bDiffusing = false;
	bElliptic = false;
	varContext = 0;
	curr = NULL;
#ifndef CH_MPI
	exactErrorVar = 0;
	relativeErrorVar = 0;
	reset();
#endif
}

Variable::~Variable()
{
	if (curr != NULL)
	{
		delete[] curr;
	}
	delete varContext;
}

double* Variable::getCurr()
{
	if (curr == NULL)
	{
		curr = new double[size];
	}
	return curr;
}

string Variable::getQualifiedName(){
	if (structure != 0){
		return structure->getName() + "::" + name;
	}
	return name;
}

void Variable::reset()
{
	const char* methodName = "(Variable::reset)";
	pout() << "Entry " << methodName << ": " << getQualifiedName() << std::endl;

	for (int i = 0; i < size; ++ i)
	{
		getCurr()[i] = BASEFAB_REAL_SETVAL;
	}

#ifndef CH_MPI
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
#endif
	pout() << "Exit " << methodName << std::endl;
}

#ifndef CH_MPI
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
		total *= 602;
	}
	if (l2Exact != 0)
	{
		l2Error /= l2Exact;
	}
	l2Error = std::sqrt(l2Error);
}
#endif