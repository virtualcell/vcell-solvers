/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/RegionSizeVariable.h>

RegionSizeVariable::RegionSizeVariable(string& nameStr, int size, bool arg_bVolume)
: Variable(size, nameStr)
{
	bVolume = arg_bVolume;
}
