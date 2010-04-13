/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef VOLUMEREGIONVARIABLE_H
#define VOLUMEREGIONVARIABLE_H

#include <VCELL/Variable.h>

class VolumeRegionVariable : public Variable
{
public:
	VolumeRegionVariable(int size, string& nameStr);

	VariableType	getVarType() {return VAR_VOLUME_REGION;}
};

#endif
