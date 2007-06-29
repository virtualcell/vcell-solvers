/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/VolumeVarContext.h>

VolumeVarContext::VolumeVarContext(Feature *Afeature, string& AspeciesName)
: VarContext(Afeature, AspeciesName)
{
	diffusionRate = NULL;
	mobility = NULL;

	convectionVelocity.x =0.0;
	convectionVelocity.y =0.0;
	convectionVelocity.z =0.0;
}


double VolumeVarContext::getDiffusionRate(long index)
{
	if (diffusionRate){
		return *diffusionRate;
	}
	throw "Application Error: neither diffusionRate nor getDiffusionRate() specified for VolumeVarContext";
}

double VolumeVarContext::getMobilityConstant()
{
	if (mobility){
		return *mobility;
	}
	throw "Application Error: neither mobility nor getMobilityConstant() specified for VolumeVarContext";
}
