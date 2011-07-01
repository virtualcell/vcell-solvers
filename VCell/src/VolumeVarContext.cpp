/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/VolumeVarContext.h>
#include <VCELL/VolumeVariable.h>
#include <VCELL/Feature.h>

VolumeVarContext::VolumeVarContext(Feature *feature, VolumeVariable* var)
: VarContext(feature, var)
{
	diffusionRate = NULL;
	//mobility = NULL;

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

//double VolumeVarContext::getMobilityConstant()
//{
//	if (mobility){
//		return *mobility;
//	}
//	throw "Application Error: neither mobility nor getMobilityConstant() specified for VolumeVarContext";
//}

double VolumeVarContext::getConvectionVelocity_X(long index) {
	return convectionVelocity.x;
}

double VolumeVarContext::getConvectionVelocity_Y(long index) {
	return convectionVelocity.y;
}

double VolumeVarContext::getConvectionVelocity_Z(long index) {
	return convectionVelocity.z;
}

bool VolumeVarContext::hasConstantCoefficients(int dimension) {
	if (((VolumeVariable*)species)->isAdvecting()) {
		return false;
	}
	return hasConstantDiffusion();
}

