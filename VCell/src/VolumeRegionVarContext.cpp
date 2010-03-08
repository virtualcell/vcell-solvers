/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/Feature.h>
#include <VCELL/VolumeRegionVarContext.h>
#include <VCELL/VolumeRegionVariable.h>

VolumeRegionVarContext::VolumeRegionVarContext(Feature* Afeature, VolumeRegionVariable* var)
: VarContext(Afeature, var)
{
}
