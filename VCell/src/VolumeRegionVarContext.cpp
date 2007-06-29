/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/VolumeRegionVarContext.h>

VolumeRegionVarContext::VolumeRegionVarContext(Feature *Afeature, string& AspeciesName)
: VarContext(Afeature, AspeciesName)
{
	uniformRate = false;
	uniformFlux = false;
	zeroFluxBoundary = false;
}
