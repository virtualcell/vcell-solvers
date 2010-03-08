/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/MembraneRegionVarContext.h>
#include <VCELL/Membrane.h>
#include <VCELL/MembraneRegionVariable.h>

MembraneRegionVarContext::MembraneRegionVarContext(Membrane *membrane, MembraneRegionVariable* var)
: VarContext(membrane, var)
{
}
