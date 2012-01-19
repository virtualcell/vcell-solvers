/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */

#include <VCELL/MembraneParticleVariable.h>
#include <VCELL/Membrane.h>

MembraneParticleVariable::MembraneParticleVariable(string& nameStr, Membrane* membrane, long size)
: Variable(nameStr, membrane, size, true)
{
}
