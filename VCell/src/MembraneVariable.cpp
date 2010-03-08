/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */

#include <VCELL/MembraneVariable.h>
#include <VCELL/Membrane.h>

MembraneVariable::MembraneVariable(long size, string& nameStr, bool pde)
: Variable(size, nameStr, pde)
{
}
