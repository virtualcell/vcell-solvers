/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */

#include <VCELL/MembraneVariable.h>

MembraneVariable::MembraneVariable(long size, string& nameStr, string& Aunits, bool pde)
: Variable(size,nameStr,Aunits, pde)
{
}
