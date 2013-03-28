/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/VolumeVariable.h>
#include <VCELL/VolumeRegionVariable.h>
#include <VCELL/Feature.h>
#include <VCELL/VolumeVarContextExpression.h>
#include <VCELL/VolumeRegionVarContextExpression.h>
#include <assert.h>
#include <sstream>
using std::stringstream;

Feature::Feature(string& name, unsigned char findex) : Structure(name)
{
	index = findex;
}

Feature::~Feature()
{
}

void Feature::resolveReferences(SimulationExpression *sim)
{
	Structure::resolveReferences(sim);
	std::map<Membrane*, BoundaryType>::iterator it = ebBcTypeMap.begin();
	if (it != ebBcTypeMap.end())
	{
		BoundaryType bt = it->second;
		for (++ it; it != ebBcTypeMap.end(); ++ it)
		{
			if (bt != it->second)
			{
				stringstream ss;
				ss << "Different EB boundary condition types are defined in feature " << getName() << ". Please make correction before proceeding.";
			}
		}
	}
}

BoundaryType Feature::getEbBcType(Membrane* mem)
{
	return getEbBcType();

	map<Membrane*, BoundaryType>::iterator iter = ebBcTypeMap.find(mem);
	assert(iter != ebBcTypeMap.end());
	return ebBcTypeMap[mem];
}

BoundaryType Feature::getEbBcType()
{
//	return name=="subVolume1" ? BOUNDARY_VALUE : BOUNDARY_FLUX;
	return BOUNDARY_VALUE;
}

