/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/Region.h>
#include <VCELL/Feature.h>
#include <VCELL/CartesianMesh.h>

Region::Region(Mesh *Amesh)
{
	index.erase(index.begin(), index.end()); 
	id = 0;
	wasChanged = false;
	mesh = Amesh;
}

Region::~Region()
{      
}

void Region::setName(string& Aname)
{
	name = Aname; 
}

void Region::setId(int AnId)
{
	id = AnId; 
}

void Region::addIndex(long i)
{
	index.push_back(i);
	wasChanged = true;
}
