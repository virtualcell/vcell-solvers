/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/VCellModel.h>
#include <VCELL/Feature.h>
#include <VCELL/Contour.h>
#include <VCELL/Element.h>
#include <VCELL/Simulation.h>
#include <VCELL/SimTool.h>

VCellModel::VCellModel()
{
	featureList = NULL;
	pContours.erase(pContours.begin(), pContours.end()); 
}

VCellModel::~VCellModel()
{      
	Feature *feature;
	while (feature = getNextFeature()){
		featureList = feature->next;
		delete feature;
	}
 }
                         
void VCellModel::addFeature(Feature *feature)
{
	feature->next = featureList;
	featureList = feature;
}
                         
void VCellModel::addContour(Contour *contour)
{
	pContours.push_back(contour);
}

Contour *VCellModel::getContour(int index)
{
	return pContours[index];
}

int VCellModel::getNumContours()
{
	return (int)pContours.size(); 
}

Feature *VCellModel::getNextFeature(Feature *ptr)
{
	if (ptr==NULL){
		return featureList;
	}else{
		return ptr->next;
	}
}

Feature *VCellModel::getFeature(FeatureHandle handle)
{
	Feature *ptr = featureList;
	while (ptr){
	if (handle==ptr->getHandle()) return ptr;
		ptr = ptr->next;
	}
	return NULL;
}

Feature *VCellModel::getFeature(string& name)
{
	Feature *ptr = featureList;
	while (ptr){
		if (name == ptr->getName()) 
			return ptr;
		ptr = ptr->next;
	}
	return NULL;
}

bool VCellModel::resolveReferences()
{	
	Feature *feature = NULL;
	Simulation* sim = SimTool::getInstance()->getSimulation();
	while (feature = getNextFeature(feature)){
		if (!feature->resolveReferences(sim)){
			printf("error resolving references\n");
			return false;
		}
	}
	return true;
}
