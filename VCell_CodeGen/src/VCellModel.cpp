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
}

VCellModel::~VCellModel()
{      
	for (int i = 0; i < (int)featureList.size(); i ++) {
		delete featureList[i];
	}
	featureList.clear();
 }
                         
void VCellModel::addFeature(Feature *feature)
{
	featureList.push_back(feature);
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

Feature *VCellModel::getFeatureFromIndex(int index)
{
	if (index < 0 || index >= (int)featureList.size()) {
		throw "VCellModel: getFeature(index) : index out of bounds";
	}
	return featureList.at(index);
}

Feature *VCellModel::getFeatureFromHandle(FeatureHandle handle)
{
	for (int i = 0; i < (int)featureList.size(); i ++) {
		Feature* feature = featureList[i];
		if (handle == feature->getHandle()) {
			return feature;
		}
	}
	return NULL;
}

Feature *VCellModel::getFeatureFromName(string& name)
{
	for (int i = 0; i < (int)featureList.size(); i ++) {
		Feature* feature = featureList[i];
		if (name == feature->getName()) {
			return feature;
		}
	}
	return NULL;
}

void VCellModel::resolveReferences()
{	
	Simulation* sim = SimTool::getInstance()->getSimulation();
	for (int i = 0; i < (int)featureList.size(); i ++) {
		Feature* feature = featureList[i];
		feature->resolveReferences(sim);		
	}
}
