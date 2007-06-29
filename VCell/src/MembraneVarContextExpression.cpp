/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/MembraneVarContextExpression.h>
#include <VCELL/Simulation.h>
#include <VCELL/Feature.h>
#include <VCELL/Element.h>
#include <VCELL/MembraneRegion.h>
#include <Expression.h>

MembraneVarContextExpression::MembraneVarContextExpression(Feature *feature, string& varName)
: MembraneVarContext(feature, varName)
{
}

MembraneVarContextExpression::~MembraneVarContextExpression()
{

	for (int i = 0; i < TOTAL_NUM_EXPRESSIONS; i ++) {
		delete expressions[i];		
	}	
	delete[] expressions;
}

double MembraneVarContextExpression::getInitialValue(MembraneElement *element){
	return getValue(element, INITIAL_VALUE_EXP);
}

double MembraneVarContextExpression::getMembraneReactionRate(MembraneElement *element){
	return getValue(element, REACT_RATE_EXP);
}
double MembraneVarContextExpression::getMembraneDiffusionRate(MembraneElement *element){
	return getValue(element, DIFF_RATE_EXP);
}

double MembraneVarContextExpression::getXmBoundaryValue(MembraneElement *element){
	return getValue(element, BOUNDARY_XM_EXP);
}
double MembraneVarContextExpression::getXpBoundaryValue(MembraneElement *element){
	return getValue(element, BOUNDARY_XP_EXP);
}
double MembraneVarContextExpression::getYmBoundaryValue(MembraneElement *element){
	return getValue(element, BOUNDARY_YM_EXP);
}
double MembraneVarContextExpression::getYpBoundaryValue(MembraneElement *element){
	return getValue(element, BOUNDARY_YP_EXP);
}
double MembraneVarContextExpression::getZmBoundaryValue(MembraneElement *element){
	return getValue(element, BOUNDARY_ZM_EXP);
}
double MembraneVarContextExpression::getZpBoundaryValue(MembraneElement *element){
	return getValue(element, BOUNDARY_ZP_EXP);
}

double MembraneVarContextExpression::getXmBoundaryFlux(MembraneElement *element){
	return getValue(element, BOUNDARY_XM_EXP);
}
double MembraneVarContextExpression::getXpBoundaryFlux(MembraneElement *element){
	return getValue(element, BOUNDARY_XP_EXP);
}
double MembraneVarContextExpression::getYmBoundaryFlux(MembraneElement *element){
	return getValue(element, BOUNDARY_YM_EXP);
}
double MembraneVarContextExpression::getYpBoundaryFlux(MembraneElement *element){
	return getValue(element, BOUNDARY_YP_EXP);
}
double MembraneVarContextExpression::getZmBoundaryFlux(MembraneElement *element){
	return getValue(element, BOUNDARY_ZM_EXP);
}
double MembraneVarContextExpression::getZpBoundaryFlux(MembraneElement *element){
	return getValue(element, BOUNDARY_ZP_EXP);
}
