/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/MembraneVarContextExpression.h>
#include <VCELL/SimulationExpression.h>
#include <VCELL/Feature.h>
#include <VCELL/Element.h>
#include <VCELL/MembraneRegion.h>
#include <Expression.h>
#include <VCELL/SimTool.h>
#include <VCELL/Solver.h>

MembraneVarContextExpression::MembraneVarContextExpression(Feature *feature, string& varName)
: MembraneVarContext(feature, varName)
{
}

bool MembraneVarContextExpression::resolveReferences(Simulation* sim) {
	bool bResolved = VarContext::resolveReferences(sim);
	if (bResolved) {
		bindAll(((SimulationExpression*)sim)->getOldSymbolTable());
	}
	return bResolved;
}

double MembraneVarContextExpression::getInitialValue(MembraneElement *element){
	return getExpressionValue(element, INITIAL_VALUE_EXP);
}

double MembraneVarContextExpression::getMembraneReactionRate(MembraneElement *element){
	return getExpressionValue(element, REACT_RATE_EXP);
}
double MembraneVarContextExpression::getMembraneDiffusionRate(MembraneElement *element){
	return getExpressionValue(element, DIFF_RATE_EXP);
}

double MembraneVarContextExpression::getXmBoundaryValue(MembraneElement *element){
	return getExpressionValue(element, BOUNDARY_XM_EXP);
}
double MembraneVarContextExpression::getXpBoundaryValue(MembraneElement *element){
	return getExpressionValue(element, BOUNDARY_XP_EXP);
}
double MembraneVarContextExpression::getYmBoundaryValue(MembraneElement *element){
	return getExpressionValue(element, BOUNDARY_YM_EXP);
}
double MembraneVarContextExpression::getYpBoundaryValue(MembraneElement *element){
	return getExpressionValue(element, BOUNDARY_YP_EXP);
}
double MembraneVarContextExpression::getZmBoundaryValue(MembraneElement *element){
	return getExpressionValue(element, BOUNDARY_ZM_EXP);
}
double MembraneVarContextExpression::getZpBoundaryValue(MembraneElement *element){
	return getExpressionValue(element, BOUNDARY_ZP_EXP);
}

double MembraneVarContextExpression::getXmBoundaryFlux(MembraneElement *element){
	return getExpressionValue(element, BOUNDARY_XM_EXP);
}
double MembraneVarContextExpression::getXpBoundaryFlux(MembraneElement *element){
	return getExpressionValue(element, BOUNDARY_XP_EXP);
}
double MembraneVarContextExpression::getYmBoundaryFlux(MembraneElement *element){
	return getExpressionValue(element, BOUNDARY_YM_EXP);
}
double MembraneVarContextExpression::getYpBoundaryFlux(MembraneElement *element){
	return getExpressionValue(element, BOUNDARY_YP_EXP);
}
double MembraneVarContextExpression::getZmBoundaryFlux(MembraneElement *element){
	return getExpressionValue(element, BOUNDARY_ZM_EXP);
}
double MembraneVarContextExpression::getZpBoundaryFlux(MembraneElement *element){
	return getExpressionValue(element, BOUNDARY_ZP_EXP);
}

bool MembraneVarContextExpression::isNullExpressionOK(int expIndex) {
	if (expIndex == INITIAL_VALUE_EXP || expIndex == REACT_RATE_EXP) {
		return false;
	}
	if (SimTool::getInstance()->getSimulation()->getSolverFromVariable(species)->isPDESolver()) {
		if (expIndex == DIFF_RATE_EXP) {
			return false;
		}
		if ((mesh->getDimension() >= 1 && (expIndex == BOUNDARY_XM_EXP || expIndex == BOUNDARY_XP_EXP)) 
			|| (mesh->getDimension() >= 2 && (expIndex == BOUNDARY_YM_EXP || expIndex == BOUNDARY_YP_EXP))
			|| (mesh->getDimension() >= 3 && (expIndex == BOUNDARY_ZM_EXP || expIndex == BOUNDARY_ZP_EXP))) {
			return false;
		}
	}
	return true;
}
