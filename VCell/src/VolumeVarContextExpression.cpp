/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/VolumeVarContextExpression.h>
#include <Expression.h>
#include <VCELL/Simulation.h>
#include <VCELL/SimulationExpression.h>
#include <VCELL/SimTool.h>
#include <VCELL/Solver.h>
#include <VCELL/Mesh.h>
#include <VCELL/Region.h>
#include <Exception.h>
using namespace VCell;

VolumeVarContextExpression::VolumeVarContextExpression(Feature *feature, string& varName)
: VolumeVarContext(feature, varName)
{
}

bool VolumeVarContextExpression::resolveReferences(Simulation* sim) {
	bool bResolved = VarContext::resolveReferences(sim);
	if (bResolved) {
		bindAll(((SimulationExpression*)sim)->getOldSymbolTable());
	}
	return bResolved;
}

double VolumeVarContextExpression::getInitialValue(long volIndex) {
	return getExpressionValue(volIndex, INITIAL_VALUE_EXP);
}

double VolumeVarContextExpression::getDiffusionRate(long volIndex)
{
	return getExpressionValue(volIndex, DIFF_RATE_EXP);
}

double VolumeVarContextExpression::getReactionRate(long volIndex)
{
	return getExpressionValue(volIndex, REACT_RATE_EXP);
}

double VolumeVarContextExpression::getXmBoundaryValue(long volIndex){
	return getExpressionValue(volIndex, BOUNDARY_XM_EXP);
}

double VolumeVarContextExpression::getXpBoundaryValue(long volIndex) {
	return getExpressionValue(volIndex, BOUNDARY_XP_EXP);
}

double VolumeVarContextExpression::getYmBoundaryValue(long volIndex) {
	return getExpressionValue(volIndex, BOUNDARY_YM_EXP);
}

double VolumeVarContextExpression::getYpBoundaryValue(long volIndex){
	return getExpressionValue(volIndex, BOUNDARY_YP_EXP);
}

double VolumeVarContextExpression::getZmBoundaryValue(long volIndex){
	return getExpressionValue(volIndex, BOUNDARY_ZM_EXP);	
}

double VolumeVarContextExpression::getZpBoundaryValue(long volIndex){
	return getExpressionValue(volIndex, BOUNDARY_ZP_EXP);
}

double VolumeVarContextExpression::getXmBoundaryFlux(long volIndex){
	return getExpressionValue(volIndex, BOUNDARY_XM_EXP);
}

double VolumeVarContextExpression::getXpBoundaryFlux(long volIndex){
	return getExpressionValue(volIndex, BOUNDARY_XP_EXP);
}

double VolumeVarContextExpression::getYmBoundaryFlux(long volIndex){
	return getExpressionValue(volIndex, BOUNDARY_YM_EXP);
}

double VolumeVarContextExpression::getYpBoundaryFlux(long volIndex){
	return getExpressionValue(volIndex, BOUNDARY_YP_EXP);
}

double VolumeVarContextExpression::getZmBoundaryFlux(long volIndex){
	return getExpressionValue(volIndex, BOUNDARY_ZM_EXP);	
}

double VolumeVarContextExpression::getZpBoundaryFlux(long volIndex){
	return getExpressionValue(volIndex, BOUNDARY_ZP_EXP);
}   
    
double VolumeVarContextExpression::getXBoundaryPeriodicConstant() {
	return getExpressionConstantValue(BOUNDARY_XM_EXP);
}

double VolumeVarContextExpression::getYBoundaryPeriodicConstant() {
	return getExpressionConstantValue(BOUNDARY_YM_EXP);
}

double VolumeVarContextExpression::getZBoundaryPeriodicConstant() {
	return getExpressionConstantValue(BOUNDARY_ZM_EXP);
}

double VolumeVarContextExpression::getConvectionVelocity_X(long volIndex){
	return getExpressionValue(volIndex, VELOCITY_X_EXP);
}

double VolumeVarContextExpression::getConvectionVelocity_Y(long volIndex){
	return getExpressionValue(volIndex, VELOCITY_Y_EXP);
}

double VolumeVarContextExpression::getConvectionVelocity_Z(long volIndex){
	return getExpressionValue(volIndex, VELOCITY_Z_EXP);
}

void VolumeVarContextExpression::getFlux(MembraneElement *element, double *inFlux, double *outFlux) {
	*inFlux = getExpressionValue(element, IN_FLUX_EXP);	
	*outFlux = getExpressionValue(element, OUT_FLUX_EXP);	
}

bool VolumeVarContextExpression::isNullExpressionOK(int expIndex) {
	if (expIndex == INITIAL_VALUE_EXP || expIndex == REACT_RATE_EXP) {
		return false;
	}

	Solver* solver = SimTool::getInstance()->getSimulation()->getSolverFromVariable(species);
	if (solver != null && solver->isPDESolver()) {
		if (expIndex == DIFF_RATE_EXP) {
			return false;
		}
		if ((mesh->getDimension() >= 1 && (expIndex == BOUNDARY_XM_EXP || expIndex == BOUNDARY_XP_EXP || expIndex == VELOCITY_X_EXP)) 
			|| (mesh->getDimension() >= 2 && (expIndex == BOUNDARY_YM_EXP || expIndex == BOUNDARY_YP_EXP || expIndex == VELOCITY_Y_EXP))
			|| (mesh->getDimension() >= 3 && (expIndex == BOUNDARY_ZM_EXP || expIndex == BOUNDARY_ZP_EXP || expIndex == VELOCITY_Z_EXP))) {
			return false;
		}
	}
	return true;
}
