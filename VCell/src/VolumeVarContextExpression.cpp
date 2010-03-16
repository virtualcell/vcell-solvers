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
#include <SimpleSymbolTable.h>
#include <VCELL/VolumeVariable.h>

VolumeVarContextExpression::VolumeVarContextExpression(Feature *feature, VolumeVariable* var)
: VolumeVarContext(feature, var)
{
}

void VolumeVarContextExpression::resolveReferences(Simulation* sim) {
	VarContext::resolveReferences(sim);
	bindAll((SimulationExpression*)sim);
}

double VolumeVarContextExpression::getInitialValue(long volIndex) {
	return evaluateExpression(volIndex, INITIAL_VALUE_EXP);
}

double VolumeVarContextExpression::getDiffusionRate(long volIndex)
{
	return evaluateExpression(volIndex, DIFF_RATE_EXP);
}

double VolumeVarContextExpression::getReactionRate(long volIndex)
{
	return evaluateExpression(volIndex, REACT_RATE_EXP);
}

double VolumeVarContextExpression::getXmBoundaryValue(long volIndex){
	return evaluateExpression(volIndex, BOUNDARY_XM_EXP);
}

double VolumeVarContextExpression::getXpBoundaryValue(long volIndex) {
	return evaluateExpression(volIndex, BOUNDARY_XP_EXP);
}

double VolumeVarContextExpression::getYmBoundaryValue(long volIndex) {
	return evaluateExpression(volIndex, BOUNDARY_YM_EXP);
}

double VolumeVarContextExpression::getYpBoundaryValue(long volIndex){
	return evaluateExpression(volIndex, BOUNDARY_YP_EXP);
}

double VolumeVarContextExpression::getZmBoundaryValue(long volIndex){
	return evaluateExpression(volIndex, BOUNDARY_ZM_EXP);	
}

double VolumeVarContextExpression::getZpBoundaryValue(long volIndex){
	return evaluateExpression(volIndex, BOUNDARY_ZP_EXP);
}

double VolumeVarContextExpression::getXmBoundaryFlux(long volIndex){
	return evaluateExpression(volIndex, BOUNDARY_XM_EXP);
}

double VolumeVarContextExpression::getXpBoundaryFlux(long volIndex){
	return evaluateExpression(volIndex, BOUNDARY_XP_EXP);
}

double VolumeVarContextExpression::getYmBoundaryFlux(long volIndex){
	return evaluateExpression(volIndex, BOUNDARY_YM_EXP);
}

double VolumeVarContextExpression::getYpBoundaryFlux(long volIndex){
	return evaluateExpression(volIndex, BOUNDARY_YP_EXP);
}

double VolumeVarContextExpression::getZmBoundaryFlux(long volIndex){
	return evaluateExpression(volIndex, BOUNDARY_ZM_EXP);	
}

double VolumeVarContextExpression::getZpBoundaryFlux(long volIndex){
	return evaluateExpression(volIndex, BOUNDARY_ZP_EXP);
}   
    
double VolumeVarContextExpression::getXBoundaryPeriodicConstant() {
	return evaluateConstantExpression(BOUNDARY_XP_EXP);
}

double VolumeVarContextExpression::getYBoundaryPeriodicConstant() {
	return evaluateConstantExpression(BOUNDARY_YP_EXP);
}

double VolumeVarContextExpression::getZBoundaryPeriodicConstant() {
	return evaluateConstantExpression(BOUNDARY_ZP_EXP);
}

double VolumeVarContextExpression::getConvectionVelocity_X(long volIndex){
	return evaluateExpression(volIndex, VELOCITY_X_EXP);
}

double VolumeVarContextExpression::getConvectionVelocity_Y(long volIndex){
	return evaluateExpression(volIndex, VELOCITY_Y_EXP);
}

double VolumeVarContextExpression::getConvectionVelocity_Z(long volIndex){
	return evaluateExpression(volIndex, VELOCITY_Z_EXP);
}

double VolumeVarContextExpression::getFlux(MembraneElement *element) {
	//return evaluateExpression(element, FLUX_EXP);
	return evaluateJumpCondition(element);
}

bool VolumeVarContextExpression::isNullExpressionOK(int expIndex) {
	if (expIndex == INITIAL_VALUE_EXP || expIndex == REACT_RATE_EXP) {
		return false;
	}

	Solver* solver = sim->getSolverFromVariable(species);
	if (solver != null && solver->isPDESolver()) {
		if (expIndex == DIFF_RATE_EXP) {
			return false;
		}
		int geodim = sim->getMesh()->getDimension();
		if ((geodim >= 1 && (expIndex == BOUNDARY_XM_EXP || expIndex == BOUNDARY_XP_EXP || expIndex == VELOCITY_X_EXP)) 
			|| (geodim >= 2 && (expIndex == BOUNDARY_YM_EXP || expIndex == BOUNDARY_YP_EXP || expIndex == VELOCITY_Y_EXP))
			|| (geodim >= 3 && (expIndex == BOUNDARY_ZM_EXP || expIndex == BOUNDARY_ZP_EXP || expIndex == VELOCITY_Z_EXP))) {
			return false;
		}
	}
	return true;
}

bool VolumeVarContextExpression::hasConstantDiffusion() {
	if (!species->isPde()) {
		throw "hasConstantDiffusion() is only for PDE variables";
	}
	return isConstantExpression(DIFF_RATE_EXP);
}

bool VolumeVarContextExpression::hasConstantDiffusionAdvection(int dimension) {
	if (!hasConstantDiffusion()) {
		return false;
	}
	if (!((VolumeVariable*)species)->isAdvecting()) {
		return true;
	}
	return isConstantExpression(VELOCITY_X_EXP)
			&& (dimension < 2 || isConstantExpression(VELOCITY_Y_EXP)) 
			&& (dimension < 3 || isConstantExpression(VELOCITY_Z_EXP));
}
