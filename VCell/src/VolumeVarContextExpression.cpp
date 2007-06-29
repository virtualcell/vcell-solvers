/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/VolumeVarContextExpression.h>
#include <Expression.h>
#include <VCELL/Simulation.h>
#include <VCELL/Mesh.h>
#include <VCELL/Region.h>
#include <Exception.h>
using namespace VCell;

VolumeVarContextExpression::VolumeVarContextExpression(Feature *feature, string& varName)
: VolumeVarContext(feature, varName)
{
}

VolumeVarContextExpression::~VolumeVarContextExpression()
{
}

double VolumeVarContextExpression::getInitialValue(long volIndex) {
	return getValue(volIndex, INITIAL_VALUE_EXP);
}

double VolumeVarContextExpression::getDiffusionRate(long volIndex)
{
	return getValue(volIndex, DIFF_RATE_EXP);
}

double VolumeVarContextExpression::getReactionRate(long volIndex)
{
	return getValue(volIndex, REACT_RATE_EXP);
}

double VolumeVarContextExpression::getXmBoundaryValue(long volIndex){
	return getValue(volIndex, BOUNDARY_XM_EXP);
}

double VolumeVarContextExpression::getXpBoundaryValue(long volIndex) {
	return getValue(volIndex, BOUNDARY_XP_EXP);
}

double VolumeVarContextExpression::getYmBoundaryValue(long volIndex) {
	return getValue(volIndex, BOUNDARY_YM_EXP);
}

double VolumeVarContextExpression::getYpBoundaryValue(long volIndex){
	return getValue(volIndex, BOUNDARY_YP_EXP);
}

double VolumeVarContextExpression::getZmBoundaryValue(long volIndex){
	return getValue(volIndex, BOUNDARY_ZM_EXP);	
}

double VolumeVarContextExpression::getZpBoundaryValue(long volIndex){
	return getValue(volIndex, BOUNDARY_ZP_EXP);
}

double VolumeVarContextExpression::getXmBoundaryFlux(long volIndex){
	return getValue(volIndex, BOUNDARY_XM_EXP);
}

double VolumeVarContextExpression::getXpBoundaryFlux(long volIndex){
	return getValue(volIndex, BOUNDARY_XP_EXP);
}

double VolumeVarContextExpression::getYmBoundaryFlux(long volIndex){
	return getValue(volIndex, BOUNDARY_YM_EXP);
}

double VolumeVarContextExpression::getYpBoundaryFlux(long volIndex){
	return getValue(volIndex, BOUNDARY_YP_EXP);
}

double VolumeVarContextExpression::getZmBoundaryFlux(long volIndex){
	return getValue(volIndex, BOUNDARY_ZM_EXP);	
}

double VolumeVarContextExpression::getZpBoundaryFlux(long volIndex){
	return getValue(volIndex, BOUNDARY_ZP_EXP);
}   
    
double VolumeVarContextExpression::getXBoundaryPeriodicConstant() {
	return getConstantValue(BOUNDARY_XM_EXP);
}

double VolumeVarContextExpression::getYBoundaryPeriodicConstant() {
	return getConstantValue(BOUNDARY_YM_EXP);
}

double VolumeVarContextExpression::getZBoundaryPeriodicConstant() {
	return getConstantValue(BOUNDARY_ZM_EXP);
}

double VolumeVarContextExpression::getConvectionVelocity_X(long volIndex){
	return getValue(volIndex, VELOCITY_X_EXP);
}

double VolumeVarContextExpression::getConvectionVelocity_Y(long volIndex){
	return getValue(volIndex, VELOCITY_Y_EXP);
}

double VolumeVarContextExpression::getConvectionVelocity_Z(long volIndex){
	return getValue(volIndex, VELOCITY_Z_EXP);
}

void VolumeVarContextExpression::getFlux(MembraneElement *element, double *inFlux, double *outFlux) {
	*inFlux = getValue(element, IN_FLUX_EXP);	
	*outFlux = getValue(element, OUT_FLUX_EXP);	
}
