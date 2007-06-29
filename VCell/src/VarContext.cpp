/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <VCELL/VarContext.h>
#include <VCELL/SimTypes.h>
#include <VCELL/Element.h>
#include <VCELL/Solver.h>
#include <VCELL/Variable.h>
#include <VCELL/Feature.h>
#include <VCELL/EqnBuilder.h>
#include <VCELL/Simulation.h>
#include <VCELL/Region.h>
#include <VCELL/VolumeRegion.h>
#include <VCELL/MembraneRegion.h>
#include <VCELL/CartesianMesh.h>

#ifdef FINITEVOLUME_STANDALONE
#include <Expression.h>
#endif

VarContext::VarContext(Feature *Afeature, string& AspeciesName)
{
	feature = Afeature;
	ASSERTION(feature);

	bInitialized = false;
	next = NULL;

	speciesName = AspeciesName;
	species = NULL;

	sim = NULL;
	mesh = NULL;
	eqnBuilder = NULL;
	    
	initialValue = NULL;

#ifdef FINITEVOLUME_STANDALONE
	expressions = new Expression*[TOTAL_NUM_EXPRESSIONS];
	constantValues = new double*[TOTAL_NUM_EXPRESSIONS];
	needsXYZ = new bool[TOTAL_NUM_EXPRESSIONS];
	memset(expressions, 0, TOTAL_NUM_EXPRESSIONS * sizeof(Expression*));
	memset(constantValues, 0, TOTAL_NUM_EXPRESSIONS * sizeof(double*));
	memset(needsXYZ, 0, TOTAL_NUM_EXPRESSIONS * sizeof(bool));
#endif
}

bool VarContext::resolveReferences(Simulation *Asim)
{
	sim = Asim;
	species = sim->getVariableFromName(speciesName);
	eqnBuilder = sim->getSolverFromVariable(species)->getEqnBuilder();
	mesh = sim->getMesh();

	if (sim && mesh && eqnBuilder){
		return true;
	}else{
		return false;
	}
#ifdef FINITEVOLUME_STANDALONE
	bindAll(sim->getOldSymbolTable());
#endif
}

double VarContext::getInitialValue(long)
{
	if (initialValue){
		return *initialValue;
	}
	throw "Application Error: neither initialValue nor getInitialValue() specified for VarContext";
}

VarContext::~VarContext()
{
#ifdef FINITEVOLUME_STANDALONE
	for (int i = 0; i < TOTAL_NUM_EXPRESSIONS; i ++) {
		delete expressions[i];		
		delete constantValues[i];
	}	
	delete[] expressions;
	delete[] constantValues;
	delete[] needsXYZ;
#endif
}

#ifdef FINITEVOLUME_STANDALONE
void VarContext::setExpression(Expression* newexp, int expIndex) {
	expressions[expIndex] = newexp;
}

void VarContext::bindAll(SimpleSymbolTable* symbolTable) {	
	for (int i = 0; i < TOTAL_NUM_EXPRESSIONS; i ++) {		
		if (expressions[i] == 0) {
			throw "VarContext::bindAll(), expression not defined";
		}
		try {
			//cout << expressions[i]->infix() << endl;
			double d = expressions[i]->evaluateConstant();
			constantValues[i] = new double[1];
			constantValues[i][0] = d;
		} catch (...) {		
			expressions[i]->bindExpression(symbolTable);
			if (expressions[i]->getSymbolBinding("x") != null ||
					expressions[i]->getSymbolBinding("y") != null ||
					expressions[i]->getSymbolBinding("z") != null) {
				needsXYZ[i] = true;
			}
		}
	}
}

double VarContext::getValue(MembraneElement* element, long expIndex)
{
	if (expressions[expIndex] == 0) { // not defined
		throw "VarContext::getValue(), expression not defined";
	}
	if (constantValues[expIndex] != NULL) {
		return constantValues[expIndex][0];
	}
	WorldCoord wc;
	sim->setCurrentCoordinate(mesh->getMembraneWorldCoord(element));
	int* indices = sim->getIndices();
	indices[VAR_MEMBRANE_INDEX] = element->index;
	indices[VAR_MEMBRANE_REGION_INDEX] = element->region->getId();
	return expressions[expIndex]->evaluateProxy();	
}

double VarContext::getConstantValue(long expIndex)
{	
	// for periodic boundary condition
	if (expressions[expIndex] == 0 || constantValues[expIndex] == 0) { // not defined
		throw "VarContext::getConstantValue() : expression not defined OR not a constant expression";
	}
	return constantValues[expIndex][0];	
}

double VarContext::getValue(long volIndex, long expIndex) {	
	if (expressions[expIndex] == 0) { // not defined
		throw "VarContext::getValue(), expression not defined";
	}
	if (constantValues[expIndex] != NULL) {
		return constantValues[expIndex][0];
	}
	if (needsXYZ[expIndex]) {
		WorldCoord wc = ((CartesianMesh*)mesh)->getVolumeWorldCoord(volIndex);
		sim->setCurrentCoordinate(wc);
	}
	int* indices = sim->getIndices();
	indices[VAR_VOLUME_INDEX] = volIndex;
	indices[VAR_VOLUME_REGION_INDEX] = mesh->getVolumeElements()[volIndex].region->getId();
	return expressions[expIndex]->evaluateProxy();	
}
#endif
