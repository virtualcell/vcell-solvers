#include <VCELL/Membrane.h>
#include <VCELL/MembraneVariable.h>
#include <VCELL/MembraneRegionVariable.h>
#include <VCELL/SimulationExpression.h>
#include <VCELL/Feature.h>
#include <VCELL/MembraneVarContextExpression.h>
#include <VCELL/MembraneRegionVarContextExpression.h>
#include <VCELL/JumpCondition.h>
#include <SimpleSymbolTable.h>
#include <sstream>
using std::stringstream;

Membrane::Membrane(string& name, Feature* f1, Feature* f2) : Structure(name)
{
	feature1 = f1;
	feature2 = f2;
}

Membrane::~Membrane(void)
{
	for (int i = 0; i < (int)membraneVarContextList.size(); i ++) {
		delete membraneVarContextList[i];
	}
	membraneVarContextList.clear();
	for (int i = 0; i < (int)membraneRegionVarContextList.size(); i ++) {
		delete membraneRegionVarContextList[i];
	}
	membraneRegionVarContextList.clear();
}

void Membrane::addMembraneVarContext(MembraneVarContextExpression *mvc)
{
	membraneVarContextList.push_back(mvc);
}

void Membrane::addMembraneRegionVarContext(MembraneRegionVarContextExpression *mrvc)
{
	membraneRegionVarContextList.push_back(mrvc);
}

MembraneVarContextExpression* Membrane::getMembraneVarContext(MembraneVariable *memVar)
{
	for (int i = 0; i < (int)membraneVarContextList.size(); i ++) {
		if (membraneVarContextList[i]->getVar() == memVar) {
			return membraneVarContextList[i];
		}
	}
	return 0;
}

MembraneRegionVarContextExpression* Membrane::getMembraneRegionVarContext(MembraneRegionVariable *memRegionVar)
{
	for (int i = 0; i < (int)membraneRegionVarContextList.size(); i ++) {
		if (membraneRegionVarContextList[i]->getVar() == memRegionVar) {
			return membraneRegionVarContextList[i];
		}
	}
	return 0;
}

bool Membrane::inBetween(Feature* f1, Feature* f2) {
	return (f1 == feature1 && f2 == feature2 || f1 == feature2 && f2 == feature1);
}

void Membrane::resolveReferences(SimulationExpression *sim)
{
	for (int i = 0; i < (int)membraneVarContextList.size(); i ++) {
		MembraneVarContextExpression *membraneVarContext = membraneVarContextList[i];
		membraneVarContext->resolveReferences(sim);
	}
	
	for (int i = 0; i < (int)membraneRegionVarContextList.size(); i ++) {
		MembraneRegionVarContextExpression* membraneRegionVarContext = membraneRegionVarContextList[i];
		membraneRegionVarContext->resolveReferences(sim);
	}
	
	for (int i = 0; i < sim->getNumMemVariables(); i ++) {
		Variable* var = sim->getMemVariable(i);
		if (isVariableDefined(var)) {
			feature1->addMemVarIndexInAdjacentMembrane(i);
			feature2->addMemVarIndexInAdjacentMembrane(i);
		}
	}
}

