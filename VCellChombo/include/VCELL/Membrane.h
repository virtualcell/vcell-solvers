#ifndef MEMBRANE
#define MEMBRANE

#include <VCELL/Structure.h>
#include <vector>
using std::vector;

class Feature;
struct MembraneElement;
class MembraneVariable;
class MembraneVarContextExpression;
class MembraneRegionVariable;
class MembraneRegionVarContextExpression;
class Simulation;
class JumpCondition;
class SimulationExpression;
class Variable;
class Expression;

class Membrane : public Structure
{
public:
	Membrane(string& name, Feature* f1, Feature* f2);
	~Membrane();

	MembraneVarContextExpression *getMembraneVarContext(string& membraneVarName);
	MembraneVarContextExpression *getMembraneVarContext(MembraneVariable *var);
	void addMembraneVarContext(MembraneVarContextExpression *vc);

	MembraneRegionVarContextExpression *getMembraneRegionVarContext(MembraneRegionVariable *var);
	void addMembraneRegionVarContext(MembraneRegionVarContextExpression *vc);

	void resolveReferences(SimulationExpression *sim);

	Feature* getFeature1() {
		return feature1;
	}
	Feature* getFeature2() {
		return feature2;
	}
	bool inBetween(Feature* f1, Feature* f2);
	void reinitConstantValues();

private:
	Feature* feature1;
	Feature* feature2;

	vector<MembraneVarContextExpression*> membraneVarContextList;
	vector<MembraneRegionVarContextExpression*> membraneRegionVarContextList;
};

#endif
