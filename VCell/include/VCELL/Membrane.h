#ifndef MEMBRANE
#define MEMBRANE

#include <VCELL/Structure.h>
#include <vector>
using std::vector;

class Feature;
class Simulation;
struct MembraneElement;
class MembraneVarContext;
class MembraneRegionVarContext;
class MembraneVariable;
class MembraneRegionVariable;

class Membrane : public Structure
{
public:
	Membrane(string& name, Feature* f1, Feature* f2);
	~Membrane();


	virtual void initMembraneValues(MembraneElement *membraneElement);
	virtual void initMembraneRegionValues(int membraneRegionIndex);

	MembraneVarContext *getMembraneVarContext(string& membraneVarName);
	MembraneVarContext *getMembraneVarContext(MembraneVariable *var);
	void addMembraneVarContext(MembraneVarContext *vc);

	MembraneRegionVarContext *getMembraneRegionVarContext(MembraneRegionVariable *var);	
	void addMembraneRegionVarContext(MembraneRegionVarContext *vc);

	void resolveReferences(Simulation *sim);

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

	vector<MembraneVarContext*> membraneVarContextList;
	vector<MembraneRegionVarContext*> membraneRegionVarContextList;
};

#endif
