/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef VARCONTEXT_H
#define VARCONTEXT_H

#include <VCELL/SimTypes.h>
#include <VCELL/Mesh.h>

#include <string>
using namespace std;

enum EXPRESSION_INDEX {INITIAL_VALUE_EXP=0, DIFF_RATE_EXP, REACT_RATE_EXP, BOUNDARY_XM_EXP, BOUNDARY_XP_EXP, 
	BOUNDARY_YM_EXP, BOUNDARY_YP_EXP, BOUNDARY_ZM_EXP, BOUNDARY_ZP_EXP, VELOCITY_X_EXP, VELOCITY_Y_EXP, VELOCITY_Z_EXP,
	IN_FLUX_EXP, OUT_FLUX_EXP, UNIFORM_RATE_EXP};
static string String_Expression_Index[] = {"INITIAL_VALUE_EXP", "DIFF_RATE_EXP", "REACT_RATE_EXP", "BOUNDARY_XM_EXP", "BOUNDARY_XP_EXP", 
	"BOUNDARY_YM_EXP", "BOUNDARY_YP_EXP", "BOUNDARY_ZM_EXP", "BOUNDARY_ZP_EXP", "VELOCITY_X_EXP", "VELOCITY_Y_EXP", "VELOCITY_Z_EXP",
	"IN_FLUX_EXP", "OUT_FLUX_EXP", "UNIFORM_RATE_EXP"};
#define TOTAL_NUM_EXPRESSIONS (UNIFORM_RATE_EXP + 1)
class Expression;
class SymbolTable;

class Variable;
class VolumeVariable;
class Feature;
class Simulation;
class Mesh;
class EqnBuilder;

class VarContext {

public:
	~VarContext();

	Variable *getVar() { return species; }
	string getVarName() { return speciesName; }
	
	//
	// ALWAYS CALL ParentClass::resolveReferences() first
	//
	virtual void resolveReferences(Simulation *sim);

	virtual double getInitialValue(long index);
	
	bool isInit() { return bInitialized; }
	virtual bool hasExact() { return false; }
	virtual bool hasRemainders() { return false; }
	
	Feature       *getParent() { return feature; }

	void setExpression(Expression* newexp, int expIndex);
	void bindAll(SymbolTable* symbolTable);
	virtual bool hasConstantDiffusion() {
		return (constantValues != 0 && constantValues[DIFF_RATE_EXP] != 0);
	}
	double evalExpression(long expIndex, double* values);
	double getExpressionConstantValue(long expIndex);

protected:
    VarContext(Feature *feature, string& speciesName);
    
    Variable      *species;
    string        speciesName;
    Feature       *feature;
    bool        bInitialized;
    
    double        *initialValue;
    Mesh          *mesh;
    Simulation    *sim;

	Expression** expressions;	
	double** constantValues;
	bool* needsXYZ;

	double getExpressionValue(long volIndex, long expIndex);
	double getExpressionValue(MembraneElement* element, long expIndex);
	virtual bool isNullExpressionOK(int expIndex) { return false; }
	Expression* getExpression(int expIndex)  { return expressions[expIndex]; }	

private:
    friend class Feature;
    friend class Contour;
};

#endif
