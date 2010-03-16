/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef VARCONTEXT_H
#define VARCONTEXT_H

#include <vector>
#include <string>
using std::string;
using std::vector;

enum EXPRESSION_INDEX {INITIAL_VALUE_EXP=0, DIFF_RATE_EXP, REACT_RATE_EXP, 
	BOUNDARY_XM_EXP, BOUNDARY_XP_EXP, BOUNDARY_YM_EXP, BOUNDARY_YP_EXP, 
	BOUNDARY_ZM_EXP, BOUNDARY_ZP_EXP, VELOCITY_X_EXP, VELOCITY_Y_EXP, VELOCITY_Z_EXP,
	FLUX_EXP, UNIFORM_RATE_EXP};
static string String_Expression_Index[] = {"INITIAL_VALUE_EXP", "DIFF_RATE_EXP", "REACT_RATE_EXP", 
	"BOUNDARY_XM_EXP", "BOUNDARY_XP_EXP", "BOUNDARY_YM_EXP", "BOUNDARY_YP_EXP", 
	"BOUNDARY_ZM_EXP", "BOUNDARY_ZP_EXP", "VELOCITY_X_EXP", "VELOCITY_Y_EXP", "VELOCITY_Z_EXP",
	"IN_FLUX_EXP", "OUT_FLUX_EXP", "UNIFORM_RATE_EXP"};
#define TOTAL_NUM_EXPRESSIONS (UNIFORM_RATE_EXP + 1)

class Expression;
class Variable;
class VolumeVariable;
class Structure;
class Simulation;
class Mesh;
class EqnBuilder;
class SimulationExpression;
class Membrane;
class JumpCondition;
struct MembraneElement;

class VarContext {

public:
	~VarContext();

	Variable *getVar() { return species; }
	
	//
	// ALWAYS CALL ParentClass::resolveReferences() first
	//
	virtual void resolveReferences(Simulation *sim);

	virtual double getInitialValue(long index);
	
	virtual bool hasExact() { return false; }
	virtual bool hasRemainders() { return false; }
	
	Structure  *getStructure() { return structure; }

	void setExpression(Expression* newexp, int expIndex);

	// exclusively for sundials pde
	double evaluateJumpCondition(MembraneElement*, double* values);
	double evaluateExpression(long expIndex, double* values);
	double evaluateConstantExpression(long expIndex);

	void addJumpCondition(Membrane* membrane, Expression* exp);	
	JumpCondition* getJumpCondition();
	void reinitConstantValues();

protected:
    VarContext(Structure *s, Variable* var);

    Variable *species;
    Structure *structure;
    Simulation    *sim;

	void bindAll(SimulationExpression* simulation);
	double evaluateJumpCondition(MembraneElement*);
	double evaluateExpression(long volIndex, long expIndex); // for volume
	double evaluateMembraneRegionExpression(long memRegionIdex, long expIndex); // for membrane region
	double evaluateVolumeRegionExpression(long volRegionIdex, long expIndex); // for volume regin
	double evaluateExpression(MembraneElement* element, long expIndex); // for membrane

	virtual bool isNullExpressionOK(int expIndex) { return false; }
	bool isConstantExpression(long expIndex);

private:
	Expression** expressions;
	double** constantValues;
	bool* needsXYZ;

	vector<JumpCondition*> jumpConditionList;
};

#endif
