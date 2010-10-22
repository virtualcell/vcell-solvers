#ifndef FASTSYSTEMEXPRESSION_H
#define FASTSYSTEMEXPRESSION_H

#include <VCELL/FastSystem.h>
#include <string>
using namespace std;

class SimulationExpression;
class SimpleSymbolTable;
namespace VCell {
	class Expression;
}

//-----------------------------------------------------------------
//
//  class FastSystem
//
//-----------------------------------------------------------------
class FastSystemExpression : public FastSystem {
public:
    FastSystemExpression(int dimension, int numDepend, SimulationExpression* sim);
	~FastSystemExpression();
	
	virtual void resolveReferences(Simulation *sim);
	void updateDependentVars();

	void setPseudoConstants(string* symbols, VCell::Expression** expressions);
    void setFastRateExpressions(VCell::Expression** expressions);
	void setFastDependencyExpressions(string* symbols, VCell::Expression** expressions);
	void setJacobianExpressions(VCell::Expression** expressions);
	void setCoordinates(double time_sec, WorldCoord& wc);

    void initVars();
	void setDependentVariables(string* vars); // must be called before other setters
	void setIndependentVariables(string* vars); // must be called before other setters
	void updateIndepValues();
	void updateMatrix();	

private:
	string* pseudoSymbols;
	SimulationExpression* simulation;
	double* pseudoConstants;
	SimpleSymbolTable* fastSymbolTable; // include independent, pseudo, field data, random variables;
	double* fastValues;	
	VCell::Expression** pseudoConstantExpressions;
	VCell::Expression** fastRateExpressions;
	VCell::Expression** fastDependencyExpressions;
	VCell::Expression** jacobianExpressions;

	void bindAllExpressions();
	SimpleSymbolTable* getFastSymbolTable();
};

#endif
