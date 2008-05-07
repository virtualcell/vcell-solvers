#ifndef FASTSYSTEMEXPRESSION_H
#define FASTSYSTEMEXPRESSION_H

#include <VCELL/FastSystem.h>
#include <string>
using namespace std;

class SimulationExpression;
class SimpleSymbolTable;
class Expression;

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

	void setPseudoConstants(string* symbols, Expression** expressions);
    void setFastRateExpressions(Expression** expressions);
	void setFastDependencyExpressions(string* symbols, Expression** expressions);
	void setJacobianExpressions(Expression** expressions);
	void setCoordinates(double time_sec, WorldCoord& wc);

    void initVars();
	void setDependentVariables(string* vars); // must be called before other setters
	void setIndependentVariables(string* vars); // must be called before other setters
	SimpleSymbolTable* getIndepPseudoFieldSymbolTable();
	void updateIndepValues();
	void updateMatrix();	

private:
	string* pseudoSymbols;
	SimulationExpression* simulation;
	double* pseudoConstants;
	SimpleSymbolTable* indepPseudoFieldSymbolTable;
	double* indepPseudoFieldValues;	
	Expression** pseudoConstantExpressions;
	Expression** fastRateExpressions;
	Expression** fastDependencyExpressions;
	Expression** jacobianExpressions;

	void bindAllExpressions();
};

#endif
