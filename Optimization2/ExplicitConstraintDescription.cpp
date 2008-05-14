// OptSolverLibrary.cpp : Defines the entry point for the application.
//

#include "ExplicitConstraintDescription.h"
#include "Constraint.h"
#include "SymbolTable.h"

#include <float.h>
#include <iostream>
using namespace std;

ExplicitConstraintDescription::ExplicitConstraintDescription(vector<Constraint*> arg_constraints, SymbolTable* symbolTable)
{
	constraintList.clear();
	numConstraintEvals = 0;

	numNonLinearInequality = 0;
	numLinearInequality = 0;
    numNonLinearEquality = 0;
	numLinearEquality = 0;
	
	//
	// put them into a particular order for CFSQP
	//
	for (unsigned int i = 0; i < arg_constraints.size(); i ++) {
		if (arg_constraints[i]->getConstraintType()==INEQUALITY_NONLINEAR){
			numNonLinearInequality++;
			arg_constraints[i]->bindExpression(symbolTable);
			constraintList.push_back(arg_constraints[i]);
		}
	}
	for (unsigned int i = 0; i < arg_constraints.size(); i ++) {
		if (arg_constraints[i]->getConstraintType()==INEQUALITY_LINEAR){
			numLinearInequality++;
			arg_constraints[i]->bindExpression(symbolTable);
			constraintList.push_back(arg_constraints[i]);
		}
	}
	for (unsigned int i = 0; i < arg_constraints.size(); i ++) {
		if (arg_constraints[i]->getConstraintType()==EQUALITY_NONLINEAR){
			numNonLinearEquality++;
			arg_constraints[i]->bindExpression(symbolTable);
			constraintList.push_back(arg_constraints[i]);
		}
	}
	for (unsigned int i = 0; i < arg_constraints.size(); i ++) {
		if (arg_constraints[i]->getConstraintType()==EQUALITY_LINEAR){
			numLinearEquality++;
			arg_constraints[i]->bindExpression(symbolTable);
			constraintList.push_back(arg_constraints[i]);
		}
	}
}

ExplicitConstraintDescription::~ExplicitConstraintDescription(){
	constraintList.clear();
}

void ExplicitConstraintDescription::constraints(int nparams, int j, double *x, double *g){
	*g = constraintList[j-1]->evaluate(x);
}

int ExplicitConstraintDescription::getNumNonlinearInequality(){
	return numNonLinearInequality;
}

int ExplicitConstraintDescription::getNumLinearInequality(){
	return numLinearInequality;
}

int ExplicitConstraintDescription::getNumNonlinearEquality(){
	return numNonLinearEquality;
}

int ExplicitConstraintDescription::getNumLinearEquality(){
	return numLinearEquality;
}
