#include "Constraint.h"
#include <Expression.h>
#include <Exception.h>
using VCell::Exception;
using VCell::Expression;

Constraint::Constraint(ConstraintType arg_constraintType, const char* expstring) {
	if (exp==null){
		throw Exception("expression cannot be null");
	}
	constraintType = arg_constraintType;
	exp = new Expression(expstring);	
}

Constraint::~Constraint() {
	delete exp;
}

void Constraint::bindExpression(SymbolTable* symbolTable) {
	exp->bindExpression(symbolTable);
}


double Constraint::evaluate(double* paramValues) {
	return exp->evaluateVector(paramValues);
}
