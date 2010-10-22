#ifndef CONSTRAINT_H
#define CONSTRAINT_H

namespace VCell {
	class Expression;
}
class SymbolTable;

enum ConstraintType {EQUALITY_LINEAR=1, EQUALITY_NONLINEAR, INEQUALITY_LINEAR, INEQUALITY_NONLINEAR};

class Constraint {
public:
	Constraint(ConstraintType arg_constraintType, const char* expString);
	~Constraint();

	void bindExpression(SymbolTable* symbolTable);
	double evaluate(double* paramValues);
	ConstraintType getConstraintType() { return constraintType; }

private:
	VCell::Expression* exp;
	ConstraintType constraintType;
};

#endif
