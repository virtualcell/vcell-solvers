#include <SExpression.h>
#include <VCellException.h>
#include <vector>
#include <array>
#include <limits>
using moving_boundary::SExpression;

SExpression::SExpression(const string& exp)
	:expression(exp),
	 constValue(nullptr)
{
	tryConstant();
}

SExpression::SExpression(const string& exp, const SymbolTable &symTable)
	:expression(exp),
	 constValue(nullptr)
{
	tryConstant();
	bindExpression(symTable);
}

double SExpression::evaluate(double* values) const
{
	if (constValue != nullptr)
	{
		return *constValue;
	}
	return expression.evaluateVector(values);
}

void SExpression::tryConstant()
{
	if (expression.isConstant())
	{
		constValue = new double;
		*constValue = expression.evaluateConstant();
	}
}

double SExpression::constantValue( ) const
{
	if (constValue == nullptr)
	{
		std::stringstream ss;
		ss << "Expression " << infix() << " is not a constant." << std::endl;
		throw ss.str();
	}
	return *constValue;
}



	
