#include <SExpression.h>
#include <VCellException.h>
#include <vector>
#include <array>
#include <limits>
using moving_boundary::SExpression;

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



	
