#include <Species.h>
#include <cstring>
using moving_boundary::biology::Species;

Species::Species(const string & name)
				:name_(name),
				 bAdvecting(false)
{
	expressions = new SExpression*[expr_size];
	std::memset(expressions, 0, expr_size * sizeof(SExpression*));
}

Species::~Species()
{
	for (int i = 0; i < expr_size; ++ i)
	{
		delete expressions[i];
	}
	delete[] expressions;
}

void Species::bindExpressions(const SimpleSymbolTable &symTable) {
	for (int i = 0; i < expr_size; ++ i)
	{
		if (expressions[i] != nullptr)
		{
			expressions[i]->bindExpression(symTable);

			if (!bAdvecting)
			{
				// if advection X or Y is not constant or 0, set bAdvecting true
				if ((i == expr_advection_x || i == expr_advection_y) && (!expressions[i]->isConstant() || expressions[i]->constantValue() != 0))
				{
					bAdvecting = true;
				}
			}
		}
	}
}

void Species::setExpression(ExprIndex exprIndex, const string& expr)
{
	expressions[exprIndex] = new SExpression(expr);
}
