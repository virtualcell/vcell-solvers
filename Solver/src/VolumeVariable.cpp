#include <VolumeVariable.h>
#include <cstring>
using moving_boundary::biology::VolumeVariable;

VolumeVariable::VolumeVariable(const string & name)
				:name_(name),
				 bAdvecting(false)
{
	expressions = new SExpression*[expr_size];
	std::memset(expressions, 0, expr_size * sizeof(SExpression*));
}

VolumeVariable::~VolumeVariable()
{
	for (int i = 0; i < expr_size; ++ i)
	{
		delete expressions[i];
	}
	delete[] expressions;
}

void VolumeVariable::bindExpressions(const SimpleSymbolTable &symTable) {
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

void VolumeVariable::setExpression(ExpressionIndex exprIndex, const string& expr)
{
	expressions[exprIndex] = new SExpression(expr);
}
