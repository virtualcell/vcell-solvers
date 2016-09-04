#include <Variable.h>
#include <cstring>
#include <sstream>
using moving_boundary::Variable;
using std::stringstream;
using std::endl;

Variable::Variable(const string & name)
				:name_(name),
				 bAdvecting(false)
{
	expressions = new SExpression*[expr_size];
	for (int i = 0; i < expr_size; ++ i)
	{
		expressions[i] = nullptr;
	}
}

Variable::~Variable()
{
	for (int i = 0; i < expr_size; ++ i)
	{
		delete expressions[i];
	}
	delete[] expressions;
}

void Variable::bindExpressions(const SimpleSymbolTable &symTable) {
	forbidNullExpression(expr_initial);
	forbidNullExpression(expr_source);
	if (getType() == vartype_volume)
	{
		forbidNullExpression(expr_diffusion);
	}

	for (int i = 0; i < expr_size; ++ i)
	{
		if (expressions[i] != nullptr)
		{
			expressions[i]->bindExpression(symTable);

			if (!bAdvecting)
			{
				// if advection X or Y is not constant or 0, set bAdvecting true
				if ((i == expr_advection_x || i == expr_advection_y) && isExpressionNonZero((ExpressionIndex)i))
				{
					bAdvecting = true;
				}
			}
		}
	}
}

void Variable::setExpression(ExpressionIndex exprIndex, const string& expr)
{
	expressions[exprIndex] = new SExpression(expr);
}

void Variable::forbidNullExpression(ExpressionIndex exprIndex) const
{
	if (expressions[exprIndex] == nullptr)
	{
		stringstream ss;
		ss << "Expression for " << ExpressionDescription[exprIndex] << " is null " << endl;
		throw ss.str();
	}
}

double Variable::evaluateExpression(ExpressionIndex exprIndex, double* inputValues) const
{
	forbidNullExpression(exprIndex);
	return expressions[exprIndex]->evaluate(inputValues);
}


double Variable::getExpressionConstantValue(ExpressionIndex exprIndex) const
{
	forbidNullExpression(exprIndex);
	return expressions[exprIndex]->constantValue();
}

bool Variable::isExpressionConstant(ExpressionIndex exprIndex) const
{
	return expressions[exprIndex] != nullptr && expressions[exprIndex]->isConstant();
}

bool Variable::isExpressionNonZero(ExpressionIndex exprIndex) const
{
	return expressions[exprIndex] != nullptr && (!expressions[exprIndex]->isConstant() || expressions[exprIndex]->constantValue() != 0);
}
