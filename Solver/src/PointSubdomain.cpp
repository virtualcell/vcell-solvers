#include <PointSubdomain.h>
using moving_boundary::PointSubdomain;

PointSubdomain::PointSubdomain(const string & name, const string& posX, const string& posY)
				:Subdomain(name)
{
	pos[0] = new SExpression(posX);
	pos[1] = new SExpression(posY);
}

PointSubdomain::~PointSubdomain()
{
	for (int i = 0; i < DIM; ++ i)
	{
		delete pos[i];
	}
}

void PointSubdomain::bindExpressions(SymbolTable* symbolTable)
{
	for (int i = 0; i < DIM; ++ i)
	{
		pos[i]->bindExpression(*symbolTable);
	}
}
