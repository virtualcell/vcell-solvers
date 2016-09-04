#ifndef PointSubdomain_h
#define PointSubdomain_h
#include <Subdomain.h>
#include <SExpression.h>
#include <MBConstants.h>
#include <vector>
using std::vector;

namespace moving_boundary
{
struct PointSubdomain : public Subdomain
{
public:
	PointSubdomain(const string & name, const string& posX, const string& posY);
	virtual ~PointSubdomain();

	SubdomainType getType()
	{
		return subdomain_point;
	}

	void bindExpressions(SymbolTable* symbolTable);

protected:
	string name_;
	SExpression* pos[DIM];
};
}

#endif
