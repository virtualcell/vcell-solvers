#include <Species.h>
using moving_boundary::biology::Species;

Species::Species(const string & name, const string& initial, const string & source, const string &diffusion)
				:name_(name)
{
	expressions = new SExpression[expr_size];
	expressions[expr_initial] = SExpression(initial);
	expressions[expr_source] = SExpression(source);
	expressions[expr_diffusion] = SExpression(diffusion);
}

Species::~Species()
{
	delete[] expressions;
}

void Species::bindExpressions(const SimpleSymbolTable &symTable) {
	for (int i = 0; i < expr_size; ++ i)
	{
		expressions[i].bindExpression(symTable);
	}
}
