#include "ExpressionBindingException.h"

ExpressionBindingException::ExpressionBindingException(string msg) : ExpressionException("ExpressionBindingException", msg)
{
}

ExpressionBindingException::~ExpressionBindingException(void)
{
}

string ExpressionBindingException::identify(){
	return "ExpressionBindingException";
}
