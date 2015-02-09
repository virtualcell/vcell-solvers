#ifndef EXPRESSIONEXCEPTION_H
#define EXPRESSIONEXCEPTION_H

#include "Exception.h"
using namespace VCell;

class ExpressionException : public Exception
{
public:
	~ExpressionException(void);
	ExpressionException(string msg);
	ExpressionException(string title, string msg);
};

#endif
