#pragma once

#include "Exception.h"
using namespace VCell;

class StoppedByUserException : public Exception
{
public:
	StoppedByUserException(string msg);
	~StoppedByUserException(void);
};