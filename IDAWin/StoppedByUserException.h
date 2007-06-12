#pragma once

#include "Exception.h"

class StoppedByUserException : public Exception
{
public:
	StoppedByUserException(string msg);
	~StoppedByUserException(void);
};