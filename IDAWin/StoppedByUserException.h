#ifndef STOPPEDBYUSEREXCEPTION_H
#define STOPPEDBYUSEREXCEPTION_H

#include "Exception.h"
using namespace VCell;

class StoppedByUserException : public Exception
{
public:
	StoppedByUserException(string msg);
	~StoppedByUserException(void);
};

#endif
