#ifndef STOPPEDBYUSEREXCEPTION_H
#define STOPPEDBYUSEREXCEPTION_H

#include <Exception.h>

class StoppedByUserException : public VCell::Exception
{
public:
	StoppedByUserException(string msg);
	~StoppedByUserException(void) throw( );
};

#endif
