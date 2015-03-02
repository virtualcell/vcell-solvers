#include "StoppedByUserException.h"

StoppedByUserException::StoppedByUserException(string msg) : Exception("StoppedByUserException: " + msg)
{	
}

StoppedByUserException::~StoppedByUserException(void) throw( )
{
}
