/* Begin CVS Header
   $Source: /fs/turing/cvs/copasi_dev/copasi/utilities/CCopasiException.h,v $
   $Revision: 1.6 $
   $Name: Build-33 $
   $Author: shoops $
   $Date: 2010/09/16 18:26:45 $
   End CVS Header */

// Copyright (C) 2010 by Pedro Mendes, Virginia Tech Intellectual
// Properties, Inc., University of Heidelberg, and The University
// of Manchester.
// All rights reserved.

// Copyright (C) 2001 - 2007 by Pedro Mendes, Virginia Tech Intellectual
// Properties, Inc. and EML Research, gGmbH.
// All rights reserved.

/**
 *  CCopasiException class. A more elaborate class description.
 */

// New Class for C++ exception handling.
// (C) Stefan Hoops 2001

#ifndef COPASI_CCopasiException
#define COPASI_CCopasiException

#include "CCopasiMessage.h"

class CCopasiException
{
private:
  /**
   * Default consructor.
   */
  CCopasiException();

public:
  /**
   * Specific consructor. Creates a exception with a message.
   * @param const CCopasiMessage & message
   */
  CCopasiException(const CCopasiMessage & message);

  /**
   * Destructor.
   */
  ~CCopasiException();

  /**
   * Retrieves the Message associated with the exception
   * @return const CCopasiMessage & message
   */
  const CCopasiMessage & getMessage() const;

private:
  const CCopasiMessage mMessage;
};
#endif // COPASI_CCopasiException
