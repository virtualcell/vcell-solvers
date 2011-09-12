/* Begin CVS Header
   $Source: /fs/turing/cvs/copasi_dev/copasi/utilities/CCopasiException.cpp,v $
   $Revision: 1.6 $
   $Name: Build-33 $
   $Author: shoops $
   $Date: 2006/04/27 01:32:42 $
   End CVS Header */

// Copyright © 2005 by Pedro Mendes, Virginia Tech Intellectual
// Properties, Inc. and EML Research, gGmbH.
// All rights reserved.

// CErrorMessage
//
// New Class based on pmutils read functionality
// (C) Stefan Hoops 2001

#include <string>

#include "copasi.h"
#include "CCopasiException.h"

CCopasiException::CCopasiException(void):
    mMessage()
{}

CCopasiException::CCopasiException(const CCopasiMessage & message):
    mMessage(message)
{}

CCopasiException::~CCopasiException(void) {}

const CCopasiMessage & CCopasiException::getMessage() const
  {return mMessage;}
