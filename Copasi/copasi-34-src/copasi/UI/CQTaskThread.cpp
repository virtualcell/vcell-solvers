// Begin CVS Header
//   $Source: /fs/turing/cvs/copasi_dev/copasi/UI/CQTaskThread.cpp,v $
//   $Revision: 1.4.2.1 $
//   $Name: Build-33 $
//   $Author: shoops $
//   $Date: 2011/02/02 16:29:25 $
// End CVS Header

// Copyright (C) 2010 by Pedro Mendes, Virginia Tech Intellectual
// Properties, Inc., University of Heidelberg, and The University
// of Manchester.
// All rights reserved.

#include "copasi.h"
#include "utilities/CCopasiException.h"
#include "TaskWidget.h"


#include "CQTaskThread.h"

CQTaskThread::CQTaskThread(TaskWidget *tw):
    QThread(),
    mpTaskWidget(tw),
    mSuccess(false)
{}

CQTaskThread::~CQTaskThread()
{
}

void CQTaskThread::run()
{
  try
    {
      mSuccess = mpTaskWidget->getTask()->process(true);
    }
  catch (CCopasiException Exception)
    {
      mSuccess = false;
    }
}

const bool & CQTaskThread::success() const
{
  return mSuccess;
}
