// Begin CVS Header
//   $Source: /fs/turing/cvs/copasi_dev/copasi/UI/Attic/CQThread.h,v $
//   $Revision: 1.1.2.1 $
//   $Name: Build-33 $
//   $Author: shoops $
//   $Date: 2011/01/05 15:25:59 $
// End CVS Header

// Copyright (C) 2010 by Pedro Mendes, Virginia Tech Intellectual
// Properties, Inc., University of Heidelberg, and The University
// of Manchester.
// All rights reserved.

#ifndef COPASI_CQThread
#define COPASI_CQThread

#include <QThread>
#include "copasi/UI/DataModelGUI.h"

class CQThread : public QThread
{
  Q_OBJECT

private:
  CQThread();

public:
  CQThread(DataModelGUI * pClass, void (DataModelGUI::*pMethod)());

  ~CQThread();

  virtual void run();

private:
  DataModelGUI * mpClass;
  void (DataModelGUI::*mpMethod)();
};

#endif // COPASI_CQThread
