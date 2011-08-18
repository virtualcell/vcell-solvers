// Begin CVS Header
//   $Source: /fs/turing/cvs/copasi_dev/copasi/UI/CQSplashWidget.ui.h,v $
//   $Revision: 1.4 $
//   $Name: Build-33 $
//   $Author: shoops $
//   $Date: 2010/09/22 13:22:54 $
// End CVS Header

// Copyright (C) 2010 by Pedro Mendes, Virginia Tech Intellectual
// Properties, Inc., University of Heidelberg, and The University
// of Manchester.
// All rights reserved.

// Copyright (C) 2008 by Pedro Mendes, Virginia Tech Intellectual
// Properties, Inc., EML Research, gGmbH, University of Heidelberg,
// and The University of Manchester.
// All rights reserved.

// Copyright (C) 2001 - 2007 by Pedro Mendes, Virginia Tech Intellectual
// Properties, Inc. and EML Research, gGmbH.
// All rights reserved.

/****************************************************************************
 ** ui.h extension file, included from the uic-generated form implementation.
 **
 ** If you want to add, delete, or rename functions or slots, use
 ** Qt Designer to update this file, preserving your code.
 **
 ** You should not define a constructor or destructor in this file.
 ** Instead, write your code in functions called init() and destroy().
 ** These will automatically be called by the form's constructor and
 ** destructor.
 *****************************************************************************/

#include "copasi.h"

#include "qtUtilities.h"
#include "AboutDialog.h"

#include "utilities/CVersion.h"
#include "report/CCopasiRootContainer.h"

void CQSplashWidget::init()
{
  mpLblVersion->setText(mpLblVersion->text().arg(FROM_UTF8(CVersion::VERSION.getVersion())));

  return;
}

void CQSplashWidget::slotViewLicense()
{
  QString FixedTitle = "COPASI ";
  FixedTitle += FROM_UTF8(CVersion::VERSION.getVersion());

  AboutDialog* aboutDialog = new AboutDialog(this, CCopasiRootContainer::getLicenseHTML(), 76, 30);
  aboutDialog->setCaption(FixedTitle);
  aboutDialog->exec();
}
