/* Begin CVS Header
   $Source: /fs/turing/cvs/copasi_dev/copasi/UI/CQStartValueReset.ui.h,v $
   $Revision: 1.1 $
   $Name: Build-33 $
   $Author: shoops $
   $Date: 2006/08/18 16:33:59 $
   End CVS Header */

// Copyright © 2006 by Pedro Mendes, Virginia Tech Intellectual
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

void CQStartValueReset::slotModel()
{
  done(MODEL);
}

void CQStartValueReset::slotRandom()
{
  done(RANDOM);
}

void CQStartValueReset::slotSolution()
{
  done(SOLUTION);
}
