// Begin CVS Header
//   $Source: /fs/turing/cvs/copasi_dev/copasi/UI/CQExperimentSelection.h,v $
//   $Revision: 1.7 $
//   $Name: Build-33 $
//   $Author: shoops $
//   $Date: 2010/06/09 16:04:29 $
// End CVS Header

// Copyright (C) 2010 by Pedro Mendes, Virginia Tech Intellectual
// Properties, Inc., University of Heidelberg, and The University
// of Manchester.
// All rights reserved.

#ifndef CQEXPERIMENTSELECTION_H
#define CQEXPERIMENTSELECTION_H

#include <QVariant>
#include <QDialog>

#include "ui_CQExperimentSelection.h"

class QComboBox;
class CExperimentSet;

class CQExperimentSelection : public QDialog, public Ui::CQExperimentSelection
{
  Q_OBJECT

public:
  CQExperimentSelection(QWidget* parent = 0, const char* name = 0, bool modal = false, Qt::WindowFlags fl = 0);
  ~CQExperimentSelection();

  virtual void load(QComboBox * pBox, const CExperimentSet * pExperimentSet);

protected:
  QComboBox * mpBox;

protected slots:
  virtual void languageChange();

  void slotBtnOK();
  void slotBtnCancel();
  void slotBtnAll();
  void slotBtnNone();


private:
  void init();

};

#endif // CQEXPERIMENTSELECTION_H
