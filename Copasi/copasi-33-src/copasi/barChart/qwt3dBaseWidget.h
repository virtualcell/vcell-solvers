// Begin CVS Header
//   $Source: /fs/turing/cvs/copasi_dev/copasi/barChart/qwt3dBaseWidget.h,v $
//   $Revision: 1.5 $
//   $Name: Build-33 $
//   $Author: shoops $
//   $Date: 2010/07/16 18:54:05 $
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

#ifndef BASE_WIDGET_H
#define BASE_WIDGET_H

#include <QWidget>
class QLabel;
class QVBoxLayout;
class QHBoxLayout;
class QGridLayout;
class QLabel;
class QSlider;
class QFrame;

class BaseWidget : public QWidget
{
  Q_OBJECT

public:
  BaseWidget(QWidget* parent = 0, const char* name = 0, Qt::WFlags fl = 0);
  ~BaseWidget();

  bool mpSlider;

  QLabel* mpLabelColumn;
  QLabel* mpLabelRow;
  QSlider* mpSliderColumn;
  QSlider* mpSliderRow;
  QFrame* mpFrame;

  QHBoxLayout* mpHBoxBig;
  QVBoxLayout* mpVBoxBig;
  QVBoxLayout* mpVBoxSmall;
  QHBoxLayout* mpHBoxSmall;

  void activateSlider();
  virtual void sliderMoved(int column, int row) = 0;

  int scaleFactor();

protected:
  QGridLayout* mpBaseWidgetLayout;

  /*!
    Scale factor for scalarizing QSlider and Qwt3D
  */
  int mScaleFactor;

protected slots:
  virtual void languageChange();
  void setSlider();
};

#endif // BASE_WIDGET_H
