// Begin CVS Header
//   $Source: /fs/turing/cvs/copasi_dev/copasi/layoutUI/NodeSizePanel.h,v $
//   $Revision: 1.2 $
//   $Name: Build-33 $
//   $Author: gauges $
//   $Date: 2010/02/03 13:53:00 $
// End CVS Header

// Copyright (C) 2008 by Pedro Mendes, Virginia Tech Intellectual
// Properties, Inc., EML Research, gGmbH, University of Heidelberg,
// and The University of Manchester.
// All rights reserved.

#ifndef NODESIZEPANEL_H__
#define NODESIZEPANEL_H__

#include "ui_NodeSizePanel.h"

class NodeSizePanel : public QDialog, public Ui::NodeSizePanel
{
  Q_OBJECT

public:
  NodeSizePanel(QWidget* parent = 0, bool modal = false, Qt::WindowFlags fl = 0);

private slots:
  void setMinAndMaxValues();
  void cancel();
  void setMinValue();
  void setMaxValue();
};

#endif // NODESIZEPANEL_H__
