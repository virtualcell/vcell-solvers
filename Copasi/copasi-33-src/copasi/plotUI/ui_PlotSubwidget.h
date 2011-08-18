/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'PlotSubwidget.ui'
**
** Created: Thu Aug 18 12:47:22 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_PLOTSUBWIDGET_H
#define UI_PLOTSUBWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QTabWidget>
#include <QtGui/QToolButton>
#include <QtGui/QWidget>
#include "copasi/UI/copasiWidget.h"

QT_BEGIN_NAMESPACE

class Ui_PlotSubwidget
{
public:
    QGridLayout *PlotWidget1Layout;
    QLabel *labelTitle;
    QHBoxLayout *layoutTitle;
    QLineEdit *titleLineEdit;
    QCheckBox *activeCheckBox;
    QLabel *labelType;
    QLabel *labelScale;
    QCheckBox *checkLogX;
    QCheckBox *checkLogY;
    QFrame *line1;
    QLabel *labelCurves;
    QHBoxLayout *layoutCurves;
    QSpacerItem *spacerCurves;
    QToolButton *addCurveButton;
    QToolButton *addHistoButton;
    QToolButton *deleteCurveButton;
    QTabWidget *tabs;
    QWidget *tab;
    QWidget *tab_2;
    QFrame *lineButton;
    QHBoxLayout *layoutButtons;
    QPushButton *startPlotButton;
    QPushButton *resetButton;
    QPushButton *addPlotButton;
    QPushButton *deletePlotButton;
    QLineEdit *typeLineEdit;

    void setupUi(CopasiWidget *PlotSubwidget)
    {
        if (PlotSubwidget->objectName().isEmpty())
            PlotSubwidget->setObjectName(QString::fromUtf8("PlotSubwidget"));
        PlotSubwidget->resize(554, 450);
        PlotWidget1Layout = new QGridLayout(PlotSubwidget);
        PlotWidget1Layout->setObjectName(QString::fromUtf8("PlotWidget1Layout"));
        labelTitle = new QLabel(PlotSubwidget);
        labelTitle->setObjectName(QString::fromUtf8("labelTitle"));
        labelTitle->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

        PlotWidget1Layout->addWidget(labelTitle, 0, 0, 1, 1);

        layoutTitle = new QHBoxLayout();
        layoutTitle->setObjectName(QString::fromUtf8("layoutTitle"));
        titleLineEdit = new QLineEdit(PlotSubwidget);
        titleLineEdit->setObjectName(QString::fromUtf8("titleLineEdit"));

        layoutTitle->addWidget(titleLineEdit);

        activeCheckBox = new QCheckBox(PlotSubwidget);
        activeCheckBox->setObjectName(QString::fromUtf8("activeCheckBox"));

        layoutTitle->addWidget(activeCheckBox);


        PlotWidget1Layout->addLayout(layoutTitle, 0, 1, 1, 3);

        labelType = new QLabel(PlotSubwidget);
        labelType->setObjectName(QString::fromUtf8("labelType"));
        labelType->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

        PlotWidget1Layout->addWidget(labelType, 1, 0, 1, 1);

        labelScale = new QLabel(PlotSubwidget);
        labelScale->setObjectName(QString::fromUtf8("labelScale"));
        labelScale->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

        PlotWidget1Layout->addWidget(labelScale, 2, 0, 1, 1);

        checkLogX = new QCheckBox(PlotSubwidget);
        checkLogX->setObjectName(QString::fromUtf8("checkLogX"));

        PlotWidget1Layout->addWidget(checkLogX, 2, 1, 1, 1);

        checkLogY = new QCheckBox(PlotSubwidget);
        checkLogY->setObjectName(QString::fromUtf8("checkLogY"));

        PlotWidget1Layout->addWidget(checkLogY, 3, 1, 1, 1);

        line1 = new QFrame(PlotSubwidget);
        line1->setObjectName(QString::fromUtf8("line1"));
        line1->setFrameShape(QFrame::HLine);
        line1->setFrameShadow(QFrame::Sunken);

        PlotWidget1Layout->addWidget(line1, 4, 0, 1, 4);

        labelCurves = new QLabel(PlotSubwidget);
        labelCurves->setObjectName(QString::fromUtf8("labelCurves"));

        PlotWidget1Layout->addWidget(labelCurves, 5, 0, 1, 2);

        layoutCurves = new QHBoxLayout();
        layoutCurves->setObjectName(QString::fromUtf8("layoutCurves"));
        spacerCurves = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        layoutCurves->addItem(spacerCurves);

        addCurveButton = new QToolButton(PlotSubwidget);
        addCurveButton->setObjectName(QString::fromUtf8("addCurveButton"));

        layoutCurves->addWidget(addCurveButton);

        addHistoButton = new QToolButton(PlotSubwidget);
        addHistoButton->setObjectName(QString::fromUtf8("addHistoButton"));

        layoutCurves->addWidget(addHistoButton);

        deleteCurveButton = new QToolButton(PlotSubwidget);
        deleteCurveButton->setObjectName(QString::fromUtf8("deleteCurveButton"));

        layoutCurves->addWidget(deleteCurveButton);


        PlotWidget1Layout->addLayout(layoutCurves, 5, 3, 1, 1);

        tabs = new QTabWidget(PlotSubwidget);
        tabs->setObjectName(QString::fromUtf8("tabs"));
        tabs->setUsesScrollButtons(true);
        tab = new QWidget();
        tab->setObjectName(QString::fromUtf8("tab"));
        tabs->addTab(tab, QString());
        tab_2 = new QWidget();
        tab_2->setObjectName(QString::fromUtf8("tab_2"));
        tabs->addTab(tab_2, QString());

        PlotWidget1Layout->addWidget(tabs, 6, 0, 1, 4);

        lineButton = new QFrame(PlotSubwidget);
        lineButton->setObjectName(QString::fromUtf8("lineButton"));
        lineButton->setFrameShape(QFrame::HLine);
        lineButton->setFrameShadow(QFrame::Sunken);

        PlotWidget1Layout->addWidget(lineButton, 7, 0, 1, 4);

        layoutButtons = new QHBoxLayout();
        layoutButtons->setObjectName(QString::fromUtf8("layoutButtons"));
        startPlotButton = new QPushButton(PlotSubwidget);
        startPlotButton->setObjectName(QString::fromUtf8("startPlotButton"));

        layoutButtons->addWidget(startPlotButton);

        resetButton = new QPushButton(PlotSubwidget);
        resetButton->setObjectName(QString::fromUtf8("resetButton"));

        layoutButtons->addWidget(resetButton);

        addPlotButton = new QPushButton(PlotSubwidget);
        addPlotButton->setObjectName(QString::fromUtf8("addPlotButton"));

        layoutButtons->addWidget(addPlotButton);

        deletePlotButton = new QPushButton(PlotSubwidget);
        deletePlotButton->setObjectName(QString::fromUtf8("deletePlotButton"));

        layoutButtons->addWidget(deletePlotButton);


        PlotWidget1Layout->addLayout(layoutButtons, 8, 0, 1, 4);

        typeLineEdit = new QLineEdit(PlotSubwidget);
        typeLineEdit->setObjectName(QString::fromUtf8("typeLineEdit"));
        QSizePolicy sizePolicy(QSizePolicy::Ignored, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(typeLineEdit->sizePolicy().hasHeightForWidth());
        typeLineEdit->setSizePolicy(sizePolicy);
        typeLineEdit->setReadOnly(true);

        PlotWidget1Layout->addWidget(typeLineEdit, 1, 1, 1, 1);


        retranslateUi(PlotSubwidget);
        QObject::connect(addCurveButton, SIGNAL(clicked()), PlotSubwidget, SLOT(addCurveSlot()));
        QObject::connect(addHistoButton, SIGNAL(clicked()), PlotSubwidget, SLOT(addHistoSlot()));
        QObject::connect(deleteCurveButton, SIGNAL(clicked()), PlotSubwidget, SLOT(removeCurve()));
        QObject::connect(startPlotButton, SIGNAL(clicked()), PlotSubwidget, SLOT(commitPlot()));
        QObject::connect(resetButton, SIGNAL(clicked()), PlotSubwidget, SLOT(resetPlot()));
        QObject::connect(addPlotButton, SIGNAL(clicked()), PlotSubwidget, SLOT(addPlot()));
        QObject::connect(deletePlotButton, SIGNAL(clicked()), PlotSubwidget, SLOT(deletePlot()));

        QMetaObject::connectSlotsByName(PlotSubwidget);
    } // setupUi

    void retranslateUi(CopasiWidget *PlotSubwidget)
    {
        PlotSubwidget->setWindowTitle(QApplication::translate("PlotSubwidget", "Form", 0, QApplication::UnicodeUTF8));
        labelTitle->setText(QApplication::translate("PlotSubwidget", "Plot title", 0, QApplication::UnicodeUTF8));
        activeCheckBox->setText(QApplication::translate("PlotSubwidget", "active?", 0, QApplication::UnicodeUTF8));
        labelType->setText(QApplication::translate("PlotSubwidget", "Type", 0, QApplication::UnicodeUTF8));
        labelScale->setText(QApplication::translate("PlotSubwidget", "Axis scales", 0, QApplication::UnicodeUTF8));
        checkLogX->setText(QApplication::translate("PlotSubwidget", "log X-axis", 0, QApplication::UnicodeUTF8));
        checkLogY->setText(QApplication::translate("PlotSubwidget", "log Y-axis", 0, QApplication::UnicodeUTF8));
        labelCurves->setText(QApplication::translate("PlotSubwidget", "Curve specifications", 0, QApplication::UnicodeUTF8));
        addCurveButton->setText(QApplication::translate("PlotSubwidget", "New curve...", 0, QApplication::UnicodeUTF8));
        addHistoButton->setText(QApplication::translate("PlotSubwidget", "New histogram", 0, QApplication::UnicodeUTF8));
        deleteCurveButton->setText(QApplication::translate("PlotSubwidget", "Delete", 0, QApplication::UnicodeUTF8));
        tabs->setTabText(tabs->indexOf(tab), QApplication::translate("PlotSubwidget", "Tab 1", 0, QApplication::UnicodeUTF8));
        tabs->setTabText(tabs->indexOf(tab_2), QApplication::translate("PlotSubwidget", "Tab 2", 0, QApplication::UnicodeUTF8));
        startPlotButton->setText(QApplication::translate("PlotSubwidget", "Commit", 0, QApplication::UnicodeUTF8));
        resetButton->setText(QApplication::translate("PlotSubwidget", "Revert", 0, QApplication::UnicodeUTF8));
        addPlotButton->setText(QApplication::translate("PlotSubwidget", "New Plot", 0, QApplication::UnicodeUTF8));
        deletePlotButton->setText(QApplication::translate("PlotSubwidget", "Delete plot", 0, QApplication::UnicodeUTF8));
        typeLineEdit->setText(QApplication::translate("PlotSubwidget", "2D Plot", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class PlotSubwidget: public Ui_PlotSubwidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_PLOTSUBWIDGET_H
