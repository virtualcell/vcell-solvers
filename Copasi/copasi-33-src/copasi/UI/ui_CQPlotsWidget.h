/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQPlotsWidget.ui'
**
** Created: Thu Aug 18 12:47:31 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQPLOTSWIDGET_H
#define UI_CQPLOTSWIDGET_H

#include <Qt3Support/Q3MimeSourceFactory>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QTableView>
#include <vector>
#include "copasi/UI/copasiWidget.h"
#include "copasiWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQPlotsWidget
{
public:
    QGridLayout *gridLayout;
    QHBoxLayout *hboxLayout;
    QSpacerItem *horizontalSpacerLeft;
    QPushButton *mpBtnNew;
    QPushButton *mpBtnDelete;
    QPushButton *mpBtnClear;
    QSpacerItem *horizontalSpacerRight;
    QTableView *mpTblPlots;
    QHBoxLayout *horizontalLayout;
    QLabel *label;
    QLineEdit *mpLEFilter;

    void setupUi(CopasiWidget *CQPlotsWidget)
    {
        if (CQPlotsWidget->objectName().isEmpty())
            CQPlotsWidget->setObjectName(QString::fromUtf8("CQPlotsWidget"));
        CQPlotsWidget->resize(541, 456);
        gridLayout = new QGridLayout(CQPlotsWidget);
        gridLayout->setSpacing(6);
        gridLayout->setContentsMargins(11, 11, 11, 11);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        horizontalSpacerLeft = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout->addItem(horizontalSpacerLeft);

        mpBtnNew = new QPushButton(CQPlotsWidget);
        mpBtnNew->setObjectName(QString::fromUtf8("mpBtnNew"));

        hboxLayout->addWidget(mpBtnNew);

        mpBtnDelete = new QPushButton(CQPlotsWidget);
        mpBtnDelete->setObjectName(QString::fromUtf8("mpBtnDelete"));
        mpBtnDelete->setFocusPolicy(Qt::NoFocus);

        hboxLayout->addWidget(mpBtnDelete);

        mpBtnClear = new QPushButton(CQPlotsWidget);
        mpBtnClear->setObjectName(QString::fromUtf8("mpBtnClear"));
        mpBtnClear->setFocusPolicy(Qt::NoFocus);

        hboxLayout->addWidget(mpBtnClear);

        horizontalSpacerRight = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout->addItem(horizontalSpacerRight);


        gridLayout->addLayout(hboxLayout, 8, 0, 1, 2);

        mpTblPlots = new QTableView(CQPlotsWidget);
        mpTblPlots->setObjectName(QString::fromUtf8("mpTblPlots"));
        mpTblPlots->setEditTriggers(QAbstractItemView::DoubleClicked|QAbstractItemView::EditKeyPressed|QAbstractItemView::SelectedClicked);
        mpTblPlots->setAlternatingRowColors(true);
        mpTblPlots->setSelectionMode(QAbstractItemView::ExtendedSelection);
        mpTblPlots->setSelectionBehavior(QAbstractItemView::SelectRows);
        mpTblPlots->setSortingEnabled(true);

        gridLayout->addWidget(mpTblPlots, 6, 0, 1, 2);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        label = new QLabel(CQPlotsWidget);
        label->setObjectName(QString::fromUtf8("label"));

        horizontalLayout->addWidget(label);

        mpLEFilter = new QLineEdit(CQPlotsWidget);
        mpLEFilter->setObjectName(QString::fromUtf8("mpLEFilter"));

        horizontalLayout->addWidget(mpLEFilter);


        gridLayout->addLayout(horizontalLayout, 3, 0, 3, 2);

        QWidget::setTabOrder(mpTblPlots, mpBtnDelete);
        QWidget::setTabOrder(mpBtnDelete, mpBtnClear);

        retranslateUi(CQPlotsWidget);
        QObject::connect(mpBtnClear, SIGNAL(pressed()), CQPlotsWidget, SLOT(slotBtnClearClicked()));
        QObject::connect(mpBtnDelete, SIGNAL(pressed()), CQPlotsWidget, SLOT(slotBtnDeleteClicked()));
        QObject::connect(mpTblPlots, SIGNAL(doubleClicked(QModelIndex)), CQPlotsWidget, SLOT(slotDoubleClicked(QModelIndex)));
        QObject::connect(mpBtnNew, SIGNAL(pressed()), CQPlotsWidget, SLOT(slotBtnNewClicked()));

        QMetaObject::connectSlotsByName(CQPlotsWidget);
    } // setupUi

    void retranslateUi(CopasiWidget *CQPlotsWidget)
    {
        CQPlotsWidget->setProperty("caption", QVariant(QApplication::translate("CQPlotsWidget", "Plots", 0, QApplication::UnicodeUTF8)));
        mpBtnNew->setText(QApplication::translate("CQPlotsWidget", "New", 0, QApplication::UnicodeUTF8));
        mpBtnDelete->setText(QApplication::translate("CQPlotsWidget", "Delete", 0, QApplication::UnicodeUTF8));
        mpBtnClear->setText(QApplication::translate("CQPlotsWidget", "Delete All", 0, QApplication::UnicodeUTF8));
        label->setText(QApplication::translate("CQPlotsWidget", "Search:", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQPlotsWidget: public Ui_CQPlotsWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQPLOTSWIDGET_H
