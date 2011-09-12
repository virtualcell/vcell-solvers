/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQFunctionsWidget.ui'
**
** Created: Sun Sep 11 10:59:24 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQFUNCTIONSWIDGET_H
#define UI_CQFUNCTIONSWIDGET_H

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
#include <vector>
#include "copasi/UI/CQTableView.h"
#include "copasi/UI/copasiWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQFunctionsWidget
{
public:
    QGridLayout *gridLayout;
    QHBoxLayout *hboxLayout;
    QSpacerItem *horizontalSpacerLeft;
    QPushButton *mpBtnNew;
    QPushButton *mpBtnDelete;
    QPushButton *mpBtnClear;
    QSpacerItem *horizontalSpacerRight;
    CQTableView *mpTblFunctions;
    QHBoxLayout *horizontalLayout;
    QLabel *label;
    QLineEdit *mpLEFilter;

    void setupUi(CopasiWidget *CQFunctionsWidget)
    {
        if (CQFunctionsWidget->objectName().isEmpty())
            CQFunctionsWidget->setObjectName(QString::fromUtf8("CQFunctionsWidget"));
        CQFunctionsWidget->resize(541, 456);
        gridLayout = new QGridLayout(CQFunctionsWidget);
        gridLayout->setSpacing(6);
        gridLayout->setContentsMargins(11, 11, 11, 11);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        horizontalSpacerLeft = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout->addItem(horizontalSpacerLeft);

        mpBtnNew = new QPushButton(CQFunctionsWidget);
        mpBtnNew->setObjectName(QString::fromUtf8("mpBtnNew"));

        hboxLayout->addWidget(mpBtnNew);

        mpBtnDelete = new QPushButton(CQFunctionsWidget);
        mpBtnDelete->setObjectName(QString::fromUtf8("mpBtnDelete"));
        mpBtnDelete->setFocusPolicy(Qt::NoFocus);

        hboxLayout->addWidget(mpBtnDelete);

        mpBtnClear = new QPushButton(CQFunctionsWidget);
        mpBtnClear->setObjectName(QString::fromUtf8("mpBtnClear"));
        mpBtnClear->setFocusPolicy(Qt::NoFocus);

        hboxLayout->addWidget(mpBtnClear);

        horizontalSpacerRight = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout->addItem(horizontalSpacerRight);


        gridLayout->addLayout(hboxLayout, 8, 0, 1, 2);

        mpTblFunctions = new CQTableView(CQFunctionsWidget);
        mpTblFunctions->setObjectName(QString::fromUtf8("mpTblFunctions"));
        mpTblFunctions->setEditTriggers(QAbstractItemView::CurrentChanged|QAbstractItemView::EditKeyPressed|QAbstractItemView::SelectedClicked);
        mpTblFunctions->setAlternatingRowColors(true);
        mpTblFunctions->setSelectionMode(QAbstractItemView::ExtendedSelection);
        mpTblFunctions->setSelectionBehavior(QAbstractItemView::SelectRows);
        mpTblFunctions->setHorizontalScrollMode(QAbstractItemView::ScrollPerPixel);
        mpTblFunctions->setSortingEnabled(true);

        gridLayout->addWidget(mpTblFunctions, 6, 0, 1, 2);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        label = new QLabel(CQFunctionsWidget);
        label->setObjectName(QString::fromUtf8("label"));

        horizontalLayout->addWidget(label);

        mpLEFilter = new QLineEdit(CQFunctionsWidget);
        mpLEFilter->setObjectName(QString::fromUtf8("mpLEFilter"));

        horizontalLayout->addWidget(mpLEFilter);


        gridLayout->addLayout(horizontalLayout, 3, 0, 3, 2);

        QWidget::setTabOrder(mpTblFunctions, mpBtnDelete);
        QWidget::setTabOrder(mpBtnDelete, mpBtnClear);

        retranslateUi(CQFunctionsWidget);
        QObject::connect(mpBtnClear, SIGNAL(pressed()), CQFunctionsWidget, SLOT(slotBtnClearClicked()));
        QObject::connect(mpBtnDelete, SIGNAL(pressed()), CQFunctionsWidget, SLOT(slotBtnDeleteClicked()));
        QObject::connect(mpTblFunctions, SIGNAL(doubleClicked(QModelIndex)), CQFunctionsWidget, SLOT(slotDoubleClicked(QModelIndex)));
        QObject::connect(mpBtnNew, SIGNAL(pressed()), CQFunctionsWidget, SLOT(slotBtnNewClicked()));

        QMetaObject::connectSlotsByName(CQFunctionsWidget);
    } // setupUi

    void retranslateUi(CopasiWidget *CQFunctionsWidget)
    {
        CQFunctionsWidget->setProperty("caption", QVariant(QApplication::translate("CQFunctionsWidget", "Functions", 0, QApplication::UnicodeUTF8)));
        mpBtnNew->setText(QApplication::translate("CQFunctionsWidget", "New", 0, QApplication::UnicodeUTF8));
        mpBtnDelete->setText(QApplication::translate("CQFunctionsWidget", "Delete", 0, QApplication::UnicodeUTF8));
        mpBtnClear->setText(QApplication::translate("CQFunctionsWidget", "Delete All", 0, QApplication::UnicodeUTF8));
        label->setText(QApplication::translate("CQFunctionsWidget", "Search:", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQFunctionsWidget: public Ui_CQFunctionsWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQFUNCTIONSWIDGET_H
