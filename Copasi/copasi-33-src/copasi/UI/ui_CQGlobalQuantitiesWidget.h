/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQGlobalQuantitiesWidget.ui'
**
** Created: Thu Aug 18 12:47:31 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQGLOBALQUANTITIESWIDGET_H
#define UI_CQGLOBALQUANTITIESWIDGET_H

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

class Ui_CQGlobalQuantitiesWidget
{
public:
    QGridLayout *gridLayout;
    QHBoxLayout *hboxLayout;
    QSpacerItem *horizontalSpacerLeft;
    QPushButton *mpBtnNew;
    QPushButton *mpBtnDelete;
    QPushButton *mpBtnClear;
    QSpacerItem *horizontalSpacerRight;
    QTableView *mpTblGlobalQuantities;
    QHBoxLayout *horizontalLayout;
    QLabel *label;
    QLineEdit *mpLEFilter;

    void setupUi(CopasiWidget *CQGlobalQuantitiesWidget)
    {
        if (CQGlobalQuantitiesWidget->objectName().isEmpty())
            CQGlobalQuantitiesWidget->setObjectName(QString::fromUtf8("CQGlobalQuantitiesWidget"));
        CQGlobalQuantitiesWidget->resize(541, 456);
        gridLayout = new QGridLayout(CQGlobalQuantitiesWidget);
        gridLayout->setSpacing(6);
        gridLayout->setContentsMargins(11, 11, 11, 11);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        horizontalSpacerLeft = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout->addItem(horizontalSpacerLeft);

        mpBtnNew = new QPushButton(CQGlobalQuantitiesWidget);
        mpBtnNew->setObjectName(QString::fromUtf8("mpBtnNew"));

        hboxLayout->addWidget(mpBtnNew);

        mpBtnDelete = new QPushButton(CQGlobalQuantitiesWidget);
        mpBtnDelete->setObjectName(QString::fromUtf8("mpBtnDelete"));
        mpBtnDelete->setFocusPolicy(Qt::NoFocus);

        hboxLayout->addWidget(mpBtnDelete);

        mpBtnClear = new QPushButton(CQGlobalQuantitiesWidget);
        mpBtnClear->setObjectName(QString::fromUtf8("mpBtnClear"));
        mpBtnClear->setFocusPolicy(Qt::NoFocus);

        hboxLayout->addWidget(mpBtnClear);

        horizontalSpacerRight = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout->addItem(horizontalSpacerRight);


        gridLayout->addLayout(hboxLayout, 8, 0, 1, 2);

        mpTblGlobalQuantities = new QTableView(CQGlobalQuantitiesWidget);
        mpTblGlobalQuantities->setObjectName(QString::fromUtf8("mpTblGlobalQuantities"));
        mpTblGlobalQuantities->setEditTriggers(QAbstractItemView::DoubleClicked|QAbstractItemView::EditKeyPressed|QAbstractItemView::SelectedClicked);
        mpTblGlobalQuantities->setAlternatingRowColors(true);
        mpTblGlobalQuantities->setSelectionMode(QAbstractItemView::ExtendedSelection);
        mpTblGlobalQuantities->setSelectionBehavior(QAbstractItemView::SelectRows);
        mpTblGlobalQuantities->setSortingEnabled(true);

        gridLayout->addWidget(mpTblGlobalQuantities, 6, 0, 1, 2);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        label = new QLabel(CQGlobalQuantitiesWidget);
        label->setObjectName(QString::fromUtf8("label"));

        horizontalLayout->addWidget(label);

        mpLEFilter = new QLineEdit(CQGlobalQuantitiesWidget);
        mpLEFilter->setObjectName(QString::fromUtf8("mpLEFilter"));

        horizontalLayout->addWidget(mpLEFilter);


        gridLayout->addLayout(horizontalLayout, 3, 0, 3, 2);

        QWidget::setTabOrder(mpTblGlobalQuantities, mpBtnDelete);
        QWidget::setTabOrder(mpBtnDelete, mpBtnClear);

        retranslateUi(CQGlobalQuantitiesWidget);
        QObject::connect(mpBtnClear, SIGNAL(pressed()), CQGlobalQuantitiesWidget, SLOT(slotBtnClearClicked()));
        QObject::connect(mpBtnDelete, SIGNAL(pressed()), CQGlobalQuantitiesWidget, SLOT(slotBtnDeleteClicked()));
        QObject::connect(mpTblGlobalQuantities, SIGNAL(doubleClicked(QModelIndex)), CQGlobalQuantitiesWidget, SLOT(slotDoubleClicked(QModelIndex)));
        QObject::connect(mpBtnNew, SIGNAL(pressed()), CQGlobalQuantitiesWidget, SLOT(slotBtnNewClicked()));

        QMetaObject::connectSlotsByName(CQGlobalQuantitiesWidget);
    } // setupUi

    void retranslateUi(CopasiWidget *CQGlobalQuantitiesWidget)
    {
        CQGlobalQuantitiesWidget->setProperty("caption", QVariant(QApplication::translate("CQGlobalQuantitiesWidget", "GlobalQuantities", 0, QApplication::UnicodeUTF8)));
        mpBtnNew->setText(QApplication::translate("CQGlobalQuantitiesWidget", "New", 0, QApplication::UnicodeUTF8));
        mpBtnDelete->setText(QApplication::translate("CQGlobalQuantitiesWidget", "Delete", 0, QApplication::UnicodeUTF8));
        mpBtnClear->setText(QApplication::translate("CQGlobalQuantitiesWidget", "Delete All", 0, QApplication::UnicodeUTF8));
        label->setText(QApplication::translate("CQGlobalQuantitiesWidget", "Search:", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQGlobalQuantitiesWidget: public Ui_CQGlobalQuantitiesWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQGLOBALQUANTITIESWIDGET_H
