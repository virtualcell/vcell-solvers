/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQCompartmentsWidget.ui'
**
** Created: Sun Sep 11 10:59:26 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQCOMPARTMENTSWIDGET_H
#define UI_CQCOMPARTMENTSWIDGET_H

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

class Ui_CQCompartmentsWidget
{
public:
    QGridLayout *gridLayout;
    QHBoxLayout *hboxLayout;
    QSpacerItem *horizontalSpacerLeft;
    QPushButton *mpBtnNew;
    QPushButton *mpBtnDelete;
    QPushButton *mpBtnClear;
    QSpacerItem *horizontalSpacerRight;
    QHBoxLayout *horizontalLayout;
    QLabel *label;
    QLineEdit *mpLEFilter;
    CQTableView *mpTblCompartments;

    void setupUi(CopasiWidget *CQCompartmentsWidget)
    {
        if (CQCompartmentsWidget->objectName().isEmpty())
            CQCompartmentsWidget->setObjectName(QString::fromUtf8("CQCompartmentsWidget"));
        CQCompartmentsWidget->resize(541, 456);
        gridLayout = new QGridLayout(CQCompartmentsWidget);
        gridLayout->setSpacing(6);
        gridLayout->setContentsMargins(11, 11, 11, 11);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        horizontalSpacerLeft = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout->addItem(horizontalSpacerLeft);

        mpBtnNew = new QPushButton(CQCompartmentsWidget);
        mpBtnNew->setObjectName(QString::fromUtf8("mpBtnNew"));

        hboxLayout->addWidget(mpBtnNew);

        mpBtnDelete = new QPushButton(CQCompartmentsWidget);
        mpBtnDelete->setObjectName(QString::fromUtf8("mpBtnDelete"));
        mpBtnDelete->setFocusPolicy(Qt::NoFocus);

        hboxLayout->addWidget(mpBtnDelete);

        mpBtnClear = new QPushButton(CQCompartmentsWidget);
        mpBtnClear->setObjectName(QString::fromUtf8("mpBtnClear"));
        mpBtnClear->setFocusPolicy(Qt::NoFocus);

        hboxLayout->addWidget(mpBtnClear);

        horizontalSpacerRight = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout->addItem(horizontalSpacerRight);


        gridLayout->addLayout(hboxLayout, 8, 0, 1, 2);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        label = new QLabel(CQCompartmentsWidget);
        label->setObjectName(QString::fromUtf8("label"));

        horizontalLayout->addWidget(label);

        mpLEFilter = new QLineEdit(CQCompartmentsWidget);
        mpLEFilter->setObjectName(QString::fromUtf8("mpLEFilter"));

        horizontalLayout->addWidget(mpLEFilter);


        gridLayout->addLayout(horizontalLayout, 3, 0, 3, 2);

        mpTblCompartments = new CQTableView(CQCompartmentsWidget);
        mpTblCompartments->setObjectName(QString::fromUtf8("mpTblCompartments"));
        mpTblCompartments->setEditTriggers(QAbstractItemView::CurrentChanged|QAbstractItemView::EditKeyPressed|QAbstractItemView::SelectedClicked);
        mpTblCompartments->setAlternatingRowColors(true);
        mpTblCompartments->setSelectionMode(QAbstractItemView::ExtendedSelection);
        mpTblCompartments->setSelectionBehavior(QAbstractItemView::SelectRows);
        mpTblCompartments->setHorizontalScrollMode(QAbstractItemView::ScrollPerPixel);
        mpTblCompartments->setSortingEnabled(true);

        gridLayout->addWidget(mpTblCompartments, 6, 0, 1, 2);

        QWidget::setTabOrder(mpTblCompartments, mpBtnDelete);
        QWidget::setTabOrder(mpBtnDelete, mpBtnClear);

        retranslateUi(CQCompartmentsWidget);
        QObject::connect(mpBtnClear, SIGNAL(pressed()), CQCompartmentsWidget, SLOT(slotBtnClearClicked()));
        QObject::connect(mpBtnDelete, SIGNAL(pressed()), CQCompartmentsWidget, SLOT(slotBtnDeleteClicked()));
        QObject::connect(mpBtnNew, SIGNAL(pressed()), CQCompartmentsWidget, SLOT(slotBtnNewClicked()));
        QObject::connect(mpTblCompartments, SIGNAL(doubleClicked(QModelIndex)), CQCompartmentsWidget, SLOT(slotDoubleClicked(QModelIndex)));

        QMetaObject::connectSlotsByName(CQCompartmentsWidget);
    } // setupUi

    void retranslateUi(CopasiWidget *CQCompartmentsWidget)
    {
        CQCompartmentsWidget->setProperty("caption", QVariant(QApplication::translate("CQCompartmentsWidget", "Compartments", 0, QApplication::UnicodeUTF8)));
        mpBtnNew->setText(QApplication::translate("CQCompartmentsWidget", "New", 0, QApplication::UnicodeUTF8));
        mpBtnDelete->setText(QApplication::translate("CQCompartmentsWidget", "Delete", 0, QApplication::UnicodeUTF8));
        mpBtnClear->setText(QApplication::translate("CQCompartmentsWidget", "Delete All", 0, QApplication::UnicodeUTF8));
        label->setText(QApplication::translate("CQCompartmentsWidget", "Search:", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQCompartmentsWidget: public Ui_CQCompartmentsWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQCOMPARTMENTSWIDGET_H
