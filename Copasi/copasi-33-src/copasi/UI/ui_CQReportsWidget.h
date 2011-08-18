/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQReportsWidget.ui'
**
** Created: Thu Aug 18 12:47:30 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQREPORTSWIDGET_H
#define UI_CQREPORTSWIDGET_H

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

class Ui_CQReportsWidget
{
public:
    QGridLayout *gridLayout;
    QHBoxLayout *hboxLayout;
    QSpacerItem *horizontalSpacerLeft;
    QPushButton *mpBtnNew;
    QPushButton *mpBtnDelete;
    QPushButton *mpBtnClear;
    QSpacerItem *horizontalSpacerRight;
    QTableView *mpTblReports;
    QHBoxLayout *horizontalLayout;
    QLabel *label;
    QLineEdit *mpLEFilter;

    void setupUi(CopasiWidget *CQReportsWidget)
    {
        if (CQReportsWidget->objectName().isEmpty())
            CQReportsWidget->setObjectName(QString::fromUtf8("CQReportsWidget"));
        CQReportsWidget->resize(541, 456);
        gridLayout = new QGridLayout(CQReportsWidget);
        gridLayout->setSpacing(6);
        gridLayout->setContentsMargins(11, 11, 11, 11);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        horizontalSpacerLeft = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout->addItem(horizontalSpacerLeft);

        mpBtnNew = new QPushButton(CQReportsWidget);
        mpBtnNew->setObjectName(QString::fromUtf8("mpBtnNew"));

        hboxLayout->addWidget(mpBtnNew);

        mpBtnDelete = new QPushButton(CQReportsWidget);
        mpBtnDelete->setObjectName(QString::fromUtf8("mpBtnDelete"));
        mpBtnDelete->setFocusPolicy(Qt::NoFocus);

        hboxLayout->addWidget(mpBtnDelete);

        mpBtnClear = new QPushButton(CQReportsWidget);
        mpBtnClear->setObjectName(QString::fromUtf8("mpBtnClear"));
        mpBtnClear->setFocusPolicy(Qt::NoFocus);

        hboxLayout->addWidget(mpBtnClear);

        horizontalSpacerRight = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout->addItem(horizontalSpacerRight);


        gridLayout->addLayout(hboxLayout, 8, 0, 1, 2);

        mpTblReports = new QTableView(CQReportsWidget);
        mpTblReports->setObjectName(QString::fromUtf8("mpTblReports"));
        mpTblReports->setEditTriggers(QAbstractItemView::DoubleClicked|QAbstractItemView::EditKeyPressed|QAbstractItemView::SelectedClicked);
        mpTblReports->setAlternatingRowColors(true);
        mpTblReports->setSelectionMode(QAbstractItemView::ExtendedSelection);
        mpTblReports->setSelectionBehavior(QAbstractItemView::SelectRows);
        mpTblReports->setSortingEnabled(true);

        gridLayout->addWidget(mpTblReports, 6, 0, 1, 2);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        label = new QLabel(CQReportsWidget);
        label->setObjectName(QString::fromUtf8("label"));

        horizontalLayout->addWidget(label);

        mpLEFilter = new QLineEdit(CQReportsWidget);
        mpLEFilter->setObjectName(QString::fromUtf8("mpLEFilter"));

        horizontalLayout->addWidget(mpLEFilter);


        gridLayout->addLayout(horizontalLayout, 3, 0, 3, 2);

        QWidget::setTabOrder(mpTblReports, mpBtnDelete);
        QWidget::setTabOrder(mpBtnDelete, mpBtnClear);

        retranslateUi(CQReportsWidget);
        QObject::connect(mpBtnClear, SIGNAL(pressed()), CQReportsWidget, SLOT(slotBtnClearClicked()));
        QObject::connect(mpBtnDelete, SIGNAL(pressed()), CQReportsWidget, SLOT(slotBtnDeleteClicked()));
        QObject::connect(mpTblReports, SIGNAL(doubleClicked(QModelIndex)), CQReportsWidget, SLOT(slotDoubleClicked(QModelIndex)));
        QObject::connect(mpBtnNew, SIGNAL(pressed()), CQReportsWidget, SLOT(slotBtnNewClicked()));

        QMetaObject::connectSlotsByName(CQReportsWidget);
    } // setupUi

    void retranslateUi(CopasiWidget *CQReportsWidget)
    {
        CQReportsWidget->setProperty("caption", QVariant(QApplication::translate("CQReportsWidget", "Reports", 0, QApplication::UnicodeUTF8)));
        mpBtnNew->setText(QApplication::translate("CQReportsWidget", "New", 0, QApplication::UnicodeUTF8));
        mpBtnDelete->setText(QApplication::translate("CQReportsWidget", "Delete", 0, QApplication::UnicodeUTF8));
        mpBtnClear->setText(QApplication::translate("CQReportsWidget", "Delete All", 0, QApplication::UnicodeUTF8));
        label->setText(QApplication::translate("CQReportsWidget", "Search:", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQReportsWidget: public Ui_CQReportsWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQREPORTSWIDGET_H
