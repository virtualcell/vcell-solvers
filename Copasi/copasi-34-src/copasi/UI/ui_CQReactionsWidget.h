/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQReactionsWidget.ui'
**
** Created: Sun Sep 11 10:59:23 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQREACTIONSWIDGET_H
#define UI_CQREACTIONSWIDGET_H

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

class Ui_CQReactionsWidget
{
public:
    QGridLayout *gridLayout;
    QHBoxLayout *hboxLayout;
    QSpacerItem *horizontalSpacerLeft;
    QPushButton *mpBtnNew;
    QPushButton *mpBtnDelete;
    QPushButton *mpBtnClear;
    QSpacerItem *horizontalSpacerRight;
    CQTableView *mpTblReactions;
    QHBoxLayout *horizontalLayout;
    QLabel *label;
    QLineEdit *mpLEFilter;

    void setupUi(CopasiWidget *CQReactionsWidget)
    {
        if (CQReactionsWidget->objectName().isEmpty())
            CQReactionsWidget->setObjectName(QString::fromUtf8("CQReactionsWidget"));
        CQReactionsWidget->resize(541, 456);
        gridLayout = new QGridLayout(CQReactionsWidget);
        gridLayout->setSpacing(6);
        gridLayout->setContentsMargins(11, 11, 11, 11);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        horizontalSpacerLeft = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout->addItem(horizontalSpacerLeft);

        mpBtnNew = new QPushButton(CQReactionsWidget);
        mpBtnNew->setObjectName(QString::fromUtf8("mpBtnNew"));

        hboxLayout->addWidget(mpBtnNew);

        mpBtnDelete = new QPushButton(CQReactionsWidget);
        mpBtnDelete->setObjectName(QString::fromUtf8("mpBtnDelete"));
        mpBtnDelete->setFocusPolicy(Qt::NoFocus);

        hboxLayout->addWidget(mpBtnDelete);

        mpBtnClear = new QPushButton(CQReactionsWidget);
        mpBtnClear->setObjectName(QString::fromUtf8("mpBtnClear"));
        mpBtnClear->setFocusPolicy(Qt::NoFocus);

        hboxLayout->addWidget(mpBtnClear);

        horizontalSpacerRight = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout->addItem(horizontalSpacerRight);


        gridLayout->addLayout(hboxLayout, 8, 0, 1, 2);

        mpTblReactions = new CQTableView(CQReactionsWidget);
        mpTblReactions->setObjectName(QString::fromUtf8("mpTblReactions"));
        mpTblReactions->setEditTriggers(QAbstractItemView::CurrentChanged|QAbstractItemView::EditKeyPressed|QAbstractItemView::SelectedClicked);
        mpTblReactions->setAlternatingRowColors(true);
        mpTblReactions->setSelectionMode(QAbstractItemView::ExtendedSelection);
        mpTblReactions->setSelectionBehavior(QAbstractItemView::SelectRows);
        mpTblReactions->setHorizontalScrollMode(QAbstractItemView::ScrollPerPixel);
        mpTblReactions->setSortingEnabled(true);

        gridLayout->addWidget(mpTblReactions, 6, 0, 1, 2);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        label = new QLabel(CQReactionsWidget);
        label->setObjectName(QString::fromUtf8("label"));

        horizontalLayout->addWidget(label);

        mpLEFilter = new QLineEdit(CQReactionsWidget);
        mpLEFilter->setObjectName(QString::fromUtf8("mpLEFilter"));

        horizontalLayout->addWidget(mpLEFilter);


        gridLayout->addLayout(horizontalLayout, 3, 0, 3, 2);

        QWidget::setTabOrder(mpTblReactions, mpBtnDelete);
        QWidget::setTabOrder(mpBtnDelete, mpBtnClear);

        retranslateUi(CQReactionsWidget);
        QObject::connect(mpBtnClear, SIGNAL(pressed()), CQReactionsWidget, SLOT(slotBtnClearClicked()));
        QObject::connect(mpBtnDelete, SIGNAL(pressed()), CQReactionsWidget, SLOT(slotBtnDeleteClicked()));
        QObject::connect(mpTblReactions, SIGNAL(doubleClicked(QModelIndex)), CQReactionsWidget, SLOT(slotDoubleClicked(QModelIndex)));
        QObject::connect(mpBtnNew, SIGNAL(pressed()), CQReactionsWidget, SLOT(slotBtnNewClicked()));

        QMetaObject::connectSlotsByName(CQReactionsWidget);
    } // setupUi

    void retranslateUi(CopasiWidget *CQReactionsWidget)
    {
        CQReactionsWidget->setProperty("caption", QVariant(QApplication::translate("CQReactionsWidget", "Reactions", 0, QApplication::UnicodeUTF8)));
        mpBtnNew->setText(QApplication::translate("CQReactionsWidget", "New", 0, QApplication::UnicodeUTF8));
        mpBtnDelete->setText(QApplication::translate("CQReactionsWidget", "Delete", 0, QApplication::UnicodeUTF8));
        mpBtnClear->setText(QApplication::translate("CQReactionsWidget", "Delete All", 0, QApplication::UnicodeUTF8));
        label->setText(QApplication::translate("CQReactionsWidget", "Search:", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQReactionsWidget: public Ui_CQReactionsWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQREACTIONSWIDGET_H
