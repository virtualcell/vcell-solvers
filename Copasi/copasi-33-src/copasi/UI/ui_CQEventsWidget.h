/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQEventsWidget.ui'
**
** Created: Thu Aug 18 12:47:32 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQEVENTSWIDGET_H
#define UI_CQEVENTSWIDGET_H

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

class Ui_CQEventsWidget
{
public:
    QGridLayout *gridLayout;
    QHBoxLayout *hboxLayout;
    QSpacerItem *horizontalSpacerLeft;
    QPushButton *mpBtnNew;
    QPushButton *mpBtnDelete;
    QPushButton *mpBtnClear;
    QSpacerItem *horizontalSpacerRight;
    QTableView *mpTblEvents;
    QHBoxLayout *horizontalLayout;
    QLabel *label;
    QLineEdit *mpLEFilter;

    void setupUi(CopasiWidget *CQEventsWidget)
    {
        if (CQEventsWidget->objectName().isEmpty())
            CQEventsWidget->setObjectName(QString::fromUtf8("CQEventsWidget"));
        CQEventsWidget->resize(541, 456);
        gridLayout = new QGridLayout(CQEventsWidget);
        gridLayout->setSpacing(6);
        gridLayout->setContentsMargins(11, 11, 11, 11);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        horizontalSpacerLeft = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout->addItem(horizontalSpacerLeft);

        mpBtnNew = new QPushButton(CQEventsWidget);
        mpBtnNew->setObjectName(QString::fromUtf8("mpBtnNew"));

        hboxLayout->addWidget(mpBtnNew);

        mpBtnDelete = new QPushButton(CQEventsWidget);
        mpBtnDelete->setObjectName(QString::fromUtf8("mpBtnDelete"));
        mpBtnDelete->setFocusPolicy(Qt::NoFocus);

        hboxLayout->addWidget(mpBtnDelete);

        mpBtnClear = new QPushButton(CQEventsWidget);
        mpBtnClear->setObjectName(QString::fromUtf8("mpBtnClear"));
        mpBtnClear->setFocusPolicy(Qt::NoFocus);

        hboxLayout->addWidget(mpBtnClear);

        horizontalSpacerRight = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout->addItem(horizontalSpacerRight);


        gridLayout->addLayout(hboxLayout, 8, 0, 1, 2);

        mpTblEvents = new QTableView(CQEventsWidget);
        mpTblEvents->setObjectName(QString::fromUtf8("mpTblEvents"));
        mpTblEvents->setEditTriggers(QAbstractItemView::DoubleClicked|QAbstractItemView::EditKeyPressed|QAbstractItemView::SelectedClicked);
        mpTblEvents->setAlternatingRowColors(true);
        mpTblEvents->setSelectionMode(QAbstractItemView::ExtendedSelection);
        mpTblEvents->setSelectionBehavior(QAbstractItemView::SelectRows);
        mpTblEvents->setSortingEnabled(true);

        gridLayout->addWidget(mpTblEvents, 6, 0, 1, 2);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        label = new QLabel(CQEventsWidget);
        label->setObjectName(QString::fromUtf8("label"));

        horizontalLayout->addWidget(label);

        mpLEFilter = new QLineEdit(CQEventsWidget);
        mpLEFilter->setObjectName(QString::fromUtf8("mpLEFilter"));

        horizontalLayout->addWidget(mpLEFilter);


        gridLayout->addLayout(horizontalLayout, 3, 0, 3, 2);

        QWidget::setTabOrder(mpTblEvents, mpBtnDelete);
        QWidget::setTabOrder(mpBtnDelete, mpBtnClear);

        retranslateUi(CQEventsWidget);
        QObject::connect(mpBtnClear, SIGNAL(pressed()), CQEventsWidget, SLOT(slotBtnClearClicked()));
        QObject::connect(mpBtnDelete, SIGNAL(pressed()), CQEventsWidget, SLOT(slotBtnDeleteClicked()));
        QObject::connect(mpTblEvents, SIGNAL(doubleClicked(QModelIndex)), CQEventsWidget, SLOT(slotDoubleClicked(QModelIndex)));
        QObject::connect(mpBtnNew, SIGNAL(pressed()), CQEventsWidget, SLOT(slotBtnNewClicked()));

        QMetaObject::connectSlotsByName(CQEventsWidget);
    } // setupUi

    void retranslateUi(CopasiWidget *CQEventsWidget)
    {
        CQEventsWidget->setProperty("caption", QVariant(QApplication::translate("CQEventsWidget", "Events", 0, QApplication::UnicodeUTF8)));
        mpBtnNew->setText(QApplication::translate("CQEventsWidget", "New", 0, QApplication::UnicodeUTF8));
        mpBtnDelete->setText(QApplication::translate("CQEventsWidget", "Delete", 0, QApplication::UnicodeUTF8));
        mpBtnClear->setText(QApplication::translate("CQEventsWidget", "Delete All", 0, QApplication::UnicodeUTF8));
        label->setText(QApplication::translate("CQEventsWidget", "Search:", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQEventsWidget: public Ui_CQEventsWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQEVENTSWIDGET_H
