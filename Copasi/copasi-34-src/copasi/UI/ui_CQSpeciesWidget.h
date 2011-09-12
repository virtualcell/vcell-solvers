/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQSpeciesWidget.ui'
**
** Created: Sun Sep 11 10:59:22 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQSPECIESWIDGET_H
#define UI_CQSPECIESWIDGET_H

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

class Ui_CQSpeciesWidget
{
public:
    QGridLayout *gridLayout;
    QHBoxLayout *hboxLayout;
    QSpacerItem *horizontalSpacerLeft;
    QPushButton *mpBtnNew;
    QPushButton *mpBtnDelete;
    QPushButton *mpBtnClear;
    QSpacerItem *horizontalSpacerRight;
    CQTableView *mpTblSpecies;
    QHBoxLayout *horizontalLayout;
    QLabel *label;
    QLineEdit *mpLEFilter;

    void setupUi(CopasiWidget *CQSpeciesWidget)
    {
        if (CQSpeciesWidget->objectName().isEmpty())
            CQSpeciesWidget->setObjectName(QString::fromUtf8("CQSpeciesWidget"));
        CQSpeciesWidget->resize(541, 456);
        gridLayout = new QGridLayout(CQSpeciesWidget);
        gridLayout->setSpacing(6);
        gridLayout->setContentsMargins(11, 11, 11, 11);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        horizontalSpacerLeft = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout->addItem(horizontalSpacerLeft);

        mpBtnNew = new QPushButton(CQSpeciesWidget);
        mpBtnNew->setObjectName(QString::fromUtf8("mpBtnNew"));

        hboxLayout->addWidget(mpBtnNew);

        mpBtnDelete = new QPushButton(CQSpeciesWidget);
        mpBtnDelete->setObjectName(QString::fromUtf8("mpBtnDelete"));
        mpBtnDelete->setFocusPolicy(Qt::NoFocus);

        hboxLayout->addWidget(mpBtnDelete);

        mpBtnClear = new QPushButton(CQSpeciesWidget);
        mpBtnClear->setObjectName(QString::fromUtf8("mpBtnClear"));
        mpBtnClear->setFocusPolicy(Qt::NoFocus);

        hboxLayout->addWidget(mpBtnClear);

        horizontalSpacerRight = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout->addItem(horizontalSpacerRight);


        gridLayout->addLayout(hboxLayout, 8, 0, 1, 2);

        mpTblSpecies = new CQTableView(CQSpeciesWidget);
        mpTblSpecies->setObjectName(QString::fromUtf8("mpTblSpecies"));
        mpTblSpecies->setEditTriggers(QAbstractItemView::CurrentChanged|QAbstractItemView::EditKeyPressed|QAbstractItemView::SelectedClicked);
        mpTblSpecies->setAlternatingRowColors(true);
        mpTblSpecies->setSelectionMode(QAbstractItemView::ExtendedSelection);
        mpTblSpecies->setSelectionBehavior(QAbstractItemView::SelectRows);
        mpTblSpecies->setHorizontalScrollMode(QAbstractItemView::ScrollPerPixel);
        mpTblSpecies->setSortingEnabled(true);

        gridLayout->addWidget(mpTblSpecies, 6, 0, 1, 2);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        label = new QLabel(CQSpeciesWidget);
        label->setObjectName(QString::fromUtf8("label"));

        horizontalLayout->addWidget(label);

        mpLEFilter = new QLineEdit(CQSpeciesWidget);
        mpLEFilter->setObjectName(QString::fromUtf8("mpLEFilter"));

        horizontalLayout->addWidget(mpLEFilter);


        gridLayout->addLayout(horizontalLayout, 3, 0, 3, 2);

        QWidget::setTabOrder(mpTblSpecies, mpBtnDelete);
        QWidget::setTabOrder(mpBtnDelete, mpBtnClear);

        retranslateUi(CQSpeciesWidget);
        QObject::connect(mpBtnClear, SIGNAL(pressed()), CQSpeciesWidget, SLOT(slotBtnClearClicked()));
        QObject::connect(mpBtnDelete, SIGNAL(pressed()), CQSpeciesWidget, SLOT(slotBtnDeleteClicked()));
        QObject::connect(mpTblSpecies, SIGNAL(doubleClicked(QModelIndex)), CQSpeciesWidget, SLOT(slotDoubleClicked(QModelIndex)));
        QObject::connect(mpBtnNew, SIGNAL(pressed()), CQSpeciesWidget, SLOT(slotBtnNewClicked()));

        QMetaObject::connectSlotsByName(CQSpeciesWidget);
    } // setupUi

    void retranslateUi(CopasiWidget *CQSpeciesWidget)
    {
        CQSpeciesWidget->setProperty("caption", QVariant(QApplication::translate("CQSpeciesWidget", "Species", 0, QApplication::UnicodeUTF8)));
        mpBtnNew->setText(QApplication::translate("CQSpeciesWidget", "New", 0, QApplication::UnicodeUTF8));
        mpBtnDelete->setText(QApplication::translate("CQSpeciesWidget", "Delete", 0, QApplication::UnicodeUTF8));
        mpBtnClear->setText(QApplication::translate("CQSpeciesWidget", "Delete All", 0, QApplication::UnicodeUTF8));
        label->setText(QApplication::translate("CQSpeciesWidget", "Search:", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQSpeciesWidget: public Ui_CQSpeciesWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQSPECIESWIDGET_H
