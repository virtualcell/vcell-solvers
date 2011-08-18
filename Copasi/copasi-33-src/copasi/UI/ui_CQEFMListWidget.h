/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQEFMListWidget.ui'
**
** Created: Thu Aug 18 12:47:32 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQEFMLISTWIDGET_H
#define UI_CQEFMLISTWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QGridLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QTableView>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_CQEFMListWidget
{
public:
    QGridLayout *gridLayout;
    QLabel *mpLblFilter;
    QLineEdit *mpEditFilter;
    QTableView *mpEFMTable;

    void setupUi(QWidget *CQEFMListWidget)
    {
        if (CQEFMListWidget->objectName().isEmpty())
            CQEFMListWidget->setObjectName(QString::fromUtf8("CQEFMListWidget"));
        CQEFMListWidget->resize(354, 199);
        gridLayout = new QGridLayout(CQEFMListWidget);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        gridLayout->setContentsMargins(0, 12, 0, 0);
        mpLblFilter = new QLabel(CQEFMListWidget);
        mpLblFilter->setObjectName(QString::fromUtf8("mpLblFilter"));

        gridLayout->addWidget(mpLblFilter, 0, 0, 1, 1);

        mpEditFilter = new QLineEdit(CQEFMListWidget);
        mpEditFilter->setObjectName(QString::fromUtf8("mpEditFilter"));

        gridLayout->addWidget(mpEditFilter, 0, 1, 1, 1);

        mpEFMTable = new QTableView(CQEFMListWidget);
        mpEFMTable->setObjectName(QString::fromUtf8("mpEFMTable"));
        mpEFMTable->setAlternatingRowColors(true);
        mpEFMTable->setSortingEnabled(true);

        gridLayout->addWidget(mpEFMTable, 1, 0, 1, 2);


        retranslateUi(CQEFMListWidget);

        QMetaObject::connectSlotsByName(CQEFMListWidget);
    } // setupUi

    void retranslateUi(QWidget *CQEFMListWidget)
    {
        CQEFMListWidget->setWindowTitle(QApplication::translate("CQEFMListWidget", "CQEFMListWidget", 0, QApplication::UnicodeUTF8));
        mpLblFilter->setText(QApplication::translate("CQEFMListWidget", "Filter Expression", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQEFMListWidget: public Ui_CQEFMListWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQEFMLISTWIDGET_H
