/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQTaskMethodWidget.ui'
**
** Created: Sun Sep 11 10:59:20 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQTASKMETHODWIDGET_H
#define UI_CQTASKMETHODWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QComboBox>
#include <QtGui/QGridLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QSpacerItem>
#include <QtGui/QTableWidget>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_CQTaskMethodWidget
{
public:
    QGridLayout *gridLayout;
    QLabel *mpLblMethod;
    QComboBox *mpBoxMethod;
    QSpacerItem *horizontalSpacer;
    QLabel *mpLblParameter;
    QTableWidget *mpTableParameter;
    QSpacerItem *horizontalSpacer_2;

    void setupUi(QWidget *CQTaskMethodWidget)
    {
        if (CQTaskMethodWidget->objectName().isEmpty())
            CQTaskMethodWidget->setObjectName(QString::fromUtf8("CQTaskMethodWidget"));
        CQTaskMethodWidget->resize(266, 137);
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(1);
        sizePolicy.setHeightForWidth(CQTaskMethodWidget->sizePolicy().hasHeightForWidth());
        CQTaskMethodWidget->setSizePolicy(sizePolicy);
        gridLayout = new QGridLayout(CQTaskMethodWidget);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        mpLblMethod = new QLabel(CQTaskMethodWidget);
        mpLblMethod->setObjectName(QString::fromUtf8("mpLblMethod"));
        mpLblMethod->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

        gridLayout->addWidget(mpLblMethod, 0, 0, 1, 1);

        mpBoxMethod = new QComboBox(CQTaskMethodWidget);
        mpBoxMethod->setObjectName(QString::fromUtf8("mpBoxMethod"));

        gridLayout->addWidget(mpBoxMethod, 0, 1, 1, 1);

        horizontalSpacer = new QSpacerItem(55, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        gridLayout->addItem(horizontalSpacer, 0, 2, 1, 2);

        mpLblParameter = new QLabel(CQTaskMethodWidget);
        mpLblParameter->setObjectName(QString::fromUtf8("mpLblParameter"));
        mpLblParameter->setAlignment(Qt::AlignRight|Qt::AlignTop|Qt::AlignTrailing);

        gridLayout->addWidget(mpLblParameter, 1, 0, 1, 1);

        mpTableParameter = new QTableWidget(CQTaskMethodWidget);
        if (mpTableParameter->columnCount() < 1)
            mpTableParameter->setColumnCount(1);
        mpTableParameter->setObjectName(QString::fromUtf8("mpTableParameter"));
        QSizePolicy sizePolicy1(QSizePolicy::Expanding, QSizePolicy::Expanding);
        sizePolicy1.setHorizontalStretch(2);
        sizePolicy1.setVerticalStretch(2);
        sizePolicy1.setHeightForWidth(mpTableParameter->sizePolicy().hasHeightForWidth());
        mpTableParameter->setSizePolicy(sizePolicy1);
        mpTableParameter->setEditTriggers(QAbstractItemView::AllEditTriggers);
        mpTableParameter->setColumnCount(1);

        gridLayout->addWidget(mpTableParameter, 1, 1, 1, 2);

        horizontalSpacer_2 = new QSpacerItem(45, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        gridLayout->addItem(horizontalSpacer_2, 1, 3, 1, 1);


        retranslateUi(CQTaskMethodWidget);

        QMetaObject::connectSlotsByName(CQTaskMethodWidget);
    } // setupUi

    void retranslateUi(QWidget *CQTaskMethodWidget)
    {
        CQTaskMethodWidget->setWindowTitle(QApplication::translate("CQTaskMethodWidget", "Form", 0, QApplication::UnicodeUTF8));
        mpLblMethod->setText(QApplication::translate("CQTaskMethodWidget", "Method", 0, QApplication::UnicodeUTF8));
        mpLblParameter->setText(QApplication::translate("CQTaskMethodWidget", "Parameter", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQTaskMethodWidget: public Ui_CQTaskMethodWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQTASKMETHODWIDGET_H
