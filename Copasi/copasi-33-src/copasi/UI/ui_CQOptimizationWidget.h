/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQOptimizationWidget.ui'
**
** Created: Thu Aug 18 12:47:31 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQOPTIMIZATIONWIDGET_H
#define UI_CQOPTIMIZATIONWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QComboBox>
#include <QtGui/QGridLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QRadioButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QTabWidget>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>
#include <map>
#include <string>
#include "CQExpressionMmlStackedWidget.h"
#include "TaskWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQOptimizationWidget
{
public:
    QVBoxLayout *verticalLayout;
    QGridLayout *gridLayout;
    QLabel *mpLblExpression;
    CQExpressionMmlStackedWidget *mpExpressionEMW;
    QWidget *page;
    QWidget *page_2;
    QRadioButton *mpBtnMinimize;
    QRadioButton *mpBtnMaximize;
    QLabel *mpLblType;
    QComboBox *mpBoxSubtask;
    QSpacerItem *mpSpacer;
    QSpacerItem *verticalSpacer;
    QTabWidget *mpTabWidget;
    QWidget *mpParametersPage;
    QWidget *mpConstraintsPage;
    QSpacerItem *verticalSpacer_2;
    QSpacerItem *verticalSpacer_3;

    void setupUi(TaskWidget *CQOptimizationWidget)
    {
        if (CQOptimizationWidget->objectName().isEmpty())
            CQOptimizationWidget->setObjectName(QString::fromUtf8("CQOptimizationWidget"));
        CQOptimizationWidget->resize(546, 254);
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(CQOptimizationWidget->sizePolicy().hasHeightForWidth());
        CQOptimizationWidget->setSizePolicy(sizePolicy);
        CQOptimizationWidget->setMinimumSize(QSize(0, 0));
        verticalLayout = new QVBoxLayout(CQOptimizationWidget);
        verticalLayout->setSpacing(6);
        verticalLayout->setContentsMargins(11, 11, 11, 11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        gridLayout = new QGridLayout();
        gridLayout->setSpacing(6);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        mpLblExpression = new QLabel(CQOptimizationWidget);
        mpLblExpression->setObjectName(QString::fromUtf8("mpLblExpression"));
        sizePolicy.setHeightForWidth(mpLblExpression->sizePolicy().hasHeightForWidth());
        mpLblExpression->setSizePolicy(sizePolicy);
        mpLblExpression->setAlignment(Qt::AlignLeading|Qt::AlignLeft|Qt::AlignTop);
        mpLblExpression->setWordWrap(false);

        gridLayout->addWidget(mpLblExpression, 0, 0, 1, 1);

        mpExpressionEMW = new CQExpressionMmlStackedWidget(CQOptimizationWidget);
        mpExpressionEMW->setObjectName(QString::fromUtf8("mpExpressionEMW"));
        mpExpressionEMW->setMinimumSize(QSize(420, 70));
        page = new QWidget();
        page->setObjectName(QString::fromUtf8("page"));
        mpExpressionEMW->addWidget(page);
        page_2 = new QWidget();
        page_2->setObjectName(QString::fromUtf8("page_2"));
        mpExpressionEMW->addWidget(page_2);

        gridLayout->addWidget(mpExpressionEMW, 0, 1, 3, 2);

        mpBtnMinimize = new QRadioButton(CQOptimizationWidget);
        mpBtnMinimize->setObjectName(QString::fromUtf8("mpBtnMinimize"));
        QSizePolicy sizePolicy1(QSizePolicy::Minimum, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(mpBtnMinimize->sizePolicy().hasHeightForWidth());
        mpBtnMinimize->setSizePolicy(sizePolicy1);
        mpBtnMinimize->setChecked(true);

        gridLayout->addWidget(mpBtnMinimize, 1, 0, 1, 1);

        mpBtnMaximize = new QRadioButton(CQOptimizationWidget);
        mpBtnMaximize->setObjectName(QString::fromUtf8("mpBtnMaximize"));
        sizePolicy1.setHeightForWidth(mpBtnMaximize->sizePolicy().hasHeightForWidth());
        mpBtnMaximize->setSizePolicy(sizePolicy1);

        gridLayout->addWidget(mpBtnMaximize, 2, 0, 1, 1);

        mpLblType = new QLabel(CQOptimizationWidget);
        mpLblType->setObjectName(QString::fromUtf8("mpLblType"));
        sizePolicy.setHeightForWidth(mpLblType->sizePolicy().hasHeightForWidth());
        mpLblType->setSizePolicy(sizePolicy);
        mpLblType->setAlignment(Qt::AlignLeading|Qt::AlignLeft|Qt::AlignVCenter);
        mpLblType->setWordWrap(false);

        gridLayout->addWidget(mpLblType, 3, 0, 1, 1);

        mpBoxSubtask = new QComboBox(CQOptimizationWidget);
        mpBoxSubtask->setObjectName(QString::fromUtf8("mpBoxSubtask"));
        QSizePolicy sizePolicy2(QSizePolicy::MinimumExpanding, QSizePolicy::Fixed);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(mpBoxSubtask->sizePolicy().hasHeightForWidth());
        mpBoxSubtask->setSizePolicy(sizePolicy2);

        gridLayout->addWidget(mpBoxSubtask, 3, 1, 1, 1);

        mpSpacer = new QSpacerItem(214, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        gridLayout->addItem(mpSpacer, 3, 2, 1, 1);


        verticalLayout->addLayout(gridLayout);

        verticalSpacer = new QSpacerItem(20, 2, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout->addItem(verticalSpacer);

        mpTabWidget = new QTabWidget(CQOptimizationWidget);
        mpTabWidget->setObjectName(QString::fromUtf8("mpTabWidget"));
        QSizePolicy sizePolicy3(QSizePolicy::Expanding, QSizePolicy::Expanding);
        sizePolicy3.setHorizontalStretch(0);
        sizePolicy3.setVerticalStretch(0);
        sizePolicy3.setHeightForWidth(mpTabWidget->sizePolicy().hasHeightForWidth());
        mpTabWidget->setSizePolicy(sizePolicy3);
        mpParametersPage = new QWidget();
        mpParametersPage->setObjectName(QString::fromUtf8("mpParametersPage"));
        mpTabWidget->addTab(mpParametersPage, QString());
        mpConstraintsPage = new QWidget();
        mpConstraintsPage->setObjectName(QString::fromUtf8("mpConstraintsPage"));
        mpTabWidget->addTab(mpConstraintsPage, QString());

        verticalLayout->addWidget(mpTabWidget);

        verticalSpacer_2 = new QSpacerItem(20, 13, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout->addItem(verticalSpacer_2);

        verticalSpacer_3 = new QSpacerItem(20, 14, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout->addItem(verticalSpacer_3);

        QWidget::setTabOrder(mpBoxSubtask, mpTabWidget);

        retranslateUi(CQOptimizationWidget);
        QObject::connect(mpTabWidget, SIGNAL(currentChanged(QWidget*)), CQOptimizationWidget, SLOT(slotPageChange(QWidget*)));
        QObject::connect(mpBoxSubtask, SIGNAL(activated(QString)), CQOptimizationWidget, SLOT(slotSubtaskChanged(QString)));

        QMetaObject::connectSlotsByName(CQOptimizationWidget);
    } // setupUi

    void retranslateUi(TaskWidget *CQOptimizationWidget)
    {
        CQOptimizationWidget->setProperty("caption", QVariant(QApplication::translate("CQOptimizationWidget", "Optimization", 0, QApplication::UnicodeUTF8)));
        mpLblExpression->setText(QApplication::translate("CQOptimizationWidget", "Expression", 0, QApplication::UnicodeUTF8));
        mpBtnMinimize->setText(QApplication::translate("CQOptimizationWidget", "minimize", 0, QApplication::UnicodeUTF8));
        mpBtnMaximize->setText(QApplication::translate("CQOptimizationWidget", "maximize", 0, QApplication::UnicodeUTF8));
        mpLblType->setText(QApplication::translate("CQOptimizationWidget", "Subtask", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpParametersPage), QApplication::translate("CQOptimizationWidget", "Parameters (0)", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpConstraintsPage), QApplication::translate("CQOptimizationWidget", "Constraints (0)", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQOptimizationWidget: public Ui_CQOptimizationWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQOPTIMIZATIONWIDGET_H
