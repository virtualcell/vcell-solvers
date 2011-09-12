/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQOptimizationWidget.ui'
**
** Created: Sun Sep 11 10:59:23 2011
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
#include <QtGui/QCheckBox>
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
#include "copasi/UI/CQFittingItemWidget.h"

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
    QCheckBox *mpCheckRandomize;
    QCheckBox *mpCheckStatistics;
    QTabWidget *mpTabWidget;
    CQFittingItemWidget *mpParameters;
    CQFittingItemWidget *mpConstraints;

    void setupUi(TaskWidget *CQOptimizationWidget)
    {
        if (CQOptimizationWidget->objectName().isEmpty())
            CQOptimizationWidget->setObjectName(QString::fromUtf8("CQOptimizationWidget"));
        CQOptimizationWidget->resize(551, 239);
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

        mpSpacer = new QSpacerItem(138, 17, QSizePolicy::Expanding, QSizePolicy::Minimum);

        gridLayout->addItem(mpSpacer, 3, 2, 1, 1);

        mpCheckRandomize = new QCheckBox(CQOptimizationWidget);
        mpCheckRandomize->setObjectName(QString::fromUtf8("mpCheckRandomize"));

        gridLayout->addWidget(mpCheckRandomize, 4, 1, 1, 1);

        mpCheckStatistics = new QCheckBox(CQOptimizationWidget);
        mpCheckStatistics->setObjectName(QString::fromUtf8("mpCheckStatistics"));

        gridLayout->addWidget(mpCheckStatistics, 4, 2, 1, 1);


        verticalLayout->addLayout(gridLayout);

        mpTabWidget = new QTabWidget(CQOptimizationWidget);
        mpTabWidget->setObjectName(QString::fromUtf8("mpTabWidget"));
        QSizePolicy sizePolicy3(QSizePolicy::Expanding, QSizePolicy::Expanding);
        sizePolicy3.setHorizontalStretch(0);
        sizePolicy3.setVerticalStretch(0);
        sizePolicy3.setHeightForWidth(mpTabWidget->sizePolicy().hasHeightForWidth());
        mpTabWidget->setSizePolicy(sizePolicy3);
        mpParameters = new CQFittingItemWidget();
        mpParameters->setObjectName(QString::fromUtf8("mpParameters"));
        mpTabWidget->addTab(mpParameters, QString());
        mpConstraints = new CQFittingItemWidget();
        mpConstraints->setObjectName(QString::fromUtf8("mpConstraints"));
        mpTabWidget->addTab(mpConstraints, QString());

        verticalLayout->addWidget(mpTabWidget);

        QWidget::setTabOrder(mpBoxSubtask, mpTabWidget);

        retranslateUi(CQOptimizationWidget);
        QObject::connect(mpTabWidget, SIGNAL(currentChanged(QWidget*)), CQOptimizationWidget, SLOT(slotPageChange(QWidget*)));
        QObject::connect(mpBoxSubtask, SIGNAL(activated(QString)), CQOptimizationWidget, SLOT(slotSubtaskChanged(QString)));

        mpTabWidget->setCurrentIndex(0);


        QMetaObject::connectSlotsByName(CQOptimizationWidget);
    } // setupUi

    void retranslateUi(TaskWidget *CQOptimizationWidget)
    {
        CQOptimizationWidget->setProperty("caption", QVariant(QApplication::translate("CQOptimizationWidget", "Optimization", 0, QApplication::UnicodeUTF8)));
        mpLblExpression->setText(QApplication::translate("CQOptimizationWidget", "Expression", 0, QApplication::UnicodeUTF8));
        mpBtnMinimize->setText(QApplication::translate("CQOptimizationWidget", "minimize", 0, QApplication::UnicodeUTF8));
        mpBtnMaximize->setText(QApplication::translate("CQOptimizationWidget", "maximize", 0, QApplication::UnicodeUTF8));
        mpLblType->setText(QApplication::translate("CQOptimizationWidget", "Subtask", 0, QApplication::UnicodeUTF8));
        mpCheckRandomize->setText(QApplication::translate("CQOptimizationWidget", "Randomize Start Values", 0, QApplication::UnicodeUTF8));
        mpCheckStatistics->setText(QApplication::translate("CQOptimizationWidget", "Calculate Statistics", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpParameters), QApplication::translate("CQOptimizationWidget", "Parameters (0)", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpConstraints), QApplication::translate("CQOptimizationWidget", "Constraints (0)", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQOptimizationWidget: public Ui_CQOptimizationWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQOPTIMIZATIONWIDGET_H
