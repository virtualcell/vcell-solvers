/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQFittingWidget.ui'
**
** Created: Sun Sep 11 10:59:24 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQFITTINGWIDGET_H
#define UI_CQFITTINGWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QTabWidget>
#include <QtGui/QVBoxLayout>
#include "TaskWidget.h"
#include "copasi/UI/CQFittingItemWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQFittingWidget
{
public:
    QVBoxLayout *verticalLayout;
    QHBoxLayout *horizontalLayout;
    QSpacerItem *mpSpacer;
    QPushButton *mpBtnExperiment;
    QPushButton *mpBtnCrossValidation;
    QHBoxLayout *horizontalLayout_2;
    QCheckBox *mpCheckRandomize;
    QCheckBox *mpCheckStatistics;
    QTabWidget *mpTabWidget;
    CQFittingItemWidget *mpParameters;
    CQFittingItemWidget *mpConstraints;

    void setupUi(TaskWidget *CQFittingWidget)
    {
        if (CQFittingWidget->objectName().isEmpty())
            CQFittingWidget->setObjectName(QString::fromUtf8("CQFittingWidget"));
        CQFittingWidget->resize(527, 207);
        verticalLayout = new QVBoxLayout(CQFittingWidget);
        verticalLayout->setSpacing(6);
        verticalLayout->setContentsMargins(11, 11, 11, 11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        mpSpacer = new QSpacerItem(13, 17, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(mpSpacer);

        mpBtnExperiment = new QPushButton(CQFittingWidget);
        mpBtnExperiment->setObjectName(QString::fromUtf8("mpBtnExperiment"));

        horizontalLayout->addWidget(mpBtnExperiment);

        mpBtnCrossValidation = new QPushButton(CQFittingWidget);
        mpBtnCrossValidation->setObjectName(QString::fromUtf8("mpBtnCrossValidation"));

        horizontalLayout->addWidget(mpBtnCrossValidation);


        verticalLayout->addLayout(horizontalLayout);

        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setSpacing(6);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        mpCheckRandomize = new QCheckBox(CQFittingWidget);
        mpCheckRandomize->setObjectName(QString::fromUtf8("mpCheckRandomize"));

        horizontalLayout_2->addWidget(mpCheckRandomize);

        mpCheckStatistics = new QCheckBox(CQFittingWidget);
        mpCheckStatistics->setObjectName(QString::fromUtf8("mpCheckStatistics"));

        horizontalLayout_2->addWidget(mpCheckStatistics);


        verticalLayout->addLayout(horizontalLayout_2);

        mpTabWidget = new QTabWidget(CQFittingWidget);
        mpTabWidget->setObjectName(QString::fromUtf8("mpTabWidget"));
        mpParameters = new CQFittingItemWidget();
        mpParameters->setObjectName(QString::fromUtf8("mpParameters"));
        mpTabWidget->addTab(mpParameters, QString());
        mpConstraints = new CQFittingItemWidget();
        mpConstraints->setObjectName(QString::fromUtf8("mpConstraints"));
        mpTabWidget->addTab(mpConstraints, QString());

        verticalLayout->addWidget(mpTabWidget);

        QWidget::setTabOrder(mpBtnExperiment, mpBtnCrossValidation);
        QWidget::setTabOrder(mpBtnCrossValidation, mpTabWidget);

        retranslateUi(CQFittingWidget);
        QObject::connect(mpBtnExperiment, SIGNAL(clicked()), CQFittingWidget, SLOT(slotExperimentData()));
        QObject::connect(mpTabWidget, SIGNAL(currentChanged(QWidget*)), CQFittingWidget, SLOT(slotPageChange(QWidget*)));
        QObject::connect(mpBtnCrossValidation, SIGNAL(clicked()), CQFittingWidget, SLOT(slotCrossValidationData()));

        mpTabWidget->setCurrentIndex(0);


        QMetaObject::connectSlotsByName(CQFittingWidget);
    } // setupUi

    void retranslateUi(TaskWidget *CQFittingWidget)
    {
        CQFittingWidget->setProperty("caption", QVariant(QApplication::translate("CQFittingWidget", "Fitting", 0, QApplication::UnicodeUTF8)));
        mpBtnExperiment->setText(QApplication::translate("CQFittingWidget", "Experimental Data", 0, QApplication::UnicodeUTF8));
        mpBtnCrossValidation->setText(QApplication::translate("CQFittingWidget", "Cross Validation Data", 0, QApplication::UnicodeUTF8));
        mpCheckRandomize->setText(QApplication::translate("CQFittingWidget", "Randomize Start Values", 0, QApplication::UnicodeUTF8));
        mpCheckStatistics->setText(QApplication::translate("CQFittingWidget", "Calculate Statistics", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpParameters), QApplication::translate("CQFittingWidget", "Parameters (0)", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpConstraints), QApplication::translate("CQFittingWidget", "Constraints (0)", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQFittingWidget: public Ui_CQFittingWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQFITTINGWIDGET_H
