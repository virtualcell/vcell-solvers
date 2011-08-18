/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQFittingWidget.ui'
**
** Created: Thu Aug 18 12:47:31 2011
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
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QTabWidget>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>
#include "TaskWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQFittingWidget
{
public:
    QVBoxLayout *vboxLayout;
    QHBoxLayout *horizontalLayout;
    QSpacerItem *mpSpacer;
    QPushButton *mpBtnExperiment;
    QPushButton *mpBtnCrossValidation;
    QTabWidget *mpTabWidget;
    QWidget *mpParametersPage;
    QWidget *mpConstraintsPage;

    void setupUi(TaskWidget *CQFittingWidget)
    {
        if (CQFittingWidget->objectName().isEmpty())
            CQFittingWidget->setObjectName(QString::fromUtf8("CQFittingWidget"));
        CQFittingWidget->resize(541, 456);
        vboxLayout = new QVBoxLayout(CQFittingWidget);
        vboxLayout->setSpacing(6);
        vboxLayout->setContentsMargins(11, 11, 11, 11);
        vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
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


        vboxLayout->addLayout(horizontalLayout);

        mpTabWidget = new QTabWidget(CQFittingWidget);
        mpTabWidget->setObjectName(QString::fromUtf8("mpTabWidget"));
        mpParametersPage = new QWidget();
        mpParametersPage->setObjectName(QString::fromUtf8("mpParametersPage"));
        mpTabWidget->addTab(mpParametersPage, QString());
        mpConstraintsPage = new QWidget();
        mpConstraintsPage->setObjectName(QString::fromUtf8("mpConstraintsPage"));
        mpTabWidget->addTab(mpConstraintsPage, QString());

        vboxLayout->addWidget(mpTabWidget);

        QWidget::setTabOrder(mpBtnExperiment, mpBtnCrossValidation);
        QWidget::setTabOrder(mpBtnCrossValidation, mpTabWidget);

        retranslateUi(CQFittingWidget);
        QObject::connect(mpBtnExperiment, SIGNAL(clicked()), CQFittingWidget, SLOT(slotExperimentData()));
        QObject::connect(mpTabWidget, SIGNAL(currentChanged(QWidget*)), CQFittingWidget, SLOT(slotPageChange(QWidget*)));
        QObject::connect(mpBtnCrossValidation, SIGNAL(clicked()), CQFittingWidget, SLOT(slotCrossValidationData()));

        QMetaObject::connectSlotsByName(CQFittingWidget);
    } // setupUi

    void retranslateUi(TaskWidget *CQFittingWidget)
    {
        CQFittingWidget->setProperty("caption", QVariant(QApplication::translate("CQFittingWidget", "Fitting", 0, QApplication::UnicodeUTF8)));
        mpBtnExperiment->setText(QApplication::translate("CQFittingWidget", "Experimental Data", 0, QApplication::UnicodeUTF8));
        mpBtnCrossValidation->setText(QApplication::translate("CQFittingWidget", "Cross Validation Data", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpParametersPage), QApplication::translate("CQFittingWidget", "Parameters (0)", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpConstraintsPage), QApplication::translate("CQFittingWidget", "Constraints (0)", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQFittingWidget: public Ui_CQFittingWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQFITTINGWIDGET_H
