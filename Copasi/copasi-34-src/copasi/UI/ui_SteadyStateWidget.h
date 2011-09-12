/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'SteadyStateWidget.ui'
**
** Created: Sun Sep 11 10:59:19 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_STEADYSTATEWIDGET_H
#define UI_STEADYSTATEWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QFrame>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QSpacerItem>
#include <QtGui/QVBoxLayout>
#include "TaskWidget.h"

QT_BEGIN_NAMESPACE

class Ui_SteadyStateWidget
{
public:
    QVBoxLayout *SteadyStateWidgetLayout;
    QHBoxLayout *horizontalLayout;
    QSpacerItem *horizontalSpacer;
    QCheckBox *taskJacobian;
    QCheckBox *taskStability;
    QFrame *line1;

    void setupUi(TaskWidget *SteadyStateWidget)
    {
        if (SteadyStateWidget->objectName().isEmpty())
            SteadyStateWidget->setObjectName(QString::fromUtf8("SteadyStateWidget"));
        SteadyStateWidget->resize(366, 74);
        SteadyStateWidgetLayout = new QVBoxLayout(SteadyStateWidget);
        SteadyStateWidgetLayout->setObjectName(QString::fromUtf8("SteadyStateWidgetLayout"));
        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);

        taskJacobian = new QCheckBox(SteadyStateWidget);
        taskJacobian->setObjectName(QString::fromUtf8("taskJacobian"));

        horizontalLayout->addWidget(taskJacobian);

        taskStability = new QCheckBox(SteadyStateWidget);
        taskStability->setObjectName(QString::fromUtf8("taskStability"));
        taskStability->setEnabled(false);

        horizontalLayout->addWidget(taskStability);


        SteadyStateWidgetLayout->addLayout(horizontalLayout);

        line1 = new QFrame(SteadyStateWidget);
        line1->setObjectName(QString::fromUtf8("line1"));
        line1->setFrameShape(QFrame::HLine);
        line1->setFrameShadow(QFrame::Sunken);

        SteadyStateWidgetLayout->addWidget(line1);


        retranslateUi(SteadyStateWidget);
        QObject::connect(taskJacobian, SIGNAL(toggled(bool)), SteadyStateWidget, SLOT(taskJacobianToggled()));

        QMetaObject::connectSlotsByName(SteadyStateWidget);
    } // setupUi

    void retranslateUi(TaskWidget *SteadyStateWidget)
    {
        SteadyStateWidget->setWindowTitle(QApplication::translate("SteadyStateWidget", "Form", 0, QApplication::UnicodeUTF8));
        taskJacobian->setText(QApplication::translate("SteadyStateWidget", "calculate Jacobian", 0, QApplication::UnicodeUTF8));
        taskStability->setText(QApplication::translate("SteadyStateWidget", "perform Stability Analysis", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class SteadyStateWidget: public Ui_SteadyStateWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_STEADYSTATEWIDGET_H
