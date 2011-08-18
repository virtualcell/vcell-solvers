/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQEFMWidget.ui'
**
** Created: Thu Aug 18 12:47:32 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQEFMWIDGET_H
#define UI_CQEFMWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QFrame>
#include <QtGui/QHeaderView>
#include <QtGui/QVBoxLayout>
#include "TaskWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQEFMWidget
{
public:
    QVBoxLayout *verticalLayout;
    QFrame *line1;

    void setupUi(TaskWidget *CQEFMWidget)
    {
        if (CQEFMWidget->objectName().isEmpty())
            CQEFMWidget->setObjectName(QString::fromUtf8("CQEFMWidget"));
        CQEFMWidget->resize(363, 25);
        verticalLayout = new QVBoxLayout(CQEFMWidget);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        line1 = new QFrame(CQEFMWidget);
        line1->setObjectName(QString::fromUtf8("line1"));
        line1->setFrameShape(QFrame::HLine);
        line1->setFrameShadow(QFrame::Sunken);

        verticalLayout->addWidget(line1);


        retranslateUi(CQEFMWidget);

        QMetaObject::connectSlotsByName(CQEFMWidget);
    } // setupUi

    void retranslateUi(TaskWidget *CQEFMWidget)
    {
        CQEFMWidget->setWindowTitle(QApplication::translate("CQEFMWidget", "Form", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQEFMWidget: public Ui_CQEFMWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQEFMWIDGET_H
