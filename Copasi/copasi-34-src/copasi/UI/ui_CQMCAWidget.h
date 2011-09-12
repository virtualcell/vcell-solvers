/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQMCAWidget.ui'
**
** Created: Sun Sep 11 10:59:24 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQMCAWIDGET_H
#define UI_CQMCAWIDGET_H

#include <Qt3Support/Q3MimeSourceFactory>
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

class Ui_CQMCAWidget
{
public:
    QVBoxLayout *vboxLayout;
    QHBoxLayout *_2;
    QSpacerItem *mpSacer;
    QCheckBox *mpCheckSteadyState;
    QFrame *mpLine;

    void setupUi(TaskWidget *CQMCAWidget)
    {
        if (CQMCAWidget->objectName().isEmpty())
            CQMCAWidget->setObjectName(QString::fromUtf8("CQMCAWidget"));
        CQMCAWidget->resize(347, 61);
        vboxLayout = new QVBoxLayout(CQMCAWidget);
        vboxLayout->setSpacing(6);
        vboxLayout->setContentsMargins(11, 11, 11, 11);
        vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
        _2 = new QHBoxLayout();
        _2->setSpacing(6);
        _2->setObjectName(QString::fromUtf8("_2"));
        mpSacer = new QSpacerItem(97, 13, QSizePolicy::Fixed, QSizePolicy::Minimum);

        _2->addItem(mpSacer);

        mpCheckSteadyState = new QCheckBox(CQMCAWidget);
        mpCheckSteadyState->setObjectName(QString::fromUtf8("mpCheckSteadyState"));

        _2->addWidget(mpCheckSteadyState);


        vboxLayout->addLayout(_2);

        mpLine = new QFrame(CQMCAWidget);
        mpLine->setObjectName(QString::fromUtf8("mpLine"));
        mpLine->setFrameShape(QFrame::HLine);
        mpLine->setFrameShadow(QFrame::Sunken);

        vboxLayout->addWidget(mpLine);


        retranslateUi(CQMCAWidget);
        QObject::connect(mpCheckSteadyState, SIGNAL(clicked()), CQMCAWidget, SLOT(slotSteadyStateChecked()));

        QMetaObject::connectSlotsByName(CQMCAWidget);
    } // setupUi

    void retranslateUi(TaskWidget *CQMCAWidget)
    {
        CQMCAWidget->setProperty("caption", QVariant(QApplication::translate("CQMCAWidget", "CQMCAWidget", 0, QApplication::UnicodeUTF8)));
        mpCheckSteadyState->setText(QApplication::translate("CQMCAWidget", "perform Steady State Analysis", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQMCAWidget: public Ui_CQMCAWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQMCAWIDGET_H
