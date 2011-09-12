/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQTaskBtnWidget.ui'
**
** Created: Sun Sep 11 10:59:20 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQTASKBTNWIDGET_H
#define UI_CQTASKBTNWIDGET_H

#include <Qt3Support/Q3MimeSourceFactory>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_CQTaskBtnWidget
{
public:
    QVBoxLayout *verticalLayout;
    QSpacerItem *verticalSpacer;
    QHBoxLayout *horizontalLayout;
    QPushButton *mpBtnRun;
    QPushButton *mpBtnRevert;
    QSpacerItem *mpSpacerHorizontal;
    QPushButton *mpBtnReport;
    QPushButton *mpBtnAssistant;

    void setupUi(QWidget *CQTaskBtnWidget)
    {
        if (CQTaskBtnWidget->objectName().isEmpty())
            CQTaskBtnWidget->setObjectName(QString::fromUtf8("CQTaskBtnWidget"));
        CQTaskBtnWidget->resize(438, 66);
        verticalLayout = new QVBoxLayout(CQTaskBtnWidget);
        verticalLayout->setSpacing(6);
        verticalLayout->setContentsMargins(11, 11, 11, 11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        verticalLayout->setContentsMargins(0, 6, 0, 0);
        verticalSpacer = new QSpacerItem(20, 15, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout->addItem(verticalSpacer);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(-1);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        mpBtnRun = new QPushButton(CQTaskBtnWidget);
        mpBtnRun->setObjectName(QString::fromUtf8("mpBtnRun"));

        horizontalLayout->addWidget(mpBtnRun);

        mpBtnRevert = new QPushButton(CQTaskBtnWidget);
        mpBtnRevert->setObjectName(QString::fromUtf8("mpBtnRevert"));

        horizontalLayout->addWidget(mpBtnRevert);

        mpSpacerHorizontal = new QSpacerItem(13, 17, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(mpSpacerHorizontal);

        mpBtnReport = new QPushButton(CQTaskBtnWidget);
        mpBtnReport->setObjectName(QString::fromUtf8("mpBtnReport"));

        horizontalLayout->addWidget(mpBtnReport);

        mpBtnAssistant = new QPushButton(CQTaskBtnWidget);
        mpBtnAssistant->setObjectName(QString::fromUtf8("mpBtnAssistant"));

        horizontalLayout->addWidget(mpBtnAssistant);


        verticalLayout->addLayout(horizontalLayout);

        QWidget::setTabOrder(mpBtnRun, mpBtnRevert);
        QWidget::setTabOrder(mpBtnRevert, mpBtnReport);
        QWidget::setTabOrder(mpBtnReport, mpBtnAssistant);

        retranslateUi(CQTaskBtnWidget);

        QMetaObject::connectSlotsByName(CQTaskBtnWidget);
    } // setupUi

    void retranslateUi(QWidget *CQTaskBtnWidget)
    {
        CQTaskBtnWidget->setWindowTitle(QApplication::translate("CQTaskBtnWidget", "CQTaskBtnWidget", 0, QApplication::UnicodeUTF8));
        mpBtnRun->setText(QApplication::translate("CQTaskBtnWidget", "Run", 0, QApplication::UnicodeUTF8));
        mpBtnRevert->setText(QApplication::translate("CQTaskBtnWidget", "Revert", 0, QApplication::UnicodeUTF8));
        mpBtnReport->setText(QApplication::translate("CQTaskBtnWidget", "Report", 0, QApplication::UnicodeUTF8));
        mpBtnAssistant->setText(QApplication::translate("CQTaskBtnWidget", "Output Assistant", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQTaskBtnWidget: public Ui_CQTaskBtnWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQTASKBTNWIDGET_H
