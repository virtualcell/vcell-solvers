/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQProgressDialog.ui'
**
** Created: Thu Aug 18 12:47:30 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQPROGRESSDIALOG_H
#define UI_CQPROGRESSDIALOG_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDialog>
#include <QtGui/QFrame>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QVBoxLayout>
#include "CQProgressItem.h"

QT_BEGIN_NAMESPACE

class Ui_CQProgressDialog
{
public:
    QVBoxLayout *vboxLayout;
    QHBoxLayout *hboxLayout;
    QPushButton *mpBtnPause;
    QPushButton *mpBtnContinue;
    QPushButton *mpBtnStop;
    QSpacerItem *mpBtnSpacer;
    QFrame *mpLine;
    QSpacerItem *mpSpacer;

    void setupUi(QDialog *CQProgressDialog)
    {
        if (CQProgressDialog->objectName().isEmpty())
            CQProgressDialog->setObjectName(QString::fromUtf8("CQProgressDialog"));
        CQProgressDialog->resize(382, 203);
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Expanding);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(CQProgressDialog->sizePolicy().hasHeightForWidth());
        CQProgressDialog->setSizePolicy(sizePolicy);
        CQProgressDialog->setMinimumSize(QSize(300, 57));
        vboxLayout = new QVBoxLayout(CQProgressDialog);
        vboxLayout->setSpacing(3);
        vboxLayout->setContentsMargins(11, 11, 11, 11);
        vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        mpBtnPause = new QPushButton(CQProgressDialog);
        mpBtnPause->setObjectName(QString::fromUtf8("mpBtnPause"));
        QSizePolicy sizePolicy1(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(mpBtnPause->sizePolicy().hasHeightForWidth());
        mpBtnPause->setSizePolicy(sizePolicy1);

        hboxLayout->addWidget(mpBtnPause);

        mpBtnContinue = new QPushButton(CQProgressDialog);
        mpBtnContinue->setObjectName(QString::fromUtf8("mpBtnContinue"));
        sizePolicy1.setHeightForWidth(mpBtnContinue->sizePolicy().hasHeightForWidth());
        mpBtnContinue->setSizePolicy(sizePolicy1);

        hboxLayout->addWidget(mpBtnContinue);

        mpBtnStop = new QPushButton(CQProgressDialog);
        mpBtnStop->setObjectName(QString::fromUtf8("mpBtnStop"));
        sizePolicy1.setHeightForWidth(mpBtnStop->sizePolicy().hasHeightForWidth());
        mpBtnStop->setSizePolicy(sizePolicy1);

        hboxLayout->addWidget(mpBtnStop);

        mpBtnSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout->addItem(mpBtnSpacer);


        vboxLayout->addLayout(hboxLayout);

        mpLine = new QFrame(CQProgressDialog);
        mpLine->setObjectName(QString::fromUtf8("mpLine"));
        mpLine->setMinimumSize(QSize(200, 0));
        mpLine->setFrameShape(QFrame::HLine);
        mpLine->setFrameShadow(QFrame::Sunken);

        vboxLayout->addWidget(mpLine);

        mpSpacer = new QSpacerItem(20, 40, QSizePolicy::Minimum, QSizePolicy::Expanding);

        vboxLayout->addItem(mpSpacer);

        QWidget::setTabOrder(mpBtnStop, mpBtnPause);
        QWidget::setTabOrder(mpBtnPause, mpBtnContinue);

        retranslateUi(CQProgressDialog);
        QObject::connect(mpBtnStop, SIGNAL(clicked()), CQProgressDialog, SLOT(btnStopPressed()));
        QObject::connect(mpBtnPause, SIGNAL(clicked()), CQProgressDialog, SLOT(btnPausePressed()));
        QObject::connect(mpBtnContinue, SIGNAL(clicked()), CQProgressDialog, SLOT(btnContinuePressed()));

        QMetaObject::connectSlotsByName(CQProgressDialog);
    } // setupUi

    void retranslateUi(QDialog *CQProgressDialog)
    {
        CQProgressDialog->setWindowTitle(QApplication::translate("CQProgressDialog", "COPASI Progress Dialog", 0, QApplication::UnicodeUTF8));
        mpBtnPause->setText(QString());
        mpBtnContinue->setText(QString());
        mpBtnStop->setText(QString());
    } // retranslateUi

};

namespace Ui {
    class CQProgressDialog: public Ui_CQProgressDialog {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQPROGRESSDIALOG_H
