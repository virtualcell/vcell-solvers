/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQSteadyStateResult.ui'
**
** Created: Sun Sep 11 10:59:21 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQSTEADYSTATERESULT_H
#define UI_CQSTEADYSTATERESULT_H

#include <Qt3Support/Q3MimeSourceFactory>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QGridLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include "copasi/UI/StateSubwidget.h"
#include "copasi/UI/copasiWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQSteadyStateResult
{
public:
    QGridLayout *gridLayout;
    QLabel *mpLblResult;
    QSpacerItem *mpSpacer;
    QPushButton *mpBtnUpdateModel;
    QPushButton *mpBtnSave;
    StateSubwidget *mpCentralWidget;

    void setupUi(CopasiWidget *CQSteadyStateResult)
    {
        if (CQSteadyStateResult->objectName().isEmpty())
            CQSteadyStateResult->setObjectName(QString::fromUtf8("CQSteadyStateResult"));
        CQSteadyStateResult->resize(473, 81);
        gridLayout = new QGridLayout(CQSteadyStateResult);
        gridLayout->setSpacing(6);
        gridLayout->setContentsMargins(11, 11, 11, 11);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        mpLblResult = new QLabel(CQSteadyStateResult);
        mpLblResult->setObjectName(QString::fromUtf8("mpLblResult"));
        QSizePolicy sizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(mpLblResult->sizePolicy().hasHeightForWidth());
        mpLblResult->setSizePolicy(sizePolicy);
        mpLblResult->setWordWrap(false);

        gridLayout->addWidget(mpLblResult, 0, 0, 1, 1);

        mpSpacer = new QSpacerItem(0, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        gridLayout->addItem(mpSpacer, 0, 1, 1, 1);

        mpBtnUpdateModel = new QPushButton(CQSteadyStateResult);
        mpBtnUpdateModel->setObjectName(QString::fromUtf8("mpBtnUpdateModel"));

        gridLayout->addWidget(mpBtnUpdateModel, 0, 2, 1, 1);

        mpBtnSave = new QPushButton(CQSteadyStateResult);
        mpBtnSave->setObjectName(QString::fromUtf8("mpBtnSave"));

        gridLayout->addWidget(mpBtnSave, 0, 3, 1, 1);

        mpCentralWidget = new StateSubwidget(CQSteadyStateResult);
        mpCentralWidget->setObjectName(QString::fromUtf8("mpCentralWidget"));

        gridLayout->addWidget(mpCentralWidget, 1, 0, 1, 4);


        retranslateUi(CQSteadyStateResult);
        QObject::connect(mpBtnSave, SIGNAL(clicked()), CQSteadyStateResult, SLOT(slotSave()));
        QObject::connect(mpBtnUpdateModel, SIGNAL(clicked()), CQSteadyStateResult, SLOT(slotUpdateModel()));

        QMetaObject::connectSlotsByName(CQSteadyStateResult);
    } // setupUi

    void retranslateUi(CopasiWidget *CQSteadyStateResult)
    {
        CQSteadyStateResult->setWindowTitle(QApplication::translate("CQSteadyStateResult", "Steady State Result Window", 0, QApplication::UnicodeUTF8));
        CQSteadyStateResult->setProperty("caption", QVariant(QApplication::translate("CQSteadyStateResult", "Fitting Result", 0, QApplication::UnicodeUTF8)));
        mpLblResult->setText(QApplication::translate("CQSteadyStateResult", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:'Lucida Grande'; font-size:13pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:16px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-size:x-large; font-weight:600;\">Steady State Result</span></p></body></html>", 0, QApplication::UnicodeUTF8));
        mpBtnUpdateModel->setText(QApplication::translate("CQSteadyStateResult", "Update Model", 0, QApplication::UnicodeUTF8));
        mpBtnSave->setText(QApplication::translate("CQSteadyStateResult", "Save to File", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQSteadyStateResult: public Ui_CQSteadyStateResult {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQSTEADYSTATERESULT_H
