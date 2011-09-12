/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQOptimizationResult.ui'
**
** Created: Sun Sep 11 10:59:23 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQOPTIMIZATIONRESULT_H
#define UI_CQOPTIMIZATIONRESULT_H

#include <Qt3Support/Q3Frame>
#include <Qt3Support/Q3Header>
#include <Qt3Support/Q3MimeSourceFactory>
#include <Qt3Support/Q3Table>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>
#include <QtGui/QVBoxLayout>
#include "copasi/UI/copasiWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQOptimizationResult
{
public:
    QVBoxLayout *vboxLayout;
    QHBoxLayout *hboxLayout;
    QLabel *mpLblResult;
    QPushButton *mpBtnUpdateModel;
    QPushButton *mpBtnSave;
    QGridLayout *gridLayout;
    QLineEdit *mpEditCPUTime;
    QLineEdit *mpEditEvaluations;
    QLineEdit *mpEditSpeed;
    QLabel *mpLblSpeed;
    QLabel *mpLblEvaluations;
    QLabel *mpLblCPUTime;
    QLabel *mpLblObjectiveValue;
    QLineEdit *mpEditObjectiveValue;
    Q3Table *mpParameters;

    void setupUi(CopasiWidget *CQOptimizationResult)
    {
        if (CQOptimizationResult->objectName().isEmpty())
            CQOptimizationResult->setObjectName(QString::fromUtf8("CQOptimizationResult"));
        CQOptimizationResult->resize(570, 559);
        vboxLayout = new QVBoxLayout(CQOptimizationResult);
        vboxLayout->setSpacing(6);
        vboxLayout->setContentsMargins(11, 11, 11, 11);
        vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        mpLblResult = new QLabel(CQOptimizationResult);
        mpLblResult->setObjectName(QString::fromUtf8("mpLblResult"));
        QSizePolicy sizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(mpLblResult->sizePolicy().hasHeightForWidth());
        mpLblResult->setSizePolicy(sizePolicy);
        mpLblResult->setWordWrap(false);

        hboxLayout->addWidget(mpLblResult);

        mpBtnUpdateModel = new QPushButton(CQOptimizationResult);
        mpBtnUpdateModel->setObjectName(QString::fromUtf8("mpBtnUpdateModel"));

        hboxLayout->addWidget(mpBtnUpdateModel);

        mpBtnSave = new QPushButton(CQOptimizationResult);
        mpBtnSave->setObjectName(QString::fromUtf8("mpBtnSave"));

        hboxLayout->addWidget(mpBtnSave);


        vboxLayout->addLayout(hboxLayout);

        gridLayout = new QGridLayout();
        gridLayout->setSpacing(6);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        mpEditCPUTime = new QLineEdit(CQOptimizationResult);
        mpEditCPUTime->setObjectName(QString::fromUtf8("mpEditCPUTime"));
        mpEditCPUTime->setReadOnly(true);

        gridLayout->addWidget(mpEditCPUTime, 3, 1, 1, 1);

        mpEditEvaluations = new QLineEdit(CQOptimizationResult);
        mpEditEvaluations->setObjectName(QString::fromUtf8("mpEditEvaluations"));
        mpEditEvaluations->setReadOnly(true);

        gridLayout->addWidget(mpEditEvaluations, 3, 0, 1, 1);

        mpEditSpeed = new QLineEdit(CQOptimizationResult);
        mpEditSpeed->setObjectName(QString::fromUtf8("mpEditSpeed"));
        mpEditSpeed->setReadOnly(true);

        gridLayout->addWidget(mpEditSpeed, 3, 2, 1, 1);

        mpLblSpeed = new QLabel(CQOptimizationResult);
        mpLblSpeed->setObjectName(QString::fromUtf8("mpLblSpeed"));
        mpLblSpeed->setWordWrap(false);

        gridLayout->addWidget(mpLblSpeed, 2, 2, 1, 1);

        mpLblEvaluations = new QLabel(CQOptimizationResult);
        mpLblEvaluations->setObjectName(QString::fromUtf8("mpLblEvaluations"));
        mpLblEvaluations->setWordWrap(false);

        gridLayout->addWidget(mpLblEvaluations, 2, 0, 1, 1);

        mpLblCPUTime = new QLabel(CQOptimizationResult);
        mpLblCPUTime->setObjectName(QString::fromUtf8("mpLblCPUTime"));
        mpLblCPUTime->setWordWrap(false);

        gridLayout->addWidget(mpLblCPUTime, 2, 1, 1, 1);

        mpLblObjectiveValue = new QLabel(CQOptimizationResult);
        mpLblObjectiveValue->setObjectName(QString::fromUtf8("mpLblObjectiveValue"));
        mpLblObjectiveValue->setWordWrap(false);

        gridLayout->addWidget(mpLblObjectiveValue, 0, 0, 1, 1);

        mpEditObjectiveValue = new QLineEdit(CQOptimizationResult);
        mpEditObjectiveValue->setObjectName(QString::fromUtf8("mpEditObjectiveValue"));
        mpEditObjectiveValue->setReadOnly(true);

        gridLayout->addWidget(mpEditObjectiveValue, 1, 0, 1, 1);


        vboxLayout->addLayout(gridLayout);

        mpParameters = new Q3Table(CQOptimizationResult);
        mpParameters->setObjectName(QString::fromUtf8("mpParameters"));
        mpParameters->setNumRows(0);
        mpParameters->setNumCols(0);

        vboxLayout->addWidget(mpParameters);


        retranslateUi(CQOptimizationResult);
        QObject::connect(mpBtnSave, SIGNAL(clicked()), CQOptimizationResult, SLOT(slotSave()));
        QObject::connect(mpBtnUpdateModel, SIGNAL(clicked()), CQOptimizationResult, SLOT(slotUpdateModel()));

        QMetaObject::connectSlotsByName(CQOptimizationResult);
    } // setupUi

    void retranslateUi(CopasiWidget *CQOptimizationResult)
    {
        CQOptimizationResult->setWindowTitle(QApplication::translate("CQOptimizationResult", "Optimization Result Window", 0, QApplication::UnicodeUTF8));
        CQOptimizationResult->setProperty("caption", QVariant(QApplication::translate("CQOptimizationResult", "Fitting Result", 0, QApplication::UnicodeUTF8)));
        mpLblResult->setText(QApplication::translate("CQOptimizationResult", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:'Lucida Grande'; font-size:13pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:16px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-size:x-large; font-weight:600;\">Optimization Result</span></p></body></html>", 0, QApplication::UnicodeUTF8));
        mpBtnUpdateModel->setText(QApplication::translate("CQOptimizationResult", "Update Model", 0, QApplication::UnicodeUTF8));
        mpBtnSave->setText(QApplication::translate("CQOptimizationResult", "Save to File", 0, QApplication::UnicodeUTF8));
        mpLblSpeed->setText(QApplication::translate("CQOptimizationResult", "Evaluations/second [1/s]", 0, QApplication::UnicodeUTF8));
        mpLblEvaluations->setText(QApplication::translate("CQOptimizationResult", "Function Evaluations", 0, QApplication::UnicodeUTF8));
        mpLblCPUTime->setText(QApplication::translate("CQOptimizationResult", "CPU Time [s]", 0, QApplication::UnicodeUTF8));
        mpLblObjectiveValue->setText(QApplication::translate("CQOptimizationResult", "Objective Value", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQOptimizationResult: public Ui_CQOptimizationResult {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQOPTIMIZATIONRESULT_H
