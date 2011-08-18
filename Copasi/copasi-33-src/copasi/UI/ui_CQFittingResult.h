/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQFittingResult.ui'
**
** Created: Thu Aug 18 12:47:31 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQFITTINGRESULT_H
#define UI_CQFITTINGRESULT_H

#include <Qt3Support/Q3Frame>
#include <Qt3Support/Q3Header>
#include <Qt3Support/Q3MimeSourceFactory>
#include <Qt3Support/Q3Table>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include <QtGui/QTabWidget>
#include <QtGui/QVBoxLayout>
#include "CQArrayAnnotationsWidget.h"
#include "CQFittingResultTab1.h"
#include "copasiWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQFittingResult
{
public:
    QVBoxLayout *verticalLayout;
    QHBoxLayout *horizontalLayout;
    QLabel *mpLblResult;
    QPushButton *mpBtnSave;
    QTabWidget *mpTabWidget;
    CQFittingResultTab1 *mpMain;
    Q3Table *mpParameters;
    Q3Table *mpExperiments;
    Q3Table *mpValues;
    CQArrayAnnotationsWidget *mpCorrelations;
    CQArrayAnnotationsWidget *mpFisherInformation;
    Q3Table *mpCrossValidations;
    Q3Table *mpCrossValidationValues;

    void setupUi(CopasiWidget *CQFittingResult)
    {
        if (CQFittingResult->objectName().isEmpty())
            CQFittingResult->setObjectName(QString::fromUtf8("CQFittingResult"));
        CQFittingResult->resize(440, 253);
        verticalLayout = new QVBoxLayout(CQFittingResult);
        verticalLayout->setSpacing(6);
        verticalLayout->setContentsMargins(11, 11, 11, 11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        mpLblResult = new QLabel(CQFittingResult);
        mpLblResult->setObjectName(QString::fromUtf8("mpLblResult"));
        QSizePolicy sizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(mpLblResult->sizePolicy().hasHeightForWidth());
        mpLblResult->setSizePolicy(sizePolicy);
        mpLblResult->setWordWrap(false);

        horizontalLayout->addWidget(mpLblResult);

        mpBtnSave = new QPushButton(CQFittingResult);
        mpBtnSave->setObjectName(QString::fromUtf8("mpBtnSave"));

        horizontalLayout->addWidget(mpBtnSave);


        verticalLayout->addLayout(horizontalLayout);

        mpTabWidget = new QTabWidget(CQFittingResult);
        mpTabWidget->setObjectName(QString::fromUtf8("mpTabWidget"));
        QSizePolicy sizePolicy1(QSizePolicy::Ignored, QSizePolicy::Preferred);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(mpTabWidget->sizePolicy().hasHeightForWidth());
        mpTabWidget->setSizePolicy(sizePolicy1);
        mpTabWidget->setMinimumSize(QSize(210, 180));
        mpMain = new CQFittingResultTab1();
        mpMain->setObjectName(QString::fromUtf8("mpMain"));
        mpTabWidget->addTab(mpMain, QString());
        mpParameters = new Q3Table();
        mpParameters->setObjectName(QString::fromUtf8("mpParameters"));
        mpTabWidget->addTab(mpParameters, QString());
        mpExperiments = new Q3Table();
        mpExperiments->setObjectName(QString::fromUtf8("mpExperiments"));
        mpTabWidget->addTab(mpExperiments, QString());
        mpValues = new Q3Table();
        mpValues->setObjectName(QString::fromUtf8("mpValues"));
        mpTabWidget->addTab(mpValues, QString());
        mpCorrelations = new CQArrayAnnotationsWidget();
        mpCorrelations->setObjectName(QString::fromUtf8("mpCorrelations"));
        mpTabWidget->addTab(mpCorrelations, QString());
        mpFisherInformation = new CQArrayAnnotationsWidget();
        mpFisherInformation->setObjectName(QString::fromUtf8("mpFisherInformation"));
        mpTabWidget->addTab(mpFisherInformation, QString());
        mpCrossValidations = new Q3Table();
        mpCrossValidations->setObjectName(QString::fromUtf8("mpCrossValidations"));
        mpTabWidget->addTab(mpCrossValidations, QString());
        mpCrossValidationValues = new Q3Table();
        mpCrossValidationValues->setObjectName(QString::fromUtf8("mpCrossValidationValues"));
        mpTabWidget->addTab(mpCrossValidationValues, QString());

        verticalLayout->addWidget(mpTabWidget);


        retranslateUi(CQFittingResult);
        QObject::connect(mpBtnSave, SIGNAL(clicked()), CQFittingResult, SLOT(slotSave()));

        QMetaObject::connectSlotsByName(CQFittingResult);
    } // setupUi

    void retranslateUi(CopasiWidget *CQFittingResult)
    {
        CQFittingResult->setWindowTitle(QApplication::translate("CQFittingResult", "Parameter Estimation Result Window", 0, QApplication::UnicodeUTF8));
        CQFittingResult->setProperty("caption", QVariant(QApplication::translate("CQFittingResult", "Fitting Result", 0, QApplication::UnicodeUTF8)));
        mpLblResult->setText(QApplication::translate("CQFittingResult", "<h2>Parameter Estimation Result</h2>", 0, QApplication::UnicodeUTF8));
        mpBtnSave->setText(QApplication::translate("CQFittingResult", "Save to File", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpMain), QApplication::translate("CQFittingResult", "Main", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpParameters), QApplication::translate("CQFittingResult", "Parameters", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpExperiments), QApplication::translate("CQFittingResult", "Experiments", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpValues), QApplication::translate("CQFittingResult", "Fitted Values", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpCorrelations), QApplication::translate("CQFittingResult", "Correlation", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpFisherInformation), QApplication::translate("CQFittingResult", "Fisher Information", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpCrossValidations), QApplication::translate("CQFittingResult", "CV Experiments", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpCrossValidationValues), QApplication::translate("CQFittingResult", "CV Fitted Values", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQFittingResult: public Ui_CQFittingResult {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQFITTINGRESULT_H
