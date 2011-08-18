/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQMoietiesTaskResult.ui'
**
** Created: Thu Aug 18 12:47:31 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQMOIETIESTASKRESULT_H
#define UI_CQMOIETIESTASKRESULT_H

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
#include <QtGui/QSpacerItem>
#include <QtGui/QTabWidget>
#include <QtGui/QVBoxLayout>
#include "CQArrayAnnotationsWidget.h"
#include "copasiWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQMoietiesTaskResult
{
public:
    QVBoxLayout *verticalLayout;
    QHBoxLayout *horizontalLayout;
    QLabel *mpLblResult;
    QPushButton *mpBtnSave;
    QSpacerItem *verticalSpacer;
    QTabWidget *mpTabWidget;
    Q3Table *mpMoieties;
    CQArrayAnnotationsWidget *mpStoichiometry;
    CQArrayAnnotationsWidget *mpLinkMatrix;
    CQArrayAnnotationsWidget *mpReducedStoichiometry;

    void setupUi(CopasiWidget *CQMoietiesTaskResult)
    {
        if (CQMoietiesTaskResult->objectName().isEmpty())
            CQMoietiesTaskResult->setObjectName(QString::fromUtf8("CQMoietiesTaskResult"));
        CQMoietiesTaskResult->resize(414, 316);
        verticalLayout = new QVBoxLayout(CQMoietiesTaskResult);
        verticalLayout->setSpacing(6);
        verticalLayout->setContentsMargins(11, 11, 11, 11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        mpLblResult = new QLabel(CQMoietiesTaskResult);
        mpLblResult->setObjectName(QString::fromUtf8("mpLblResult"));
        QSizePolicy sizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(mpLblResult->sizePolicy().hasHeightForWidth());
        mpLblResult->setSizePolicy(sizePolicy);
        mpLblResult->setWordWrap(false);

        horizontalLayout->addWidget(mpLblResult);

        mpBtnSave = new QPushButton(CQMoietiesTaskResult);
        mpBtnSave->setObjectName(QString::fromUtf8("mpBtnSave"));

        horizontalLayout->addWidget(mpBtnSave);


        verticalLayout->addLayout(horizontalLayout);

        verticalSpacer = new QSpacerItem(20, 10, QSizePolicy::Minimum, QSizePolicy::Fixed);

        verticalLayout->addItem(verticalSpacer);

        mpTabWidget = new QTabWidget(CQMoietiesTaskResult);
        mpTabWidget->setObjectName(QString::fromUtf8("mpTabWidget"));
        QSizePolicy sizePolicy1(QSizePolicy::Ignored, QSizePolicy::Preferred);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(mpTabWidget->sizePolicy().hasHeightForWidth());
        mpTabWidget->setSizePolicy(sizePolicy1);
        mpTabWidget->setMinimumSize(QSize(210, 180));
        mpMoieties = new Q3Table();
        mpMoieties->setObjectName(QString::fromUtf8("mpMoieties"));
        mpTabWidget->addTab(mpMoieties, QString());
        mpStoichiometry = new CQArrayAnnotationsWidget();
        mpStoichiometry->setObjectName(QString::fromUtf8("mpStoichiometry"));
        mpTabWidget->addTab(mpStoichiometry, QString());
        mpLinkMatrix = new CQArrayAnnotationsWidget();
        mpLinkMatrix->setObjectName(QString::fromUtf8("mpLinkMatrix"));
        mpTabWidget->addTab(mpLinkMatrix, QString());
        mpReducedStoichiometry = new CQArrayAnnotationsWidget();
        mpReducedStoichiometry->setObjectName(QString::fromUtf8("mpReducedStoichiometry"));
        mpTabWidget->addTab(mpReducedStoichiometry, QString());

        verticalLayout->addWidget(mpTabWidget);


        retranslateUi(CQMoietiesTaskResult);
        QObject::connect(mpBtnSave, SIGNAL(clicked()), CQMoietiesTaskResult, SLOT(slotSave()));

        mpTabWidget->setCurrentIndex(0);


        QMetaObject::connectSlotsByName(CQMoietiesTaskResult);
    } // setupUi

    void retranslateUi(CopasiWidget *CQMoietiesTaskResult)
    {
        CQMoietiesTaskResult->setWindowTitle(QApplication::translate("CQMoietiesTaskResult", "Mass Conservation Result Window", 0, QApplication::UnicodeUTF8));
        CQMoietiesTaskResult->setProperty("caption", QVariant(QApplication::translate("CQMoietiesTaskResult", "Mass Conservation Result", 0, QApplication::UnicodeUTF8)));
        mpLblResult->setText(QApplication::translate("CQMoietiesTaskResult", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:'Lucida Grande'; font-size:13pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:16px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-size:x-large; font-weight:600;\">Mass Conservation Result</span></p></body></html>", 0, QApplication::UnicodeUTF8));
        mpBtnSave->setText(QApplication::translate("CQMoietiesTaskResult", "Save to File", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpMoieties), QApplication::translate("CQMoietiesTaskResult", "Moieties", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpStoichiometry), QApplication::translate("CQMoietiesTaskResult", "Stoichiometry", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpLinkMatrix), QApplication::translate("CQMoietiesTaskResult", "Link Matrix", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpReducedStoichiometry), QApplication::translate("CQMoietiesTaskResult", "Reduced Stoichiometry", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQMoietiesTaskResult: public Ui_CQMoietiesTaskResult {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQMOIETIESTASKRESULT_H
