/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQSensResultWidget.ui'
**
** Created: Thu Aug 18 12:47:30 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQSENSRESULTWIDGET_H
#define UI_CQSENSRESULTWIDGET_H

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
#include <QtGui/QWidget>
#include "CQArrayAnnotationsWidget.h"
#include "copasiWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQSensResultWidget
{
public:
    QVBoxLayout *mWidgetLayout;
    QHBoxLayout *horizontalLayout;
    QLabel *mpLblResult;
    QPushButton *mSaveButton;
    QSpacerItem *verticalSpacer;
    QTabWidget *mpTab;
    QWidget *tab;
    QHBoxLayout *hboxLayout;
    CQArrayAnnotationsWidget *mArrayWidget;
    QWidget *tab_2;
    QHBoxLayout *hboxLayout1;
    CQArrayAnnotationsWidget *mArrayWidgetScaled;
    QWidget *tab_3;
    QHBoxLayout *hboxLayout2;
    CQArrayAnnotationsWidget *mArrayWidgetCollapsed;

    void setupUi(CopasiWidget *CQSensResultWidget)
    {
        if (CQSensResultWidget->objectName().isEmpty())
            CQSensResultWidget->setObjectName(QString::fromUtf8("CQSensResultWidget"));
        CQSensResultWidget->resize(571, 276);
        mWidgetLayout = new QVBoxLayout(CQSensResultWidget);
        mWidgetLayout->setObjectName(QString::fromUtf8("mWidgetLayout"));
        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        mpLblResult = new QLabel(CQSensResultWidget);
        mpLblResult->setObjectName(QString::fromUtf8("mpLblResult"));
        QSizePolicy sizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(mpLblResult->sizePolicy().hasHeightForWidth());
        mpLblResult->setSizePolicy(sizePolicy);

        horizontalLayout->addWidget(mpLblResult);

        mSaveButton = new QPushButton(CQSensResultWidget);
        mSaveButton->setObjectName(QString::fromUtf8("mSaveButton"));
        QSizePolicy sizePolicy1(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(mSaveButton->sizePolicy().hasHeightForWidth());
        mSaveButton->setSizePolicy(sizePolicy1);

        horizontalLayout->addWidget(mSaveButton);


        mWidgetLayout->addLayout(horizontalLayout);

        verticalSpacer = new QSpacerItem(20, 10, QSizePolicy::Minimum, QSizePolicy::Fixed);

        mWidgetLayout->addItem(verticalSpacer);

        mpTab = new QTabWidget(CQSensResultWidget);
        mpTab->setObjectName(QString::fromUtf8("mpTab"));
        tab = new QWidget();
        tab->setObjectName(QString::fromUtf8("tab"));
        hboxLayout = new QHBoxLayout(tab);
        hboxLayout->setContentsMargins(0, 0, 0, 0);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        mArrayWidget = new CQArrayAnnotationsWidget(tab);
        mArrayWidget->setObjectName(QString::fromUtf8("mArrayWidget"));

        hboxLayout->addWidget(mArrayWidget);

        mpTab->addTab(tab, QString());
        tab_2 = new QWidget();
        tab_2->setObjectName(QString::fromUtf8("tab_2"));
        hboxLayout1 = new QHBoxLayout(tab_2);
        hboxLayout1->setContentsMargins(0, 0, 0, 0);
        hboxLayout1->setObjectName(QString::fromUtf8("hboxLayout1"));
        mArrayWidgetScaled = new CQArrayAnnotationsWidget(tab_2);
        mArrayWidgetScaled->setObjectName(QString::fromUtf8("mArrayWidgetScaled"));

        hboxLayout1->addWidget(mArrayWidgetScaled);

        mpTab->addTab(tab_2, QString());
        tab_3 = new QWidget();
        tab_3->setObjectName(QString::fromUtf8("tab_3"));
        hboxLayout2 = new QHBoxLayout(tab_3);
        hboxLayout2->setContentsMargins(0, 0, 0, 0);
        hboxLayout2->setObjectName(QString::fromUtf8("hboxLayout2"));
        mArrayWidgetCollapsed = new CQArrayAnnotationsWidget(tab_3);
        mArrayWidgetCollapsed->setObjectName(QString::fromUtf8("mArrayWidgetCollapsed"));

        hboxLayout2->addWidget(mArrayWidgetCollapsed);

        mpTab->addTab(tab_3, QString());

        mWidgetLayout->addWidget(mpTab);


        retranslateUi(CQSensResultWidget);
        QObject::connect(mSaveButton, SIGNAL(clicked()), CQSensResultWidget, SLOT(saveToFile()));

        mpTab->setCurrentIndex(0);


        QMetaObject::connectSlotsByName(CQSensResultWidget);
    } // setupUi

    void retranslateUi(CopasiWidget *CQSensResultWidget)
    {
        CQSensResultWidget->setWindowTitle(QApplication::translate("CQSensResultWidget", "Sensitivities Result Window", 0, QApplication::UnicodeUTF8));
        mpLblResult->setText(QApplication::translate("CQSensResultWidget", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:'Lucida Grande'; font-size:13pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:16px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-size:x-large; font-weight:600;\">Sensitivities Result</span></p></body></html>", 0, QApplication::UnicodeUTF8));
        mSaveButton->setText(QApplication::translate("CQSensResultWidget", "Save to File", 0, QApplication::UnicodeUTF8));
        mpTab->setTabText(mpTab->indexOf(tab), QApplication::translate("CQSensResultWidget", "unscaled", 0, QApplication::UnicodeUTF8));
        mpTab->setTabText(mpTab->indexOf(tab_2), QApplication::translate("CQSensResultWidget", "scaled", 0, QApplication::UnicodeUTF8));
        mpTab->setTabText(mpTab->indexOf(tab_3), QApplication::translate("CQSensResultWidget", "summarized", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQSensResultWidget: public Ui_CQSensResultWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQSENSRESULTWIDGET_H
