/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'TimeSeriesSubwidget.ui'
**
** Created: Thu Aug 18 12:47:29 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_TIMESERIESSUBWIDGET_H
#define UI_TIMESERIESSUBWIDGET_H

#include <Qt3Support/Q3Frame>
#include <Qt3Support/Q3TextEdit>
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
#include <QtGui/QWidget>
#include "CTimeSeriesTable.h"
#include "copasi.h"
#include "copasiWidget.h"

QT_BEGIN_NAMESPACE

class Ui_TimeSeriesSubWidget
{
public:
    QVBoxLayout *verticalLayout;
    QHBoxLayout *horizontalLayout;
    QLabel *mpLblResult;
    QPushButton *ButtonSaveData;
    QTabWidget *tabWidget2;
    QWidget *tab;
    QVBoxLayout *vboxLayout;
    Q3TextEdit *optimizationResultText;
    QWidget *tab1;
    QVBoxLayout *vboxLayout1;
    CTimeSeriesTable *dataTable;

    void setupUi(CopasiWidget *TimeSeriesSubWidget)
    {
        if (TimeSeriesSubWidget->objectName().isEmpty())
            TimeSeriesSubWidget->setObjectName(QString::fromUtf8("TimeSeriesSubWidget"));
        TimeSeriesSubWidget->resize(600, 382);
        QSizePolicy sizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(TimeSeriesSubWidget->sizePolicy().hasHeightForWidth());
        TimeSeriesSubWidget->setSizePolicy(sizePolicy);
        verticalLayout = new QVBoxLayout(TimeSeriesSubWidget);
        verticalLayout->setSpacing(6);
        verticalLayout->setContentsMargins(11, 11, 11, 11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        mpLblResult = new QLabel(TimeSeriesSubWidget);
        mpLblResult->setObjectName(QString::fromUtf8("mpLblResult"));
        QSizePolicy sizePolicy1(QSizePolicy::MinimumExpanding, QSizePolicy::Preferred);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(mpLblResult->sizePolicy().hasHeightForWidth());
        mpLblResult->setSizePolicy(sizePolicy1);

        horizontalLayout->addWidget(mpLblResult);

        ButtonSaveData = new QPushButton(TimeSeriesSubWidget);
        ButtonSaveData->setObjectName(QString::fromUtf8("ButtonSaveData"));
        QSizePolicy sizePolicy2(QSizePolicy::Minimum, QSizePolicy::Fixed);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(ButtonSaveData->sizePolicy().hasHeightForWidth());
        ButtonSaveData->setSizePolicy(sizePolicy2);

        horizontalLayout->addWidget(ButtonSaveData);


        verticalLayout->addLayout(horizontalLayout);

        tabWidget2 = new QTabWidget(TimeSeriesSubWidget);
        tabWidget2->setObjectName(QString::fromUtf8("tabWidget2"));
        tab = new QWidget();
        tab->setObjectName(QString::fromUtf8("tab"));
        vboxLayout = new QVBoxLayout(tab);
        vboxLayout->setSpacing(6);
        vboxLayout->setContentsMargins(11, 11, 11, 11);
        vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
        optimizationResultText = new Q3TextEdit(tab);
        optimizationResultText->setObjectName(QString::fromUtf8("optimizationResultText"));
        optimizationResultText->setReadOnly(true);

        vboxLayout->addWidget(optimizationResultText);

        tabWidget2->addTab(tab, QString());
        tab1 = new QWidget();
        tab1->setObjectName(QString::fromUtf8("tab1"));
        vboxLayout1 = new QVBoxLayout(tab1);
        vboxLayout1->setSpacing(6);
        vboxLayout1->setContentsMargins(11, 11, 11, 11);
        vboxLayout1->setObjectName(QString::fromUtf8("vboxLayout1"));
        dataTable = new CTimeSeriesTable(tab1);
        dataTable->setObjectName(QString::fromUtf8("dataTable"));

        vboxLayout1->addWidget(dataTable);

        tabWidget2->addTab(tab1, QString());

        verticalLayout->addWidget(tabWidget2);


        retranslateUi(TimeSeriesSubWidget);
        QObject::connect(ButtonSaveData, SIGNAL(clicked()), TimeSeriesSubWidget, SLOT(saveDataToFile()));

        QMetaObject::connectSlotsByName(TimeSeriesSubWidget);
    } // setupUi

    void retranslateUi(CopasiWidget *TimeSeriesSubWidget)
    {
        TimeSeriesSubWidget->setWindowTitle(QApplication::translate("TimeSeriesSubWidget", "Time Course Result Window", 0, QApplication::UnicodeUTF8));
        mpLblResult->setText(QApplication::translate("TimeSeriesSubWidget", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:'Lucida Grande'; font-size:13pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:16px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-size:x-large; font-weight:600;\">Time Course Result</span></p></body></html>", 0, QApplication::UnicodeUTF8));
        ButtonSaveData->setText(QApplication::translate("TimeSeriesSubWidget", "Save to File", 0, QApplication::UnicodeUTF8));
        tabWidget2->setTabText(tabWidget2->indexOf(tab), QApplication::translate("TimeSeriesSubWidget", "OptimizationResult", 0, QApplication::UnicodeUTF8));
        tabWidget2->setTabText(tabWidget2->indexOf(tab1), QApplication::translate("TimeSeriesSubWidget", "TimeSeries", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class TimeSeriesSubWidget: public Ui_TimeSeriesSubWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_TIMESERIESSUBWIDGET_H
