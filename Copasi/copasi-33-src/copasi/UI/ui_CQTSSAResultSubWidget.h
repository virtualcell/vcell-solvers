/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQTSSAResultSubWidget.ui'
**
** Created: Thu Aug 18 12:47:30 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQTSSARESULTSUBWIDGET_H
#define UI_CQTSSARESULTSUBWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QComboBox>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include <QtGui/QRadioButton>
#include <QtGui/QSlider>
#include <QtGui/QSpacerItem>
#include <QtGui/QVBoxLayout>
#include "CQArrayAnnotationsWidget.h"
#include "CQTSSATimeScaleWidget.h"
#include "CTimeSeriesTable.h"
#include "copasiWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQTSSAResultSubWidget
{
public:
    QVBoxLayout *verticalLayout;
    QLabel *mpLblResult;
    QGridLayout *gridLayout_2;
    QSpacerItem *spacer;
    QPushButton *ButtonSaveData;
    QHBoxLayout *horizontalLayout_2;
    QRadioButton *mpButton1;
    QSpacerItem *spacer_3;
    QRadioButton *mpButton2;
    QSpacerItem *spacer_4;
    QLabel *mpLabel;
    QSpacerItem *horizontalSpacer_3;
    QPushButton *mpButton;
    QSlider *mpSlider;
    CQTSSATimeScaleWidget *mpTimeScaleWidget;
    QHBoxLayout *horizontalLayout;
    QComboBox *mpBox1;
    QSpacerItem *horizontalSpacer_2;
    CQArrayAnnotationsWidget *mpArrayWidget;

    void setupUi(CopasiWidget *CQTSSAResultSubWidget)
    {
        if (CQTSSAResultSubWidget->objectName().isEmpty())
            CQTSSAResultSubWidget->setObjectName(QString::fromUtf8("CQTSSAResultSubWidget"));
        CQTSSAResultSubWidget->resize(892, 499);
        QSizePolicy sizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(CQTSSAResultSubWidget->sizePolicy().hasHeightForWidth());
        CQTSSAResultSubWidget->setSizePolicy(sizePolicy);
        verticalLayout = new QVBoxLayout(CQTSSAResultSubWidget);
        verticalLayout->setSpacing(6);
        verticalLayout->setContentsMargins(11, 11, 11, 11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        mpLblResult = new QLabel(CQTSSAResultSubWidget);
        mpLblResult->setObjectName(QString::fromUtf8("mpLblResult"));
        QSizePolicy sizePolicy1(QSizePolicy::MinimumExpanding, QSizePolicy::Preferred);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(mpLblResult->sizePolicy().hasHeightForWidth());
        mpLblResult->setSizePolicy(sizePolicy1);

        verticalLayout->addWidget(mpLblResult);

        gridLayout_2 = new QGridLayout();
        gridLayout_2->setSpacing(6);
        gridLayout_2->setObjectName(QString::fromUtf8("gridLayout_2"));
        spacer = new QSpacerItem(608, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        gridLayout_2->addItem(spacer, 0, 0, 1, 1);

        ButtonSaveData = new QPushButton(CQTSSAResultSubWidget);
        ButtonSaveData->setObjectName(QString::fromUtf8("ButtonSaveData"));
        QSizePolicy sizePolicy2(QSizePolicy::Minimum, QSizePolicy::Fixed);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(ButtonSaveData->sizePolicy().hasHeightForWidth());
        ButtonSaveData->setSizePolicy(sizePolicy2);

        gridLayout_2->addWidget(ButtonSaveData, 0, 1, 1, 1);


        verticalLayout->addLayout(gridLayout_2);

        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setSpacing(6);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        mpButton1 = new QRadioButton(CQTSSAResultSubWidget);
        mpButton1->setObjectName(QString::fromUtf8("mpButton1"));
        mpButton1->setChecked(false);

        horizontalLayout_2->addWidget(mpButton1);

        spacer_3 = new QSpacerItem(18, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(spacer_3);

        mpButton2 = new QRadioButton(CQTSSAResultSubWidget);
        mpButton2->setObjectName(QString::fromUtf8("mpButton2"));
        mpButton2->setChecked(true);

        horizontalLayout_2->addWidget(mpButton2);

        spacer_4 = new QSpacerItem(18, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(spacer_4);

        mpLabel = new QLabel(CQTSSAResultSubWidget);
        mpLabel->setObjectName(QString::fromUtf8("mpLabel"));

        horizontalLayout_2->addWidget(mpLabel);

        horizontalSpacer_3 = new QSpacerItem(458, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer_3);

        mpButton = new QPushButton(CQTSSAResultSubWidget);
        mpButton->setObjectName(QString::fromUtf8("mpButton"));
        sizePolicy2.setHeightForWidth(mpButton->sizePolicy().hasHeightForWidth());
        mpButton->setSizePolicy(sizePolicy2);

        horizontalLayout_2->addWidget(mpButton);


        verticalLayout->addLayout(horizontalLayout_2);

        mpSlider = new QSlider(CQTSSAResultSubWidget);
        mpSlider->setObjectName(QString::fromUtf8("mpSlider"));
        QSizePolicy sizePolicy3(QSizePolicy::Expanding, QSizePolicy::Minimum);
        sizePolicy3.setHorizontalStretch(0);
        sizePolicy3.setVerticalStretch(0);
        sizePolicy3.setHeightForWidth(mpSlider->sizePolicy().hasHeightForWidth());
        mpSlider->setSizePolicy(sizePolicy3);
        mpSlider->setMouseTracking(true);
        mpSlider->setAcceptDrops(true);
        mpSlider->setMaximum(0);
        mpSlider->setPageStep(1);
        mpSlider->setTracking(true);
        mpSlider->setOrientation(Qt::Horizontal);
        mpSlider->setTickPosition(QSlider::NoTicks);
        mpSlider->setTickInterval(1);

        verticalLayout->addWidget(mpSlider);

        mpTimeScaleWidget = new CQTSSATimeScaleWidget(CQTSSAResultSubWidget);
        mpTimeScaleWidget->setObjectName(QString::fromUtf8("mpTimeScaleWidget"));

        verticalLayout->addWidget(mpTimeScaleWidget);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        mpBox1 = new QComboBox(CQTSSAResultSubWidget);
        mpBox1->setObjectName(QString::fromUtf8("mpBox1"));

        horizontalLayout->addWidget(mpBox1);

        horizontalSpacer_2 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer_2);


        verticalLayout->addLayout(horizontalLayout);

        mpArrayWidget = new CQArrayAnnotationsWidget(CQTSSAResultSubWidget);
        mpArrayWidget->setObjectName(QString::fromUtf8("mpArrayWidget"));

        verticalLayout->addWidget(mpArrayWidget);


        retranslateUi(CQTSSAResultSubWidget);

        QMetaObject::connectSlotsByName(CQTSSAResultSubWidget);
    } // setupUi

    void retranslateUi(CopasiWidget *CQTSSAResultSubWidget)
    {
        CQTSSAResultSubWidget->setWindowTitle(QApplication::translate("CQTSSAResultSubWidget", "TSSA Result Window", 0, QApplication::UnicodeUTF8));
        mpLblResult->setText(QApplication::translate("CQTSSAResultSubWidget", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:'Lucida Grande'; font-size:13pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:16px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-size:x-large; font-weight:600;\">Time Scale Separation Analysis Result</span></p></body></html>", 0, QApplication::UnicodeUTF8));
        ButtonSaveData->setText(QApplication::translate("CQTSSAResultSubWidget", "Save data to file", 0, QApplication::UnicodeUTF8));
        mpButton1->setText(QApplication::translate("CQTSSAResultSubWidget", "Time", 0, QApplication::UnicodeUTF8));
        mpButton2->setText(QApplication::translate("CQTSSAResultSubWidget", "Step", 0, QApplication::UnicodeUTF8));
        mpLabel->setText(QApplication::translate("CQTSSAResultSubWidget", "1", 0, QApplication::UnicodeUTF8));
        mpButton->setText(QApplication::translate("CQTSSAResultSubWidget", "Show Time scales", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_TOOLTIP
        mpSlider->setToolTip(QApplication::translate("CQTSSAResultSubWidget", "move mouse-cursor over slider and use scrollwheel to adjust required step accurately", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        mpSlider->setWhatsThis(QApplication::translate("CQTSSAResultSubWidget", "use slider to adjust required step", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_WHATSTHIS
    } // retranslateUi

};

namespace Ui {
    class CQTSSAResultSubWidget: public Ui_CQTSSAResultSubWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQTSSARESULTSUBWIDGET_H
