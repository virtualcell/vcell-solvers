/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CMCAResultSubwidget.ui'
**
** Created: Thu Aug 18 12:47:32 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CMCARESULTSUBWIDGET_H
#define UI_CMCARESULTSUBWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QComboBox>
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

class Ui_CMCAResultSubwidget
{
public:
    QVBoxLayout *verticalLayout;
    QHBoxLayout *horizontalLayout;
    QLabel *mpLblResult;
    QPushButton *mSaveButton;
    QHBoxLayout *hboxLayout;
    QLabel *mTopLabel;
    QComboBox *mComboScale;
    QSpacerItem *verticalSpacer;
    QTabWidget *mTabWidget;
    QWidget *tab;
    QHBoxLayout *hboxLayout1;
    CQArrayAnnotationsWidget *mpArrayElasticities;
    QWidget *tab1;
    QHBoxLayout *hboxLayout2;
    CQArrayAnnotationsWidget *mpArrayFCC;
    QWidget *TabPage;
    QHBoxLayout *hboxLayout3;
    CQArrayAnnotationsWidget *mpArrayCCC;

    void setupUi(CopasiWidget *CMCAResultSubwidget)
    {
        if (CMCAResultSubwidget->objectName().isEmpty())
            CMCAResultSubwidget->setObjectName(QString::fromUtf8("CMCAResultSubwidget"));
        CMCAResultSubwidget->resize(580, 422);
        verticalLayout = new QVBoxLayout(CMCAResultSubwidget);
        verticalLayout->setSpacing(6);
        verticalLayout->setContentsMargins(11, 11, 11, 11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        mpLblResult = new QLabel(CMCAResultSubwidget);
        mpLblResult->setObjectName(QString::fromUtf8("mpLblResult"));
        QSizePolicy sizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(mpLblResult->sizePolicy().hasHeightForWidth());
        mpLblResult->setSizePolicy(sizePolicy);

        horizontalLayout->addWidget(mpLblResult);

        mSaveButton = new QPushButton(CMCAResultSubwidget);
        mSaveButton->setObjectName(QString::fromUtf8("mSaveButton"));
        QSizePolicy sizePolicy1(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(mSaveButton->sizePolicy().hasHeightForWidth());
        mSaveButton->setSizePolicy(sizePolicy1);

        horizontalLayout->addWidget(mSaveButton);


        verticalLayout->addLayout(horizontalLayout);

        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        mTopLabel = new QLabel(CMCAResultSubwidget);
        mTopLabel->setObjectName(QString::fromUtf8("mTopLabel"));
        mTopLabel->setWordWrap(false);

        hboxLayout->addWidget(mTopLabel);

        mComboScale = new QComboBox(CMCAResultSubwidget);
        mComboScale->setObjectName(QString::fromUtf8("mComboScale"));
        sizePolicy1.setHeightForWidth(mComboScale->sizePolicy().hasHeightForWidth());
        mComboScale->setSizePolicy(sizePolicy1);

        hboxLayout->addWidget(mComboScale);


        verticalLayout->addLayout(hboxLayout);

        verticalSpacer = new QSpacerItem(20, 5, QSizePolicy::Minimum, QSizePolicy::Fixed);

        verticalLayout->addItem(verticalSpacer);

        mTabWidget = new QTabWidget(CMCAResultSubwidget);
        mTabWidget->setObjectName(QString::fromUtf8("mTabWidget"));
        tab = new QWidget();
        tab->setObjectName(QString::fromUtf8("tab"));
        hboxLayout1 = new QHBoxLayout(tab);
        hboxLayout1->setSpacing(6);
        hboxLayout1->setContentsMargins(0, 0, 0, 0);
        hboxLayout1->setObjectName(QString::fromUtf8("hboxLayout1"));
        mpArrayElasticities = new CQArrayAnnotationsWidget(tab);
        mpArrayElasticities->setObjectName(QString::fromUtf8("mpArrayElasticities"));

        hboxLayout1->addWidget(mpArrayElasticities);

        mTabWidget->addTab(tab, QString());
        tab1 = new QWidget();
        tab1->setObjectName(QString::fromUtf8("tab1"));
        hboxLayout2 = new QHBoxLayout(tab1);
        hboxLayout2->setSpacing(6);
        hboxLayout2->setContentsMargins(0, 0, 0, 0);
        hboxLayout2->setObjectName(QString::fromUtf8("hboxLayout2"));
        mpArrayFCC = new CQArrayAnnotationsWidget(tab1);
        mpArrayFCC->setObjectName(QString::fromUtf8("mpArrayFCC"));

        hboxLayout2->addWidget(mpArrayFCC);

        mTabWidget->addTab(tab1, QString());
        TabPage = new QWidget();
        TabPage->setObjectName(QString::fromUtf8("TabPage"));
        hboxLayout3 = new QHBoxLayout(TabPage);
        hboxLayout3->setSpacing(6);
        hboxLayout3->setContentsMargins(0, 0, 0, 0);
        hboxLayout3->setObjectName(QString::fromUtf8("hboxLayout3"));
        mpArrayCCC = new CQArrayAnnotationsWidget(TabPage);
        mpArrayCCC->setObjectName(QString::fromUtf8("mpArrayCCC"));

        hboxLayout3->addWidget(mpArrayCCC);

        mTabWidget->addTab(TabPage, QString());

        verticalLayout->addWidget(mTabWidget);


        retranslateUi(CMCAResultSubwidget);
        QObject::connect(mComboScale, SIGNAL(activated(int)), CMCAResultSubwidget, SLOT(slotScaled()));
        QObject::connect(mSaveButton, SIGNAL(clicked()), CMCAResultSubwidget, SLOT(slotSave()));

        QMetaObject::connectSlotsByName(CMCAResultSubwidget);
    } // setupUi

    void retranslateUi(CopasiWidget *CMCAResultSubwidget)
    {
        CMCAResultSubwidget->setWindowTitle(QApplication::translate("CMCAResultSubwidget", "MCA Result Window", 0, QApplication::UnicodeUTF8));
        mpLblResult->setText(QApplication::translate("CMCAResultSubwidget", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:'Lucida Grande'; font-size:13pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:16px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-size:x-large; font-weight:600;\">Metabolic Control Analysis Result</span></p></body></html>", 0, QApplication::UnicodeUTF8));
        mSaveButton->setText(QApplication::translate("CMCAResultSubwidget", "Save to File", 0, QApplication::UnicodeUTF8));
        mTopLabel->setText(QString());
        mComboScale->clear();
        mComboScale->insertItems(0, QStringList()
         << QApplication::translate("CMCAResultSubwidget", "scaled", 0, QApplication::UnicodeUTF8)
         << QApplication::translate("CMCAResultSubwidget", "unscaled", 0, QApplication::UnicodeUTF8)
        );
        mTabWidget->setTabText(mTabWidget->indexOf(tab), QApplication::translate("CMCAResultSubwidget", "Elasticities", 0, QApplication::UnicodeUTF8));
        mTabWidget->setTabText(mTabWidget->indexOf(tab1), QApplication::translate("CMCAResultSubwidget", "Flux Control Coefficients", 0, QApplication::UnicodeUTF8));
        mTabWidget->setTabText(mTabWidget->indexOf(TabPage), QApplication::translate("CMCAResultSubwidget", "Concentration Control Coefficients", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CMCAResultSubwidget: public Ui_CMCAResultSubwidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CMCARESULTSUBWIDGET_H
