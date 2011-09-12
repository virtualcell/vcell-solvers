/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQLyapResultWidget.ui'
**
** Created: Sun Sep 11 10:59:24 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQLYAPRESULTWIDGET_H
#define UI_CQLYAPRESULTWIDGET_H

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
#include <QtGui/QSpacerItem>
#include <QtGui/QTableWidget>
#include <QtGui/QVBoxLayout>
#include "copasi/UI/copasiWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQLyapResultWidget
{
public:
    QVBoxLayout *verticalLayout;
    QHBoxLayout *horizontalLayout;
    QLabel *mpLblResult;
    QPushButton *mSaveButton;
    QGridLayout *mWidgetLayout;
    QLabel *mLabelExponents;
    QTableWidget *mTableExponents;
    QLabel *mLabelSum;
    QLineEdit *mLineEditSum;
    QLabel *mLabelDivergence;
    QLineEdit *mLineEditDivergence;
    QSpacerItem *spacer;
    QLabel *mLabelComment;

    void setupUi(CopasiWidget *CQLyapResultWidget)
    {
        if (CQLyapResultWidget->objectName().isEmpty())
            CQLyapResultWidget->setObjectName(QString::fromUtf8("CQLyapResultWidget"));
        CQLyapResultWidget->resize(428, 257);
        verticalLayout = new QVBoxLayout(CQLyapResultWidget);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        mpLblResult = new QLabel(CQLyapResultWidget);
        mpLblResult->setObjectName(QString::fromUtf8("mpLblResult"));
        QSizePolicy sizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(mpLblResult->sizePolicy().hasHeightForWidth());
        mpLblResult->setSizePolicy(sizePolicy);

        horizontalLayout->addWidget(mpLblResult);

        mSaveButton = new QPushButton(CQLyapResultWidget);
        mSaveButton->setObjectName(QString::fromUtf8("mSaveButton"));
        QSizePolicy sizePolicy1(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(mSaveButton->sizePolicy().hasHeightForWidth());
        mSaveButton->setSizePolicy(sizePolicy1);

        horizontalLayout->addWidget(mSaveButton);


        verticalLayout->addLayout(horizontalLayout);

        mWidgetLayout = new QGridLayout();
        mWidgetLayout->setObjectName(QString::fromUtf8("mWidgetLayout"));
        mLabelExponents = new QLabel(CQLyapResultWidget);
        mLabelExponents->setObjectName(QString::fromUtf8("mLabelExponents"));
        mLabelExponents->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

        mWidgetLayout->addWidget(mLabelExponents, 0, 0, 1, 1);

        mTableExponents = new QTableWidget(CQLyapResultWidget);
        if (mTableExponents->columnCount() < 1)
            mTableExponents->setColumnCount(1);
        QTableWidgetItem *__qtablewidgetitem = new QTableWidgetItem();
        mTableExponents->setHorizontalHeaderItem(0, __qtablewidgetitem);
        mTableExponents->setObjectName(QString::fromUtf8("mTableExponents"));
        QSizePolicy sizePolicy2(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(mTableExponents->sizePolicy().hasHeightForWidth());
        mTableExponents->setSizePolicy(sizePolicy2);
        mTableExponents->setColumnCount(1);

        mWidgetLayout->addWidget(mTableExponents, 0, 1, 1, 1);

        mLabelSum = new QLabel(CQLyapResultWidget);
        mLabelSum->setObjectName(QString::fromUtf8("mLabelSum"));
        mLabelSum->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

        mWidgetLayout->addWidget(mLabelSum, 1, 0, 1, 1);

        mLineEditSum = new QLineEdit(CQLyapResultWidget);
        mLineEditSum->setObjectName(QString::fromUtf8("mLineEditSum"));
        mLineEditSum->setReadOnly(true);

        mWidgetLayout->addWidget(mLineEditSum, 1, 1, 1, 1);

        mLabelDivergence = new QLabel(CQLyapResultWidget);
        mLabelDivergence->setObjectName(QString::fromUtf8("mLabelDivergence"));
        mLabelDivergence->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

        mWidgetLayout->addWidget(mLabelDivergence, 2, 0, 1, 1);

        mLineEditDivergence = new QLineEdit(CQLyapResultWidget);
        mLineEditDivergence->setObjectName(QString::fromUtf8("mLineEditDivergence"));
        mLineEditDivergence->setReadOnly(true);

        mWidgetLayout->addWidget(mLineEditDivergence, 2, 1, 1, 1);

        spacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        mWidgetLayout->addItem(spacer, 3, 0, 1, 1);

        mLabelComment = new QLabel(CQLyapResultWidget);
        mLabelComment->setObjectName(QString::fromUtf8("mLabelComment"));
        mLabelComment->setWordWrap(true);

        mWidgetLayout->addWidget(mLabelComment, 3, 1, 1, 1);


        verticalLayout->addLayout(mWidgetLayout);


        retranslateUi(CQLyapResultWidget);
        QObject::connect(mSaveButton, SIGNAL(clicked()), CQLyapResultWidget, SLOT(saveToFile()));

        QMetaObject::connectSlotsByName(CQLyapResultWidget);
    } // setupUi

    void retranslateUi(CopasiWidget *CQLyapResultWidget)
    {
        CQLyapResultWidget->setWindowTitle(QApplication::translate("CQLyapResultWidget", "Lyapunov Exponents Result Window", 0, QApplication::UnicodeUTF8));
        mpLblResult->setText(QApplication::translate("CQLyapResultWidget", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:'Lucida Grande'; font-size:13pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:16px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-size:x-large; font-weight:600;\">Lyapunov Exponents Result</span></p></body></html>", 0, QApplication::UnicodeUTF8));
        mSaveButton->setText(QApplication::translate("CQLyapResultWidget", "Save to File", 0, QApplication::UnicodeUTF8));
        mLabelExponents->setText(QApplication::translate("CQLyapResultWidget", "Lyapunov Exponents", 0, QApplication::UnicodeUTF8));
        QTableWidgetItem *___qtablewidgetitem = mTableExponents->horizontalHeaderItem(0);
        ___qtablewidgetitem->setText(QApplication::translate("CQLyapResultWidget", "Exponent", 0, QApplication::UnicodeUTF8));
        mLabelSum->setText(QApplication::translate("CQLyapResultWidget", "Sum of Lyapunov Exponents", 0, QApplication::UnicodeUTF8));
        mLabelDivergence->setText(QApplication::translate("CQLyapResultWidget", "Divergence", 0, QApplication::UnicodeUTF8));
        mLabelComment->setText(QString());
    } // retranslateUi

};

namespace Ui {
    class CQLyapResultWidget: public Ui_CQLyapResultWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQLYAPRESULTWIDGET_H
