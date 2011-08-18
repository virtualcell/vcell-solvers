/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQEFMResultWidget.ui'
**
** Created: Thu Aug 18 12:47:32 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQEFMRESULTWIDGET_H
#define UI_CQEFMRESULTWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QTabWidget>
#include <QtGui/QTableView>
#include <QtGui/QVBoxLayout>
#include "CQEFMListWidget.h"
#include "copasiWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQEFMResultWidget
{
public:
    QVBoxLayout *verticalLayout;
    QHBoxLayout *horizontalLayout;
    QLabel *mpLblResult;
    QPushButton *mpBtnSave;
    QHBoxLayout *horizontalLayout_2;
    QLabel *mpLblFluxModes;
    QLineEdit *mpEditFluxModes;
    QSpacerItem *mpSpacer;
    QSpacerItem *verticalSpacer;
    QTabWidget *mpTabWidget;
    CQEFMListWidget *mpEFMListWidget;
    QTableView *mpNetReactions;
    QTableView *mpReactionMatrix;
    QTableView *mpSpeciesMatrix;

    void setupUi(CopasiWidget *CQEFMResultWidget)
    {
        if (CQEFMResultWidget->objectName().isEmpty())
            CQEFMResultWidget->setObjectName(QString::fromUtf8("CQEFMResultWidget"));
        CQEFMResultWidget->resize(408, 430);
        verticalLayout = new QVBoxLayout(CQEFMResultWidget);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        mpLblResult = new QLabel(CQEFMResultWidget);
        mpLblResult->setObjectName(QString::fromUtf8("mpLblResult"));
        QSizePolicy sizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(mpLblResult->sizePolicy().hasHeightForWidth());
        mpLblResult->setSizePolicy(sizePolicy);

        horizontalLayout->addWidget(mpLblResult);

        mpBtnSave = new QPushButton(CQEFMResultWidget);
        mpBtnSave->setObjectName(QString::fromUtf8("mpBtnSave"));

        horizontalLayout->addWidget(mpBtnSave);


        verticalLayout->addLayout(horizontalLayout);

        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        mpLblFluxModes = new QLabel(CQEFMResultWidget);
        mpLblFluxModes->setObjectName(QString::fromUtf8("mpLblFluxModes"));
        mpLblFluxModes->setWordWrap(false);

        horizontalLayout_2->addWidget(mpLblFluxModes);

        mpEditFluxModes = new QLineEdit(CQEFMResultWidget);
        mpEditFluxModes->setObjectName(QString::fromUtf8("mpEditFluxModes"));
        mpEditFluxModes->setReadOnly(true);

        horizontalLayout_2->addWidget(mpEditFluxModes);

        mpSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(mpSpacer);


        verticalLayout->addLayout(horizontalLayout_2);

        verticalSpacer = new QSpacerItem(20, 5, QSizePolicy::Minimum, QSizePolicy::Fixed);

        verticalLayout->addItem(verticalSpacer);

        mpTabWidget = new QTabWidget(CQEFMResultWidget);
        mpTabWidget->setObjectName(QString::fromUtf8("mpTabWidget"));
        mpEFMListWidget = new CQEFMListWidget();
        mpEFMListWidget->setObjectName(QString::fromUtf8("mpEFMListWidget"));
        mpTabWidget->addTab(mpEFMListWidget, QString());
        mpNetReactions = new QTableView();
        mpNetReactions->setObjectName(QString::fromUtf8("mpNetReactions"));
        mpTabWidget->addTab(mpNetReactions, QString());
        mpReactionMatrix = new QTableView();
        mpReactionMatrix->setObjectName(QString::fromUtf8("mpReactionMatrix"));
        mpTabWidget->addTab(mpReactionMatrix, QString());
        mpSpeciesMatrix = new QTableView();
        mpSpeciesMatrix->setObjectName(QString::fromUtf8("mpSpeciesMatrix"));
        mpTabWidget->addTab(mpSpeciesMatrix, QString());

        verticalLayout->addWidget(mpTabWidget);

        QWidget::setTabOrder(mpEditFluxModes, mpBtnSave);
        QWidget::setTabOrder(mpBtnSave, mpTabWidget);

        retranslateUi(CQEFMResultWidget);
        QObject::connect(mpBtnSave, SIGNAL(clicked()), CQEFMResultWidget, SLOT(slotSave()));

        mpTabWidget->setCurrentIndex(0);


        QMetaObject::connectSlotsByName(CQEFMResultWidget);
    } // setupUi

    void retranslateUi(CopasiWidget *CQEFMResultWidget)
    {
        CQEFMResultWidget->setWindowTitle(QApplication::translate("CQEFMResultWidget", "Elementary Modes Result Window", 0, QApplication::UnicodeUTF8));
        mpLblResult->setText(QApplication::translate("CQEFMResultWidget", "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0//EN\" \"http://www.w3.org/TR/REC-html40/strict.dtd\">\n"
"<html><head><meta name=\"qrichtext\" content=\"1\" /><style type=\"text/css\">\n"
"p, li { white-space: pre-wrap; }\n"
"</style></head><body style=\" font-family:'Lucida Grande'; font-size:13pt; font-weight:400; font-style:normal;\">\n"
"<p style=\" margin-top:16px; margin-bottom:12px; margin-left:0px; margin-right:0px; -qt-block-indent:0; text-indent:0px;\"><span style=\" font-size:x-large; font-weight:600;\">Elementary Modes Result</span></p></body></html>", 0, QApplication::UnicodeUTF8));
        mpBtnSave->setText(QApplication::translate("CQEFMResultWidget", "Save to File", 0, QApplication::UnicodeUTF8));
        mpLblFluxModes->setText(QApplication::translate("CQEFMResultWidget", "Flux Modes", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpEFMListWidget), QApplication::translate("CQEFMResultWidget", "List", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpNetReactions), QApplication::translate("CQEFMResultWidget", "Net Reactions", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpReactionMatrix), QApplication::translate("CQEFMResultWidget", "EFM vs Reactions", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpSpeciesMatrix), QApplication::translate("CQEFMResultWidget", "EFM vs Species", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQEFMResultWidget: public Ui_CQEFMResultWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQEFMRESULTWIDGET_H
