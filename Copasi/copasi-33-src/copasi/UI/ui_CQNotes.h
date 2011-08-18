/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQNotes.ui'
**
** Created: Thu Aug 18 12:47:31 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQNOTES_H
#define UI_CQNOTES_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QGridLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QPlainTextEdit>
#include <QtGui/QSpacerItem>
#include <QtGui/QToolButton>
#include <QtGui/QVBoxLayout>
#include <QtWebKit/QWebView>
#include "copasi/UI/copasiWidget.h"
#include "copasiWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQNotes
{
public:
    QGridLayout *gridLayout;
    QVBoxLayout *verticalLayout;
    QWebView *mpWebView;
    QPlainTextEdit *mpEdit;
    QToolButton *mpBtnToggleEdit;
    QSpacerItem *mpSpacer;

    void setupUi(CopasiWidget *CQNotes)
    {
        if (CQNotes->objectName().isEmpty())
            CQNotes->setObjectName(QString::fromUtf8("CQNotes"));
        CQNotes->resize(511, 205);
        gridLayout = new QGridLayout(CQNotes);
        gridLayout->setContentsMargins(3, 3, 3, 3);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        verticalLayout = new QVBoxLayout();
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        mpWebView = new QWebView(CQNotes);
        mpWebView->setObjectName(QString::fromUtf8("mpWebView"));
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(mpWebView->sizePolicy().hasHeightForWidth());
        mpWebView->setSizePolicy(sizePolicy);
        mpWebView->setUrl(QUrl("about:blank"));

        verticalLayout->addWidget(mpWebView);

        mpEdit = new QPlainTextEdit(CQNotes);
        mpEdit->setObjectName(QString::fromUtf8("mpEdit"));
        sizePolicy.setHeightForWidth(mpEdit->sizePolicy().hasHeightForWidth());
        mpEdit->setSizePolicy(sizePolicy);

        verticalLayout->addWidget(mpEdit);


        gridLayout->addLayout(verticalLayout, 0, 0, 2, 1);

        mpBtnToggleEdit = new QToolButton(CQNotes);
        mpBtnToggleEdit->setObjectName(QString::fromUtf8("mpBtnToggleEdit"));
        mpBtnToggleEdit->setAutoRaise(false);

        gridLayout->addWidget(mpBtnToggleEdit, 0, 1, 1, 1);

        mpSpacer = new QSpacerItem(20, 151, QSizePolicy::Minimum, QSizePolicy::Expanding);

        gridLayout->addItem(mpSpacer, 1, 1, 1, 1);


        retranslateUi(CQNotes);
        QObject::connect(mpBtnToggleEdit, SIGNAL(clicked()), CQNotes, SLOT(slotToggleMode()));
        QObject::connect(mpWebView, SIGNAL(linkClicked(QUrl)), CQNotes, SLOT(slotOpenUrl(QUrl)));
        QObject::connect(mpEdit, SIGNAL(textChanged()), CQNotes, SLOT(slotValidateXML()));

        QMetaObject::connectSlotsByName(CQNotes);
    } // setupUi

    void retranslateUi(CopasiWidget *CQNotes)
    {
        CQNotes->setWindowTitle(QApplication::translate("CQNotes", "Form", 0, QApplication::UnicodeUTF8));
        mpBtnToggleEdit->setText(QApplication::translate("CQNotes", "...", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQNotes: public Ui_CQNotes {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQNOTES_H
