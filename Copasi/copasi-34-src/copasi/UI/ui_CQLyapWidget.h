/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQLyapWidget.ui'
**
** Created: Sun Sep 11 10:59:24 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQLYAPWIDGET_H
#define UI_CQLYAPWIDGET_H

#include <Qt3Support/Q3MimeSourceFactory>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QVBoxLayout>
#include "TaskWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQLyapWidget
{
public:
    QVBoxLayout *vboxLayout;
    QGridLayout *gridLayout;
    QHBoxLayout *hboxLayout;
    QCheckBox *mpCheckDelay;
    QLineEdit *mpEditDelay;
    QLineEdit *mpEditExponent;
    QCheckBox *mpCheckDivergence;
    QLabel *mpLblExponents;
    QFrame *mpLine;

    void setupUi(TaskWidget *CQLyapWidget)
    {
        if (CQLyapWidget->objectName().isEmpty())
            CQLyapWidget->setObjectName(QString::fromUtf8("CQLyapWidget"));
        CQLyapWidget->resize(374, 127);
        vboxLayout = new QVBoxLayout(CQLyapWidget);
        vboxLayout->setSpacing(6);
        vboxLayout->setContentsMargins(11, 11, 11, 11);
        vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
        gridLayout = new QGridLayout();
        gridLayout->setSpacing(6);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(0);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        mpCheckDelay = new QCheckBox(CQLyapWidget);
        mpCheckDelay->setObjectName(QString::fromUtf8("mpCheckDelay"));

        hboxLayout->addWidget(mpCheckDelay);

        mpEditDelay = new QLineEdit(CQLyapWidget);
        mpEditDelay->setObjectName(QString::fromUtf8("mpEditDelay"));

        hboxLayout->addWidget(mpEditDelay);


        gridLayout->addLayout(hboxLayout, 1, 1, 1, 1);

        mpEditExponent = new QLineEdit(CQLyapWidget);
        mpEditExponent->setObjectName(QString::fromUtf8("mpEditExponent"));

        gridLayout->addWidget(mpEditExponent, 0, 1, 1, 1);

        mpCheckDivergence = new QCheckBox(CQLyapWidget);
        mpCheckDivergence->setObjectName(QString::fromUtf8("mpCheckDivergence"));

        gridLayout->addWidget(mpCheckDivergence, 2, 1, 1, 1);

        mpLblExponents = new QLabel(CQLyapWidget);
        mpLblExponents->setObjectName(QString::fromUtf8("mpLblExponents"));
        mpLblExponents->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblExponents->setWordWrap(false);

        gridLayout->addWidget(mpLblExponents, 0, 0, 1, 1);


        vboxLayout->addLayout(gridLayout);

        mpLine = new QFrame(CQLyapWidget);
        mpLine->setObjectName(QString::fromUtf8("mpLine"));
        mpLine->setFrameShape(QFrame::HLine);
        mpLine->setFrameShadow(QFrame::Sunken);

        vboxLayout->addWidget(mpLine);

        QWidget::setTabOrder(mpEditExponent, mpCheckDelay);
        QWidget::setTabOrder(mpCheckDelay, mpEditDelay);
        QWidget::setTabOrder(mpEditDelay, mpCheckDivergence);

        retranslateUi(CQLyapWidget);
        QObject::connect(mpCheckDelay, SIGNAL(clicked()), CQLyapWidget, SLOT(slotDelayChecked()));

        QMetaObject::connectSlotsByName(CQLyapWidget);
    } // setupUi

    void retranslateUi(TaskWidget *CQLyapWidget)
    {
        CQLyapWidget->setProperty("caption", QVariant(QApplication::translate("CQLyapWidget", "CQLyapWidget", 0, QApplication::UnicodeUTF8)));
        mpCheckDelay->setText(QApplication::translate("CQLyapWidget", "start averaging after t =", 0, QApplication::UnicodeUTF8));
        mpCheckDivergence->setText(QApplication::translate("CQLyapWidget", "calculate Divergence", 0, QApplication::UnicodeUTF8));
        mpLblExponents->setText(QApplication::translate("CQLyapWidget", "Number of Exponents", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQLyapWidget: public Ui_CQLyapWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQLYAPWIDGET_H
