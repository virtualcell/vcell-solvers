/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQExpressionMmlStackedWidget.ui'
**
** Created: Thu Aug 18 12:47:31 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQEXPRESSIONMMLSTACKEDWIDGET_H
#define UI_CQEXPRESSIONMMLSTACKEDWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QSpacerItem>
#include <QtGui/QStackedWidget>
#include <QtGui/QToolButton>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>
#include "CQExpressionWidget.h"
#include "CQMmlScrollView.h"

QT_BEGIN_NAMESPACE

class Ui_CQExpressionMmlStackedWidget
{
public:
    QWidget *mpExpressionPage;
    QHBoxLayout *horizontalLayout;
    CQExpressionWidget *mpExpressionWidget;
    QVBoxLayout *verticalLayout_ExpPage;
    QToolButton *mpBtnExpressionObject;
    QToolButton *mpBtnViewExpression;
    QSpacerItem *verticalSpacer;
    QWidget *mpMmlPage;
    QHBoxLayout *horizontalLayout_2;
    CQMmlScrollView *mpMmlScrollView;
    QVBoxLayout *verticalLayout_MmlPage;
    QToolButton *mpBtnEditExpression;
    QToolButton *mpBtnSaveExpression;
    QSpacerItem *mpSpacerMmlObject;

    void setupUi(QStackedWidget *CQExpressionMmlStackedWidget)
    {
        if (CQExpressionMmlStackedWidget->objectName().isEmpty())
            CQExpressionMmlStackedWidget->setObjectName(QString::fromUtf8("CQExpressionMmlStackedWidget"));
        CQExpressionMmlStackedWidget->resize(400, 300);
        mpExpressionPage = new QWidget();
        mpExpressionPage->setObjectName(QString::fromUtf8("mpExpressionPage"));
        horizontalLayout = new QHBoxLayout(mpExpressionPage);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        mpExpressionWidget = new CQExpressionWidget(mpExpressionPage);
        mpExpressionWidget->setObjectName(QString::fromUtf8("mpExpressionWidget"));

        horizontalLayout->addWidget(mpExpressionWidget);

        verticalLayout_ExpPage = new QVBoxLayout();
        verticalLayout_ExpPage->setObjectName(QString::fromUtf8("verticalLayout_ExpPage"));
        mpBtnExpressionObject = new QToolButton(mpExpressionPage);
        mpBtnExpressionObject->setObjectName(QString::fromUtf8("mpBtnExpressionObject"));
        mpBtnExpressionObject->setMaximumSize(QSize(20, 20));

        verticalLayout_ExpPage->addWidget(mpBtnExpressionObject);

        mpBtnViewExpression = new QToolButton(mpExpressionPage);
        mpBtnViewExpression->setObjectName(QString::fromUtf8("mpBtnViewExpression"));
        mpBtnViewExpression->setMaximumSize(QSize(20, 20));

        verticalLayout_ExpPage->addWidget(mpBtnViewExpression);

        verticalSpacer = new QSpacerItem(20, 40, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_ExpPage->addItem(verticalSpacer);


        horizontalLayout->addLayout(verticalLayout_ExpPage);

        CQExpressionMmlStackedWidget->addWidget(mpExpressionPage);
        mpMmlPage = new QWidget();
        mpMmlPage->setObjectName(QString::fromUtf8("mpMmlPage"));
        horizontalLayout_2 = new QHBoxLayout(mpMmlPage);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        mpMmlScrollView = new CQMmlScrollView(mpMmlPage);
        mpMmlScrollView->setObjectName(QString::fromUtf8("mpMmlScrollView"));
        QSizePolicy sizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(mpMmlScrollView->sizePolicy().hasHeightForWidth());
        mpMmlScrollView->setSizePolicy(sizePolicy);
        mpMmlScrollView->setFrameShape(QFrame::StyledPanel);
        mpMmlScrollView->setFrameShadow(QFrame::Raised);

        horizontalLayout_2->addWidget(mpMmlScrollView);

        verticalLayout_MmlPage = new QVBoxLayout();
        verticalLayout_MmlPage->setObjectName(QString::fromUtf8("verticalLayout_MmlPage"));
        mpBtnEditExpression = new QToolButton(mpMmlPage);
        mpBtnEditExpression->setObjectName(QString::fromUtf8("mpBtnEditExpression"));
        mpBtnEditExpression->setMaximumSize(QSize(20, 20));

        verticalLayout_MmlPage->addWidget(mpBtnEditExpression);

        mpBtnSaveExpression = new QToolButton(mpMmlPage);
        mpBtnSaveExpression->setObjectName(QString::fromUtf8("mpBtnSaveExpression"));
        mpBtnSaveExpression->setMaximumSize(QSize(20, 20));

        verticalLayout_MmlPage->addWidget(mpBtnSaveExpression);

        mpSpacerMmlObject = new QSpacerItem(20, 40, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_MmlPage->addItem(mpSpacerMmlObject);


        horizontalLayout_2->addLayout(verticalLayout_MmlPage);

        CQExpressionMmlStackedWidget->addWidget(mpMmlPage);

        retranslateUi(CQExpressionMmlStackedWidget);
        QObject::connect(mpBtnEditExpression, SIGNAL(clicked()), CQExpressionMmlStackedWidget, SLOT(slotGoExpressionWidget()));
        QObject::connect(mpBtnSaveExpression, SIGNAL(clicked()), CQExpressionMmlStackedWidget, SLOT(slotSaveExpression()));
        QObject::connect(mpBtnExpressionObject, SIGNAL(clicked()), mpExpressionWidget, SLOT(slotSelectObject()));
        QObject::connect(mpBtnViewExpression, SIGNAL(clicked()), CQExpressionMmlStackedWidget, SLOT(updateWidget()));
        QObject::connect(mpExpressionWidget, SIGNAL(valid(bool)), mpBtnViewExpression, SLOT(setEnabled(bool)));

        CQExpressionMmlStackedWidget->setCurrentIndex(0);


        QMetaObject::connectSlotsByName(CQExpressionMmlStackedWidget);
    } // setupUi

    void retranslateUi(QStackedWidget *CQExpressionMmlStackedWidget)
    {
        CQExpressionMmlStackedWidget->setWindowTitle(QApplication::translate("CQExpressionMmlStackedWidget", "Expression-Mml Widget", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_TOOLTIP
        mpBtnExpressionObject->setToolTip(QApplication::translate("CQExpressionMmlStackedWidget", "select object", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        mpBtnViewExpression->setToolTip(QApplication::translate("CQExpressionMmlStackedWidget", "view expression", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        mpBtnEditExpression->setToolTip(QApplication::translate("CQExpressionMmlStackedWidget", "edit expression", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        mpBtnSaveExpression->setToolTip(QApplication::translate("CQExpressionMmlStackedWidget", "save expression", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
    } // retranslateUi

};

namespace Ui {
    class CQExpressionMmlStackedWidget: public Ui_CQExpressionMmlStackedWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQEXPRESSIONMMLSTACKEDWIDGET_H
