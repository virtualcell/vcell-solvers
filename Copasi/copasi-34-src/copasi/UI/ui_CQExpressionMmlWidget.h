/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQExpressionMmlWidget.ui'
**
** Created: Sun Sep 11 10:59:25 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQEXPRESSIONMMLWIDGET_H
#define UI_CQEXPRESSIONMMLWIDGET_H

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

class Ui_CQExpressionMmlWidget
{
public:
    QVBoxLayout *verticalLayout;
    QStackedWidget *mpWidgetStackExpressionMml;
    QWidget *mpExpressionPage;
    QHBoxLayout *horizontalLayout_2;
    CQExpressionWidget *mpExpressionWidget;
    QVBoxLayout *verticalLayout_ExpPage;
    QToolButton *mpBtnExpressionObject;
    QToolButton *mpBtnViewExpression;
    QSpacerItem *mpSpacerExpressionObject;
    QWidget *mpMmlPage;
    QHBoxLayout *horizontalLayout;
    CQMmlScrollView *mpMmlScrollView;
    QVBoxLayout *verticalLayout_MmlPage;
    QToolButton *mpBtnEditExpression;
    QToolButton *mpBtnSaveExpression;
    QSpacerItem *mpSpacerMmlObject;

    void setupUi(QWidget *CQExpressionMmlWidget)
    {
        if (CQExpressionMmlWidget->objectName().isEmpty())
            CQExpressionMmlWidget->setObjectName(QString::fromUtf8("CQExpressionMmlWidget"));
        CQExpressionMmlWidget->resize(374, 261);
        verticalLayout = new QVBoxLayout(CQExpressionMmlWidget);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        verticalLayout->setContentsMargins(0, 0, 12, 0);
        mpWidgetStackExpressionMml = new QStackedWidget(CQExpressionMmlWidget);
        mpWidgetStackExpressionMml->setObjectName(QString::fromUtf8("mpWidgetStackExpressionMml"));
        mpExpressionPage = new QWidget();
        mpExpressionPage->setObjectName(QString::fromUtf8("mpExpressionPage"));
        horizontalLayout_2 = new QHBoxLayout(mpExpressionPage);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        mpExpressionWidget = new CQExpressionWidget(mpExpressionPage);
        mpExpressionWidget->setObjectName(QString::fromUtf8("mpExpressionWidget"));

        horizontalLayout_2->addWidget(mpExpressionWidget);

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

        mpSpacerExpressionObject = new QSpacerItem(20, 20, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_ExpPage->addItem(mpSpacerExpressionObject);


        horizontalLayout_2->addLayout(verticalLayout_ExpPage);

        mpWidgetStackExpressionMml->addWidget(mpExpressionPage);
        mpMmlPage = new QWidget();
        mpMmlPage->setObjectName(QString::fromUtf8("mpMmlPage"));
        horizontalLayout = new QHBoxLayout(mpMmlPage);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        mpMmlScrollView = new CQMmlScrollView(mpMmlPage);
        mpMmlScrollView->setObjectName(QString::fromUtf8("mpMmlScrollView"));
        QSizePolicy sizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(mpMmlScrollView->sizePolicy().hasHeightForWidth());
        mpMmlScrollView->setSizePolicy(sizePolicy);
        mpMmlScrollView->setFrameShape(QFrame::StyledPanel);
        mpMmlScrollView->setFrameShadow(QFrame::Raised);

        horizontalLayout->addWidget(mpMmlScrollView);

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

        mpSpacerMmlObject = new QSpacerItem(20, 20, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout_MmlPage->addItem(mpSpacerMmlObject);


        horizontalLayout->addLayout(verticalLayout_MmlPage);

        mpWidgetStackExpressionMml->addWidget(mpMmlPage);

        verticalLayout->addWidget(mpWidgetStackExpressionMml);


        retranslateUi(CQExpressionMmlWidget);
        QObject::connect(mpBtnExpressionObject, SIGNAL(clicked()), mpExpressionWidget, SLOT(slotSelectObject()));
        QObject::connect(mpBtnViewExpression, SIGNAL(clicked()), CQExpressionMmlWidget, SLOT(updateWidget()));
        QObject::connect(mpExpressionWidget, SIGNAL(valid(bool)), mpBtnViewExpression, SLOT(setEnabled(bool)));
        QObject::connect(mpBtnEditExpression, SIGNAL(clicked()), CQExpressionMmlWidget, SLOT(slotGoExpressionWidget()));
        QObject::connect(mpBtnSaveExpression, SIGNAL(clicked()), CQExpressionMmlWidget, SLOT(slotSaveExpression()));

        mpWidgetStackExpressionMml->setCurrentIndex(1);


        QMetaObject::connectSlotsByName(CQExpressionMmlWidget);
    } // setupUi

    void retranslateUi(QWidget *CQExpressionMmlWidget)
    {
        CQExpressionMmlWidget->setWindowTitle(QApplication::translate("CQExpressionMmlWidget", "Expression-Mml Widget", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_TOOLTIP
        mpBtnExpressionObject->setToolTip(QApplication::translate("CQExpressionMmlWidget", "select object", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        mpBtnViewExpression->setToolTip(QApplication::translate("CQExpressionMmlWidget", "view expression", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        mpBtnEditExpression->setToolTip(QApplication::translate("CQExpressionMmlWidget", "edit expression", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_TOOLTIP
        mpBtnSaveExpression->setToolTip(QApplication::translate("CQExpressionMmlWidget", "save expression", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
    } // retranslateUi

};

namespace Ui {
    class CQExpressionMmlWidget: public Ui_CQExpressionMmlWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQEXPRESSIONMMLWIDGET_H
