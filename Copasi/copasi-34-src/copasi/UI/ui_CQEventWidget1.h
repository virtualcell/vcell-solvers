/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQEventWidget1.ui'
**
** Created: Sun Sep 11 10:59:25 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQEVENTWIDGET1_H
#define UI_CQEVENTWIDGET1_H

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
#include <QtGui/QListWidget>
#include <QtGui/QPushButton>
#include <QtGui/QRadioButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QSpinBox>
#include <QtGui/QToolButton>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>
#include <string>
#include <vector>
#include "CQExpressionMmlStackedWidget.h"
#include "MyLineEdit.h"
#include "copasi/UI/copasiWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQEventWidget1
{
public:
    QVBoxLayout *verticalLayout;
    QGridLayout *gridLayout;
    QLabel *mpLabelEventName;
    QLineEdit *mpLineEditName;
    QLabel *mpLabelOrder;
    QSpinBox *mpSpinOrder;
    QLabel *mpLabelTrigger;
    CQExpressionMmlStackedWidget *mpExpressionTrigger;
    QWidget *page_11;
    QWidget *page_12;
    QCheckBox *mpCheckDelay;
    QRadioButton *mpBtnDelayCalculation;
    QRadioButton *mpBtnDelayAssignment;
    QLabel *mpLabelDelayExpression;
    CQExpressionMmlStackedWidget *mpExpressionDelay;
    QWidget *page_7;
    QWidget *page_8;
    QFrame *mpLine1;
    QGridLayout *gridLayout1;
    QHBoxLayout *hboxLayout;
    QListWidget *mpLBTarget;
    QVBoxLayout *vboxLayout;
    QToolButton *mpBtnSelectObject;
    QToolButton *mpBtnAddTarget;
    QToolButton *mpBtnDeleteTarget;
    QSpacerItem *spacerTargetIcons;
    QHBoxLayout *hboxLayout1;
    QLabel *mpLabelTarget;
    QSpacerItem *spacerTarget;
    QHBoxLayout *hboxLayout2;
    QLabel *mpLabelEA;
    QSpacerItem *spacerExpression;
    CQExpressionMmlStackedWidget *mpExpressionEA;
    QWidget *page;
    QWidget *page_2;
    QFrame *mpLine2;
    QHBoxLayout *hboxLayout3;
    QPushButton *mpBtnCommit;
    QPushButton *mpBtnRevert;
    QPushButton *mpBtnNew;
    QPushButton *mpBtnDelete;

    void setupUi(CopasiWidget *CQEventWidget1)
    {
        if (CQEventWidget1->objectName().isEmpty())
            CQEventWidget1->setObjectName(QString::fromUtf8("CQEventWidget1"));
        CQEventWidget1->resize(487, 504);
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(CQEventWidget1->sizePolicy().hasHeightForWidth());
        CQEventWidget1->setSizePolicy(sizePolicy);
        verticalLayout = new QVBoxLayout(CQEventWidget1);
        verticalLayout->setSpacing(6);
        verticalLayout->setContentsMargins(11, 11, 11, 11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        gridLayout = new QGridLayout();
        gridLayout->setSpacing(6);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        mpLabelEventName = new QLabel(CQEventWidget1);
        mpLabelEventName->setObjectName(QString::fromUtf8("mpLabelEventName"));
        QSizePolicy sizePolicy1(QSizePolicy::Preferred, QSizePolicy::Minimum);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(mpLabelEventName->sizePolicy().hasHeightForWidth());
        mpLabelEventName->setSizePolicy(sizePolicy1);
        mpLabelEventName->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLabelEventName->setWordWrap(false);

        gridLayout->addWidget(mpLabelEventName, 0, 0, 1, 1);

        mpLineEditName = new QLineEdit(CQEventWidget1);
        mpLineEditName->setObjectName(QString::fromUtf8("mpLineEditName"));

        gridLayout->addWidget(mpLineEditName, 0, 1, 1, 2);

        mpLabelOrder = new QLabel(CQEventWidget1);
        mpLabelOrder->setObjectName(QString::fromUtf8("mpLabelOrder"));
        sizePolicy1.setHeightForWidth(mpLabelOrder->sizePolicy().hasHeightForWidth());
        mpLabelOrder->setSizePolicy(sizePolicy1);
        mpLabelOrder->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLabelOrder->setWordWrap(false);

        gridLayout->addWidget(mpLabelOrder, 0, 3, 1, 1);

        mpSpinOrder = new QSpinBox(CQEventWidget1);
        mpSpinOrder->setObjectName(QString::fromUtf8("mpSpinOrder"));

        gridLayout->addWidget(mpSpinOrder, 0, 4, 1, 1);

        mpLabelTrigger = new QLabel(CQEventWidget1);
        mpLabelTrigger->setObjectName(QString::fromUtf8("mpLabelTrigger"));
        sizePolicy1.setHeightForWidth(mpLabelTrigger->sizePolicy().hasHeightForWidth());
        mpLabelTrigger->setSizePolicy(sizePolicy1);
        mpLabelTrigger->setAlignment(Qt::AlignRight|Qt::AlignTop|Qt::AlignTrailing);
        mpLabelTrigger->setWordWrap(true);

        gridLayout->addWidget(mpLabelTrigger, 1, 0, 1, 1);

        mpExpressionTrigger = new CQExpressionMmlStackedWidget(CQEventWidget1);
        mpExpressionTrigger->setObjectName(QString::fromUtf8("mpExpressionTrigger"));
        mpExpressionTrigger->setMinimumSize(QSize(380, 100));
        page_11 = new QWidget();
        page_11->setObjectName(QString::fromUtf8("page_11"));
        mpExpressionTrigger->addWidget(page_11);
        page_12 = new QWidget();
        page_12->setObjectName(QString::fromUtf8("page_12"));
        mpExpressionTrigger->addWidget(page_12);

        gridLayout->addWidget(mpExpressionTrigger, 1, 1, 1, 4);

        mpCheckDelay = new QCheckBox(CQEventWidget1);
        mpCheckDelay->setObjectName(QString::fromUtf8("mpCheckDelay"));
        mpCheckDelay->setLayoutDirection(Qt::LeftToRight);

        gridLayout->addWidget(mpCheckDelay, 2, 0, 1, 1);

        mpBtnDelayCalculation = new QRadioButton(CQEventWidget1);
        mpBtnDelayCalculation->setObjectName(QString::fromUtf8("mpBtnDelayCalculation"));

        gridLayout->addWidget(mpBtnDelayCalculation, 2, 1, 1, 1);

        mpBtnDelayAssignment = new QRadioButton(CQEventWidget1);
        mpBtnDelayAssignment->setObjectName(QString::fromUtf8("mpBtnDelayAssignment"));
        mpBtnDelayAssignment->setChecked(true);

        gridLayout->addWidget(mpBtnDelayAssignment, 2, 2, 1, 1);

        mpLabelDelayExpression = new QLabel(CQEventWidget1);
        mpLabelDelayExpression->setObjectName(QString::fromUtf8("mpLabelDelayExpression"));
        sizePolicy1.setHeightForWidth(mpLabelDelayExpression->sizePolicy().hasHeightForWidth());
        mpLabelDelayExpression->setSizePolicy(sizePolicy1);
        mpLabelDelayExpression->setAlignment(Qt::AlignRight|Qt::AlignTop|Qt::AlignTrailing);
        mpLabelDelayExpression->setWordWrap(false);

        gridLayout->addWidget(mpLabelDelayExpression, 3, 0, 1, 1);

        mpExpressionDelay = new CQExpressionMmlStackedWidget(CQEventWidget1);
        mpExpressionDelay->setObjectName(QString::fromUtf8("mpExpressionDelay"));
        mpExpressionDelay->setMinimumSize(QSize(380, 100));
        page_7 = new QWidget();
        page_7->setObjectName(QString::fromUtf8("page_7"));
        mpExpressionDelay->addWidget(page_7);
        page_8 = new QWidget();
        page_8->setObjectName(QString::fromUtf8("page_8"));
        mpExpressionDelay->addWidget(page_8);

        gridLayout->addWidget(mpExpressionDelay, 3, 1, 1, 4);


        verticalLayout->addLayout(gridLayout);

        mpLine1 = new QFrame(CQEventWidget1);
        mpLine1->setObjectName(QString::fromUtf8("mpLine1"));
        mpLine1->setFrameShape(QFrame::HLine);
        mpLine1->setFrameShadow(QFrame::Sunken);

        verticalLayout->addWidget(mpLine1);

        gridLayout1 = new QGridLayout();
        gridLayout1->setSpacing(6);
        gridLayout1->setObjectName(QString::fromUtf8("gridLayout1"));
        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        mpLBTarget = new QListWidget(CQEventWidget1);
        mpLBTarget->setObjectName(QString::fromUtf8("mpLBTarget"));
        QSizePolicy sizePolicy2(QSizePolicy::Preferred, QSizePolicy::Expanding);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(mpLBTarget->sizePolicy().hasHeightForWidth());
        mpLBTarget->setSizePolicy(sizePolicy2);

        hboxLayout->addWidget(mpLBTarget);

        vboxLayout = new QVBoxLayout();
        vboxLayout->setSpacing(6);
        vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
        mpBtnSelectObject = new QToolButton(CQEventWidget1);
        mpBtnSelectObject->setObjectName(QString::fromUtf8("mpBtnSelectObject"));
        mpBtnSelectObject->setMaximumSize(QSize(20, 20));

        vboxLayout->addWidget(mpBtnSelectObject);

        mpBtnAddTarget = new QToolButton(CQEventWidget1);
        mpBtnAddTarget->setObjectName(QString::fromUtf8("mpBtnAddTarget"));
        mpBtnAddTarget->setMaximumSize(QSize(20, 20));

        vboxLayout->addWidget(mpBtnAddTarget);

        mpBtnDeleteTarget = new QToolButton(CQEventWidget1);
        mpBtnDeleteTarget->setObjectName(QString::fromUtf8("mpBtnDeleteTarget"));
        mpBtnDeleteTarget->setMaximumSize(QSize(20, 20));

        vboxLayout->addWidget(mpBtnDeleteTarget);

        spacerTargetIcons = new QSpacerItem(20, 40, QSizePolicy::Minimum, QSizePolicy::Expanding);

        vboxLayout->addItem(spacerTargetIcons);


        hboxLayout->addLayout(vboxLayout);


        gridLayout1->addLayout(hboxLayout, 1, 0, 1, 1);

        hboxLayout1 = new QHBoxLayout();
        hboxLayout1->setSpacing(6);
        hboxLayout1->setObjectName(QString::fromUtf8("hboxLayout1"));
        mpLabelTarget = new QLabel(CQEventWidget1);
        mpLabelTarget->setObjectName(QString::fromUtf8("mpLabelTarget"));
        mpLabelTarget->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLabelTarget->setWordWrap(false);

        hboxLayout1->addWidget(mpLabelTarget);

        spacerTarget = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout1->addItem(spacerTarget);


        gridLayout1->addLayout(hboxLayout1, 0, 0, 1, 1);

        hboxLayout2 = new QHBoxLayout();
        hboxLayout2->setSpacing(6);
        hboxLayout2->setObjectName(QString::fromUtf8("hboxLayout2"));
        mpLabelEA = new QLabel(CQEventWidget1);
        mpLabelEA->setObjectName(QString::fromUtf8("mpLabelEA"));
        mpLabelEA->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLabelEA->setWordWrap(false);

        hboxLayout2->addWidget(mpLabelEA);

        spacerExpression = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout2->addItem(spacerExpression);


        gridLayout1->addLayout(hboxLayout2, 0, 1, 1, 1);

        mpExpressionEA = new CQExpressionMmlStackedWidget(CQEventWidget1);
        mpExpressionEA->setObjectName(QString::fromUtf8("mpExpressionEA"));
        mpExpressionEA->setMinimumSize(QSize(300, 60));
        page = new QWidget();
        page->setObjectName(QString::fromUtf8("page"));
        mpExpressionEA->addWidget(page);
        page_2 = new QWidget();
        page_2->setObjectName(QString::fromUtf8("page_2"));
        mpExpressionEA->addWidget(page_2);

        gridLayout1->addWidget(mpExpressionEA, 1, 1, 1, 1);


        verticalLayout->addLayout(gridLayout1);

        mpLine2 = new QFrame(CQEventWidget1);
        mpLine2->setObjectName(QString::fromUtf8("mpLine2"));
        mpLine2->setFrameShape(QFrame::HLine);
        mpLine2->setFrameShadow(QFrame::Sunken);

        verticalLayout->addWidget(mpLine2);

        hboxLayout3 = new QHBoxLayout();
        hboxLayout3->setSpacing(6);
        hboxLayout3->setObjectName(QString::fromUtf8("hboxLayout3"));
        mpBtnCommit = new QPushButton(CQEventWidget1);
        mpBtnCommit->setObjectName(QString::fromUtf8("mpBtnCommit"));

        hboxLayout3->addWidget(mpBtnCommit);

        mpBtnRevert = new QPushButton(CQEventWidget1);
        mpBtnRevert->setObjectName(QString::fromUtf8("mpBtnRevert"));

        hboxLayout3->addWidget(mpBtnRevert);

        mpBtnNew = new QPushButton(CQEventWidget1);
        mpBtnNew->setObjectName(QString::fromUtf8("mpBtnNew"));

        hboxLayout3->addWidget(mpBtnNew);

        mpBtnDelete = new QPushButton(CQEventWidget1);
        mpBtnDelete->setObjectName(QString::fromUtf8("mpBtnDelete"));

        hboxLayout3->addWidget(mpBtnDelete);


        verticalLayout->addLayout(hboxLayout3);

        QWidget::setTabOrder(mpLineEditName, mpCheckDelay);
        QWidget::setTabOrder(mpCheckDelay, mpBtnDelayCalculation);
        QWidget::setTabOrder(mpBtnDelayCalculation, mpBtnDelayAssignment);
        QWidget::setTabOrder(mpBtnDelayAssignment, mpLBTarget);
        QWidget::setTabOrder(mpLBTarget, mpBtnSelectObject);
        QWidget::setTabOrder(mpBtnSelectObject, mpBtnAddTarget);
        QWidget::setTabOrder(mpBtnAddTarget, mpBtnDeleteTarget);
        QWidget::setTabOrder(mpBtnDeleteTarget, mpBtnCommit);
        QWidget::setTabOrder(mpBtnCommit, mpBtnRevert);
        QWidget::setTabOrder(mpBtnRevert, mpBtnNew);
        QWidget::setTabOrder(mpBtnNew, mpBtnDelete);

        retranslateUi(CQEventWidget1);
        QObject::connect(mpBtnCommit, SIGNAL(clicked()), CQEventWidget1, SLOT(slotBtnCommitClicked()));
        QObject::connect(mpBtnDelete, SIGNAL(clicked()), CQEventWidget1, SLOT(slotBtnDeleteClicked()));
        QObject::connect(mpBtnNew, SIGNAL(clicked()), CQEventWidget1, SLOT(slotBtnNewClicked()));
        QObject::connect(mpBtnRevert, SIGNAL(clicked()), CQEventWidget1, SLOT(slotBtnRevertClicked()));
        QObject::connect(mpBtnAddTarget, SIGNAL(clicked()), CQEventWidget1, SLOT(slotAddTarget()));
        QObject::connect(mpBtnDeleteTarget, SIGNAL(clicked()), CQEventWidget1, SLOT(slotDeleteTarget()));
        QObject::connect(mpBtnSelectObject, SIGNAL(clicked()), CQEventWidget1, SLOT(slotSelectObject()));

        mpExpressionTrigger->setCurrentIndex(1);


        QMetaObject::connectSlotsByName(CQEventWidget1);
    } // setupUi

    void retranslateUi(CopasiWidget *CQEventWidget1)
    {
        CQEventWidget1->setProperty("caption", QVariant(QApplication::translate("CQEventWidget1", "Event Widget", 0, QApplication::UnicodeUTF8)));
        mpLabelEventName->setText(QApplication::translate("CQEventWidget1", "Name", 0, QApplication::UnicodeUTF8));
        mpLabelOrder->setText(QApplication::translate("CQEventWidget1", "Order", 0, QApplication::UnicodeUTF8));
        mpLabelTrigger->setText(QApplication::translate("CQEventWidget1", "Trigger Expression", 0, QApplication::UnicodeUTF8));
        mpCheckDelay->setText(QApplication::translate("CQEventWidget1", "Delay", 0, QApplication::UnicodeUTF8));
        mpBtnDelayCalculation->setText(QApplication::translate("CQEventWidget1", "Calculation", 0, QApplication::UnicodeUTF8));
        mpBtnDelayAssignment->setText(QApplication::translate("CQEventWidget1", "Assignment", 0, QApplication::UnicodeUTF8));
        mpLabelDelayExpression->setText(QApplication::translate("CQEventWidget1", "Expression", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_TOOLTIP
        mpBtnSelectObject->setToolTip(QApplication::translate("CQEventWidget1", "edit target", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
        mpBtnSelectObject->setText(QString());
#ifndef QT_NO_TOOLTIP
        mpBtnAddTarget->setToolTip(QApplication::translate("CQEventWidget1", "add target", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
        mpBtnAddTarget->setText(QString());
#ifndef QT_NO_TOOLTIP
        mpBtnDeleteTarget->setToolTip(QApplication::translate("CQEventWidget1", "delete target", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
        mpBtnDeleteTarget->setText(QString());
        mpLabelTarget->setText(QApplication::translate("CQEventWidget1", "Target", 0, QApplication::UnicodeUTF8));
        mpLabelEA->setText(QApplication::translate("CQEventWidget1", "Expression", 0, QApplication::UnicodeUTF8));
        mpBtnCommit->setText(QApplication::translate("CQEventWidget1", "Commit", 0, QApplication::UnicodeUTF8));
        mpBtnRevert->setText(QApplication::translate("CQEventWidget1", "Revert", 0, QApplication::UnicodeUTF8));
        mpBtnNew->setText(QApplication::translate("CQEventWidget1", "New", 0, QApplication::UnicodeUTF8));
        mpBtnDelete->setText(QApplication::translate("CQEventWidget1", "Delete", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQEventWidget1: public Ui_CQEventWidget1 {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQEVENTWIDGET1_H
