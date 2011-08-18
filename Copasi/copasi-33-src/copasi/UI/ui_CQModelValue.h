/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQModelValue.ui'
**
** Created: Thu Aug 18 12:47:31 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQMODELVALUE_H
#define UI_CQMODELVALUE_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QComboBox>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>
#include <string>
#include "CQExpressionMmlStackedWidget.h"
#include "CQExpressionMmlWidget.h"
#include "copasiWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQModelValue
{
public:
    QVBoxLayout *verticalLayout;
    QGridLayout *gridLayout;
    QLabel *mpLblName;
    QLabel *mpLblType;
    QComboBox *mpComboBoxType;
    QLabel *mpLblExpression;
    QLabel *mpLblInitialValue;
    QCheckBox *mpBoxUseInitialExpression;
    QLabel *mpLblInitialExpression;
    QFrame *mpLine1;
    QLabel *mpLblValue;
    QLabel *mpLblRate;
    QFrame *mpLine2;
    QLineEdit *mpEditName;
    CQExpressionMmlStackedWidget *mpExpressionEMW;
    QWidget *page;
    QWidget *page_2;
    CQExpressionMmlStackedWidget *mpInitialExpressionEMW;
    QWidget *page_3;
    QWidget *page_4;
    QLineEdit *mpEditInitialValue;
    QLineEdit *mpEditRate;
    QLineEdit *mpEditCurrentValue;
    QSpacerItem *mpSpacer;
    QHBoxLayout *hboxLayout;
    QPushButton *mpBtnCommit;
    QPushButton *mpBtnRevert;
    QPushButton *mpBtnNew;
    QPushButton *mpBtnDelete;

    void setupUi(CopasiWidget *CQModelValue)
    {
        if (CQModelValue->objectName().isEmpty())
            CQModelValue->setObjectName(QString::fromUtf8("CQModelValue"));
        CQModelValue->resize(462, 394);
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(CQModelValue->sizePolicy().hasHeightForWidth());
        CQModelValue->setSizePolicy(sizePolicy);
        verticalLayout = new QVBoxLayout(CQModelValue);
        verticalLayout->setSpacing(6);
        verticalLayout->setContentsMargins(11, 11, 11, 11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        gridLayout = new QGridLayout();
        gridLayout->setSpacing(6);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        mpLblName = new QLabel(CQModelValue);
        mpLblName->setObjectName(QString::fromUtf8("mpLblName"));
        QSizePolicy sizePolicy1(QSizePolicy::Preferred, QSizePolicy::Minimum);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(mpLblName->sizePolicy().hasHeightForWidth());
        mpLblName->setSizePolicy(sizePolicy1);
        mpLblName->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblName->setWordWrap(false);

        gridLayout->addWidget(mpLblName, 0, 0, 1, 1);

        mpLblType = new QLabel(CQModelValue);
        mpLblType->setObjectName(QString::fromUtf8("mpLblType"));
        sizePolicy1.setHeightForWidth(mpLblType->sizePolicy().hasHeightForWidth());
        mpLblType->setSizePolicy(sizePolicy1);
        mpLblType->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblType->setWordWrap(false);

        gridLayout->addWidget(mpLblType, 1, 0, 1, 1);

        mpComboBoxType = new QComboBox(CQModelValue);
        mpComboBoxType->setObjectName(QString::fromUtf8("mpComboBoxType"));
        QSizePolicy sizePolicy2(QSizePolicy::MinimumExpanding, QSizePolicy::Fixed);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(mpComboBoxType->sizePolicy().hasHeightForWidth());
        mpComboBoxType->setSizePolicy(sizePolicy2);

        gridLayout->addWidget(mpComboBoxType, 1, 1, 1, 2);

        mpLblExpression = new QLabel(CQModelValue);
        mpLblExpression->setObjectName(QString::fromUtf8("mpLblExpression"));
        sizePolicy1.setHeightForWidth(mpLblExpression->sizePolicy().hasHeightForWidth());
        mpLblExpression->setSizePolicy(sizePolicy1);
        mpLblExpression->setAlignment(Qt::AlignRight|Qt::AlignTop|Qt::AlignTrailing);
        mpLblExpression->setWordWrap(false);

        gridLayout->addWidget(mpLblExpression, 2, 0, 1, 1);

        mpLblInitialValue = new QLabel(CQModelValue);
        mpLblInitialValue->setObjectName(QString::fromUtf8("mpLblInitialValue"));
        sizePolicy.setHeightForWidth(mpLblInitialValue->sizePolicy().hasHeightForWidth());
        mpLblInitialValue->setSizePolicy(sizePolicy);
        mpLblInitialValue->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblInitialValue->setWordWrap(false);

        gridLayout->addWidget(mpLblInitialValue, 3, 0, 1, 1);

        mpBoxUseInitialExpression = new QCheckBox(CQModelValue);
        mpBoxUseInitialExpression->setObjectName(QString::fromUtf8("mpBoxUseInitialExpression"));

        gridLayout->addWidget(mpBoxUseInitialExpression, 3, 3, 1, 1);

        mpLblInitialExpression = new QLabel(CQModelValue);
        mpLblInitialExpression->setObjectName(QString::fromUtf8("mpLblInitialExpression"));
        sizePolicy1.setHeightForWidth(mpLblInitialExpression->sizePolicy().hasHeightForWidth());
        mpLblInitialExpression->setSizePolicy(sizePolicy1);
        mpLblInitialExpression->setAlignment(Qt::AlignRight|Qt::AlignTop|Qt::AlignTrailing);
        mpLblInitialExpression->setWordWrap(false);

        gridLayout->addWidget(mpLblInitialExpression, 4, 0, 1, 1);

        mpLine1 = new QFrame(CQModelValue);
        mpLine1->setObjectName(QString::fromUtf8("mpLine1"));
        mpLine1->setFrameShape(QFrame::HLine);
        mpLine1->setFrameShadow(QFrame::Sunken);

        gridLayout->addWidget(mpLine1, 5, 0, 1, 4);

        mpLblValue = new QLabel(CQModelValue);
        mpLblValue->setObjectName(QString::fromUtf8("mpLblValue"));
        sizePolicy1.setHeightForWidth(mpLblValue->sizePolicy().hasHeightForWidth());
        mpLblValue->setSizePolicy(sizePolicy1);
        mpLblValue->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblValue->setWordWrap(false);

        gridLayout->addWidget(mpLblValue, 6, 0, 1, 1);

        mpLblRate = new QLabel(CQModelValue);
        mpLblRate->setObjectName(QString::fromUtf8("mpLblRate"));
        sizePolicy1.setHeightForWidth(mpLblRate->sizePolicy().hasHeightForWidth());
        mpLblRate->setSizePolicy(sizePolicy1);
        mpLblRate->setAlignment(Qt::AlignRight|Qt::AlignTop|Qt::AlignTrailing);
        mpLblRate->setWordWrap(false);

        gridLayout->addWidget(mpLblRate, 7, 0, 1, 1);

        mpLine2 = new QFrame(CQModelValue);
        mpLine2->setObjectName(QString::fromUtf8("mpLine2"));
        mpLine2->setFrameShape(QFrame::HLine);
        mpLine2->setFrameShadow(QFrame::Sunken);

        gridLayout->addWidget(mpLine2, 9, 0, 1, 4);

        mpEditName = new QLineEdit(CQModelValue);
        mpEditName->setObjectName(QString::fromUtf8("mpEditName"));
        sizePolicy2.setHeightForWidth(mpEditName->sizePolicy().hasHeightForWidth());
        mpEditName->setSizePolicy(sizePolicy2);

        gridLayout->addWidget(mpEditName, 0, 1, 1, 3);

        mpExpressionEMW = new CQExpressionMmlStackedWidget(CQModelValue);
        mpExpressionEMW->setObjectName(QString::fromUtf8("mpExpressionEMW"));
        mpExpressionEMW->setMinimumSize(QSize(310, 60));
        page = new QWidget();
        page->setObjectName(QString::fromUtf8("page"));
        mpExpressionEMW->addWidget(page);
        page_2 = new QWidget();
        page_2->setObjectName(QString::fromUtf8("page_2"));
        mpExpressionEMW->addWidget(page_2);

        gridLayout->addWidget(mpExpressionEMW, 2, 1, 1, 3);

        mpInitialExpressionEMW = new CQExpressionMmlStackedWidget(CQModelValue);
        mpInitialExpressionEMW->setObjectName(QString::fromUtf8("mpInitialExpressionEMW"));
        mpInitialExpressionEMW->setMinimumSize(QSize(310, 60));
        page_3 = new QWidget();
        page_3->setObjectName(QString::fromUtf8("page_3"));
        mpInitialExpressionEMW->addWidget(page_3);
        page_4 = new QWidget();
        page_4->setObjectName(QString::fromUtf8("page_4"));
        mpInitialExpressionEMW->addWidget(page_4);

        gridLayout->addWidget(mpInitialExpressionEMW, 4, 1, 1, 3);

        mpEditInitialValue = new QLineEdit(CQModelValue);
        mpEditInitialValue->setObjectName(QString::fromUtf8("mpEditInitialValue"));
        QSizePolicy sizePolicy3(QSizePolicy::MinimumExpanding, QSizePolicy::Preferred);
        sizePolicy3.setHorizontalStretch(0);
        sizePolicy3.setVerticalStretch(0);
        sizePolicy3.setHeightForWidth(mpEditInitialValue->sizePolicy().hasHeightForWidth());
        mpEditInitialValue->setSizePolicy(sizePolicy3);

        gridLayout->addWidget(mpEditInitialValue, 3, 1, 1, 2);

        mpEditRate = new QLineEdit(CQModelValue);
        mpEditRate->setObjectName(QString::fromUtf8("mpEditRate"));
        sizePolicy3.setHeightForWidth(mpEditRate->sizePolicy().hasHeightForWidth());
        mpEditRate->setSizePolicy(sizePolicy3);
        mpEditRate->setReadOnly(true);

        gridLayout->addWidget(mpEditRate, 7, 1, 1, 2);

        mpEditCurrentValue = new QLineEdit(CQModelValue);
        mpEditCurrentValue->setObjectName(QString::fromUtf8("mpEditCurrentValue"));
        sizePolicy3.setHeightForWidth(mpEditCurrentValue->sizePolicy().hasHeightForWidth());
        mpEditCurrentValue->setSizePolicy(sizePolicy3);
        mpEditCurrentValue->setReadOnly(true);

        gridLayout->addWidget(mpEditCurrentValue, 6, 1, 1, 2);

        mpSpacer = new QSpacerItem(20, 20, QSizePolicy::Minimum, QSizePolicy::Expanding);

        gridLayout->addItem(mpSpacer, 8, 1, 1, 2);


        verticalLayout->addLayout(gridLayout);

        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        mpBtnCommit = new QPushButton(CQModelValue);
        mpBtnCommit->setObjectName(QString::fromUtf8("mpBtnCommit"));

        hboxLayout->addWidget(mpBtnCommit);

        mpBtnRevert = new QPushButton(CQModelValue);
        mpBtnRevert->setObjectName(QString::fromUtf8("mpBtnRevert"));

        hboxLayout->addWidget(mpBtnRevert);

        mpBtnNew = new QPushButton(CQModelValue);
        mpBtnNew->setObjectName(QString::fromUtf8("mpBtnNew"));

        hboxLayout->addWidget(mpBtnNew);

        mpBtnDelete = new QPushButton(CQModelValue);
        mpBtnDelete->setObjectName(QString::fromUtf8("mpBtnDelete"));

        hboxLayout->addWidget(mpBtnDelete);


        verticalLayout->addLayout(hboxLayout);

        QWidget::setTabOrder(mpComboBoxType, mpBoxUseInitialExpression);
        QWidget::setTabOrder(mpBoxUseInitialExpression, mpBtnCommit);
        QWidget::setTabOrder(mpBtnCommit, mpBtnRevert);
        QWidget::setTabOrder(mpBtnRevert, mpBtnNew);
        QWidget::setTabOrder(mpBtnNew, mpBtnDelete);

        retranslateUi(CQModelValue);
        QObject::connect(mpBtnCommit, SIGNAL(clicked()), CQModelValue, SLOT(slotBtnCommit()));
        QObject::connect(mpBtnDelete, SIGNAL(clicked()), CQModelValue, SLOT(slotBtnDelete()));
        QObject::connect(mpBtnNew, SIGNAL(clicked()), CQModelValue, SLOT(slotBtnNew()));
        QObject::connect(mpBtnRevert, SIGNAL(clicked()), CQModelValue, SLOT(slotBtnRevert()));
        QObject::connect(mpComboBoxType, SIGNAL(activated(int)), CQModelValue, SLOT(slotTypeChanged(int)));
        QObject::connect(mpBoxUseInitialExpression, SIGNAL(toggled(bool)), CQModelValue, SLOT(slotInitialTypeChanged(bool)));
        QObject::connect(mpEditName, SIGNAL(lostFocus()), CQModelValue, SLOT(slotNameLostFocus()));

        QMetaObject::connectSlotsByName(CQModelValue);
    } // setupUi

    void retranslateUi(CopasiWidget *CQModelValue)
    {
        CQModelValue->setProperty("caption", QVariant(QApplication::translate("CQModelValue", "Global Quantity", 0, QApplication::UnicodeUTF8)));
        mpLblName->setText(QApplication::translate("CQModelValue", "Name", 0, QApplication::UnicodeUTF8));
        mpLblType->setText(QApplication::translate("CQModelValue", "Simulation Type", 0, QApplication::UnicodeUTF8));
        mpLblExpression->setText(QApplication::translate("CQModelValue", "Expression", 0, QApplication::UnicodeUTF8));
        mpLblInitialValue->setText(QApplication::translate("CQModelValue", "Initial Value", 0, QApplication::UnicodeUTF8));
        mpBoxUseInitialExpression->setText(QApplication::translate("CQModelValue", "Use Initial Expression", 0, QApplication::UnicodeUTF8));
        mpLblInitialExpression->setText(QApplication::translate("CQModelValue", "Initial Expression", 0, QApplication::UnicodeUTF8));
        mpLblValue->setText(QApplication::translate("CQModelValue", "Value", 0, QApplication::UnicodeUTF8));
        mpLblRate->setText(QApplication::translate("CQModelValue", "Rate", 0, QApplication::UnicodeUTF8));
        mpBtnCommit->setText(QApplication::translate("CQModelValue", "Commit", 0, QApplication::UnicodeUTF8));
        mpBtnRevert->setText(QApplication::translate("CQModelValue", "Revert", 0, QApplication::UnicodeUTF8));
        mpBtnNew->setText(QApplication::translate("CQModelValue", "New", 0, QApplication::UnicodeUTF8));
        mpBtnDelete->setText(QApplication::translate("CQModelValue", "Delete", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQModelValue: public Ui_CQModelValue {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQMODELVALUE_H
