/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQSpeciesDetail.ui'
**
** Created: Thu Aug 18 12:47:30 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQSPECIESDETAIL_H
#define UI_CQSPECIESDETAIL_H

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
#include <QtGui/QTableWidget>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>
#include <string>
#include "CQExpressionMmlStackedWidget.h"
#include "CQExpressionMmlWidget.h"
#include "copasiWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQSpeciesDetail
{
public:
    QVBoxLayout *verticalLayout;
    QGridLayout *gridLayout;
    QLabel *mpLblName;
    QLabel *mpLblCompartment;
    QComboBox *mpComboBoxCompartment;
    QLabel *mpLblType;
    QComboBox *mpComboBoxType;
    QLabel *mpLblExpression;
    QLabel *mpLblInitialValue;
    QLabel *mpLblInitialExpression;
    QFrame *mpLine1;
    QLabel *mpLblValue;
    QLabel *mpLblRate;
    QLabel *mpLblTransitionTime;
    QLabel *mpLblReactions;
    QFrame *mpLine2;
    QTableWidget *mpReactionTable;
    QLineEdit *mpEditTransitionTime;
    QLineEdit *mpEditRate;
    QLineEdit *mpEditCurrentValue;
    CQExpressionMmlStackedWidget *mpInitialExpressionEMW;
    QWidget *page_3;
    QWidget *page_4;
    QLineEdit *mpEditInitialValue;
    CQExpressionMmlStackedWidget *mpExpressionEMW;
    QWidget *page;
    QWidget *page_2;
    QLineEdit *mpEditName;
    QCheckBox *mpBoxUseInitialExpression;
    QSpacerItem *mpSpacer;
    QHBoxLayout *mpBtnLayout;
    QPushButton *mpBtnCommit;
    QPushButton *mpBtnRevert;
    QSpacerItem *mpBtnSpacer;
    QPushButton *mpBtnNew;
    QPushButton *mpBtnDelete;

    void setupUi(CopasiWidget *CQSpeciesDetail)
    {
        if (CQSpeciesDetail->objectName().isEmpty())
            CQSpeciesDetail->setObjectName(QString::fromUtf8("CQSpeciesDetail"));
        CQSpeciesDetail->resize(628, 486);
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(CQSpeciesDetail->sizePolicy().hasHeightForWidth());
        CQSpeciesDetail->setSizePolicy(sizePolicy);
        verticalLayout = new QVBoxLayout(CQSpeciesDetail);
        verticalLayout->setSpacing(6);
        verticalLayout->setContentsMargins(11, 11, 11, 11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        gridLayout = new QGridLayout();
        gridLayout->setSpacing(6);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        mpLblName = new QLabel(CQSpeciesDetail);
        mpLblName->setObjectName(QString::fromUtf8("mpLblName"));
        QSizePolicy sizePolicy1(QSizePolicy::Preferred, QSizePolicy::Minimum);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(mpLblName->sizePolicy().hasHeightForWidth());
        mpLblName->setSizePolicy(sizePolicy1);
        mpLblName->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblName->setWordWrap(false);

        gridLayout->addWidget(mpLblName, 0, 0, 1, 1);

        mpLblCompartment = new QLabel(CQSpeciesDetail);
        mpLblCompartment->setObjectName(QString::fromUtf8("mpLblCompartment"));
        sizePolicy1.setHeightForWidth(mpLblCompartment->sizePolicy().hasHeightForWidth());
        mpLblCompartment->setSizePolicy(sizePolicy1);
        mpLblCompartment->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblCompartment->setWordWrap(false);

        gridLayout->addWidget(mpLblCompartment, 1, 0, 1, 1);

        mpComboBoxCompartment = new QComboBox(CQSpeciesDetail);
        mpComboBoxCompartment->setObjectName(QString::fromUtf8("mpComboBoxCompartment"));
        QSizePolicy sizePolicy2(QSizePolicy::MinimumExpanding, QSizePolicy::Fixed);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(mpComboBoxCompartment->sizePolicy().hasHeightForWidth());
        mpComboBoxCompartment->setSizePolicy(sizePolicy2);

        gridLayout->addWidget(mpComboBoxCompartment, 1, 1, 1, 1);

        mpLblType = new QLabel(CQSpeciesDetail);
        mpLblType->setObjectName(QString::fromUtf8("mpLblType"));
        sizePolicy1.setHeightForWidth(mpLblType->sizePolicy().hasHeightForWidth());
        mpLblType->setSizePolicy(sizePolicy1);
        mpLblType->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblType->setWordWrap(false);

        gridLayout->addWidget(mpLblType, 2, 0, 1, 1);

        mpComboBoxType = new QComboBox(CQSpeciesDetail);
        mpComboBoxType->setObjectName(QString::fromUtf8("mpComboBoxType"));
        sizePolicy2.setHeightForWidth(mpComboBoxType->sizePolicy().hasHeightForWidth());
        mpComboBoxType->setSizePolicy(sizePolicy2);

        gridLayout->addWidget(mpComboBoxType, 2, 1, 1, 1);

        mpLblExpression = new QLabel(CQSpeciesDetail);
        mpLblExpression->setObjectName(QString::fromUtf8("mpLblExpression"));
        sizePolicy1.setHeightForWidth(mpLblExpression->sizePolicy().hasHeightForWidth());
        mpLblExpression->setSizePolicy(sizePolicy1);
        mpLblExpression->setAlignment(Qt::AlignRight|Qt::AlignTop|Qt::AlignTrailing);
        mpLblExpression->setWordWrap(false);

        gridLayout->addWidget(mpLblExpression, 3, 0, 1, 1);

        mpLblInitialValue = new QLabel(CQSpeciesDetail);
        mpLblInitialValue->setObjectName(QString::fromUtf8("mpLblInitialValue"));
        sizePolicy.setHeightForWidth(mpLblInitialValue->sizePolicy().hasHeightForWidth());
        mpLblInitialValue->setSizePolicy(sizePolicy);
        mpLblInitialValue->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblInitialValue->setWordWrap(false);

        gridLayout->addWidget(mpLblInitialValue, 4, 0, 1, 1);

        mpLblInitialExpression = new QLabel(CQSpeciesDetail);
        mpLblInitialExpression->setObjectName(QString::fromUtf8("mpLblInitialExpression"));
        sizePolicy1.setHeightForWidth(mpLblInitialExpression->sizePolicy().hasHeightForWidth());
        mpLblInitialExpression->setSizePolicy(sizePolicy1);
        mpLblInitialExpression->setAlignment(Qt::AlignRight|Qt::AlignTop|Qt::AlignTrailing);
        mpLblInitialExpression->setWordWrap(false);

        gridLayout->addWidget(mpLblInitialExpression, 5, 0, 1, 1);

        mpLine1 = new QFrame(CQSpeciesDetail);
        mpLine1->setObjectName(QString::fromUtf8("mpLine1"));
        mpLine1->setFrameShape(QFrame::HLine);
        mpLine1->setFrameShadow(QFrame::Sunken);

        gridLayout->addWidget(mpLine1, 6, 0, 1, 4);

        mpLblValue = new QLabel(CQSpeciesDetail);
        mpLblValue->setObjectName(QString::fromUtf8("mpLblValue"));
        sizePolicy1.setHeightForWidth(mpLblValue->sizePolicy().hasHeightForWidth());
        mpLblValue->setSizePolicy(sizePolicy1);
        mpLblValue->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblValue->setWordWrap(false);

        gridLayout->addWidget(mpLblValue, 7, 0, 1, 1);

        mpLblRate = new QLabel(CQSpeciesDetail);
        mpLblRate->setObjectName(QString::fromUtf8("mpLblRate"));
        sizePolicy1.setHeightForWidth(mpLblRate->sizePolicy().hasHeightForWidth());
        mpLblRate->setSizePolicy(sizePolicy1);
        mpLblRate->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblRate->setWordWrap(false);

        gridLayout->addWidget(mpLblRate, 8, 0, 1, 1);

        mpLblTransitionTime = new QLabel(CQSpeciesDetail);
        mpLblTransitionTime->setObjectName(QString::fromUtf8("mpLblTransitionTime"));
        sizePolicy1.setHeightForWidth(mpLblTransitionTime->sizePolicy().hasHeightForWidth());
        mpLblTransitionTime->setSizePolicy(sizePolicy1);
        mpLblTransitionTime->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblTransitionTime->setWordWrap(false);

        gridLayout->addWidget(mpLblTransitionTime, 9, 0, 1, 1);

        mpLblReactions = new QLabel(CQSpeciesDetail);
        mpLblReactions->setObjectName(QString::fromUtf8("mpLblReactions"));
        sizePolicy1.setHeightForWidth(mpLblReactions->sizePolicy().hasHeightForWidth());
        mpLblReactions->setSizePolicy(sizePolicy1);
        mpLblReactions->setTextFormat(Qt::AutoText);
        mpLblReactions->setAlignment(Qt::AlignRight|Qt::AlignTop|Qt::AlignTrailing);
        mpLblReactions->setWordWrap(true);

        gridLayout->addWidget(mpLblReactions, 10, 0, 1, 1);

        mpLine2 = new QFrame(CQSpeciesDetail);
        mpLine2->setObjectName(QString::fromUtf8("mpLine2"));
        mpLine2->setFrameShape(QFrame::HLine);
        mpLine2->setFrameShadow(QFrame::Sunken);

        gridLayout->addWidget(mpLine2, 12, 0, 1, 4);

        mpReactionTable = new QTableWidget(CQSpeciesDetail);
        if (mpReactionTable->columnCount() < 2)
            mpReactionTable->setColumnCount(2);
        QTableWidgetItem *__qtablewidgetitem = new QTableWidgetItem();
        mpReactionTable->setHorizontalHeaderItem(0, __qtablewidgetitem);
        QTableWidgetItem *__qtablewidgetitem1 = new QTableWidgetItem();
        mpReactionTable->setHorizontalHeaderItem(1, __qtablewidgetitem1);
        mpReactionTable->setObjectName(QString::fromUtf8("mpReactionTable"));
        mpReactionTable->setSelectionBehavior(QAbstractItemView::SelectRows);
        mpReactionTable->setShowGrid(false);
        mpReactionTable->setColumnCount(2);

        gridLayout->addWidget(mpReactionTable, 10, 1, 1, 2);

        mpEditTransitionTime = new QLineEdit(CQSpeciesDetail);
        mpEditTransitionTime->setObjectName(QString::fromUtf8("mpEditTransitionTime"));
        QSizePolicy sizePolicy3(QSizePolicy::MinimumExpanding, QSizePolicy::Preferred);
        sizePolicy3.setHorizontalStretch(0);
        sizePolicy3.setVerticalStretch(0);
        sizePolicy3.setHeightForWidth(mpEditTransitionTime->sizePolicy().hasHeightForWidth());
        mpEditTransitionTime->setSizePolicy(sizePolicy3);
        mpEditTransitionTime->setReadOnly(true);

        gridLayout->addWidget(mpEditTransitionTime, 9, 1, 1, 1);

        mpEditRate = new QLineEdit(CQSpeciesDetail);
        mpEditRate->setObjectName(QString::fromUtf8("mpEditRate"));
        sizePolicy3.setHeightForWidth(mpEditRate->sizePolicy().hasHeightForWidth());
        mpEditRate->setSizePolicy(sizePolicy3);
        mpEditRate->setReadOnly(true);

        gridLayout->addWidget(mpEditRate, 8, 1, 1, 1);

        mpEditCurrentValue = new QLineEdit(CQSpeciesDetail);
        mpEditCurrentValue->setObjectName(QString::fromUtf8("mpEditCurrentValue"));
        sizePolicy3.setHeightForWidth(mpEditCurrentValue->sizePolicy().hasHeightForWidth());
        mpEditCurrentValue->setSizePolicy(sizePolicy3);
        mpEditCurrentValue->setReadOnly(true);

        gridLayout->addWidget(mpEditCurrentValue, 7, 1, 1, 1);

        mpInitialExpressionEMW = new CQExpressionMmlStackedWidget(CQSpeciesDetail);
        mpInitialExpressionEMW->setObjectName(QString::fromUtf8("mpInitialExpressionEMW"));
        mpInitialExpressionEMW->setMinimumSize(QSize(300, 30));
        mpInitialExpressionEMW->setFocusPolicy(Qt::TabFocus);
        page_3 = new QWidget();
        page_3->setObjectName(QString::fromUtf8("page_3"));
        mpInitialExpressionEMW->addWidget(page_3);
        page_4 = new QWidget();
        page_4->setObjectName(QString::fromUtf8("page_4"));
        mpInitialExpressionEMW->addWidget(page_4);

        gridLayout->addWidget(mpInitialExpressionEMW, 5, 1, 1, 2);

        mpEditInitialValue = new QLineEdit(CQSpeciesDetail);
        mpEditInitialValue->setObjectName(QString::fromUtf8("mpEditInitialValue"));
        sizePolicy3.setHeightForWidth(mpEditInitialValue->sizePolicy().hasHeightForWidth());
        mpEditInitialValue->setSizePolicy(sizePolicy3);

        gridLayout->addWidget(mpEditInitialValue, 4, 1, 1, 1);

        mpExpressionEMW = new CQExpressionMmlStackedWidget(CQSpeciesDetail);
        mpExpressionEMW->setObjectName(QString::fromUtf8("mpExpressionEMW"));
        mpExpressionEMW->setMinimumSize(QSize(300, 30));
        mpExpressionEMW->setFocusPolicy(Qt::TabFocus);
        page = new QWidget();
        page->setObjectName(QString::fromUtf8("page"));
        mpExpressionEMW->addWidget(page);
        page_2 = new QWidget();
        page_2->setObjectName(QString::fromUtf8("page_2"));
        mpExpressionEMW->addWidget(page_2);

        gridLayout->addWidget(mpExpressionEMW, 3, 1, 1, 2);

        mpEditName = new QLineEdit(CQSpeciesDetail);
        mpEditName->setObjectName(QString::fromUtf8("mpEditName"));
        sizePolicy2.setHeightForWidth(mpEditName->sizePolicy().hasHeightForWidth());
        mpEditName->setSizePolicy(sizePolicy2);

        gridLayout->addWidget(mpEditName, 0, 1, 1, 2);

        mpBoxUseInitialExpression = new QCheckBox(CQSpeciesDetail);
        mpBoxUseInitialExpression->setObjectName(QString::fromUtf8("mpBoxUseInitialExpression"));

        gridLayout->addWidget(mpBoxUseInitialExpression, 4, 2, 1, 1);

        mpSpacer = new QSpacerItem(17, 13, QSizePolicy::Minimum, QSizePolicy::Expanding);

        gridLayout->addItem(mpSpacer, 11, 1, 1, 2);


        verticalLayout->addLayout(gridLayout);

        mpBtnLayout = new QHBoxLayout();
        mpBtnLayout->setSpacing(6);
        mpBtnLayout->setObjectName(QString::fromUtf8("mpBtnLayout"));
        mpBtnCommit = new QPushButton(CQSpeciesDetail);
        mpBtnCommit->setObjectName(QString::fromUtf8("mpBtnCommit"));

        mpBtnLayout->addWidget(mpBtnCommit);

        mpBtnRevert = new QPushButton(CQSpeciesDetail);
        mpBtnRevert->setObjectName(QString::fromUtf8("mpBtnRevert"));

        mpBtnLayout->addWidget(mpBtnRevert);

        mpBtnSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        mpBtnLayout->addItem(mpBtnSpacer);

        mpBtnNew = new QPushButton(CQSpeciesDetail);
        mpBtnNew->setObjectName(QString::fromUtf8("mpBtnNew"));

        mpBtnLayout->addWidget(mpBtnNew);

        mpBtnDelete = new QPushButton(CQSpeciesDetail);
        mpBtnDelete->setObjectName(QString::fromUtf8("mpBtnDelete"));

        mpBtnLayout->addWidget(mpBtnDelete);


        verticalLayout->addLayout(mpBtnLayout);

        QWidget::setTabOrder(mpComboBoxCompartment, mpComboBoxType);
        QWidget::setTabOrder(mpComboBoxType, mpBtnCommit);
        QWidget::setTabOrder(mpBtnCommit, mpBtnRevert);
        QWidget::setTabOrder(mpBtnRevert, mpBtnNew);
        QWidget::setTabOrder(mpBtnNew, mpBtnDelete);

        retranslateUi(CQSpeciesDetail);
        QObject::connect(mpBtnCommit, SIGNAL(clicked()), CQSpeciesDetail, SLOT(slotBtnCommit()));
        QObject::connect(mpBtnDelete, SIGNAL(clicked()), CQSpeciesDetail, SLOT(slotBtnDelete()));
        QObject::connect(mpBtnNew, SIGNAL(clicked()), CQSpeciesDetail, SLOT(slotBtnNew()));
        QObject::connect(mpBtnRevert, SIGNAL(clicked()), CQSpeciesDetail, SLOT(slotBtnRevert()));
        QObject::connect(mpComboBoxCompartment, SIGNAL(activated(int)), CQSpeciesDetail, SLOT(slotCompartmentChanged(int)));
        QObject::connect(mpComboBoxType, SIGNAL(activated(int)), CQSpeciesDetail, SLOT(slotTypeChanged(int)));
        QObject::connect(mpEditInitialValue, SIGNAL(editingFinished()), CQSpeciesDetail, SLOT(slotInitialValueLostFocus()));
        QObject::connect(mpBoxUseInitialExpression, SIGNAL(toggled(bool)), CQSpeciesDetail, SLOT(slotInitialTypeChanged(bool)));
        QObject::connect(mpEditName, SIGNAL(editingFinished()), CQSpeciesDetail, SLOT(slotNameLostFocus()));
        QObject::connect(mpReactionTable, SIGNAL(cellDoubleClicked(int,int)), CQSpeciesDetail, SLOT(slotSwitchToReaction(int,int)));

        QMetaObject::connectSlotsByName(CQSpeciesDetail);
    } // setupUi

    void retranslateUi(CopasiWidget *CQSpeciesDetail)
    {
        CQSpeciesDetail->setProperty("caption", QVariant(QApplication::translate("CQSpeciesDetail", "Metabolite", 0, QApplication::UnicodeUTF8)));
        mpLblName->setText(QApplication::translate("CQSpeciesDetail", "Name", 0, QApplication::UnicodeUTF8));
        mpLblCompartment->setText(QApplication::translate("CQSpeciesDetail", "Compartment", 0, QApplication::UnicodeUTF8));
        mpLblType->setText(QApplication::translate("CQSpeciesDetail", "Simulation Type", 0, QApplication::UnicodeUTF8));
        mpLblExpression->setText(QApplication::translate("CQSpeciesDetail", "Expression", 0, QApplication::UnicodeUTF8));
        mpLblInitialValue->setText(QApplication::translate("CQSpeciesDetail", "Initial Value", 0, QApplication::UnicodeUTF8));
        mpLblInitialExpression->setText(QApplication::translate("CQSpeciesDetail", "Initial Expression", 0, QApplication::UnicodeUTF8));
        mpLblValue->setText(QApplication::translate("CQSpeciesDetail", "Concentration", 0, QApplication::UnicodeUTF8));
        mpLblRate->setText(QApplication::translate("CQSpeciesDetail", "Rate", 0, QApplication::UnicodeUTF8));
        mpLblTransitionTime->setText(QApplication::translate("CQSpeciesDetail", "Transition Time", 0, QApplication::UnicodeUTF8));
        mpLblReactions->setText(QApplication::translate("CQSpeciesDetail", "Involved in Reactions", 0, QApplication::UnicodeUTF8));
        QTableWidgetItem *___qtablewidgetitem = mpReactionTable->horizontalHeaderItem(0);
        ___qtablewidgetitem->setText(QApplication::translate("CQSpeciesDetail", "Name", 0, QApplication::UnicodeUTF8));
        QTableWidgetItem *___qtablewidgetitem1 = mpReactionTable->horizontalHeaderItem(1);
        ___qtablewidgetitem1->setText(QApplication::translate("CQSpeciesDetail", "Reaction", 0, QApplication::UnicodeUTF8));
        mpBoxUseInitialExpression->setText(QApplication::translate("CQSpeciesDetail", "Use Initial Expression", 0, QApplication::UnicodeUTF8));
        mpBtnCommit->setText(QApplication::translate("CQSpeciesDetail", "Commit", 0, QApplication::UnicodeUTF8));
        mpBtnRevert->setText(QApplication::translate("CQSpeciesDetail", "Revert", 0, QApplication::UnicodeUTF8));
        mpBtnNew->setText(QApplication::translate("CQSpeciesDetail", "New", 0, QApplication::UnicodeUTF8));
        mpBtnDelete->setText(QApplication::translate("CQSpeciesDetail", "Delete", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQSpeciesDetail: public Ui_CQSpeciesDetail {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQSPECIESDETAIL_H
