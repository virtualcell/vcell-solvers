/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQCompartment.ui'
**
** Created: Thu Aug 18 12:47:32 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQCOMPARTMENT_H
#define UI_CQCOMPARTMENT_H

#include <Qt3Support/Q3Header>
#include <Qt3Support/Q3ListView>
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
#include "copasiWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQCompartment
{
public:
    QVBoxLayout *verticalLayout;
    QGridLayout *gridLayout;
    QLabel *mpLblName;
    QLineEdit *mpEditName;
    QLabel *mpLblDim;
    QComboBox *mpComboBoxDim;
    QLabel *mpLblType;
    QComboBox *mpComboBoxType;
    QLabel *mpLblExpression;
    CQExpressionMmlStackedWidget *mpExpressionEMW;
    QWidget *page;
    QWidget *page_2;
    QLabel *mpLblInitialValue;
    QLineEdit *mpEditInitialVolume;
    QCheckBox *mpBoxUseInitialExpression;
    QLabel *mpLblInitialExpression;
    CQExpressionMmlStackedWidget *mpInitialExpressionEMW;
    QWidget *page_3;
    QWidget *page_4;
    QFrame *mpLine1;
    QLabel *mpLblVolume;
    QLineEdit *mpEditCurrentVolume;
    QLabel *mpLblRate;
    QLineEdit *mpEditRate;
    QLabel *mpLblMetabolites;
    Q3ListView *mpMetaboliteTable;
    QSpacerItem *mpVerticalSpacer;
    QFrame *mpLine2;
    QHBoxLayout *horizontalLayout;
    QPushButton *mpBtnCommit;
    QPushButton *mpBtnRevert;
    QPushButton *mpBtnNew;
    QPushButton *mpBtnDelete;

    void setupUi(CopasiWidget *CQCompartment)
    {
        if (CQCompartment->objectName().isEmpty())
            CQCompartment->setObjectName(QString::fromUtf8("CQCompartment"));
        CQCompartment->resize(511, 448);
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(CQCompartment->sizePolicy().hasHeightForWidth());
        CQCompartment->setSizePolicy(sizePolicy);
        verticalLayout = new QVBoxLayout(CQCompartment);
        verticalLayout->setSpacing(6);
        verticalLayout->setContentsMargins(11, 11, 11, 11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        gridLayout = new QGridLayout();
        gridLayout->setSpacing(6);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        mpLblName = new QLabel(CQCompartment);
        mpLblName->setObjectName(QString::fromUtf8("mpLblName"));
        QSizePolicy sizePolicy1(QSizePolicy::Preferred, QSizePolicy::Minimum);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(mpLblName->sizePolicy().hasHeightForWidth());
        mpLblName->setSizePolicy(sizePolicy1);
        mpLblName->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblName->setWordWrap(false);

        gridLayout->addWidget(mpLblName, 0, 0, 1, 1);

        mpEditName = new QLineEdit(CQCompartment);
        mpEditName->setObjectName(QString::fromUtf8("mpEditName"));
        QSizePolicy sizePolicy2(QSizePolicy::MinimumExpanding, QSizePolicy::Fixed);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(mpEditName->sizePolicy().hasHeightForWidth());
        mpEditName->setSizePolicy(sizePolicy2);

        gridLayout->addWidget(mpEditName, 0, 1, 1, 2);

        mpLblDim = new QLabel(CQCompartment);
        mpLblDim->setObjectName(QString::fromUtf8("mpLblDim"));
        sizePolicy1.setHeightForWidth(mpLblDim->sizePolicy().hasHeightForWidth());
        mpLblDim->setSizePolicy(sizePolicy1);
        mpLblDim->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblDim->setWordWrap(false);

        gridLayout->addWidget(mpLblDim, 1, 0, 1, 1);

        mpComboBoxDim = new QComboBox(CQCompartment);
        mpComboBoxDim->setObjectName(QString::fromUtf8("mpComboBoxDim"));
        sizePolicy2.setHeightForWidth(mpComboBoxDim->sizePolicy().hasHeightForWidth());
        mpComboBoxDim->setSizePolicy(sizePolicy2);

        gridLayout->addWidget(mpComboBoxDim, 1, 1, 1, 1);

        mpLblType = new QLabel(CQCompartment);
        mpLblType->setObjectName(QString::fromUtf8("mpLblType"));
        sizePolicy1.setHeightForWidth(mpLblType->sizePolicy().hasHeightForWidth());
        mpLblType->setSizePolicy(sizePolicy1);
        mpLblType->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblType->setWordWrap(false);

        gridLayout->addWidget(mpLblType, 2, 0, 1, 1);

        mpComboBoxType = new QComboBox(CQCompartment);
        mpComboBoxType->setObjectName(QString::fromUtf8("mpComboBoxType"));
        sizePolicy2.setHeightForWidth(mpComboBoxType->sizePolicy().hasHeightForWidth());
        mpComboBoxType->setSizePolicy(sizePolicy2);

        gridLayout->addWidget(mpComboBoxType, 2, 1, 1, 1);

        mpLblExpression = new QLabel(CQCompartment);
        mpLblExpression->setObjectName(QString::fromUtf8("mpLblExpression"));
        sizePolicy1.setHeightForWidth(mpLblExpression->sizePolicy().hasHeightForWidth());
        mpLblExpression->setSizePolicy(sizePolicy1);
        mpLblExpression->setAlignment(Qt::AlignRight|Qt::AlignTop|Qt::AlignTrailing);
        mpLblExpression->setWordWrap(false);

        gridLayout->addWidget(mpLblExpression, 3, 0, 1, 1);

        mpExpressionEMW = new CQExpressionMmlStackedWidget(CQCompartment);
        mpExpressionEMW->setObjectName(QString::fromUtf8("mpExpressionEMW"));
        mpExpressionEMW->setMinimumSize(QSize(300, 30));
        page = new QWidget();
        page->setObjectName(QString::fromUtf8("page"));
        mpExpressionEMW->addWidget(page);
        page_2 = new QWidget();
        page_2->setObjectName(QString::fromUtf8("page_2"));
        mpExpressionEMW->addWidget(page_2);

        gridLayout->addWidget(mpExpressionEMW, 3, 1, 1, 2);

        mpLblInitialValue = new QLabel(CQCompartment);
        mpLblInitialValue->setObjectName(QString::fromUtf8("mpLblInitialValue"));
        sizePolicy1.setHeightForWidth(mpLblInitialValue->sizePolicy().hasHeightForWidth());
        mpLblInitialValue->setSizePolicy(sizePolicy1);
        mpLblInitialValue->setAlignment(Qt::AlignRight);
        mpLblInitialValue->setWordWrap(false);

        gridLayout->addWidget(mpLblInitialValue, 4, 0, 1, 1);

        mpEditInitialVolume = new QLineEdit(CQCompartment);
        mpEditInitialVolume->setObjectName(QString::fromUtf8("mpEditInitialVolume"));
        sizePolicy2.setHeightForWidth(mpEditInitialVolume->sizePolicy().hasHeightForWidth());
        mpEditInitialVolume->setSizePolicy(sizePolicy2);

        gridLayout->addWidget(mpEditInitialVolume, 4, 1, 1, 1);

        mpBoxUseInitialExpression = new QCheckBox(CQCompartment);
        mpBoxUseInitialExpression->setObjectName(QString::fromUtf8("mpBoxUseInitialExpression"));

        gridLayout->addWidget(mpBoxUseInitialExpression, 4, 2, 1, 1);

        mpLblInitialExpression = new QLabel(CQCompartment);
        mpLblInitialExpression->setObjectName(QString::fromUtf8("mpLblInitialExpression"));
        sizePolicy1.setHeightForWidth(mpLblInitialExpression->sizePolicy().hasHeightForWidth());
        mpLblInitialExpression->setSizePolicy(sizePolicy1);
        mpLblInitialExpression->setAlignment(Qt::AlignRight|Qt::AlignTop|Qt::AlignTrailing);
        mpLblInitialExpression->setWordWrap(false);

        gridLayout->addWidget(mpLblInitialExpression, 5, 0, 1, 1);

        mpInitialExpressionEMW = new CQExpressionMmlStackedWidget(CQCompartment);
        mpInitialExpressionEMW->setObjectName(QString::fromUtf8("mpInitialExpressionEMW"));
        mpInitialExpressionEMW->setMinimumSize(QSize(300, 30));
        page_3 = new QWidget();
        page_3->setObjectName(QString::fromUtf8("page_3"));
        mpInitialExpressionEMW->addWidget(page_3);
        page_4 = new QWidget();
        page_4->setObjectName(QString::fromUtf8("page_4"));
        mpInitialExpressionEMW->addWidget(page_4);

        gridLayout->addWidget(mpInitialExpressionEMW, 5, 1, 1, 2);

        mpLine1 = new QFrame(CQCompartment);
        mpLine1->setObjectName(QString::fromUtf8("mpLine1"));
        mpLine1->setFrameShape(QFrame::HLine);
        mpLine1->setFrameShadow(QFrame::Sunken);

        gridLayout->addWidget(mpLine1, 6, 0, 1, 3);

        mpLblVolume = new QLabel(CQCompartment);
        mpLblVolume->setObjectName(QString::fromUtf8("mpLblVolume"));
        sizePolicy1.setHeightForWidth(mpLblVolume->sizePolicy().hasHeightForWidth());
        mpLblVolume->setSizePolicy(sizePolicy1);
        mpLblVolume->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblVolume->setWordWrap(false);

        gridLayout->addWidget(mpLblVolume, 7, 0, 1, 1);

        mpEditCurrentVolume = new QLineEdit(CQCompartment);
        mpEditCurrentVolume->setObjectName(QString::fromUtf8("mpEditCurrentVolume"));
        sizePolicy2.setHeightForWidth(mpEditCurrentVolume->sizePolicy().hasHeightForWidth());
        mpEditCurrentVolume->setSizePolicy(sizePolicy2);
        mpEditCurrentVolume->setReadOnly(true);

        gridLayout->addWidget(mpEditCurrentVolume, 7, 1, 1, 1);

        mpLblRate = new QLabel(CQCompartment);
        mpLblRate->setObjectName(QString::fromUtf8("mpLblRate"));
        sizePolicy1.setHeightForWidth(mpLblRate->sizePolicy().hasHeightForWidth());
        mpLblRate->setSizePolicy(sizePolicy1);
        mpLblRate->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblRate->setWordWrap(false);

        gridLayout->addWidget(mpLblRate, 8, 0, 1, 1);

        mpEditRate = new QLineEdit(CQCompartment);
        mpEditRate->setObjectName(QString::fromUtf8("mpEditRate"));
        sizePolicy2.setHeightForWidth(mpEditRate->sizePolicy().hasHeightForWidth());
        mpEditRate->setSizePolicy(sizePolicy2);
        mpEditRate->setReadOnly(true);

        gridLayout->addWidget(mpEditRate, 8, 1, 1, 1);

        mpLblMetabolites = new QLabel(CQCompartment);
        mpLblMetabolites->setObjectName(QString::fromUtf8("mpLblMetabolites"));
        sizePolicy1.setHeightForWidth(mpLblMetabolites->sizePolicy().hasHeightForWidth());
        mpLblMetabolites->setSizePolicy(sizePolicy1);
        mpLblMetabolites->setAlignment(Qt::AlignRight|Qt::AlignTop|Qt::AlignTrailing);
        mpLblMetabolites->setWordWrap(false);

        gridLayout->addWidget(mpLblMetabolites, 9, 0, 1, 1);

        mpMetaboliteTable = new Q3ListView(CQCompartment);
        mpMetaboliteTable->addColumn(QApplication::translate("CQCompartment", "Name", 0, QApplication::UnicodeUTF8));
        mpMetaboliteTable->header()->setClickEnabled(true, mpMetaboliteTable->header()->count() - 1);
        mpMetaboliteTable->header()->setResizeEnabled(true, mpMetaboliteTable->header()->count() - 1);
        mpMetaboliteTable->setObjectName(QString::fromUtf8("mpMetaboliteTable"));
        sizePolicy.setHeightForWidth(mpMetaboliteTable->sizePolicy().hasHeightForWidth());
        mpMetaboliteTable->setSizePolicy(sizePolicy);

        gridLayout->addWidget(mpMetaboliteTable, 9, 1, 1, 2);


        verticalLayout->addLayout(gridLayout);

        mpVerticalSpacer = new QSpacerItem(20, 3, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout->addItem(mpVerticalSpacer);

        mpLine2 = new QFrame(CQCompartment);
        mpLine2->setObjectName(QString::fromUtf8("mpLine2"));
        mpLine2->setFrameShape(QFrame::HLine);
        mpLine2->setFrameShadow(QFrame::Sunken);

        verticalLayout->addWidget(mpLine2);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        mpBtnCommit = new QPushButton(CQCompartment);
        mpBtnCommit->setObjectName(QString::fromUtf8("mpBtnCommit"));

        horizontalLayout->addWidget(mpBtnCommit);

        mpBtnRevert = new QPushButton(CQCompartment);
        mpBtnRevert->setObjectName(QString::fromUtf8("mpBtnRevert"));

        horizontalLayout->addWidget(mpBtnRevert);

        mpBtnNew = new QPushButton(CQCompartment);
        mpBtnNew->setObjectName(QString::fromUtf8("mpBtnNew"));

        horizontalLayout->addWidget(mpBtnNew);

        mpBtnDelete = new QPushButton(CQCompartment);
        mpBtnDelete->setObjectName(QString::fromUtf8("mpBtnDelete"));

        horizontalLayout->addWidget(mpBtnDelete);


        verticalLayout->addLayout(horizontalLayout);

        QWidget::setTabOrder(mpEditName, mpComboBoxDim);
        QWidget::setTabOrder(mpComboBoxDim, mpComboBoxType);
        QWidget::setTabOrder(mpComboBoxType, mpExpressionEMW);
        QWidget::setTabOrder(mpExpressionEMW, mpEditInitialVolume);
        QWidget::setTabOrder(mpEditInitialVolume, mpBoxUseInitialExpression);
        QWidget::setTabOrder(mpBoxUseInitialExpression, mpInitialExpressionEMW);
        QWidget::setTabOrder(mpInitialExpressionEMW, mpEditCurrentVolume);
        QWidget::setTabOrder(mpEditCurrentVolume, mpEditRate);
        QWidget::setTabOrder(mpEditRate, mpMetaboliteTable);
        QWidget::setTabOrder(mpMetaboliteTable, mpBtnCommit);
        QWidget::setTabOrder(mpBtnCommit, mpBtnRevert);
        QWidget::setTabOrder(mpBtnRevert, mpBtnNew);
        QWidget::setTabOrder(mpBtnNew, mpBtnDelete);

        retranslateUi(CQCompartment);
        QObject::connect(mpBtnCommit, SIGNAL(clicked()), CQCompartment, SLOT(slotBtnCommit()));
        QObject::connect(mpBtnDelete, SIGNAL(clicked()), CQCompartment, SLOT(slotBtnDelete()));
        QObject::connect(mpBtnNew, SIGNAL(clicked()), CQCompartment, SLOT(slotBtnNew()));
        QObject::connect(mpBtnRevert, SIGNAL(clicked()), CQCompartment, SLOT(slotBtnRevert()));
        QObject::connect(mpComboBoxType, SIGNAL(activated(int)), CQCompartment, SLOT(slotTypeChanged(int)));
        QObject::connect(mpMetaboliteTable, SIGNAL(doubleClicked(Q3ListViewItem*)), CQCompartment, SLOT(slotMetaboliteTableCurrentChanged(Q3ListViewItem*)));
        QObject::connect(mpEditName, SIGNAL(lostFocus()), CQCompartment, SLOT(slotNameLostFocus()));
        QObject::connect(mpBoxUseInitialExpression, SIGNAL(toggled(bool)), CQCompartment, SLOT(slotInitialTypeChanged(bool)));

        mpExpressionEMW->setCurrentIndex(1);
        mpInitialExpressionEMW->setCurrentIndex(1);


        QMetaObject::connectSlotsByName(CQCompartment);
    } // setupUi

    void retranslateUi(CopasiWidget *CQCompartment)
    {
        CQCompartment->setProperty("caption", QVariant(QApplication::translate("CQCompartment", "Compartment", 0, QApplication::UnicodeUTF8)));
        mpLblName->setText(QApplication::translate("CQCompartment", "Compartment Name", 0, QApplication::UnicodeUTF8));
        mpLblDim->setText(QApplication::translate("CQCompartment", "Dimensionality", 0, QApplication::UnicodeUTF8));
        mpComboBoxDim->clear();
        mpComboBoxDim->insertItems(0, QStringList()
         << QApplication::translate("CQCompartment", "0 - dimensionless", 0, QApplication::UnicodeUTF8)
         << QApplication::translate("CQCompartment", "1D - Length", 0, QApplication::UnicodeUTF8)
         << QApplication::translate("CQCompartment", "2D - Area", 0, QApplication::UnicodeUTF8)
         << QApplication::translate("CQCompartment", "3D - Volume", 0, QApplication::UnicodeUTF8)
        );
        mpLblType->setText(QApplication::translate("CQCompartment", "Simulation Type", 0, QApplication::UnicodeUTF8));
        mpLblExpression->setText(QApplication::translate("CQCompartment", "Expression", 0, QApplication::UnicodeUTF8));
        mpLblInitialValue->setText(QApplication::translate("CQCompartment", "Initial Volume", 0, QApplication::UnicodeUTF8));
        mpBoxUseInitialExpression->setText(QApplication::translate("CQCompartment", "Use Initial Expression", 0, QApplication::UnicodeUTF8));
        mpLblInitialExpression->setText(QApplication::translate("CQCompartment", "Initial Expression", 0, QApplication::UnicodeUTF8));
        mpLblVolume->setText(QApplication::translate("CQCompartment", "Volume", 0, QApplication::UnicodeUTF8));
        mpLblRate->setText(QApplication::translate("CQCompartment", "Rate", 0, QApplication::UnicodeUTF8));
        mpLblMetabolites->setText(QApplication::translate("CQCompartment", "Contained Metabolites", 0, QApplication::UnicodeUTF8));
        mpMetaboliteTable->header()->setLabel(0, QApplication::translate("CQCompartment", "Name", 0, QApplication::UnicodeUTF8));
        mpBtnCommit->setText(QApplication::translate("CQCompartment", "Commit", 0, QApplication::UnicodeUTF8));
        mpBtnRevert->setText(QApplication::translate("CQCompartment", "Revert", 0, QApplication::UnicodeUTF8));
        mpBtnNew->setText(QApplication::translate("CQCompartment", "New", 0, QApplication::UnicodeUTF8));
        mpBtnDelete->setText(QApplication::translate("CQCompartment", "Delete", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQCompartment: public Ui_CQCompartment {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQCOMPARTMENT_H
