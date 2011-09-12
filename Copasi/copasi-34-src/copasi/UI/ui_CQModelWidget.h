/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQModelWidget.ui'
**
** Created: Sun Sep 11 10:59:24 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQMODELWIDGET_H
#define UI_CQMODELWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QComboBox>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include "copasi/UI/CQNotes.h"
#include "copasi/UI/copasiWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQModelWidget
{
public:
    QGridLayout *gridLayout;
    QLabel *mpLblNAme;
    QLineEdit *mpEditName;
    QLabel *mpLblTimeUnit;
    QComboBox *mpComboTimeUnit;
    QLabel *mpLblInitialTime;
    QLineEdit *mpEditInitialTime;
    QFrame *mpLine1;
    QLabel *mpLblCurrentTime;
    QLineEdit *mpEditCurrentTime;
    QFrame *mpLine2;
    QHBoxLayout *mpBtnLayout;
    QPushButton *mpBtnCommit;
    QPushButton *mpBtnRevert;
    QSpacerItem *mpHorizontalSpacer;
    QComboBox *mpComboVolumeUnit;
    QLabel *mpLblVolumeUnit;
    QComboBox *mpComboAreaUnit;
    QLabel *mpLblAreaUnit;
    QComboBox *mpComboLengthUnit;
    QLabel *mpLblLengthUnit;
    QLabel *mpLblQuantityUnit;
    QComboBox *mpComboQuantityUnit;
    QLabel *mpLblModelType;
    QComboBox *mpComboModelType;
    CQNotes *mpNotes;

    void setupUi(CopasiWidget *CQModelWidget)
    {
        if (CQModelWidget->objectName().isEmpty())
            CQModelWidget->setObjectName(QString::fromUtf8("CQModelWidget"));
        CQModelWidget->resize(408, 258);
        gridLayout = new QGridLayout(CQModelWidget);
        gridLayout->setContentsMargins(0, 0, 0, 0);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        mpLblNAme = new QLabel(CQModelWidget);
        mpLblNAme->setObjectName(QString::fromUtf8("mpLblNAme"));
        mpLblNAme->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblNAme->setWordWrap(false);

        gridLayout->addWidget(mpLblNAme, 0, 0, 1, 1);

        mpEditName = new QLineEdit(CQModelWidget);
        mpEditName->setObjectName(QString::fromUtf8("mpEditName"));

        gridLayout->addWidget(mpEditName, 0, 1, 1, 3);

        mpLblTimeUnit = new QLabel(CQModelWidget);
        mpLblTimeUnit->setObjectName(QString::fromUtf8("mpLblTimeUnit"));
        mpLblTimeUnit->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblTimeUnit->setWordWrap(false);

        gridLayout->addWidget(mpLblTimeUnit, 1, 0, 1, 1);

        mpComboTimeUnit = new QComboBox(CQModelWidget);
        mpComboTimeUnit->setObjectName(QString::fromUtf8("mpComboTimeUnit"));

        gridLayout->addWidget(mpComboTimeUnit, 1, 1, 1, 1);

        mpLblInitialTime = new QLabel(CQModelWidget);
        mpLblInitialTime->setObjectName(QString::fromUtf8("mpLblInitialTime"));
        mpLblInitialTime->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblInitialTime->setWordWrap(false);

        gridLayout->addWidget(mpLblInitialTime, 7, 0, 1, 1);

        mpEditInitialTime = new QLineEdit(CQModelWidget);
        mpEditInitialTime->setObjectName(QString::fromUtf8("mpEditInitialTime"));
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(mpEditInitialTime->sizePolicy().hasHeightForWidth());
        mpEditInitialTime->setSizePolicy(sizePolicy);

        gridLayout->addWidget(mpEditInitialTime, 7, 1, 1, 1);

        mpLine1 = new QFrame(CQModelWidget);
        mpLine1->setObjectName(QString::fromUtf8("mpLine1"));
        mpLine1->setFrameShape(QFrame::HLine);
        mpLine1->setFrameShadow(QFrame::Sunken);

        gridLayout->addWidget(mpLine1, 8, 0, 1, 4);

        mpLblCurrentTime = new QLabel(CQModelWidget);
        mpLblCurrentTime->setObjectName(QString::fromUtf8("mpLblCurrentTime"));
        mpLblCurrentTime->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblCurrentTime->setWordWrap(false);

        gridLayout->addWidget(mpLblCurrentTime, 9, 0, 1, 1);

        mpEditCurrentTime = new QLineEdit(CQModelWidget);
        mpEditCurrentTime->setObjectName(QString::fromUtf8("mpEditCurrentTime"));
        sizePolicy.setHeightForWidth(mpEditCurrentTime->sizePolicy().hasHeightForWidth());
        mpEditCurrentTime->setSizePolicy(sizePolicy);
        mpEditCurrentTime->setReadOnly(true);

        gridLayout->addWidget(mpEditCurrentTime, 9, 1, 1, 1);

        mpLine2 = new QFrame(CQModelWidget);
        mpLine2->setObjectName(QString::fromUtf8("mpLine2"));
        mpLine2->setFrameShape(QFrame::HLine);
        mpLine2->setFrameShadow(QFrame::Sunken);

        gridLayout->addWidget(mpLine2, 11, 0, 1, 4);

        mpBtnLayout = new QHBoxLayout();
        mpBtnLayout->setObjectName(QString::fromUtf8("mpBtnLayout"));
        mpBtnLayout->setSizeConstraint(QLayout::SetMaximumSize);
        mpBtnCommit = new QPushButton(CQModelWidget);
        mpBtnCommit->setObjectName(QString::fromUtf8("mpBtnCommit"));
        QSizePolicy sizePolicy1(QSizePolicy::Preferred, QSizePolicy::Maximum);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(mpBtnCommit->sizePolicy().hasHeightForWidth());
        mpBtnCommit->setSizePolicy(sizePolicy1);

        mpBtnLayout->addWidget(mpBtnCommit);

        mpBtnRevert = new QPushButton(CQModelWidget);
        mpBtnRevert->setObjectName(QString::fromUtf8("mpBtnRevert"));
        sizePolicy1.setHeightForWidth(mpBtnRevert->sizePolicy().hasHeightForWidth());
        mpBtnRevert->setSizePolicy(sizePolicy1);

        mpBtnLayout->addWidget(mpBtnRevert);

        mpHorizontalSpacer = new QSpacerItem(248, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        mpBtnLayout->addItem(mpHorizontalSpacer);


        gridLayout->addLayout(mpBtnLayout, 12, 0, 1, 4);

        mpComboVolumeUnit = new QComboBox(CQModelWidget);
        mpComboVolumeUnit->setObjectName(QString::fromUtf8("mpComboVolumeUnit"));

        gridLayout->addWidget(mpComboVolumeUnit, 1, 3, 1, 1);

        mpLblVolumeUnit = new QLabel(CQModelWidget);
        mpLblVolumeUnit->setObjectName(QString::fromUtf8("mpLblVolumeUnit"));
        mpLblVolumeUnit->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblVolumeUnit->setWordWrap(false);

        gridLayout->addWidget(mpLblVolumeUnit, 1, 2, 1, 1);

        mpComboAreaUnit = new QComboBox(CQModelWidget);
        mpComboAreaUnit->setObjectName(QString::fromUtf8("mpComboAreaUnit"));

        gridLayout->addWidget(mpComboAreaUnit, 3, 3, 1, 1);

        mpLblAreaUnit = new QLabel(CQModelWidget);
        mpLblAreaUnit->setObjectName(QString::fromUtf8("mpLblAreaUnit"));
        mpLblAreaUnit->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblAreaUnit->setWordWrap(false);

        gridLayout->addWidget(mpLblAreaUnit, 3, 2, 1, 1);

        mpComboLengthUnit = new QComboBox(CQModelWidget);
        mpComboLengthUnit->setObjectName(QString::fromUtf8("mpComboLengthUnit"));

        gridLayout->addWidget(mpComboLengthUnit, 4, 3, 1, 1);

        mpLblLengthUnit = new QLabel(CQModelWidget);
        mpLblLengthUnit->setObjectName(QString::fromUtf8("mpLblLengthUnit"));
        mpLblLengthUnit->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblLengthUnit->setWordWrap(false);

        gridLayout->addWidget(mpLblLengthUnit, 4, 2, 1, 1);

        mpLblQuantityUnit = new QLabel(CQModelWidget);
        mpLblQuantityUnit->setObjectName(QString::fromUtf8("mpLblQuantityUnit"));
        mpLblQuantityUnit->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblQuantityUnit->setWordWrap(false);

        gridLayout->addWidget(mpLblQuantityUnit, 3, 0, 1, 1);

        mpComboQuantityUnit = new QComboBox(CQModelWidget);
        mpComboQuantityUnit->setObjectName(QString::fromUtf8("mpComboQuantityUnit"));

        gridLayout->addWidget(mpComboQuantityUnit, 3, 1, 1, 1);

        mpLblModelType = new QLabel(CQModelWidget);
        mpLblModelType->setObjectName(QString::fromUtf8("mpLblModelType"));
        mpLblModelType->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblModelType->setWordWrap(true);

        gridLayout->addWidget(mpLblModelType, 4, 0, 1, 1);

        mpComboModelType = new QComboBox(CQModelWidget);
        mpComboModelType->setObjectName(QString::fromUtf8("mpComboModelType"));

        gridLayout->addWidget(mpComboModelType, 4, 1, 1, 1);

        mpNotes = new CQNotes(CQModelWidget);
        mpNotes->setObjectName(QString::fromUtf8("mpNotes"));
        QSizePolicy sizePolicy2(QSizePolicy::Expanding, QSizePolicy::Preferred);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(mpNotes->sizePolicy().hasHeightForWidth());
        mpNotes->setSizePolicy(sizePolicy2);

        gridLayout->addWidget(mpNotes, 10, 0, 1, 4);

        QWidget::setTabOrder(mpEditName, mpComboTimeUnit);
        QWidget::setTabOrder(mpComboTimeUnit, mpComboVolumeUnit);
        QWidget::setTabOrder(mpComboVolumeUnit, mpComboQuantityUnit);
        QWidget::setTabOrder(mpComboQuantityUnit, mpComboAreaUnit);
        QWidget::setTabOrder(mpComboAreaUnit, mpComboModelType);
        QWidget::setTabOrder(mpComboModelType, mpComboLengthUnit);
        QWidget::setTabOrder(mpComboLengthUnit, mpEditInitialTime);
        QWidget::setTabOrder(mpEditInitialTime, mpEditCurrentTime);
        QWidget::setTabOrder(mpEditCurrentTime, mpBtnCommit);
        QWidget::setTabOrder(mpBtnCommit, mpBtnRevert);

        retranslateUi(CQModelWidget);
        QObject::connect(mpBtnCommit, SIGNAL(clicked()), CQModelWidget, SLOT(slotBtnOKClicked()));
        QObject::connect(mpBtnRevert, SIGNAL(clicked()), CQModelWidget, SLOT(slotBtnRevertClicked()));

        QMetaObject::connectSlotsByName(CQModelWidget);
    } // setupUi

    void retranslateUi(CopasiWidget *CQModelWidget)
    {
        CQModelWidget->setWindowTitle(QApplication::translate("CQModelWidget", "Form", 0, QApplication::UnicodeUTF8));
        mpLblNAme->setText(QApplication::translate("CQModelWidget", "Model Name", 0, QApplication::UnicodeUTF8));
        mpLblTimeUnit->setText(QApplication::translate("CQModelWidget", "Time Unit", 0, QApplication::UnicodeUTF8));
        mpLblInitialTime->setText(QApplication::translate("CQModelWidget", "Initial Time", 0, QApplication::UnicodeUTF8));
        mpLblCurrentTime->setText(QApplication::translate("CQModelWidget", "Current Time", 0, QApplication::UnicodeUTF8));
        mpBtnCommit->setText(QApplication::translate("CQModelWidget", "Commit", 0, QApplication::UnicodeUTF8));
        mpBtnRevert->setText(QApplication::translate("CQModelWidget", "Revert", 0, QApplication::UnicodeUTF8));
        mpLblVolumeUnit->setText(QApplication::translate("CQModelWidget", "Volume Unit", 0, QApplication::UnicodeUTF8));
        mpLblAreaUnit->setText(QApplication::translate("CQModelWidget", "Area Unit", 0, QApplication::UnicodeUTF8));
        mpLblLengthUnit->setText(QApplication::translate("CQModelWidget", "Length Unit", 0, QApplication::UnicodeUTF8));
        mpLblQuantityUnit->setText(QApplication::translate("CQModelWidget", "Quantity Unit", 0, QApplication::UnicodeUTF8));
        mpLblModelType->setText(QApplication::translate("CQModelWidget", "Rate Law Interpretation", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQModelWidget: public Ui_CQModelWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQMODELWIDGET_H
