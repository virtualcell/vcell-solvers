/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQFittingItemWidget.ui'
**
** Created: Thu Aug 18 12:47:31 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQFITTINGITEMWIDGET_H
#define UI_CQFITTINGITEMWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QComboBox>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QTableWidget>
#include <QtGui/QToolButton>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>
#include "TaskWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQFittingItemWidget
{
public:
    QVBoxLayout *verticalLayout;
    QTableWidget *mpTable;
    QGridLayout *gridLayout;
    QLabel *mpLblObject;
    QHBoxLayout *hboxLayout;
    QLineEdit *mpEditObject;
    QToolButton *mpBtnObject;
    QSpacerItem *mpSpacer1;
    QToolButton *mpBtnNew;
    QToolButton *mpBtnUp;
    QLabel *mpLblLower;
    QHBoxLayout *hboxLayout1;
    QLineEdit *mpEditLower;
    QToolButton *mpBtnLowerEdit;
    QLabel *mpLblUpper;
    QHBoxLayout *hboxLayout2;
    QLineEdit *mpEditUpper;
    QToolButton *mpBtnUpperEdit;
    QLabel *mpLblStart;
    QHBoxLayout *hboxLayout3;
    QLineEdit *mpEditStart;
    QToolButton *mpBtnReset;
    QLabel *mpLblExperiments;
    QHBoxLayout *hboxLayout4;
    QCheckBox *mpCheckAll;
    QComboBox *mpBoxExperiments;
    QToolButton *mpBtnExperiments;
    QLabel *mpLblCrossValidations;
    QHBoxLayout *hboxLayout5;
    QCheckBox *mpCheckCrossValidationsAll;
    QComboBox *mpBoxCrossValidations;
    QToolButton *mpBtnCrossValidations;
    QCheckBox *mpCheckLowerInf;
    QCheckBox *mpCheckUpperInf;
    QToolButton *mpBtnDown;
    QToolButton *mpBtnDel;
    QSpacerItem *mpSpacer2;
    QToolButton *mpBtnCopy;
    QSpacerItem *mpSpacer3;
    QPushButton *mpBtnPerExperiment;

    void setupUi(QWidget *CQFittingItemWidget)
    {
        if (CQFittingItemWidget->objectName().isEmpty())
            CQFittingItemWidget->setObjectName(QString::fromUtf8("CQFittingItemWidget"));
        CQFittingItemWidget->resize(528, 530);
        QSizePolicy sizePolicy(QSizePolicy::Expanding, QSizePolicy::Expanding);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(CQFittingItemWidget->sizePolicy().hasHeightForWidth());
        CQFittingItemWidget->setSizePolicy(sizePolicy);
        verticalLayout = new QVBoxLayout(CQFittingItemWidget);
        verticalLayout->setSpacing(6);
        verticalLayout->setContentsMargins(11, 11, 11, 11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        mpTable = new QTableWidget(CQFittingItemWidget);
        if (mpTable->columnCount() < 1)
            mpTable->setColumnCount(1);
        QTableWidgetItem *__qtablewidgetitem = new QTableWidgetItem();
        mpTable->setHorizontalHeaderItem(0, __qtablewidgetitem);
        mpTable->setObjectName(QString::fromUtf8("mpTable"));
        sizePolicy.setHeightForWidth(mpTable->sizePolicy().hasHeightForWidth());
        mpTable->setSizePolicy(sizePolicy);
        mpTable->setLineWidth(2);
        mpTable->setSelectionMode(QAbstractItemView::MultiSelection);

        verticalLayout->addWidget(mpTable);

        gridLayout = new QGridLayout();
        gridLayout->setSpacing(6);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        gridLayout->setVerticalSpacing(0);
        mpLblObject = new QLabel(CQFittingItemWidget);
        mpLblObject->setObjectName(QString::fromUtf8("mpLblObject"));
        mpLblObject->setWordWrap(false);

        gridLayout->addWidget(mpLblObject, 0, 0, 1, 2);

        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        mpEditObject = new QLineEdit(CQFittingItemWidget);
        mpEditObject->setObjectName(QString::fromUtf8("mpEditObject"));
        mpEditObject->setReadOnly(true);

        hboxLayout->addWidget(mpEditObject);

        mpBtnObject = new QToolButton(CQFittingItemWidget);
        mpBtnObject->setObjectName(QString::fromUtf8("mpBtnObject"));
        mpBtnObject->setMaximumSize(QSize(20, 20));
        QIcon icon;
        icon.addFile(QString::fromUtf8("../../../../.designer/backup/image2"), QSize(), QIcon::Normal, QIcon::Off);
        mpBtnObject->setIcon(icon);

        hboxLayout->addWidget(mpBtnObject);


        gridLayout->addLayout(hboxLayout, 0, 2, 1, 6);

        mpSpacer1 = new QSpacerItem(13, 21, QSizePolicy::Fixed, QSizePolicy::Minimum);

        gridLayout->addItem(mpSpacer1, 0, 8, 1, 1);

        mpBtnNew = new QToolButton(CQFittingItemWidget);
        mpBtnNew->setObjectName(QString::fromUtf8("mpBtnNew"));
        mpBtnNew->setMaximumSize(QSize(20, 20));
        QIcon icon1;
        icon1.addFile(QString::fromUtf8("../../../../.designer/backup/image3"), QSize(), QIcon::Normal, QIcon::Off);
        mpBtnNew->setIcon(icon1);

        gridLayout->addWidget(mpBtnNew, 0, 9, 1, 1);

        mpBtnUp = new QToolButton(CQFittingItemWidget);
        mpBtnUp->setObjectName(QString::fromUtf8("mpBtnUp"));
        mpBtnUp->setMaximumSize(QSize(20, 20));
        QIcon icon2;
        icon2.addFile(QString::fromUtf8("../../../../.designer/backup/image1"), QSize(), QIcon::Normal, QIcon::Off);
        mpBtnUp->setIcon(icon2);

        gridLayout->addWidget(mpBtnUp, 0, 10, 1, 1);

        mpLblLower = new QLabel(CQFittingItemWidget);
        mpLblLower->setObjectName(QString::fromUtf8("mpLblLower"));
        mpLblLower->setWordWrap(false);

        gridLayout->addWidget(mpLblLower, 1, 0, 1, 1);

        hboxLayout1 = new QHBoxLayout();
        hboxLayout1->setSpacing(6);
        hboxLayout1->setObjectName(QString::fromUtf8("hboxLayout1"));
        mpEditLower = new QLineEdit(CQFittingItemWidget);
        mpEditLower->setObjectName(QString::fromUtf8("mpEditLower"));
        mpEditLower->setEnabled(false);

        hboxLayout1->addWidget(mpEditLower);

        mpBtnLowerEdit = new QToolButton(CQFittingItemWidget);
        mpBtnLowerEdit->setObjectName(QString::fromUtf8("mpBtnLowerEdit"));
        mpBtnLowerEdit->setMaximumSize(QSize(20, 20));
        mpBtnLowerEdit->setIcon(icon);

        hboxLayout1->addWidget(mpBtnLowerEdit);


        gridLayout->addLayout(hboxLayout1, 1, 2, 1, 6);

        mpLblUpper = new QLabel(CQFittingItemWidget);
        mpLblUpper->setObjectName(QString::fromUtf8("mpLblUpper"));
        mpLblUpper->setWordWrap(false);

        gridLayout->addWidget(mpLblUpper, 2, 0, 1, 1);

        hboxLayout2 = new QHBoxLayout();
        hboxLayout2->setSpacing(6);
        hboxLayout2->setObjectName(QString::fromUtf8("hboxLayout2"));
        mpEditUpper = new QLineEdit(CQFittingItemWidget);
        mpEditUpper->setObjectName(QString::fromUtf8("mpEditUpper"));
        mpEditUpper->setEnabled(false);

        hboxLayout2->addWidget(mpEditUpper);

        mpBtnUpperEdit = new QToolButton(CQFittingItemWidget);
        mpBtnUpperEdit->setObjectName(QString::fromUtf8("mpBtnUpperEdit"));
        mpBtnUpperEdit->setMaximumSize(QSize(20, 20));
        mpBtnUpperEdit->setIcon(icon);

        hboxLayout2->addWidget(mpBtnUpperEdit);


        gridLayout->addLayout(hboxLayout2, 2, 2, 1, 6);

        mpLblStart = new QLabel(CQFittingItemWidget);
        mpLblStart->setObjectName(QString::fromUtf8("mpLblStart"));
        mpLblStart->setWordWrap(false);

        gridLayout->addWidget(mpLblStart, 3, 0, 1, 2);

        hboxLayout3 = new QHBoxLayout();
        hboxLayout3->setSpacing(6);
        hboxLayout3->setObjectName(QString::fromUtf8("hboxLayout3"));
        mpEditStart = new QLineEdit(CQFittingItemWidget);
        mpEditStart->setObjectName(QString::fromUtf8("mpEditStart"));
        mpEditStart->setReadOnly(false);

        hboxLayout3->addWidget(mpEditStart);

        mpBtnReset = new QToolButton(CQFittingItemWidget);
        mpBtnReset->setObjectName(QString::fromUtf8("mpBtnReset"));
        mpBtnReset->setMaximumSize(QSize(20, 20));
        QIcon icon3;
        icon3.addFile(QString::fromUtf8("../../../../.designer/backup/image6"), QSize(), QIcon::Normal, QIcon::Off);
        mpBtnReset->setIcon(icon3);

        hboxLayout3->addWidget(mpBtnReset);


        gridLayout->addLayout(hboxLayout3, 3, 2, 1, 6);

        mpLblExperiments = new QLabel(CQFittingItemWidget);
        mpLblExperiments->setObjectName(QString::fromUtf8("mpLblExperiments"));
        mpLblExperiments->setWordWrap(false);

        gridLayout->addWidget(mpLblExperiments, 4, 0, 1, 2);

        hboxLayout4 = new QHBoxLayout();
        hboxLayout4->setSpacing(6);
        hboxLayout4->setObjectName(QString::fromUtf8("hboxLayout4"));
        mpCheckAll = new QCheckBox(CQFittingItemWidget);
        mpCheckAll->setObjectName(QString::fromUtf8("mpCheckAll"));
        mpCheckAll->setChecked(true);

        hboxLayout4->addWidget(mpCheckAll);

        mpBoxExperiments = new QComboBox(CQFittingItemWidget);
        mpBoxExperiments->setObjectName(QString::fromUtf8("mpBoxExperiments"));
        mpBoxExperiments->setEnabled(true);
        QSizePolicy sizePolicy1(QSizePolicy::Expanding, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(mpBoxExperiments->sizePolicy().hasHeightForWidth());
        mpBoxExperiments->setSizePolicy(sizePolicy1);
        mpBoxExperiments->setDuplicatesEnabled(false);

        hboxLayout4->addWidget(mpBoxExperiments);

        mpBtnExperiments = new QToolButton(CQFittingItemWidget);
        mpBtnExperiments->setObjectName(QString::fromUtf8("mpBtnExperiments"));
        mpBtnExperiments->setMaximumSize(QSize(20, 20));
        mpBtnExperiments->setIcon(icon3);

        hboxLayout4->addWidget(mpBtnExperiments);


        gridLayout->addLayout(hboxLayout4, 4, 2, 1, 6);

        mpLblCrossValidations = new QLabel(CQFittingItemWidget);
        mpLblCrossValidations->setObjectName(QString::fromUtf8("mpLblCrossValidations"));
        mpLblCrossValidations->setWordWrap(false);

        gridLayout->addWidget(mpLblCrossValidations, 5, 0, 1, 2);

        hboxLayout5 = new QHBoxLayout();
        hboxLayout5->setSpacing(6);
        hboxLayout5->setObjectName(QString::fromUtf8("hboxLayout5"));
        mpCheckCrossValidationsAll = new QCheckBox(CQFittingItemWidget);
        mpCheckCrossValidationsAll->setObjectName(QString::fromUtf8("mpCheckCrossValidationsAll"));
        mpCheckCrossValidationsAll->setChecked(true);

        hboxLayout5->addWidget(mpCheckCrossValidationsAll);

        mpBoxCrossValidations = new QComboBox(CQFittingItemWidget);
        mpBoxCrossValidations->setObjectName(QString::fromUtf8("mpBoxCrossValidations"));
        mpBoxCrossValidations->setEnabled(true);
        sizePolicy1.setHeightForWidth(mpBoxCrossValidations->sizePolicy().hasHeightForWidth());
        mpBoxCrossValidations->setSizePolicy(sizePolicy1);
        mpBoxCrossValidations->setDuplicatesEnabled(false);

        hboxLayout5->addWidget(mpBoxCrossValidations);

        mpBtnCrossValidations = new QToolButton(CQFittingItemWidget);
        mpBtnCrossValidations->setObjectName(QString::fromUtf8("mpBtnCrossValidations"));
        mpBtnCrossValidations->setMaximumSize(QSize(20, 20));
        mpBtnCrossValidations->setIcon(icon3);

        hboxLayout5->addWidget(mpBtnCrossValidations);


        gridLayout->addLayout(hboxLayout5, 5, 2, 1, 6);

        mpCheckLowerInf = new QCheckBox(CQFittingItemWidget);
        mpCheckLowerInf->setObjectName(QString::fromUtf8("mpCheckLowerInf"));
        mpCheckLowerInf->setChecked(true);

        gridLayout->addWidget(mpCheckLowerInf, 1, 1, 1, 1);

        mpCheckUpperInf = new QCheckBox(CQFittingItemWidget);
        mpCheckUpperInf->setObjectName(QString::fromUtf8("mpCheckUpperInf"));
        mpCheckUpperInf->setChecked(true);

        gridLayout->addWidget(mpCheckUpperInf, 2, 1, 1, 1);

        mpBtnDown = new QToolButton(CQFittingItemWidget);
        mpBtnDown->setObjectName(QString::fromUtf8("mpBtnDown"));
        mpBtnDown->setMaximumSize(QSize(20, 20));
        QIcon icon4;
        icon4.addFile(QString::fromUtf8("../../../../.designer/backup/image4"), QSize(), QIcon::Normal, QIcon::Off);
        mpBtnDown->setIcon(icon4);

        gridLayout->addWidget(mpBtnDown, 1, 10, 1, 1);

        mpBtnDel = new QToolButton(CQFittingItemWidget);
        mpBtnDel->setObjectName(QString::fromUtf8("mpBtnDel"));
        mpBtnDel->setMaximumSize(QSize(20, 20));
        QIcon icon5;
        icon5.addFile(QString::fromUtf8("../../../../.designer/backup/image0"), QSize(), QIcon::Normal, QIcon::Off);
        mpBtnDel->setIcon(icon5);

        gridLayout->addWidget(mpBtnDel, 1, 9, 1, 1);

        mpSpacer2 = new QSpacerItem(13, 19, QSizePolicy::Fixed, QSizePolicy::Minimum);

        gridLayout->addItem(mpSpacer2, 1, 8, 1, 1);

        mpBtnCopy = new QToolButton(CQFittingItemWidget);
        mpBtnCopy->setObjectName(QString::fromUtf8("mpBtnCopy"));
        mpBtnCopy->setMaximumSize(QSize(20, 20));
        QIcon icon6;
        icon6.addFile(QString::fromUtf8("../../../../.designer/backup/image5"), QSize(), QIcon::Normal, QIcon::Off);
        mpBtnCopy->setIcon(icon6);

        gridLayout->addWidget(mpBtnCopy, 2, 9, 1, 1);

        mpSpacer3 = new QSpacerItem(13, 18, QSizePolicy::Fixed, QSizePolicy::Minimum);

        gridLayout->addItem(mpSpacer3, 2, 8, 1, 1);


        verticalLayout->addLayout(gridLayout);

        mpBtnPerExperiment = new QPushButton(CQFittingItemWidget);
        mpBtnPerExperiment->setObjectName(QString::fromUtf8("mpBtnPerExperiment"));

        verticalLayout->addWidget(mpBtnPerExperiment);

        QWidget::setTabOrder(mpEditObject, mpEditLower);
        QWidget::setTabOrder(mpEditLower, mpEditUpper);
        QWidget::setTabOrder(mpEditUpper, mpCheckAll);
        QWidget::setTabOrder(mpCheckAll, mpBoxExperiments);
        QWidget::setTabOrder(mpBoxExperiments, mpCheckCrossValidationsAll);
        QWidget::setTabOrder(mpCheckCrossValidationsAll, mpBoxCrossValidations);
        QWidget::setTabOrder(mpBoxCrossValidations, mpBtnPerExperiment);

        retranslateUi(CQFittingItemWidget);
        QObject::connect(mpCheckLowerInf, SIGNAL(toggled(bool)), CQFittingItemWidget, SLOT(slotCheckLowerInf(bool)));
        QObject::connect(mpCheckUpperInf, SIGNAL(toggled(bool)), CQFittingItemWidget, SLOT(slotCheckUpperInf(bool)));
        QObject::connect(mpBtnLowerEdit, SIGNAL(clicked()), CQFittingItemWidget, SLOT(slotLowerEdit()));
        QObject::connect(mpBtnUpperEdit, SIGNAL(clicked()), CQFittingItemWidget, SLOT(slotUpperEdit()));
        QObject::connect(mpBtnObject, SIGNAL(clicked()), CQFittingItemWidget, SLOT(slotParamEdit()));
        QObject::connect(mpBtnExperiments, SIGNAL(clicked()), CQFittingItemWidget, SLOT(slotExperiments()));
        QObject::connect(mpBtnDel, SIGNAL(clicked()), CQFittingItemWidget, SLOT(slotDelete()));
        QObject::connect(mpCheckAll, SIGNAL(toggled(bool)), CQFittingItemWidget, SLOT(slotCheckAllExperiments(bool)));
        QObject::connect(mpBtnDown, SIGNAL(clicked()), CQFittingItemWidget, SLOT(slotDown()));
        QObject::connect(mpBtnPerExperiment, SIGNAL(clicked()), CQFittingItemWidget, SLOT(slotDuplicatePerExperiment()));
        QObject::connect(mpBtnUp, SIGNAL(clicked()), CQFittingItemWidget, SLOT(slotUp()));
        QObject::connect(mpBtnCopy, SIGNAL(clicked()), CQFittingItemWidget, SLOT(slotCopy()));
        QObject::connect(mpBtnNew, SIGNAL(clicked()), CQFittingItemWidget, SLOT(slotNew()));
        QObject::connect(mpEditLower, SIGNAL(editingFinished()), CQFittingItemWidget, SLOT(slotLowerLostFocus()));
        QObject::connect(mpEditUpper, SIGNAL(editingFinished()), CQFittingItemWidget, SLOT(slotUpperLostFocus()));
        QObject::connect(mpBtnReset, SIGNAL(clicked()), CQFittingItemWidget, SLOT(slotReset()));
        QObject::connect(mpEditStart, SIGNAL(editingFinished()), CQFittingItemWidget, SLOT(slotStartLostFocus()));
        QObject::connect(mpBtnCrossValidations, SIGNAL(clicked()), CQFittingItemWidget, SLOT(slotCrossValidations()));
        QObject::connect(mpCheckCrossValidationsAll, SIGNAL(toggled(bool)), CQFittingItemWidget, SLOT(slotCheckAllCrossValidations(bool)));
        QObject::connect(mpTable, SIGNAL(itemSelectionChanged()), CQFittingItemWidget, SLOT(slotSelectionChanged()));

        QMetaObject::connectSlotsByName(CQFittingItemWidget);
    } // setupUi

    void retranslateUi(QWidget *CQFittingItemWidget)
    {
        CQFittingItemWidget->setWindowTitle(QApplication::translate("CQFittingItemWidget", "CQFittingItemWidget", 0, QApplication::UnicodeUTF8));
        QTableWidgetItem *___qtablewidgetitem = mpTable->horizontalHeaderItem(0);
        ___qtablewidgetitem->setText(QApplication::translate("CQFittingItemWidget", "New Column", 0, QApplication::UnicodeUTF8));
        mpLblObject->setText(QApplication::translate("CQFittingItemWidget", "Object", 0, QApplication::UnicodeUTF8));
        mpBtnObject->setText(QApplication::translate("CQFittingItemWidget", "select", 0, QApplication::UnicodeUTF8));
        mpBtnNew->setText(QApplication::translate("CQFittingItemWidget", "new", 0, QApplication::UnicodeUTF8));
        mpBtnUp->setText(QApplication::translate("CQFittingItemWidget", "move up", 0, QApplication::UnicodeUTF8));
        mpLblLower->setText(QApplication::translate("CQFittingItemWidget", "Lower Bound", 0, QApplication::UnicodeUTF8));
        mpBtnLowerEdit->setText(QApplication::translate("CQFittingItemWidget", "select", 0, QApplication::UnicodeUTF8));
        mpLblUpper->setText(QApplication::translate("CQFittingItemWidget", "Upper Bound", 0, QApplication::UnicodeUTF8));
        mpBtnUpperEdit->setText(QApplication::translate("CQFittingItemWidget", "select", 0, QApplication::UnicodeUTF8));
        mpLblStart->setText(QApplication::translate("CQFittingItemWidget", "Start Value", 0, QApplication::UnicodeUTF8));
        mpBtnReset->setText(QApplication::translate("CQFittingItemWidget", "reset", 0, QApplication::UnicodeUTF8));
        mpLblExperiments->setText(QApplication::translate("CQFittingItemWidget", "Affected Experiments", 0, QApplication::UnicodeUTF8));
        mpCheckAll->setText(QApplication::translate("CQFittingItemWidget", "all", 0, QApplication::UnicodeUTF8));
        mpBtnExperiments->setText(QString());
        mpLblCrossValidations->setText(QApplication::translate("CQFittingItemWidget", "Affected Cross Validations", 0, QApplication::UnicodeUTF8));
        mpCheckCrossValidationsAll->setText(QApplication::translate("CQFittingItemWidget", "all", 0, QApplication::UnicodeUTF8));
        mpBtnCrossValidations->setText(QString());
        mpCheckLowerInf->setText(QApplication::translate("CQFittingItemWidget", "- Infinity", 0, QApplication::UnicodeUTF8));
        mpCheckUpperInf->setText(QApplication::translate("CQFittingItemWidget", "+ Infinity", 0, QApplication::UnicodeUTF8));
        mpBtnDown->setText(QApplication::translate("CQFittingItemWidget", "move down", 0, QApplication::UnicodeUTF8));
        mpBtnDel->setText(QApplication::translate("CQFittingItemWidget", "delete", 0, QApplication::UnicodeUTF8));
        mpBtnCopy->setText(QApplication::translate("CQFittingItemWidget", "copy", 0, QApplication::UnicodeUTF8));
        mpBtnPerExperiment->setText(QApplication::translate("CQFittingItemWidget", "Duplicate for each Experiment", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQFittingItemWidget: public Ui_CQFittingItemWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQFITTINGITEMWIDGET_H
