/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQExpandModelData.ui'
**
** Created: Sun Sep 11 10:59:25 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQEXPANDMODELDATA_H
#define UI_CQEXPANDMODELDATA_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QComboBox>
#include <QtGui/QDialog>
#include <QtGui/QFrame>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>
#include <QtGui/QTableWidget>
#include <QtGui/QVBoxLayout>
#include <map>
#include <string>

QT_BEGIN_NAMESPACE

class Ui_CQExpandModelData
{
public:
    QVBoxLayout *vboxLayout;
    QFrame *frame;
    QLabel *mpLblCompartmentName;
    QComboBox *mpBoxCompartmentName;
    QLabel *mpLblNumber;
    QLineEdit *mpEditNumber;
    QCheckBox *mpCheckDiffusion;
    QTableWidget *mpSpeciesTable;
    QHBoxLayout *hboxLayout;
    QPushButton *mpBtnOK;
    QPushButton *mpBtnCancel;

    void setupUi(QDialog *CQExpandModelData)
    {
        if (CQExpandModelData->objectName().isEmpty())
            CQExpandModelData->setObjectName(QString::fromUtf8("CQExpandModelData"));
        CQExpandModelData->resize(623, 291);
        CQExpandModelData->setSizeGripEnabled(true);
        vboxLayout = new QVBoxLayout(CQExpandModelData);
        vboxLayout->setSpacing(6);
        vboxLayout->setContentsMargins(11, 11, 11, 11);
        vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
        frame = new QFrame(CQExpandModelData);
        frame->setObjectName(QString::fromUtf8("frame"));
        mpLblCompartmentName = new QLabel(frame);
        mpLblCompartmentName->setObjectName(QString::fromUtf8("mpLblCompartmentName"));
        mpLblCompartmentName->setGeometry(QRect(9, 9, 139, 16));
        mpLblCompartmentName->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblCompartmentName->setWordWrap(false);
        mpBoxCompartmentName = new QComboBox(frame);
        mpBoxCompartmentName->setObjectName(QString::fromUtf8("mpBoxCompartmentName"));
        mpBoxCompartmentName->setGeometry(QRect(160, 10, 191, 21));
        mpLblNumber = new QLabel(frame);
        mpLblNumber->setObjectName(QString::fromUtf8("mpLblNumber"));
        mpLblNumber->setGeometry(QRect(391, 9, 114, 16));
        mpLblNumber->setMinimumSize(QSize(0, 0));
        mpLblNumber->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblNumber->setWordWrap(false);
        mpEditNumber = new QLineEdit(frame);
        mpEditNumber->setObjectName(QString::fromUtf8("mpEditNumber"));
        mpEditNumber->setEnabled(false);
        mpEditNumber->setGeometry(QRect(511, 9, 85, 22));
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(mpEditNumber->sizePolicy().hasHeightForWidth());
        mpEditNumber->setSizePolicy(sizePolicy);
        mpCheckDiffusion = new QCheckBox(frame);
        mpCheckDiffusion->setObjectName(QString::fromUtf8("mpCheckDiffusion"));
        mpCheckDiffusion->setGeometry(QRect(9, 123, 126, 21));
        mpCheckDiffusion->setLayoutDirection(Qt::LeftToRight);
        mpSpeciesTable = new QTableWidget(frame);
        if (mpSpeciesTable->columnCount() < 2)
            mpSpeciesTable->setColumnCount(2);
        QTableWidgetItem *__qtablewidgetitem = new QTableWidgetItem();
        mpSpeciesTable->setHorizontalHeaderItem(0, __qtablewidgetitem);
        QTableWidgetItem *__qtablewidgetitem1 = new QTableWidgetItem();
        mpSpeciesTable->setHorizontalHeaderItem(1, __qtablewidgetitem1);
        mpSpeciesTable->setObjectName(QString::fromUtf8("mpSpeciesTable"));
        mpSpeciesTable->setGeometry(QRect(154, 38, 230, 192));
        mpSpeciesTable->setSelectionBehavior(QAbstractItemView::SelectRows);
        mpSpeciesTable->setShowGrid(false);
        mpSpeciesTable->setColumnCount(2);

        vboxLayout->addWidget(frame);

        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setContentsMargins(0, 0, 0, 0);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        mpBtnOK = new QPushButton(CQExpandModelData);
        mpBtnOK->setObjectName(QString::fromUtf8("mpBtnOK"));

        hboxLayout->addWidget(mpBtnOK);

        mpBtnCancel = new QPushButton(CQExpandModelData);
        mpBtnCancel->setObjectName(QString::fromUtf8("mpBtnCancel"));

        hboxLayout->addWidget(mpBtnCancel);


        vboxLayout->addLayout(hboxLayout);

        QWidget::setTabOrder(mpBtnOK, mpBtnCancel);

        retranslateUi(CQExpandModelData);
        QObject::connect(mpBtnOK, SIGNAL(clicked()), CQExpandModelData, SLOT(slotOK()));
        QObject::connect(mpBtnCancel, SIGNAL(clicked()), CQExpandModelData, SLOT(slotCancel()));

        QMetaObject::connectSlotsByName(CQExpandModelData);
    } // setupUi

    void retranslateUi(QDialog *CQExpandModelData)
    {
        CQExpandModelData->setWindowTitle(QApplication::translate("CQExpandModelData", "Create array of compartments", 0, QApplication::UnicodeUTF8));
        mpLblCompartmentName->setText(QApplication::translate("CQExpandModelData", "Compartment to copy ", 0, QApplication::UnicodeUTF8));
        mpLblNumber->setText(QApplication::translate("CQExpandModelData", "Number of copies ", 0, QApplication::UnicodeUTF8));
        mpCheckDiffusion->setText(QApplication::translate("CQExpandModelData", "include diffusion", 0, QApplication::UnicodeUTF8));
        QTableWidgetItem *___qtablewidgetitem = mpSpeciesTable->horizontalHeaderItem(0);
        ___qtablewidgetitem->setText(QApplication::translate("CQExpandModelData", "Species", 0, QApplication::UnicodeUTF8));
        QTableWidgetItem *___qtablewidgetitem1 = mpSpeciesTable->horizontalHeaderItem(1);
        ___qtablewidgetitem1->setText(QApplication::translate("CQExpandModelData", "Diffusion", 0, QApplication::UnicodeUTF8));
        mpBtnOK->setText(QApplication::translate("CQExpandModelData", "&OK", 0, QApplication::UnicodeUTF8));
        mpBtnOK->setShortcut(QApplication::translate("CQExpandModelData", "Alt+O", 0, QApplication::UnicodeUTF8));
        mpBtnCancel->setText(QApplication::translate("CQExpandModelData", "&Cancel", 0, QApplication::UnicodeUTF8));
        mpBtnCancel->setShortcut(QApplication::translate("CQExpandModelData", "Alt+C", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQExpandModelData: public Ui_CQExpandModelData {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQEXPANDMODELDATA_H
