/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQExperimentSelection.ui'
**
** Created: Sun Sep 11 10:59:25 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQEXPERIMENTSELECTION_H
#define UI_CQEXPERIMENTSELECTION_H

#include <Qt3Support/Q3Frame>
#include <Qt3Support/Q3Header>
#include <Qt3Support/Q3MimeSourceFactory>
#include <Qt3Support/Q3Table>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDialog>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QVBoxLayout>

QT_BEGIN_NAMESPACE

class Ui_CQExperimentSelection
{
public:
    QVBoxLayout *vboxLayout;
    QHBoxLayout *hboxLayout;
    QPushButton *mpBtnAll;
    QPushButton *mpBtnNone;
    QHBoxLayout *hboxLayout1;
    Q3Table *mpTable;
    QSpacerItem *spacer10;
    QHBoxLayout *hboxLayout2;
    QPushButton *mpBtnOK;
    QPushButton *mpBtnCancel;

    void setupUi(QDialog *CQExperimentSelection)
    {
        if (CQExperimentSelection->objectName().isEmpty())
            CQExperimentSelection->setObjectName(QString::fromUtf8("CQExperimentSelection"));
        CQExperimentSelection->resize(194, 180);
        vboxLayout = new QVBoxLayout(CQExperimentSelection);
        vboxLayout->setSpacing(6);
        vboxLayout->setContentsMargins(11, 11, 11, 11);
        vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        mpBtnAll = new QPushButton(CQExperimentSelection);
        mpBtnAll->setObjectName(QString::fromUtf8("mpBtnAll"));

        hboxLayout->addWidget(mpBtnAll);

        mpBtnNone = new QPushButton(CQExperimentSelection);
        mpBtnNone->setObjectName(QString::fromUtf8("mpBtnNone"));

        hboxLayout->addWidget(mpBtnNone);


        vboxLayout->addLayout(hboxLayout);

        hboxLayout1 = new QHBoxLayout();
        hboxLayout1->setSpacing(6);
        hboxLayout1->setObjectName(QString::fromUtf8("hboxLayout1"));
        mpTable = new Q3Table(CQExperimentSelection);
        mpTable->setObjectName(QString::fromUtf8("mpTable"));
        QSizePolicy sizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(mpTable->sizePolicy().hasHeightForWidth());
        mpTable->setSizePolicy(sizePolicy);
        mpTable->setLineWidth(0);
        mpTable->setNumRows(3);
        mpTable->setNumCols(1);
        mpTable->setShowGrid(false);
        mpTable->setSelectionMode(Q3Table::NoSelection);

        hboxLayout1->addWidget(mpTable);


        vboxLayout->addLayout(hboxLayout1);

        spacer10 = new QSpacerItem(20, 1, QSizePolicy::Minimum, QSizePolicy::Expanding);

        vboxLayout->addItem(spacer10);

        hboxLayout2 = new QHBoxLayout();
        hboxLayout2->setSpacing(6);
        hboxLayout2->setObjectName(QString::fromUtf8("hboxLayout2"));
        mpBtnOK = new QPushButton(CQExperimentSelection);
        mpBtnOK->setObjectName(QString::fromUtf8("mpBtnOK"));

        hboxLayout2->addWidget(mpBtnOK);

        mpBtnCancel = new QPushButton(CQExperimentSelection);
        mpBtnCancel->setObjectName(QString::fromUtf8("mpBtnCancel"));

        hboxLayout2->addWidget(mpBtnCancel);


        vboxLayout->addLayout(hboxLayout2);


        retranslateUi(CQExperimentSelection);
        QObject::connect(mpBtnAll, SIGNAL(clicked()), CQExperimentSelection, SLOT(slotBtnAll()));
        QObject::connect(mpBtnNone, SIGNAL(clicked()), CQExperimentSelection, SLOT(slotBtnNone()));
        QObject::connect(mpBtnCancel, SIGNAL(clicked()), CQExperimentSelection, SLOT(slotBtnCancel()));
        QObject::connect(mpBtnOK, SIGNAL(clicked()), CQExperimentSelection, SLOT(slotBtnOK()));

        QMetaObject::connectSlotsByName(CQExperimentSelection);
    } // setupUi

    void retranslateUi(QDialog *CQExperimentSelection)
    {
        CQExperimentSelection->setWindowTitle(QApplication::translate("CQExperimentSelection", "CQExperimentSelection", 0, QApplication::UnicodeUTF8));
        mpBtnAll->setText(QApplication::translate("CQExperimentSelection", "Select All", 0, QApplication::UnicodeUTF8));
        mpBtnNone->setText(QApplication::translate("CQExperimentSelection", "Deselect All", 0, QApplication::UnicodeUTF8));
        mpTable->horizontalHeader()->setLabel(0, QApplication::translate("CQExperimentSelection", "1", 0, QApplication::UnicodeUTF8));
        mpTable->verticalHeader()->setLabel(0, QApplication::translate("CQExperimentSelection", "1", 0, QApplication::UnicodeUTF8));
        mpTable->verticalHeader()->setLabel(1, QApplication::translate("CQExperimentSelection", "2", 0, QApplication::UnicodeUTF8));
        mpTable->verticalHeader()->setLabel(2, QApplication::translate("CQExperimentSelection", "3", 0, QApplication::UnicodeUTF8));
        mpBtnOK->setText(QApplication::translate("CQExperimentSelection", "&OK", 0, QApplication::UnicodeUTF8));
        mpBtnOK->setShortcut(QApplication::translate("CQExperimentSelection", "Alt+O", 0, QApplication::UnicodeUTF8));
        mpBtnCancel->setText(QApplication::translate("CQExperimentSelection", "&Cancel", 0, QApplication::UnicodeUTF8));
        mpBtnCancel->setShortcut(QApplication::translate("CQExperimentSelection", "Alt+C", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQExperimentSelection: public Ui_CQExperimentSelection {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQEXPERIMENTSELECTION_H
