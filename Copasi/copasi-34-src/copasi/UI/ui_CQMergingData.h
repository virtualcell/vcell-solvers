/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQMergingData.ui'
**
** Created: Sun Sep 11 10:59:24 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQMERGINGDATA_H
#define UI_CQMERGINGDATA_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDialog>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QPushButton>
#include <QtGui/QTableWidget>
#include <QtGui/QVBoxLayout>
#include <map>
#include <string>

QT_BEGIN_NAMESPACE

class Ui_CQMergingData
{
public:
    QVBoxLayout *verticalLayout;
    QTableWidget *mpTable;
    QHBoxLayout *_2;
    QPushButton *mpBtnMerge;
    QPushButton *mpBtnCancel;

    void setupUi(QDialog *CQMergingData)
    {
        if (CQMergingData->objectName().isEmpty())
            CQMergingData->setObjectName(QString::fromUtf8("CQMergingData"));
        CQMergingData->resize(654, 209);
        CQMergingData->setSizeGripEnabled(true);
        verticalLayout = new QVBoxLayout(CQMergingData);
        verticalLayout->setSpacing(6);
        verticalLayout->setContentsMargins(11, 11, 11, 11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        mpTable = new QTableWidget(CQMergingData);
        if (mpTable->columnCount() < 6)
            mpTable->setColumnCount(6);
        QTableWidgetItem *__qtablewidgetitem = new QTableWidgetItem();
        mpTable->setHorizontalHeaderItem(0, __qtablewidgetitem);
        QTableWidgetItem *__qtablewidgetitem1 = new QTableWidgetItem();
        mpTable->setHorizontalHeaderItem(1, __qtablewidgetitem1);
        QTableWidgetItem *__qtablewidgetitem2 = new QTableWidgetItem();
        mpTable->setHorizontalHeaderItem(2, __qtablewidgetitem2);
        QTableWidgetItem *__qtablewidgetitem3 = new QTableWidgetItem();
        mpTable->setHorizontalHeaderItem(3, __qtablewidgetitem3);
        QTableWidgetItem *__qtablewidgetitem4 = new QTableWidgetItem();
        mpTable->setHorizontalHeaderItem(4, __qtablewidgetitem4);
        QTableWidgetItem *__qtablewidgetitem5 = new QTableWidgetItem();
        mpTable->setHorizontalHeaderItem(5, __qtablewidgetitem5);
        if (mpTable->rowCount() < 3)
            mpTable->setRowCount(3);
        QTableWidgetItem *__qtablewidgetitem6 = new QTableWidgetItem();
        mpTable->setVerticalHeaderItem(0, __qtablewidgetitem6);
        QTableWidgetItem *__qtablewidgetitem7 = new QTableWidgetItem();
        mpTable->setVerticalHeaderItem(1, __qtablewidgetitem7);
        QTableWidgetItem *__qtablewidgetitem8 = new QTableWidgetItem();
        mpTable->setVerticalHeaderItem(2, __qtablewidgetitem8);
        mpTable->setObjectName(QString::fromUtf8("mpTable"));

        verticalLayout->addWidget(mpTable);

        _2 = new QHBoxLayout();
        _2->setSpacing(6);
        _2->setContentsMargins(0, 0, 0, 0);
        _2->setObjectName(QString::fromUtf8("_2"));
        mpBtnMerge = new QPushButton(CQMergingData);
        mpBtnMerge->setObjectName(QString::fromUtf8("mpBtnMerge"));

        _2->addWidget(mpBtnMerge);

        mpBtnCancel = new QPushButton(CQMergingData);
        mpBtnCancel->setObjectName(QString::fromUtf8("mpBtnCancel"));

        _2->addWidget(mpBtnCancel);


        verticalLayout->addLayout(_2);

        QWidget::setTabOrder(mpBtnMerge, mpBtnCancel);

        retranslateUi(CQMergingData);
        QObject::connect(mpBtnMerge, SIGNAL(clicked()), CQMergingData, SLOT(slotBtnMerge()));
        QObject::connect(mpBtnCancel, SIGNAL(clicked()), CQMergingData, SLOT(slotBtnCancel()));

        QMetaObject::connectSlotsByName(CQMergingData);
    } // setupUi

    void retranslateUi(QDialog *CQMergingData)
    {
        CQMergingData->setWindowTitle(QApplication::translate("CQMergingData", "Merging Data", 0, QApplication::UnicodeUTF8));
        QTableWidgetItem *___qtablewidgetitem = mpTable->horizontalHeaderItem(0);
        ___qtablewidgetitem->setText(QApplication::translate("CQMergingData", "Column Name", 0, QApplication::UnicodeUTF8));
        QTableWidgetItem *___qtablewidgetitem1 = mpTable->horizontalHeaderItem(1);
        ___qtablewidgetitem1->setText(QApplication::translate("CQMergingData", "Status", 0, QApplication::UnicodeUTF8));
        QTableWidgetItem *___qtablewidgetitem2 = mpTable->horizontalHeaderItem(2);
        ___qtablewidgetitem2->setText(QApplication::translate("CQMergingData", "Hidden", 0, QApplication::UnicodeUTF8));
        QTableWidgetItem *___qtablewidgetitem3 = mpTable->horizontalHeaderItem(3);
        ___qtablewidgetitem3->setText(QApplication::translate("CQMergingData", "<>", 0, QApplication::UnicodeUTF8));
        QTableWidgetItem *___qtablewidgetitem4 = mpTable->horizontalHeaderItem(4);
        ___qtablewidgetitem4->setText(QApplication::translate("CQMergingData", "Model Object", 0, QApplication::UnicodeUTF8));
        QTableWidgetItem *___qtablewidgetitem5 = mpTable->horizontalHeaderItem(5);
        ___qtablewidgetitem5->setText(QApplication::translate("CQMergingData", "Hidden", 0, QApplication::UnicodeUTF8));
        QTableWidgetItem *___qtablewidgetitem6 = mpTable->verticalHeaderItem(0);
        ___qtablewidgetitem6->setText(QApplication::translate("CQMergingData", "1", 0, QApplication::UnicodeUTF8));
        QTableWidgetItem *___qtablewidgetitem7 = mpTable->verticalHeaderItem(1);
        ___qtablewidgetitem7->setText(QApplication::translate("CQMergingData", "2", 0, QApplication::UnicodeUTF8));
        QTableWidgetItem *___qtablewidgetitem8 = mpTable->verticalHeaderItem(2);
        ___qtablewidgetitem8->setText(QApplication::translate("CQMergingData", "3", 0, QApplication::UnicodeUTF8));
        mpBtnMerge->setText(QApplication::translate("CQMergingData", "&Merge", 0, QApplication::UnicodeUTF8));
        mpBtnMerge->setShortcut(QApplication::translate("CQMergingData", "Alt+O", 0, QApplication::UnicodeUTF8));
        mpBtnCancel->setText(QApplication::translate("CQMergingData", "&Cancel", 0, QApplication::UnicodeUTF8));
        mpBtnCancel->setShortcut(QApplication::translate("CQMergingData", "Alt+C", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQMergingData: public Ui_CQMergingData {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQMERGINGDATA_H
