/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQPreferenceDialog.ui'
**
** Created: Thu Aug 18 12:47:30 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQPREFERENCEDIALOG_H
#define UI_CQPREFERENCEDIALOG_H

#include <Qt3Support/Q3MimeSourceFactory>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDialog>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QPushButton>
#include <QtGui/QTreeWidget>
#include <QtGui/QVBoxLayout>

QT_BEGIN_NAMESPACE

class Ui_CQPreferenceDialog
{
public:
    QVBoxLayout *verticalLayout;
    QTreeWidget *mpTreeWidget;
    QHBoxLayout *mpBtnLayout;
    QPushButton *mpBtnOk;
    QPushButton *mpBtnCancel;

    void setupUi(QDialog *CQPreferenceDialog)
    {
        if (CQPreferenceDialog->objectName().isEmpty())
            CQPreferenceDialog->setObjectName(QString::fromUtf8("CQPreferenceDialog"));
        CQPreferenceDialog->resize(379, 243);
        CQPreferenceDialog->setSizeGripEnabled(true);
        verticalLayout = new QVBoxLayout(CQPreferenceDialog);
        verticalLayout->setSpacing(6);
        verticalLayout->setContentsMargins(11, 11, 11, 11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        mpTreeWidget = new QTreeWidget(CQPreferenceDialog);
        mpTreeWidget->setObjectName(QString::fromUtf8("mpTreeWidget"));

        verticalLayout->addWidget(mpTreeWidget);

        mpBtnLayout = new QHBoxLayout();
        mpBtnLayout->setSpacing(6);
        mpBtnLayout->setObjectName(QString::fromUtf8("mpBtnLayout"));
        mpBtnOk = new QPushButton(CQPreferenceDialog);
        mpBtnOk->setObjectName(QString::fromUtf8("mpBtnOk"));
        mpBtnOk->setAutoDefault(true);
        mpBtnOk->setDefault(true);

        mpBtnLayout->addWidget(mpBtnOk);

        mpBtnCancel = new QPushButton(CQPreferenceDialog);
        mpBtnCancel->setObjectName(QString::fromUtf8("mpBtnCancel"));
        mpBtnCancel->setAutoDefault(true);

        mpBtnLayout->addWidget(mpBtnCancel);


        verticalLayout->addLayout(mpBtnLayout);

        QWidget::setTabOrder(mpBtnOk, mpBtnCancel);

        retranslateUi(CQPreferenceDialog);
        QObject::connect(mpBtnOk, SIGNAL(clicked()), CQPreferenceDialog, SLOT(slotBtnOk()));
        QObject::connect(mpBtnCancel, SIGNAL(clicked()), CQPreferenceDialog, SLOT(slotBtnCancel()));
        QObject::connect(mpTreeWidget, SIGNAL(itemDoubleClicked(QTreeWidgetItem*,int)), CQPreferenceDialog, SLOT(slotItemDoubleClicked(QTreeWidgetItem*,int)));

        QMetaObject::connectSlotsByName(CQPreferenceDialog);
    } // setupUi

    void retranslateUi(QDialog *CQPreferenceDialog)
    {
        CQPreferenceDialog->setWindowTitle(QApplication::translate("CQPreferenceDialog", "Preferences", 0, QApplication::UnicodeUTF8));
        QTreeWidgetItem *___qtreewidgetitem = mpTreeWidget->headerItem();
        ___qtreewidgetitem->setText(1, QApplication::translate("CQPreferenceDialog", "Value", 0, QApplication::UnicodeUTF8));
        ___qtreewidgetitem->setText(0, QApplication::translate("CQPreferenceDialog", "Name", 0, QApplication::UnicodeUTF8));
        mpBtnOk->setText(QApplication::translate("CQPreferenceDialog", "&OK", 0, QApplication::UnicodeUTF8));
        mpBtnOk->setShortcut(QString());
        mpBtnCancel->setText(QApplication::translate("CQPreferenceDialog", "&Cancel", 0, QApplication::UnicodeUTF8));
        mpBtnCancel->setShortcut(QString());
    } // retranslateUi

};

namespace Ui {
    class CQPreferenceDialog: public Ui_CQPreferenceDialog {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQPREFERENCEDIALOG_H
