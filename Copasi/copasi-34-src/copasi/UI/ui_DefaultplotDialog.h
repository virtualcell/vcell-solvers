/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'DefaultplotDialog.ui'
**
** Created: Sun Sep 11 10:59:19 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_DEFAULTPLOTDIALOG_H
#define UI_DEFAULTPLOTDIALOG_H

#include <Qt3Support/Q3Frame>
#include <Qt3Support/Q3ListBox>
#include <Qt3Support/Q3MimeSourceFactory>
#include <Qt3Support/Q3TextEdit>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDialog>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <vector>
#include "copasi.h"

QT_BEGIN_NAMESPACE

class Ui_DefaultPlotDialog
{
public:
    QGridLayout *gridLayout;
    Q3ListBox *listBox;
    QLabel *titleLabel;
    QLineEdit *lineEditTitle;
    Q3TextEdit *textEdit;
    QHBoxLayout *hboxLayout;
    QSpacerItem *spacerButtons;
    QPushButton *createButton;
    QPushButton *cancelButton;

    void setupUi(QDialog *DefaultPlotDialog)
    {
        if (DefaultPlotDialog->objectName().isEmpty())
            DefaultPlotDialog->setObjectName(QString::fromUtf8("DefaultPlotDialog"));
        DefaultPlotDialog->resize(586, 383);
        gridLayout = new QGridLayout(DefaultPlotDialog);
        gridLayout->setSpacing(6);
        gridLayout->setContentsMargins(11, 11, 11, 11);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        listBox = new Q3ListBox(DefaultPlotDialog);
        listBox->setObjectName(QString::fromUtf8("listBox"));

        gridLayout->addWidget(listBox, 0, 0, 3, 1);

        titleLabel = new QLabel(DefaultPlotDialog);
        titleLabel->setObjectName(QString::fromUtf8("titleLabel"));
        titleLabel->setWordWrap(false);

        gridLayout->addWidget(titleLabel, 0, 1, 1, 1);

        lineEditTitle = new QLineEdit(DefaultPlotDialog);
        lineEditTitle->setObjectName(QString::fromUtf8("lineEditTitle"));

        gridLayout->addWidget(lineEditTitle, 1, 1, 1, 1);

        textEdit = new Q3TextEdit(DefaultPlotDialog);
        textEdit->setObjectName(QString::fromUtf8("textEdit"));

        gridLayout->addWidget(textEdit, 2, 1, 1, 1);

        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        spacerButtons = new QSpacerItem(51, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout->addItem(spacerButtons);

        createButton = new QPushButton(DefaultPlotDialog);
        createButton->setObjectName(QString::fromUtf8("createButton"));

        hboxLayout->addWidget(createButton);

        cancelButton = new QPushButton(DefaultPlotDialog);
        cancelButton->setObjectName(QString::fromUtf8("cancelButton"));

        hboxLayout->addWidget(cancelButton);


        gridLayout->addLayout(hboxLayout, 3, 0, 1, 2);


        retranslateUi(DefaultPlotDialog);
        QObject::connect(cancelButton, SIGNAL(clicked()), DefaultPlotDialog, SLOT(close()));
        QObject::connect(createButton, SIGNAL(clicked()), DefaultPlotDialog, SLOT(slotCreate()));
        QObject::connect(listBox, SIGNAL(selectionChanged()), DefaultPlotDialog, SLOT(slotSelect()));

        QMetaObject::connectSlotsByName(DefaultPlotDialog);
    } // setupUi

    void retranslateUi(QDialog *DefaultPlotDialog)
    {
        DefaultPlotDialog->setWindowTitle(QApplication::translate("DefaultPlotDialog", "Form1", 0, QApplication::UnicodeUTF8));
        listBox->clear();
        listBox->insertItem(QApplication::translate("DefaultPlotDialog", "New Item", 0, QApplication::UnicodeUTF8));
        titleLabel->setText(QApplication::translate("DefaultPlotDialog", "Title", 0, QApplication::UnicodeUTF8));
        createButton->setText(QApplication::translate("DefaultPlotDialog", "Create!", 0, QApplication::UnicodeUTF8));
        cancelButton->setText(QApplication::translate("DefaultPlotDialog", "Cancel", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class DefaultPlotDialog: public Ui_DefaultPlotDialog {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_DEFAULTPLOTDIALOG_H
