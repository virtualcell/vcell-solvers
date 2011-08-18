/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'wizard.ui'
**
** Created: Thu Aug 18 13:25:01 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_WIZARD_H
#define UI_WIZARD_H

#include <Qt3Support/Q3MimeSourceFactory>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDialog>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QTextBrowser>

QT_BEGIN_NAMESPACE

class Ui_WizardDialog
{
public:
    QGridLayout *gridLayout;
    QPushButton *button1;
    QTextBrowser *textBrowser;
    QPushButton *button2;
    QPushButton *button3;
    QPushButton *button4;
    QPushButton *button5;
    QPushButton *button6;
    QSpacerItem *verticalSpacer;
    QHBoxLayout *hboxLayout;
    QPushButton *backButton;
    QPushButton *forwardButton;
    QSpacerItem *spacer1;
    QPushButton *cancelButton;

    void setupUi(QDialog *WizardDialog)
    {
        if (WizardDialog->objectName().isEmpty())
            WizardDialog->setObjectName(QString::fromUtf8("WizardDialog"));
        WizardDialog->resize(635, 565);
        WizardDialog->setModal(false);
        gridLayout = new QGridLayout(WizardDialog);
        gridLayout->setSpacing(6);
        gridLayout->setContentsMargins(11, 11, 11, 11);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        button1 = new QPushButton(WizardDialog);
        button1->setObjectName(QString::fromUtf8("button1"));
        button1->setEnabled(true);
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(button1->sizePolicy().hasHeightForWidth());
        button1->setSizePolicy(sizePolicy);
        button1->setFocusPolicy(Qt::NoFocus);
        button1->setCheckable(true);
        button1->setChecked(true);
        button1->setAutoDefault(false);
        button1->setDefault(false);
        button1->setFlat(true);

        gridLayout->addWidget(button1, 0, 0, 1, 1);

        textBrowser = new QTextBrowser(WizardDialog);
        textBrowser->setObjectName(QString::fromUtf8("textBrowser"));

        gridLayout->addWidget(textBrowser, 0, 1, 7, 1);

        button2 = new QPushButton(WizardDialog);
        button2->setObjectName(QString::fromUtf8("button2"));
        sizePolicy.setHeightForWidth(button2->sizePolicy().hasHeightForWidth());
        button2->setSizePolicy(sizePolicy);
        button2->setCheckable(true);
        button2->setAutoDefault(false);
        button2->setFlat(true);

        gridLayout->addWidget(button2, 1, 0, 1, 1);

        button3 = new QPushButton(WizardDialog);
        button3->setObjectName(QString::fromUtf8("button3"));
        sizePolicy.setHeightForWidth(button3->sizePolicy().hasHeightForWidth());
        button3->setSizePolicy(sizePolicy);
        button3->setCheckable(true);
        button3->setAutoDefault(false);
        button3->setFlat(true);

        gridLayout->addWidget(button3, 2, 0, 1, 1);

        button4 = new QPushButton(WizardDialog);
        button4->setObjectName(QString::fromUtf8("button4"));
        sizePolicy.setHeightForWidth(button4->sizePolicy().hasHeightForWidth());
        button4->setSizePolicy(sizePolicy);
        button4->setCheckable(true);
        button4->setAutoDefault(false);
        button4->setFlat(true);

        gridLayout->addWidget(button4, 3, 0, 1, 1);

        button5 = new QPushButton(WizardDialog);
        button5->setObjectName(QString::fromUtf8("button5"));
        sizePolicy.setHeightForWidth(button5->sizePolicy().hasHeightForWidth());
        button5->setSizePolicy(sizePolicy);
        button5->setCheckable(true);
        button5->setAutoDefault(false);
        button5->setFlat(true);

        gridLayout->addWidget(button5, 4, 0, 1, 1);

        button6 = new QPushButton(WizardDialog);
        button6->setObjectName(QString::fromUtf8("button6"));
        sizePolicy.setHeightForWidth(button6->sizePolicy().hasHeightForWidth());
        button6->setSizePolicy(sizePolicy);
        button6->setCheckable(true);
        button6->setAutoDefault(false);
        button6->setFlat(true);

        gridLayout->addWidget(button6, 5, 0, 1, 1);

        verticalSpacer = new QSpacerItem(20, 40, QSizePolicy::Minimum, QSizePolicy::Expanding);

        gridLayout->addItem(verticalSpacer, 6, 0, 1, 1);

        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        backButton = new QPushButton(WizardDialog);
        backButton->setObjectName(QString::fromUtf8("backButton"));
        backButton->setEnabled(false);
        backButton->setAutoDefault(false);

        hboxLayout->addWidget(backButton);

        forwardButton = new QPushButton(WizardDialog);
        forwardButton->setObjectName(QString::fromUtf8("forwardButton"));
        forwardButton->setAutoDefault(false);

        hboxLayout->addWidget(forwardButton);

        spacer1 = new QSpacerItem(310, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout->addItem(spacer1);

        cancelButton = new QPushButton(WizardDialog);
        cancelButton->setObjectName(QString::fromUtf8("cancelButton"));
        cancelButton->setAutoDefault(false);

        hboxLayout->addWidget(cancelButton);


        gridLayout->addLayout(hboxLayout, 7, 0, 1, 2);


        retranslateUi(WizardDialog);
        QObject::connect(forwardButton, SIGNAL(clicked()), WizardDialog, SLOT(forwardButton_clicked()));
        QObject::connect(backButton, SIGNAL(clicked()), WizardDialog, SLOT(backButton_clicked()));
        QObject::connect(cancelButton, SIGNAL(clicked()), WizardDialog, SLOT(close()));

        QMetaObject::connectSlotsByName(WizardDialog);
    } // setupUi

    void retranslateUi(QDialog *WizardDialog)
    {
        WizardDialog->setWindowTitle(QApplication::translate("WizardDialog", "Tutorial", 0, QApplication::UnicodeUTF8));
        button1->setText(QApplication::translate("WizardDialog", "Step 1\n"
"Naming and Description", 0, QApplication::UnicodeUTF8));
        button2->setText(QApplication::translate("WizardDialog", "Step 2\n"
"Creating Reactions", 0, QApplication::UnicodeUTF8));
        button3->setText(QApplication::translate("WizardDialog", "Step 3\n"
"Refining the Model", 0, QApplication::UnicodeUTF8));
        button4->setText(QApplication::translate("WizardDialog", "Step 4\n"
"Calculating a Time Course", 0, QApplication::UnicodeUTF8));
        button5->setText(QApplication::translate("WizardDialog", "Step 5\n"
"Defining the Output", 0, QApplication::UnicodeUTF8));
        button6->setText(QApplication::translate("WizardDialog", "Step 6\n"
"Plotting a Trajectory", 0, QApplication::UnicodeUTF8));
        backButton->setText(QApplication::translate("WizardDialog", "<< back", 0, QApplication::UnicodeUTF8));
        forwardButton->setText(QApplication::translate("WizardDialog", "forward >>", 0, QApplication::UnicodeUTF8));
        cancelButton->setText(QApplication::translate("WizardDialog", "Cancel", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class WizardDialog: public Ui_WizardDialog {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_WIZARD_H
