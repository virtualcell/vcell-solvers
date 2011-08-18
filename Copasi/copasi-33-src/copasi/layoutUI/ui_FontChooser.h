/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'FontChooser.ui'
**
** Created: Thu Aug 18 12:47:01 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_FONTCHOOSER_H
#define UI_FONTCHOOSER_H

#include <Qt3Support/Q3MimeSourceFactory>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDialog>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include <QtGui/QSpinBox>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_FontChooser
{
public:
    QWidget *Layout1;
    QHBoxLayout *hboxLayout;
    QPushButton *buttonOk;
    QPushButton *buttonCancel;
    QWidget *layout3;
    QHBoxLayout *hboxLayout1;
    QLabel *textLabel1;
    QSpinBox *spinBox1;

    void setupUi(QDialog *FontChooser)
    {
        if (FontChooser->objectName().isEmpty())
            FontChooser->setObjectName(QString::fromUtf8("FontChooser"));
        FontChooser->resize(174, 112);
        FontChooser->setSizeGripEnabled(true);
        Layout1 = new QWidget(FontChooser);
        Layout1->setObjectName(QString::fromUtf8("Layout1"));
        Layout1->setGeometry(QRect(10, 70, 164, 34));
        hboxLayout = new QHBoxLayout(Layout1);
        hboxLayout->setSpacing(6);
        hboxLayout->setContentsMargins(11, 11, 11, 11);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        hboxLayout->setContentsMargins(0, 0, 0, 0);
        buttonOk = new QPushButton(Layout1);
        buttonOk->setObjectName(QString::fromUtf8("buttonOk"));
        buttonOk->setAutoDefault(true);
        buttonOk->setDefault(true);

        hboxLayout->addWidget(buttonOk);

        buttonCancel = new QPushButton(Layout1);
        buttonCancel->setObjectName(QString::fromUtf8("buttonCancel"));
        buttonCancel->setAutoDefault(true);

        hboxLayout->addWidget(buttonCancel);

        layout3 = new QWidget(FontChooser);
        layout3->setObjectName(QString::fromUtf8("layout3"));
        layout3->setGeometry(QRect(10, 20, 150, 50));
        hboxLayout1 = new QHBoxLayout(layout3);
        hboxLayout1->setSpacing(6);
        hboxLayout1->setContentsMargins(11, 11, 11, 11);
        hboxLayout1->setObjectName(QString::fromUtf8("hboxLayout1"));
        hboxLayout1->setContentsMargins(0, 0, 0, 0);
        textLabel1 = new QLabel(layout3);
        textLabel1->setObjectName(QString::fromUtf8("textLabel1"));
        textLabel1->setWordWrap(false);

        hboxLayout1->addWidget(textLabel1);

        spinBox1 = new QSpinBox(layout3);
        spinBox1->setObjectName(QString::fromUtf8("spinBox1"));
        spinBox1->setMinimum(5);
        spinBox1->setMaximum(250);
        spinBox1->setValue(12);

        hboxLayout1->addWidget(spinBox1);


        retranslateUi(FontChooser);
        QObject::connect(buttonOk, SIGNAL(clicked()), FontChooser, SLOT(accept()));
        QObject::connect(buttonCancel, SIGNAL(clicked()), FontChooser, SLOT(reject()));
        QObject::connect(spinBox1, SIGNAL(valueChanged(int)), FontChooser, SLOT(changeFontSize()));

        QMetaObject::connectSlotsByName(FontChooser);
    } // setupUi

    void retranslateUi(QDialog *FontChooser)
    {
        FontChooser->setWindowTitle(QApplication::translate("FontChooser", "FontChooser", 0, QApplication::UnicodeUTF8));
        buttonOk->setText(QApplication::translate("FontChooser", "&OK", 0, QApplication::UnicodeUTF8));
        buttonOk->setShortcut(QString());
        buttonCancel->setText(QApplication::translate("FontChooser", "&Cancel", 0, QApplication::UnicodeUTF8));
        buttonCancel->setShortcut(QString());
        textLabel1->setText(QApplication::translate("FontChooser", "Font Size: ", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class FontChooser: public Ui_FontChooser {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_FONTCHOOSER_H
