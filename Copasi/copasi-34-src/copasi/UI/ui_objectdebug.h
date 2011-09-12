/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'objectdebug.ui'
**
** Created: Sun Sep 11 10:59:19 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_OBJECTDEBUG_H
#define UI_OBJECTDEBUG_H

#include <Qt3Support/Q3Frame>
#include <Qt3Support/Q3Header>
#include <Qt3Support/Q3ListView>
#include <Qt3Support/Q3MimeSourceFactory>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDialog>
#include <QtGui/QGridLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <iostream>

QT_BEGIN_NAMESPACE

class Ui_ObjectDebug
{
public:
    QGridLayout *gridLayout;
    QPushButton *UpdateButton;
    Q3ListView *ListOfObjects;
    QPushButton *pushButton2;
    QSpacerItem *spacer5;
    QPushButton *ButtonModelCheck;

    void setupUi(QDialog *ObjectDebug)
    {
        if (ObjectDebug->objectName().isEmpty())
            ObjectDebug->setObjectName(QString::fromUtf8("ObjectDebug"));
        ObjectDebug->resize(517, 486);
        gridLayout = new QGridLayout(ObjectDebug);
        gridLayout->setSpacing(6);
        gridLayout->setContentsMargins(11, 11, 11, 11);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        UpdateButton = new QPushButton(ObjectDebug);
        UpdateButton->setObjectName(QString::fromUtf8("UpdateButton"));

        gridLayout->addWidget(UpdateButton, 1, 3, 1, 1);

        ListOfObjects = new Q3ListView(ObjectDebug);
        ListOfObjects->addColumn(QApplication::translate("ObjectDebug", "Column 1", 0, QApplication::UnicodeUTF8));
        ListOfObjects->header()->setClickEnabled(true, ListOfObjects->header()->count() - 1);
        ListOfObjects->header()->setResizeEnabled(true, ListOfObjects->header()->count() - 1);
        ListOfObjects->setObjectName(QString::fromUtf8("ListOfObjects"));

        gridLayout->addWidget(ListOfObjects, 0, 0, 1, 4);

        pushButton2 = new QPushButton(ObjectDebug);
        pushButton2->setObjectName(QString::fromUtf8("pushButton2"));

        gridLayout->addWidget(pushButton2, 1, 2, 1, 1);

        spacer5 = new QSpacerItem(90, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        gridLayout->addItem(spacer5, 1, 0, 1, 1);

        ButtonModelCheck = new QPushButton(ObjectDebug);
        ButtonModelCheck->setObjectName(QString::fromUtf8("ButtonModelCheck"));

        gridLayout->addWidget(ButtonModelCheck, 1, 1, 1, 1);


        retranslateUi(ObjectDebug);
        QObject::connect(UpdateButton, SIGNAL(clicked()), ObjectDebug, SLOT(update()));
        QObject::connect(pushButton2, SIGNAL(clicked()), ObjectDebug, SLOT(writeDot()));
        QObject::connect(ButtonModelCheck, SIGNAL(clicked()), ObjectDebug, SLOT(checkModel()));

        QMetaObject::connectSlotsByName(ObjectDebug);
    } // setupUi

    void retranslateUi(QDialog *ObjectDebug)
    {
        ObjectDebug->setWindowTitle(QApplication::translate("ObjectDebug", "Copasi Object Tree", 0, QApplication::UnicodeUTF8));
        UpdateButton->setText(QApplication::translate("ObjectDebug", "update", 0, QApplication::UnicodeUTF8));
        ListOfObjects->header()->setLabel(0, QApplication::translate("ObjectDebug", "Column 1", 0, QApplication::UnicodeUTF8));
        ListOfObjects->clear();

        Q3ListViewItem *__item = new Q3ListViewItem(ListOfObjects);
        __item->setText(0, QApplication::translate("ObjectDebug", "New Item", 0, QApplication::UnicodeUTF8));
        pushButton2->setText(QApplication::translate("ObjectDebug", "write Dependencies", 0, QApplication::UnicodeUTF8));
        ButtonModelCheck->setText(QApplication::translate("ObjectDebug", "check model", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class ObjectDebug: public Ui_ObjectDebug {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_OBJECTDEBUG_H
