/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'NodeSizePanel.ui'
**
** Created: Thu Aug 18 12:47:01 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_NODESIZEPANEL_H
#define UI_NODESIZEPANEL_H

#include <Qt3Support/Q3MimeSourceFactory>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDialog>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>

QT_BEGIN_NAMESPACE

class Ui_NodeSizePanel
{
public:
    QPushButton *pushButton2;
    QLabel *textLabel2;
    QLineEdit *lineEdit2;
    QPushButton *pushButton1;
    QLineEdit *lineEdit1;
    QLabel *textLabel1;

    void setupUi(QDialog *NodeSizePanel)
    {
        if (NodeSizePanel->objectName().isEmpty())
            NodeSizePanel->setObjectName(QString::fromUtf8("NodeSizePanel"));
        NodeSizePanel->resize(224, 144);
        pushButton2 = new QPushButton(NodeSizePanel);
        pushButton2->setObjectName(QString::fromUtf8("pushButton2"));
        pushButton2->setGeometry(QRect(110, 100, 81, 24));
        textLabel2 = new QLabel(NodeSizePanel);
        textLabel2->setObjectName(QString::fromUtf8("textLabel2"));
        textLabel2->setGeometry(QRect(10, 60, 130, 20));
        textLabel2->setWordWrap(false);
        lineEdit2 = new QLineEdit(NodeSizePanel);
        lineEdit2->setObjectName(QString::fromUtf8("lineEdit2"));
        lineEdit2->setGeometry(QRect(140, 60, 40, 22));
        pushButton1 = new QPushButton(NodeSizePanel);
        pushButton1->setObjectName(QString::fromUtf8("pushButton1"));
        pushButton1->setGeometry(QRect(10, 100, 70, 24));
        lineEdit1 = new QLineEdit(NodeSizePanel);
        lineEdit1->setObjectName(QString::fromUtf8("lineEdit1"));
        lineEdit1->setGeometry(QRect(140, 20, 40, 22));
        textLabel1 = new QLabel(NodeSizePanel);
        textLabel1->setObjectName(QString::fromUtf8("textLabel1"));
        textLabel1->setGeometry(QRect(10, 10, 125, 41));
        textLabel1->setWordWrap(false);

        retranslateUi(NodeSizePanel);
        QObject::connect(pushButton1, SIGNAL(clicked()), NodeSizePanel, SLOT(setMinAndMaxValues()));
        QObject::connect(pushButton2, SIGNAL(clicked()), NodeSizePanel, SLOT(cancel()));
        QObject::connect(lineEdit1, SIGNAL(returnPressed()), NodeSizePanel, SLOT(setMinAndMaxValues()));
        QObject::connect(lineEdit2, SIGNAL(returnPressed()), NodeSizePanel, SLOT(setMinAndMaxValues()));

        QMetaObject::connectSlotsByName(NodeSizePanel);
    } // setupUi

    void retranslateUi(QDialog *NodeSizePanel)
    {
        NodeSizePanel->setWindowTitle(QApplication::translate("NodeSizePanel", "Set Min/Max Node Sizes", 0, QApplication::UnicodeUTF8));
        pushButton2->setText(QApplication::translate("NodeSizePanel", "CANCEL", 0, QApplication::UnicodeUTF8));
        textLabel2->setText(QApplication::translate("NodeSizePanel", "Maximum Node Size:", 0, QApplication::UnicodeUTF8));
        lineEdit2->setText(QApplication::translate("NodeSizePanel", "100", 0, QApplication::UnicodeUTF8));
        pushButton1->setText(QApplication::translate("NodeSizePanel", "OK", 0, QApplication::UnicodeUTF8));
        lineEdit1->setText(QApplication::translate("NodeSizePanel", "10", 0, QApplication::UnicodeUTF8));
        textLabel1->setText(QApplication::translate("NodeSizePanel", "Minimum Node Size:", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class NodeSizePanel: public Ui_NodeSizePanel {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_NODESIZEPANEL_H
