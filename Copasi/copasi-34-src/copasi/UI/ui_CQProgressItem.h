/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQProgressItem.ui'
**
** Created: Sun Sep 11 10:59:23 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQPROGRESSITEM_H
#define UI_CQPROGRESSITEM_H

#include <Qt3Support/Q3MimeSourceFactory>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QWidget>
#include "utilities/CProcessReport.h"
#include "utilities/CVector.h"

QT_BEGIN_NAMESPACE

class Ui_CQProgressItem
{
public:
    QWidget *mLabel;
    QHBoxLayout *hboxLayout;

    void setupUi(QWidget *CQProgressItem)
    {
        if (CQProgressItem->objectName().isEmpty())
            CQProgressItem->setObjectName(QString::fromUtf8("CQProgressItem"));
        CQProgressItem->resize(404, 44);
        mLabel = new QWidget(CQProgressItem);
        mLabel->setObjectName(QString::fromUtf8("mLabel"));
        mLabel->setGeometry(QRect(10, 10, 330, 22));
        hboxLayout = new QHBoxLayout(mLabel);
        hboxLayout->setSpacing(6);
        hboxLayout->setContentsMargins(11, 11, 11, 11);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        hboxLayout->setContentsMargins(0, 0, 0, 0);

        retranslateUi(CQProgressItem);

        QMetaObject::connectSlotsByName(CQProgressItem);
    } // setupUi

    void retranslateUi(QWidget *CQProgressItem)
    {
        CQProgressItem->setWindowTitle(QApplication::translate("CQProgressItem", "Form1", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQProgressItem: public Ui_CQProgressItem {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQPROGRESSITEM_H
