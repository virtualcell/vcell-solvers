/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQRDFTreeView.ui'
**
** Created: Sun Sep 11 10:59:19 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQRDFTREEVIEW_H
#define UI_CQRDFTREEVIEW_H

#include <Qt3Support/Q3MimeSourceFactory>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QTreeWidget>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_CQRDFTreeView
{
public:
    QHBoxLayout *horizontalLayout;
    QTreeWidget *mpTreeWidget;

    void setupUi(QWidget *CQRDFTreeView)
    {
        if (CQRDFTreeView->objectName().isEmpty())
            CQRDFTreeView->setObjectName(QString::fromUtf8("CQRDFTreeView"));
        CQRDFTreeView->resize(600, 480);
        horizontalLayout = new QHBoxLayout(CQRDFTreeView);
        horizontalLayout->setSpacing(6);
        horizontalLayout->setContentsMargins(11, 11, 11, 11);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        mpTreeWidget = new QTreeWidget(CQRDFTreeView);
        mpTreeWidget->setObjectName(QString::fromUtf8("mpTreeWidget"));

        horizontalLayout->addWidget(mpTreeWidget);


        retranslateUi(CQRDFTreeView);

        QMetaObject::connectSlotsByName(CQRDFTreeView);
    } // setupUi

    void retranslateUi(QWidget *CQRDFTreeView)
    {
        CQRDFTreeView->setProperty("caption", QVariant(QApplication::translate("CQRDFTreeView", "CQRDFListViewWidget", 0, QApplication::UnicodeUTF8)));
        QTreeWidgetItem *___qtreewidgetitem = mpTreeWidget->headerItem();
        ___qtreewidgetitem->setText(2, QApplication::translate("CQRDFTreeView", "Object", 0, QApplication::UnicodeUTF8));
        ___qtreewidgetitem->setText(1, QApplication::translate("CQRDFTreeView", "Predicate", 0, QApplication::UnicodeUTF8));
        ___qtreewidgetitem->setText(0, QApplication::translate("CQRDFTreeView", "Subject", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQRDFTreeView: public Ui_CQRDFTreeView {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQRDFTREEVIEW_H
