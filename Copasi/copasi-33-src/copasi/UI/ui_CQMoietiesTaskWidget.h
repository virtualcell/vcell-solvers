/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQMoietiesTaskWidget.ui'
**
** Created: Thu Aug 18 12:47:31 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQMOIETIESTASKWIDGET_H
#define UI_CQMOIETIESTASKWIDGET_H

#include <Qt3Support/Q3MimeSourceFactory>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QHeaderView>
#include <QtGui/QSpacerItem>
#include <QtGui/QVBoxLayout>
#include "TaskWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQMoietiesTaskWidget
{
public:
    QVBoxLayout *vboxLayout;
    QSpacerItem *mpSpacer;

    void setupUi(TaskWidget *CQMoietiesTaskWidget)
    {
        if (CQMoietiesTaskWidget->objectName().isEmpty())
            CQMoietiesTaskWidget->setObjectName(QString::fromUtf8("CQMoietiesTaskWidget"));
        CQMoietiesTaskWidget->resize(260, 23);
        vboxLayout = new QVBoxLayout(CQMoietiesTaskWidget);
        vboxLayout->setSpacing(6);
        vboxLayout->setContentsMargins(11, 11, 11, 11);
        vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
        mpSpacer = new QSpacerItem(20, 40, QSizePolicy::Minimum, QSizePolicy::Expanding);

        vboxLayout->addItem(mpSpacer);


        retranslateUi(CQMoietiesTaskWidget);

        QMetaObject::connectSlotsByName(CQMoietiesTaskWidget);
    } // setupUi

    void retranslateUi(TaskWidget *CQMoietiesTaskWidget)
    {
        CQMoietiesTaskWidget->setProperty("caption", QVariant(QApplication::translate("CQMoietiesTaskWidget", "Mass Conservation", 0, QApplication::UnicodeUTF8)));
    } // retranslateUi

};

namespace Ui {
    class CQMoietiesTaskWidget: public Ui_CQMoietiesTaskWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQMOIETIESTASKWIDGET_H
