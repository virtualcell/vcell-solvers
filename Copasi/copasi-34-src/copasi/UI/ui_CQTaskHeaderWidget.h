/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQTaskHeaderWidget.ui'
**
** Created: Sun Sep 11 10:59:20 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQTASKHEADERWIDGET_H
#define UI_CQTASKHEADERWIDGET_H

#include <Qt3Support/Q3MimeSourceFactory>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QSpacerItem>
#include <QtGui/QWidget>
#include <string>

QT_BEGIN_NAMESPACE

class Ui_CQTaskHeaderWidget
{
public:
    QHBoxLayout *horizontalLayout;
    QLabel *mpLblName;
    QSpacerItem *mpSpacer;
    QCheckBox *mpUpdateModel;
    QCheckBox *mpBoxExecutable;

    void setupUi(QWidget *CQTaskHeaderWidget)
    {
        if (CQTaskHeaderWidget->objectName().isEmpty())
            CQTaskHeaderWidget->setObjectName(QString::fromUtf8("CQTaskHeaderWidget"));
        CQTaskHeaderWidget->resize(346, 50);
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(CQTaskHeaderWidget->sizePolicy().hasHeightForWidth());
        CQTaskHeaderWidget->setSizePolicy(sizePolicy);
        horizontalLayout = new QHBoxLayout(CQTaskHeaderWidget);
        horizontalLayout->setSpacing(6);
        horizontalLayout->setContentsMargins(11, 11, 11, 11);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        horizontalLayout->setContentsMargins(0, 6, -1, 0);
        mpLblName = new QLabel(CQTaskHeaderWidget);
        mpLblName->setObjectName(QString::fromUtf8("mpLblName"));
        QSizePolicy sizePolicy1(QSizePolicy::MinimumExpanding, QSizePolicy::Preferred);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(mpLblName->sizePolicy().hasHeightForWidth());
        mpLblName->setSizePolicy(sizePolicy1);
        mpLblName->setWordWrap(false);

        horizontalLayout->addWidget(mpLblName);

        mpSpacer = new QSpacerItem(20, 20, QSizePolicy::Maximum, QSizePolicy::Minimum);

        horizontalLayout->addItem(mpSpacer);

        mpUpdateModel = new QCheckBox(CQTaskHeaderWidget);
        mpUpdateModel->setObjectName(QString::fromUtf8("mpUpdateModel"));

        horizontalLayout->addWidget(mpUpdateModel);

        mpBoxExecutable = new QCheckBox(CQTaskHeaderWidget);
        mpBoxExecutable->setObjectName(QString::fromUtf8("mpBoxExecutable"));

        horizontalLayout->addWidget(mpBoxExecutable);


        retranslateUi(CQTaskHeaderWidget);
        QObject::connect(mpBoxExecutable, SIGNAL(toggled(bool)), CQTaskHeaderWidget, SLOT(slotExecutable()));
        QObject::connect(mpUpdateModel, SIGNAL(toggled(bool)), CQTaskHeaderWidget, SLOT(slotUpdate()));

        QMetaObject::connectSlotsByName(CQTaskHeaderWidget);
    } // setupUi

    void retranslateUi(QWidget *CQTaskHeaderWidget)
    {
        CQTaskHeaderWidget->setWindowTitle(QApplication::translate("CQTaskHeaderWidget", "CQTaskHeaderWidget", 0, QApplication::UnicodeUTF8));
        mpLblName->setText(QApplication::translate("CQTaskHeaderWidget", "<h2>Task</h2>", 0, QApplication::UnicodeUTF8));
        mpUpdateModel->setText(QApplication::translate("CQTaskHeaderWidget", "update model", 0, QApplication::UnicodeUTF8));
        mpBoxExecutable->setText(QApplication::translate("CQTaskHeaderWidget", "executable", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQTaskHeaderWidget: public Ui_CQTaskHeaderWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQTASKHEADERWIDGET_H
