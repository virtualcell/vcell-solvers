/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CUpDownSubwidget.ui'
**
** Created: Sun Sep 11 10:59:20 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CUPDOWNSUBWIDGET_H
#define UI_CUPDOWNSUBWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QGridLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QSpacerItem>
#include <QtGui/QToolButton>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_CUpDownSubwidget
{
public:
    QVBoxLayout *vboxLayout;
    QGridLayout *gridLayout;
    QToolButton *toolButtonDel;
    QToolButton *toolButtonUp;
    QToolButton *toolButtonCopy;
    QToolButton *toolButtonDown;
    QSpacerItem *spacer;

    void setupUi(QWidget *CUpDownSubwidget)
    {
        if (CUpDownSubwidget->objectName().isEmpty())
            CUpDownSubwidget->setObjectName(QString::fromUtf8("CUpDownSubwidget"));
        CUpDownSubwidget->resize(120, 60);
        QSizePolicy sizePolicy(QSizePolicy::Minimum, QSizePolicy::Minimum);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(CUpDownSubwidget->sizePolicy().hasHeightForWidth());
        CUpDownSubwidget->setSizePolicy(sizePolicy);
        CUpDownSubwidget->setFocusPolicy(Qt::WheelFocus);
        vboxLayout = new QVBoxLayout(CUpDownSubwidget);
        vboxLayout->setSpacing(0);
        vboxLayout->setContentsMargins(3, 3, 3, 3);
        vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
        gridLayout = new QGridLayout();
        gridLayout->setSpacing(6);
        gridLayout->setContentsMargins(0, 0, 0, 0);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        toolButtonDel = new QToolButton(CUpDownSubwidget);
        toolButtonDel->setObjectName(QString::fromUtf8("toolButtonDel"));
        toolButtonDel->setMaximumSize(QSize(20, 20));
        QIcon icon;
        icon.addFile(QString::fromUtf8("image0"), QSize(), QIcon::Normal, QIcon::Off);
        toolButtonDel->setIcon(icon);

        gridLayout->addWidget(toolButtonDel, 0, 0, 1, 1);

        toolButtonUp = new QToolButton(CUpDownSubwidget);
        toolButtonUp->setObjectName(QString::fromUtf8("toolButtonUp"));
        toolButtonUp->setMaximumSize(QSize(20, 20));
        QIcon icon1;
        icon1.addFile(QString::fromUtf8("image1"), QSize(), QIcon::Normal, QIcon::Off);
        toolButtonUp->setIcon(icon1);

        gridLayout->addWidget(toolButtonUp, 0, 1, 1, 1);

        toolButtonCopy = new QToolButton(CUpDownSubwidget);
        toolButtonCopy->setObjectName(QString::fromUtf8("toolButtonCopy"));
        toolButtonCopy->setMaximumSize(QSize(20, 20));
        QIcon icon2;
        icon2.addFile(QString::fromUtf8("image2"), QSize(), QIcon::Normal, QIcon::Off);
        toolButtonCopy->setIcon(icon2);

        gridLayout->addWidget(toolButtonCopy, 1, 0, 1, 1);

        toolButtonDown = new QToolButton(CUpDownSubwidget);
        toolButtonDown->setObjectName(QString::fromUtf8("toolButtonDown"));
        toolButtonDown->setMaximumSize(QSize(20, 20));
        QIcon icon3;
        icon3.addFile(QString::fromUtf8("image3"), QSize(), QIcon::Normal, QIcon::Off);
        toolButtonDown->setIcon(icon3);

        gridLayout->addWidget(toolButtonDown, 1, 1, 1, 1);


        vboxLayout->addLayout(gridLayout);

        spacer = new QSpacerItem(20, 6, QSizePolicy::Minimum, QSizePolicy::Expanding);

        vboxLayout->addItem(spacer);


        retranslateUi(CUpDownSubwidget);
        QObject::connect(toolButtonDown, SIGNAL(clicked()), CUpDownSubwidget, SLOT(slotDown()));
        QObject::connect(toolButtonUp, SIGNAL(clicked()), CUpDownSubwidget, SLOT(slotUp()));
        QObject::connect(toolButtonDel, SIGNAL(clicked()), CUpDownSubwidget, SLOT(slotDel()));
        QObject::connect(toolButtonCopy, SIGNAL(clicked()), CUpDownSubwidget, SLOT(slotCopy()));

        QMetaObject::connectSlotsByName(CUpDownSubwidget);
    } // setupUi

    void retranslateUi(QWidget *CUpDownSubwidget)
    {
        CUpDownSubwidget->setWindowTitle(QApplication::translate("CUpDownSubwidget", "Form2", 0, QApplication::UnicodeUTF8));
        toolButtonDel->setText(QString());
        toolButtonUp->setText(QString());
        toolButtonCopy->setText(QString());
        toolButtonDown->setText(QString());
    } // retranslateUi

};

namespace Ui {
    class CUpDownSubwidget: public Ui_CUpDownSubwidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CUPDOWNSUBWIDGET_H
