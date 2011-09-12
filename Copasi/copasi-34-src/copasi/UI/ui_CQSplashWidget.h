/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQSplashWidget.ui'
**
** Created: Sun Sep 11 10:59:21 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQSPLASHWIDGET_H
#define UI_CQSPLASHWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QVBoxLayout>
#include "copasi/UI/copasiWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQSplashWidget
{
public:
    QVBoxLayout *vboxLayout;
    QLabel *mpPixmap;
    QLabel *mpLblVersion;
    QSpacerItem *mpSpacer;
    QHBoxLayout *hboxLayout;
    QLabel *mpLblLicense;
    QPushButton *mpBtnLicense;

    void setupUi(CopasiWidget *CQSplashWidget)
    {
        if (CQSplashWidget->objectName().isEmpty())
            CQSplashWidget->setObjectName(QString::fromUtf8("CQSplashWidget"));
        CQSplashWidget->resize(367, 444);
        vboxLayout = new QVBoxLayout(CQSplashWidget);
        vboxLayout->setSpacing(6);
        vboxLayout->setContentsMargins(11, 11, 11, 11);
        vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
        mpPixmap = new QLabel(CQSplashWidget);
        mpPixmap->setObjectName(QString::fromUtf8("mpPixmap"));
        mpPixmap->setPixmap(QPixmap(QString::fromUtf8("image0")));
        mpPixmap->setScaledContents(false);
        mpPixmap->setAlignment(Qt::AlignCenter);
        mpPixmap->setWordWrap(false);

        vboxLayout->addWidget(mpPixmap);

        mpLblVersion = new QLabel(CQSplashWidget);
        mpLblVersion->setObjectName(QString::fromUtf8("mpLblVersion"));
        mpLblVersion->setAlignment(Qt::AlignCenter);
        mpLblVersion->setWordWrap(true);

        vboxLayout->addWidget(mpLblVersion);

        mpSpacer = new QSpacerItem(20, 16, QSizePolicy::Minimum, QSizePolicy::Expanding);

        vboxLayout->addItem(mpSpacer);

        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        mpLblLicense = new QLabel(CQSplashWidget);
        mpLblLicense->setObjectName(QString::fromUtf8("mpLblLicense"));
        QSizePolicy sizePolicy(QSizePolicy::Expanding, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(mpLblLicense->sizePolicy().hasHeightForWidth());
        mpLblLicense->setSizePolicy(sizePolicy);
        mpLblLicense->setAlignment(Qt::AlignVCenter);
        mpLblLicense->setWordWrap(true);

        hboxLayout->addWidget(mpLblLicense);

        mpBtnLicense = new QPushButton(CQSplashWidget);
        mpBtnLicense->setObjectName(QString::fromUtf8("mpBtnLicense"));

        hboxLayout->addWidget(mpBtnLicense);


        vboxLayout->addLayout(hboxLayout);


        retranslateUi(CQSplashWidget);
        QObject::connect(mpBtnLicense, SIGNAL(clicked()), CQSplashWidget, SLOT(slotViewLicense()));

        QMetaObject::connectSlotsByName(CQSplashWidget);
    } // setupUi

    void retranslateUi(CopasiWidget *CQSplashWidget)
    {
        CQSplashWidget->setProperty("caption", QVariant(QApplication::translate("CQSplashWidget", "Form1", 0, QApplication::UnicodeUTF8)));
        mpLblVersion->setText(QApplication::translate("CQSplashWidget", "<h1 style=\"color:#000088;\">Version %1</h1>", 0, QApplication::UnicodeUTF8));
        mpLblLicense->setText(QApplication::translate("CQSplashWidget", "<p style=\"color:#000088;\">The use of this software indicates the acceptance of the attached license.</p>", 0, QApplication::UnicodeUTF8));
        mpBtnLicense->setText(QApplication::translate("CQSplashWidget", "View License", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQSplashWidget: public Ui_CQSplashWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQSPLASHWIDGET_H
