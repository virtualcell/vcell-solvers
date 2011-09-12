/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQStartValueReset.ui'
**
** Created: Sun Sep 11 10:59:21 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQSTARTVALUERESET_H
#define UI_CQSTARTVALUERESET_H

#include <Qt3Support/Q3MimeSourceFactory>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDialog>
#include <QtGui/QHeaderView>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QVBoxLayout>

QT_BEGIN_NAMESPACE

class Ui_CQStartValueReset
{
public:
    QVBoxLayout *vboxLayout;
    QPushButton *mpBtnModel;
    QPushButton *mpBtnSolution;
    QPushButton *mpBtnRandom;
    QPushButton *mpBtnCancel;
    QSpacerItem *mpSpace;

    void setupUi(QDialog *CQStartValueReset)
    {
        if (CQStartValueReset->objectName().isEmpty())
            CQStartValueReset->setObjectName(QString::fromUtf8("CQStartValueReset"));
        CQStartValueReset->resize(120, 151);
        CQStartValueReset->setSizeGripEnabled(true);
        vboxLayout = new QVBoxLayout(CQStartValueReset);
        vboxLayout->setSpacing(6);
        vboxLayout->setContentsMargins(11, 11, 11, 11);
        vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
        mpBtnModel = new QPushButton(CQStartValueReset);
        mpBtnModel->setObjectName(QString::fromUtf8("mpBtnModel"));
        mpBtnModel->setAutoDefault(true);
        mpBtnModel->setDefault(true);

        vboxLayout->addWidget(mpBtnModel);

        mpBtnSolution = new QPushButton(CQStartValueReset);
        mpBtnSolution->setObjectName(QString::fromUtf8("mpBtnSolution"));
        mpBtnSolution->setAutoDefault(true);

        vboxLayout->addWidget(mpBtnSolution);

        mpBtnRandom = new QPushButton(CQStartValueReset);
        mpBtnRandom->setObjectName(QString::fromUtf8("mpBtnRandom"));
        mpBtnRandom->setAutoDefault(true);

        vboxLayout->addWidget(mpBtnRandom);

        mpBtnCancel = new QPushButton(CQStartValueReset);
        mpBtnCancel->setObjectName(QString::fromUtf8("mpBtnCancel"));
        mpBtnCancel->setAutoDefault(true);

        vboxLayout->addWidget(mpBtnCancel);

        mpSpace = new QSpacerItem(20, 60, QSizePolicy::Minimum, QSizePolicy::Expanding);

        vboxLayout->addItem(mpSpace);


        retranslateUi(CQStartValueReset);
        QObject::connect(mpBtnCancel, SIGNAL(clicked()), CQStartValueReset, SLOT(reject()));
        QObject::connect(mpBtnRandom, SIGNAL(clicked()), CQStartValueReset, SLOT(slotRandom()));
        QObject::connect(mpBtnModel, SIGNAL(clicked()), CQStartValueReset, SLOT(slotModel()));
        QObject::connect(mpBtnSolution, SIGNAL(clicked()), CQStartValueReset, SLOT(slotSolution()));

        QMetaObject::connectSlotsByName(CQStartValueReset);
    } // setupUi

    void retranslateUi(QDialog *CQStartValueReset)
    {
        CQStartValueReset->setWindowTitle(QApplication::translate("CQStartValueReset", "Start Value Reset", 0, QApplication::UnicodeUTF8));
        mpBtnModel->setText(QApplication::translate("CQStartValueReset", "Model Value", 0, QApplication::UnicodeUTF8));
        mpBtnModel->setShortcut(QString());
        mpBtnSolution->setText(QApplication::translate("CQStartValueReset", "Current Solution", 0, QApplication::UnicodeUTF8));
        mpBtnSolution->setShortcut(QString());
        mpBtnRandom->setText(QApplication::translate("CQStartValueReset", "Random", 0, QApplication::UnicodeUTF8));
        mpBtnRandom->setShortcut(QString());
        mpBtnCancel->setText(QApplication::translate("CQStartValueReset", "Cancel", 0, QApplication::UnicodeUTF8));
        mpBtnCancel->setShortcut(QString());
    } // retranslateUi

};

namespace Ui {
    class CQStartValueReset: public Ui_CQStartValueReset {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQSTARTVALUERESET_H
