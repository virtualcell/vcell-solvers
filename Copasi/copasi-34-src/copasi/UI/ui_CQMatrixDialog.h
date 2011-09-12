/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQMatrixDialog.ui'
**
** Created: Sun Sep 11 10:59:24 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQMATRIXDIALOG_H
#define UI_CQMATRIXDIALOG_H

#include <Qt3Support/Q3MimeSourceFactory>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QComboBox>
#include <QtGui/QDialog>
#include <QtGui/QFrame>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QVBoxLayout>

QT_BEGIN_NAMESPACE

class Ui_CQMatrixDialog
{
public:
    QVBoxLayout *vboxLayout;
    QLabel *mpLabelRow;
    QComboBox *mpCBRow;
    QLabel *mpLabelColumn;
    QComboBox *mpCBColumn;
    QLabel *mpLabelDim3;
    QComboBox *mpCBDim3;
    QSpacerItem *spacer1;
    QFrame *line1;
    QHBoxLayout *hboxLayout;
    QPushButton *mpOKBtn;
    QPushButton *mpCancelBtn;

    void setupUi(QDialog *CQMatrixDialog)
    {
        if (CQMatrixDialog->objectName().isEmpty())
            CQMatrixDialog->setObjectName(QString::fromUtf8("CQMatrixDialog"));
        CQMatrixDialog->resize(425, 211);
        vboxLayout = new QVBoxLayout(CQMatrixDialog);
        vboxLayout->setSpacing(6);
        vboxLayout->setContentsMargins(11, 11, 11, 11);
        vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
        mpLabelRow = new QLabel(CQMatrixDialog);
        mpLabelRow->setObjectName(QString::fromUtf8("mpLabelRow"));
        mpLabelRow->setWordWrap(false);

        vboxLayout->addWidget(mpLabelRow);

        mpCBRow = new QComboBox(CQMatrixDialog);
        mpCBRow->setObjectName(QString::fromUtf8("mpCBRow"));

        vboxLayout->addWidget(mpCBRow);

        mpLabelColumn = new QLabel(CQMatrixDialog);
        mpLabelColumn->setObjectName(QString::fromUtf8("mpLabelColumn"));
        mpLabelColumn->setWordWrap(false);

        vboxLayout->addWidget(mpLabelColumn);

        mpCBColumn = new QComboBox(CQMatrixDialog);
        mpCBColumn->setObjectName(QString::fromUtf8("mpCBColumn"));

        vboxLayout->addWidget(mpCBColumn);

        mpLabelDim3 = new QLabel(CQMatrixDialog);
        mpLabelDim3->setObjectName(QString::fromUtf8("mpLabelDim3"));
        mpLabelDim3->setWordWrap(false);

        vboxLayout->addWidget(mpLabelDim3);

        mpCBDim3 = new QComboBox(CQMatrixDialog);
        mpCBDim3->setObjectName(QString::fromUtf8("mpCBDim3"));

        vboxLayout->addWidget(mpCBDim3);

        spacer1 = new QSpacerItem(20, 40, QSizePolicy::Minimum, QSizePolicy::Expanding);

        vboxLayout->addItem(spacer1);

        line1 = new QFrame(CQMatrixDialog);
        line1->setObjectName(QString::fromUtf8("line1"));
        line1->setFrameShape(QFrame::HLine);
        line1->setFrameShadow(QFrame::Sunken);

        vboxLayout->addWidget(line1);

        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        mpOKBtn = new QPushButton(CQMatrixDialog);
        mpOKBtn->setObjectName(QString::fromUtf8("mpOKBtn"));

        hboxLayout->addWidget(mpOKBtn);

        mpCancelBtn = new QPushButton(CQMatrixDialog);
        mpCancelBtn->setObjectName(QString::fromUtf8("mpCancelBtn"));

        hboxLayout->addWidget(mpCancelBtn);


        vboxLayout->addLayout(hboxLayout);


        retranslateUi(CQMatrixDialog);
        QObject::connect(mpOKBtn, SIGNAL(clicked()), CQMatrixDialog, SLOT(accept()));
        QObject::connect(mpCancelBtn, SIGNAL(clicked()), CQMatrixDialog, SLOT(close()));

        QMetaObject::connectSlotsByName(CQMatrixDialog);
    } // setupUi

    void retranslateUi(QDialog *CQMatrixDialog)
    {
        CQMatrixDialog->setWindowTitle(QApplication::translate("CQMatrixDialog", "Cell Matrix Selection Dialog", 0, QApplication::UnicodeUTF8));
        mpLabelRow->setText(QApplication::translate("CQMatrixDialog", "Row :", 0, QApplication::UnicodeUTF8));
        mpLabelColumn->setText(QApplication::translate("CQMatrixDialog", "Column :", 0, QApplication::UnicodeUTF8));
        mpLabelDim3->setText(QApplication::translate("CQMatrixDialog", "Dimension 3 :", 0, QApplication::UnicodeUTF8));
        mpOKBtn->setText(QApplication::translate("CQMatrixDialog", "OK", 0, QApplication::UnicodeUTF8));
        mpCancelBtn->setText(QApplication::translate("CQMatrixDialog", "Cancel", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQMatrixDialog: public Ui_CQMatrixDialog {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQMATRIXDIALOG_H
