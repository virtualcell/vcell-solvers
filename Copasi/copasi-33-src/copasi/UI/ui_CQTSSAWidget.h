/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQTSSAWidget.ui'
**
** Created: Thu Aug 18 12:47:30 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQTSSAWIDGET_H
#define UI_CQTSSAWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QVBoxLayout>
#include "TaskWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQTSSAWidget
{
public:
    QVBoxLayout *vboxLayout;
    QGridLayout *gridLayout;
    QLineEdit *mpEditIntervals;
    QLineEdit *mpEditIntervalSize;
    QLineEdit *mpEditDuration;
    QLabel *mpLblIntervals;
    QLabel *mpLblDuration;
    QLabel *mpLblIntervalSize;
    QCheckBox *mpCheckSave;
    QFrame *line1;

    void setupUi(TaskWidget *CQTSSAWidget)
    {
        if (CQTSSAWidget->objectName().isEmpty())
            CQTSSAWidget->setObjectName(QString::fromUtf8("CQTSSAWidget"));
        CQTSSAWidget->resize(364, 183);
        vboxLayout = new QVBoxLayout(CQTSSAWidget);
        vboxLayout->setSpacing(6);
        vboxLayout->setContentsMargins(11, 11, 11, 11);
        vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
        gridLayout = new QGridLayout();
        gridLayout->setSpacing(6);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        mpEditIntervals = new QLineEdit(CQTSSAWidget);
        mpEditIntervals->setObjectName(QString::fromUtf8("mpEditIntervals"));

        gridLayout->addWidget(mpEditIntervals, 1, 3, 1, 1);

        mpEditIntervalSize = new QLineEdit(CQTSSAWidget);
        mpEditIntervalSize->setObjectName(QString::fromUtf8("mpEditIntervalSize"));

        gridLayout->addWidget(mpEditIntervalSize, 1, 1, 1, 1);

        mpEditDuration = new QLineEdit(CQTSSAWidget);
        mpEditDuration->setObjectName(QString::fromUtf8("mpEditDuration"));

        gridLayout->addWidget(mpEditDuration, 0, 1, 1, 1);

        mpLblIntervals = new QLabel(CQTSSAWidget);
        mpLblIntervals->setObjectName(QString::fromUtf8("mpLblIntervals"));
        mpLblIntervals->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblIntervals->setWordWrap(false);

        gridLayout->addWidget(mpLblIntervals, 1, 2, 1, 1);

        mpLblDuration = new QLabel(CQTSSAWidget);
        mpLblDuration->setObjectName(QString::fromUtf8("mpLblDuration"));
        mpLblDuration->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblDuration->setWordWrap(false);

        gridLayout->addWidget(mpLblDuration, 0, 0, 1, 1);

        mpLblIntervalSize = new QLabel(CQTSSAWidget);
        mpLblIntervalSize->setObjectName(QString::fromUtf8("mpLblIntervalSize"));
        mpLblIntervalSize->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblIntervalSize->setWordWrap(false);

        gridLayout->addWidget(mpLblIntervalSize, 1, 0, 1, 1);

        mpCheckSave = new QCheckBox(CQTSSAWidget);
        mpCheckSave->setObjectName(QString::fromUtf8("mpCheckSave"));

        gridLayout->addWidget(mpCheckSave, 2, 1, 1, 2);


        vboxLayout->addLayout(gridLayout);

        line1 = new QFrame(CQTSSAWidget);
        line1->setObjectName(QString::fromUtf8("line1"));
        line1->setFrameShape(QFrame::HLine);
        line1->setFrameShadow(QFrame::Sunken);

        vboxLayout->addWidget(line1);

        QWidget::setTabOrder(mpEditDuration, mpEditIntervalSize);
        QWidget::setTabOrder(mpEditIntervalSize, mpEditIntervals);
        QWidget::setTabOrder(mpEditIntervals, mpCheckSave);

        retranslateUi(CQTSSAWidget);
        QObject::connect(mpEditDuration, SIGNAL(lostFocus()), CQTSSAWidget, SLOT(slotDuration()));
        QObject::connect(mpEditIntervalSize, SIGNAL(lostFocus()), CQTSSAWidget, SLOT(slotIntervalSize()));
        QObject::connect(mpEditIntervals, SIGNAL(lostFocus()), CQTSSAWidget, SLOT(slotIntervals()));

        QMetaObject::connectSlotsByName(CQTSSAWidget);
    } // setupUi

    void retranslateUi(TaskWidget *CQTSSAWidget)
    {
        CQTSSAWidget->setProperty("caption", QVariant(QApplication::translate("CQTSSAWidget", "Time Scale Separation Analysis", 0, QApplication::UnicodeUTF8)));
        mpLblIntervals->setText(QApplication::translate("CQTSSAWidget", "Intervals", 0, QApplication::UnicodeUTF8));
        mpLblDuration->setText(QApplication::translate("CQTSSAWidget", "Duration", 0, QApplication::UnicodeUTF8));
        mpLblIntervalSize->setText(QApplication::translate("CQTSSAWidget", "Interval Size", 0, QApplication::UnicodeUTF8));
        mpCheckSave->setText(QApplication::translate("CQTSSAWidget", "Save Result in Memory", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQTSSAWidget: public Ui_CQTSSAWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQTSSAWIDGET_H
