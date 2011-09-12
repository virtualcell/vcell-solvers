/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQTrajectoryWidget.ui'
**
** Created: Sun Sep 11 10:59:20 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQTRAJECTORYWIDGET_H
#define UI_CQTRAJECTORYWIDGET_H

#include <Qt3Support/Q3MimeSourceFactory>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QSpacerItem>
#include <QtGui/QVBoxLayout>
#include "TaskWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQTrajectoryWidget
{
public:
    QVBoxLayout *verticalLayout;
    QGridLayout *mpGridLayout;
    QLabel *mpLblDuration;
    QLineEdit *mpEditDuration;
    QLabel *mpLblIntervalSize;
    QLineEdit *mpEditIntervalSize;
    QLabel *mpLblIntervals;
    QLineEdit *mpEditIntervals;
    QHBoxLayout *mpLayoutDelayed;
    QCheckBox *mpCheckDelay;
    QLineEdit *mpEditDelay;
    QSpacerItem *mpSpacer;
    QHBoxLayout *horizontalLayout;
    QCheckBox *mpCheckOutputEvent;
    QCheckBox *mpCheckSave;
    QSpacerItem *horizontalSpacer;
    QFrame *mpLine2;
    QLabel *mpLblIntegrationInterval;
    QLineEdit *mpEditIntegrationInterval;
    QLabel *mpLblOutputInterval;
    QLineEdit *mpEditOutputInterval;
    QFrame *mpLine1;

    void setupUi(TaskWidget *CQTrajectoryWidget)
    {
        if (CQTrajectoryWidget->objectName().isEmpty())
            CQTrajectoryWidget->setObjectName(QString::fromUtf8("CQTrajectoryWidget"));
        CQTrajectoryWidget->resize(447, 216);
        CQTrajectoryWidget->setAutoFillBackground(false);
        verticalLayout = new QVBoxLayout(CQTrajectoryWidget);
        verticalLayout->setSpacing(6);
        verticalLayout->setContentsMargins(11, 11, 11, 11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        mpGridLayout = new QGridLayout();
        mpGridLayout->setSpacing(6);
        mpGridLayout->setObjectName(QString::fromUtf8("mpGridLayout"));
        mpLblDuration = new QLabel(CQTrajectoryWidget);
        mpLblDuration->setObjectName(QString::fromUtf8("mpLblDuration"));
        mpLblDuration->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblDuration->setWordWrap(false);

        mpGridLayout->addWidget(mpLblDuration, 0, 0, 1, 1);

        mpEditDuration = new QLineEdit(CQTrajectoryWidget);
        mpEditDuration->setObjectName(QString::fromUtf8("mpEditDuration"));

        mpGridLayout->addWidget(mpEditDuration, 0, 1, 1, 1);

        mpLblIntervalSize = new QLabel(CQTrajectoryWidget);
        mpLblIntervalSize->setObjectName(QString::fromUtf8("mpLblIntervalSize"));
        mpLblIntervalSize->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblIntervalSize->setWordWrap(false);

        mpGridLayout->addWidget(mpLblIntervalSize, 1, 0, 1, 1);

        mpEditIntervalSize = new QLineEdit(CQTrajectoryWidget);
        mpEditIntervalSize->setObjectName(QString::fromUtf8("mpEditIntervalSize"));

        mpGridLayout->addWidget(mpEditIntervalSize, 1, 1, 1, 1);

        mpLblIntervals = new QLabel(CQTrajectoryWidget);
        mpLblIntervals->setObjectName(QString::fromUtf8("mpLblIntervals"));
        mpLblIntervals->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblIntervals->setWordWrap(false);

        mpGridLayout->addWidget(mpLblIntervals, 1, 2, 1, 1);

        mpEditIntervals = new QLineEdit(CQTrajectoryWidget);
        mpEditIntervals->setObjectName(QString::fromUtf8("mpEditIntervals"));

        mpGridLayout->addWidget(mpEditIntervals, 1, 3, 1, 1);

        mpLayoutDelayed = new QHBoxLayout();
        mpLayoutDelayed->setSpacing(6);
        mpLayoutDelayed->setObjectName(QString::fromUtf8("mpLayoutDelayed"));
        mpCheckDelay = new QCheckBox(CQTrajectoryWidget);
        mpCheckDelay->setObjectName(QString::fromUtf8("mpCheckDelay"));

        mpLayoutDelayed->addWidget(mpCheckDelay);

        mpEditDelay = new QLineEdit(CQTrajectoryWidget);
        mpEditDelay->setObjectName(QString::fromUtf8("mpEditDelay"));

        mpLayoutDelayed->addWidget(mpEditDelay);

        mpSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        mpLayoutDelayed->addItem(mpSpacer);


        mpGridLayout->addLayout(mpLayoutDelayed, 2, 1, 1, 3);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        mpCheckOutputEvent = new QCheckBox(CQTrajectoryWidget);
        mpCheckOutputEvent->setObjectName(QString::fromUtf8("mpCheckOutputEvent"));

        horizontalLayout->addWidget(mpCheckOutputEvent);

        mpCheckSave = new QCheckBox(CQTrajectoryWidget);
        mpCheckSave->setObjectName(QString::fromUtf8("mpCheckSave"));

        horizontalLayout->addWidget(mpCheckSave);

        horizontalSpacer = new QSpacerItem(13, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout->addItem(horizontalSpacer);


        mpGridLayout->addLayout(horizontalLayout, 3, 1, 1, 3);

        mpLine2 = new QFrame(CQTrajectoryWidget);
        mpLine2->setObjectName(QString::fromUtf8("mpLine2"));
        mpLine2->setFrameShape(QFrame::HLine);
        mpLine2->setFrameShadow(QFrame::Sunken);

        mpGridLayout->addWidget(mpLine2, 4, 0, 1, 4);

        mpLblIntegrationInterval = new QLabel(CQTrajectoryWidget);
        mpLblIntegrationInterval->setObjectName(QString::fromUtf8("mpLblIntegrationInterval"));
        mpLblIntegrationInterval->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblIntegrationInterval->setWordWrap(false);

        mpGridLayout->addWidget(mpLblIntegrationInterval, 5, 0, 1, 1);

        mpEditIntegrationInterval = new QLineEdit(CQTrajectoryWidget);
        mpEditIntegrationInterval->setObjectName(QString::fromUtf8("mpEditIntegrationInterval"));
        mpEditIntegrationInterval->setReadOnly(true);

        mpGridLayout->addWidget(mpEditIntegrationInterval, 5, 1, 1, 1);

        mpLblOutputInterval = new QLabel(CQTrajectoryWidget);
        mpLblOutputInterval->setObjectName(QString::fromUtf8("mpLblOutputInterval"));
        mpLblOutputInterval->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblOutputInterval->setWordWrap(false);

        mpGridLayout->addWidget(mpLblOutputInterval, 5, 2, 1, 1);

        mpEditOutputInterval = new QLineEdit(CQTrajectoryWidget);
        mpEditOutputInterval->setObjectName(QString::fromUtf8("mpEditOutputInterval"));
        mpEditOutputInterval->setReadOnly(true);

        mpGridLayout->addWidget(mpEditOutputInterval, 5, 3, 1, 1);

        mpLine1 = new QFrame(CQTrajectoryWidget);
        mpLine1->setObjectName(QString::fromUtf8("mpLine1"));
        mpLine1->setFrameShape(QFrame::HLine);
        mpLine1->setFrameShadow(QFrame::Sunken);

        mpGridLayout->addWidget(mpLine1, 6, 0, 1, 4);


        verticalLayout->addLayout(mpGridLayout);

        QWidget::setTabOrder(mpEditDuration, mpEditIntervalSize);
        QWidget::setTabOrder(mpEditIntervalSize, mpEditIntervals);
        QWidget::setTabOrder(mpEditIntervals, mpEditDelay);
        QWidget::setTabOrder(mpEditDelay, mpCheckSave);

        retranslateUi(CQTrajectoryWidget);
        QObject::connect(mpEditDuration, SIGNAL(lostFocus()), CQTrajectoryWidget, SLOT(slotDuration()));
        QObject::connect(mpEditIntervalSize, SIGNAL(lostFocus()), CQTrajectoryWidget, SLOT(slotIntervalSize()));
        QObject::connect(mpEditIntervals, SIGNAL(lostFocus()), CQTrajectoryWidget, SLOT(slotIntervals()));
        QObject::connect(mpEditDelay, SIGNAL(lostFocus()), CQTrajectoryWidget, SLOT(updateIntervals()));
        QObject::connect(mpCheckDelay, SIGNAL(toggled(bool)), CQTrajectoryWidget, SLOT(slotOutputDelay(bool)));

        QMetaObject::connectSlotsByName(CQTrajectoryWidget);
    } // setupUi

    void retranslateUi(TaskWidget *CQTrajectoryWidget)
    {
        CQTrajectoryWidget->setProperty("caption", QVariant(QApplication::translate("CQTrajectoryWidget", "Time Course", 0, QApplication::UnicodeUTF8)));
        mpLblDuration->setText(QApplication::translate("CQTrajectoryWidget", "Duration", 0, QApplication::UnicodeUTF8));
        mpLblIntervalSize->setText(QApplication::translate("CQTrajectoryWidget", "Interval Size", 0, QApplication::UnicodeUTF8));
        mpLblIntervals->setText(QApplication::translate("CQTrajectoryWidget", "Intervals", 0, QApplication::UnicodeUTF8));
        mpCheckDelay->setText(QApplication::translate("CQTrajectoryWidget", "Suppress Output Before", 0, QApplication::UnicodeUTF8));
        mpCheckOutputEvent->setText(QApplication::translate("CQTrajectoryWidget", "Output Events  ", 0, QApplication::UnicodeUTF8));
        mpCheckSave->setText(QApplication::translate("CQTrajectoryWidget", "Save Result in Memory", 0, QApplication::UnicodeUTF8));
        mpLblIntegrationInterval->setText(QApplication::translate("CQTrajectoryWidget", "Integration Interval", 0, QApplication::UnicodeUTF8));
        mpLblOutputInterval->setText(QApplication::translate("CQTrajectoryWidget", "Output Interval", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQTrajectoryWidget: public Ui_CQTrajectoryWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQTRAJECTORYWIDGET_H
