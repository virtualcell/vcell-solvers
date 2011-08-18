/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'ParaPanel.ui'
**
** Created: Thu Aug 18 12:47:01 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_PARAPANEL_H
#define UI_PARAPANEL_H

#include <Qt3Support/Q3MimeSourceFactory>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QGridLayout>
#include <QtGui/QGroupBox>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QRadioButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QSpinBox>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_CQParaPanel
{
public:
    QVBoxLayout *vboxLayout;
    QGridLayout *gridLayout;
    QLineEdit *mpStepEdit;
    QLabel *mpStepLabel;
    QGroupBox *mpScalingButtonGroup;
    QVBoxLayout *vboxLayout1;
    QRadioButton *mpIndividScalButton;
    QRadioButton *mpGlobalScalButton;
    QLabel *mpFrameRateLabel;
    QLabel *mpSchedModeLabel;
    QLabel *mpSchedModeLabel_2;
    QLabel *mpParaLabel;
    QGroupBox *mpScalingButtonGroup_2;
    QVBoxLayout *vboxLayout2;
    QRadioButton *mpSizeModeButton;
    QRadioButton *mpColorModeButton;
    QSpinBox *mpSpinBox1;
    QSpacerItem *mpSpacer1;

    void setupUi(QWidget *CQParaPanel)
    {
        if (CQParaPanel->objectName().isEmpty())
            CQParaPanel->setObjectName(QString::fromUtf8("CQParaPanel"));
        CQParaPanel->setEnabled(true);
        CQParaPanel->resize(358, 454);
        QSizePolicy sizePolicy(QSizePolicy::Fixed, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(CQParaPanel->sizePolicy().hasHeightForWidth());
        CQParaPanel->setSizePolicy(sizePolicy);
        vboxLayout = new QVBoxLayout(CQParaPanel);
        vboxLayout->setSpacing(6);
        vboxLayout->setContentsMargins(11, 11, 11, 11);
        vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
        gridLayout = new QGridLayout();
        gridLayout->setSpacing(6);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        mpStepEdit = new QLineEdit(CQParaPanel);
        mpStepEdit->setObjectName(QString::fromUtf8("mpStepEdit"));
        mpStepEdit->setEnabled(false);

        gridLayout->addWidget(mpStepEdit, 4, 1, 1, 1);

        mpStepLabel = new QLabel(CQParaPanel);
        mpStepLabel->setObjectName(QString::fromUtf8("mpStepLabel"));
        mpStepLabel->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpStepLabel->setWordWrap(false);

        gridLayout->addWidget(mpStepLabel, 4, 0, 1, 1);

        mpScalingButtonGroup = new QGroupBox(CQParaPanel);
        mpScalingButtonGroup->setObjectName(QString::fromUtf8("mpScalingButtonGroup"));
        sizePolicy.setHeightForWidth(mpScalingButtonGroup->sizePolicy().hasHeightForWidth());
        mpScalingButtonGroup->setSizePolicy(sizePolicy);
        vboxLayout1 = new QVBoxLayout(mpScalingButtonGroup);
        vboxLayout1->setSpacing(6);
        vboxLayout1->setContentsMargins(11, 11, 11, 11);
        vboxLayout1->setObjectName(QString::fromUtf8("vboxLayout1"));
        mpIndividScalButton = new QRadioButton(mpScalingButtonGroup);
        mpIndividScalButton->setObjectName(QString::fromUtf8("mpIndividScalButton"));
        QSizePolicy sizePolicy1(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(mpIndividScalButton->sizePolicy().hasHeightForWidth());
        mpIndividScalButton->setSizePolicy(sizePolicy1);
        mpIndividScalButton->setChecked(true);

        vboxLayout1->addWidget(mpIndividScalButton);

        mpGlobalScalButton = new QRadioButton(mpScalingButtonGroup);
        mpGlobalScalButton->setObjectName(QString::fromUtf8("mpGlobalScalButton"));
        sizePolicy1.setHeightForWidth(mpGlobalScalButton->sizePolicy().hasHeightForWidth());
        mpGlobalScalButton->setSizePolicy(sizePolicy1);

        vboxLayout1->addWidget(mpGlobalScalButton);


        gridLayout->addWidget(mpScalingButtonGroup, 2, 1, 1, 1);

        mpFrameRateLabel = new QLabel(CQParaPanel);
        mpFrameRateLabel->setObjectName(QString::fromUtf8("mpFrameRateLabel"));
        mpFrameRateLabel->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpFrameRateLabel->setWordWrap(true);

        gridLayout->addWidget(mpFrameRateLabel, 1, 0, 1, 1);

        mpSchedModeLabel = new QLabel(CQParaPanel);
        mpSchedModeLabel->setObjectName(QString::fromUtf8("mpSchedModeLabel"));
        mpSchedModeLabel->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpSchedModeLabel->setWordWrap(false);

        gridLayout->addWidget(mpSchedModeLabel, 2, 0, 1, 1);

        mpSchedModeLabel_2 = new QLabel(CQParaPanel);
        mpSchedModeLabel_2->setObjectName(QString::fromUtf8("mpSchedModeLabel_2"));
        mpSchedModeLabel_2->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpSchedModeLabel_2->setWordWrap(false);

        gridLayout->addWidget(mpSchedModeLabel_2, 3, 0, 1, 1);

        mpParaLabel = new QLabel(CQParaPanel);
        mpParaLabel->setObjectName(QString::fromUtf8("mpParaLabel"));
        QFont font;
        font.setPointSize(12);
        font.setBold(true);
        font.setWeight(75);
        mpParaLabel->setFont(font);
        mpParaLabel->setWordWrap(false);

        gridLayout->addWidget(mpParaLabel, 0, 0, 1, 2);

        mpScalingButtonGroup_2 = new QGroupBox(CQParaPanel);
        mpScalingButtonGroup_2->setObjectName(QString::fromUtf8("mpScalingButtonGroup_2"));
        vboxLayout2 = new QVBoxLayout(mpScalingButtonGroup_2);
        vboxLayout2->setSpacing(6);
        vboxLayout2->setContentsMargins(11, 11, 11, 11);
        vboxLayout2->setObjectName(QString::fromUtf8("vboxLayout2"));
        mpSizeModeButton = new QRadioButton(mpScalingButtonGroup_2);
        mpSizeModeButton->setObjectName(QString::fromUtf8("mpSizeModeButton"));
        sizePolicy1.setHeightForWidth(mpSizeModeButton->sizePolicy().hasHeightForWidth());
        mpSizeModeButton->setSizePolicy(sizePolicy1);
        mpSizeModeButton->setChecked(true);

        vboxLayout2->addWidget(mpSizeModeButton);

        mpColorModeButton = new QRadioButton(mpScalingButtonGroup_2);
        mpColorModeButton->setObjectName(QString::fromUtf8("mpColorModeButton"));
        sizePolicy1.setHeightForWidth(mpColorModeButton->sizePolicy().hasHeightForWidth());
        mpColorModeButton->setSizePolicy(sizePolicy1);

        vboxLayout2->addWidget(mpColorModeButton);


        gridLayout->addWidget(mpScalingButtonGroup_2, 3, 1, 1, 1);

        mpSpinBox1 = new QSpinBox(CQParaPanel);
        mpSpinBox1->setObjectName(QString::fromUtf8("mpSpinBox1"));
        mpSpinBox1->setMinimum(1);
        mpSpinBox1->setMaximum(50);
        mpSpinBox1->setValue(10);

        gridLayout->addWidget(mpSpinBox1, 1, 1, 1, 1);


        vboxLayout->addLayout(gridLayout);

        mpSpacer1 = new QSpacerItem(20, 50, QSizePolicy::Minimum, QSizePolicy::Expanding);

        vboxLayout->addItem(mpSpacer1);

#ifndef QT_NO_SHORTCUT
        mpFrameRateLabel->setBuddy(mpSpinBox1);
        mpSchedModeLabel->setBuddy(mpIndividScalButton);
        mpSchedModeLabel_2->setBuddy(mpIndividScalButton);
#endif // QT_NO_SHORTCUT
        QWidget::setTabOrder(mpSpinBox1, mpIndividScalButton);
        QWidget::setTabOrder(mpIndividScalButton, mpGlobalScalButton);

        retranslateUi(CQParaPanel);
        QObject::connect(mpSpinBox1, SIGNAL(valueChanged(int)), CQParaPanel, SLOT(changeFrameRate()));
        QObject::connect(mpGlobalScalButton, SIGNAL(clicked()), CQParaPanel, SLOT(setGlobalScaling()));
        QObject::connect(mpIndividScalButton, SIGNAL(clicked()), CQParaPanel, SLOT(setIndividualScaling()));
        QObject::connect(mpStepEdit, SIGNAL(returnPressed()), CQParaPanel, SLOT(stepEdit_returnPressed()));
        QObject::connect(mpSizeModeButton, SIGNAL(clicked()), CQParaPanel, SLOT(setSizeMode()));
        QObject::connect(mpColorModeButton, SIGNAL(clicked()), CQParaPanel, SLOT(setColorMode()));

        QMetaObject::connectSlotsByName(CQParaPanel);
    } // setupUi

    void retranslateUi(QWidget *CQParaPanel)
    {
        CQParaPanel->setWindowTitle(QApplication::translate("CQParaPanel", "Form1", 0, QApplication::UnicodeUTF8));
        mpStepEdit->setText(QApplication::translate("CQParaPanel", "0", 0, QApplication::UnicodeUTF8));
        mpStepLabel->setText(QApplication::translate("CQParaPanel", "Step Number", 0, QApplication::UnicodeUTF8));
        mpScalingButtonGroup->setTitle(QString());
        mpIndividScalButton->setText(QApplication::translate("CQParaPanel", "Individual Scaling", 0, QApplication::UnicodeUTF8));
        mpGlobalScalButton->setText(QApplication::translate("CQParaPanel", "Global Scaling", 0, QApplication::UnicodeUTF8));
        mpFrameRateLabel->setText(QApplication::translate("CQParaPanel", "Frame rate (frames/sec)", 0, QApplication::UnicodeUTF8));
        mpSchedModeLabel->setText(QApplication::translate("CQParaPanel", "Scheduling Mode", 0, QApplication::UnicodeUTF8));
        mpSchedModeLabel_2->setText(QApplication::translate("CQParaPanel", "Mapping Mode", 0, QApplication::UnicodeUTF8));
        mpParaLabel->setText(QApplication::translate("CQParaPanel", "Simulation Parameters", 0, QApplication::UnicodeUTF8));
        mpScalingButtonGroup_2->setTitle(QString());
        mpSizeModeButton->setText(QApplication::translate("CQParaPanel", "Size Mode", 0, QApplication::UnicodeUTF8));
        mpColorModeButton->setText(QApplication::translate("CQParaPanel", "Color Mode", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQParaPanel: public Ui_CQParaPanel {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_PARAPANEL_H
