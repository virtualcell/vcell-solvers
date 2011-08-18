/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'SliderSettingsDialog.ui'
**
** Created: Thu Aug 18 12:47:30 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_SLIDERSETTINGSDIALOG_H
#define UI_SLIDERSETTINGSDIALOG_H

#include <Qt3Support/Q3MimeSourceFactory>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QDialog>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QVBoxLayout>
#include <vector>

QT_BEGIN_NAMESPACE

class Ui_SliderSettingsDialog
{
public:
    QVBoxLayout *mpVerticalLayout;
    QHBoxLayout *horizontalLayout;
    QLabel *mpObjectLabel;
    QLineEdit *mpObjectNameLineEdit;
    QPushButton *mpObjectBrowseButton;
    QHBoxLayout *horizontalLayout_2;
    QLabel *mpMinValueLabel;
    QLineEdit *mpMinValueEdit;
    QSpacerItem *mpSpacer1;
    QLabel *mpMaxValueLabel;
    QLineEdit *mpMaxValueEdit;
    QHBoxLayout *horizontalLayout_3;
    QCheckBox *mpLogCheckBox;
    QSpacerItem *mpSpacer1_2_2;
    QHBoxLayout *horizontalLayout_4;
    QPushButton *mpExtendedOptionsButton;
    QSpacerItem *spacer6;
    QGridLayout *mpOptionsGridLayout;
    QLabel *mpNumMinorTicksLabel;
    QLineEdit *mpNumMinorTicksEdit;
    QSpacerItem *mpSpacer1_2;
    QLabel *mpMinorTickSizeLabel;
    QLineEdit *mpMinorTickSizeEdit;
    QLabel *mpMinorMajorFactorLabel;
    QLineEdit *mpMinorMajorFactorEdit;
    QSpacerItem *mpSpacer1_3;
    QLabel *mpOriginalValueLabel;
    QLineEdit *mpOriginalValueEdit;
    QLabel *mpObjectValueLabel;
    QLineEdit *mpObjectValueEdit;
    QSpacerItem *mpSpacer1_3_2;
    QSpacerItem *verticalSpacer;
    QHBoxLayout *horizontalLayout_5;
    QSpacerItem *horizontalSpacer;
    QPushButton *mpOkButton;
    QPushButton *mpCancelButton;
    QSpacerItem *horizontalSpacer_3;

    void setupUi(QDialog *SliderSettingsDialog)
    {
        if (SliderSettingsDialog->objectName().isEmpty())
            SliderSettingsDialog->setObjectName(QString::fromUtf8("SliderSettingsDialog"));
        SliderSettingsDialog->resize(635, 323);
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(SliderSettingsDialog->sizePolicy().hasHeightForWidth());
        SliderSettingsDialog->setSizePolicy(sizePolicy);
        mpVerticalLayout = new QVBoxLayout(SliderSettingsDialog);
        mpVerticalLayout->setSpacing(6);
        mpVerticalLayout->setContentsMargins(11, 11, 11, 11);
        mpVerticalLayout->setObjectName(QString::fromUtf8("mpVerticalLayout"));
        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        mpObjectLabel = new QLabel(SliderSettingsDialog);
        mpObjectLabel->setObjectName(QString::fromUtf8("mpObjectLabel"));
        sizePolicy.setHeightForWidth(mpObjectLabel->sizePolicy().hasHeightForWidth());
        mpObjectLabel->setSizePolicy(sizePolicy);
        mpObjectLabel->setWordWrap(false);

        horizontalLayout->addWidget(mpObjectLabel);

        mpObjectNameLineEdit = new QLineEdit(SliderSettingsDialog);
        mpObjectNameLineEdit->setObjectName(QString::fromUtf8("mpObjectNameLineEdit"));
        mpObjectNameLineEdit->setEnabled(false);
        QSizePolicy sizePolicy1(QSizePolicy::MinimumExpanding, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(mpObjectNameLineEdit->sizePolicy().hasHeightForWidth());
        mpObjectNameLineEdit->setSizePolicy(sizePolicy1);
        mpObjectNameLineEdit->setReadOnly(true);

        horizontalLayout->addWidget(mpObjectNameLineEdit);

        mpObjectBrowseButton = new QPushButton(SliderSettingsDialog);
        mpObjectBrowseButton->setObjectName(QString::fromUtf8("mpObjectBrowseButton"));
        QSizePolicy sizePolicy2(QSizePolicy::Minimum, QSizePolicy::Fixed);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(mpObjectBrowseButton->sizePolicy().hasHeightForWidth());
        mpObjectBrowseButton->setSizePolicy(sizePolicy2);
        mpObjectBrowseButton->setMinimumSize(QSize(30, 0));
        QIcon icon;
        icon.addFile(QString::fromUtf8("icons/Copasi.ico"), QSize(), QIcon::Normal, QIcon::Off);
        mpObjectBrowseButton->setIcon(icon);

        horizontalLayout->addWidget(mpObjectBrowseButton);


        mpVerticalLayout->addLayout(horizontalLayout);

        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setSpacing(6);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        mpMinValueLabel = new QLabel(SliderSettingsDialog);
        mpMinValueLabel->setObjectName(QString::fromUtf8("mpMinValueLabel"));
        mpMinValueLabel->setWordWrap(false);

        horizontalLayout_2->addWidget(mpMinValueLabel);

        mpMinValueEdit = new QLineEdit(SliderSettingsDialog);
        mpMinValueEdit->setObjectName(QString::fromUtf8("mpMinValueEdit"));
        sizePolicy1.setHeightForWidth(mpMinValueEdit->sizePolicy().hasHeightForWidth());
        mpMinValueEdit->setSizePolicy(sizePolicy1);
        mpMinValueEdit->setAlignment(Qt::AlignRight);

        horizontalLayout_2->addWidget(mpMinValueEdit);

        mpSpacer1 = new QSpacerItem(18, 20, QSizePolicy::Fixed, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(mpSpacer1);

        mpMaxValueLabel = new QLabel(SliderSettingsDialog);
        mpMaxValueLabel->setObjectName(QString::fromUtf8("mpMaxValueLabel"));
        mpMaxValueLabel->setWordWrap(false);

        horizontalLayout_2->addWidget(mpMaxValueLabel);

        mpMaxValueEdit = new QLineEdit(SliderSettingsDialog);
        mpMaxValueEdit->setObjectName(QString::fromUtf8("mpMaxValueEdit"));
        sizePolicy1.setHeightForWidth(mpMaxValueEdit->sizePolicy().hasHeightForWidth());
        mpMaxValueEdit->setSizePolicy(sizePolicy1);
        mpMaxValueEdit->setAlignment(Qt::AlignRight);

        horizontalLayout_2->addWidget(mpMaxValueEdit);


        mpVerticalLayout->addLayout(horizontalLayout_2);

        horizontalLayout_3 = new QHBoxLayout();
        horizontalLayout_3->setSpacing(6);
        horizontalLayout_3->setObjectName(QString::fromUtf8("horizontalLayout_3"));
        mpLogCheckBox = new QCheckBox(SliderSettingsDialog);
        mpLogCheckBox->setObjectName(QString::fromUtf8("mpLogCheckBox"));

        horizontalLayout_3->addWidget(mpLogCheckBox);

        mpSpacer1_2_2 = new QSpacerItem(200, 20, QSizePolicy::Fixed, QSizePolicy::Minimum);

        horizontalLayout_3->addItem(mpSpacer1_2_2);


        mpVerticalLayout->addLayout(horizontalLayout_3);

        horizontalLayout_4 = new QHBoxLayout();
        horizontalLayout_4->setSpacing(6);
        horizontalLayout_4->setObjectName(QString::fromUtf8("horizontalLayout_4"));
        mpExtendedOptionsButton = new QPushButton(SliderSettingsDialog);
        mpExtendedOptionsButton->setObjectName(QString::fromUtf8("mpExtendedOptionsButton"));
        sizePolicy2.setHeightForWidth(mpExtendedOptionsButton->sizePolicy().hasHeightForWidth());
        mpExtendedOptionsButton->setSizePolicy(sizePolicy2);

        horizontalLayout_4->addWidget(mpExtendedOptionsButton);

        spacer6 = new QSpacerItem(423, 22, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_4->addItem(spacer6);


        mpVerticalLayout->addLayout(horizontalLayout_4);

        mpOptionsGridLayout = new QGridLayout();
        mpOptionsGridLayout->setSpacing(6);
        mpOptionsGridLayout->setObjectName(QString::fromUtf8("mpOptionsGridLayout"));
        mpNumMinorTicksLabel = new QLabel(SliderSettingsDialog);
        mpNumMinorTicksLabel->setObjectName(QString::fromUtf8("mpNumMinorTicksLabel"));
        mpNumMinorTicksLabel->setWordWrap(false);

        mpOptionsGridLayout->addWidget(mpNumMinorTicksLabel, 0, 0, 1, 1);

        mpNumMinorTicksEdit = new QLineEdit(SliderSettingsDialog);
        mpNumMinorTicksEdit->setObjectName(QString::fromUtf8("mpNumMinorTicksEdit"));
        sizePolicy1.setHeightForWidth(mpNumMinorTicksEdit->sizePolicy().hasHeightForWidth());
        mpNumMinorTicksEdit->setSizePolicy(sizePolicy1);
        mpNumMinorTicksEdit->setAlignment(Qt::AlignRight);

        mpOptionsGridLayout->addWidget(mpNumMinorTicksEdit, 0, 1, 1, 1);

        mpSpacer1_2 = new QSpacerItem(40, 20, QSizePolicy::Fixed, QSizePolicy::Minimum);

        mpOptionsGridLayout->addItem(mpSpacer1_2, 0, 2, 1, 1);

        mpMinorTickSizeLabel = new QLabel(SliderSettingsDialog);
        mpMinorTickSizeLabel->setObjectName(QString::fromUtf8("mpMinorTickSizeLabel"));
        mpMinorTickSizeLabel->setWordWrap(false);

        mpOptionsGridLayout->addWidget(mpMinorTickSizeLabel, 0, 3, 1, 1);

        mpMinorTickSizeEdit = new QLineEdit(SliderSettingsDialog);
        mpMinorTickSizeEdit->setObjectName(QString::fromUtf8("mpMinorTickSizeEdit"));
        sizePolicy1.setHeightForWidth(mpMinorTickSizeEdit->sizePolicy().hasHeightForWidth());
        mpMinorTickSizeEdit->setSizePolicy(sizePolicy1);
        mpMinorTickSizeEdit->setAlignment(Qt::AlignRight);

        mpOptionsGridLayout->addWidget(mpMinorTickSizeEdit, 0, 4, 1, 1);

        mpMinorMajorFactorLabel = new QLabel(SliderSettingsDialog);
        mpMinorMajorFactorLabel->setObjectName(QString::fromUtf8("mpMinorMajorFactorLabel"));
        mpMinorMajorFactorLabel->setWordWrap(false);

        mpOptionsGridLayout->addWidget(mpMinorMajorFactorLabel, 1, 0, 1, 1);

        mpMinorMajorFactorEdit = new QLineEdit(SliderSettingsDialog);
        mpMinorMajorFactorEdit->setObjectName(QString::fromUtf8("mpMinorMajorFactorEdit"));
        sizePolicy1.setHeightForWidth(mpMinorMajorFactorEdit->sizePolicy().hasHeightForWidth());
        mpMinorMajorFactorEdit->setSizePolicy(sizePolicy1);
        mpMinorMajorFactorEdit->setAlignment(Qt::AlignRight);

        mpOptionsGridLayout->addWidget(mpMinorMajorFactorEdit, 1, 1, 1, 1);

        mpSpacer1_3 = new QSpacerItem(40, 20, QSizePolicy::Fixed, QSizePolicy::Minimum);

        mpOptionsGridLayout->addItem(mpSpacer1_3, 1, 2, 1, 1);

        mpOriginalValueLabel = new QLabel(SliderSettingsDialog);
        mpOriginalValueLabel->setObjectName(QString::fromUtf8("mpOriginalValueLabel"));
        mpOriginalValueLabel->setWordWrap(false);

        mpOptionsGridLayout->addWidget(mpOriginalValueLabel, 1, 3, 1, 1);

        mpOriginalValueEdit = new QLineEdit(SliderSettingsDialog);
        mpOriginalValueEdit->setObjectName(QString::fromUtf8("mpOriginalValueEdit"));
        sizePolicy1.setHeightForWidth(mpOriginalValueEdit->sizePolicy().hasHeightForWidth());
        mpOriginalValueEdit->setSizePolicy(sizePolicy1);
        mpOriginalValueEdit->setAlignment(Qt::AlignRight);

        mpOptionsGridLayout->addWidget(mpOriginalValueEdit, 1, 4, 1, 1);

        mpObjectValueLabel = new QLabel(SliderSettingsDialog);
        mpObjectValueLabel->setObjectName(QString::fromUtf8("mpObjectValueLabel"));
        mpObjectValueLabel->setWordWrap(false);
        mpObjectValueLabel->setMargin(0);

        mpOptionsGridLayout->addWidget(mpObjectValueLabel, 2, 0, 1, 1);

        mpObjectValueEdit = new QLineEdit(SliderSettingsDialog);
        mpObjectValueEdit->setObjectName(QString::fromUtf8("mpObjectValueEdit"));
        sizePolicy1.setHeightForWidth(mpObjectValueEdit->sizePolicy().hasHeightForWidth());
        mpObjectValueEdit->setSizePolicy(sizePolicy1);
        mpObjectValueEdit->setAlignment(Qt::AlignRight);

        mpOptionsGridLayout->addWidget(mpObjectValueEdit, 2, 1, 1, 1);

        mpSpacer1_3_2 = new QSpacerItem(238, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        mpOptionsGridLayout->addItem(mpSpacer1_3_2, 2, 2, 1, 3);


        mpVerticalLayout->addLayout(mpOptionsGridLayout);

        verticalSpacer = new QSpacerItem(20, 1, QSizePolicy::Minimum, QSizePolicy::Expanding);

        mpVerticalLayout->addItem(verticalSpacer);

        horizontalLayout_5 = new QHBoxLayout();
        horizontalLayout_5->setSpacing(6);
        horizontalLayout_5->setObjectName(QString::fromUtf8("horizontalLayout_5"));
        horizontalSpacer = new QSpacerItem(38, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_5->addItem(horizontalSpacer);

        mpOkButton = new QPushButton(SliderSettingsDialog);
        mpOkButton->setObjectName(QString::fromUtf8("mpOkButton"));
        sizePolicy2.setHeightForWidth(mpOkButton->sizePolicy().hasHeightForWidth());
        mpOkButton->setSizePolicy(sizePolicy2);
        mpOkButton->setDefault(true);

        horizontalLayout_5->addWidget(mpOkButton);

        mpCancelButton = new QPushButton(SliderSettingsDialog);
        mpCancelButton->setObjectName(QString::fromUtf8("mpCancelButton"));
        sizePolicy2.setHeightForWidth(mpCancelButton->sizePolicy().hasHeightForWidth());
        mpCancelButton->setSizePolicy(sizePolicy2);

        horizontalLayout_5->addWidget(mpCancelButton);

        horizontalSpacer_3 = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_5->addItem(horizontalSpacer_3);


        mpVerticalLayout->addLayout(horizontalLayout_5);

        QWidget::setTabOrder(mpObjectNameLineEdit, mpObjectBrowseButton);
        QWidget::setTabOrder(mpObjectBrowseButton, mpMinValueEdit);
        QWidget::setTabOrder(mpMinValueEdit, mpMaxValueEdit);
        QWidget::setTabOrder(mpMaxValueEdit, mpLogCheckBox);
        QWidget::setTabOrder(mpLogCheckBox, mpExtendedOptionsButton);
        QWidget::setTabOrder(mpExtendedOptionsButton, mpOkButton);
        QWidget::setTabOrder(mpOkButton, mpCancelButton);

        retranslateUi(SliderSettingsDialog);
        QObject::connect(mpCancelButton, SIGNAL(clicked()), SliderSettingsDialog, SLOT(cancelButtonPressed()));
        QObject::connect(mpOkButton, SIGNAL(clicked()), SliderSettingsDialog, SLOT(okButtonPressed()));
        QObject::connect(mpMinValueEdit, SIGNAL(lostFocus()), SliderSettingsDialog, SLOT(minValueChanged()));
        QObject::connect(mpMinValueEdit, SIGNAL(textChanged(QString)), SliderSettingsDialog, SLOT(minValueTextChanged()));
        QObject::connect(mpMaxValueEdit, SIGNAL(lostFocus()), SliderSettingsDialog, SLOT(maxValueChanged()));
        QObject::connect(mpMaxValueEdit, SIGNAL(textChanged(QString)), SliderSettingsDialog, SLOT(maxValueTextChanged()));
        QObject::connect(mpLogCheckBox, SIGNAL(toggled(bool)), SliderSettingsDialog, SLOT(logCheckBoxToggled(bool)));
        QObject::connect(mpMinValueEdit, SIGNAL(returnPressed()), SliderSettingsDialog, SLOT(minValueChanged()));
        QObject::connect(mpExtendedOptionsButton, SIGNAL(clicked()), SliderSettingsDialog, SLOT(extendedOptionsClicked()));
        QObject::connect(mpObjectBrowseButton, SIGNAL(clicked()), SliderSettingsDialog, SLOT(browseButtonPressed()));

        QMetaObject::connectSlotsByName(SliderSettingsDialog);
    } // setupUi

    void retranslateUi(QDialog *SliderSettingsDialog)
    {
        SliderSettingsDialog->setWindowTitle(QApplication::translate("SliderSettingsDialog", "Slider Settings", 0, QApplication::UnicodeUTF8));
        mpObjectLabel->setText(QApplication::translate("SliderSettingsDialog", "Object:", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_TOOLTIP
        mpObjectBrowseButton->setToolTip(QApplication::translate("SliderSettingsDialog", "Select an object for the slider", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        mpObjectBrowseButton->setWhatsThis(QApplication::translate("SliderSettingsDialog", "Clicking this button opens a dialog that\n"
"lets the user choose the object that the \n"
"slider will manipulate. The object has to \n"
"represent either an integer value or a float\n"
"value.", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_WHATSTHIS
        mpObjectBrowseButton->setText(QString());
#ifndef QT_NO_TOOLTIP
        mpMinValueLabel->setToolTip(QApplication::translate("SliderSettingsDialog", "Lowest value the slider can have", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        mpMinValueLabel->setWhatsThis(QApplication::translate("SliderSettingsDialog", "Here the user can enter the lowest value the slider will take.\n"
"For logarithmic sliders this has to be a positive value greater\n"
"than 0.0.", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_WHATSTHIS
        mpMinValueLabel->setText(QApplication::translate("SliderSettingsDialog", "Minimum Value:", 0, QApplication::UnicodeUTF8));
        mpMinValueEdit->setText(QApplication::translate("SliderSettingsDialog", "0.0", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_TOOLTIP
        mpMaxValueLabel->setToolTip(QApplication::translate("SliderSettingsDialog", "Maximum value the slider will take.", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        mpMaxValueLabel->setWhatsThis(QApplication::translate("SliderSettingsDialog", "Here the user can enter the maximal value that\n"
"the slider will take.", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_WHATSTHIS
        mpMaxValueLabel->setText(QApplication::translate("SliderSettingsDialog", "Maximum Value:", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_WHATSTHIS
        mpMaxValueEdit->setWhatsThis(QString());
#endif // QT_NO_WHATSTHIS
        mpMaxValueEdit->setText(QApplication::translate("SliderSettingsDialog", "100.0", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_TOOLTIP
        mpLogCheckBox->setToolTip(QApplication::translate("SliderSettingsDialog", "Wether slider has logarithmic scale.", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        mpLogCheckBox->setWhatsThis(QApplication::translate("SliderSettingsDialog", "If checked, the slider will have a \n"
"logarithmic scale. Minimum\n"
"and maximum value must be \n"
"greater than 0.0.", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_WHATSTHIS
        mpLogCheckBox->setText(QApplication::translate("SliderSettingsDialog", "logarithmic", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_TOOLTIP
        mpExtendedOptionsButton->setToolTip(QApplication::translate("SliderSettingsDialog", "Displays or hides an extended set of options.", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        mpExtendedOptionsButton->setWhatsThis(QApplication::translate("SliderSettingsDialog", "Clicking this button either displays or hides\n"
"some more less often needed options.", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_WHATSTHIS
        mpExtendedOptionsButton->setText(QApplication::translate("SliderSettingsDialog", "more options", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_TOOLTIP
        mpNumMinorTicksLabel->setToolTip(QApplication::translate("SliderSettingsDialog", "Number of steps for the slider.", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        mpNumMinorTicksLabel->setWhatsThis(QApplication::translate("SliderSettingsDialog", "This value determines how many steps\n"
"the slider makes from minimum to \n"
"maximum value.", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_WHATSTHIS
        mpNumMinorTicksLabel->setText(QApplication::translate("SliderSettingsDialog", "Number of Minor Ticks:", 0, QApplication::UnicodeUTF8));
        mpNumMinorTicksEdit->setText(QApplication::translate("SliderSettingsDialog", "10.0", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_TOOLTIP
        mpMinorTickSizeLabel->setToolTip(QApplication::translate("SliderSettingsDialog", "Value change corresponding to a minor step", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        mpMinorTickSizeLabel->setWhatsThis(QApplication::translate("SliderSettingsDialog", "This value signifies the value change that\n"
"corresponds to one minor step on the slider.\n"
"The user can either set this directly or indirectly\n"
"when setting the \"Number of Minor Ticks\".", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_WHATSTHIS
        mpMinorTickSizeLabel->setText(QApplication::translate("SliderSettingsDialog", "Minor Ticksize:", 0, QApplication::UnicodeUTF8));
        mpMinorTickSizeEdit->setText(QApplication::translate("SliderSettingsDialog", "1.0", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_TOOLTIP
        mpMinorMajorFactorLabel->setToolTip(QApplication::translate("SliderSettingsDialog", "how many minor ticks make a major tick", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        mpMinorMajorFactorLabel->setWhatsThis(QApplication::translate("SliderSettingsDialog", "This value determines how many minor \n"
"ticks make up a major tick. Minor ticks\n"
"are small steps on the slider whereas major\n"
"ticks are larger steps.\n"
"The user can make a minor step via the cursor\n"
"keys and a major step via the page up/down keys.", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_WHATSTHIS
        mpMinorMajorFactorLabel->setText(QApplication::translate("SliderSettingsDialog", "Ticksize factor:", 0, QApplication::UnicodeUTF8));
        mpMinorMajorFactorEdit->setText(QApplication::translate("SliderSettingsDialog", "10.0", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_TOOLTIP
        mpOriginalValueLabel->setToolTip(QApplication::translate("SliderSettingsDialog", "Value change corresponding to a minor step", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        mpOriginalValueLabel->setWhatsThis(QApplication::translate("SliderSettingsDialog", "This value signifies the value change that\n"
"corresponds to one minor step on the slider.\n"
"The user can either set this directly or indirectly\n"
"when setting the \"Number of Minor Ticks\".", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_WHATSTHIS
        mpOriginalValueLabel->setText(QApplication::translate("SliderSettingsDialog", "Original Value:", 0, QApplication::UnicodeUTF8));
        mpOriginalValueEdit->setText(QApplication::translate("SliderSettingsDialog", "1.0", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_TOOLTIP
        mpObjectValueLabel->setToolTip(QApplication::translate("SliderSettingsDialog", "The value of the underlying object.", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
#ifndef QT_NO_WHATSTHIS
        mpObjectValueLabel->setWhatsThis(QApplication::translate("SliderSettingsDialog", "Here the user can change the actual value of the object\n"
"when editing or creating a slider.", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_WHATSTHIS
        mpObjectValueLabel->setText(QApplication::translate("SliderSettingsDialog", "Object Value:", 0, QApplication::UnicodeUTF8));
        mpObjectValueEdit->setText(QApplication::translate("SliderSettingsDialog", "50.0", 0, QApplication::UnicodeUTF8));
        mpOkButton->setText(QApplication::translate("SliderSettingsDialog", "OK", 0, QApplication::UnicodeUTF8));
        mpCancelButton->setText(QApplication::translate("SliderSettingsDialog", "Cancel", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class SliderSettingsDialog: public Ui_SliderSettingsDialog {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_SLIDERSETTINGSDIALOG_H
