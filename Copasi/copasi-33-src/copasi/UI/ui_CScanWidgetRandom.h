/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CScanWidgetRandom.ui'
**
** Created: Thu Aug 18 12:47:30 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CSCANWIDGETRANDOM_H
#define UI_CSCANWIDGETRANDOM_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QComboBox>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QToolButton>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_CScanWidgetRandom
{
public:
    QVBoxLayout *verticalLayout_2;
    QFrame *frame;
    QVBoxLayout *verticalLayout;
    QLabel *labelTitle;
    QHBoxLayout *horizontalLayout;
    QLabel *labelObject;
    QLineEdit *lineEditObject;
    QToolButton *buttonObject;
    QGridLayout *gridLayout;
    QLabel *labelType;
    QLabel *labelMin;
    QLabel *labelMax;
    QComboBox *comboBoxType;
    QLineEdit *lineEditMin;
    QLineEdit *lineEditMax;
    QCheckBox *checkBoxLog;

    void setupUi(QWidget *CScanWidgetRandom)
    {
        if (CScanWidgetRandom->objectName().isEmpty())
            CScanWidgetRandom->setObjectName(QString::fromUtf8("CScanWidgetRandom"));
        CScanWidgetRandom->resize(453, 201);
        QPalette palette;
        QBrush brush(QColor(220, 220, 220, 255));
        brush.setStyle(Qt::SolidPattern);
        palette.setBrush(QPalette::Active, QPalette::Base, brush);
        palette.setBrush(QPalette::Inactive, QPalette::Base, brush);
        QBrush brush1(QColor(212, 208, 200, 255));
        brush1.setStyle(Qt::SolidPattern);
        palette.setBrush(QPalette::Disabled, QPalette::Base, brush1);
        CScanWidgetRandom->setPalette(palette);
        verticalLayout_2 = new QVBoxLayout(CScanWidgetRandom);
        verticalLayout_2->setSpacing(6);
        verticalLayout_2->setContentsMargins(11, 11, 11, 11);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        frame = new QFrame(CScanWidgetRandom);
        frame->setObjectName(QString::fromUtf8("frame"));
        QPalette palette1;
        QBrush brush2(QColor(221, 203, 249, 255));
        brush2.setStyle(Qt::SolidPattern);
        palette1.setBrush(QPalette::Active, QPalette::Base, brush2);
        palette1.setBrush(QPalette::Inactive, QPalette::Base, brush2);
        palette1.setBrush(QPalette::Disabled, QPalette::Base, brush1);
        frame->setPalette(palette1);
        frame->setAutoFillBackground(true);
        frame->setFrameShape(QFrame::StyledPanel);
        frame->setFrameShadow(QFrame::Raised);
        verticalLayout = new QVBoxLayout(frame);
        verticalLayout->setSpacing(6);
        verticalLayout->setContentsMargins(11, 11, 11, 11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        labelTitle = new QLabel(frame);
        labelTitle->setObjectName(QString::fromUtf8("labelTitle"));
        labelTitle->setWordWrap(false);

        verticalLayout->addWidget(labelTitle);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setSpacing(6);
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        labelObject = new QLabel(frame);
        labelObject->setObjectName(QString::fromUtf8("labelObject"));
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(labelObject->sizePolicy().hasHeightForWidth());
        labelObject->setSizePolicy(sizePolicy);
        labelObject->setWordWrap(false);

        horizontalLayout->addWidget(labelObject);

        lineEditObject = new QLineEdit(frame);
        lineEditObject->setObjectName(QString::fromUtf8("lineEditObject"));
        QSizePolicy sizePolicy1(QSizePolicy::Expanding, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(lineEditObject->sizePolicy().hasHeightForWidth());
        lineEditObject->setSizePolicy(sizePolicy1);
        QPalette palette2;
        QBrush brush3(QColor(255, 255, 255, 255));
        brush3.setStyle(Qt::SolidPattern);
        palette2.setBrush(QPalette::Active, QPalette::Base, brush3);
        palette2.setBrush(QPalette::Inactive, QPalette::Base, brush3);
        palette2.setBrush(QPalette::Disabled, QPalette::Base, brush1);
        lineEditObject->setPalette(palette2);
        lineEditObject->setAutoFillBackground(true);

        horizontalLayout->addWidget(lineEditObject);

        buttonObject = new QToolButton(frame);
        buttonObject->setObjectName(QString::fromUtf8("buttonObject"));

        horizontalLayout->addWidget(buttonObject);


        verticalLayout->addLayout(horizontalLayout);

        gridLayout = new QGridLayout();
        gridLayout->setSpacing(6);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        labelType = new QLabel(frame);
        labelType->setObjectName(QString::fromUtf8("labelType"));
        sizePolicy.setHeightForWidth(labelType->sizePolicy().hasHeightForWidth());
        labelType->setSizePolicy(sizePolicy);
        labelType->setWordWrap(false);

        gridLayout->addWidget(labelType, 0, 0, 1, 1);

        labelMin = new QLabel(frame);
        labelMin->setObjectName(QString::fromUtf8("labelMin"));
        sizePolicy.setHeightForWidth(labelMin->sizePolicy().hasHeightForWidth());
        labelMin->setSizePolicy(sizePolicy);
        labelMin->setWordWrap(false);

        gridLayout->addWidget(labelMin, 0, 1, 1, 1);

        labelMax = new QLabel(frame);
        labelMax->setObjectName(QString::fromUtf8("labelMax"));
        sizePolicy.setHeightForWidth(labelMax->sizePolicy().hasHeightForWidth());
        labelMax->setSizePolicy(sizePolicy);
        labelMax->setWordWrap(false);

        gridLayout->addWidget(labelMax, 0, 2, 1, 1);

        comboBoxType = new QComboBox(frame);
        comboBoxType->setObjectName(QString::fromUtf8("comboBoxType"));
        QSizePolicy sizePolicy2(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(comboBoxType->sizePolicy().hasHeightForWidth());
        comboBoxType->setSizePolicy(sizePolicy2);

        gridLayout->addWidget(comboBoxType, 1, 0, 1, 1);

        lineEditMin = new QLineEdit(frame);
        lineEditMin->setObjectName(QString::fromUtf8("lineEditMin"));
        sizePolicy1.setHeightForWidth(lineEditMin->sizePolicy().hasHeightForWidth());
        lineEditMin->setSizePolicy(sizePolicy1);
        QPalette palette3;
        palette3.setBrush(QPalette::Active, QPalette::Base, brush3);
        palette3.setBrush(QPalette::Inactive, QPalette::Base, brush3);
        palette3.setBrush(QPalette::Disabled, QPalette::Base, brush1);
        lineEditMin->setPalette(palette3);
        lineEditMin->setAutoFillBackground(true);

        gridLayout->addWidget(lineEditMin, 1, 1, 1, 1);

        lineEditMax = new QLineEdit(frame);
        lineEditMax->setObjectName(QString::fromUtf8("lineEditMax"));
        sizePolicy1.setHeightForWidth(lineEditMax->sizePolicy().hasHeightForWidth());
        lineEditMax->setSizePolicy(sizePolicy1);
        QPalette palette4;
        palette4.setBrush(QPalette::Active, QPalette::Base, brush3);
        palette4.setBrush(QPalette::Inactive, QPalette::Base, brush3);
        palette4.setBrush(QPalette::Disabled, QPalette::Base, brush1);
        lineEditMax->setPalette(palette4);
        lineEditMax->setAutoFillBackground(true);

        gridLayout->addWidget(lineEditMax, 1, 2, 1, 1);


        verticalLayout->addLayout(gridLayout);

        checkBoxLog = new QCheckBox(frame);
        checkBoxLog->setObjectName(QString::fromUtf8("checkBoxLog"));

        verticalLayout->addWidget(checkBoxLog);


        verticalLayout_2->addWidget(frame);


        retranslateUi(CScanWidgetRandom);
        QObject::connect(buttonObject, SIGNAL(clicked()), CScanWidgetRandom, SLOT(slotChooseObject()));
        QObject::connect(comboBoxType, SIGNAL(activated(int)), CScanWidgetRandom, SLOT(changeType()));

        QMetaObject::connectSlotsByName(CScanWidgetRandom);
    } // setupUi

    void retranslateUi(QWidget *CScanWidgetRandom)
    {
        CScanWidgetRandom->setWindowTitle(QApplication::translate("CScanWidgetRandom", "Form1", 0, QApplication::UnicodeUTF8));
        labelTitle->setText(QApplication::translate("CScanWidgetRandom", "<h2>Random Distribution</h2>", 0, QApplication::UnicodeUTF8));
        labelObject->setText(QApplication::translate("CScanWidgetRandom", "Parameter", 0, QApplication::UnicodeUTF8));
        labelType->setText(QApplication::translate("CScanWidgetRandom", "Type", 0, QApplication::UnicodeUTF8));
        labelMin->setText(QApplication::translate("CScanWidgetRandom", "min", 0, QApplication::UnicodeUTF8));
        labelMax->setText(QApplication::translate("CScanWidgetRandom", "max", 0, QApplication::UnicodeUTF8));
        comboBoxType->clear();
        comboBoxType->insertItems(0, QStringList()
         << QApplication::translate("CScanWidgetRandom", "Uniform distribution", 0, QApplication::UnicodeUTF8)
         << QApplication::translate("CScanWidgetRandom", "Normal distribution", 0, QApplication::UnicodeUTF8)
         << QApplication::translate("CScanWidgetRandom", "Poisson distribution", 0, QApplication::UnicodeUTF8)
        );
        checkBoxLog->setText(QApplication::translate("CScanWidgetRandom", "logarithmic", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CScanWidgetRandom: public Ui_CScanWidgetRandom {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CSCANWIDGETRANDOM_H
