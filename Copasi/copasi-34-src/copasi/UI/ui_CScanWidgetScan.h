/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CScanWidgetScan.ui'
**
** Created: Sun Sep 11 10:59:20 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CSCANWIDGETSCAN_H
#define UI_CSCANWIDGETSCAN_H

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
#include <QtGui/QToolButton>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_CScanWidgetScan
{
public:
    QVBoxLayout *verticalLayout_2;
    QFrame *frame;
    QVBoxLayout *verticalLayout;
    QLabel *labelTitle;
    QHBoxLayout *hboxLayout;
    QLabel *labelObject;
    QLineEdit *lineEditObject;
    QToolButton *buttonObject;
    QGridLayout *gridLayout;
    QLabel *labelNumber;
    QLabel *labelMin;
    QLabel *labelMax;
    QLineEdit *lineEditNumber;
    QLineEdit *lineEditMin;
    QLineEdit *lineEditMax;
    QCheckBox *checkBoxLog;

    void setupUi(QWidget *CScanWidgetScan)
    {
        if (CScanWidgetScan->objectName().isEmpty())
            CScanWidgetScan->setObjectName(QString::fromUtf8("CScanWidgetScan"));
        CScanWidgetScan->resize(371, 196);
        verticalLayout_2 = new QVBoxLayout(CScanWidgetScan);
        verticalLayout_2->setSpacing(6);
        verticalLayout_2->setContentsMargins(11, 11, 11, 11);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        frame = new QFrame(CScanWidgetScan);
        frame->setObjectName(QString::fromUtf8("frame"));
        QPalette palette;
        QBrush brush(QColor(207, 206, 249, 255));
        brush.setStyle(Qt::SolidPattern);
        palette.setBrush(QPalette::Active, QPalette::Base, brush);
        palette.setBrush(QPalette::Inactive, QPalette::Base, brush);
        QBrush brush1(QColor(212, 208, 200, 255));
        brush1.setStyle(Qt::SolidPattern);
        palette.setBrush(QPalette::Disabled, QPalette::Base, brush1);
        frame->setPalette(palette);
        frame->setAutoFillBackground(true);
        frame->setFrameShape(QFrame::StyledPanel);
        frame->setFrameShadow(QFrame::Raised);
        verticalLayout = new QVBoxLayout(frame);
        verticalLayout->setSpacing(6);
        verticalLayout->setContentsMargins(11, 11, 11, 11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        labelTitle = new QLabel(frame);
        labelTitle->setObjectName(QString::fromUtf8("labelTitle"));
        labelTitle->setAlignment(Qt::AlignTop);
        labelTitle->setWordWrap(true);

        verticalLayout->addWidget(labelTitle);

        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        labelObject = new QLabel(frame);
        labelObject->setObjectName(QString::fromUtf8("labelObject"));
        labelObject->setWordWrap(false);

        hboxLayout->addWidget(labelObject);

        lineEditObject = new QLineEdit(frame);
        lineEditObject->setObjectName(QString::fromUtf8("lineEditObject"));
        QPalette palette1;
        QBrush brush2(QColor(255, 255, 255, 255));
        brush2.setStyle(Qt::SolidPattern);
        palette1.setBrush(QPalette::Active, QPalette::Base, brush2);
        palette1.setBrush(QPalette::Inactive, QPalette::Base, brush2);
        palette1.setBrush(QPalette::Disabled, QPalette::Base, brush1);
        lineEditObject->setPalette(palette1);
        lineEditObject->setAutoFillBackground(true);

        hboxLayout->addWidget(lineEditObject);

        buttonObject = new QToolButton(frame);
        buttonObject->setObjectName(QString::fromUtf8("buttonObject"));
        QIcon icon;
        icon.addFile(QString::fromUtf8("image0"), QSize(), QIcon::Normal, QIcon::Off);
        buttonObject->setIcon(icon);

        hboxLayout->addWidget(buttonObject);


        verticalLayout->addLayout(hboxLayout);

        gridLayout = new QGridLayout();
        gridLayout->setSpacing(6);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        labelNumber = new QLabel(frame);
        labelNumber->setObjectName(QString::fromUtf8("labelNumber"));
        labelNumber->setWordWrap(false);

        gridLayout->addWidget(labelNumber, 0, 0, 1, 1);

        labelMin = new QLabel(frame);
        labelMin->setObjectName(QString::fromUtf8("labelMin"));
        labelMin->setWordWrap(false);

        gridLayout->addWidget(labelMin, 0, 1, 1, 1);

        labelMax = new QLabel(frame);
        labelMax->setObjectName(QString::fromUtf8("labelMax"));
        labelMax->setWordWrap(false);

        gridLayout->addWidget(labelMax, 0, 2, 1, 1);

        lineEditNumber = new QLineEdit(frame);
        lineEditNumber->setObjectName(QString::fromUtf8("lineEditNumber"));
        QPalette palette2;
        palette2.setBrush(QPalette::Active, QPalette::Base, brush2);
        palette2.setBrush(QPalette::Inactive, QPalette::Base, brush2);
        palette2.setBrush(QPalette::Disabled, QPalette::Base, brush1);
        lineEditNumber->setPalette(palette2);
        lineEditNumber->setAutoFillBackground(true);

        gridLayout->addWidget(lineEditNumber, 1, 0, 1, 1);

        lineEditMin = new QLineEdit(frame);
        lineEditMin->setObjectName(QString::fromUtf8("lineEditMin"));
        QPalette palette3;
        palette3.setBrush(QPalette::Active, QPalette::Base, brush2);
        palette3.setBrush(QPalette::Inactive, QPalette::Base, brush2);
        palette3.setBrush(QPalette::Disabled, QPalette::Base, brush1);
        lineEditMin->setPalette(palette3);
        lineEditMin->setAutoFillBackground(true);

        gridLayout->addWidget(lineEditMin, 1, 1, 1, 1);

        lineEditMax = new QLineEdit(frame);
        lineEditMax->setObjectName(QString::fromUtf8("lineEditMax"));
        QPalette palette4;
        palette4.setBrush(QPalette::Active, QPalette::Base, brush2);
        palette4.setBrush(QPalette::Inactive, QPalette::Base, brush2);
        palette4.setBrush(QPalette::Disabled, QPalette::Base, brush1);
        lineEditMax->setPalette(palette4);
        lineEditMax->setAutoFillBackground(true);

        gridLayout->addWidget(lineEditMax, 1, 2, 1, 1);


        verticalLayout->addLayout(gridLayout);

        checkBoxLog = new QCheckBox(frame);
        checkBoxLog->setObjectName(QString::fromUtf8("checkBoxLog"));

        verticalLayout->addWidget(checkBoxLog);


        verticalLayout_2->addWidget(frame);


        retranslateUi(CScanWidgetScan);
        QObject::connect(buttonObject, SIGNAL(clicked()), CScanWidgetScan, SLOT(slotChooseObject()));

        QMetaObject::connectSlotsByName(CScanWidgetScan);
    } // setupUi

    void retranslateUi(QWidget *CScanWidgetScan)
    {
        CScanWidgetScan->setWindowTitle(QApplication::translate("CScanWidgetScan", "Form1", 0, QApplication::UnicodeUTF8));
        labelTitle->setText(QApplication::translate("CScanWidgetScan", "<h2>Scan</h2>", 0, QApplication::UnicodeUTF8));
        labelObject->setText(QApplication::translate("CScanWidgetScan", "Parameter", 0, QApplication::UnicodeUTF8));
        buttonObject->setText(QString());
        labelNumber->setText(QApplication::translate("CScanWidgetScan", "Intervals", 0, QApplication::UnicodeUTF8));
        labelMin->setText(QApplication::translate("CScanWidgetScan", "min", 0, QApplication::UnicodeUTF8));
        labelMax->setText(QApplication::translate("CScanWidgetScan", "max", 0, QApplication::UnicodeUTF8));
        checkBoxLog->setText(QApplication::translate("CScanWidgetScan", "logarithmic scan", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CScanWidgetScan: public Ui_CScanWidgetScan {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CSCANWIDGETSCAN_H
