/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'curve2dwidget.ui'
**
** Created: Thu Aug 18 12:47:22 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CURVE2DWIDGET_H
#define UI_CURVE2DWIDGET_H

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
#include <QtGui/QSpacerItem>
#include <QtGui/QToolButton>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_Curve2DWidget
{
public:
    QGridLayout *gridLayout;
    QSpacerItem *mpSpacerV2;
    QSpacerItem *mpSpacerV3;
    QLabel *mpLblTitle;
    QLabel *mpLblHeader;
    QLabel *mpLblY;
    QLabel *mpLblX;
    QLabel *mpLblCaptureData;
    QLabel *mpLblType;
    QFrame *mpLine;
    QLineEdit *mpEditTitle;
    QHBoxLayout *hboxLayout;
    QComboBox *mpBoxType;
    QSpacerItem *mpSpacerType;
    QHBoxLayout *hboxLayout1;
    QCheckBox *mpCheckBefore;
    QCheckBox *mpCheckDuring;
    QCheckBox *mpCheckAfter;
    QSpacerItem *mpSpacerCaptureData;
    QHBoxLayout *hboxLayout2;
    QLineEdit *mpEditY;
    QToolButton *mpBtnY;
    QHBoxLayout *hboxLayout3;
    QLineEdit *mpEditX;
    QToolButton *mpBtnX;

    void setupUi(QWidget *Curve2DWidget)
    {
        if (Curve2DWidget->objectName().isEmpty())
            Curve2DWidget->setObjectName(QString::fromUtf8("Curve2DWidget"));
        Curve2DWidget->resize(340, 279);
        gridLayout = new QGridLayout(Curve2DWidget);
        gridLayout->setSpacing(6);
        gridLayout->setContentsMargins(11, 11, 11, 11);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        mpSpacerV2 = new QSpacerItem(20, 16, QSizePolicy::Minimum, QSizePolicy::Fixed);

        gridLayout->addItem(mpSpacerV2, 7, 1, 1, 1);

        mpSpacerV3 = new QSpacerItem(20, 80, QSizePolicy::Minimum, QSizePolicy::Expanding);

        gridLayout->addItem(mpSpacerV3, 10, 1, 1, 1);

        mpLblTitle = new QLabel(Curve2DWidget);
        mpLblTitle->setObjectName(QString::fromUtf8("mpLblTitle"));
        mpLblTitle->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblTitle->setWordWrap(false);

        gridLayout->addWidget(mpLblTitle, 1, 0, 1, 1);

        mpLblHeader = new QLabel(Curve2DWidget);
        mpLblHeader->setObjectName(QString::fromUtf8("mpLblHeader"));
        mpLblHeader->setWordWrap(false);

        gridLayout->addWidget(mpLblHeader, 0, 0, 1, 2);

        mpLblY = new QLabel(Curve2DWidget);
        mpLblY->setObjectName(QString::fromUtf8("mpLblY"));
        mpLblY->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblY->setWordWrap(false);

        gridLayout->addWidget(mpLblY, 5, 0, 2, 1);

        mpLblX = new QLabel(Curve2DWidget);
        mpLblX->setObjectName(QString::fromUtf8("mpLblX"));
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(mpLblX->sizePolicy().hasHeightForWidth());
        mpLblX->setSizePolicy(sizePolicy);
        mpLblX->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblX->setWordWrap(false);

        gridLayout->addWidget(mpLblX, 4, 0, 1, 1);

        mpLblCaptureData = new QLabel(Curve2DWidget);
        mpLblCaptureData->setObjectName(QString::fromUtf8("mpLblCaptureData"));
        mpLblCaptureData->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblCaptureData->setWordWrap(false);

        gridLayout->addWidget(mpLblCaptureData, 9, 0, 1, 1);

        mpLblType = new QLabel(Curve2DWidget);
        mpLblType->setObjectName(QString::fromUtf8("mpLblType"));
        mpLblType->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblType->setWordWrap(false);

        gridLayout->addWidget(mpLblType, 8, 0, 1, 1);

        mpLine = new QFrame(Curve2DWidget);
        mpLine->setObjectName(QString::fromUtf8("mpLine"));
        mpLine->setFrameShape(QFrame::HLine);
        mpLine->setFrameShadow(QFrame::Sunken);

        gridLayout->addWidget(mpLine, 2, 0, 2, 2);

        mpEditTitle = new QLineEdit(Curve2DWidget);
        mpEditTitle->setObjectName(QString::fromUtf8("mpEditTitle"));

        gridLayout->addWidget(mpEditTitle, 1, 1, 1, 1);

        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        mpBoxType = new QComboBox(Curve2DWidget);
        mpBoxType->setObjectName(QString::fromUtf8("mpBoxType"));

        hboxLayout->addWidget(mpBoxType);

        mpSpacerType = new QSpacerItem(284, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout->addItem(mpSpacerType);


        gridLayout->addLayout(hboxLayout, 8, 1, 1, 1);

        hboxLayout1 = new QHBoxLayout();
        hboxLayout1->setSpacing(6);
        hboxLayout1->setObjectName(QString::fromUtf8("hboxLayout1"));
        mpCheckBefore = new QCheckBox(Curve2DWidget);
        mpCheckBefore->setObjectName(QString::fromUtf8("mpCheckBefore"));

        hboxLayout1->addWidget(mpCheckBefore);

        mpCheckDuring = new QCheckBox(Curve2DWidget);
        mpCheckDuring->setObjectName(QString::fromUtf8("mpCheckDuring"));
        mpCheckDuring->setChecked(true);

        hboxLayout1->addWidget(mpCheckDuring);

        mpCheckAfter = new QCheckBox(Curve2DWidget);
        mpCheckAfter->setObjectName(QString::fromUtf8("mpCheckAfter"));

        hboxLayout1->addWidget(mpCheckAfter);

        mpSpacerCaptureData = new QSpacerItem(160, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout1->addItem(mpSpacerCaptureData);


        gridLayout->addLayout(hboxLayout1, 9, 1, 1, 1);

        hboxLayout2 = new QHBoxLayout();
        hboxLayout2->setSpacing(6);
        hboxLayout2->setObjectName(QString::fromUtf8("hboxLayout2"));
        mpEditY = new QLineEdit(Curve2DWidget);
        mpEditY->setObjectName(QString::fromUtf8("mpEditY"));
        mpEditY->setReadOnly(true);

        hboxLayout2->addWidget(mpEditY);

        mpBtnY = new QToolButton(Curve2DWidget);
        mpBtnY->setObjectName(QString::fromUtf8("mpBtnY"));
        mpBtnY->setMaximumSize(QSize(20, 20));
        QIcon icon;
        icon.addFile(QString::fromUtf8("image0"), QSize(), QIcon::Normal, QIcon::Off);
        mpBtnY->setIcon(icon);

        hboxLayout2->addWidget(mpBtnY);


        gridLayout->addLayout(hboxLayout2, 6, 1, 1, 1);

        hboxLayout3 = new QHBoxLayout();
        hboxLayout3->setSpacing(6);
        hboxLayout3->setObjectName(QString::fromUtf8("hboxLayout3"));
        mpEditX = new QLineEdit(Curve2DWidget);
        mpEditX->setObjectName(QString::fromUtf8("mpEditX"));
        mpEditX->setReadOnly(true);

        hboxLayout3->addWidget(mpEditX);

        mpBtnX = new QToolButton(Curve2DWidget);
        mpBtnX->setObjectName(QString::fromUtf8("mpBtnX"));
        mpBtnX->setMaximumSize(QSize(20, 20));
        mpBtnX->setIcon(icon);

        hboxLayout3->addWidget(mpBtnX);


        gridLayout->addLayout(hboxLayout3, 3, 1, 3, 1);


        retranslateUi(Curve2DWidget);
        QObject::connect(mpBtnX, SIGNAL(clicked()), Curve2DWidget, SLOT(buttonPressedX()));
        QObject::connect(mpBtnY, SIGNAL(clicked()), Curve2DWidget, SLOT(buttonPressedY()));

        QMetaObject::connectSlotsByName(Curve2DWidget);
    } // setupUi

    void retranslateUi(QWidget *Curve2DWidget)
    {
        Curve2DWidget->setWindowTitle(QApplication::translate("Curve2DWidget", "Form2", 0, QApplication::UnicodeUTF8));
        mpLblTitle->setText(QApplication::translate("Curve2DWidget", "Title", 0, QApplication::UnicodeUTF8));
        mpLblHeader->setText(QApplication::translate("Curve2DWidget", "<h3>2D curve</h3>", 0, QApplication::UnicodeUTF8));
        mpLblY->setText(QApplication::translate("Curve2DWidget", "Y-Axis", 0, QApplication::UnicodeUTF8));
        mpLblX->setText(QApplication::translate("Curve2DWidget", "X-Axis", 0, QApplication::UnicodeUTF8));
        mpLblCaptureData->setText(QApplication::translate("Curve2DWidget", "Capture Data", 0, QApplication::UnicodeUTF8));
        mpLblType->setText(QApplication::translate("Curve2DWidget", "Type", 0, QApplication::UnicodeUTF8));
        mpBoxType->clear();
        mpBoxType->insertItems(0, QStringList()
         << QApplication::translate("Curve2DWidget", "lines", 0, QApplication::UnicodeUTF8)
         << QApplication::translate("Curve2DWidget", "points", 0, QApplication::UnicodeUTF8)
         << QApplication::translate("Curve2DWidget", "symbols", 0, QApplication::UnicodeUTF8)
        );
        mpCheckBefore->setText(QApplication::translate("Curve2DWidget", "before", 0, QApplication::UnicodeUTF8));
        mpCheckDuring->setText(QApplication::translate("Curve2DWidget", "during", 0, QApplication::UnicodeUTF8));
        mpCheckAfter->setText(QApplication::translate("Curve2DWidget", "after Task", 0, QApplication::UnicodeUTF8));
        mpBtnY->setText(QApplication::translate("Curve2DWidget", "select", 0, QApplication::UnicodeUTF8));
        mpBtnX->setText(QApplication::translate("Curve2DWidget", "select", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class Curve2DWidget: public Ui_Curve2DWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CURVE2DWIDGET_H
