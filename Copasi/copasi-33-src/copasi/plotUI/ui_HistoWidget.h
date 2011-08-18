/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'HistoWidget.ui'
**
** Created: Thu Aug 18 12:47:22 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_HISTOWIDGET_H
#define UI_HISTOWIDGET_H

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
#include <QtGui/QToolButton>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_HistoWidget
{
public:
    QGridLayout *gridLayout;
    QFrame *mpLine;
    QHBoxLayout *hboxLayout;
    QCheckBox *mpCheckBefore;
    QCheckBox *mpCheckDuring;
    QCheckBox *mpCheckAfter;
    QSpacerItem *mpSpacerCaptureData;
    QLabel *mpLblHistogram;
    QLabel *mpLblTitle;
    QLabel *mpLblCaptureData;
    QHBoxLayout *hboxLayout1;
    QLineEdit *mpEditVariable;
    QToolButton *mpBtnVariable;
    QHBoxLayout *hboxLayout2;
    QLineEdit *mpEditIncrement;
    QSpacerItem *mpSpacer1;
    QLineEdit *mpEditTitle;
    QLabel *mpLblIncrement;
    QLabel *mpLblVariable;
    QSpacerItem *mpSpacer2;

    void setupUi(QWidget *HistoWidget)
    {
        if (HistoWidget->objectName().isEmpty())
            HistoWidget->setObjectName(QString::fromUtf8("HistoWidget"));
        HistoWidget->resize(338, 219);
        gridLayout = new QGridLayout(HistoWidget);
        gridLayout->setSpacing(6);
        gridLayout->setContentsMargins(11, 11, 11, 11);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        mpLine = new QFrame(HistoWidget);
        mpLine->setObjectName(QString::fromUtf8("mpLine"));
        mpLine->setFrameShape(QFrame::HLine);
        mpLine->setFrameShadow(QFrame::Sunken);

        gridLayout->addWidget(mpLine, 2, 0, 1, 2);

        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        mpCheckBefore = new QCheckBox(HistoWidget);
        mpCheckBefore->setObjectName(QString::fromUtf8("mpCheckBefore"));

        hboxLayout->addWidget(mpCheckBefore);

        mpCheckDuring = new QCheckBox(HistoWidget);
        mpCheckDuring->setObjectName(QString::fromUtf8("mpCheckDuring"));
        mpCheckDuring->setChecked(true);

        hboxLayout->addWidget(mpCheckDuring);

        mpCheckAfter = new QCheckBox(HistoWidget);
        mpCheckAfter->setObjectName(QString::fromUtf8("mpCheckAfter"));

        hboxLayout->addWidget(mpCheckAfter);

        mpSpacerCaptureData = new QSpacerItem(160, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout->addItem(mpSpacerCaptureData);


        gridLayout->addLayout(hboxLayout, 6, 1, 1, 1);

        mpLblHistogram = new QLabel(HistoWidget);
        mpLblHistogram->setObjectName(QString::fromUtf8("mpLblHistogram"));
        mpLblHistogram->setWordWrap(false);

        gridLayout->addWidget(mpLblHistogram, 0, 0, 1, 2);

        mpLblTitle = new QLabel(HistoWidget);
        mpLblTitle->setObjectName(QString::fromUtf8("mpLblTitle"));
        mpLblTitle->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblTitle->setWordWrap(false);

        gridLayout->addWidget(mpLblTitle, 1, 0, 1, 1);

        mpLblCaptureData = new QLabel(HistoWidget);
        mpLblCaptureData->setObjectName(QString::fromUtf8("mpLblCaptureData"));
        mpLblCaptureData->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblCaptureData->setWordWrap(false);

        gridLayout->addWidget(mpLblCaptureData, 6, 0, 1, 1);

        hboxLayout1 = new QHBoxLayout();
        hboxLayout1->setSpacing(6);
        hboxLayout1->setObjectName(QString::fromUtf8("hboxLayout1"));
        mpEditVariable = new QLineEdit(HistoWidget);
        mpEditVariable->setObjectName(QString::fromUtf8("mpEditVariable"));
        mpEditVariable->setReadOnly(true);

        hboxLayout1->addWidget(mpEditVariable);

        mpBtnVariable = new QToolButton(HistoWidget);
        mpBtnVariable->setObjectName(QString::fromUtf8("mpBtnVariable"));
        mpBtnVariable->setMaximumSize(QSize(20, 20));
        QIcon icon;
        icon.addFile(QString::fromUtf8("image0"), QSize(), QIcon::Normal, QIcon::Off);
        mpBtnVariable->setIcon(icon);

        hboxLayout1->addWidget(mpBtnVariable);


        gridLayout->addLayout(hboxLayout1, 3, 1, 1, 1);

        hboxLayout2 = new QHBoxLayout();
        hboxLayout2->setSpacing(6);
        hboxLayout2->setObjectName(QString::fromUtf8("hboxLayout2"));
        mpEditIncrement = new QLineEdit(HistoWidget);
        mpEditIncrement->setObjectName(QString::fromUtf8("mpEditIncrement"));

        hboxLayout2->addWidget(mpEditIncrement);

        mpSpacer1 = new QSpacerItem(31, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout2->addItem(mpSpacer1);


        gridLayout->addLayout(hboxLayout2, 4, 1, 1, 1);

        mpEditTitle = new QLineEdit(HistoWidget);
        mpEditTitle->setObjectName(QString::fromUtf8("mpEditTitle"));

        gridLayout->addWidget(mpEditTitle, 1, 1, 1, 1);

        mpLblIncrement = new QLabel(HistoWidget);
        mpLblIncrement->setObjectName(QString::fromUtf8("mpLblIncrement"));
        mpLblIncrement->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblIncrement->setWordWrap(false);

        gridLayout->addWidget(mpLblIncrement, 4, 0, 1, 1);

        mpLblVariable = new QLabel(HistoWidget);
        mpLblVariable->setObjectName(QString::fromUtf8("mpLblVariable"));
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(mpLblVariable->sizePolicy().hasHeightForWidth());
        mpLblVariable->setSizePolicy(sizePolicy);
        mpLblVariable->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblVariable->setWordWrap(false);

        gridLayout->addWidget(mpLblVariable, 3, 0, 1, 1);

        mpSpacer2 = new QSpacerItem(20, 16, QSizePolicy::Minimum, QSizePolicy::Expanding);

        gridLayout->addItem(mpSpacer2, 7, 1, 1, 1);


        retranslateUi(HistoWidget);
        QObject::connect(mpBtnVariable, SIGNAL(clicked()), HistoWidget, SLOT(buttonPressedX()));

        QMetaObject::connectSlotsByName(HistoWidget);
    } // setupUi

    void retranslateUi(QWidget *HistoWidget)
    {
        HistoWidget->setWindowTitle(QApplication::translate("HistoWidget", "Form2", 0, QApplication::UnicodeUTF8));
        mpCheckBefore->setText(QApplication::translate("HistoWidget", "before", 0, QApplication::UnicodeUTF8));
        mpCheckDuring->setText(QApplication::translate("HistoWidget", "during", 0, QApplication::UnicodeUTF8));
        mpCheckAfter->setText(QApplication::translate("HistoWidget", "after Task", 0, QApplication::UnicodeUTF8));
        mpLblHistogram->setText(QApplication::translate("HistoWidget", "<h3>Histogram</h3>", 0, QApplication::UnicodeUTF8));
        mpLblTitle->setText(QApplication::translate("HistoWidget", "Title", 0, QApplication::UnicodeUTF8));
        mpLblCaptureData->setText(QApplication::translate("HistoWidget", "Capture Data", 0, QApplication::UnicodeUTF8));
        mpBtnVariable->setText(QApplication::translate("HistoWidget", "select", 0, QApplication::UnicodeUTF8));
        mpLblIncrement->setText(QApplication::translate("HistoWidget", "Increment", 0, QApplication::UnicodeUTF8));
        mpLblVariable->setText(QApplication::translate("HistoWidget", "Variable", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class HistoWidget: public Ui_HistoWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_HISTOWIDGET_H
