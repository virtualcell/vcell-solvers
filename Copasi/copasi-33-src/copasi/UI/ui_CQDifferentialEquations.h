/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQDifferentialEquations.ui'
**
** Created: Thu Aug 18 12:47:32 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQDIFFERENTIALEQUATIONS_H
#define UI_CQDIFFERENTIALEQUATIONS_H

#include <Qt3Support/Q3ScrollView>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QComboBox>
#include <QtGui/QGridLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QVBoxLayout>
#include <sstream>
#include "copasiWidget.h"
#include "Qt3Support/Q3ScrollView"

QT_BEGIN_NAMESPACE

class Ui_CQDifferentialEquations
{
public:
    QVBoxLayout *vboxLayout;
    Q3ScrollView *mpScrollView;
    QGridLayout *gridLayout;
    QPushButton *mpSaveButton;
    QSpacerItem *spacer1_2;
    QComboBox *comboBoxFunctions;
    QComboBox *comboBoxParameters;
    QLabel *textLabelParameters;
    QSpacerItem *spacer1;
    QLabel *textLabelFunctions;

    void setupUi(CopasiWidget *CQDifferentialEquations)
    {
        if (CQDifferentialEquations->objectName().isEmpty())
            CQDifferentialEquations->setObjectName(QString::fromUtf8("CQDifferentialEquations"));
        CQDifferentialEquations->resize(673, 573);
        vboxLayout = new QVBoxLayout(CQDifferentialEquations);
        vboxLayout->setSpacing(6);
        vboxLayout->setContentsMargins(11, 11, 11, 11);
        vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
        mpScrollView = new Q3ScrollView(CQDifferentialEquations);
        mpScrollView->setObjectName(QString::fromUtf8("mpScrollView"));
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Minimum);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(194);
        sizePolicy.setHeightForWidth(mpScrollView->sizePolicy().hasHeightForWidth());
        mpScrollView->setSizePolicy(sizePolicy);

        vboxLayout->addWidget(mpScrollView);

        gridLayout = new QGridLayout();
        gridLayout->setSpacing(6);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        mpSaveButton = new QPushButton(CQDifferentialEquations);
        mpSaveButton->setObjectName(QString::fromUtf8("mpSaveButton"));

        gridLayout->addWidget(mpSaveButton, 0, 4, 1, 1);

        spacer1_2 = new QSpacerItem(242, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        gridLayout->addItem(spacer1_2, 0, 2, 1, 2);

        comboBoxFunctions = new QComboBox(CQDifferentialEquations);
        comboBoxFunctions->setObjectName(QString::fromUtf8("comboBoxFunctions"));

        gridLayout->addWidget(comboBoxFunctions, 1, 1, 1, 2);

        comboBoxParameters = new QComboBox(CQDifferentialEquations);
        comboBoxParameters->setObjectName(QString::fromUtf8("comboBoxParameters"));

        gridLayout->addWidget(comboBoxParameters, 0, 1, 1, 1);

        textLabelParameters = new QLabel(CQDifferentialEquations);
        textLabelParameters->setObjectName(QString::fromUtf8("textLabelParameters"));
        textLabelParameters->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        textLabelParameters->setWordWrap(false);

        gridLayout->addWidget(textLabelParameters, 0, 0, 1, 1);

        spacer1 = new QSpacerItem(212, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        gridLayout->addItem(spacer1, 1, 3, 1, 1);

        textLabelFunctions = new QLabel(CQDifferentialEquations);
        textLabelFunctions->setObjectName(QString::fromUtf8("textLabelFunctions"));
        textLabelFunctions->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        textLabelFunctions->setWordWrap(false);

        gridLayout->addWidget(textLabelFunctions, 1, 0, 1, 1);


        vboxLayout->addLayout(gridLayout);


        retranslateUi(CQDifferentialEquations);
        QObject::connect(mpSaveButton, SIGNAL(clicked()), CQDifferentialEquations, SLOT(slotSave()));
        QObject::connect(comboBoxParameters, SIGNAL(activated(int)), CQDifferentialEquations, SLOT(slotUpdateWidget()));
        QObject::connect(comboBoxFunctions, SIGNAL(activated(int)), CQDifferentialEquations, SLOT(slotUpdateWidget()));

        QMetaObject::connectSlotsByName(CQDifferentialEquations);
    } // setupUi

    void retranslateUi(CopasiWidget *CQDifferentialEquations)
    {
        CQDifferentialEquations->setProperty("caption", QVariant(QApplication::translate("CQDifferentialEquations", "Form1", 0, QApplication::UnicodeUTF8)));
        mpSaveButton->setText(QApplication::translate("CQDifferentialEquations", "Save Formula to Disk", 0, QApplication::UnicodeUTF8));
        comboBoxFunctions->clear();
        comboBoxFunctions->insertItems(0, QStringList()
         << QApplication::translate("CQDifferentialEquations", "display name", 0, QApplication::UnicodeUTF8)
         << QApplication::translate("CQDifferentialEquations", "expand only kinetic functions", 0, QApplication::UnicodeUTF8)
         << QApplication::translate("CQDifferentialEquations", "expand all functions", 0, QApplication::UnicodeUTF8)
        );
        comboBoxParameters->clear();
        comboBoxParameters->insertItems(0, QStringList()
         << QApplication::translate("CQDifferentialEquations", "display numerical value", 0, QApplication::UnicodeUTF8)
         << QApplication::translate("CQDifferentialEquations", "display name", 0, QApplication::UnicodeUTF8)
        );
        textLabelParameters->setText(QApplication::translate("CQDifferentialEquations", "local parameters", 0, QApplication::UnicodeUTF8));
        textLabelFunctions->setText(QApplication::translate("CQDifferentialEquations", "functions", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQDifferentialEquations: public Ui_CQDifferentialEquations {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQDIFFERENTIALEQUATIONS_H
