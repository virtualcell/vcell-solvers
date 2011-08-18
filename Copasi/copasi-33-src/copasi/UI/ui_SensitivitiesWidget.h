/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'SensitivitiesWidget.ui'
**
** Created: Thu Aug 18 12:47:30 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_SENSITIVITIESWIDGET_H
#define UI_SENSITIVITIESWIDGET_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QComboBox>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QToolButton>
#include <QtGui/QVBoxLayout>
#include "SensWidgetComboBox.h"
#include "TaskWidget.h"

QT_BEGIN_NAMESPACE

class Ui_SensitivitiesWidget
{
public:
    QVBoxLayout *verticalLayout;
    QGridLayout *gridLayout;
    QLabel *TextLabel1;
    QComboBox *SubTaskChooser;
    QFrame *line;
    QLabel *TextLabel2;
    SensWidgetComboBox *FunctionChooser;
    QLineEdit *FunctionLineEdit;
    QToolButton *SingleFunctionChooser;
    QLabel *TextLabel3;
    SensWidgetComboBox *VariableChooser;
    QLineEdit *VariableLineEdit;
    QToolButton *SingleVariableChooser;
    QLabel *TextLabel4;
    SensWidgetComboBox *Variable2Chooser;
    QLineEdit *Variable2LineEdit;
    QToolButton *SingleVariable2Chooser;
    QFrame *line_2;

    void setupUi(TaskWidget *SensitivitiesWidget)
    {
        if (SensitivitiesWidget->objectName().isEmpty())
            SensitivitiesWidget->setObjectName(QString::fromUtf8("SensitivitiesWidget"));
        SensitivitiesWidget->resize(333, 223);
        verticalLayout = new QVBoxLayout(SensitivitiesWidget);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        gridLayout = new QGridLayout();
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        TextLabel1 = new QLabel(SensitivitiesWidget);
        TextLabel1->setObjectName(QString::fromUtf8("TextLabel1"));
        QSizePolicy sizePolicy(QSizePolicy::Fixed, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(TextLabel1->sizePolicy().hasHeightForWidth());
        TextLabel1->setSizePolicy(sizePolicy);
        TextLabel1->setMaximumSize(QSize(120, 16777215));

        gridLayout->addWidget(TextLabel1, 0, 0, 1, 1);

        SubTaskChooser = new QComboBox(SensitivitiesWidget);
        SubTaskChooser->setObjectName(QString::fromUtf8("SubTaskChooser"));

        gridLayout->addWidget(SubTaskChooser, 0, 1, 1, 1);

        line = new QFrame(SensitivitiesWidget);
        line->setObjectName(QString::fromUtf8("line"));
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);

        gridLayout->addWidget(line, 1, 0, 1, 3);

        TextLabel2 = new QLabel(SensitivitiesWidget);
        TextLabel2->setObjectName(QString::fromUtf8("TextLabel2"));
        sizePolicy.setHeightForWidth(TextLabel2->sizePolicy().hasHeightForWidth());
        TextLabel2->setSizePolicy(sizePolicy);
        TextLabel2->setMaximumSize(QSize(120, 16777215));

        gridLayout->addWidget(TextLabel2, 2, 0, 1, 1);

        FunctionChooser = new SensWidgetComboBox(SensitivitiesWidget);
        FunctionChooser->setObjectName(QString::fromUtf8("FunctionChooser"));

        gridLayout->addWidget(FunctionChooser, 2, 1, 1, 1);

        FunctionLineEdit = new QLineEdit(SensitivitiesWidget);
        FunctionLineEdit->setObjectName(QString::fromUtf8("FunctionLineEdit"));
        FunctionLineEdit->setMinimumSize(QSize(200, 0));

        gridLayout->addWidget(FunctionLineEdit, 3, 1, 1, 1);

        SingleFunctionChooser = new QToolButton(SensitivitiesWidget);
        SingleFunctionChooser->setObjectName(QString::fromUtf8("SingleFunctionChooser"));

        gridLayout->addWidget(SingleFunctionChooser, 3, 2, 1, 1);

        TextLabel3 = new QLabel(SensitivitiesWidget);
        TextLabel3->setObjectName(QString::fromUtf8("TextLabel3"));
        sizePolicy.setHeightForWidth(TextLabel3->sizePolicy().hasHeightForWidth());
        TextLabel3->setSizePolicy(sizePolicy);
        TextLabel3->setMaximumSize(QSize(120, 16777215));

        gridLayout->addWidget(TextLabel3, 4, 0, 1, 1);

        VariableChooser = new SensWidgetComboBox(SensitivitiesWidget);
        VariableChooser->setObjectName(QString::fromUtf8("VariableChooser"));

        gridLayout->addWidget(VariableChooser, 4, 1, 1, 1);

        VariableLineEdit = new QLineEdit(SensitivitiesWidget);
        VariableLineEdit->setObjectName(QString::fromUtf8("VariableLineEdit"));
        QSizePolicy sizePolicy1(QSizePolicy::Expanding, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(VariableLineEdit->sizePolicy().hasHeightForWidth());
        VariableLineEdit->setSizePolicy(sizePolicy1);
        VariableLineEdit->setMinimumSize(QSize(200, 0));

        gridLayout->addWidget(VariableLineEdit, 5, 1, 1, 1);

        SingleVariableChooser = new QToolButton(SensitivitiesWidget);
        SingleVariableChooser->setObjectName(QString::fromUtf8("SingleVariableChooser"));

        gridLayout->addWidget(SingleVariableChooser, 5, 2, 1, 1);

        TextLabel4 = new QLabel(SensitivitiesWidget);
        TextLabel4->setObjectName(QString::fromUtf8("TextLabel4"));
        sizePolicy.setHeightForWidth(TextLabel4->sizePolicy().hasHeightForWidth());
        TextLabel4->setSizePolicy(sizePolicy);
        TextLabel4->setMaximumSize(QSize(120, 16777215));

        gridLayout->addWidget(TextLabel4, 6, 0, 1, 1);

        Variable2Chooser = new SensWidgetComboBox(SensitivitiesWidget);
        Variable2Chooser->setObjectName(QString::fromUtf8("Variable2Chooser"));

        gridLayout->addWidget(Variable2Chooser, 6, 1, 1, 1);

        Variable2LineEdit = new QLineEdit(SensitivitiesWidget);
        Variable2LineEdit->setObjectName(QString::fromUtf8("Variable2LineEdit"));
        Variable2LineEdit->setMinimumSize(QSize(200, 0));

        gridLayout->addWidget(Variable2LineEdit, 7, 1, 1, 1);

        SingleVariable2Chooser = new QToolButton(SensitivitiesWidget);
        SingleVariable2Chooser->setObjectName(QString::fromUtf8("SingleVariable2Chooser"));

        gridLayout->addWidget(SingleVariable2Chooser, 7, 2, 1, 1);

        line_2 = new QFrame(SensitivitiesWidget);
        line_2->setObjectName(QString::fromUtf8("line_2"));
        line_2->setFrameShape(QFrame::HLine);
        line_2->setFrameShadow(QFrame::Sunken);

        gridLayout->addWidget(line_2, 8, 0, 1, 3);


        verticalLayout->addLayout(gridLayout);


        retranslateUi(SensitivitiesWidget);
        QObject::connect(SubTaskChooser, SIGNAL(activated(int)), SensitivitiesWidget, SLOT(slotChooseSubTask(int)));
        QObject::connect(FunctionChooser, SIGNAL(activated(int)), SensitivitiesWidget, SLOT(slotChooseFunction(int)));
        QObject::connect(VariableChooser, SIGNAL(activated(int)), SensitivitiesWidget, SLOT(slotChooseVariable(int)));
        QObject::connect(Variable2Chooser, SIGNAL(activated(int)), SensitivitiesWidget, SLOT(slotChooseSingleVariable2()));
        QObject::connect(SingleFunctionChooser, SIGNAL(clicked()), SensitivitiesWidget, SLOT(slotChooseSingleFunction()));
        QObject::connect(SingleVariableChooser, SIGNAL(clicked()), SensitivitiesWidget, SLOT(slotChooseSingleVariable()));
        QObject::connect(SingleVariable2Chooser, SIGNAL(clicked()), SensitivitiesWidget, SLOT(slotChooseSingleVariable2()));

        QMetaObject::connectSlotsByName(SensitivitiesWidget);
    } // setupUi

    void retranslateUi(TaskWidget *SensitivitiesWidget)
    {
        TextLabel1->setText(QApplication::translate("SensitivitiesWidget", "Subtask", 0, QApplication::UnicodeUTF8));
        TextLabel2->setText(QApplication::translate("SensitivitiesWidget", "Function", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_TOOLTIP
        FunctionChooser->setToolTip(QApplication::translate("SensitivitiesWidget", "This specifies the set of target values for which<br>the sensitivities are to be calculated.", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
        FunctionLineEdit->setText(QApplication::translate("SensitivitiesWidget", "[Please Choose Object.] --->", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_TOOLTIP
        SingleFunctionChooser->setToolTip(QApplication::translate("SensitivitiesWidget", "If the target value of the sensitivities calculation is a single object <br>the object can be selected by clicking this button.", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
        TextLabel3->setText(QApplication::translate("SensitivitiesWidget", "Variable", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_TOOLTIP
        VariableChooser->setToolTip(QApplication::translate("SensitivitiesWidget", "This specifies a set of parameters. The sensitivities are calculated with respect to these parameters.", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
        VariableLineEdit->setText(QApplication::translate("SensitivitiesWidget", "[Please Choose Object.] --->", 0, QApplication::UnicodeUTF8));
        TextLabel4->setText(QApplication::translate("SensitivitiesWidget", "Second Variable", 0, QApplication::UnicodeUTF8));
#ifndef QT_NO_TOOLTIP
        Variable2Chooser->setToolTip(QApplication::translate("SensitivitiesWidget", "This specifies a second set of parameters. If this is set, second order sensitivities will be calculated.", 0, QApplication::UnicodeUTF8));
#endif // QT_NO_TOOLTIP
        Variable2LineEdit->setText(QApplication::translate("SensitivitiesWidget", "[Please Choose Object.] --->", 0, QApplication::UnicodeUTF8));
        Q_UNUSED(SensitivitiesWidget);
    } // retranslateUi

};

namespace Ui {
    class SensitivitiesWidget: public Ui_SensitivitiesWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_SENSITIVITIESWIDGET_H
