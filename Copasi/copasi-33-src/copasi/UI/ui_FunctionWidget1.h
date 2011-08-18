/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'FunctionWidget1.ui'
**
** Created: Thu Aug 18 12:47:30 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_FUNCTIONWIDGET1_H
#define UI_FUNCTIONWIDGET1_H

#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>
#include <QtGui/QRadioButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QTableWidget>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>
#include "CQExpressionMmlStackedWidget.h"
#include "copasiWidget.h"

QT_BEGIN_NAMESPACE

class Ui_FunctionWidget1
{
public:
    QVBoxLayout *verticalLayout;
    QGridLayout *FunctionWidget1Layout;
    QLabel *TextLabel1;
    QLineEdit *LineEdit1;
    QLabel *TextLabel2;
    CQExpressionMmlStackedWidget *mpExpressionEMSW;
    QWidget *page;
    QWidget *page_2;
    QLabel *TextLabel3;
    QHBoxLayout *horizontalLayout;
    QRadioButton *RadioButton1;
    QRadioButton *RadioButton2;
    QRadioButton *RadioButton3;
    QLabel *TextLabel4;
    QLabel *TextLabel5;
    QTableWidget *Table1;
    QLineEdit *mpEditApplicationRestrictions;
    QSpacerItem *verticalSpacer;
    QFrame *line;
    QHBoxLayout *Layout1;
    QPushButton *commitChanges;
    QPushButton *cancelChanges;
    QPushButton *newFcn;
    QPushButton *deleteFcn;

    void setupUi(CopasiWidget *FunctionWidget1)
    {
        if (FunctionWidget1->objectName().isEmpty())
            FunctionWidget1->setObjectName(QString::fromUtf8("FunctionWidget1"));
        FunctionWidget1->resize(438, 239);
        verticalLayout = new QVBoxLayout(FunctionWidget1);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        FunctionWidget1Layout = new QGridLayout();
        FunctionWidget1Layout->setObjectName(QString::fromUtf8("FunctionWidget1Layout"));
        TextLabel1 = new QLabel(FunctionWidget1);
        TextLabel1->setObjectName(QString::fromUtf8("TextLabel1"));
        TextLabel1->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

        FunctionWidget1Layout->addWidget(TextLabel1, 0, 0, 1, 1);

        LineEdit1 = new QLineEdit(FunctionWidget1);
        LineEdit1->setObjectName(QString::fromUtf8("LineEdit1"));

        FunctionWidget1Layout->addWidget(LineEdit1, 0, 2, 1, 1);

        TextLabel2 = new QLabel(FunctionWidget1);
        TextLabel2->setObjectName(QString::fromUtf8("TextLabel2"));
        TextLabel2->setAlignment(Qt::AlignRight|Qt::AlignTop|Qt::AlignTrailing);

        FunctionWidget1Layout->addWidget(TextLabel2, 1, 0, 1, 1);

        mpExpressionEMSW = new CQExpressionMmlStackedWidget(FunctionWidget1);
        mpExpressionEMSW->setObjectName(QString::fromUtf8("mpExpressionEMSW"));
        page = new QWidget();
        page->setObjectName(QString::fromUtf8("page"));
        mpExpressionEMSW->addWidget(page);
        page_2 = new QWidget();
        page_2->setObjectName(QString::fromUtf8("page_2"));
        mpExpressionEMSW->addWidget(page_2);

        FunctionWidget1Layout->addWidget(mpExpressionEMSW, 1, 2, 1, 1);

        TextLabel3 = new QLabel(FunctionWidget1);
        TextLabel3->setObjectName(QString::fromUtf8("TextLabel3"));
        TextLabel3->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);

        FunctionWidget1Layout->addWidget(TextLabel3, 2, 0, 1, 1);

        horizontalLayout = new QHBoxLayout();
        horizontalLayout->setObjectName(QString::fromUtf8("horizontalLayout"));
        RadioButton1 = new QRadioButton(FunctionWidget1);
        RadioButton1->setObjectName(QString::fromUtf8("RadioButton1"));

        horizontalLayout->addWidget(RadioButton1);

        RadioButton2 = new QRadioButton(FunctionWidget1);
        RadioButton2->setObjectName(QString::fromUtf8("RadioButton2"));

        horizontalLayout->addWidget(RadioButton2);

        RadioButton3 = new QRadioButton(FunctionWidget1);
        RadioButton3->setObjectName(QString::fromUtf8("RadioButton3"));

        horizontalLayout->addWidget(RadioButton3);


        FunctionWidget1Layout->addLayout(horizontalLayout, 2, 2, 1, 1);

        TextLabel4 = new QLabel(FunctionWidget1);
        TextLabel4->setObjectName(QString::fromUtf8("TextLabel4"));
        TextLabel4->setAlignment(Qt::AlignRight|Qt::AlignTop|Qt::AlignTrailing);

        FunctionWidget1Layout->addWidget(TextLabel4, 3, 0, 1, 1);

        TextLabel5 = new QLabel(FunctionWidget1);
        TextLabel5->setObjectName(QString::fromUtf8("TextLabel5"));
        TextLabel5->setAlignment(Qt::AlignRight|Qt::AlignTop|Qt::AlignTrailing);

        FunctionWidget1Layout->addWidget(TextLabel5, 4, 0, 1, 1);

        Table1 = new QTableWidget(FunctionWidget1);
        if (Table1->columnCount() < 3)
            Table1->setColumnCount(3);
        QTableWidgetItem *__qtablewidgetitem = new QTableWidgetItem();
        Table1->setHorizontalHeaderItem(0, __qtablewidgetitem);
        QTableWidgetItem *__qtablewidgetitem1 = new QTableWidgetItem();
        Table1->setHorizontalHeaderItem(1, __qtablewidgetitem1);
        QTableWidgetItem *__qtablewidgetitem2 = new QTableWidgetItem();
        Table1->setHorizontalHeaderItem(2, __qtablewidgetitem2);
        Table1->setObjectName(QString::fromUtf8("Table1"));
        QSizePolicy sizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Preferred);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(Table1->sizePolicy().hasHeightForWidth());
        Table1->setSizePolicy(sizePolicy);
        Table1->setEditTriggers(QAbstractItemView::NoEditTriggers);
        Table1->setRowCount(0);
        Table1->setColumnCount(3);
        Table1->horizontalHeader()->setVisible(true);
        Table1->horizontalHeader()->setCascadingSectionResizes(true);
        Table1->horizontalHeader()->setMinimumSectionSize(100);
        Table1->verticalHeader()->setVisible(false);

        FunctionWidget1Layout->addWidget(Table1, 3, 2, 1, 1);

        mpEditApplicationRestrictions = new QLineEdit(FunctionWidget1);
        mpEditApplicationRestrictions->setObjectName(QString::fromUtf8("mpEditApplicationRestrictions"));
        QSizePolicy sizePolicy1(QSizePolicy::Expanding, QSizePolicy::Preferred);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(mpEditApplicationRestrictions->sizePolicy().hasHeightForWidth());
        mpEditApplicationRestrictions->setSizePolicy(sizePolicy1);
        mpEditApplicationRestrictions->setReadOnly(true);

        FunctionWidget1Layout->addWidget(mpEditApplicationRestrictions, 4, 2, 1, 1);


        verticalLayout->addLayout(FunctionWidget1Layout);

        verticalSpacer = new QSpacerItem(20, 13, QSizePolicy::Minimum, QSizePolicy::Expanding);

        verticalLayout->addItem(verticalSpacer);

        line = new QFrame(FunctionWidget1);
        line->setObjectName(QString::fromUtf8("line"));
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);

        verticalLayout->addWidget(line);

        Layout1 = new QHBoxLayout();
        Layout1->setObjectName(QString::fromUtf8("Layout1"));
        commitChanges = new QPushButton(FunctionWidget1);
        commitChanges->setObjectName(QString::fromUtf8("commitChanges"));

        Layout1->addWidget(commitChanges);

        cancelChanges = new QPushButton(FunctionWidget1);
        cancelChanges->setObjectName(QString::fromUtf8("cancelChanges"));

        Layout1->addWidget(cancelChanges);

        newFcn = new QPushButton(FunctionWidget1);
        newFcn->setObjectName(QString::fromUtf8("newFcn"));

        Layout1->addWidget(newFcn);

        deleteFcn = new QPushButton(FunctionWidget1);
        deleteFcn->setObjectName(QString::fromUtf8("deleteFcn"));

        Layout1->addWidget(deleteFcn);


        verticalLayout->addLayout(Layout1);

        QWidget::setTabOrder(LineEdit1, RadioButton1);
        QWidget::setTabOrder(RadioButton1, RadioButton2);
        QWidget::setTabOrder(RadioButton2, RadioButton3);
        QWidget::setTabOrder(RadioButton3, Table1);
        QWidget::setTabOrder(Table1, commitChanges);
        QWidget::setTabOrder(commitChanges, cancelChanges);
        QWidget::setTabOrder(cancelChanges, newFcn);
        QWidget::setTabOrder(newFcn, deleteFcn);

        retranslateUi(FunctionWidget1);
        QObject::connect(commitChanges, SIGNAL(clicked()), FunctionWidget1, SLOT(slotCommitButtonClicked()));
        QObject::connect(cancelChanges, SIGNAL(clicked()), FunctionWidget1, SLOT(slotCancelButtonClicked()));
        QObject::connect(deleteFcn, SIGNAL(clicked()), FunctionWidget1, SLOT(slotDeleteButtonClicked()));
        QObject::connect(newFcn, SIGNAL(clicked()), FunctionWidget1, SLOT(slotNewButtonClicked()));
        QObject::connect(RadioButton1, SIGNAL(toggled(bool)), FunctionWidget1, SLOT(slotReversibilityChanged()));
        QObject::connect(RadioButton2, SIGNAL(toggled(bool)), FunctionWidget1, SLOT(slotReversibilityChanged()));
        QObject::connect(RadioButton3, SIGNAL(toggled(bool)), FunctionWidget1, SLOT(slotReversibilityChanged()));

        QMetaObject::connectSlotsByName(FunctionWidget1);
    } // setupUi

    void retranslateUi(CopasiWidget *FunctionWidget1)
    {
        FunctionWidget1->setWindowTitle(QApplication::translate("FunctionWidget1", "Function Window", 0, QApplication::UnicodeUTF8));
        TextLabel1->setText(QApplication::translate("FunctionWidget1", "Function Name", 0, QApplication::UnicodeUTF8));
        TextLabel2->setText(QApplication::translate("FunctionWidget1", "Formula", 0, QApplication::UnicodeUTF8));
        TextLabel3->setText(QApplication::translate("FunctionWidget1", "Function Type", 0, QApplication::UnicodeUTF8));
        RadioButton1->setText(QApplication::translate("FunctionWidget1", "reversible", 0, QApplication::UnicodeUTF8));
        RadioButton2->setText(QApplication::translate("FunctionWidget1", "irreversible", 0, QApplication::UnicodeUTF8));
        RadioButton3->setText(QApplication::translate("FunctionWidget1", "General", 0, QApplication::UnicodeUTF8));
        TextLabel4->setText(QApplication::translate("FunctionWidget1", "Parameters", 0, QApplication::UnicodeUTF8));
        TextLabel5->setText(QApplication::translate("FunctionWidget1", "Application\n"
"Restrictions", 0, QApplication::UnicodeUTF8));
        QTableWidgetItem *___qtablewidgetitem = Table1->horizontalHeaderItem(0);
        ___qtablewidgetitem->setText(QApplication::translate("FunctionWidget1", "Name", 0, QApplication::UnicodeUTF8));
        QTableWidgetItem *___qtablewidgetitem1 = Table1->horizontalHeaderItem(1);
        ___qtablewidgetitem1->setText(QApplication::translate("FunctionWidget1", "Description", 0, QApplication::UnicodeUTF8));
        QTableWidgetItem *___qtablewidgetitem2 = Table1->horizontalHeaderItem(2);
        ___qtablewidgetitem2->setText(QApplication::translate("FunctionWidget1", "Unit", 0, QApplication::UnicodeUTF8));
        commitChanges->setText(QApplication::translate("FunctionWidget1", "Commit", 0, QApplication::UnicodeUTF8));
        cancelChanges->setText(QApplication::translate("FunctionWidget1", "Revert", 0, QApplication::UnicodeUTF8));
        newFcn->setText(QApplication::translate("FunctionWidget1", "New", 0, QApplication::UnicodeUTF8));
        deleteFcn->setText(QApplication::translate("FunctionWidget1", "Delete", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class FunctionWidget1: public Ui_FunctionWidget1 {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_FUNCTIONWIDGET1_H
