/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQExperimentData.ui'
**
** Created: Sun Sep 11 10:59:25 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQEXPERIMENTDATA_H
#define UI_CQEXPERIMENTDATA_H

#include <Qt3Support/Q3ButtonGroup>
#include <Qt3Support/Q3Frame>
#include <Qt3Support/Q3GroupBox>
#include <Qt3Support/Q3Header>
#include <Qt3Support/Q3ListBox>
#include <Qt3Support/Q3Table>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QComboBox>
#include <QtGui/QDialog>
#include <QtGui/QFrame>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QLineEdit>
#include <QtGui/QPushButton>
#include <QtGui/QRadioButton>
#include <QtGui/QToolButton>
#include <QtGui/QVBoxLayout>
#include <map>
#include <string>

QT_BEGIN_NAMESPACE

class Ui_CQExperimentData
{
public:
    QVBoxLayout *vboxLayout;
    QHBoxLayout *hboxLayout;
    QLabel *mpLblWeight;
    QLineEdit *mpEditWeight;
    QLabel *mpLblThreshold;
    QLineEdit *mpEditThreshold;
    QFrame *mpLineCrossValidation;
    QGridLayout *gridLayout;
    QToolButton *mpBtnFileAdd;
    QLabel *mpLblFile;
    Q3ListBox *mpBoxFile;
    QToolButton *mpBtnExperimentDelete;
    QToolButton *mpBtnFileDelete;
    QToolButton *mpBtnExperimentAdd;
    Q3ListBox *mpBoxExperiment;
    QLabel *mpLblExperiment;
    QGridLayout *gridLayout1;
    QLabel *mpLblHeader;
    QLineEdit *mpEditName;
    QFrame *mpLine;
    QLabel *mpLblName;
    QLineEdit *mpEditSeparator;
    QLineEdit *mpEditLast;
    QComboBox *mpBoxWeightMethod;
    QLabel *mpLblExperimentType;
    QLineEdit *mpEditHeader;
    QLabel *mpLblFirst;
    QCheckBox *mpCheckTab;
    Q3ButtonGroup *mpBtnGroup;
    QHBoxLayout *hboxLayout1;
    QRadioButton *mpBtnSteadystate;
    QRadioButton *mpBtnTimeCourse;
    QHBoxLayout *hboxLayout2;
    QCheckBox *mpCheckFrom;
    QCheckBox *mpCheckTo;
    QCheckBox *mpCheckHeader;
    QLabel *mpLblSeperator;
    QLabel *mpLblCopy;
    QLabel *mpLblWeightMethod;
    QLineEdit *mpEditFirst;
    QLabel *mpLblLast;
    Q3Table *mpTable;
    QHBoxLayout *hboxLayout3;
    QPushButton *mpBtnOK;
    QPushButton *mpBtnRevert;
    QPushButton *mpBtnCancel;

    void setupUi(QDialog *CQExperimentData)
    {
        if (CQExperimentData->objectName().isEmpty())
            CQExperimentData->setObjectName(QString::fromUtf8("CQExperimentData"));
        CQExperimentData->resize(522, 421);
        CQExperimentData->setSizeGripEnabled(true);
        vboxLayout = new QVBoxLayout(CQExperimentData);
        vboxLayout->setSpacing(6);
        vboxLayout->setContentsMargins(11, 11, 11, 11);
        vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        mpLblWeight = new QLabel(CQExperimentData);
        mpLblWeight->setObjectName(QString::fromUtf8("mpLblWeight"));
        mpLblWeight->setWordWrap(false);

        hboxLayout->addWidget(mpLblWeight);

        mpEditWeight = new QLineEdit(CQExperimentData);
        mpEditWeight->setObjectName(QString::fromUtf8("mpEditWeight"));

        hboxLayout->addWidget(mpEditWeight);

        mpLblThreshold = new QLabel(CQExperimentData);
        mpLblThreshold->setObjectName(QString::fromUtf8("mpLblThreshold"));
        mpLblThreshold->setWordWrap(false);

        hboxLayout->addWidget(mpLblThreshold);

        mpEditThreshold = new QLineEdit(CQExperimentData);
        mpEditThreshold->setObjectName(QString::fromUtf8("mpEditThreshold"));

        hboxLayout->addWidget(mpEditThreshold);


        vboxLayout->addLayout(hboxLayout);

        mpLineCrossValidation = new QFrame(CQExperimentData);
        mpLineCrossValidation->setObjectName(QString::fromUtf8("mpLineCrossValidation"));
        mpLineCrossValidation->setFrameShape(QFrame::HLine);
        mpLineCrossValidation->setFrameShadow(QFrame::Sunken);

        vboxLayout->addWidget(mpLineCrossValidation);

        gridLayout = new QGridLayout();
        gridLayout->setSpacing(3);
        gridLayout->setContentsMargins(0, 0, 0, 0);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        mpBtnFileAdd = new QToolButton(CQExperimentData);
        mpBtnFileAdd->setObjectName(QString::fromUtf8("mpBtnFileAdd"));
        mpBtnFileAdd->setMaximumSize(QSize(20, 20));
        QIcon icon;
        icon.addFile(QString::fromUtf8("image0"), QSize(), QIcon::Normal, QIcon::Off);
        mpBtnFileAdd->setIcon(icon);

        gridLayout->addWidget(mpBtnFileAdd, 0, 1, 1, 1);

        mpLblFile = new QLabel(CQExperimentData);
        mpLblFile->setObjectName(QString::fromUtf8("mpLblFile"));
        mpLblFile->setWordWrap(false);

        gridLayout->addWidget(mpLblFile, 0, 0, 1, 1);

        mpBoxFile = new Q3ListBox(CQExperimentData);
        mpBoxFile->setObjectName(QString::fromUtf8("mpBoxFile"));
        QSizePolicy sizePolicy(QSizePolicy::Expanding, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(mpBoxFile->sizePolicy().hasHeightForWidth());
        mpBoxFile->setSizePolicy(sizePolicy);
        mpBoxFile->setMinimumSize(QSize(0, 80));
        mpBoxFile->setMaximumSize(QSize(32767, 80));

        gridLayout->addWidget(mpBoxFile, 1, 0, 1, 3);

        mpBtnExperimentDelete = new QToolButton(CQExperimentData);
        mpBtnExperimentDelete->setObjectName(QString::fromUtf8("mpBtnExperimentDelete"));
        mpBtnExperimentDelete->setMaximumSize(QSize(20, 20));
        QIcon icon1;
        icon1.addFile(QString::fromUtf8("image1"), QSize(), QIcon::Normal, QIcon::Off);
        mpBtnExperimentDelete->setIcon(icon1);

        gridLayout->addWidget(mpBtnExperimentDelete, 0, 5, 1, 1);

        mpBtnFileDelete = new QToolButton(CQExperimentData);
        mpBtnFileDelete->setObjectName(QString::fromUtf8("mpBtnFileDelete"));
        mpBtnFileDelete->setMaximumSize(QSize(20, 20));
        mpBtnFileDelete->setIcon(icon1);

        gridLayout->addWidget(mpBtnFileDelete, 0, 2, 1, 1);

        mpBtnExperimentAdd = new QToolButton(CQExperimentData);
        mpBtnExperimentAdd->setObjectName(QString::fromUtf8("mpBtnExperimentAdd"));
        mpBtnExperimentAdd->setMaximumSize(QSize(20, 20));
        mpBtnExperimentAdd->setIcon(icon);

        gridLayout->addWidget(mpBtnExperimentAdd, 0, 4, 1, 1);

        mpBoxExperiment = new Q3ListBox(CQExperimentData);
        mpBoxExperiment->setObjectName(QString::fromUtf8("mpBoxExperiment"));
        sizePolicy.setHeightForWidth(mpBoxExperiment->sizePolicy().hasHeightForWidth());
        mpBoxExperiment->setSizePolicy(sizePolicy);
        mpBoxExperiment->setMinimumSize(QSize(0, 80));
        mpBoxExperiment->setMaximumSize(QSize(32767, 80));

        gridLayout->addWidget(mpBoxExperiment, 1, 3, 1, 3);

        mpLblExperiment = new QLabel(CQExperimentData);
        mpLblExperiment->setObjectName(QString::fromUtf8("mpLblExperiment"));
        mpLblExperiment->setWordWrap(false);

        gridLayout->addWidget(mpLblExperiment, 0, 3, 1, 1);


        vboxLayout->addLayout(gridLayout);

        gridLayout1 = new QGridLayout();
        gridLayout1->setSpacing(6);
        gridLayout1->setObjectName(QString::fromUtf8("gridLayout1"));
        mpLblHeader = new QLabel(CQExperimentData);
        mpLblHeader->setObjectName(QString::fromUtf8("mpLblHeader"));
        mpLblHeader->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblHeader->setWordWrap(false);

        gridLayout1->addWidget(mpLblHeader, 3, 3, 1, 2);

        mpEditName = new QLineEdit(CQExperimentData);
        mpEditName->setObjectName(QString::fromUtf8("mpEditName"));
        QSizePolicy sizePolicy1(QSizePolicy::MinimumExpanding, QSizePolicy::Preferred);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(mpEditName->sizePolicy().hasHeightForWidth());
        mpEditName->setSizePolicy(sizePolicy1);

        gridLayout1->addWidget(mpEditName, 0, 1, 1, 2);

        mpLine = new QFrame(CQExperimentData);
        mpLine->setObjectName(QString::fromUtf8("mpLine"));
        mpLine->setFrameShape(QFrame::HLine);
        mpLine->setFrameShadow(QFrame::Sunken);

        gridLayout1->addWidget(mpLine, 2, 0, 1, 9);

        mpLblName = new QLabel(CQExperimentData);
        mpLblName->setObjectName(QString::fromUtf8("mpLblName"));
        mpLblName->setAlignment(Qt::AlignVCenter);
        mpLblName->setWordWrap(false);

        gridLayout1->addWidget(mpLblName, 0, 0, 1, 1);

        mpEditSeparator = new QLineEdit(CQExperimentData);
        mpEditSeparator->setObjectName(QString::fromUtf8("mpEditSeparator"));
        mpEditSeparator->setEnabled(false);
        QSizePolicy sizePolicy2(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(mpEditSeparator->sizePolicy().hasHeightForWidth());
        mpEditSeparator->setSizePolicy(sizePolicy2);

        gridLayout1->addWidget(mpEditSeparator, 4, 5, 1, 3);

        mpEditLast = new QLineEdit(CQExperimentData);
        mpEditLast->setObjectName(QString::fromUtf8("mpEditLast"));
        QSizePolicy sizePolicy3(QSizePolicy::Expanding, QSizePolicy::Preferred);
        sizePolicy3.setHorizontalStretch(0);
        sizePolicy3.setVerticalStretch(0);
        sizePolicy3.setHeightForWidth(mpEditLast->sizePolicy().hasHeightForWidth());
        mpEditLast->setSizePolicy(sizePolicy3);
        mpEditLast->setMinimumSize(QSize(50, 0));

        gridLayout1->addWidget(mpEditLast, 0, 7, 1, 2);

        mpBoxWeightMethod = new QComboBox(CQExperimentData);
        mpBoxWeightMethod->setObjectName(QString::fromUtf8("mpBoxWeightMethod"));

        gridLayout1->addWidget(mpBoxWeightMethod, 4, 2, 1, 1);

        mpLblExperimentType = new QLabel(CQExperimentData);
        mpLblExperimentType->setObjectName(QString::fromUtf8("mpLblExperimentType"));
        mpLblExperimentType->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblExperimentType->setWordWrap(false);

        gridLayout1->addWidget(mpLblExperimentType, 3, 0, 1, 2);

        mpEditHeader = new QLineEdit(CQExperimentData);
        mpEditHeader->setObjectName(QString::fromUtf8("mpEditHeader"));
        mpEditHeader->setEnabled(false);

        gridLayout1->addWidget(mpEditHeader, 3, 5, 1, 3);

        mpLblFirst = new QLabel(CQExperimentData);
        mpLblFirst->setObjectName(QString::fromUtf8("mpLblFirst"));
        mpLblFirst->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblFirst->setWordWrap(false);

        gridLayout1->addWidget(mpLblFirst, 0, 3, 1, 1);

        mpCheckTab = new QCheckBox(CQExperimentData);
        mpCheckTab->setObjectName(QString::fromUtf8("mpCheckTab"));
        mpCheckTab->setChecked(true);

        gridLayout1->addWidget(mpCheckTab, 4, 8, 1, 1);

        mpBtnGroup = new Q3ButtonGroup(CQExperimentData);
        mpBtnGroup->setObjectName(QString::fromUtf8("mpBtnGroup"));
        QSizePolicy sizePolicy4(QSizePolicy::Fixed, QSizePolicy::Maximum);
        sizePolicy4.setHorizontalStretch(0);
        sizePolicy4.setVerticalStretch(0);
        sizePolicy4.setHeightForWidth(mpBtnGroup->sizePolicy().hasHeightForWidth());
        mpBtnGroup->setSizePolicy(sizePolicy4);
        mpBtnGroup->setAlignment(Qt::AlignBottom|Qt::AlignCenter|Qt::AlignHCenter|Qt::AlignJustify|Qt::AlignLeading|Qt::AlignLeft|Qt::AlignRight|Qt::AlignTop|Qt::AlignTrailing|Qt::AlignVCenter|Qt::AlignVertical_Mask);
        mpBtnGroup->setFrameShape(Q3GroupBox::NoFrame);
        mpBtnGroup->setColumnLayout(0, Qt::Vertical);
        mpBtnGroup->layout()->setSpacing(6);
        mpBtnGroup->layout()->setContentsMargins(0, 0, 0, 0);
        hboxLayout1 = new QHBoxLayout();
        QBoxLayout *boxlayout = qobject_cast<QBoxLayout *>(mpBtnGroup->layout());
        if (boxlayout)
            boxlayout->addLayout(hboxLayout1);
        hboxLayout1->setAlignment(Qt::AlignTop);
        hboxLayout1->setObjectName(QString::fromUtf8("hboxLayout1"));
        mpBtnSteadystate = new QRadioButton(mpBtnGroup);
        mpBtnSteadystate->setObjectName(QString::fromUtf8("mpBtnSteadystate"));
        mpBtnSteadystate->setChecked(true);

        hboxLayout1->addWidget(mpBtnSteadystate);

        mpBtnTimeCourse = new QRadioButton(mpBtnGroup);
        mpBtnTimeCourse->setObjectName(QString::fromUtf8("mpBtnTimeCourse"));

        hboxLayout1->addWidget(mpBtnTimeCourse);


        gridLayout1->addWidget(mpBtnGroup, 3, 2, 1, 1);

        hboxLayout2 = new QHBoxLayout();
        hboxLayout2->setSpacing(6);
        hboxLayout2->setObjectName(QString::fromUtf8("hboxLayout2"));
        mpCheckFrom = new QCheckBox(CQExperimentData);
        mpCheckFrom->setObjectName(QString::fromUtf8("mpCheckFrom"));

        hboxLayout2->addWidget(mpCheckFrom);

        mpCheckTo = new QCheckBox(CQExperimentData);
        mpCheckTo->setObjectName(QString::fromUtf8("mpCheckTo"));

        hboxLayout2->addWidget(mpCheckTo);


        gridLayout1->addLayout(hboxLayout2, 1, 2, 1, 1);

        mpCheckHeader = new QCheckBox(CQExperimentData);
        mpCheckHeader->setObjectName(QString::fromUtf8("mpCheckHeader"));

        gridLayout1->addWidget(mpCheckHeader, 3, 8, 1, 1);

        mpLblSeperator = new QLabel(CQExperimentData);
        mpLblSeperator->setObjectName(QString::fromUtf8("mpLblSeperator"));
        mpLblSeperator->setMinimumSize(QSize(0, 0));
        mpLblSeperator->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblSeperator->setWordWrap(false);

        gridLayout1->addWidget(mpLblSeperator, 4, 3, 1, 2);

        mpLblCopy = new QLabel(CQExperimentData);
        mpLblCopy->setObjectName(QString::fromUtf8("mpLblCopy"));
        mpLblCopy->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblCopy->setWordWrap(false);

        gridLayout1->addWidget(mpLblCopy, 1, 0, 1, 2);

        mpLblWeightMethod = new QLabel(CQExperimentData);
        mpLblWeightMethod->setObjectName(QString::fromUtf8("mpLblWeightMethod"));
        mpLblWeightMethod->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblWeightMethod->setWordWrap(false);

        gridLayout1->addWidget(mpLblWeightMethod, 4, 0, 1, 2);

        mpEditFirst = new QLineEdit(CQExperimentData);
        mpEditFirst->setObjectName(QString::fromUtf8("mpEditFirst"));
        sizePolicy3.setHeightForWidth(mpEditFirst->sizePolicy().hasHeightForWidth());
        mpEditFirst->setSizePolicy(sizePolicy3);
        mpEditFirst->setMinimumSize(QSize(50, 0));

        gridLayout1->addWidget(mpEditFirst, 0, 4, 1, 2);

        mpLblLast = new QLabel(CQExperimentData);
        mpLblLast->setObjectName(QString::fromUtf8("mpLblLast"));
        mpLblLast->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpLblLast->setWordWrap(false);

        gridLayout1->addWidget(mpLblLast, 0, 6, 1, 1);


        vboxLayout->addLayout(gridLayout1);

        mpTable = new Q3Table(CQExperimentData);
        mpTable->setObjectName(QString::fromUtf8("mpTable"));
        mpTable->setNumRows(3);
        mpTable->setNumCols(7);
        mpTable->setSelectionMode(Q3Table::NoSelection);

        vboxLayout->addWidget(mpTable);

        hboxLayout3 = new QHBoxLayout();
        hboxLayout3->setSpacing(6);
        hboxLayout3->setContentsMargins(0, 0, 0, 0);
        hboxLayout3->setObjectName(QString::fromUtf8("hboxLayout3"));
        mpBtnOK = new QPushButton(CQExperimentData);
        mpBtnOK->setObjectName(QString::fromUtf8("mpBtnOK"));

        hboxLayout3->addWidget(mpBtnOK);

        mpBtnRevert = new QPushButton(CQExperimentData);
        mpBtnRevert->setObjectName(QString::fromUtf8("mpBtnRevert"));

        hboxLayout3->addWidget(mpBtnRevert);

        mpBtnCancel = new QPushButton(CQExperimentData);
        mpBtnCancel->setObjectName(QString::fromUtf8("mpBtnCancel"));

        hboxLayout3->addWidget(mpBtnCancel);


        vboxLayout->addLayout(hboxLayout3);

        QWidget::setTabOrder(mpBoxFile, mpBoxExperiment);
        QWidget::setTabOrder(mpBoxExperiment, mpEditName);
        QWidget::setTabOrder(mpEditName, mpEditFirst);
        QWidget::setTabOrder(mpEditFirst, mpEditLast);
        QWidget::setTabOrder(mpEditLast, mpCheckFrom);
        QWidget::setTabOrder(mpCheckFrom, mpCheckTo);
        QWidget::setTabOrder(mpCheckTo, mpBtnSteadystate);
        QWidget::setTabOrder(mpBtnSteadystate, mpEditHeader);
        QWidget::setTabOrder(mpEditHeader, mpCheckHeader);
        QWidget::setTabOrder(mpCheckHeader, mpEditSeparator);
        QWidget::setTabOrder(mpEditSeparator, mpCheckTab);
        QWidget::setTabOrder(mpCheckTab, mpTable);
        QWidget::setTabOrder(mpTable, mpBtnOK);
        QWidget::setTabOrder(mpBtnOK, mpBtnRevert);
        QWidget::setTabOrder(mpBtnRevert, mpBtnCancel);

        retranslateUi(CQExperimentData);
        QObject::connect(mpCheckHeader, SIGNAL(toggled(bool)), CQExperimentData, SLOT(slotCheckHeader(bool)));
        QObject::connect(mpCheckTab, SIGNAL(toggled(bool)), CQExperimentData, SLOT(slotCheckTab(bool)));
        QObject::connect(mpBtnSteadystate, SIGNAL(toggled(bool)), CQExperimentData, SLOT(slotExprimentType(bool)));
        QObject::connect(mpBtnOK, SIGNAL(clicked()), CQExperimentData, SLOT(slotOK()));
        QObject::connect(mpBtnRevert, SIGNAL(clicked()), CQExperimentData, SLOT(slotRevert()));
        QObject::connect(mpBtnCancel, SIGNAL(clicked()), CQExperimentData, SLOT(slotCancel()));
        QObject::connect(mpBtnFileAdd, SIGNAL(clicked()), CQExperimentData, SLOT(slotFileAdd()));
        QObject::connect(mpBtnFileDelete, SIGNAL(clicked()), CQExperimentData, SLOT(slotFileDelete()));
        QObject::connect(mpBtnExperimentAdd, SIGNAL(clicked()), CQExperimentData, SLOT(slotExperimentAdd()));
        QObject::connect(mpBtnExperimentDelete, SIGNAL(clicked()), CQExperimentData, SLOT(slotExperimentDelete()));
        QObject::connect(mpBoxFile, SIGNAL(currentChanged(Q3ListBoxItem*)), CQExperimentData, SLOT(slotFileChanged(Q3ListBoxItem*)));
        QObject::connect(mpBoxExperiment, SIGNAL(currentChanged(Q3ListBoxItem*)), CQExperimentData, SLOT(slotExperimentChanged(Q3ListBoxItem*)));
        QObject::connect(mpEditHeader, SIGNAL(lostFocus()), CQExperimentData, SLOT(slotUpdateTable()));
        QObject::connect(mpEditSeparator, SIGNAL(textChanged(QString)), CQExperimentData, SLOT(slotSeparator()));
        QObject::connect(mpCheckFrom, SIGNAL(toggled(bool)), CQExperimentData, SLOT(slotCheckFrom(bool)));
        QObject::connect(mpCheckTo, SIGNAL(toggled(bool)), CQExperimentData, SLOT(slotCheckTo(bool)));
        QObject::connect(mpBoxWeightMethod, SIGNAL(activated(int)), CQExperimentData, SLOT(slotWeightMethod(int)));

        QMetaObject::connectSlotsByName(CQExperimentData);
    } // setupUi

    void retranslateUi(QDialog *CQExperimentData)
    {
        CQExperimentData->setWindowTitle(QApplication::translate("CQExperimentData", "Experimental Data", 0, QApplication::UnicodeUTF8));
        mpLblWeight->setText(QApplication::translate("CQExperimentData", "Weight", 0, QApplication::UnicodeUTF8));
        mpLblThreshold->setText(QApplication::translate("CQExperimentData", "Threshold", 0, QApplication::UnicodeUTF8));
        mpBtnFileAdd->setText(QString());
        mpLblFile->setText(QApplication::translate("CQExperimentData", "File", 0, QApplication::UnicodeUTF8));
        mpBoxFile->clear();
        mpBoxFile->insertItem(QString());
        mpBtnExperimentDelete->setText(QString());
        mpBtnFileDelete->setText(QString());
        mpBtnExperimentAdd->setText(QString());
        mpBoxExperiment->clear();
        mpBoxExperiment->insertItem(QString());
        mpLblExperiment->setText(QApplication::translate("CQExperimentData", "Experiment", 0, QApplication::UnicodeUTF8));
        mpLblHeader->setText(QApplication::translate("CQExperimentData", "Header", 0, QApplication::UnicodeUTF8));
        mpLblName->setText(QApplication::translate("CQExperimentData", "Name", 0, QApplication::UnicodeUTF8));
        mpLblExperimentType->setText(QApplication::translate("CQExperimentData", "Experiment Type", 0, QApplication::UnicodeUTF8));
        mpLblFirst->setText(QApplication::translate("CQExperimentData", "First Row", 0, QApplication::UnicodeUTF8));
        mpCheckTab->setText(QApplication::translate("CQExperimentData", "<tab>", 0, QApplication::UnicodeUTF8));
        mpBtnGroup->setTitle(QString());
        mpBtnSteadystate->setText(QApplication::translate("CQExperimentData", "Steady State", 0, QApplication::UnicodeUTF8));
        mpBtnTimeCourse->setText(QApplication::translate("CQExperimentData", "Time Course", 0, QApplication::UnicodeUTF8));
        mpCheckFrom->setText(QApplication::translate("CQExperimentData", "from previous", 0, QApplication::UnicodeUTF8));
        mpCheckTo->setText(QApplication::translate("CQExperimentData", "to next", 0, QApplication::UnicodeUTF8));
        mpCheckHeader->setText(QString());
        mpLblSeperator->setText(QApplication::translate("CQExperimentData", "Separator", 0, QApplication::UnicodeUTF8));
        mpLblCopy->setText(QApplication::translate("CQExperimentData", "Copy settings below", 0, QApplication::UnicodeUTF8));
        mpLblWeightMethod->setText(QApplication::translate("CQExperimentData", "Weight Method", 0, QApplication::UnicodeUTF8));
        mpLblLast->setText(QApplication::translate("CQExperimentData", "Last Row", 0, QApplication::UnicodeUTF8));
        mpTable->horizontalHeader()->setLabel(0, QApplication::translate("CQExperimentData", "Column Name", 0, QApplication::UnicodeUTF8));
        mpTable->horizontalHeader()->setLabel(1, QApplication::translate("CQExperimentData", "Type", 0, QApplication::UnicodeUTF8));
        mpTable->horizontalHeader()->setLabel(2, QApplication::translate("CQExperimentData", "Hidden", 0, QApplication::UnicodeUTF8));
        mpTable->horizontalHeader()->setLabel(3, QApplication::translate("CQExperimentData", "<>", 0, QApplication::UnicodeUTF8));
        mpTable->horizontalHeader()->setLabel(4, QApplication::translate("CQExperimentData", "Model Object", 0, QApplication::UnicodeUTF8));
        mpTable->horizontalHeader()->setLabel(5, QApplication::translate("CQExperimentData", "Hidden", 0, QApplication::UnicodeUTF8));
        mpTable->horizontalHeader()->setLabel(6, QApplication::translate("CQExperimentData", "Weight", 0, QApplication::UnicodeUTF8));
        mpTable->verticalHeader()->setLabel(0, QApplication::translate("CQExperimentData", "1", 0, QApplication::UnicodeUTF8));
        mpTable->verticalHeader()->setLabel(1, QApplication::translate("CQExperimentData", "2", 0, QApplication::UnicodeUTF8));
        mpTable->verticalHeader()->setLabel(2, QApplication::translate("CQExperimentData", "3", 0, QApplication::UnicodeUTF8));
        mpBtnOK->setText(QApplication::translate("CQExperimentData", "&OK", 0, QApplication::UnicodeUTF8));
        mpBtnOK->setShortcut(QApplication::translate("CQExperimentData", "Alt+O", 0, QApplication::UnicodeUTF8));
        mpBtnRevert->setText(QApplication::translate("CQExperimentData", "&Revert", 0, QApplication::UnicodeUTF8));
        mpBtnRevert->setShortcut(QApplication::translate("CQExperimentData", "Alt+R", 0, QApplication::UnicodeUTF8));
        mpBtnCancel->setText(QApplication::translate("CQExperimentData", "&Cancel", 0, QApplication::UnicodeUTF8));
        mpBtnCancel->setShortcut(QApplication::translate("CQExperimentData", "Alt+C", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQExperimentData: public Ui_CQExperimentData {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQEXPERIMENTDATA_H
