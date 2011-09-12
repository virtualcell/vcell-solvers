/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQReportDefinition.ui'
**
** Created: Sun Sep 11 10:59:23 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQREPORTDEFINITION_H
#define UI_CQREPORTDEFINITION_H

#include <Qt3Support/Q3ListBox>
#include <Qt3Support/Q3MimeSourceFactory>
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
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QTabWidget>
#include <QtGui/QVBoxLayout>
#include <Qt3Support/Q3ListBoxPixmap>
#include "copasi/UI/CQNotes.h"
#include "copasi/UI/copasiWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQReportDefinition
{
public:
    QVBoxLayout *verticalLayout;
    QGridLayout *gridLayout;
    QLabel *mpNameLabel;
    QLineEdit *mpName;
    QLabel *mpTaskLabel;
    QComboBox *mpTaskBox;
    QLabel *mpCommentLabel;
    CQNotes *mpNotes;
    QLabel *mpSeparatorLabel;
    QHBoxLayout *mpSeparatorLayout;
    QLineEdit *mpSeparator;
    QCheckBox *mpTabCheck;
    QLabel *mpPrecisionLabel;
    QLineEdit *mpPrecision;
    QHBoxLayout *mpHorizontalLayout;
    QVBoxLayout *mpVerticalBtnLayout;
    QPushButton *mpBtnAdvanced;
    QPushButton *mpBtnItem;
    QPushButton *mpBtnSeparator;
    QPushButton *mpBtnText;
    QPushButton *mpBtnDelete;
    QPushButton *mpBtnUp;
    QPushButton *mpBtnDown;
    QCheckBox *mpTitleCheck;
    QSpacerItem *mpSpacerBtnLayout;
    QTabWidget *mpReportSectionTab;
    Q3ListBox *mpTableList;
    Q3ListBox *mpHeaderList;
    Q3ListBox *mpBodyList;
    Q3ListBox *mpFooterList;
    QFrame *line;
    QHBoxLayout *mpHorizontalBtnLayout;
    QPushButton *mpBtnCommit;
    QPushButton *mpBtnRevert;
    QPushButton *mpBtnNewReport;
    QPushButton *mpBtnDeleteReport;

    void setupUi(CopasiWidget *CQReportDefinition)
    {
        if (CQReportDefinition->objectName().isEmpty())
            CQReportDefinition->setObjectName(QString::fromUtf8("CQReportDefinition"));
        CQReportDefinition->resize(455, 471);
        verticalLayout = new QVBoxLayout(CQReportDefinition);
        verticalLayout->setSpacing(6);
        verticalLayout->setContentsMargins(11, 11, 11, 11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        gridLayout = new QGridLayout();
        gridLayout->setSpacing(6);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        mpNameLabel = new QLabel(CQReportDefinition);
        mpNameLabel->setObjectName(QString::fromUtf8("mpNameLabel"));
        mpNameLabel->setMinimumSize(QSize(62, 0));
        mpNameLabel->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpNameLabel->setWordWrap(false);

        gridLayout->addWidget(mpNameLabel, 0, 0, 1, 1);

        mpName = new QLineEdit(CQReportDefinition);
        mpName->setObjectName(QString::fromUtf8("mpName"));
        QSizePolicy sizePolicy(QSizePolicy::MinimumExpanding, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(mpName->sizePolicy().hasHeightForWidth());
        mpName->setSizePolicy(sizePolicy);
        mpName->setMinimumSize(QSize(150, 0));

        gridLayout->addWidget(mpName, 0, 1, 1, 1);

        mpTaskLabel = new QLabel(CQReportDefinition);
        mpTaskLabel->setObjectName(QString::fromUtf8("mpTaskLabel"));
        mpTaskLabel->setMinimumSize(QSize(45, 0));
        mpTaskLabel->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpTaskLabel->setWordWrap(false);

        gridLayout->addWidget(mpTaskLabel, 0, 2, 1, 1);

        mpTaskBox = new QComboBox(CQReportDefinition);
        mpTaskBox->setObjectName(QString::fromUtf8("mpTaskBox"));
        sizePolicy.setHeightForWidth(mpTaskBox->sizePolicy().hasHeightForWidth());
        mpTaskBox->setSizePolicy(sizePolicy);
        mpTaskBox->setMinimumSize(QSize(150, 0));

        gridLayout->addWidget(mpTaskBox, 0, 3, 1, 2);

        mpCommentLabel = new QLabel(CQReportDefinition);
        mpCommentLabel->setObjectName(QString::fromUtf8("mpCommentLabel"));
        mpCommentLabel->setMinimumSize(QSize(62, 0));
        mpCommentLabel->setAlignment(Qt::AlignRight|Qt::AlignTop|Qt::AlignTrailing);
        mpCommentLabel->setWordWrap(false);

        gridLayout->addWidget(mpCommentLabel, 1, 0, 1, 1);

        mpNotes = new CQNotes(CQReportDefinition);
        mpNotes->setObjectName(QString::fromUtf8("mpNotes"));
        QSizePolicy sizePolicy1(QSizePolicy::Preferred, QSizePolicy::Maximum);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(60);
        sizePolicy1.setHeightForWidth(mpNotes->sizePolicy().hasHeightForWidth());
        mpNotes->setSizePolicy(sizePolicy1);
        mpNotes->setMinimumSize(QSize(0, 60));
        mpNotes->setMaximumSize(QSize(16777215, 60));

        gridLayout->addWidget(mpNotes, 1, 1, 1, 4);

        mpSeparatorLabel = new QLabel(CQReportDefinition);
        mpSeparatorLabel->setObjectName(QString::fromUtf8("mpSeparatorLabel"));
        mpSeparatorLabel->setMinimumSize(QSize(62, 0));
        mpSeparatorLabel->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpSeparatorLabel->setWordWrap(false);

        gridLayout->addWidget(mpSeparatorLabel, 2, 0, 1, 1);

        mpSeparatorLayout = new QHBoxLayout();
        mpSeparatorLayout->setSpacing(6);
        mpSeparatorLayout->setObjectName(QString::fromUtf8("mpSeparatorLayout"));
        mpSeparator = new QLineEdit(CQReportDefinition);
        mpSeparator->setObjectName(QString::fromUtf8("mpSeparator"));
        QSizePolicy sizePolicy2(QSizePolicy::Expanding, QSizePolicy::Fixed);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(mpSeparator->sizePolicy().hasHeightForWidth());
        mpSeparator->setSizePolicy(sizePolicy2);

        mpSeparatorLayout->addWidget(mpSeparator);

        mpTabCheck = new QCheckBox(CQReportDefinition);
        mpTabCheck->setObjectName(QString::fromUtf8("mpTabCheck"));

        mpSeparatorLayout->addWidget(mpTabCheck);


        gridLayout->addLayout(mpSeparatorLayout, 2, 1, 1, 2);

        mpPrecisionLabel = new QLabel(CQReportDefinition);
        mpPrecisionLabel->setObjectName(QString::fromUtf8("mpPrecisionLabel"));
        mpPrecisionLabel->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpPrecisionLabel->setWordWrap(false);

        gridLayout->addWidget(mpPrecisionLabel, 2, 3, 1, 1);

        mpPrecision = new QLineEdit(CQReportDefinition);
        mpPrecision->setObjectName(QString::fromUtf8("mpPrecision"));

        gridLayout->addWidget(mpPrecision, 2, 4, 1, 1);


        verticalLayout->addLayout(gridLayout);

        mpHorizontalLayout = new QHBoxLayout();
        mpHorizontalLayout->setSpacing(6);
        mpHorizontalLayout->setObjectName(QString::fromUtf8("mpHorizontalLayout"));
        mpVerticalBtnLayout = new QVBoxLayout();
        mpVerticalBtnLayout->setSpacing(6);
        mpVerticalBtnLayout->setObjectName(QString::fromUtf8("mpVerticalBtnLayout"));
        mpBtnAdvanced = new QPushButton(CQReportDefinition);
        mpBtnAdvanced->setObjectName(QString::fromUtf8("mpBtnAdvanced"));
        QSizePolicy sizePolicy3(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy3.setHorizontalStretch(0);
        sizePolicy3.setVerticalStretch(0);
        sizePolicy3.setHeightForWidth(mpBtnAdvanced->sizePolicy().hasHeightForWidth());
        mpBtnAdvanced->setSizePolicy(sizePolicy3);
        mpBtnAdvanced->setMinimumSize(QSize(90, 0));
        mpBtnAdvanced->setMaximumSize(QSize(112, 32767));

        mpVerticalBtnLayout->addWidget(mpBtnAdvanced);

        mpBtnItem = new QPushButton(CQReportDefinition);
        mpBtnItem->setObjectName(QString::fromUtf8("mpBtnItem"));
        sizePolicy3.setHeightForWidth(mpBtnItem->sizePolicy().hasHeightForWidth());
        mpBtnItem->setSizePolicy(sizePolicy3);
        mpBtnItem->setMaximumSize(QSize(112, 32767));

        mpVerticalBtnLayout->addWidget(mpBtnItem);

        mpBtnSeparator = new QPushButton(CQReportDefinition);
        mpBtnSeparator->setObjectName(QString::fromUtf8("mpBtnSeparator"));
        sizePolicy3.setHeightForWidth(mpBtnSeparator->sizePolicy().hasHeightForWidth());
        mpBtnSeparator->setSizePolicy(sizePolicy3);
        mpBtnSeparator->setMaximumSize(QSize(112, 32767));

        mpVerticalBtnLayout->addWidget(mpBtnSeparator);

        mpBtnText = new QPushButton(CQReportDefinition);
        mpBtnText->setObjectName(QString::fromUtf8("mpBtnText"));
        sizePolicy3.setHeightForWidth(mpBtnText->sizePolicy().hasHeightForWidth());
        mpBtnText->setSizePolicy(sizePolicy3);
        mpBtnText->setMaximumSize(QSize(112, 32767));

        mpVerticalBtnLayout->addWidget(mpBtnText);

        mpBtnDelete = new QPushButton(CQReportDefinition);
        mpBtnDelete->setObjectName(QString::fromUtf8("mpBtnDelete"));
        sizePolicy3.setHeightForWidth(mpBtnDelete->sizePolicy().hasHeightForWidth());
        mpBtnDelete->setSizePolicy(sizePolicy3);
        mpBtnDelete->setMaximumSize(QSize(112, 32767));

        mpVerticalBtnLayout->addWidget(mpBtnDelete);

        mpBtnUp = new QPushButton(CQReportDefinition);
        mpBtnUp->setObjectName(QString::fromUtf8("mpBtnUp"));
        sizePolicy3.setHeightForWidth(mpBtnUp->sizePolicy().hasHeightForWidth());
        mpBtnUp->setSizePolicy(sizePolicy3);
        mpBtnUp->setMaximumSize(QSize(112, 32767));

        mpVerticalBtnLayout->addWidget(mpBtnUp);

        mpBtnDown = new QPushButton(CQReportDefinition);
        mpBtnDown->setObjectName(QString::fromUtf8("mpBtnDown"));
        sizePolicy3.setHeightForWidth(mpBtnDown->sizePolicy().hasHeightForWidth());
        mpBtnDown->setSizePolicy(sizePolicy3);
        mpBtnDown->setMaximumSize(QSize(112, 32767));

        mpVerticalBtnLayout->addWidget(mpBtnDown);

        mpTitleCheck = new QCheckBox(CQReportDefinition);
        mpTitleCheck->setObjectName(QString::fromUtf8("mpTitleCheck"));
        sizePolicy3.setHeightForWidth(mpTitleCheck->sizePolicy().hasHeightForWidth());
        mpTitleCheck->setSizePolicy(sizePolicy3);
        mpTitleCheck->setMaximumSize(QSize(112, 32767));

        mpVerticalBtnLayout->addWidget(mpTitleCheck);

        mpSpacerBtnLayout = new QSpacerItem(20, 16, QSizePolicy::Minimum, QSizePolicy::Expanding);

        mpVerticalBtnLayout->addItem(mpSpacerBtnLayout);


        mpHorizontalLayout->addLayout(mpVerticalBtnLayout);

        mpReportSectionTab = new QTabWidget(CQReportDefinition);
        mpReportSectionTab->setObjectName(QString::fromUtf8("mpReportSectionTab"));
        QSizePolicy sizePolicy4(QSizePolicy::MinimumExpanding, QSizePolicy::MinimumExpanding);
        sizePolicy4.setHorizontalStretch(0);
        sizePolicy4.setVerticalStretch(0);
        sizePolicy4.setHeightForWidth(mpReportSectionTab->sizePolicy().hasHeightForWidth());
        mpReportSectionTab->setSizePolicy(sizePolicy4);
        mpReportSectionTab->setMinimumSize(QSize(0, 0));
        mpTableList = new Q3ListBox();
        mpTableList->setObjectName(QString::fromUtf8("mpTableList"));
        mpReportSectionTab->addTab(mpTableList, QString());
        mpHeaderList = new Q3ListBox();
        mpHeaderList->setObjectName(QString::fromUtf8("mpHeaderList"));
        mpReportSectionTab->addTab(mpHeaderList, QString());
        mpBodyList = new Q3ListBox();
        mpBodyList->setObjectName(QString::fromUtf8("mpBodyList"));
        mpReportSectionTab->addTab(mpBodyList, QString());
        mpFooterList = new Q3ListBox();
        mpFooterList->setObjectName(QString::fromUtf8("mpFooterList"));
        mpReportSectionTab->addTab(mpFooterList, QString());

        mpHorizontalLayout->addWidget(mpReportSectionTab);


        verticalLayout->addLayout(mpHorizontalLayout);

        line = new QFrame(CQReportDefinition);
        line->setObjectName(QString::fromUtf8("line"));
        line->setFrameShape(QFrame::HLine);
        line->setFrameShadow(QFrame::Sunken);

        verticalLayout->addWidget(line);

        mpHorizontalBtnLayout = new QHBoxLayout();
        mpHorizontalBtnLayout->setSpacing(6);
        mpHorizontalBtnLayout->setObjectName(QString::fromUtf8("mpHorizontalBtnLayout"));
        mpBtnCommit = new QPushButton(CQReportDefinition);
        mpBtnCommit->setObjectName(QString::fromUtf8("mpBtnCommit"));

        mpHorizontalBtnLayout->addWidget(mpBtnCommit);

        mpBtnRevert = new QPushButton(CQReportDefinition);
        mpBtnRevert->setObjectName(QString::fromUtf8("mpBtnRevert"));

        mpHorizontalBtnLayout->addWidget(mpBtnRevert);

        mpBtnNewReport = new QPushButton(CQReportDefinition);
        mpBtnNewReport->setObjectName(QString::fromUtf8("mpBtnNewReport"));

        mpHorizontalBtnLayout->addWidget(mpBtnNewReport);

        mpBtnDeleteReport = new QPushButton(CQReportDefinition);
        mpBtnDeleteReport->setObjectName(QString::fromUtf8("mpBtnDeleteReport"));

        mpHorizontalBtnLayout->addWidget(mpBtnDeleteReport);


        verticalLayout->addLayout(mpHorizontalBtnLayout);

        QWidget::setTabOrder(mpBtnCommit, mpBtnRevert);
        QWidget::setTabOrder(mpBtnRevert, mpBtnNewReport);
        QWidget::setTabOrder(mpBtnNewReport, mpBtnDeleteReport);

        retranslateUi(CQReportDefinition);
        QObject::connect(mpName, SIGNAL(textChanged(QString)), CQReportDefinition, SLOT(nameChanged(QString)));
        QObject::connect(mpTaskBox, SIGNAL(activated(QString)), CQReportDefinition, SLOT(taskChanged(QString)));
        QObject::connect(mpSeparator, SIGNAL(textChanged(QString)), CQReportDefinition, SLOT(separatorChanged(QString)));
        QObject::connect(mpPrecision, SIGNAL(textChanged(QString)), CQReportDefinition, SLOT(precisionChanged(QString)));
        QObject::connect(mpTabCheck, SIGNAL(clicked()), CQReportDefinition, SLOT(chkTabClicked()));
        QObject::connect(mpTitleCheck, SIGNAL(clicked()), CQReportDefinition, SLOT(chkTitleClicked()));
        QObject::connect(mpBtnAdvanced, SIGNAL(clicked()), CQReportDefinition, SLOT(btnAdvancedClicked()));
        QObject::connect(mpBtnItem, SIGNAL(clicked()), CQReportDefinition, SLOT(btnItemClicked()));
        QObject::connect(mpBtnSeparator, SIGNAL(clicked()), CQReportDefinition, SLOT(btnSeparatorClicked()));
        QObject::connect(mpBtnText, SIGNAL(clicked()), CQReportDefinition, SLOT(btnTextClicked()));
        QObject::connect(mpBtnDelete, SIGNAL(clicked()), CQReportDefinition, SLOT(btnDeleteClicked()));
        QObject::connect(mpBtnUp, SIGNAL(clicked()), CQReportDefinition, SLOT(btnUpClicked()));
        QObject::connect(mpBtnDown, SIGNAL(clicked()), CQReportDefinition, SLOT(btnDownClicked()));
        QObject::connect(mpBtnCommit, SIGNAL(clicked()), CQReportDefinition, SLOT(btnCommitClicked()));
        QObject::connect(mpBtnRevert, SIGNAL(clicked()), CQReportDefinition, SLOT(btnRevertClicked()));
        QObject::connect(mpBtnNewReport, SIGNAL(clicked()), CQReportDefinition, SLOT(btnNewReportClicked()));
        QObject::connect(mpBtnDeleteReport, SIGNAL(clicked()), CQReportDefinition, SLOT(btnDeleteReportClicked()));

        mpReportSectionTab->setCurrentIndex(1);


        QMetaObject::connectSlotsByName(CQReportDefinition);
    } // setupUi

    void retranslateUi(CopasiWidget *CQReportDefinition)
    {
        CQReportDefinition->setProperty("caption", QVariant(QApplication::translate("CQReportDefinition", "Report", 0, QApplication::UnicodeUTF8)));
        mpNameLabel->setText(QApplication::translate("CQReportDefinition", "Name", 0, QApplication::UnicodeUTF8));
        mpTaskLabel->setText(QApplication::translate("CQReportDefinition", "Task", 0, QApplication::UnicodeUTF8));
        mpCommentLabel->setText(QApplication::translate("CQReportDefinition", "Comment", 0, QApplication::UnicodeUTF8));
        mpSeparatorLabel->setText(QApplication::translate("CQReportDefinition", "Separator", 0, QApplication::UnicodeUTF8));
        mpTabCheck->setText(QApplication::translate("CQReportDefinition", "<tab>", 0, QApplication::UnicodeUTF8));
        mpPrecisionLabel->setText(QApplication::translate("CQReportDefinition", "Precision", 0, QApplication::UnicodeUTF8));
        mpBtnAdvanced->setText(QApplication::translate("CQReportDefinition", "Advanced >>", 0, QApplication::UnicodeUTF8));
        mpBtnItem->setText(QApplication::translate("CQReportDefinition", "Item", 0, QApplication::UnicodeUTF8));
        mpBtnSeparator->setText(QApplication::translate("CQReportDefinition", "Separator", 0, QApplication::UnicodeUTF8));
        mpBtnText->setText(QApplication::translate("CQReportDefinition", "Text", 0, QApplication::UnicodeUTF8));
        mpBtnDelete->setText(QApplication::translate("CQReportDefinition", "Delete", 0, QApplication::UnicodeUTF8));
        mpBtnUp->setText(QApplication::translate("CQReportDefinition", "Up", 0, QApplication::UnicodeUTF8));
        mpBtnDown->setText(QApplication::translate("CQReportDefinition", "Down", 0, QApplication::UnicodeUTF8));
        mpTitleCheck->setText(QApplication::translate("CQReportDefinition", "Title Row", 0, QApplication::UnicodeUTF8));
        mpReportSectionTab->setTabText(mpReportSectionTab->indexOf(mpTableList), QApplication::translate("CQReportDefinition", "Table", 0, QApplication::UnicodeUTF8));
        mpReportSectionTab->setTabText(mpReportSectionTab->indexOf(mpHeaderList), QApplication::translate("CQReportDefinition", "Header", 0, QApplication::UnicodeUTF8));
        mpReportSectionTab->setTabText(mpReportSectionTab->indexOf(mpBodyList), QApplication::translate("CQReportDefinition", "Body", 0, QApplication::UnicodeUTF8));
        mpReportSectionTab->setTabText(mpReportSectionTab->indexOf(mpFooterList), QApplication::translate("CQReportDefinition", "Footer", 0, QApplication::UnicodeUTF8));
        mpBtnCommit->setText(QApplication::translate("CQReportDefinition", "&Commit", 0, QApplication::UnicodeUTF8));
        mpBtnCommit->setShortcut(QApplication::translate("CQReportDefinition", "Alt+C", 0, QApplication::UnicodeUTF8));
        mpBtnRevert->setText(QApplication::translate("CQReportDefinition", "Revert", 0, QApplication::UnicodeUTF8));
        mpBtnRevert->setShortcut(QString());
        mpBtnNewReport->setText(QApplication::translate("CQReportDefinition", "&New", 0, QApplication::UnicodeUTF8));
        mpBtnNewReport->setShortcut(QApplication::translate("CQReportDefinition", "Alt+N", 0, QApplication::UnicodeUTF8));
        mpBtnDeleteReport->setText(QApplication::translate("CQReportDefinition", "&Delete", 0, QApplication::UnicodeUTF8));
        mpBtnDeleteReport->setShortcut(QApplication::translate("CQReportDefinition", "Alt+D", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQReportDefinition: public Ui_CQReportDefinition {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQREPORTDEFINITION_H
