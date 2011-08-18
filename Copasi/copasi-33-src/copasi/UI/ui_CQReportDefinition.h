/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQReportDefinition.ui'
**
** Created: Thu Aug 18 12:47:30 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQREPORTDEFINITION_H
#define UI_CQREPORTDEFINITION_H

#include <Qt3Support/Q3Frame>
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
#include <QtGui/QTextEdit>
#include <QtGui/QVBoxLayout>
#include <Qt3Support/Q3ListBoxPixmap>
#include "copasiWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQReportDefinition
{
public:
    QVBoxLayout *vboxLayout;
    QFrame *mpReportFrame;
    QGridLayout *gridLayout;
    QLineEdit *mpName;
    QComboBox *mpTaskBox;
    QLabel *mpTaskLabel;
    QLabel *mpNameLabel;
    QLabel *mpCommentLabel;
    QLabel *mpSeparatorLabel;
    QHBoxLayout *hboxLayout;
    QLineEdit *mpSeparator;
    QCheckBox *mpTabCheck;
    QLabel *mpPrecisionLabel;
    QLineEdit *mpPrecision;
    QTextEdit *mpCommentEdit;
    QFrame *mpEditListsFrame;
    QHBoxLayout *hboxLayout1;
    QVBoxLayout *vboxLayout1;
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
    QHBoxLayout *hboxLayout2;
    QPushButton *mpBtnCommit;
    QPushButton *mpBtnRevert;
    QPushButton *mpBtnNewReport;
    QPushButton *mpBtnDeleteReport;

    void setupUi(CopasiWidget *CQReportDefinition)
    {
        if (CQReportDefinition->objectName().isEmpty())
            CQReportDefinition->setObjectName(QString::fromUtf8("CQReportDefinition"));
        CQReportDefinition->resize(463, 533);
        vboxLayout = new QVBoxLayout(CQReportDefinition);
        vboxLayout->setSpacing(6);
        vboxLayout->setContentsMargins(11, 11, 11, 11);
        vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
        mpReportFrame = new QFrame(CQReportDefinition);
        mpReportFrame->setObjectName(QString::fromUtf8("mpReportFrame"));
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(mpReportFrame->sizePolicy().hasHeightForWidth());
        mpReportFrame->setSizePolicy(sizePolicy);
        mpReportFrame->setFrameShape(QFrame::Box);
        mpReportFrame->setFrameShadow(QFrame::Sunken);
        gridLayout = new QGridLayout(mpReportFrame);
        gridLayout->setSpacing(6);
        gridLayout->setContentsMargins(11, 11, 11, 11);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        mpName = new QLineEdit(mpReportFrame);
        mpName->setObjectName(QString::fromUtf8("mpName"));
        QSizePolicy sizePolicy1(QSizePolicy::MinimumExpanding, QSizePolicy::Fixed);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(mpName->sizePolicy().hasHeightForWidth());
        mpName->setSizePolicy(sizePolicy1);
        mpName->setMinimumSize(QSize(150, 0));

        gridLayout->addWidget(mpName, 0, 1, 1, 1);

        mpTaskBox = new QComboBox(mpReportFrame);
        mpTaskBox->setObjectName(QString::fromUtf8("mpTaskBox"));
        sizePolicy1.setHeightForWidth(mpTaskBox->sizePolicy().hasHeightForWidth());
        mpTaskBox->setSizePolicy(sizePolicy1);
        mpTaskBox->setMinimumSize(QSize(150, 0));

        gridLayout->addWidget(mpTaskBox, 0, 3, 1, 2);

        mpTaskLabel = new QLabel(mpReportFrame);
        mpTaskLabel->setObjectName(QString::fromUtf8("mpTaskLabel"));
        mpTaskLabel->setMinimumSize(QSize(45, 0));
        mpTaskLabel->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpTaskLabel->setWordWrap(false);

        gridLayout->addWidget(mpTaskLabel, 0, 2, 1, 1);

        mpNameLabel = new QLabel(mpReportFrame);
        mpNameLabel->setObjectName(QString::fromUtf8("mpNameLabel"));
        mpNameLabel->setMinimumSize(QSize(62, 0));
        mpNameLabel->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpNameLabel->setWordWrap(false);

        gridLayout->addWidget(mpNameLabel, 0, 0, 1, 1);

        mpCommentLabel = new QLabel(mpReportFrame);
        mpCommentLabel->setObjectName(QString::fromUtf8("mpCommentLabel"));
        mpCommentLabel->setMinimumSize(QSize(62, 0));
        mpCommentLabel->setAlignment(Qt::AlignRight|Qt::AlignTop|Qt::AlignTrailing);
        mpCommentLabel->setWordWrap(false);

        gridLayout->addWidget(mpCommentLabel, 1, 0, 1, 1);

        mpSeparatorLabel = new QLabel(mpReportFrame);
        mpSeparatorLabel->setObjectName(QString::fromUtf8("mpSeparatorLabel"));
        mpSeparatorLabel->setMinimumSize(QSize(62, 0));
        mpSeparatorLabel->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpSeparatorLabel->setWordWrap(false);

        gridLayout->addWidget(mpSeparatorLabel, 2, 0, 1, 1);

        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        mpSeparator = new QLineEdit(mpReportFrame);
        mpSeparator->setObjectName(QString::fromUtf8("mpSeparator"));
        QSizePolicy sizePolicy2(QSizePolicy::Expanding, QSizePolicy::Fixed);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(mpSeparator->sizePolicy().hasHeightForWidth());
        mpSeparator->setSizePolicy(sizePolicy2);

        hboxLayout->addWidget(mpSeparator);

        mpTabCheck = new QCheckBox(mpReportFrame);
        mpTabCheck->setObjectName(QString::fromUtf8("mpTabCheck"));

        hboxLayout->addWidget(mpTabCheck);


        gridLayout->addLayout(hboxLayout, 2, 1, 1, 1);

        mpPrecisionLabel = new QLabel(mpReportFrame);
        mpPrecisionLabel->setObjectName(QString::fromUtf8("mpPrecisionLabel"));
        mpPrecisionLabel->setAlignment(Qt::AlignRight|Qt::AlignTrailing|Qt::AlignVCenter);
        mpPrecisionLabel->setWordWrap(false);

        gridLayout->addWidget(mpPrecisionLabel, 2, 3, 1, 1);

        mpPrecision = new QLineEdit(mpReportFrame);
        mpPrecision->setObjectName(QString::fromUtf8("mpPrecision"));

        gridLayout->addWidget(mpPrecision, 2, 4, 1, 1);

        mpCommentEdit = new QTextEdit(mpReportFrame);
        mpCommentEdit->setObjectName(QString::fromUtf8("mpCommentEdit"));
        QSizePolicy sizePolicy3(QSizePolicy::Preferred, QSizePolicy::Preferred);
        sizePolicy3.setHorizontalStretch(0);
        sizePolicy3.setVerticalStretch(0);
        sizePolicy3.setHeightForWidth(mpCommentEdit->sizePolicy().hasHeightForWidth());
        mpCommentEdit->setSizePolicy(sizePolicy3);
        mpCommentEdit->setMaximumSize(QSize(16777215, 50));

        gridLayout->addWidget(mpCommentEdit, 1, 1, 1, 4);


        vboxLayout->addWidget(mpReportFrame);

        mpEditListsFrame = new QFrame(CQReportDefinition);
        mpEditListsFrame->setObjectName(QString::fromUtf8("mpEditListsFrame"));
        QSizePolicy sizePolicy4(QSizePolicy::Preferred, QSizePolicy::Expanding);
        sizePolicy4.setHorizontalStretch(0);
        sizePolicy4.setVerticalStretch(0);
        sizePolicy4.setHeightForWidth(mpEditListsFrame->sizePolicy().hasHeightForWidth());
        mpEditListsFrame->setSizePolicy(sizePolicy4);
        mpEditListsFrame->setFrameShape(QFrame::Box);
        mpEditListsFrame->setFrameShadow(QFrame::Sunken);
        hboxLayout1 = new QHBoxLayout(mpEditListsFrame);
        hboxLayout1->setSpacing(6);
        hboxLayout1->setContentsMargins(11, 11, 11, 11);
        hboxLayout1->setObjectName(QString::fromUtf8("hboxLayout1"));
        vboxLayout1 = new QVBoxLayout();
        vboxLayout1->setSpacing(6);
        vboxLayout1->setObjectName(QString::fromUtf8("vboxLayout1"));
        mpBtnAdvanced = new QPushButton(mpEditListsFrame);
        mpBtnAdvanced->setObjectName(QString::fromUtf8("mpBtnAdvanced"));
        sizePolicy.setHeightForWidth(mpBtnAdvanced->sizePolicy().hasHeightForWidth());
        mpBtnAdvanced->setSizePolicy(sizePolicy);
        mpBtnAdvanced->setMinimumSize(QSize(90, 0));
        mpBtnAdvanced->setMaximumSize(QSize(112, 32767));

        vboxLayout1->addWidget(mpBtnAdvanced);

        mpBtnItem = new QPushButton(mpEditListsFrame);
        mpBtnItem->setObjectName(QString::fromUtf8("mpBtnItem"));
        sizePolicy.setHeightForWidth(mpBtnItem->sizePolicy().hasHeightForWidth());
        mpBtnItem->setSizePolicy(sizePolicy);
        mpBtnItem->setMaximumSize(QSize(112, 32767));

        vboxLayout1->addWidget(mpBtnItem);

        mpBtnSeparator = new QPushButton(mpEditListsFrame);
        mpBtnSeparator->setObjectName(QString::fromUtf8("mpBtnSeparator"));
        sizePolicy.setHeightForWidth(mpBtnSeparator->sizePolicy().hasHeightForWidth());
        mpBtnSeparator->setSizePolicy(sizePolicy);
        mpBtnSeparator->setMaximumSize(QSize(112, 32767));

        vboxLayout1->addWidget(mpBtnSeparator);

        mpBtnText = new QPushButton(mpEditListsFrame);
        mpBtnText->setObjectName(QString::fromUtf8("mpBtnText"));
        sizePolicy.setHeightForWidth(mpBtnText->sizePolicy().hasHeightForWidth());
        mpBtnText->setSizePolicy(sizePolicy);
        mpBtnText->setMaximumSize(QSize(112, 32767));

        vboxLayout1->addWidget(mpBtnText);

        mpBtnDelete = new QPushButton(mpEditListsFrame);
        mpBtnDelete->setObjectName(QString::fromUtf8("mpBtnDelete"));
        sizePolicy.setHeightForWidth(mpBtnDelete->sizePolicy().hasHeightForWidth());
        mpBtnDelete->setSizePolicy(sizePolicy);
        mpBtnDelete->setMaximumSize(QSize(112, 32767));

        vboxLayout1->addWidget(mpBtnDelete);

        mpBtnUp = new QPushButton(mpEditListsFrame);
        mpBtnUp->setObjectName(QString::fromUtf8("mpBtnUp"));
        sizePolicy.setHeightForWidth(mpBtnUp->sizePolicy().hasHeightForWidth());
        mpBtnUp->setSizePolicy(sizePolicy);
        mpBtnUp->setMaximumSize(QSize(112, 32767));

        vboxLayout1->addWidget(mpBtnUp);

        mpBtnDown = new QPushButton(mpEditListsFrame);
        mpBtnDown->setObjectName(QString::fromUtf8("mpBtnDown"));
        sizePolicy.setHeightForWidth(mpBtnDown->sizePolicy().hasHeightForWidth());
        mpBtnDown->setSizePolicy(sizePolicy);
        mpBtnDown->setMaximumSize(QSize(112, 32767));

        vboxLayout1->addWidget(mpBtnDown);

        mpTitleCheck = new QCheckBox(mpEditListsFrame);
        mpTitleCheck->setObjectName(QString::fromUtf8("mpTitleCheck"));
        sizePolicy.setHeightForWidth(mpTitleCheck->sizePolicy().hasHeightForWidth());
        mpTitleCheck->setSizePolicy(sizePolicy);
        mpTitleCheck->setMaximumSize(QSize(112, 32767));

        vboxLayout1->addWidget(mpTitleCheck);

        mpSpacerBtnLayout = new QSpacerItem(20, 16, QSizePolicy::Minimum, QSizePolicy::Expanding);

        vboxLayout1->addItem(mpSpacerBtnLayout);


        hboxLayout1->addLayout(vboxLayout1);

        mpReportSectionTab = new QTabWidget(mpEditListsFrame);
        mpReportSectionTab->setObjectName(QString::fromUtf8("mpReportSectionTab"));
        sizePolicy3.setHeightForWidth(mpReportSectionTab->sizePolicy().hasHeightForWidth());
        mpReportSectionTab->setSizePolicy(sizePolicy3);
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

        hboxLayout1->addWidget(mpReportSectionTab);


        vboxLayout->addWidget(mpEditListsFrame);

        hboxLayout2 = new QHBoxLayout();
        hboxLayout2->setSpacing(6);
        hboxLayout2->setContentsMargins(0, 0, 0, 0);
        hboxLayout2->setObjectName(QString::fromUtf8("hboxLayout2"));
        mpBtnCommit = new QPushButton(CQReportDefinition);
        mpBtnCommit->setObjectName(QString::fromUtf8("mpBtnCommit"));

        hboxLayout2->addWidget(mpBtnCommit);

        mpBtnRevert = new QPushButton(CQReportDefinition);
        mpBtnRevert->setObjectName(QString::fromUtf8("mpBtnRevert"));

        hboxLayout2->addWidget(mpBtnRevert);

        mpBtnNewReport = new QPushButton(CQReportDefinition);
        mpBtnNewReport->setObjectName(QString::fromUtf8("mpBtnNewReport"));

        hboxLayout2->addWidget(mpBtnNewReport);

        mpBtnDeleteReport = new QPushButton(CQReportDefinition);
        mpBtnDeleteReport->setObjectName(QString::fromUtf8("mpBtnDeleteReport"));

        hboxLayout2->addWidget(mpBtnDeleteReport);


        vboxLayout->addLayout(hboxLayout2);

        QWidget::setTabOrder(mpName, mpTaskBox);
        QWidget::setTabOrder(mpTaskBox, mpCommentEdit);
        QWidget::setTabOrder(mpCommentEdit, mpSeparator);
        QWidget::setTabOrder(mpSeparator, mpTabCheck);
        QWidget::setTabOrder(mpTabCheck, mpBtnAdvanced);
        QWidget::setTabOrder(mpBtnAdvanced, mpBtnItem);
        QWidget::setTabOrder(mpBtnItem, mpBtnSeparator);
        QWidget::setTabOrder(mpBtnSeparator, mpBtnText);
        QWidget::setTabOrder(mpBtnText, mpBtnDelete);
        QWidget::setTabOrder(mpBtnDelete, mpBtnUp);
        QWidget::setTabOrder(mpBtnUp, mpBtnDown);
        QWidget::setTabOrder(mpBtnDown, mpTitleCheck);
        QWidget::setTabOrder(mpTitleCheck, mpReportSectionTab);
        QWidget::setTabOrder(mpReportSectionTab, mpBtnCommit);
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
        QObject::connect(mpCommentEdit, SIGNAL(textChanged()), CQReportDefinition, SLOT(commentChanged()));

        QMetaObject::connectSlotsByName(CQReportDefinition);
    } // setupUi

    void retranslateUi(CopasiWidget *CQReportDefinition)
    {
        CQReportDefinition->setProperty("caption", QVariant(QApplication::translate("CQReportDefinition", "Report", 0, QApplication::UnicodeUTF8)));
        mpTaskLabel->setText(QApplication::translate("CQReportDefinition", "Task", 0, QApplication::UnicodeUTF8));
        mpNameLabel->setText(QApplication::translate("CQReportDefinition", "Name", 0, QApplication::UnicodeUTF8));
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
