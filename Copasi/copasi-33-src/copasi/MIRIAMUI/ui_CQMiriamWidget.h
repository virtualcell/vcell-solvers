/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'CQMiriamWidget.ui'
**
** Created: Thu Aug 18 12:47:00 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_CQMIRIAMWIDGET_H
#define UI_CQMIRIAMWIDGET_H

#include <Qt3Support/Q3MimeSourceFactory>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QDateTimeEdit>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QTableView>
#include <vector>
#include "copasi/UI/copasiWidget.h"

QT_BEGIN_NAMESPACE

class Ui_CQMiriamWidget
{
public:
    QGridLayout *gridLayout;
    QLabel *mpLblCreated;
    QSpacerItem *mpSpcCreated;
    QHBoxLayout *hboxLayout;
    QSpacerItem *horizontalSpacerLeft;
    QPushButton *mpBtnDelete;
    QPushButton *mpBtnClear;
    QSpacerItem *horizontalSpacerRight;
    QLabel *mpLblModified;
    QLabel *mpLblAuthors;
    QLabel *mpLblReferences;
    QLabel *mpLblDescription;
    QTableView *mpTblDescription;
    QTableView *mpTblModified;
    QTableView *mpTblReferences;
    QTableView *mpTblAuthors;
    QDateTimeEdit *mpDTCreated;

    void setupUi(CopasiWidget *CQMiriamWidget)
    {
        if (CQMiriamWidget->objectName().isEmpty())
            CQMiriamWidget->setObjectName(QString::fromUtf8("CQMiriamWidget"));
        CQMiriamWidget->resize(541, 456);
        gridLayout = new QGridLayout(CQMiriamWidget);
        gridLayout->setSpacing(6);
        gridLayout->setContentsMargins(11, 11, 11, 11);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        mpLblCreated = new QLabel(CQMiriamWidget);
        mpLblCreated->setObjectName(QString::fromUtf8("mpLblCreated"));
        mpLblCreated->setWordWrap(false);

        gridLayout->addWidget(mpLblCreated, 0, 0, 1, 1);

        mpSpcCreated = new QSpacerItem(80, 20, QSizePolicy::MinimumExpanding, QSizePolicy::Minimum);

        gridLayout->addItem(mpSpcCreated, 0, 3, 1, 1);

        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        horizontalSpacerLeft = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout->addItem(horizontalSpacerLeft);

        mpBtnDelete = new QPushButton(CQMiriamWidget);
        mpBtnDelete->setObjectName(QString::fromUtf8("mpBtnDelete"));
        mpBtnDelete->setFocusPolicy(Qt::NoFocus);

        hboxLayout->addWidget(mpBtnDelete);

        mpBtnClear = new QPushButton(CQMiriamWidget);
        mpBtnClear->setObjectName(QString::fromUtf8("mpBtnClear"));
        mpBtnClear->setFocusPolicy(Qt::NoFocus);

        hboxLayout->addWidget(mpBtnClear);

        horizontalSpacerRight = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout->addItem(horizontalSpacerRight);


        gridLayout->addLayout(hboxLayout, 6, 0, 1, 4);

        mpLblModified = new QLabel(CQMiriamWidget);
        mpLblModified->setObjectName(QString::fromUtf8("mpLblModified"));
        mpLblModified->setAlignment(Qt::AlignTop);
        mpLblModified->setWordWrap(false);

        gridLayout->addWidget(mpLblModified, 4, 0, 1, 1);

        mpLblAuthors = new QLabel(CQMiriamWidget);
        mpLblAuthors->setObjectName(QString::fromUtf8("mpLblAuthors"));
        mpLblAuthors->setAlignment(Qt::AlignTop);
        mpLblAuthors->setWordWrap(false);

        gridLayout->addWidget(mpLblAuthors, 1, 0, 1, 1);

        mpLblReferences = new QLabel(CQMiriamWidget);
        mpLblReferences->setObjectName(QString::fromUtf8("mpLblReferences"));
        mpLblReferences->setAlignment(Qt::AlignTop);
        mpLblReferences->setWordWrap(false);

        gridLayout->addWidget(mpLblReferences, 2, 0, 1, 1);

        mpLblDescription = new QLabel(CQMiriamWidget);
        mpLblDescription->setObjectName(QString::fromUtf8("mpLblDescription"));
        mpLblDescription->setAlignment(Qt::AlignTop);
        mpLblDescription->setWordWrap(false);

        gridLayout->addWidget(mpLblDescription, 3, 0, 1, 1);

        mpTblDescription = new QTableView(CQMiriamWidget);
        mpTblDescription->setObjectName(QString::fromUtf8("mpTblDescription"));
        mpTblDescription->setEditTriggers(QAbstractItemView::DoubleClicked|QAbstractItemView::EditKeyPressed|QAbstractItemView::SelectedClicked);
        mpTblDescription->setAlternatingRowColors(false);
        mpTblDescription->setSelectionMode(QAbstractItemView::ExtendedSelection);
        mpTblDescription->setSelectionBehavior(QAbstractItemView::SelectRows);
        mpTblDescription->setSortingEnabled(true);

        gridLayout->addWidget(mpTblDescription, 3, 1, 1, 3);

        mpTblModified = new QTableView(CQMiriamWidget);
        mpTblModified->setObjectName(QString::fromUtf8("mpTblModified"));
        mpTblModified->setEditTriggers(QAbstractItemView::DoubleClicked|QAbstractItemView::EditKeyPressed|QAbstractItemView::SelectedClicked);
        mpTblModified->setAlternatingRowColors(false);
        mpTblModified->setSelectionMode(QAbstractItemView::ExtendedSelection);
        mpTblModified->setSelectionBehavior(QAbstractItemView::SelectRows);
        mpTblModified->setSortingEnabled(true);

        gridLayout->addWidget(mpTblModified, 4, 1, 1, 3);

        mpTblReferences = new QTableView(CQMiriamWidget);
        mpTblReferences->setObjectName(QString::fromUtf8("mpTblReferences"));
        mpTblReferences->setEditTriggers(QAbstractItemView::DoubleClicked|QAbstractItemView::EditKeyPressed|QAbstractItemView::SelectedClicked);
        mpTblReferences->setAlternatingRowColors(false);
        mpTblReferences->setSelectionMode(QAbstractItemView::ExtendedSelection);
        mpTblReferences->setSelectionBehavior(QAbstractItemView::SelectRows);
        mpTblReferences->setSortingEnabled(true);

        gridLayout->addWidget(mpTblReferences, 2, 1, 1, 3);

        mpTblAuthors = new QTableView(CQMiriamWidget);
        mpTblAuthors->setObjectName(QString::fromUtf8("mpTblAuthors"));
        mpTblAuthors->setEditTriggers(QAbstractItemView::DoubleClicked|QAbstractItemView::EditKeyPressed|QAbstractItemView::SelectedClicked);
        mpTblAuthors->setAlternatingRowColors(false);
        mpTblAuthors->setSelectionMode(QAbstractItemView::ExtendedSelection);
        mpTblAuthors->setSelectionBehavior(QAbstractItemView::SelectRows);
        mpTblAuthors->setSortingEnabled(true);

        gridLayout->addWidget(mpTblAuthors, 1, 1, 1, 3);

        mpDTCreated = new QDateTimeEdit(CQMiriamWidget);
        mpDTCreated->setObjectName(QString::fromUtf8("mpDTCreated"));

        gridLayout->addWidget(mpDTCreated, 0, 1, 1, 1);

        QWidget::setTabOrder(mpDTCreated, mpTblAuthors);
        QWidget::setTabOrder(mpTblAuthors, mpTblReferences);
        QWidget::setTabOrder(mpTblReferences, mpTblDescription);
        QWidget::setTabOrder(mpTblDescription, mpTblModified);
        QWidget::setTabOrder(mpTblModified, mpBtnDelete);
        QWidget::setTabOrder(mpBtnDelete, mpBtnClear);

        retranslateUi(CQMiriamWidget);
        QObject::connect(mpBtnClear, SIGNAL(pressed()), CQMiriamWidget, SLOT(slotBtnClearClicked()));
        QObject::connect(mpBtnDelete, SIGNAL(pressed()), CQMiriamWidget, SLOT(slotBtnDeleteClicked()));
        QObject::connect(mpDTCreated, SIGNAL(dateTimeChanged(QDateTime)), CQMiriamWidget, SLOT(slotCreatedDTChanged(QDateTime)));

        QMetaObject::connectSlotsByName(CQMiriamWidget);
    } // setupUi

    void retranslateUi(CopasiWidget *CQMiriamWidget)
    {
        CQMiriamWidget->setProperty("caption", QVariant(QApplication::translate("CQMiriamWidget", "MIRIAM Annotation", 0, QApplication::UnicodeUTF8)));
        mpLblCreated->setText(QApplication::translate("CQMiriamWidget", "Created at", 0, QApplication::UnicodeUTF8));
        mpBtnDelete->setText(QApplication::translate("CQMiriamWidget", "Delete", 0, QApplication::UnicodeUTF8));
        mpBtnClear->setText(QApplication::translate("CQMiriamWidget", "Delete All", 0, QApplication::UnicodeUTF8));
        mpLblModified->setText(QApplication::translate("CQMiriamWidget", "Modified at", 0, QApplication::UnicodeUTF8));
        mpLblAuthors->setText(QApplication::translate("CQMiriamWidget", "Authors", 0, QApplication::UnicodeUTF8));
        mpLblReferences->setText(QApplication::translate("CQMiriamWidget", "References", 0, QApplication::UnicodeUTF8));
        mpLblDescription->setText(QApplication::translate("CQMiriamWidget", "Description", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQMiriamWidget: public Ui_CQMiriamWidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_CQMIRIAMWIDGET_H
