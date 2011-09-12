/*
 All rights reserved. 
*/

/********************************************************************************
** Form generated from reading UI file 'StateSubwidget.ui'
**
** Created: Sun Sep 11 10:59:19 2011
**      by: Qt User Interface Compiler version 4.7.3
**
** WARNING! All changes made in this file will be lost when recompiling UI file!
********************************************************************************/

#ifndef UI_STATESUBWIDGET_H
#define UI_STATESUBWIDGET_H

#include <Qt3Support/Q3Header>
#include <Qt3Support/Q3Table>
#include <Qt3Support/Q3TextEdit>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QGridLayout>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QSpacerItem>
#include <QtGui/QSplitter>
#include <QtGui/QTabWidget>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>
#include "CQArrayAnnotationsWidget.h"
#include "copasi/UI/copasiWidget.h"

QT_BEGIN_NAMESPACE

class Ui_StateSubwidget
{
public:
    QVBoxLayout *verticalLayout;
    QHBoxLayout *horizontalLayout_2;
    QLabel *topLabel;
    QSpacerItem *horizontalSpacer;
    QTabWidget *mpTabWidget;
    QWidget *mpOptimizationPage;
    QVBoxLayout *vboxLayout;
    Q3TextEdit *optimizationResultText;
    QWidget *mpMetabolitesPage;
    QVBoxLayout *vboxLayout1;
    Q3Table *mpTblMetabolites;
    QWidget *mpCompartmentsPage;
    QVBoxLayout *vboxLayout2;
    Q3Table *mpTblCompartments;
    QWidget *mpModelQuantitiesPage;
    QHBoxLayout *hboxLayout;
    Q3Table *mpTblModelValues;
    QWidget *mpReactionsPage;
    QVBoxLayout *vboxLayout3;
    Q3Table *mpTblReactions;
    QWidget *mpJacobianPage;
    QVBoxLayout *verticalLayout_2;
    QSplitter *splitter;
    QWidget *layoutWidget;
    QGridLayout *gridLayout;
    QSpacerItem *spacer3;
    QLabel *textLabelJacobian;
    CQArrayAnnotationsWidget *mpJacobianAnnotationWidget;
    QWidget *layoutWidget1;
    QGridLayout *gridLayout1;
    Q3Table *tableEigenValues;
    QSpacerItem *spacer4;
    QLabel *textLabelEigenvalues;
    QWidget *mpReducedJacobianPage;
    QVBoxLayout *verticalLayout_3;
    QSplitter *splitter_2;
    QWidget *layoutWidget2;
    QGridLayout *gridLayout2;
    CQArrayAnnotationsWidget *mpJacobianXAnnotationWidget;
    QSpacerItem *spacer3_2;
    QLabel *textLabelJacobianX;
    QWidget *layoutWidget3;
    QGridLayout *gridLayout3;
    Q3Table *tableEigenValuesX;
    QSpacerItem *spacer4_2;
    QLabel *textLabelEigenvaluesX;
    QWidget *mpStabilityPage;
    QVBoxLayout *vboxLayout4;
    Q3TextEdit *stabilityTextEdit;
    QWidget *mpProtocolPage;
    QGridLayout *gridLayout4;
    Q3TextEdit *protocolTextEdit;

    void setupUi(CopasiWidget *StateSubwidget)
    {
        if (StateSubwidget->objectName().isEmpty())
            StateSubwidget->setObjectName(QString::fromUtf8("StateSubwidget"));
        StateSubwidget->resize(443, 264);
        QSizePolicy sizePolicy(QSizePolicy::Preferred, QSizePolicy::Expanding);
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(StateSubwidget->sizePolicy().hasHeightForWidth());
        StateSubwidget->setSizePolicy(sizePolicy);
        verticalLayout = new QVBoxLayout(StateSubwidget);
        verticalLayout->setSpacing(6);
        verticalLayout->setContentsMargins(11, 11, 11, 11);
        verticalLayout->setObjectName(QString::fromUtf8("verticalLayout"));
        horizontalLayout_2 = new QHBoxLayout();
        horizontalLayout_2->setSpacing(6);
        horizontalLayout_2->setObjectName(QString::fromUtf8("horizontalLayout_2"));
        topLabel = new QLabel(StateSubwidget);
        topLabel->setObjectName(QString::fromUtf8("topLabel"));
        topLabel->setWordWrap(false);

        horizontalLayout_2->addWidget(topLabel);

        horizontalSpacer = new QSpacerItem(40, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        horizontalLayout_2->addItem(horizontalSpacer);


        verticalLayout->addLayout(horizontalLayout_2);

        mpTabWidget = new QTabWidget(StateSubwidget);
        mpTabWidget->setObjectName(QString::fromUtf8("mpTabWidget"));
        QSizePolicy sizePolicy1(QSizePolicy::Expanding, QSizePolicy::Expanding);
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(mpTabWidget->sizePolicy().hasHeightForWidth());
        mpTabWidget->setSizePolicy(sizePolicy1);
        mpTabWidget->setUsesScrollButtons(true);
        mpOptimizationPage = new QWidget();
        mpOptimizationPage->setObjectName(QString::fromUtf8("mpOptimizationPage"));
        vboxLayout = new QVBoxLayout(mpOptimizationPage);
        vboxLayout->setSpacing(6);
        vboxLayout->setContentsMargins(0, 0, 0, 0);
        vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
        optimizationResultText = new Q3TextEdit(mpOptimizationPage);
        optimizationResultText->setObjectName(QString::fromUtf8("optimizationResultText"));
        optimizationResultText->setReadOnly(true);

        vboxLayout->addWidget(optimizationResultText);

        mpTabWidget->addTab(mpOptimizationPage, QString());
        mpMetabolitesPage = new QWidget();
        mpMetabolitesPage->setObjectName(QString::fromUtf8("mpMetabolitesPage"));
        vboxLayout1 = new QVBoxLayout(mpMetabolitesPage);
        vboxLayout1->setSpacing(6);
        vboxLayout1->setContentsMargins(0, 0, 0, 0);
        vboxLayout1->setObjectName(QString::fromUtf8("vboxLayout1"));
        mpTblMetabolites = new Q3Table(mpMetabolitesPage);
        mpTblMetabolites->setObjectName(QString::fromUtf8("mpTblMetabolites"));
        mpTblMetabolites->setNumRows(0);
        mpTblMetabolites->setNumCols(7);
        mpTblMetabolites->setReadOnly(true);

        vboxLayout1->addWidget(mpTblMetabolites);

        mpTabWidget->addTab(mpMetabolitesPage, QString());
        mpCompartmentsPage = new QWidget();
        mpCompartmentsPage->setObjectName(QString::fromUtf8("mpCompartmentsPage"));
        vboxLayout2 = new QVBoxLayout(mpCompartmentsPage);
        vboxLayout2->setSpacing(6);
        vboxLayout2->setContentsMargins(0, 0, 0, 0);
        vboxLayout2->setObjectName(QString::fromUtf8("vboxLayout2"));
        mpTblCompartments = new Q3Table(mpCompartmentsPage);
        mpTblCompartments->setObjectName(QString::fromUtf8("mpTblCompartments"));
        mpTblCompartments->setNumRows(0);
        mpTblCompartments->setNumCols(4);
        mpTblCompartments->setReadOnly(true);

        vboxLayout2->addWidget(mpTblCompartments);

        mpTabWidget->addTab(mpCompartmentsPage, QString());
        mpModelQuantitiesPage = new QWidget();
        mpModelQuantitiesPage->setObjectName(QString::fromUtf8("mpModelQuantitiesPage"));
        hboxLayout = new QHBoxLayout(mpModelQuantitiesPage);
        hboxLayout->setSpacing(6);
        hboxLayout->setContentsMargins(0, 0, 0, 0);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        mpTblModelValues = new Q3Table(mpModelQuantitiesPage);
        mpTblModelValues->setObjectName(QString::fromUtf8("mpTblModelValues"));
        mpTblModelValues->setNumRows(0);
        mpTblModelValues->setNumCols(4);

        hboxLayout->addWidget(mpTblModelValues);

        mpTabWidget->addTab(mpModelQuantitiesPage, QString());
        mpReactionsPage = new QWidget();
        mpReactionsPage->setObjectName(QString::fromUtf8("mpReactionsPage"));
        vboxLayout3 = new QVBoxLayout(mpReactionsPage);
        vboxLayout3->setSpacing(6);
        vboxLayout3->setContentsMargins(0, 0, 0, 0);
        vboxLayout3->setObjectName(QString::fromUtf8("vboxLayout3"));
        mpTblReactions = new Q3Table(mpReactionsPage);
        mpTblReactions->setObjectName(QString::fromUtf8("mpTblReactions"));
        mpTblReactions->setNumRows(0);
        mpTblReactions->setNumCols(4);
        mpTblReactions->setReadOnly(true);

        vboxLayout3->addWidget(mpTblReactions);

        mpTabWidget->addTab(mpReactionsPage, QString());
        mpJacobianPage = new QWidget();
        mpJacobianPage->setObjectName(QString::fromUtf8("mpJacobianPage"));
        verticalLayout_2 = new QVBoxLayout(mpJacobianPage);
        verticalLayout_2->setSpacing(6);
        verticalLayout_2->setContentsMargins(0, 0, 0, 0);
        verticalLayout_2->setObjectName(QString::fromUtf8("verticalLayout_2"));
        splitter = new QSplitter(mpJacobianPage);
        splitter->setObjectName(QString::fromUtf8("splitter"));
        splitter->setOrientation(Qt::Vertical);
        layoutWidget = new QWidget(splitter);
        layoutWidget->setObjectName(QString::fromUtf8("layoutWidget"));
        gridLayout = new QGridLayout(layoutWidget);
        gridLayout->setSpacing(6);
        gridLayout->setContentsMargins(11, 11, 11, 11);
        gridLayout->setObjectName(QString::fromUtf8("gridLayout"));
        gridLayout->setContentsMargins(0, 0, 0, 0);
        spacer3 = new QSpacerItem(516, 21, QSizePolicy::Expanding, QSizePolicy::Minimum);

        gridLayout->addItem(spacer3, 0, 1, 1, 1);

        textLabelJacobian = new QLabel(layoutWidget);
        textLabelJacobian->setObjectName(QString::fromUtf8("textLabelJacobian"));
        QSizePolicy sizePolicy2(QSizePolicy::Preferred, QSizePolicy::Fixed);
        sizePolicy2.setHorizontalStretch(0);
        sizePolicy2.setVerticalStretch(0);
        sizePolicy2.setHeightForWidth(textLabelJacobian->sizePolicy().hasHeightForWidth());
        textLabelJacobian->setSizePolicy(sizePolicy2);
        textLabelJacobian->setWordWrap(false);

        gridLayout->addWidget(textLabelJacobian, 0, 0, 1, 1);

        mpJacobianAnnotationWidget = new CQArrayAnnotationsWidget(layoutWidget);
        mpJacobianAnnotationWidget->setObjectName(QString::fromUtf8("mpJacobianAnnotationWidget"));
        sizePolicy1.setHeightForWidth(mpJacobianAnnotationWidget->sizePolicy().hasHeightForWidth());
        mpJacobianAnnotationWidget->setSizePolicy(sizePolicy1);

        gridLayout->addWidget(mpJacobianAnnotationWidget, 1, 0, 1, 2);

        splitter->addWidget(layoutWidget);
        layoutWidget1 = new QWidget(splitter);
        layoutWidget1->setObjectName(QString::fromUtf8("layoutWidget1"));
        gridLayout1 = new QGridLayout(layoutWidget1);
        gridLayout1->setSpacing(6);
        gridLayout1->setContentsMargins(11, 11, 11, 11);
        gridLayout1->setObjectName(QString::fromUtf8("gridLayout1"));
        gridLayout1->setContentsMargins(0, 0, 0, 0);
        tableEigenValues = new Q3Table(layoutWidget1);
        tableEigenValues->setObjectName(QString::fromUtf8("tableEigenValues"));
        QSizePolicy sizePolicy3(QSizePolicy::Expanding, QSizePolicy::Preferred);
        sizePolicy3.setHorizontalStretch(0);
        sizePolicy3.setVerticalStretch(0);
        sizePolicy3.setHeightForWidth(tableEigenValues->sizePolicy().hasHeightForWidth());
        tableEigenValues->setSizePolicy(sizePolicy3);
        tableEigenValues->setNumRows(0);
        tableEigenValues->setNumCols(2);
        tableEigenValues->setReadOnly(true);

        gridLayout1->addWidget(tableEigenValues, 1, 0, 1, 2);

        spacer4 = new QSpacerItem(141, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        gridLayout1->addItem(spacer4, 0, 1, 1, 1);

        textLabelEigenvalues = new QLabel(layoutWidget1);
        textLabelEigenvalues->setObjectName(QString::fromUtf8("textLabelEigenvalues"));
        sizePolicy2.setHeightForWidth(textLabelEigenvalues->sizePolicy().hasHeightForWidth());
        textLabelEigenvalues->setSizePolicy(sizePolicy2);
        textLabelEigenvalues->setWordWrap(false);

        gridLayout1->addWidget(textLabelEigenvalues, 0, 0, 1, 1);

        splitter->addWidget(layoutWidget1);

        verticalLayout_2->addWidget(splitter);

        mpTabWidget->addTab(mpJacobianPage, QString());
        mpReducedJacobianPage = new QWidget();
        mpReducedJacobianPage->setObjectName(QString::fromUtf8("mpReducedJacobianPage"));
        verticalLayout_3 = new QVBoxLayout(mpReducedJacobianPage);
        verticalLayout_3->setSpacing(6);
        verticalLayout_3->setContentsMargins(0, 0, 0, 0);
        verticalLayout_3->setObjectName(QString::fromUtf8("verticalLayout_3"));
        splitter_2 = new QSplitter(mpReducedJacobianPage);
        splitter_2->setObjectName(QString::fromUtf8("splitter_2"));
        splitter_2->setOrientation(Qt::Vertical);
        layoutWidget2 = new QWidget(splitter_2);
        layoutWidget2->setObjectName(QString::fromUtf8("layoutWidget2"));
        gridLayout2 = new QGridLayout(layoutWidget2);
        gridLayout2->setSpacing(6);
        gridLayout2->setContentsMargins(11, 11, 11, 11);
        gridLayout2->setObjectName(QString::fromUtf8("gridLayout2"));
        gridLayout2->setContentsMargins(0, 0, 0, 0);
        mpJacobianXAnnotationWidget = new CQArrayAnnotationsWidget(layoutWidget2);
        mpJacobianXAnnotationWidget->setObjectName(QString::fromUtf8("mpJacobianXAnnotationWidget"));
        sizePolicy1.setHeightForWidth(mpJacobianXAnnotationWidget->sizePolicy().hasHeightForWidth());
        mpJacobianXAnnotationWidget->setSizePolicy(sizePolicy1);

        gridLayout2->addWidget(mpJacobianXAnnotationWidget, 1, 0, 1, 2);

        spacer3_2 = new QSpacerItem(518, 21, QSizePolicy::Expanding, QSizePolicy::Minimum);

        gridLayout2->addItem(spacer3_2, 0, 1, 1, 1);

        textLabelJacobianX = new QLabel(layoutWidget2);
        textLabelJacobianX->setObjectName(QString::fromUtf8("textLabelJacobianX"));
        sizePolicy2.setHeightForWidth(textLabelJacobianX->sizePolicy().hasHeightForWidth());
        textLabelJacobianX->setSizePolicy(sizePolicy2);
        textLabelJacobianX->setWordWrap(false);

        gridLayout2->addWidget(textLabelJacobianX, 0, 0, 1, 1);

        splitter_2->addWidget(layoutWidget2);
        layoutWidget3 = new QWidget(splitter_2);
        layoutWidget3->setObjectName(QString::fromUtf8("layoutWidget3"));
        gridLayout3 = new QGridLayout(layoutWidget3);
        gridLayout3->setSpacing(6);
        gridLayout3->setContentsMargins(11, 11, 11, 11);
        gridLayout3->setObjectName(QString::fromUtf8("gridLayout3"));
        gridLayout3->setContentsMargins(0, 0, 0, 0);
        tableEigenValuesX = new Q3Table(layoutWidget3);
        tableEigenValuesX->setObjectName(QString::fromUtf8("tableEigenValuesX"));
        sizePolicy3.setHeightForWidth(tableEigenValuesX->sizePolicy().hasHeightForWidth());
        tableEigenValuesX->setSizePolicy(sizePolicy3);
        tableEigenValuesX->setNumRows(0);
        tableEigenValuesX->setNumCols(2);
        tableEigenValuesX->setReadOnly(true);

        gridLayout3->addWidget(tableEigenValuesX, 1, 0, 1, 2);

        spacer4_2 = new QSpacerItem(141, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        gridLayout3->addItem(spacer4_2, 0, 1, 1, 1);

        textLabelEigenvaluesX = new QLabel(layoutWidget3);
        textLabelEigenvaluesX->setObjectName(QString::fromUtf8("textLabelEigenvaluesX"));
        sizePolicy2.setHeightForWidth(textLabelEigenvaluesX->sizePolicy().hasHeightForWidth());
        textLabelEigenvaluesX->setSizePolicy(sizePolicy2);
        textLabelEigenvaluesX->setWordWrap(false);

        gridLayout3->addWidget(textLabelEigenvaluesX, 0, 0, 1, 1);

        splitter_2->addWidget(layoutWidget3);

        verticalLayout_3->addWidget(splitter_2);

        mpTabWidget->addTab(mpReducedJacobianPage, QString());
        mpStabilityPage = new QWidget();
        mpStabilityPage->setObjectName(QString::fromUtf8("mpStabilityPage"));
        vboxLayout4 = new QVBoxLayout(mpStabilityPage);
        vboxLayout4->setSpacing(6);
        vboxLayout4->setContentsMargins(0, 0, 0, 0);
        vboxLayout4->setObjectName(QString::fromUtf8("vboxLayout4"));
        stabilityTextEdit = new Q3TextEdit(mpStabilityPage);
        stabilityTextEdit->setObjectName(QString::fromUtf8("stabilityTextEdit"));
        stabilityTextEdit->setReadOnly(true);

        vboxLayout4->addWidget(stabilityTextEdit);

        mpTabWidget->addTab(mpStabilityPage, QString());
        mpProtocolPage = new QWidget();
        mpProtocolPage->setObjectName(QString::fromUtf8("mpProtocolPage"));
        gridLayout4 = new QGridLayout(mpProtocolPage);
        gridLayout4->setSpacing(6);
        gridLayout4->setContentsMargins(0, 0, 0, 0);
        gridLayout4->setObjectName(QString::fromUtf8("gridLayout4"));
        protocolTextEdit = new Q3TextEdit(mpProtocolPage);
        protocolTextEdit->setObjectName(QString::fromUtf8("protocolTextEdit"));
        protocolTextEdit->setReadOnly(true);
        protocolTextEdit->setUndoRedoEnabled(false);
        protocolTextEdit->setAutoFormatting(Q3TextEdit::AutoAll);

        gridLayout4->addWidget(protocolTextEdit, 0, 0, 1, 1);

        mpTabWidget->addTab(mpProtocolPage, QString());

        verticalLayout->addWidget(mpTabWidget);


        retranslateUi(StateSubwidget);

        mpTabWidget->setCurrentIndex(1);


        QMetaObject::connectSlotsByName(StateSubwidget);
    } // setupUi

    void retranslateUi(CopasiWidget *StateSubwidget)
    {
        StateSubwidget->setWindowTitle(QApplication::translate("StateSubwidget", "Steady-State Result Window", 0, QApplication::UnicodeUTF8));
        topLabel->setText(QApplication::translate("StateSubwidget", "textLabel1", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpOptimizationPage), QApplication::translate("StateSubwidget", "Optimization Result", 0, QApplication::UnicodeUTF8));
        mpTblMetabolites->horizontalHeader()->setLabel(0, QApplication::translate("StateSubwidget", "Name", 0, QApplication::UnicodeUTF8));
        mpTblMetabolites->horizontalHeader()->setLabel(1, QApplication::translate("StateSubwidget", "Type", 0, QApplication::UnicodeUTF8));
        mpTblMetabolites->horizontalHeader()->setLabel(2, QApplication::translate("StateSubwidget", "Concentration", 0, QApplication::UnicodeUTF8));
        mpTblMetabolites->horizontalHeader()->setLabel(3, QApplication::translate("StateSubwidget", "Particle Number", 0, QApplication::UnicodeUTF8));
        mpTblMetabolites->horizontalHeader()->setLabel(4, QApplication::translate("StateSubwidget", "Rate", 0, QApplication::UnicodeUTF8));
        mpTblMetabolites->horizontalHeader()->setLabel(5, QApplication::translate("StateSubwidget", "Rate", 0, QApplication::UnicodeUTF8));
        mpTblMetabolites->horizontalHeader()->setLabel(6, QApplication::translate("StateSubwidget", "Transition Time", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpMetabolitesPage), QApplication::translate("StateSubwidget", "Species", 0, QApplication::UnicodeUTF8));
        mpTblCompartments->horizontalHeader()->setLabel(0, QApplication::translate("StateSubwidget", "Name", 0, QApplication::UnicodeUTF8));
        mpTblCompartments->horizontalHeader()->setLabel(1, QApplication::translate("StateSubwidget", "Type", 0, QApplication::UnicodeUTF8));
        mpTblCompartments->horizontalHeader()->setLabel(2, QApplication::translate("StateSubwidget", "Volume", 0, QApplication::UnicodeUTF8));
        mpTblCompartments->horizontalHeader()->setLabel(3, QApplication::translate("StateSubwidget", "Rate", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpCompartmentsPage), QApplication::translate("StateSubwidget", "Compartments", 0, QApplication::UnicodeUTF8));
        mpTblModelValues->horizontalHeader()->setLabel(0, QApplication::translate("StateSubwidget", "Name", 0, QApplication::UnicodeUTF8));
        mpTblModelValues->horizontalHeader()->setLabel(1, QApplication::translate("StateSubwidget", "Type", 0, QApplication::UnicodeUTF8));
        mpTblModelValues->horizontalHeader()->setLabel(2, QApplication::translate("StateSubwidget", "Value", 0, QApplication::UnicodeUTF8));
        mpTblModelValues->horizontalHeader()->setLabel(3, QApplication::translate("StateSubwidget", "Rate", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpModelQuantitiesPage), QApplication::translate("StateSubwidget", "Model Quantities", 0, QApplication::UnicodeUTF8));
        mpTblReactions->horizontalHeader()->setLabel(0, QApplication::translate("StateSubwidget", "Name", 0, QApplication::UnicodeUTF8));
        mpTblReactions->horizontalHeader()->setLabel(1, QApplication::translate("StateSubwidget", "Flux", 0, QApplication::UnicodeUTF8));
        mpTblReactions->horizontalHeader()->setLabel(2, QApplication::translate("StateSubwidget", "Particle Flux", 0, QApplication::UnicodeUTF8));
        mpTblReactions->horizontalHeader()->setLabel(3, QApplication::translate("StateSubwidget", "Chemical Equation", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpReactionsPage), QApplication::translate("StateSubwidget", "Reactions", 0, QApplication::UnicodeUTF8));
        textLabelJacobian->setText(QApplication::translate("StateSubwidget", "Jacobian (Complete System)", 0, QApplication::UnicodeUTF8));
        tableEigenValues->horizontalHeader()->setLabel(0, QApplication::translate("StateSubwidget", "Real", 0, QApplication::UnicodeUTF8));
        tableEigenValues->horizontalHeader()->setLabel(1, QApplication::translate("StateSubwidget", "Imaginary", 0, QApplication::UnicodeUTF8));
        textLabelEigenvalues->setText(QApplication::translate("StateSubwidget", "Eigenvalues (Complete System)", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpJacobianPage), QApplication::translate("StateSubwidget", "Jacobian (Complete)", 0, QApplication::UnicodeUTF8));
        textLabelJacobianX->setText(QApplication::translate("StateSubwidget", "Jacobian (Reduced System)", 0, QApplication::UnicodeUTF8));
        tableEigenValuesX->horizontalHeader()->setLabel(0, QApplication::translate("StateSubwidget", "Real", 0, QApplication::UnicodeUTF8));
        tableEigenValuesX->horizontalHeader()->setLabel(1, QApplication::translate("StateSubwidget", "Imaginary", 0, QApplication::UnicodeUTF8));
        textLabelEigenvaluesX->setText(QApplication::translate("StateSubwidget", "Eigenvalues (Reduced System)", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpReducedJacobianPage), QApplication::translate("StateSubwidget", "Jacobian (Reduced)", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpStabilityPage), QApplication::translate("StateSubwidget", "Stability", 0, QApplication::UnicodeUTF8));
        mpTabWidget->setTabText(mpTabWidget->indexOf(mpProtocolPage), QApplication::translate("StateSubwidget", "Protocol", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class StateSubwidget: public Ui_StateSubwidget {};
} // namespace Ui

QT_END_NAMESPACE

#endif // UI_STATESUBWIDGET_H
