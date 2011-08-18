#ifndef CQTASKBTNWIDGET_H
#define CQTASKBTNWIDGET_H

#include <qvariant.h>


#include <Qt3Support/Q3MimeSourceFactory>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QPushButton>
#include <QtGui/QSpacerItem>
#include <QtGui/QVBoxLayout>
#include <QtGui/QWidget>

QT_BEGIN_NAMESPACE

class Ui_CQTaskBtnWidget
{
public:
    QVBoxLayout *vboxLayout;
    QSpacerItem *mpSpacerVertical;
    QHBoxLayout *hboxLayout;
    QPushButton *mpBtnRun;
    QPushButton *mpBtnRevert;
    QSpacerItem *mpSpacerHorizontal;
    QPushButton *mpBtnReport;
    QPushButton *mpBtnAssistant;

    void setupUi(QWidget *CQTaskBtnWidget)
    {
        if (CQTaskBtnWidget->objectName().isEmpty())
            CQTaskBtnWidget->setObjectName(QString::fromUtf8("CQTaskBtnWidget"));
        CQTaskBtnWidget->resize(356, 35);
        vboxLayout = new QVBoxLayout(CQTaskBtnWidget);
        vboxLayout->setSpacing(6);
        vboxLayout->setContentsMargins(0, 0, 0, 0);
        vboxLayout->setObjectName(QString::fromUtf8("vboxLayout"));
        mpSpacerVertical = new QSpacerItem(20, 16, QSizePolicy::Minimum, QSizePolicy::Preferred);

        vboxLayout->addItem(mpSpacerVertical);

        hboxLayout = new QHBoxLayout();
        hboxLayout->setSpacing(6);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        mpBtnRun = new QPushButton(CQTaskBtnWidget);
        mpBtnRun->setObjectName(QString::fromUtf8("mpBtnRun"));

        hboxLayout->addWidget(mpBtnRun);

        mpBtnRevert = new QPushButton(CQTaskBtnWidget);
        mpBtnRevert->setObjectName(QString::fromUtf8("mpBtnRevert"));

        hboxLayout->addWidget(mpBtnRevert);

        mpSpacerHorizontal = new QSpacerItem(20, 20, QSizePolicy::Expanding, QSizePolicy::Minimum);

        hboxLayout->addItem(mpSpacerHorizontal);

        mpBtnReport = new QPushButton(CQTaskBtnWidget);
        mpBtnReport->setObjectName(QString::fromUtf8("mpBtnReport"));

        hboxLayout->addWidget(mpBtnReport);

        mpBtnAssistant = new QPushButton(CQTaskBtnWidget);
        mpBtnAssistant->setObjectName(QString::fromUtf8("mpBtnAssistant"));

        hboxLayout->addWidget(mpBtnAssistant);


        vboxLayout->addLayout(hboxLayout);

        QWidget::setTabOrder(mpBtnRun, mpBtnRevert);
        QWidget::setTabOrder(mpBtnRevert, mpBtnReport);
        QWidget::setTabOrder(mpBtnReport, mpBtnAssistant);

        retranslateUi(CQTaskBtnWidget);

        QMetaObject::connectSlotsByName(CQTaskBtnWidget);
    } // setupUi

    void retranslateUi(QWidget *CQTaskBtnWidget)
    {
        CQTaskBtnWidget->setWindowTitle(QApplication::translate("CQTaskBtnWidget", "CQTaskBtnWidget", 0, QApplication::UnicodeUTF8));
        mpBtnRun->setText(QApplication::translate("CQTaskBtnWidget", "Run", 0, QApplication::UnicodeUTF8));
        mpBtnRevert->setText(QApplication::translate("CQTaskBtnWidget", "Revert", 0, QApplication::UnicodeUTF8));
        mpBtnReport->setText(QApplication::translate("CQTaskBtnWidget", "Report", 0, QApplication::UnicodeUTF8));
        mpBtnAssistant->setText(QApplication::translate("CQTaskBtnWidget", "Output Assistant", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQTaskBtnWidget: public Ui_CQTaskBtnWidget {};
} // namespace Ui

QT_END_NAMESPACE

class CQTaskBtnWidget : public QWidget, public Ui::CQTaskBtnWidget
{
    Q_OBJECT

public:
    CQTaskBtnWidget(QWidget* parent = 0, const char* name = 0, Qt::WindowFlags fl = 0);
    ~CQTaskBtnWidget();

protected slots:
    virtual void languageChange();

};

#endif // CQTASKBTNWIDGET_H
