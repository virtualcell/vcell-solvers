#ifndef CQTASKHEADERWIDGET_H
#define CQTASKHEADERWIDGET_H

#include <qvariant.h>

class CCopasiTask;

#include <Qt3Support/Q3MimeSourceFactory>
#include <QtCore/QVariant>
#include <QtGui/QAction>
#include <QtGui/QApplication>
#include <QtGui/QButtonGroup>
#include <QtGui/QCheckBox>
#include <QtGui/QHBoxLayout>
#include <QtGui/QHeaderView>
#include <QtGui/QLabel>
#include <QtGui/QSpacerItem>
#include <QtGui/QWidget>
#include <string>

QT_BEGIN_NAMESPACE

class Ui_CQTaskHeaderWidget
{
public:
    QHBoxLayout *hboxLayout;
    QLabel *mpLblName;
    QCheckBox *mpUpdateModel;
    QSpacerItem *mpSpacer;
    QCheckBox *mpBoxExecutable;

    void setupUi(QWidget *CQTaskHeaderWidget)
    {
        if (CQTaskHeaderWidget->objectName().isEmpty())
            CQTaskHeaderWidget->setObjectName(QString::fromUtf8("CQTaskHeaderWidget"));
        CQTaskHeaderWidget->resize(230, 26);
        QSizePolicy sizePolicy(static_cast<QSizePolicy::Policy>(5), static_cast<QSizePolicy::Policy>(0));
        sizePolicy.setHorizontalStretch(0);
        sizePolicy.setVerticalStretch(0);
        sizePolicy.setHeightForWidth(CQTaskHeaderWidget->sizePolicy().hasHeightForWidth());
        CQTaskHeaderWidget->setSizePolicy(sizePolicy);
        hboxLayout = new QHBoxLayout(CQTaskHeaderWidget);
        hboxLayout->setSpacing(6);
        hboxLayout->setContentsMargins(0, 0, 0, 0);
        hboxLayout->setObjectName(QString::fromUtf8("hboxLayout"));
        mpLblName = new QLabel(CQTaskHeaderWidget);
        mpLblName->setObjectName(QString::fromUtf8("mpLblName"));
        QSizePolicy sizePolicy1(static_cast<QSizePolicy::Policy>(3), static_cast<QSizePolicy::Policy>(5));
        sizePolicy1.setHorizontalStretch(0);
        sizePolicy1.setVerticalStretch(0);
        sizePolicy1.setHeightForWidth(mpLblName->sizePolicy().hasHeightForWidth());
        mpLblName->setSizePolicy(sizePolicy1);
        mpLblName->setWordWrap(false);

        hboxLayout->addWidget(mpLblName);

        mpUpdateModel = new QCheckBox(CQTaskHeaderWidget);
        mpUpdateModel->setObjectName(QString::fromUtf8("mpUpdateModel"));

        hboxLayout->addWidget(mpUpdateModel);

        mpSpacer = new QSpacerItem(20, 20, QSizePolicy::Maximum, QSizePolicy::Minimum);

        hboxLayout->addItem(mpSpacer);

        mpBoxExecutable = new QCheckBox(CQTaskHeaderWidget);
        mpBoxExecutable->setObjectName(QString::fromUtf8("mpBoxExecutable"));

        hboxLayout->addWidget(mpBoxExecutable);


        retranslateUi(CQTaskHeaderWidget);
        QObject::connect(mpBoxExecutable, SIGNAL(toggled(bool)), CQTaskHeaderWidget, SLOT(slotExecutable()));
        QObject::connect(mpUpdateModel, SIGNAL(toggled(bool)), CQTaskHeaderWidget, SLOT(slotUpdate()));

        QMetaObject::connectSlotsByName(CQTaskHeaderWidget);
    } // setupUi

    void retranslateUi(QWidget *CQTaskHeaderWidget)
    {
        CQTaskHeaderWidget->setWindowTitle(QApplication::translate("CQTaskHeaderWidget", "CQTaskHeaderWidget", 0, QApplication::UnicodeUTF8));
        mpLblName->setText(QApplication::translate("CQTaskHeaderWidget", "<h2>Task</h2>", 0, QApplication::UnicodeUTF8));
        mpUpdateModel->setText(QApplication::translate("CQTaskHeaderWidget", "update model", 0, QApplication::UnicodeUTF8));
        mpBoxExecutable->setText(QApplication::translate("CQTaskHeaderWidget", "executable", 0, QApplication::UnicodeUTF8));
    } // retranslateUi

};

namespace Ui {
    class CQTaskHeaderWidget: public Ui_CQTaskHeaderWidget {};
} // namespace Ui

QT_END_NAMESPACE

class CQTaskHeaderWidget : public QWidget, public Ui::CQTaskHeaderWidget
{
    Q_OBJECT

public:
    CQTaskHeaderWidget(QWidget* parent = 0, const char* name = 0, Qt::WindowFlags fl = 0);
    ~CQTaskHeaderWidget();

    bool setTaskName(const std::string & name);
    void saved();

protected:
    bool mExecutableChanged;
    bool mUpdateChanged;
    QColor mSavedColor;
    QColor mChangedColor;

protected slots:
    virtual void languageChange();

    void slotUpdate();
    void slotExecutable();


private:
    void init();

};

#endif // CQTASKHEADERWIDGET_H
