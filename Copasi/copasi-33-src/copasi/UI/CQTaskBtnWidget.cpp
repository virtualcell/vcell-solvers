#include "CQTaskBtnWidget.h"

#include <qvariant.h>
/*
 *  Constructs a CQTaskBtnWidget as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 */
CQTaskBtnWidget::CQTaskBtnWidget(QWidget* parent, const char* name, Qt::WindowFlags fl)
    : QWidget(parent, name, fl)
{
    setupUi(this);

}

/*
 *  Destroys the object and frees any allocated resources
 */
CQTaskBtnWidget::~CQTaskBtnWidget()
{
    // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void CQTaskBtnWidget::languageChange()
{
    retranslateUi(this);
}

