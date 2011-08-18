#include "CUpDownSubwidget.h"

#include <qvariant.h>
#include "CUpDownSubwidget.ui.h"
/*
 *  Constructs a CUpDownSubwidget as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 */
CUpDownSubwidget::CUpDownSubwidget(QWidget* parent, const char* name, Qt::WindowFlags fl)
    : QWidget(parent, name, fl)
{
    setupUi(this);

    init();
}

/*
 *  Destroys the object and frees any allocated resources
 */
CUpDownSubwidget::~CUpDownSubwidget()
{
    // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void CUpDownSubwidget::languageChange()
{
    retranslateUi(this);
}

