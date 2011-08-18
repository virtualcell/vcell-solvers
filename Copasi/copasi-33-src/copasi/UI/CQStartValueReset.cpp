#include "CQStartValueReset.h"

#include <qvariant.h>
#include "CQStartValueReset.ui.h"
/*
 *  Constructs a CQStartValueReset as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 *
 *  The dialog will by default be modeless, unless you set 'modal' to
 *  true to construct a modal dialog.
 */
CQStartValueReset::CQStartValueReset(QWidget* parent, const char* name, bool modal, Qt::WindowFlags fl)
    : QDialog(parent, name, modal, fl)
{
    setupUi(this);

}

/*
 *  Destroys the object and frees any allocated resources
 */
CQStartValueReset::~CQStartValueReset()
{
    // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void CQStartValueReset::languageChange()
{
    retranslateUi(this);
}

