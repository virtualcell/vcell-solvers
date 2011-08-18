#include "objectdebug.h"

#include <qvariant.h>
#include "objectdebug.ui.h"
/*
 *  Constructs a ObjectDebug as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 *
 *  The dialog will by default be modeless, unless you set 'modal' to
 *  true to construct a modal dialog.
 */
ObjectDebug::ObjectDebug(QWidget* parent, const char* name, bool modal, Qt::WindowFlags fl)
    : QDialog(parent, name, modal, fl)
{
    setupUi(this);

    init();
}

/*
 *  Destroys the object and frees any allocated resources
 */
ObjectDebug::~ObjectDebug()
{
    // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void ObjectDebug::languageChange()
{
    retranslateUi(this);
}

