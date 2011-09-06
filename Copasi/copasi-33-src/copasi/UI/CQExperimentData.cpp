#include "CQExperimentData.h"

#include <qvariant.h>
#include "CQExperimentData.ui.h"
/*
 *  Constructs a CQExperimentData as a child of 'parent', with the
 *  name 'name' and widget flags set to 'f'.
 *
 *  The dialog will by default be modeless, unless you set 'modal' to
 *  true to construct a modal dialog.
 */
CQExperimentData::CQExperimentData(QWidget* parent, const char* name, bool modal, Qt::WindowFlags fl)
    : QDialog(parent, name, modal, fl)
{
    setupUi(this);

    init();
}

/*
 *  Destroys the object and frees any allocated resources
 */
CQExperimentData::~CQExperimentData()
{
    destroy();
    // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void CQExperimentData::languageChange()
{
    retranslateUi(this);
}
