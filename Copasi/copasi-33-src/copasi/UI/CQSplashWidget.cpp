#include "CQSplashWidget.h"

#include <qvariant.h>
#include "CQSplashWidget.ui.h"
/*
 *  Constructs a CQSplashWidget which is a child of 'parent', with the
 *  name 'name'.' 
 */
CQSplashWidget::CQSplashWidget(QWidget* parent, const char* name)
    : CopasiWidget(parent, name)
{
    setupUi(this);

    init();
}

/*
 *  Destroys the object and frees any allocated resources
 */
CQSplashWidget::~CQSplashWidget()
{
    // no need to delete child widgets, Qt does it all for us
}

/*
 *  Sets the strings of the subwidgets using the current
 *  language.
 */
void CQSplashWidget::languageChange()
{
    retranslateUi(this);
}

