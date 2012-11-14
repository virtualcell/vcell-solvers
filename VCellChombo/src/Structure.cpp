#include <VCELL/Structure.h>

Structure::Structure(string& Aname) {
	name = Aname;
	odeVarCount = 0;
	pdeVarCount = 0;

	for (int i = 0; i < 6; i ++) {
		boundaryType[i] = BOUNDARY_VALUE;
	}
}

Structure::~Structure(void)
{
}

