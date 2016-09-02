#ifndef VolumeVariable_h
#define VolumeVariable_h
#include <string>
#include <Variable.h>
#include <SExpression.h>
using std::string;

namespace moving_boundary
{
  namespace biology
	{
  	struct Physiology;

		struct VolumeVariable : public Variable {

			VolumeVariable(const string & name);
			~VolumeVariable();
		};
	}
}

#endif
