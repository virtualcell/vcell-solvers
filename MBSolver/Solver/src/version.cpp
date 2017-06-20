#include <version.h>
#if !defined(SVNVERSION)
#error SVNVERSION version not defined
#endif

#define VCELLSVNQ(x) #x
#define VCELLSVNQUOTE(x) VCELLSVNQ(x)
using vcell_util::Version;
namespace {
	std::string svnRev( ) {
		std::string rval(VCELLSVNQUOTE(SVNVERSION));
		//macro adds quotes to either end, so strip those off
		std::string rvo = rval.substr(1,rval.length( ) - 2);
		return rvo;
	}
}


Version::Version( ) 
	:svn(svnRev( )),
	compileDate(__DATE__),
	compileTime(__TIME__) {}

const Version & Version::get( )  {
	static Version v;
	return v;
}
