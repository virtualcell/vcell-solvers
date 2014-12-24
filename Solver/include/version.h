#ifndef version_h
#define version_h
#include <string>
/**
* return string representing SVN version of code
*/
namespace vcell_util {
	struct Version {
		const std::string svn;
		const std::string compileDate; 
		const std::string compileTime; 
		static const Version & get( );
	private:
		Version( );
	};
}
#endif
