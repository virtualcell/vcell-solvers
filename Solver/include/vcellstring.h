#ifndef vcellstring_h
#define vcellstring_h
#include <string>
/**
* vcell string utilities
*/
namespace vcell_util {
	std::string convertNonPrintable(const std::string &source);
	inline bool endsWith(std::string const & value, std::string const & ending);
}
#endif
