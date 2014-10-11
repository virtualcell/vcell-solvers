#include <sstream>
#include <vcellstring.h>
using std::string;
std::string vcell_util::convertNonPrintable(const std::string &source) {
	string rval;
	rval.reserve(source.size( ) * 4); //near worse case each character replaced with \0x--
	for (string::const_iterator iter = source.begin( ); iter != source.end( ); ++iter) {
		char c = *iter;
		if (isprint(c)) {
			rval.push_back(c);
		}
		else {
			std::ostringstream oss;
			oss << '<' << std::hex << static_cast<unsigned int>(c) << '>'; 
			rval.append(oss.str( ));
		}
	}
	return rval;
}