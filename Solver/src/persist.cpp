#include <string>
#include <iostream>
#include <persist.h>
#include <VCellException.h>
#include <ManagedArrayPtr.h>
#include <vcellstring.h>
using namespace vcell_persist;
using namespace vcell_util;

void TokenT<bool>::insert(std::ostream &os, const std::string & token) {
	os << token;
}

void TokenT<bool>::check(std::istream &is, const std::string & token) {
	const size_t ts = token.size( );
	StackPtr<char,100> b(ts);
	char * buffer = b.get( );
	is.read(buffer,ts);
	const bool match = strncmp(buffer,token.c_str( ),ts ) == 0;
	if (!match) {
		std::string tokenRead = convertNonPrintable(std::string(buffer,ts) );
		std::string expected = convertNonPrintable(token);
		VCELL_EXCEPTION(invalid_argument,"Read token " << tokenRead << ", " << expected << " expected");
	}
}
