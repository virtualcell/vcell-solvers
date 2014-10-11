#include <string>
#include <cstring>
#include <iostream>
#include <stdexcept>
#include <typeinfo>
#include <map>
#include <Persistent.h>
#include <persist.h>
#include <VCellException.h>
#include <ManagedArrayPtr.h>
#include <vcellstring.h>
using namespace vcell_persist;
using namespace vcell_util;
namespace {
	typedef std::map<const type_info *,std::string> TokenMap; 
	TokenMap typeTokens;
	inline const std::string &lookup(const type_info & ti) {
		TokenMap::iterator iter = typeTokens.find(&ti);
		if (iter != typeTokens.end( )) {
			return iter->second;
		}
		VCELL_EXCEPTION(invalid_argument,"type " << ti.name( ) << " not registered with call to registerTypeToken");
	}
}

void TokenT<bool>::insert(std::ostream &os, const type_info & ti) {
	insert(os,lookup(ti));
}

void TokenT<bool>::insert(std::ostream &os, const std::string & token) {
	os << token;
}

void TokenT<bool>::check(std::istream &is, const type_info & ti) {
	check(is,lookup(ti));
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

void vcell_persist::registerTypeToken(const type_info & ti, const char * token) {
	typeTokens[&ti] = std::string(token);
}

