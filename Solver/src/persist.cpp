#include <string>
#include <cstring>
#include <iostream>
#include <stdexcept>
#include <typeinfo>
#include <map>
#include <persist.h>
#include <VCellException.h>
#include <ManagedArrayPtr.h>
#include <vcellstring.h>
using std::type_info;
using namespace vcell_persist;
using namespace vcell_util;
namespace {
	struct TokenMap : public std::map<const type_info *,std::string> {
		TokenMap( ) {
			registerTypeToken(typeid(char),"char");
			registerTypeToken(typeid(short),"short");
			registerTypeToken(typeid(int),"int");
			registerTypeToken(typeid(long),"long");
			registerTypeToken(typeid(float),"float");
			registerTypeToken(typeid(double),"double");
		}
	};

	TokenMap typeTokens;

	/*
	inline const std::string &lookup(const type_info & ti) {
		TokenMap::iterator iter = typeTokens.find(&ti);
		if (iter != typeTokens.end( )) {
			return iter->second;
		}
		VCELL_EXCEPTION(invalid_argument,"type " << ti.name( ) << " not registered with call to registerTypeToken");
	}
	*/

}

void TokenT<bool>::insert(std::ostream &os, const type_info & ti) {
	insert(os,getTypeToken(ti));
}

void TokenT<bool>::insert(std::ostream &os, const std::string & token) {
	os << token;
}

void TokenT<bool>::check(std::istream &is, const type_info & ti) {
	check(is,getTypeToken(ti));
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

const std::string & vcell_persist::getTypeToken(const type_info & ti) {
		TokenMap::iterator iter = typeTokens.find(&ti);
		if (iter != typeTokens.end( )) {
			return iter->second;
		}
		VCELL_EXCEPTION(invalid_argument,"type " << ti.name( ) << " not registered with call to registerTypeToken");
}

void vcell_persist::registerTypeToken(const type_info &clzz, const char *classname, const std::type_info &templateParameter, int dim) {
	std::ostringstream oss;
	oss << classname << '<' << getTypeToken(templateParameter) << ',' << dim << '>';
	typeTokens[&clzz] = oss.str( ); 
}

void vcell_persist::registerTypeToken(const type_info &clzz, const char *classname, const std::type_info &templateParameterA, const std::type_info &templateParameterB, int dim) {
	std::ostringstream oss;
	oss << classname << '<' << getTypeToken(templateParameterA) << ',' << getTypeToken(templateParameterB) << ',' << dim << '>';
	typeTokens[&clzz] = oss.str( ); 
}

