#include <iostream>
#include <cstring>
#include <map>
#include <Logger.h>
#include <VCellException.h>
#include <stdexcept>
using vcell_util::Logger;
namespace {
	class DefaultDest: public Logger::Destination {
		void report(const char * msg, bool newline) {
			std::cout << msg;
			if (newline) {
				std::cout << std::endl;
			}
		}
	} defDest;
}

Logger Logger::instance; 

struct Logger::KeyStore {
	std::map<std::string,Logger::Level> keys;
};

#pragma warning ( disable: 4355 )
Logger::Logger( )
	:logStream(*this),
	dest(&defDest),
	level(fatal),
	keyStore(new KeyStore)
{ }

Logger::~Logger( ) {
	delete keyStore;
}

//static 
/**
* convert string to level
* @param in lower case string
* @return corresponding level
* @throws std::domain_error if no match for string
*/
Logger::Level Logger::read(const char *in) {
#	define MATCH(x) if (strcmp(in,#x) ==0) return x; 
	MATCH(verbose);
	MATCH(trace);
	MATCH(debug);
	MATCH(info);
	MATCH(warn);
	MATCH(fatal);
# undef MATCH
	VCELL_EXCEPTION(domain_error,"no match for logging level '" << in << "'");
}
void Logger::set(Level lvl, const char * key) {
	Level current = keyStore->keys[key];
	if (lvl > current) {
		keyStore->keys[key] = lvl;
	}
}

bool Logger::inStore(Level lvl, const char *key) const {
	std::map<std::string,Logger::Level>::const_iterator iter = keyStore->keys.find(key);
	if (iter == keyStore->keys.end( )) {
		return false;
	}
	return lvl >= iter->second;
}
namespace vcell_util {
	std::ostream &operator<<(std::ostream & os, const Logger::Level & lvl) {
#	define CASE(x) case Logger::x: os << #x; break;
		switch (lvl) {
			CASE(verbose);
			CASE(trace);
			CASE(debug);
			CASE(info);
			CASE(warn);
			CASE(fatal);
		}
#	undef CASE
		return os;
	}
}


/*
std::ostream & EndLog(std::ostream &in) {
Logger::LogStream & ls = static_cast<Logger::LogStream &>(in);
ls.owner.dest->report(ls.str( ).c_str( ),true);
ls.str("");
ls.clear( );
return in;
}
*/
