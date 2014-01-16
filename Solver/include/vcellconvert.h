#ifndef vcellconvert_h
#define vcellconvert_h
#include <boost/lexical_cast.hpp>
namespace vcell_util {

	template <typename TARGET>
	struct CharPointerConvert {
		TARGET operator( )(const char *v) {
			return boost::lexical_cast<TARGET>(v);
		}
	};

	/*
	* specialize template; boost does not handle std::string conversion
	*/
	template <>
	struct CharPointerConvert<std::string> {
		std::string operator( )(const char *v) {
			return std::string(v);
		}
	};
	template <>
	struct CharPointerConvert<const char *> {
		const char * operator( )(const char *v) {
			return v;
		} 
	};
	template <>
	struct CharPointerConvert<char *> {
		char * operator( )(const char *v) {
		}  //do not use this specialization; use <const char *> instead
	};

	/**
	* convenience method to convert const char * to specified type
	* @tparam T type to convert to
	* @throws boost std::exception subclass if string not parseable / convertible
	*/
	template <class TARGET>
	TARGET convertType(const char *v) {
		return CharPointerConvert<TARGET>( )(v); 
	}

}
#endif
