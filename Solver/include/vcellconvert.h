#ifndef vcellconvert_h
#define vcellconvert_h
#include <boost/lexical_cast.hpp>
#include <string>
#include <cassert>
namespace vcell_util {

	template <typename TARGET>
	struct VCellConvert {
		TARGET operator( )(const char *v) {
			return boost::lexical_cast<TARGET>(v);
		}
	};

	/*
	* specialize template; boost does not handle std::string conversion
	*/
	template <>
	struct VCellConvert<std::string> {
		std::string operator( )(const char *v) {
			return std::string(v);
		}
	};
	template <>
	struct VCellConvert<const char *> {
		const char * operator( )(const char *v) {
			return v;
		} 
	};
	template <>
	struct VCellConvert<char *> {
		char * operator( )(const char *v) {
			assert(false);
			//do not use this specialization; use <const char *> instead
			return nullptr;
		}  
	};
	template <>
	struct VCellConvert<bool> {
		bool operator( )(const char *v) {
			return strcmp(v,"false")!=0;
		}
	};

	/**
	* convenience method to convert const char * to specified type
	* @tparam T type to convert to
	* @throws boost std::exception subclass if string not parseable / convertible
	*/
	template <class TARGET>
	TARGET convertType(const char *v) {
		return VCellConvert<TARGET>( )(v); 
	}

	/**
	* intentional convert types with possible loss of data / truncation
	* i.e. static_cast
	* @tparam A one type 
	* @tparam B other type 
	*/
	template <typename A, typename B>
	struct LossyConvert {
		B operator( )(A s) {
			return static_cast<B>(s);
		}
		A operator( )(B s) {
			return static_cast<A>(s);
		}
	};

}
#endif
