#ifndef persist_h
#define persist_h
#include <ostream>
namespace vcell_persist {
	template <typename E> struct TokenT;

	/**
	* typedef to control token usage; tokens used if and only if parameter type is "bool"
	*/
	typedef TokenT<bool> Token;

	/**
	* insert/read check tokens into persistence streams
	* @tparam E conditionally include / exclude checks
	*/
	template <typename E>
	struct TokenT {
		static void insert(std::ostream &os, const std::string & token) {}
		static void check(std::istream &os, const std::string & token) {}
	};

	template <>
	struct TokenT<bool> {
		template<typename C>
		static void insert(std::ostream &os) {
			insert(os,typeid(C).name( ));
		}
		template<typename C>
		static void check(std::istream &is) {
			check(is,typeid(C).name( ));
		}
		static void insert(std::ostream &os, const std::string & token);
		static void check(std::istream &os, const std::string & token);
		
	};

	/**
	* functor to write binary data
	*/
	template<typename T>
	struct binaryWrite {
		std::ostream &os;
		binaryWrite(std::ostream &os_) 
			:os(os_) {}
		void operator( )(T t) {
			os.write(reinterpret_cast<char *>(&t),sizeof(T) );
		}
	};

	/**
	* functor to read binary data
	*/
	template<typename T>
	struct binaryRead {
		std::istream &is;
		binaryRead(std::istream &is_) 
			:is(is_) {}
		void operator( )(T &in) {
			char * const location = reinterpret_cast<char *>(&in);
			is.read(location,sizeof(T) );
		}
	};
}	
#endif
