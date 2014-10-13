#ifndef persist_h
#define persist_h
#include <ostream>
#include <typeinfo>
namespace vcell_persist {
	void registerTypeToken(const std::type_info &, const char * token); 
	const std::string & getTypeToken(const std::type_info &); 

	#define VCELL_PERSIST_REGISTER_MACRO(X) registerTypeToken(typeid(X),#X);
	#define VCELL_PERSIST_REGISTER_MACRO2(X,Y) registerTypeToken(typeid(X,Y),#X","#Y);

	template <typename E> struct TokenT;

	/**
	* typedef to control token usage; tokens used if and only if parameter type is "bool"
	*/
	typedef TokenT<bool> Token;		//tokens on
	//typedef TokenT<int> Token;			//tokens off

	/**
	* insert/read check tokens into persistence streams
	* @tparam E conditionally include / exclude checks
	*/
	template <typename E>
	struct TokenT {
		template<typename C>
		static void insert(std::ostream &os) { }
		template<typename C>
		static void check(std::istream &is) { }
		static void insert(std::ostream &os, const std::string & token) {}
		static void check(std::istream &os, const std::string & token) {}
	};

	template <>
	struct TokenT<bool> {
		template<typename C>
		static void insert(std::ostream &os) {
			insert(os,typeid(C));
		}
		template<typename C>
		static void check(std::istream &is) {
			check(is,typeid(C));
		}
		static void insert(std::ostream &os, const std::string & token);
		static void check(std::istream &os, const std::string & token);
		static void insert(std::ostream &os,const std::type_info &);
		static void check(std::istream &os, const std::type_info &);
		
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
