#ifndef persist_h
#define persist_h
#include <ostream>
#include <typeinfo>
#include <vector>
namespace vcell_persist {
		void registerTypeToken(const std::type_info &, const char * token); 
		void registerTypeToken(const std::type_info &, const char * token,const std::type_info &templateParameter, int dim); 
		void registerTypeToken(const std::type_info &, const char * token,const std::type_info &templateParameterA, const std::type_info &templateParameterB, int dim); 

	template <class T, int N>
	void tRegisterTypeToken(const std::type_info &ti, const char *token) {
			registerTypeToken(ti,token,typeid(T),N);
	};
	template <class A, class B, int N>
	void tRegisterTypeToken(const std::type_info &ti, const char *token) {
			registerTypeToken(ti,token,typeid(A),typeid(B),N);
	};

	const std::string & getTypeToken(const std::type_info &); 
	//const std::string typeNameFor(const char *classname, const std::type_info &templateParameter, int dim); 

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
	* binary write single value to stream
	* @param os output stream -- must be ios::binary
	*/
	template<typename T>
	void binaryWrite(std::ostream &os, const T & t)  {
		std::streamsize s = sizeof(T);
		const char * const location = reinterpret_cast<const char *>(&t);
		os.write(location,s);
	}
	/**
	* binary read single value form stream
	* @param is input stream -- must be ios::binary
	* @param t destination to set 
	*/
	template<typename T>
	void binaryRead(std::istream &is, T & t)  {
		std::streamsize s = sizeof(T);
		char * const location = reinterpret_cast<char *>(&t);
		is.read(location,s);
	}
	

	/**
	* functor to write binary data
	*/
	template<typename T>
	struct binaryWriter {
		std::ostream &os;
		/**
		* @param os_ output stream -- must be ios::binary
		*/
		binaryWriter(std::ostream &os_) 
			:os(os_) {}
		void operator( )(const T  &t) {
			binaryWrite(os,t);
		}
	};

	/**
	* functor to read binary data
	*/
	template<typename T>
	struct binaryReader {
		std::istream &is;
		/**
		* @param is_ input stream -- must be ios::binary
		*/
		binaryReader(std::istream &is_) 
			:is(is_) {}
		void operator( )(T &in) {
			binaryRead(is,in);
		}
	};

	

	/*
	template<typename T>
	struct persistGen {
		std::istream &is;
		persistGen(std::istream &is_)
			:is(is_) {}
		T operator( )( ) {
			T t(is);
			return t;
		}
	};
	*/
}	
#endif
