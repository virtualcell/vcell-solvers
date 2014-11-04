#ifndef persistcontainer
#define persistcontainer
#include <ostream>
#include <typeinfo>
#include <vector>
#include <type_traits>
#include <persist.h>
/**
* set of functions for persisting from containers. Because addition of elements to containers
* varies, easier to write custom methods for each type
*/
namespace vcell_persist {
	/**
	* default "W" writer which using "persist" for objects of type Persistent
	* and binaryWrite for objects of other types
	*/
	struct persistWrite {
		/**
		* operator for classes which derived from Persistent -- calls U.persist( .. )
		*/
		template <typename U>
		typename std::enable_if<std::is_base_of<Persistent,U>::value,void>::type 
		operator( )(std::ostream &os, const U & u) {
			u.persist(os);
		}

		/**
		* operator for classes which do not derived from Persistent -- calls binaryWrite 
		*/
		template <typename U>
		typename std::enable_if< ! std::is_base_of<Persistent,U>::value,void>::type 
		operator( )(std::ostream &os, const U & u) {
			binaryWrite(os,u);
		}
	};

	template <typename W = persistWrite>
	struct writeFunctor {
		std::ostream &os;
		W & w;
		writeFunctor(std::ostream &os_, W & w_ = W( ) ) 
			:os(os_),
			w(w_) {}

		template <typename U>
		void operator( )(const U & u) {
			w(os,u);
		}

	};

	/**
	* generator which returns new object from stream; supports
	* binaryReads and classes with constructor of form U(std::istream &is)
	*/
	struct persistRead {
		/**
		* operator for classes which derived from Persistent -- calls U(istream &)
		*/
		template <typename U>
		typename std::enable_if<std::is_base_of<Persistent,U>::value,U>::type 
		generate(std::istream &is) {
			return U(is);
		}

		/**
		* operator for classes which do not derived from Persistent -- calls binaryRead
		*/
		template <typename U>
		typename std::enable_if< ! std::is_base_of<Persistent,U>::value,U>::type 
		generate(std::istream &is) {
			U u;
			binaryRead(is,u);
			return u;
		}
	};

	/**
	* for each compatible function object
	* @tparam R generator; must implement U generate(std::istream &is) 
	*/
	template <typename R = persistRead>
	struct readFunctor {
		std::istream &is;
		R & r;
		readFunctor(std::istream &is_, R & r_ = R( )) 
			:is(is_),
			r(r_) {}

		/**
		* operator for classes which derived from Persistent -- calls U(istream &)
		*/
		template <typename U>
		void operator( )(U &u) {
			u = r.generate<U>(is);
		}
	};

	/**
	* vector insertion function object
	* @tparam R generator; must implement U generate(std::istream &is) 
	*/
	template <typename R = persistRead>
	struct InsertFrom {
		R & r;
		InsertFrom(R & r_ = R( ))
			:r(r_) {}

		template <typename U>
		void operator ( )(std::istream &is, std::vector<U> &vec)  {
			vec.push_back( r.generate<U>(is) );
		}
	};

	//--------------------------------
	// Vector
	//--------------------------------
	/**
	* write vector of T (Persistent or binary) 
	* @ tparam T type to restore
	* @ tparam W type which implements void operator(const T &t) to
	* write t to os
	*/
	template<typename T, typename W>
	void save(std::ostream &os,  const std::vector<T> & vec, W & w) {
		binaryWrite(os, vec.size( ));
		std::for_each(vec.begin( ), vec.end( ),w);
	}

	/**
	* write vector of T (Persistent or binary) using writeFunctor<persistWrite>
	* @ tparam T type to restore
	*/
	template<typename T>
	void save(std::ostream &os,  const std::vector<T> & vec) {
		save(os,vec,writeFunctor<>(os) );
	}

	/**
	* restore vector of T (Persistent or binary) 
	* @ tparam T type to restore
	* @ tparam I inserter which implements I(std::istream &is, std::vector<U> &vec)  to insert
	* new object from stream into vector (e.g. InsertFrom)
	*/
	template<typename T, class I>
	void restore(std::istream &is, std::vector<T> & vec, I & inserter) {
		typedef typename std::vector<T>::size_type Stype;
		Stype size; 
		binaryRead(is,size);

		vec.clear( );
		vec.reserve(size);
		for (Stype i = 0; i < size; ++i) {
			inserter(is,vec);
		}
	}

	/**
	* restore vector of T (Persistent or binary) using 
	* InsertFrom 
	* @ tparam T type to restore
	*/
	template<typename T>
	void restore(std::istream &is, std::vector<T> & vec) {
		restore(is,vec,InsertFrom<persistRead>( ));
	}
	//--------------------------------
	// array 
	//--------------------------------
	/**
	* write array of T (Persistent or binary) 
	* @ tparam T array type
	* @ tparam n array size 
	* @ tparam W type which implements void operator(const T &t) to
    *	write t to stream
	*/
	template<typename T, size_t N, typename W>
	void save(std::ostream &os,  const std::array<T,N> & arr, W & w) {
		std::for_each(arr.begin( ), arr.end( ),w);
	}

	template<typename T, size_t N>
	void save(std::ostream &os,  const std::array<T,N> & arr) {
		save(os,arr,writeFunctor<>(os));
	}

	/**
	* restore array of T (Persistent or binary) 
	* @ tparam T array type
	* @ tparam n array size 
	* @ tparam R type which implements void operator(T &t) to
    * set t from stream	
	*/
	template<typename T, size_t N, typename R>
	void restore(std::istream &is, std::array<T,N> & arr, R &r) {
		std::for_each(arr.begin( ), arr.end( ),r);
	}

	/**
	* restore array of T (Persistent or binary) using readFunctor<>
	* @ tparam T array type
	* @ tparam n array size 
	*/
	template<typename T, size_t N>
	void restore(std::istream &is, std::array<T,N> & arr) {
		restore(is,arr,readFunctor<>(is));
	}
}	
#endif
