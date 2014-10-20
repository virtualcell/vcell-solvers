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
	* std::for_each functor to persist data
	*/
	template<typename T>
	struct persistWrite {
		std::ostream &os;
		persistWrite(std::ostream &os_) 
			:os(os_) {}

		/**
		* operator for classes which derived from Persistent -- calls U.persist( .. )
		*/
		template <typename U>
		typename std::enable_if<std::is_base_of<Persistent,U>::value,void>::type 
		operator( )(U u) {
			u.persist(os);
		}

		/**
		* operator for classes which do not derived from Persistent -- calls binaryWrite 
		*/
		template <typename U>
		typename std::enable_if< ! std::is_base_of<Persistent,U>::value,void>::type 
		operator( )(U u) {
			binaryWrite(os,u);
		}
	};

	/**
	* operator for classes which derived from Persistent -- calls U(std::istream &)
	*/
	template <typename U>
	typename std::enable_if<std::is_base_of<Persistent,U>::value,void>::type 
	insertFrom(std::istream &is, std::vector<U> &vec)  {
		U u(is);
		vec.push_back(u);
	}

	/**
	* operator for classes which don't derived from Persistent -- calls binaryRead 
	*/
	template <typename U>
	typename std::enable_if< ! std::is_base_of<Persistent,U>::value,void>::type 
	insertFrom(std::istream &is, std::vector<U> &vec)  {
		U u;
		binaryRead(is,u);
		vec.push_back(u);
	}

	/**
	* write vector of T which implements persist
	*/
	template<typename T>
	void persist(std::ostream &os,  const std::vector<T> & vec) {
		binaryWrite(os, vec.size( ));
		std::for_each(vec.begin( ), vec.end( ),persistWrite<T>(os));
	}

	template<typename T>
	void restore(std::istream &is, std::vector<T> & vec) {
		typedef typename std::vector<T>::size_type Stype;
		Stype size; 
		binaryRead(is,size);

		vec.clear( );
		vec.reserve(size);
		for (Stype i = 0; i < size; ++i) {
			insertFrom(is,vec);
		}
	}
}	
#endif
