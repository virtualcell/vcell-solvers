#ifndef persistvector_h
#define persistvector_h
#include <ostream>
#include <typeinfo>
#include <vector>
#include <persist.h>
namespace vcell_persist {
	/**
	* write vector of T which implements persist
	*/
	template<typename T>
	void persistVector(std::ostream &os,  const std::vector<T> & vec) {
		binaryWrite(os, vec.size( ));
		std::for_each(vec.begin( ), vec.end( ),persistWrite<T>(os));
	}

	template<typename T>
	void readVector(std::istream &is, std::vector<T> & vec) {
		typedef std::vector<T>::size_type Stype;
		Stype size; 
		binaryRead(is,size);

		vec.clear( );
		vec.reserve(size);
		for (Stype i = 0; i < size; ++i) {
			T r(is);
			vec.push_back( r );
		}
	}

	/**
	* std::for_each functor to persist data
	*/
	template<typename T>
	struct persistWrite {
		std::ostream &os;
		persistWrite(std::ostream &os_) 
			:os(os_) {}
		void operator( )(T t) {
			t.persist(os);
		}
	};

}	
#endif
