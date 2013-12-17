#ifndef vlen_h
#define vlen_h
#include <vhdf5/types.h>
#include <vhdf5/vH5cpp.h>
namespace vcellH5 {
	template <class T>
	struct VarLen {
		/**
		* @param dt datatype (okay if the argument is invalidated after construction as the HDF5 implementation is reference counted)
		*/
		VarLen(H5::DataType dt) 
			:dataType(H5::VarLenType(&dt)) {}
		/**
		* use #TPredType for #T to generate default
		*/
		VarLen( )
			:dataType(H5::VarLenType(&TPredType<T>::predType( ) ) ) {} 

		/**
		* @return hvl_t backed by vector passed at constructor; will most likely become invalidated if vector altered 
		*/
		hvl_t adapt(const std::vector<T> & vec) {
			hvl_t  hdf5Data;
			hdf5Data.p = const_cast<T *>(vec.data( ));
			hdf5Data.len = vec.size( );
			return hdf5Data;
		}

		/**
		* @return variable length type based on data type passed into constructor
		*/
		H5::DataType getType( ) const {
			return dataType;
		}
	private:
		const H5::DataType dataType;
	};
}
#endif
