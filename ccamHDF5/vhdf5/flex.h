#ifndef flex_h
#define flex_h
#include <cassert>
#include <vhdf5/vH5cpp.h>
namespace vcellH5 {


	template <class T>
	struct Flex2Col;

	/**
	* provide flexible interpretation of static buffer for writing
	* different size blocks to HDF5
	*/
#pragma warning ( disable : 4351 )
	template <class T>
	struct Flex2 {
		Flex2(size_t columnSize, size_t rowSize)
			:storage(columnSize* rowSize ),
			colSize_(columnSize),
			rowSize_(rowSize) {}

		/**
		* allow construction with same array used to create DataSpace
		* array must have size >= 2
		*/
		Flex2(hsize_t *dims)
			:storage(dims[0] * dims[1]),
			colSize_(dims[0]),
			rowSize_(dims[1]) {}
		/**
		* default ctor -- must call reindex before use
		*/
		Flex2( )
			:storage(0),
			colSize_(0),
			rowSize_(0) {}

		size_t rowSize( ) const {
			return rowSize_;
		}
		size_t columnSize( ) const {
			return colSize_;
		}

		Flex2Col<T> operator[](size_t column) {
			assert(column < colSize_);
			return Flex2Col<T>(*this,column);
		}
		const Flex2Col<T> operator[](size_t column) const {
			Flex2<T> & us = const_cast<Flex2<T> & >(*this); 
			return Flex2Col<T>(us,column);
		}
		const void *ptr( ) const {
			return storage.data( );
		}
		/**
		* reconfigure to difference layout; previous values no
		* longer accessible using previous indexing scheme
		*/
		void reindex(size_t columnSize, size_t rowSize) {
			colSize_ = columnSize;
			rowSize_ = rowSize;
			storage.resize(columnSize * rowSize);
		}
		/**
		* allow setting with same array used to create DataSpace
		* array must have size >= 2
		* reconfigure to difference layout; previous values no
		* longer accessible using previous indexing scheme
		*/
		void reindex(hsize_t *dims) {
			colSize_ = dims[0]; 
			rowSize_ = dims[1]; 
			storage.resize(colSize_ * rowSize_);
		}
	private:
		friend struct Flex2Col<T>;
		std::vector<T> storage;
		size_t colSize_;
		size_t rowSize_;
	};

	template <class T>
	struct Flex2Col {
		T & operator[](size_t r) {
			assert(r < flex.rowSize( ));
			const size_t index = col * flex.rowSize( ) + r; 
			return flex.storage[index];
		}
		const T & operator[](size_t r) const {
			assert(r < flex.rowSize( ));
			const size_t index = col * flex.rowSize( ) + r; 
			return flex.storage[index];
		}

	private:
		friend struct Flex2<T>;
		Flex2Col(Flex2<T> & parent, size_t c)
			:flex(parent),
			col(c) {}
		Flex2<T> & flex;
		const size_t col;
	};

}
#endif
