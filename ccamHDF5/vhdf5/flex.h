#ifndef vhd5_flex_h
#define vhd5_flex_h
#include <cassert>
#include <vhdf5/vH5cpp.h>
namespace vcellH5 {


	template <class T> struct Flex2Col;

	/**
	*
	* provide flexible interpretation of static buffer for writing
	* different size blocks to HDF5
	* e.g.  vcellH5::Flex2<double> buffer(3,4);
	* buffer[2][3] = 4;
	* dataset.write(buffer.ptr( ),doublePointType,memoryspace,dataspace);
	*
	*  vcellH5::Flex3<double> data(3,4,5);
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
		Flex2 & reindex(size_t columnSize, size_t rowSize) {
			colSize_ = columnSize;
			rowSize_ = rowSize;
			storage.resize(columnSize * rowSize);
			return *this;
		}
		/**
		* allow setting with same array used to create DataSpace
		* array must have size >= 2
		* reconfigure to difference layout; previous values no
		* longer accessible using previous indexing scheme
		*/
		Flex2 & reindex(hsize_t *dims) {
			return reindex( dims[0],dims[1]);
		}
		/**
		* rest all values to default constructor value
		*/
		void reset( ) {
			std::fill(storage.begin( ),storage.end( ),T( ));
		}
	private:
		friend Flex2Col<T>;
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
		friend Flex2<T>;
		Flex2Col(Flex2<T> & parent, size_t c)
			:flex(parent),
			col(c) {}
		Flex2<T> & flex;
		const size_t col;
	};

// ------------------------------------------------	
// 3 dimensional 
// ------------------------------------------------	
	template <class T> struct Flex3Depth;
	template <class T> struct Flex3Col;

#pragma warning ( disable : 4351 )
	template <class T>
	struct Flex3 {
		Flex3(size_t depthSize, size_t columnSize, size_t rowSize)
			:storage(depthSize * columnSize* rowSize ),
			depthSize_(depthSize),
			colSize_(columnSize),
			rowSize_(rowSize), 
			depthBlock(colSize_ * rowSize_) {}

		/**
		* allow construction with same array used to create DataSpace
		* array must have size >= 3
		*/
		Flex3(hsize_t *dims)
			:storage(dims[0] * dims[1] * dims[2]),
			depthSize_(dims[0]),
			colSize_(dims[1]),
			rowSize_(dims[2]),
			depthBlock(colSize_ * rowSize_) {}

		/**
		* default ctor -- must call reindex before use
		*/
		Flex3( )
			:storage(0),
			depthSize_(0),
			colSize_(0),
			rowSize_(0),
			depthBlock(0) {}

		size_t rowSize( ) const {
			return rowSize_;
		}
		size_t columnSize( ) const {
			return colSize_;
		}
		size_t depthSize( ) const {
			return depthSize_;
		}

		Flex3Depth<T> operator[](size_t depth) {
			assert(depth < depthSize_);
			return Flex3Depth<T>(*this,depth);
		}
		const Flex3Depth<T> operator[](size_t depth) const {
			assert(depth < depthSize_);
			Flex3<T> & us = const_cast<Flex3<T> & > (*this);
	
			return Flex3Depth<T>(us,depth);
		}
		const void *ptr( ) const {
			return storage.data( );
		}
		/**
		* reconfigure to difference layout; previous values no
		* longer accessible using previous indexing scheme
		*/
		Flex3 & reindex(size_t depthSize, size_t columnSize, size_t rowSize) {
			depthSize_ = depthSize;
			colSize_ = columnSize;
			rowSize_ = rowSize;
			depthBlock = columnSize * rowSize;
			storage.resize(depthSize * depthBlock);
			return *this;
		}
		/*
		* allow setting with same array used to create DataSpace
		* array must have size >= 2
		* reconfigure to difference layout; previous values no
		* longer accessible using previous indexing scheme
		*/
		Flex3 & reindex(hsize_t *dims) {
			return reindex(dims[0],dims[1],dims[2]);
		}
		/**
		* rest all values to default constructor value
		*/
		void reset( ) {
			std::fill(storage.begin( ),storage.end( ),T( ));
		}
	private:
		friend Flex3Col<T>;
		std::vector<T> storage;
		size_t depthSize_;
		size_t colSize_;
		size_t rowSize_;
		size_t depthBlock;
	};

	template <class T>
	struct Flex3Depth {
		Flex3Col<T> operator[](size_t c) {
			return Flex3Col<T>(flex,depth,c);
		}
		const Flex3Col<T> operator[](size_t c) const {
			return Flex3Col<T>(flex,depth,c);
		}
  private:
		friend Flex3<T>;
		Flex3Depth(Flex3<T> & parent, size_t d)
			:flex(parent),
			depth(d) {}
		Flex3<T> & flex;
		const size_t depth;
	};

	template <class T>
	struct Flex3Col {
		T & operator[](size_t r) {
			assert(r < flex.rowSize( ));
			const size_t index = depth * flex.depthBlock + col * flex.rowSize( ) + r; 
			return flex.storage[index];
		}
		const T & operator[](size_t r) const {
			assert(r < flex.rowSize( ));
			const size_t index = depth * flex.depthBlock + col * flex.rowSize( ) + r; 
			return flex.storage[index];
		}

	private:
		friend Flex3<T>;
		friend Flex3Depth<T>;
		Flex3Col(Flex3<T> & parent, size_t d, size_t c)
			:flex(parent),
			depth(d),
			col(c) {}
		Flex3<T> & flex;
		const size_t depth;
		const size_t col;
	};

}
#endif
