#include <Volume.h>
#include <Edge.h>
#include "intersection.h"
namespace spatial {
	//forward
	template <class REAL>
	struct Polygon; 

	template <class REAL>
	struct Polygons; 

	/**
	* support double dispatch virtual functions
	*/
	template <class REAL>
	struct VolumeImplDoubleDispatch : public VolumeImpl<REAL,2> {
		virtual Volume<REAL, 2> intersectionSingle(const Polygon<REAL> &rhs) const = 0; 
		virtual Volume<REAL, 2> intersectionMany(const Polygons<REAL> &rhs) const = 0; 
	};


	template <class REAL>
	struct EmptyVolume2 : public VolumeImplDoubleDispatch<REAL> {
		typedef typename VolumeImpl<REAL,2>::PointVector PointVector; 
		typedef typename VolumeImpl<REAL,2>::VectorOfVectors VectorOfVectors;

		static EmptyVolume2<REAL> *getSingleton( ) {
			return &instance;
		}
		REAL volume() const {
			return 0;
		}
		bool empty( ) const {
			return true;
		}
		void clear( ) {}
		VectorOfVectors points() const {
			return VectorOfVectors( );
		}
		bool more(size_t index) const {
			return false;
		}
		Edge<REAL,2> get(size_t index) const {
			throw std::invalid_argument("Invalid index");
		}
		virtual void close( ) {} 

		virtual bool inside(const TPoint<REAL,2> &point) const {
			return false;
		}
		/**
		* EmptyVolume doesn't exist if point has been added, so
		* logically we're still on the first section
		*/
		VolumeImpl<REAL,2> *nextSection( ) {
			return this;	
		}

		virtual VolumeImpl <REAL,2> *add(const TPoint<REAL,2> & point) {
			Polygon<REAL> *poly = new Polygon<REAL>( );
			poly->add(point);
			return poly;
		}

		virtual VolumeImpl<REAL,2>* prep(typename VolumeImpl<REAL,2>::Operation op) {
		        void * ptr = VolumeImpl<REAL,2>::prep(op);
			assert ((op == VolumeImpl< REAL,2>::opFillingIterator));
			return new Polygon<REAL>( ); 
		}
		/**
		* should never be called, as #prep should replace this with 
		* implementation that supports call
		*/
		virtual typename std::vector<TPoint<REAL,2> >::iterator fillingIterator(size_t n) { 
			throw std::domain_error("fillingIterator call on EmptyVolume");
		}
		virtual Volume<REAL, 2> intersection(const std::vector<TPoint<REAL,2> > &rhs) const {
			return Volume<REAL,2>(0); 
		}
		virtual Volume<REAL, 2> intersection(const Volume<REAL,2> &rhs) const {
			return Volume<REAL,2>(0); 
		}
		virtual Volume<REAL, 2> intersectionSingle(const Polygon<REAL> &rhs) const { 
			return Volume<REAL,2>(0); 
		}
		virtual Volume<REAL, 2> intersectionMany(const Polygons<REAL> &rhs) const {
			return Volume<REAL,2>(0); 
		}
		/**
		* don't delete this object
		*/
		virtual bool singleton( ) {
			return true;
		}
		void operator delete(void *ptr) {
			throw std::logic_error("EmptyVolume2 delete");
		}
	private:
		void *operator new[](size_t s); 
		/**
		* prevent allocating new object, as all
		* empty volumes are equivalent
		*/
		void *operator new(size_t s); 
		static EmptyVolume2<REAL> instance;
	};

	template <class REAL>
	EmptyVolume2<REAL> EmptyVolume2<REAL>::instance;

	template <class REAL>
	struct Polygon : public VolumeImplDoubleDispatch <REAL> {
		typedef typename VolumeImpl<REAL,2>::PointVector PointVector; 
		typedef typename VolumeImpl<REAL,2>::VectorOfVectors VectorOfVectors;
		typedef TPoint<REAL,2> Point;
		Polygon( )
			:pointStorage( ),
			dirty(true),
			vol(0) {}

		Polygon(const std::vector<Point> &points_)
			:pointStorage(points_),
			dirty(true),
			vol(0) {}

		/**
		* construct rectangular Polygon
		*/
		Polygon(const std::array<REAL,2> &origin, const std::array<REAL,2> & lengths)
			:pointStorage(),
			dirty(false),
			vol(lengths[cX] * lengths[cY]) 
		{
			std::array<REAL,2> working(origin);
			pointStorage.push_back(working); //top left
			working[cX] += lengths[cX];
			pointStorage.push_back(working); //top right 
			working[cY] += lengths[cY];
			pointStorage.push_back(working); //bottom right 
			working[cX] -= lengths[cX];
			pointStorage.push_back(working); //bottom left 
			pointStorage.push_back(origin); //top left again (close)
		}

		/**
		* add point to current polygon; 
		*/
		Polygon<REAL> *add(const Point &p) {
			pointStorage.push_back(p);
			dirty = true;
			return this;
		}

		void add(double x, double y) {
			add(Point(x,y));
		}

		bool closed( ) const {
			if (!pointStorage.empty( )) {
				return pointStorage.front( ) == pointStorage.back( );
			}
			return false;
		}

		virtual bool inside(const TPoint<REAL,2> &point) const {
			return spatial::inside(pointStorage,point);
		}

		/**
		* close polygon
		* @throws std::invalid_argument if polygon empty
		*/
		void close( ) {
			if (!closed( )) {
				if (pointStorage.empty( )) {
					throw std::invalid_argument("close on empty polygon");
				}
				pointStorage.push_back(pointStorage.front( ));
			}
			assert(closed( ));
		}

		/**
		* set internal vector to have n points and return iterator 
		*/
		virtual typename std::vector<TPoint<REAL,2> >::iterator fillingIterator(size_t n) { 
			dirty = true;
			pointStorage.reserve(n + 1); //allow extra for closing polygon
			pointStorage.resize(n);
			return pointStorage.begin( );
		}

		/**
		* remove existing vertices
		*/
		void clear( ) {
			dirty = true;
			pointStorage.clear( );
		}

		VolumeImpl<REAL,2> *nextSection( ) {
			if (pointStorage.empty( )) {
				return this;
			}
			assert(pointStorage.size( ) >= 3); //should be at least three points in polygon
			Polygons<REAL> *polys = new Polygons<REAL>(pointStorage);
			delete this;
			return polys->nextSection( );
		}

		VectorOfVectors points() const {
			VectorOfVectors rval;
			rval.push_back(pointStorage);
			return rval;
		};

		/**
		* calculate or return polygon volume
		* evaluated lazily 
		* @throws std::invalid_argument if polygon not closed 
		*/
		REAL volume( ) const {
			if (!closed( )) {
				throw std::invalid_argument("volume call on open polygon");
			}
			if (dirty) { 
				Polygon & us = const_cast<Polygon &>(*this); //"logically" const
				us.calculateVolume( );
			}
			return vol;
		}

		const std::vector<Point> & vertices( ) const {
			return pointStorage;
		}

		bool empty( )  const {
			return pointStorage.empty( );
		}
		virtual Volume<REAL, 2> intersection(const Volume<REAL,2> &rhs) const {
			const VolumeImplDoubleDispatch<REAL> & rhsDD = static_cast<const VolumeImplDoubleDispatch<REAL> &>(this->pal(rhs));
			return rhsDD.intersectionSingle(*this); 
		}
		virtual Volume<REAL, 2> intersection(const std::vector<TPoint<REAL,2> > &rhs) const {
			Volume<REAL,2> rval;
			spatial::intersections(rval,pointStorage,rhs);
			return rval;
		}

		virtual Volume<REAL, 2> intersectionSingle(const Polygon<REAL> &rhs) const { 
			Volume<REAL,2> rval;
			spatial::intersections(rval,pointStorage,rhs.pointStorage);
			return rval;
		}
		virtual Volume<REAL, 2> intersectionMany(const Polygons<REAL> &rhs) const {
			Volume<REAL,2> rval;
			spatial::intersectionsManySingle(rval,rhs.storage( ), pointStorage);
			return rval;
		}
	protected:
		bool more(size_t index) const {
			return index < pointStorage.size( ) - 1;
		}
		Edge<REAL,2> get(size_t index) const {
			return Edge<REAL,2>(pointStorage[index],pointStorage[index+1]);
		}

	private:
		void calculateVolume( ) {
			assert(dirty);
			assert(closed());
			using spatial::cX;
			using spatial::cY;
			vol = 0;	
			typename std::vector<Point>::const_iterator iter = pointStorage.begin( );
			typename std::vector<Point>::const_iterator next(iter); 
			++next; //initialize to following point
			for (;next!=pointStorage.end( );++iter,++next) {
				vol += iter->get(cX) * next->get(cY) - iter->get(cY) * next->get(cX);
			}
			vol = std::abs(vol/2);
			dirty = false;
		}
		std::vector<Point> pointStorage;
		bool dirty;
		REAL vol;
	};

	template <class REAL>
	struct Polygons : public VolumeImplDoubleDispatch<REAL> {
		typedef typename VolumeImpl<REAL,2>::PointVector PointVector; 
		typedef typename VolumeImpl<REAL,2>::VectorOfVectors VectorOfVectors;
		typedef TPoint<REAL,2> Point;

		Polygons( )
			:polys(),
			current(0),
			dirty(true),
			vol(0) {}

		Polygons(const std::vector<Point> &input)
			:polys(),
			dirty(true),
			vol(0) {
				polys.push_back(input);
				current = &polys.front( );
		}

		bool empty( )  const {
			return polys.empty( );
		}

		/**
		* remove existing vertices
		*/
		void clear( ) {
			polys.clear( );
		}
		/**
		* close polygon
		* @throws std::invalid_argument if polygon empty
		*/
		void close( ) {
			if (!closed( )) {
				if (!current || current->size( ) < 3) {
					throw std::invalid_argument("close with less than 3 points"); 
				}
				current->push_back(current->front( ));
			}
			assert(closed( ));
		}

		bool closed( ) const {
			if (!current) {
				return false;
			}
			return current->front( ) == current->back( );
		}

		virtual bool inside(const TPoint<REAL,2> &point) const {
			for (typename VectorOfVectors::const_iterator iter = polys.begin( ); iter != polys.end( ); ++iter) {
				const PointVector &pv = *iter;
				bool in = spatial::inside(pv,point);
				if (in) {
					return true;
				}
			}
			return false;
		}
		/**
		* move to next polygon (if current has points)
		*/
		Polygons<REAL> *nextSection( ) {
			assert(empty( ) || current->size( ) >= 3); //should be at least three points in polygon
			if (empty( )) {
				return this;
			}
			close( ); //close current if necessary
			polys.push_back(std::vector<Point>( ));
			current = &polys.back( );
			return this;
		}
		/**
		* add point to current polygon; 
		*/
		Polygons<REAL> * add(const TPoint<REAL,2> & point) {
			if (current) {
				current->push_back(point);
				return this;
			}
			polys.push_back(std::vector<Point>( ));
			current = &polys.back( );
			current->push_back(point);
			return this;
		}

		virtual typename std::vector<TPoint<REAL,2> >::iterator fillingIterator(size_t n) { 
			current->reserve(n + 1); //allow extra for closing polygon
			current->resize(n);
			return current->begin( );
		}

		VectorOfVectors points() const {
			return polys;
		}

		/**
		* calculate or return polygon volume
		* evaluated lazily 
		* @throws std::invalid_argument if polygon not closed 
		*/
		REAL volume( ) const {
			if (dirty) { 
				Polygons & us = const_cast<Polygons &>(*this); //"logically" const
				us.calculateVolume( );
			}
			return vol;
		}
		virtual Volume<REAL, 2> intersection(const Volume<REAL,2> &rhs) const {
			const VolumeImplDoubleDispatch<REAL> & rhsDD = static_cast<const VolumeImplDoubleDispatch<REAL> &>(this->pal(rhs));
			return rhsDD.intersectionMany(*this); 
		}
		virtual Volume<REAL, 2> intersection(const std::vector<TPoint<REAL,2> > &rhs) const {
			Volume<REAL,2> rval;
			spatial::intersectionsManySingle(rval,polys,rhs);
			return rval;
		}
		virtual Volume<REAL, 2> intersectionSingle(const Polygon<REAL> &rhs) const { 
			Volume<REAL,2> rval;
			spatial::intersectionsManySingle(rval,polys,rhs.vertices( ));
			return rval;
		}
		virtual Volume<REAL, 2> intersectionMany(const Polygons<REAL> &rhs) const {
			Volume<REAL,2> rval;
			spatial::intersectionsManyMany(rval,polys,rhs.polys);
			return rval;
		}

		const VectorOfVectors &storage( ) const {
			return polys;
		}
	protected:
		bool more(size_t index) const {
			if (index >= referenceIndex)  {
				const size_t adjusted = index - referenceIndex;
				if (adjusted < getVector->size( ) - 1) {
					return true;
				}
			}

			size_t base = 0; 
			for (size_t pIndex = 0; pIndex < polys.size( ); ++pIndex) {
				const size_t adjusted = index - base;
				if (adjusted < polys[pIndex].size( ) - 1) {
					referenceIndex = base;
					getVector = &polys[pIndex];
					return true;
				}
				base += polys[pIndex].size( ) - 1;
			}
			return false;
		}
		Edge<REAL,2> get(size_t index) const {
			if (index >= referenceIndex)  {
				const size_t adjusted = index - referenceIndex;
				if (adjusted < getVector->size( ) - 1) {
					const PointVector & pv = *getVector;
					return Edge<REAL,2>(pv[adjusted],pv[adjusted+1]);
				}
			}
			size_t adjusted = index;
			for (size_t pIndex = 0; pIndex < polys.size( ); ++pIndex) {
				if (adjusted < polys[pIndex].size( ) - 1) {
					return Edge<REAL,2>(polys[pIndex][adjusted],polys[pIndex][adjusted+1]);
				}
			}
			throw std::invalid_argument("invalid call to get");
		}


	private:
		void calculateVolume( ) {
			assert(dirty);
			vol = 0;
			typename std::vector<std::vector<Point> >::const_iterator iter = polys.begin( );
			for (;iter != polys.end( );++iter) {
				spatial::Polygon<REAL> p(*iter);
				vol += p.volume( );
			}

			dirty = false;
		}
		VectorOfVectors polys;
		/**
		*support for nextSection, add
		*/
		PointVector * current; 
		bool dirty;
		REAL vol;
		/**
		* more and get support
		* base index of #getVector
		*/
		mutable size_t referenceIndex; 
		/**
		* cached pointer
		*/
		mutable const PointVector *getVector;
	};

	template <class REAL, int N>
	Volume<REAL, N>::Volume(size_t nPolygons)
		:state(nullptr) {
			if (N == 2) {
				switch (nPolygons) {
				case 0:
					state = EmptyVolume2<REAL>::getSingleton( );
					break;
				case 1:
					state = new Polygon<REAL>( );
					break;
				default:
					state = new Polygons<REAL>( );
					break;
				}
			}
	}
	template <class REAL, int N>
	Volume<REAL,N>::Volume(const std::array<REAL,N> &origin, const std::array<REAL,N> & lengths)
		:state(nullptr) {
			if (N == 2) {
				state = new Polygon<REAL>(origin,lengths);
			}
	}

	/*
	template <class REAL, int N>
	typename std::vector<TPoint<REAL,N> >::iterator fillingIterator(size_t n) {
	}
	*/
}

template struct spatial::Polygon<double>;
template struct spatial::Polygons<double>;
//template spatial::EmptyVolume2<double>;
template struct spatial::Volume<double,2>;