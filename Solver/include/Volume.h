#ifndef Volume_h
#define Volume_h
#include <stddef.h>
#include <vector>
#include <array>
namespace spatial {
	//forward
	template<class T, int N>
	class TPoint;

	template<class T, int N>
	struct Segment;

	template<class T, int N>
	class Edge;

	template<class C, class V,int N>
	struct Volume;

	/**
	* external accessor to get edges from Volume
	*/
	template <class COORD_TYPE, class VALUE_TYPE,int N>
	struct SegmentAccessor {
		SegmentAccessor(const Volume<COORD_TYPE,VALUE_TYPE,N> &volume)
		 :vol(volume),
		 index(0) {}

		bool hasNext( ) const {
			return vol.more(index);
		}
		/**
		* get segment, advance index
		*/
		const Segment<COORD_TYPE,N> getAndAdvance( ) {
			return vol.getSegment(index++);
		}
	private:
		const Volume<COORD_TYPE,VALUE_TYPE,N> &vol;
		int index;
	};

	/**
	* specify volume behavior(s)
	* see <a href="http://en.wikipedia.org/wiki/Decorator_pattern">Decorator pattern </a>
	*/
	struct VolumeTraitsFast { 

	};


	template <class COORD_TYPE, class VALUE_TYPE, int N>
	struct VolumeImpl; 

	/**
	* volume without regard to geometry of sources
	* see <a href="http://en.wikipedia.org/wiki/State_pattern">State pattern </a>
	* @tparam COORD_TYPE vertices and edges
	* @tparam VALUE_TYPE type for volume
	* @tparam N number of dimensions
	*/
	template <class COORD_TYPE, class VALUE_TYPE, int N>
	struct Volume {
		typedef SegmentAccessor<COORD_TYPE,VALUE_TYPE,N> SegAccessor;
	
		Volume(size_t nConstructs = 0 );
		
		/**
		*  assumes ownership of implementation
		*/
		Volume(const Volume &rhs) 
			:state(rhs.state) {
				Volume &r = const_cast<Volume &>(rhs);
				r.state = nullptr;
		}
		/**
		* assumes ownership of implementation
		*/
		Volume & operator=(const Volume &rhs) {
			releaseExisting( );
			state = rhs.state;
			Volume &r = const_cast<Volume &>(rhs);
			r.state = nullptr;
			return *this;
		}

		/**
		* construct rectangular construct (rectangle, rectangular prism, etc)
		*/
		Volume(const std::array<COORD_TYPE,N> &origin, const std::array<COORD_TYPE,N> & lengths);

		~Volume( ) {
			releaseExisting( );
		}

		VALUE_TYPE volume() const {
			return state->volume( );
		};
		/**
		* contains no space
		*/
		bool empty( ) const {
			return state->empty( );
		}
		/**
		* remove existing vertices
		*/
		void clear( ) {
			state->clear( );
		}
		/**
		* close existing construct. It must contain at least
		* N + 1 points or std::invalid_argument will be thrown
		*/
		void close( ) {
			state->close( );
		}

		/**
		* is point inside Volume?
		*/
		bool inside(const TPoint<COORD_TYPE,N> &point) const {
			return state->inside(point);
		}

		/**
		* return volume as vector of contiguous vectors of points 
		*/
		typedef std::vector<TPoint<COORD_TYPE,N> > PointVector; 
		typedef std::vector<PointVector> VectorOfVectors; 
		/**
		* return copy of internal points
		*/
		VectorOfVectors points() const {
			return state->points( );
		}; 

		SegAccessor accessor( ) const {
			return SegAccessor(*this);
		}
		/**
		* move to next section for "add";  close polygon if necessary
		*/
		void nextSection( ) {
			state = state->nextSection( );
		};

		/**
		* return value for #fillingIterator
		*/
		typedef typename std::vector<TPoint<COORD_TYPE,N> >::iterator FillingIteratorType;
		/**
		* return iterator to fill current polygon 
		* @param n expected size of polygon, if known
		*/
		FillingIteratorType fillingIterator(size_t n) {
			state = state->prep(VolumeImpl<COORD_TYPE,VALUE_TYPE,N>::opFillingIterator);
			return state->fillingIterator(n);
		}
		/**
		* add point to current polygon; 
		*/
		void add(const TPoint<COORD_TYPE,N> & point) {
			state = state->add(point);
		}
		Volume<COORD_TYPE, VALUE_TYPE,N> intersection(const Volume<COORD_TYPE,VALUE_TYPE,N> &rhs) const {
			return state->intersection(rhs);
		}
		Volume<COORD_TYPE, VALUE_TYPE,N> intersection(const std::vector<TPoint<COORD_TYPE,N> > &rhs) const {
			return state->intersection(rhs);
		}

	protected:
		void releaseExisting( ) {
			if (state && !state->singleton( )) {
				delete state;
			}
		}
		bool more(size_t index) const {
			return state->more(index);
		};
		Segment<COORD_TYPE,N> getSegment(size_t index) const {
			return state->getSegment(index);
		}
		friend SegAccessor;

		VolumeImpl<COORD_TYPE,VALUE_TYPE,N> *state;
		/**
		* allow implement classes direct access to state
		*/
		friend struct VolumeImpl<COORD_TYPE,VALUE_TYPE,N>;
	};

	template <class COORD_TYPE, class VALUE_TYPE, int N>
	struct VolumeImpl {
		typedef std::vector<TPoint<COORD_TYPE,N> > PointVector; 
		typedef std::vector<PointVector> VectorOfVectors; 

		/**
		* operations which may require changing implementation body
		*/
		enum Operation {
			opFillingIterator
		};

		virtual ~VolumeImpl( ) {};
		/**
		* call before operations which may require changing implementation body,
		* default is to do nothing 
		* @return this by default
		*/
		virtual VolumeImpl<COORD_TYPE,VALUE_TYPE,N>* prep(Operation) {
			return this;
		}
		virtual VALUE_TYPE volume() const = 0;
		virtual bool empty( ) const = 0;
		virtual void clear( ) = 0;
		virtual void close( ) = 0;
		virtual VectorOfVectors points() const = 0;
		virtual VolumeImpl<COORD_TYPE,VALUE_TYPE,N> * nextSection( ) = 0;
		virtual VolumeImpl<COORD_TYPE,VALUE_TYPE,N> * add(const TPoint<COORD_TYPE,N> & point) = 0;
		virtual Volume<COORD_TYPE, VALUE_TYPE,N> intersection(const Volume<COORD_TYPE,VALUE_TYPE,N> &rhs) const = 0;
		virtual Volume<COORD_TYPE, VALUE_TYPE,N> intersection(const std::vector<TPoint<COORD_TYPE,N> > &rhs) const = 0; 

		virtual bool more(size_t index) const = 0; 
		virtual bool inside(const TPoint<COORD_TYPE,N> &point) const=0; 
		virtual Edge<COORD_TYPE,N> getEdge(size_t index) const = 0; 
		virtual Segment<COORD_TYPE,N> getSegment(size_t index) const = 0; 
		virtual typename std::vector<TPoint<COORD_TYPE,N> >::iterator fillingIterator(size_t n) = 0; 
		virtual bool singleton( ) {
			return false;
		}
	protected:
		const VolumeImpl<COORD_TYPE,VALUE_TYPE,N> & pal(const Volume<COORD_TYPE,VALUE_TYPE,N> &rhs) const {
			return *rhs.state;
		}
	};

}
#endif
