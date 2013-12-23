#ifndef Mesh_h
#define Mesh_h
#include <cassert>
#include <vector>
#include <limits>
#include <algorithm>
#include <TPoint.h>
#include <DiffuseAdvectCache.h>
#include <boost/iterator/iterator_facade.hpp>
namespace spatial {

	/**
	* helper class for mesh to allow specialization of recursive template
	* for D == 0 case
	*/
	template <int N, int D>
	struct MeshHelper {
		/**
		* return storage size of given dimension
		*/
		template <class MESH>
		static size_t dimsize(const MESH &m) { 
			return m.nPoints[D-1] * MeshHelper<N, D - 1>::dimsize(m); 
		}

		/**
		* return index in storage array of given multi dimension
		* coordinates
		*/
		template <class MESH>
		static size_t index(const MESH & m ,const std::array<size_t,N> & position) {
			int idx =  N - D;
			const size_t current = position[idx] * dimsize(m);
			const size_t lower = MeshHelper<N,D - 1>::index(m,position);
			return lower + current;
		}
	};

	/**
	* specialzation for 0 to stop recursion
	*/
	template <int N>
	struct MeshHelper<N, 0> {
		template <class MESH>
		static size_t dimsize(const MESH &m) { 
			return 1;
		}

		template <class MESH>
		static size_t index(const MESH & m ,const std::array<size_t,N> & position) {
			return position[0];
		}
	};

	//forward
	struct VoronoiResult;

	//forward
	template<class REAL, int N, class TELEMENT> struct Mesh;

	/**
	* definition of mesh geometry
	*/
	template<class REAL, int N>
	struct MeshDef {
		typedef REAL realType;
		static int numDim( ) { return N; }

		/**
		* @param origin_ coordinates of "lower left" corner of modeled region
		* @param sizes_ sizes of sides
		* @param nPoints_ how many points in each region
		*/
		MeshDef(const std::array<REAL,N> & origin_, const std::array<REAL,N> & sizes_, const std::array<size_t,N> & nPoints_)
			:origin(origin_),
			intervals( ),
			nPoints(nPoints_), 
			minInterval(std::numeric_limits<REAL>::max( )) {
				for (int i = 0; i < N; i++) {
					intervals[i] = sizes_[i]/nPoints[i];
					minInterval = std::min(minInterval,intervals[i]);
				}
		}
		/**
		* default origin to 0,0,...
		* @param sizes_ sizes of sides
		* @param nPoints_ how many points in each region
		*/
		MeshDef(const std::array<REAL,N> & sizes_, const std::array<size_t,N> & nPoints_)
			:origin( ),
			intervals( ),
			nPoints(nPoints_), 
			minInterval(std::numeric_limits<REAL>::max( )) {
				for (int i = 0; i < N; i++) {
					intervals[i] = sizes_[i]/nPoints[i];
					minInterval = std::min(minInterval,intervals[i]);
				}
		}

		/**
		* copy ctor, of course
		*/
		MeshDef(const MeshDef & rhs)
			:origin(rhs.origin),
			intervals(rhs.intervals),
			nPoints(rhs.nPoints),
			minInterval(rhs.minInterval) {}

		REAL startCorner(Axis a) const {
			return origin[a];
		}

		REAL interval(Axis a) const {
			return intervals[a];
		}

		REAL size(Axis a) const {
			return intervals[a] * nPoints[a];
		}

		size_t numCells(Axis a ) const {
			return nPoints[a];
		}

		size_t numCells( ) const {
			size_t s = 1;
			for (int i =0; i < N; i++) {
				s *= nPoints[i];
			}
			return s;
		}

		REAL minimumInterval( ) const {
			return minInterval;
		}
		/**
		* translate point from grid coordinates to scaled
		*/
		template <class IN_TYPE>
		TPoint<REAL,N> gridToSpatial(const TPoint<IN_TYPE,N> & gridReferencedPoint) const {
			TPoint<REAL,N> rval; 
			for (int d = 0 ; d < N; d++ ) {
				Axis a = static_cast<Axis>(d);
				rval(a) = origin[d] + (gridReferencedPoint(a) + 0.5) * intervals[d];
			}
			return rval;
		}

		/**
		* return vector of indexes for specified dimension
		*/
		std::vector<REAL> coordinateValues(spatial::Axis a) const;

	protected:

		std::array<REAL,N> origin; 
		/**
		* distance between adjacent points 
		*/
		std::array<REAL,N> intervals; 
		/**
		* number nodes in one dimension
		*/
		std::array<size_t,N> nPoints; 
		REAL minInterval;
	};

	template<class REAL, int N, class TELEMENT>
	struct Mesh : public MeshDef<REAL,N> {
		typedef TDiffuseAdvectCache<REAL,N,TELEMENT::numSpecies> DiffuseAdvectCache; 
		Mesh(const MeshDef<REAL,N> &definition) 
			:MeshDef<REAL,N>(definition),
			storage(0),
			daCache(TELEMENT::createCache(definition.minimumInterval( ) ))
		{
			std::array<size_t,N> loop;
			std::array<REAL,N> startPoint; 
			for (int i =0; i < N; i++) {
				loop[i] = 0; 
				startPoint[i] = this->origin[i] + this->intervals[i] / 2.0;
			}
			const size_t needed =  this->numCells( ) * sizeof(TELEMENT);
			storage = static_cast<TELEMENT *>(malloc(needed));
			do {
				std::array<REAL,N> values;
				for (int d = 0; d < N; d++) {
					values[d] = loop[d] * this->intervals[d] + startPoint[d];
				}
				size_t idx = index<N-1>(loop);
				void * addr = &storage[idx];
				new (addr) TELEMENT(loop.data( ) ,values.data( )); //placement new
			}
			while (increment(0,loop));
		}
	private:


		/**
		* increment loop to next set of coordinates
		* @param dimension to increment
		* @loop in/out coordinates to increment, should be of length N
		*/
		bool increment(int dim, std::array<size_t,N> & loop) const {
			if (dim == N) {
				return false;
			}

			if (loop[dim] < this->nPoints[dim] - 1) {
				loop[dim]++;
				return true;
			}
			if (dim < N && increment(dim+1,loop)) {
				loop[dim] = 0;
				return true;
			}
			return false;
		}


		/**
		* return storage size of given dimension
		*/
		template <int D>
		size_t dimsize( ) const { 
			return MeshHelper<N,D>::dimsize(*this);
		}

		/**
		* return index in storage array of given multi dimension
		* coordinates
		*/
		template <int D>
		size_t index(const std::array<size_t,N> & position) const {
			return MeshHelper<N, D>::index(*this,position);
		}

		TELEMENT *storage;
		DiffuseAdvectCache *daCache;

		static void callDestructor(TELEMENT &element)  {
			element.~TELEMENT( );
		}

	public:
		typedef TELEMENT elementType;

		~Mesh( ) {
			std::for_each(begin( ),end( ),callDestructor);
			free(storage);
			delete(daCache);
		}


		DiffuseAdvectCache &diffuseAdvectCache( ) { 
			return *daCache;
		}

		/**
		* retrieve element given set of coordinates
		* @param position ( length N ) to retrieve
		* undefined behavior if invalid coordinates
		*/
		TELEMENT &get(const std::array<size_t,N> & position) const {
			size_t i = index<N-1>(position);
			return storage[i];
		}

		/**
		* retrieve element given set of coordinates
		* @param position ( length N ) to retrieve
		* return null pointer if coordinates out of range
		*/
		TELEMENT *query(const std::array<size_t,N> & position) const {
			for (int i = 0; i < N; i++) {
				if (position[i] < 0 || position[i] >= this->nPoints[i]) {
					return nullptr;
				}
			}
			return &get(position);
		}


		/**
		* iterator base class
		*/
		template<class ETYPE> 
		class ibase : public boost::iterator_facade<ibase<ETYPE>, ETYPE,std::forward_iterator_tag> {
		public:
			typedef const Mesh<REAL,N,TELEMENT> OwnerType; 
			OwnerType & owner;
		private:
			std::array<size_t,N> position;
		public:
			ibase(OwnerType &own)
				: owner(own), 
				position( ) {}
			ibase(OwnerType &own, const size_t *pos)
				: owner(own) {
					for (int i = 0; i <N; i++) {
						position[i] = pos[i];
					}
			}
			template <class ALT>
			ibase(ibase<ALT> const & rhs)
				:owner(rhs.owner),
				position(rhs.getPosition( )) {}

			const std::array<size_t,N> & getPosition( ) const {
				return position;
			}

			//ideally these would be private but I couldn't get the proper "friend" declaration to work
			//in VS2012
			bool increment(int dim) {
				if (dim < N) {
					if (position[dim] < owner.nPoints[dim] -1 ) { 
						position[dim]++;
						return true;
					}
					const bool istat = increment(dim + 1);
					if (istat) {
						position[dim] = 0;
					}
					return istat;
				}
				//we're going to use position[N-1] == owner.nPoints[N-1] for "end" position
				position[dim-1]++;
				return false;
			}
			template <class ALT>
			bool equal(const ibase<ALT> & rhs) const {
				for (int i = 0; i < N ; i++) {
					if (position[i] != rhs.getPosition( )[i]) {
						return false;
					}
				}
				return true;
			}

			void increment( ) {
				increment(0);
			}

			ETYPE & dereference( ) const {
				ETYPE & rval = owner.get(position);
				return rval;
			}

		};
		//helper classes
		template<int A, int B> friend struct MeshHelper;
		friend class ibase<TELEMENT>;
		friend class ibase<const TELEMENT>;
		/**
		* forward iterator
		*/
		typedef ibase<TELEMENT> iterator;
		/**
		* const forward iterator
		*/
		typedef ibase<const TELEMENT> const_iterator;

		const_iterator begin( ) const {
			const_iterator rval(*this);
			return rval;
		}

		const_iterator end( ) const {
			size_t construct[N];
			for (int i = 0; i < N; i++) {
				construct[i] = this->nPoints[i] - 1; 
			}
			construct[N-1]++;
			return const_iterator(*this,construct);
		}

		iterator begin( ) {
			iterator rval(*this);
			return rval;
		}

		iterator end( )  {
			size_t construct[N];
			for (int i = 0; i < N; i++) {
				construct[i] = this->nPoints[i] - 1; 
			}
			construct[N-1]++;
			return iterator(*this,construct);
		}

	};

	template<class MPOINT >
	struct Positions {
		std::vector<MPOINT *> inside;
		std::vector<MPOINT *> outside;
		std::vector<MPOINT *> boundary;
	}; 
}
#endif
