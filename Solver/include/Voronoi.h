#ifndef Voronoi_h
#define Voronoi_h
#include "TPoint.h"
#include "VoronoiResult.h"
#pragma warning ( disable : 4267 4244 )
#include <boost/polygon/voronoi.hpp>
#pragma warning ( default : 4267 )
namespace spatial {
	typedef TPoint<VoronoiType,2> VoronoiPoint;
}

namespace boost {
	namespace polygon {
	template<>
	struct geometry_concept<spatial::VoronoiPoint> { typedef point_concept type; };

	template<>
	struct point_traits<spatial::VoronoiPoint> {
		typedef double coordinate_type;
		static inline coordinate_type get(const spatial::VoronoiPoint& point, orientation_2d orient) {
				return (orient == HORIZONTAL) ? point.get(spatial::cX) : point.get(spatial::cY);
			}
		};

	/*
	template<>
	struct geometry_concept<spatial::GhostPoint<double,2> > { typedef point_concept type; };

	template<>
	struct point_traits<spatial::GhostPoint<double,2> > {
		typedef double coordinate_type;
		static inline coordinate_type get(const spatial::GhostPoint<double,2> & point, orientation_2d orient) {
				return (orient == HORIZONTAL) ? point.get(spatial::cX) : point.get(spatial::cY);
			}
		};
	*/
	}
}
namespace spatial {

	template <class T, int N>
	class Voronoi {
#ifdef NOT_USED 
		//const static int DEFAULT_N_POINTS = 10;
		const static int DEFAULT_GHOST_DIST = 1000;
	public:
		Voronoi(long ghost_distance_ = DEFAULT_GHOST_DIST)
			:impl(ghost_distance) {}

		void add(const TPoint<T,N> &p) {
			impl.add(p);
		}

		/**
		* remove all points
		*/
		void clear( ) {
			impl.clear( );
		}

		const TPoint<T,N> & cell(size_t index) const {
			return impl.cell(index); 
		}

		size_t numberPoints( ) const {
			return impl.numberPoints( ); 
		}

		size_t numberCells( ) const {
			return impl.numberCells( ); 
		}

		std::vector<TPoint<T,N> > getVertices(size_t cellIndex) const {
			return impl.getVertices( );
		}
#endif
	};


	class Voronoi2D {
	public:
		typedef TPoint<VoronoiType, 2> VoronoiPoint;
		Voronoi2D(VoronoiType ghost_distance_)
			:points(),
			dirty(true),
			vd( ),
			ghostDistance(ghost_distance_) {
		}

		void add(const VoronoiPoint &p) {
			points.push_back(p);
			dirty = true;
		}
		void add(VoronoiType x, VoronoiType y) {
			points.push_back(VoronoiPoint(x,y) );
			dirty = true;
		}

		/**
		* set ghost distance 
		*/
		void setGhostDistance(VoronoiType gd) {
			assert(gd > 0);
			ghostDistance = gd;
		}

		/**
		* remove all points
		*/
		void clear( ) {
			points.clear( );
			vd.clear( );
			dirty = true;
		}

		const VoronoiPoint & cell(size_t index) const {
			return points[index];
		}

		size_t numberPoints( ) const {
			return points.size( );
		}
		//return copy of current points
		std::vector<VoronoiPoint> currentPoints( ) const {
			return points;
		}

		size_t numberCells( ) const {
			makeClean( );
			return vd.num_cells( );
		}
		void getVertices(VoronoiResult & result, size_t cellIndex) const;


private:
	typedef double ImplementationType;
	typedef Voronoi2D OurType;
	//convenience typedefs
	typedef boost::polygon::voronoi_diagram<ImplementationType>::edge_type EdgeType;
	typedef boost::polygon::voronoi_diagram<ImplementationType>::const_edge_iterator EdgeIterator;
	typedef boost::polygon::voronoi_vertex<ImplementationType> Vertex;

	std::vector<VoronoiPoint> points;
	mutable bool dirty;
	mutable boost::polygon::voronoi_diagram<ImplementationType> vd;
	VoronoiType ghostDistance;

	/**
	* do actual calculation. set dirty to false
	**/
	void calculate( );
	void makeClean( ) const {
		if (dirty) {
			OurType & self = const_cast<OurType &>(*this);
			self.calculate( );
		}
	}

	//private, not implemented
	Voronoi2D(const Voronoi2D& other);
	Voronoi2D & operator=(const Voronoi2D& other);


	/**
	* extract single point from edge at ; actual or ghosted
	* @param results output ; collection to store results in 
	* @param edge to extract vertex from
	* @param anchor pointto extract vertex from; if null, ghost point created
	* @param other vertex at other end of edge, used for ghost building 
	* @return type of point(s) inserted 
	*/
	VoronoiResult::Type extractPoint(std::vector<VoronoiGhostPoint> & results, const EdgeType & edge, const Vertex * const anchor, const Vertex * const other, bool reverse) const;

	/**
	* return infinite edge, ghosted. numberCells must be 2 
	*/
	void getDoubleInfinite(VoronoiResult & result, size_t cellIndex) const;
	};

}
#endif
