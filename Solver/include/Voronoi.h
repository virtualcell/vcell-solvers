#ifndef Voronoi_h
#define Voronoi_h
#include "TPoint.h"
#include "VoronoiResult.h"
#pragma warning ( disable : 4267 4244 )
#include <boost/polygon/voronoi.hpp>
#pragma warning ( default : 4267 )
namespace spatial {
	/**
	* implementation type for boost::polygon::voronoi routines
	*/
	typedef signed int VoronoiType;
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
		typedef GhostPoint<double,2> Ghost2D;
		Voronoi2D(double ghost_distance_,VoronoiType scaleFactor_) 
			:scale(scaleFactor_),
			points(),
			scaledPoints( ),
			dirty(true),
			vd( ),
			ghostDistance(ghost_distance_) {
				scale = scaleFactor_;
		}

		void add(const Point2D &p) {
			points.push_back(p);
			dirty = true;
		}

		void add(double x, double y) {
			add(Point2D(x,y));
		}

		/**
		* set ghost distance in problem domain (input) scale
		*/
		void setGhostDistance(double gd) {
			assert(gd > 0);
			ghostDistance = gd;
		}

		/**
		* remove all points
		*/
		void clear( ) {
			points.clear( );
			scaledPoints.clear( );
			vd.clear( );
			dirty = true;
		}

		const Point2D & cell(size_t index) const {
			return points[index];
		}

		size_t numberPoints( ) const {
			return points.size( );
		}
		//return copy of current points
		std::vector<Point2D> currentPoints( ) const {
			return points;
		}

		size_t numberCells( ) const {
			makeClean( );
			return vd.num_cells( );
		}
		void getVertices(VoronoiResult & result, size_t cellIndex) const;


private:
	VoronoiType scale;
	std::vector<Point2D> points;
	mutable std::vector<VoronoiPoint> scaledPoints;
	mutable bool dirty;
	mutable boost::polygon::voronoi_diagram<double> vd;
	double ghostDistance;

	/**
	* do actual calculation. set dirty to false
	**/
	void calculate( );
	void makeClean( ) const {
		if (dirty) {
			Voronoi2D & self = const_cast<Voronoi2D&>(*this);
			self.calculate( );
		}
	}

	//private, not implemented
	Voronoi2D(const Voronoi2D& other);
	Voronoi2D & operator=(const Voronoi2D& other);

	//convenience typedefs
	typedef boost::polygon::voronoi_diagram<double>::edge_type EdgeType;
	typedef boost::polygon::voronoi_diagram<double>::const_edge_iterator EdgeIterator;
	typedef boost::polygon::voronoi_vertex<double> Vertex;

	/**
	* extract single point from edge at ; actual or ghosted
	* @param results output ; collection to store results in 
	* @param edge to extract vertex from
	* @param anchor pointto extract vertex from; if null, ghost point created
	* @param other vertex at other end of edge, used for ghost building 
	* @return type of point(s) inserted 
	*/
	VoronoiResult::Type extractPoint(std::vector<Ghost2D> & results, const EdgeType & edge, const Vertex * const anchor, const Vertex * const other, bool reverse) const;

	/**
	* return infinite edge, ghosted. numberCells must be 2 
	*/
	void getDoubleInfinite(VoronoiResult & result, size_t cellIndex) const;
	};

	/*
	template <>
	struct Voronoi<double,2> :public Voronoi2D {
		Voronoi(long ghost_distance)
			:Voronoi2D(ghost_distance) {}
	};
	*/

#ifdef UNUSED
	class Voronoi2I {
	protected:
		const static int DEFAULT_GHOST_DIST = 1000;
	public:
		typedef GhostPoint<double,2> Ghost2D;
		Voronoi2I(double ghost_distance_ = DEFAULT_GHOST_DIST)
			:points(),
			dirty(true),
			vd( ),
			ghostDistance(ghost_distance_) {}

		void add(const TPoint<VoronoiType,2> &p) {
			points.push_back(p);
			dirty = true;
		}

		void add(VoronoiType x, VoronoiType y) {
			add(TPoint<VoronoiType,2>(x,y));
		}

		/**
		* set ghost distance in problem domain (input) scale
		*/
		void setGhostDistance(double gd) {
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

		const TPoint<VoronoiType,2> & cell(size_t index) const {
			return points[index];
		}

		size_t numberPoints( ) const {
			return points.size( );
		}
		//return copy of current points
		std::vector<TPoint<VoronoiType,2> > currentPoints( ) const {
			return points;
		}

		size_t numberCells( ) const {
			makeClean( );
			return vd.num_cells( );
		}
		void getVertices(VoronoiResult & result, size_t cellIndex) const;


private:
	typedef GhostPoint<VoronoiType,2> GhostPointType;
	std::vector<VoronoiPoint> points;
	mutable bool dirty;
	mutable boost::polygon::voronoi_diagram<double> vd;
	double ghostDistance;

	/**
	* do actual calculation. set dirty to false
	**/
	void calculate( );
	void makeClean( ) const {
		if (dirty) {
			Voronoi2I & self = const_cast<Voronoi2I&>(*this);
			self.calculate( );
		}
	}

	//private, not implemented
	Voronoi2I(const Voronoi2I& other);
	Voronoi2I & operator=(const Voronoi2I& other);

	//convenience typedefs
	typedef boost::polygon::voronoi_diagram<double>::edge_type EdgeType;
	typedef boost::polygon::voronoi_diagram<double>::const_edge_iterator EdgeIterator;
	typedef boost::polygon::voronoi_vertex<double> Vertex;

	/**
	* extract single point from edge at ; actual or ghosted
	* @param results output ; collection to store results in 
	* @param edge to extract vertex from
	* @param anchor pointto extract vertex from; if null, ghost point created
	* @param other vertex at other end of edge, used for ghost building 
	* @return type of point(s) inserted 
	*/
	VoronoiResult::Type extractPoint(std::vector<Ghost2D> & results, const EdgeType & edge, const Vertex * const anchor, const Vertex * const other, bool reverse) const;

	/**
	* return infinite edge, ghosted. numberCells must be 2 
	*/
	void getDoubleInfinite(VoronoiResult & result, size_t cellIndex) const;
	};
#endif 
}
#endif
