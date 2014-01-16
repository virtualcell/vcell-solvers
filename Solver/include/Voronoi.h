#ifndef Voronoi_h
#define Voronoi_h
#include <TPoint.h>
#include <VoronoiResult.h>
#include <GeoLimit.h>
#include <SVector.h>
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
	};


	class Voronoi2D {
	public:
		typedef TPoint<VoronoiType, 2> VoronoiPoint;
		Voronoi2D(const std::array<TGeoLimit<VoronoiType> ,2> & limits_)
			:points(),
			dirty(true),
			vd( ),
			limits(limits_) {
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
	const std::array<TGeoLimit<VoronoiType> ,2> limits;

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

	/**
	* return ghost point going to edge of limits
	*/
	VoronoiGhostPoint ghostTo(const VoronoiPoint & origin, const spatial::SVector<VoronoiType,2> & direction) const; 
	/**
	* distance to wall
	* @param origin starting point 
	* @param norm direction  
	* @param a axis  
	*/
	double toWall(const VoronoiPoint & origin, const NormVector<double,2> norm, const Axis a) const; 
	};

}
#endif
