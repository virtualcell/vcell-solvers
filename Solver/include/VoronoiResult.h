#ifndef VoronoiResult_h
#define VoronoiResult_h
#include <vector>
#include <MovingBoundaryTypes.h>
#include <TPoint.h>
namespace spatial {
	/**
	* implementation type for boost::polygon::voronoi routines
	*/
	typedef signed int VoronoiType;

	typedef GhostPoint<VoronoiType,2> VoronoiGhostPoint;

	/**
	* POD
	* voronoi result types
	* closed - simple closed polygon
	* straightLine - open polygon with two infinite edges - marked as ghost points and 
	*	at ends of vertices
	* singleOpen - open polygon with two infinite edges - marked as ghost points and 
	*	at ends of vertices
	* multiOpen - multiple segments with infinite edges; in vertices in sets of
	*	three with midpoint non-ghost and ends ghost points
	*/
	struct VoronoiResult {
		enum Type {closed,straightLine,singleOpen, multiOpen};
		VoronoiResult(Type t = closed)
			:type(t) {}
		Type type;
		//OPT: include space for allocation in structure
		std::vector<VoronoiGhostPoint> vertices; 
	};
}

#endif
