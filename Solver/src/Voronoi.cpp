#include <iostream>
#include <cassert>

#include <vector>
#include <algorithm>
#include <fstream>
#include "Voronoi.h"
#include "SVector.h"
#include "algo.h"
namespace {
	const int SCALE_MARGIN = 100000; 
}
using boost::polygon::voronoi_diagram;
using namespace spatial;
typedef SVector<double,2> DVector;

template <class T>
std::ostream &operator<<(std::ostream & os, const boost::polygon::voronoi_vertex<T> &vertex) {
	os << '[' << vertex.x( ) <<  ',' << vertex.y( ) << ']'; 
	return os;
}
std::ostream &operator<<(std::ostream & os, const boost::polygon::voronoi_vertex<double> *vertex) {
	if (vertex != nullptr) {
		os << '[' << vertex->x( ) <<  ',' << vertex->y( ) << ']'; 
	}
	else {
		os << "infinite";
	}
	return os;
}

void Voronoi2D::getDoubleInfinite(VoronoiResult & result,size_t cellIndex ) const{
	result.type = VoronoiResult::straightLine;
	assert(result.vertices.size( ) == 0);
	assert(!dirty);
	assert (numberPoints( ) == 2);
	assert(vd.num_edges( ) == 2);
	assert(vd.num_cells( ) > cellIndex);
	const boost::polygon::voronoi_cell<double> & cell = vd.cells( )[cellIndex];
	const EdgeType * const edge =  cell.incident_edge( );
	const size_t srcIdx = edge->cell( )->source_index( );
	const size_t otherIdx = edge->twin( )->cell( )->source_index( );
	const Ghost2D & cellPoint = points[srcIdx]; 
	const Ghost2D & otherCellPoint = points[otherIdx]; 
	const DVector direction(cellPoint,otherCellPoint);
	DVector perpendicular = direction.perpendicular( ); 
	Ghost2D center = midPoint(cellPoint,otherCellPoint);
	Ghost2D ghost1 = pointThrough<double,2>(center,perpendicular,ghostDistance);
	ghost1.setGhost(true);
	result.vertices.push_back(ghost1);
	result.vertices.push_back(center);

	perpendicular.reverse( );
	Ghost2D ghost2 = pointThrough<double,2>(center,perpendicular,ghostDistance);
	ghost2.setGhost(true);
	result.vertices.push_back(ghost2);
}

#pragma warning ( disable : 4244 ) //converting doubles to long
VoronoiResult::Type Voronoi2D::extractPoint(std::vector<Voronoi2D::Ghost2D> &results,const EdgeType & edge, const Vertex * const anchor, const Vertex * const otherVertex, bool reverse) const {
	assert(!dirty);
	static VoronoiType debugLimit = 100; 
	const double invScale = 1.0 / scale;
	if (anchor != nullptr) {
		Ghost2D t(anchor->x( ),anchor->y( ), invScale,false);
		if (t(cX) *invScale > debugLimit) {
			std::cout << "debug limit" << std::endl;
		}
		results.push_back(t);
		return VoronoiResult::closed;
	}
	else  { //infinite point, make ghost point
		const size_t srcIdx = edge.cell( )->source_index( );
		const size_t otherIdx = edge.twin( )->cell( )->source_index( );
		const Ghost2D & gridPoint = points[srcIdx]; 
		const Ghost2D & otherGridPoint = points[otherIdx]; 
		const DVector direction(gridPoint,otherGridPoint);
		const Point2D mid = spatial::midPoint(gridPoint,otherGridPoint);
		DVector perpendicular = direction.perpendicular( ); 
		const Point2D center = midPoint(gridPoint,otherGridPoint);
		if (otherVertex != nullptr) { //single ghost, anchored at real point
			Ghost2D vertexPoint(otherVertex->x( ),otherVertex->y( ), invScale,false);
			if (reverse) {
				perpendicular.reverse( );
			}
			Ghost2D ghost = pointThrough<double,2>(center,perpendicular,ghostDistance);
			ghost.setGhost(true);
			results.push_back(ghost);
			return VoronoiResult::singleOpen; 
		}
		else { //double ghost
			Ghost2D ghost1 = pointThrough<double,2>(center,perpendicular,ghostDistance);
			Ghost2D ghost2 = pointThrough<double,2>(center,perpendicular,-ghostDistance);
			ghost1.setGhost(true);
			ghost2.setGhost(true);
			results.push_back(ghost1);
			results.push_back(center);
			results.push_back(ghost2);
			return VoronoiResult::straightLine;
		}
	}
}

void Voronoi2D::getVertices(VoronoiResult & result, size_t cellIndex ) const{
	assert(result.vertices.size( ) == 0);
	assert(cellIndex >= 0);
	assert(cellIndex < numberCells( )); 
	makeClean( );
	size_t nPoints = numberPoints( );
	switch (nPoints) {
	case 1:
		throw std::invalid_argument("must have more than one point for voronoi");
	case 2:
		getDoubleInfinite(result,cellIndex); 
		return;
	default:
		{
			result.type = VoronoiResult::closed;
			//boost::polygon::voronoi does not preserve original indexes, so search for correct cell
			boost::polygon::voronoi_diagram<double>::cell_container_type::const_iterator iter = vd.cells( ).begin( );
			for (;iter != vd.cells( ).end( ); ++iter) {
				if (iter->source_index( ) == cellIndex) {
					break;
				}
			}
			assert(iter != vd.cells( ).end( ));
			const boost::polygon::voronoi_cell<double> & cell = *iter;
			const EdgeType * const incident =  cell.incident_edge( );
			assert(incident != nullptr);
			const EdgeType * edge = incident;
			//want to start list of vertices with ghost point (null) vertex, if one exists
			do {
				if (edge->vertex0( ) == nullptr) {
					break; //infinite edge, start here
				}
				edge = edge->next( );
			}
			while (edge != incident); //closed, so start at arbitrary point

			const EdgeType * const start = edge;
			const EdgeType * last; 
			do {
				const Vertex * first = edge->vertex0( );
				const Vertex * second = edge->vertex1( );
				//std::cout << edge->is_primary( ) << ' ' << first << " , " << second << std::endl;
				VoronoiResult::Type t = extractPoint(result.vertices,*edge,first,second, false);
				switch (t) {
				case VoronoiResult::closed:
					break;
				case VoronoiResult::straightLine:
					if (result.vertices.size( ) == 3) {
						result.type = VoronoiResult::straightLine;
					}
					else { //if we have multiple straight lines that's "multiOpen"
						result.type = VoronoiResult::multiOpen; 
					}
					break;
				case VoronoiResult::singleOpen:
				case VoronoiResult::multiOpen:
					result.type = std::max(result.type,t);
					break;
				default:
					throw std::invalid_argument("bad VoronoiResult::Type in switch");
				}
				last = edge;
				edge = edge->next( );
			}
			while (edge != start); //only go around once
			//catch last point if open
			if (result.type == VoronoiResult::singleOpen) {
				const Vertex * first = last->vertex0( );
				const Vertex * second = last->vertex1( );
				extractPoint(result.vertices,*last,second,first,true);
			}
			//if we have multiple open we should have one or two sets of three points 
			assert(result.type != VoronoiResult::multiOpen || result.vertices.size( ) == 3 || result.vertices.size( ) == 6);;
		}
	}
}
#pragma warning ( default : 4244 )

void  Voronoi2D::calculate( ) {
	assert(dirty);

	scaledPoints.clear( );
	scaledPoints.reserve(points.size( ));
	for (std::vector<Point2D>::iterator iter = points.begin( ); iter != points.end( );++iter) {
		scaledPoints.push_back(VoronoiPoint(*iter,scale));
	}
	vd.clear( );
	boost::polygon::construct_voronoi(scaledPoints.begin( ),scaledPoints.end( ),&vd);
	dirty = false;
	const bool dumpDebugOutput = true;
	if (!dumpDebugOutput) {
		return;
	}

	{
		std::ofstream dump("vpoint.txt");
		for (std::vector<VoronoiPoint>::iterator iter = scaledPoints.begin( ); iter != scaledPoints.end( );++iter) {
			dump <<  iter->get(spatial::cX) << ',' << iter->get(spatial::cY) << std::endl;
		}
	}

	{
		typedef boost::polygon::voronoi_diagram<double>::vertex_container_type VCT;
		std::ofstream dump("vdpoint.txt");
		for (VCT::const_iterator iter = vd.vertices( ).begin( ); iter != vd.vertices( ).end( ); ++iter) {
			dump << iter->x( ) << ',' << iter->y( ) << ',' << iter->is_degenerate( ) << std::endl;
		}
	}

	{
		typedef boost::polygon::voronoi_diagram<double>::cell_container_type CCT;
		std::ofstream dump("cell.txt");
		for (CCT::const_iterator iter = vd.cells( ).begin( ); iter != vd.cells( ).end( ); ++iter) {
			dump << iter->source_index( )  << ',' << iter->contains_point( ) << ',' << iter->contains_segment( )
				<< ',' << iter->source_category( ) << std::endl;
		}
	}

	{
		typedef boost::polygon::voronoi_diagram<double>::edge_container_type ECT;
		std::ofstream dump("edge.txt");
		for (ECT::const_iterator iter = vd.edges( ).begin( ); iter != vd.edges( ).end( ); ++iter) {
			const Vertex *v0 = iter->vertex0( );
			const Vertex *v1 = iter->vertex1( );
			if (v0 != nullptr && v1 != nullptr) {
				dump << v0->x( ) << ',' << v0->y( ) << ',' << v1->x( ) << ',' << v1->y( ) << std::endl;
			}
		}
	}
}
