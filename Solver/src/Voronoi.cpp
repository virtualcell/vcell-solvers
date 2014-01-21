#include <iostream>
#include <cassert>

#include <vector>
#include <algorithm>
#include <fstream>
#include "Voronoi.h"
#include "SVector.h"
#include "algo.h"
using boost::polygon::voronoi_diagram;
using namespace spatial;
typedef SVector<VoronoiType,2> DVector;

namespace {
	const int SCALE_MARGIN = 100000; 
	typedef Voronoi2D::ImplementationType vdVertexType;
	const vdVertexType epilson = 0.001;
	inline VoronoiType fixDriftFromInt( vdVertexType in) {
		double junk;
		if (std::modf(in,&junk) == 0) { //no fractional part
			return static_cast<VoronoiType>(in);
		}
		if (in > 0) {
			return static_cast<VoronoiType>(in + epilson);
		}
		return static_cast<VoronoiType>(in - epilson);
	}
	struct ConvertingGhostPoint : public VoronoiGhostPoint {
		ConvertingGhostPoint(vdVertexType x, vdVertexType y, bool isGhost = false) 
			:VoronoiGhostPoint(fixDriftFromInt(x), fixDriftFromInt(y),isGhost) {}
	};
}

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
	const VoronoiPoint & cellPoint = points[srcIdx]; 
	const VoronoiPoint & otherCellPoint = points[otherIdx]; 
	const DVector direction(cellPoint,otherCellPoint);
	DVector perpendicular = direction.perpendicular( ); 
	VoronoiGhostPoint center = midPoint(cellPoint,otherCellPoint);
	VoronoiGhostPoint ghost1 = ghostTo(center,perpendicular);
	ghost1.setGhost(true);
	result.vertices.push_back(ghost1);
	result.vertices.push_back(center);

	perpendicular.reverse( );
	VoronoiGhostPoint ghost2 = ghostTo(center,perpendicular);
	ghost2.setGhost(true);
	result.vertices.push_back(ghost2);
}

#pragma warning ( disable : 4244 ) //converting doubles to long
VoronoiResult::Type Voronoi2D::extractPoint(std::vector<VoronoiGhostPoint> &results,const EdgeType & edge, const Vertex * const anchor, const Vertex * const otherVertex, bool reverse) const {
	assert(!dirty);
	static VoronoiType debugLimit = 100; 
	if (anchor != nullptr) {
		ConvertingGhostPoint t(anchor->x( ),anchor->y( ));
		results.push_back(t);
		return VoronoiResult::closed;
	}
	else  { //infinite point, make ghost point
		const size_t srcIdx = edge.cell( )->source_index( );
		const size_t otherIdx = edge.twin( )->cell( )->source_index( );
		const VoronoiPoint & gridPoint = this->points[srcIdx]; 
		const VoronoiPoint & otherGridPoint = points[otherIdx]; 
		const DVector direction(gridPoint,otherGridPoint);
		//const VoronoiPoint mid = spatial::midPoint(gridPoint,otherGridPoint);
		DVector perpendicular = direction.perpendicular( ); 
		const VoronoiPoint center = midPoint(gridPoint,otherGridPoint);
		if (otherVertex != nullptr) { //single ghost, anchored at real point
			ConvertingGhostPoint vertexPoint(otherVertex->x( ),otherVertex->y( ));
			if (reverse) {
				perpendicular.reverse( );
			}
			VoronoiGhostPoint ghost = ghostTo(center,perpendicular);
			ghost.setGhost(true);
			results.push_back(ghost);
			return VoronoiResult::singleOpen; 
		}
		else { //double ghost
			VoronoiGhostPoint ghost1 = ghostTo(center,perpendicular);
			VoronoiGhostPoint ghost2 = ghostTo(center,perpendicular.reverse( ));
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

	vd.clear( );
	boost::polygon::construct_voronoi(points.begin( ),points.end( ),&vd);
	dirty = false;
	const bool dumpDebugOutput = true;
	if (!dumpDebugOutput) {
		return;
	}

	{
		std::ofstream dump("vpoint.txt");
		for (std::vector<VoronoiPoint>::iterator iter = points.begin( ); iter != points.end( );++iter) {
			dump <<  iter->get(spatial::cX) << ',' << iter->get(spatial::cY) << std::endl;
		}
	}

	{
		typedef boost::polygon::voronoi_diagram<ImplementationType>::vertex_container_type VCT;
		std::ofstream dump("vdpoint.txt");
		for (VCT::const_iterator iter = vd.vertices( ).begin( ); iter != vd.vertices( ).end( ); ++iter) {
			dump << iter->x( ) << ',' << iter->y( ) << ',' << iter->is_degenerate( ) << std::endl;
		}
	}

	{
		typedef boost::polygon::voronoi_diagram<ImplementationType>::cell_container_type CCT;
		std::ofstream dump("cell.txt");
		for (CCT::const_iterator iter = vd.cells( ).begin( ); iter != vd.cells( ).end( ); ++iter) {
			dump << iter->source_index( )  << ',' << iter->contains_point( ) << ',' << iter->contains_segment( )
				<< ',' << iter->source_category( ) << std::endl;
		}
	}

	{
		typedef boost::polygon::voronoi_diagram<ImplementationType>::edge_container_type ECT;
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

double Voronoi2D::toWall(const VoronoiPoint & origin, const NormVector<double,2> norm, const Axis a) const { 
	VoronoiType wall = (norm(a) < 0) ? limits[a].low( ) : limits[a].high( );
	const double toWallDist = (wall - origin(a) ) / norm(a);
	return toWallDist;
}

VoronoiGhostPoint Voronoi2D::ghostTo(const VoronoiPoint & origin, const spatial::SVector<VoronoiType,2> & direction) const {
	NormVector<double,2> norm(direction.convert<double>( ));
	double length = toWall(origin,norm,cX); 
	double ylength = toWall(origin,norm,cY); 
	length = std::min(length,ylength);

	VoronoiType x = origin(cX) + static_cast<VoronoiType>(norm(cX) * length); 
	VoronoiType y = origin(cY) + static_cast<VoronoiType>(norm(cY) * length); 
	return VoronoiGhostPoint(x,y,true);
}