#include <cassert>
#include <iostream>
#include "SVector.h"
#include "NormVector.h"
#include "TPoint.h"
#include "algo.h"

//using spatial::Slope; 
using spatial::TPoint; 
using spatial::cX; 
using spatial::cY; 
using std::cout;
using std::endl;

template <class T,int N>
TPoint<T,N> spatial::pointThrough(const spatial::TPoint<T,N> & origin, const spatial::NormVector<T,N> & direction, T distance) {
	std::array<T,N> coords;
	for (size_t i = 0; i < N; i++) {
		const Axis a = static_cast<Axis>(i);
		coords[i] = origin(a) + direction(a) * distance;
	}
	return TPoint<T,N>(coords);
}

template <class T,int N>
TPoint<T,N> spatial::pointThrough(const spatial::TPoint<T,N> & origin, const spatial::SVector<T,N> & direction, T distance) {
	NormVector<T,N> norm(direction);
	return spatial::pointThrough(origin,norm,distance);
}


template <class T>
bool spatial::below(const TPoint<T,2> & point, const TPoint<T,2> & a, const TPoint<T,2> & b) {
	//static_assert(T::numDim( ) == 2,"for 2d only");
	using spatial::cX;
	using spatial::cY;
	//sort out which is left and which is right
	const bool swapped = a(cX) > b(cX); 
	const TPoint<T,2> & lhs = swapped ? b : a; 
	const TPoint<T,2> & rhs = swapped ? a : b; 

	T px = point(cX);
	if ( (px < lhs(cX) ) || (px > rhs(cX) ) ) {
		return false;
	}
	T py = point(cY);
	T leftY = lhs(cY);
	T rightY = rhs(cY);
	if ( (py < leftY) && (py < rightY) ) {
		return true;
	}
	if ( (py > leftY) && (py > rightY) ) {
		return false;
	}
	T leftPointX = px - lhs(cX); 
	T leftPointY = py - lhs(cY); 
	T leftPointSlope = leftPointY  / leftPointX;   

	T edgeX = rhs(cX) - lhs(cX); 
	T edgeY = rhs(cY) - lhs(cY); 
	T edgeSlope = edgeY / edgeX; 

	return leftPointSlope < edgeSlope;
}

template <class TPOINT>
bool spatial::inside(const std::vector<TPOINT> &  polygon, const TPOINT & point) {
	typedef const TPOINT PT;
	if (polygon.size( ) < 3) {
		return false;
	}
	//static_assert(point.numDim( ) == 2,"for 2d only");
	int below = 0;
	typename std::vector<TPOINT>::const_iterator iter = polygon.begin( );
	typename std::vector<TPOINT>::const_iterator end = polygon.end( );
	PT * a = &(*iter);
	++iter;
	PT * b = &(*iter);
	for (;;) {
		if (spatial::below(point,*a,*b)) {
			if ( !(point(cX) == a->get(cX) && point(cX) == b->get(cX) ) ) { //check for under vertical line
				below++;
			}
		}
		a = b;
		++iter;
		if (iter == end) {
			break;
		}
		b = &(*iter);
	}; 

	PT & first = polygon.front( );
	if (spatial::below(point,*a,first)) {
		if ( !(point(cX) == a->get(cX) && point(cX) == first(cX) ) ) { //check for under vertical line
			below++;
		}
	}
	//point is inside if it's below an odd number of edges
	return below%2 == 1;
}


template <class T>
bool spatial::between(const TPoint<T,2> & origin, const SVector<T,2> & direction,const TPoint<T,2> & vertex, const TPoint<T,2> & otherVertex) {
	SVector<T,2>  vertexVector(vertex,otherVertex);
	std::pair<bool,T> w = intersectParameter(origin,direction,vertex,vertexVector);
	if (w.first) {
		return w.second >= 0 && w.second <= 1;
	}
	return false;
}


namespace I1{
	typedef double IType; //"instantiation type"
	template bool spatial::below(const spatial::TPoint<IType,2> &, const spatial::TPoint<IType,2> &, const spatial::TPoint<IType,2> &);
	template bool spatial::inside(const std::vector<TPoint<IType,2> > &  polygon, const TPoint<IType,2> & point);
}

namespace I2{
	typedef int IType; //"instantiation type"
	typedef spatial::TPoint<IType,2> PType;
	template bool spatial::below(const spatial::TPoint<IType,2> &, const spatial::TPoint<IType,2> &, const spatial::TPoint<IType,2> &);
	template bool spatial::inside(const std::vector<TPoint<IType,2> > &  polygon, const TPoint<IType,2> & point);
	template PType spatial::pointThrough(const PType & origin, const spatial::SVector<IType,2> & direction, IType distance); 
}

typedef spatial::TPoint<double,2> LPoint;
template LPoint spatial::pointThrough(const LPoint & origin, const spatial::SVector<double,2> & direction, double distance); 
template bool spatial::between(const LPoint & origin, const SVector<double,2> & direction,const LPoint & vertex, const LPoint & otherVertex); 
