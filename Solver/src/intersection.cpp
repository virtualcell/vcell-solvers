#include <vector>
#include <algorithm>
#include <fstream>
#include "intersection.h"
#include <TPoint.h>
#include <stack_container.h>
//temp
#include <iomanip>

#include "clipper.hpp"
//#define TIMEIT 
#ifdef TIMEIT 
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef max
#endif
namespace {
	using spatial::TPoint;
	using spatial::Volume;
	using spatial::Axis;
	using spatial::cX;
	using spatial::cY;
	
	const int CLIPPER_INTERSECTION_MARGIN = 1000;
	/**
	* functor object for for_each; must use reference instead of contained value because
	* Func is passed by value in for_each
	*/
	template <class T>
	struct MaxValue {
		T & max;
		MaxValue(T &storage ):max(storage) {}

		//default copy constructor works great

		template<int N>
		void operator( )(const TPoint<T,N> &point) {
			for (int i = 0; i < N; i++) {
				const T val =  point(static_cast<Axis>(i));
				const T abs = std::abs(val); 
				max = std::max(max,abs);
			}
		}
	};

	/**
	* scale vcell to clipper 
	*/
	struct VtoCScaler {
		const double scale;
		VtoCScaler(double scale_):scale(scale_){}
		template <class T>
		ClipperLib::IntPoint operator( )(const TPoint<T,2> &point) {
			ClipperLib::long64 x = static_cast<long long>(point(cX) * scale);
			ClipperLib::long64 y = static_cast<long long>(point(cY) * scale);
			return ClipperLib::IntPoint(x,y);
		}
	};

	/**
	* scale clipper to vcell
	*/
	template <class T>
	struct CtoVScaler {
		const double scale;
		CtoVScaler(double scale_):scale(scale_){}
		TPoint<T,2> operator( )(const ClipperLib::IntPoint & point) {
			T x = point.X * scale;
			T y = point.Y * scale;
			return TPoint<T,2>(x,y); 
		}
	};


	/**
	* move results from clipper into Volume 
	* @param result in/out parameter 
	* @param results clipper output 
	* @param inverseScale clipper to problem domain scale factor 
	*/
	template <class REAL>
	void transformClipperResults(Volume<REAL,2> &result, ClipperLib::Polygons & results, double inverseScale) { 
		typedef TPoint<REAL,2> POINT;
		switch (results.size( )){
		case 0:
			{
			result.clear( );
			return;
			}

		case 1:
			{
				ClipperLib::Polygon interSect =  results.front( );
				CtoVScaler<double> inverseScaler(inverseScale);
				std::transform(interSect.begin( ),interSect.end( ),result.fillingIterator(interSect.size( )),inverseScaler);
				result.close( );
				return; 
			}

		default:
			std::vector<std::vector<POINT> > data;
			CtoVScaler<double> inverseScaler(inverseScale);
			for (ClipperLib::Polygons::iterator iter = results.begin( ); iter != results.end( );++iter) {
				ClipperLib::Polygon interSect = *iter;
				result.nextSection( );
				std::vector<POINT> chunk;
				size_t expected = interSect.size( );
				std::transform(interSect.begin( ),interSect.end( ),result.fillingIterator(expected),inverseScaler);
				result.close( );
			}
		}
	}
	/**
	* debug
	*/
	void dump(std::ostream &sink, const char *var, ClipperLib::Polygon &p) {
		for (ClipperLib::Polygon::const_iterator iter = p.begin( );iter != p.end( ); ++iter) {
			sink << '\t' << var << ".push_back(IntPoint(" << iter->X << ',' << iter->Y << "));" << std::endl; 
		}
	}

	//temp
	struct LogItForMe {
		LogItForMe(double scale,double iscale) {
			std::cout << "scale " << std::setprecision(20) << scale << " iscale " << std::setprecision(20) << iscale << std::endl;
		}
	};
}

namespace spatial {
	/**
	* returns intersection of a and b
	* @param result in/out parameter 
	* @param a polygon
	* @param b other polygon
	*/
	template <class REAL,class VECTORA, class VECTORB>
	void intersections(Volume<REAL,2> &result, const VECTORA &a,const VECTORB &b) {
		typedef TPoint<REAL,2> POINT;

		double maxVal = 0;
		MaxValue<double> mv(maxVal);
		std::for_each(a.begin( ),a.end( ),mv);
		std::for_each(b.begin( ),b.end( ),mv);
		ClipperLib::Polygon clipperA(a.size( ));
		ClipperLib::Polygon clipperB(b.size());
		mv.max *= CLIPPER_INTERSECTION_MARGIN;
		const double scale = std::numeric_limits<ClipperLib::long64>::max( ) / mv.max;
		const double inverseScale = mv.max / std::numeric_limits<ClipperLib::long64>::max( );
		static LogItForMe lifm(scale,inverseScale);

		VtoCScaler scaler(scale);
		std::transform(a.begin( ),a.end( ),clipperA.begin( ),scaler);
		std::transform(b.begin( ),b.end( ),clipperB.begin( ),scaler);

		ClipperLib::Clipper c;
		c.AddPolygon(clipperA,ClipperLib::ptSubject);
		c.AddPolygon(clipperB,ClipperLib::ptClip);
		ClipperLib::Polygons results;
#ifdef TIMEIT
		LARGE_INTEGER start;
		LARGE_INTEGER stop;
		QueryPerformanceCounter(&start);
#endif
		c.Execute(ClipperLib::ctIntersection,results,ClipperLib::pftEvenOdd,ClipperLib::pftEvenOdd);
#ifdef TIMEIT
		QueryPerformanceCounter(&stop);
		std::cout << clipperA.size( ) << " by " << clipperB.size( ) << " ticks " << (stop.QuadPart - start.QuadPart) << std::endl;
#endif
		transformClipperResults(result,results,inverseScale);
#ifdef CLIPPER_DUMP
		static int count = 0;
		count++;
		static std::ofstream cDebug("clipperDebug.txt");
		cDebug << count << std::endl; 
		dump(cDebug,"a",clipperA);
		dump(cDebug,"b",clipperB);
		//dump(cDebug,3,results);
#endif
	}

	template <class REAL,class VVA, class VECTORB>
	void intersectionsManySingle(Volume<REAL,2> &result, const VVA &vOfVa,const VECTORB &b) {
		typedef TPoint<REAL,2> POINT;

		double maxVal;
		MaxValue<double> mv(maxVal);
		for (typename VVA::const_iterator iter = vOfVa.begin( ); iter != vOfVa.end( ); ++iter) {
			std::for_each(iter->begin( ),iter->end( ),mv);
		}
		std::for_each(b.begin( ),b.end( ),mv);

		mv.max *= CLIPPER_INTERSECTION_MARGIN;
		const double scale = std::numeric_limits<ClipperLib::long64>::max( ) / mv.max;
		const double inverseScale = mv.max / std::numeric_limits<ClipperLib::long64>::max( );
		VtoCScaler scaler(scale);

		ClipperLib::Clipper c;
		for (typename VVA::const_iterator iter = vOfVa.begin( ); iter != vOfVa.end( ); ++iter) {
			const typename VVA::value_type & vectorA = *iter;
			ClipperLib::Polygon clipperA(vectorA.size( ));
			std::transform(vectorA.begin( ),vectorA.end( ),clipperA.begin( ),scaler);
			c.AddPolygon(clipperA,ClipperLib::ptSubject);
		}

		ClipperLib::Polygon clipperB(b.size());

		std::transform(b.begin( ),b.end( ),clipperB.begin( ),scaler);

		c.AddPolygon(clipperB,ClipperLib::ptClip);
		ClipperLib::Polygons results;
		c.Execute(ClipperLib::ctIntersection,results,ClipperLib::pftEvenOdd,ClipperLib::pftEvenOdd);
		transformClipperResults(result,results,inverseScale);
	}

	template <class REAL,class VVA, class VVB>
	void intersectionsManyMany(Volume<REAL,2> &result, const VVA &vOfVa,const VVB &vOfVb) {
		typedef TPoint<REAL,2> POINT;

		double maxVal;
		MaxValue<double> mv(maxVal);
		for (typename VVA::const_iterator iter = vOfVa.begin( ); iter != vOfVa.end( ); ++iter) {
			std::for_each(iter->begin( ),iter->end( ),mv);
		}
		for (typename VVB::const_iterator iter = vOfVb.begin( ); iter != vOfVb.end( ); ++iter) {
			std::for_each(iter->begin( ),iter->end( ),mv);
		}

		mv.max *= CLIPPER_INTERSECTION_MARGIN;
		const double scale = std::numeric_limits<ClipperLib::long64>::max( ) / mv.max;
		const double inverseScale = mv.max / std::numeric_limits<ClipperLib::long64>::max( );
		VtoCScaler scaler(scale);

		ClipperLib::Clipper c;

		for (typename VVA::const_iterator iter = vOfVa.begin( ); iter != vOfVa.end( ); ++iter) {
			const typename VVA::value_type & vectorA = *iter;
			ClipperLib::Polygon clipperA(vectorA.size( ));
			std::transform(vectorA.begin( ),vectorA.end( ),clipperA.begin( ),scaler);
			c.AddPolygon(clipperA,ClipperLib::ptSubject);
		}

		for (typename VVB::const_iterator iter = vOfVb.begin( ); iter != vOfVb.end( ); ++iter) {
			const typename VVB::value_type & vectorB = *iter;
			ClipperLib::Polygon clipperB(vectorB.size( ));
			std::transform(vectorB.begin( ),vectorB.end( ),clipperB.begin( ),scaler);
			c.AddPolygon(clipperB,ClipperLib::ptSubject);
		}

		ClipperLib::Polygons results;
		c.Execute(ClipperLib::ctIntersection,results,ClipperLib::pftEvenOdd,ClipperLib::pftEvenOdd);
		transformClipperResults(result,results,inverseScale);
	}
}

typedef std::vector <spatial::TPoint<double,2> > PointVector; 
template void spatial::intersections(spatial::Volume<double,2> &result, const PointVector &,const PointVector &) ;

typedef std::vector <spatial::GhostPoint<double,2> > GhostVector; 
template void spatial::intersections(spatial::Volume<double,2> &result, const GhostVector &,const PointVector &) ;


typedef std::vector<spatial::TPoint<double,2>, chromium::StackAllocator<spatial::TPoint<double,2>,spatial::MAX_EXPECTED_VORONOI_POINTS> > VPointVector; 
template void spatial::intersections(spatial::Volume<double,2> &result, const VPointVector &,const PointVector &) ;

typedef std::vector <PointVector> VectorOfVectors;
template void spatial::intersectionsManySingle(spatial::Volume<double,2> &result, const VectorOfVectors &,const PointVector &) ;

template void spatial::intersectionsManyMany(spatial::Volume<double,2> &result, const VectorOfVectors &,const VectorOfVectors &) ;