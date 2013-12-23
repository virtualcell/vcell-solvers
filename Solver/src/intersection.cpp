#include <vector>
#include <algorithm>
#include <fstream>
#include "intersection.h"
#include <TPoint.h>
#include <World.h>
#include <Logger.h>
#include <stack_container.h>
#include <MBridge/MBPolygon.h>
#include <MBridge/FronTierAdapt.h>
//temp
#include <iomanip>

#include "clipper.hpp"
//#define TIMEIT 
#ifdef TIMEIT 
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#undef max
#endif
namespace ClipperLib {
  static cInt const hiRange = 0x3FFFFFFFFFFFFFFFLL;
}
namespace {
	using spatial::TPoint;
	using spatial::Volume;
	using spatial::Axis;
	using spatial::cX;
	using spatial::cY;
	
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
		typedef ClipperLib::cInt cInt;
		typedef ClipperLib::cUInt cUInt;
		const cUInt scale;
		VtoCScaler(cUInt scale_):scale(scale_){}
		template <class T>
		ClipperLib::IntPoint operator( )(const TPoint<T,2> &point) {
			cInt x = static_cast<cInt>(point(cX) * scale);
			cInt y = static_cast<cInt>(point(cY) * scale);
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
	void transformClipperResults(Volume<REAL,2> &result, ClipperLib::Paths & results, double inverseScale) { 
		typedef TPoint<REAL,2> POINT;
		switch (results.size( )){
		case 0:
			{
			result.clear( );
			return;
			}

		case 1:
			{
				ClipperLib::Path interSect =  results.front( );
				CtoVScaler<double> inverseScaler(inverseScale);
				std::transform(interSect.begin( ),interSect.end( ),result.fillingIterator(interSect.size( )),inverseScaler);
				result.close( );
				return; 
			}

		default:
			std::vector<std::vector<POINT> > data;
			CtoVScaler<double> inverseScaler(inverseScale);
			for (ClipperLib::Paths::iterator iter = results.begin( ); iter != results.end( );++iter) {
				ClipperLib::Path interSect = *iter;
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
	void dump(std::ostream &sink, const char *var, ClipperLib::Path &p) {
		for (ClipperLib::Path::const_iterator iter = p.begin( );iter != p.end( ); ++iter) {
			sink << '\t' << var << ".push_back(IntPoint(" << iter->X << ',' << iter->Y << "));" << std::endl; 
		}
	}

	template <class WORLD_TYPE> 
	ClipperLib::cInt calculateWorldScale( ) {
		const spatial::World<WORLD_TYPE,2> & world = spatial::World<WORLD_TYPE,2>::get( );
		ClipperLib::cInt ws = world.scaleFactorFor(ClipperLib::hiRange);
		VCELL_LOG(info,"intersection scaling " << ws);
		//return ws/8; //just because ...
		return ws; 
	}
	template <class WORLD_TYPE> 
	ClipperLib::cInt getWorldScale( ) {
		static ClipperLib::cInt ws = calculateWorldScale<WORLD_TYPE>( ); 
		return ws;
	}

	void copyInto(const ClipperLib::Path &source,matlabBridge::Polygon & dest) {
		for (ClipperLib::Path::const_iterator iter = source.begin( ); iter != source.end( ); ++iter) {
			dest.add(static_cast<double>(iter->X),static_cast<double>(iter->Y));
		}
	}

	void AddPath(ClipperLib::Clipper &c, ClipperLib::Path &path, ClipperLib::PolyType pt) {
		const bool r = c.AddPath(path,pt,true);
		if (r) {
			return;
		}
		VCELL_EXCEPTION(logic_error,"bad clipper add");
	}
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
		static ClipperLib::cInt scale = getWorldScale<REAL>( );
		static long double inverseScale = 1.0L/scale; 

		ClipperLib::Path clipperA(a.size( ));
		ClipperLib::Path clipperB(b.size());
		VtoCScaler scaler(scale);
		std::transform(a.begin( ),a.end( ),clipperA.begin( ),scaler);
		std::transform(b.begin( ),b.end( ),clipperB.begin( ),scaler);
		matlabBridge::Polygon pa("-g",2);
		copyInto(clipperA,pa);
		matlabBridge::Polygon pb("-b",2);
		copyInto(clipperB,pb);
		std::ofstream id("idebug.m");
		id << pa << pb;
		std::ofstream code("frag.txt");
		dump(code,"polyA",clipperA);
		dump(code,"polyB",clipperB);
		

		ClipperLib::Clipper c;
		AddPath(c,clipperA,ClipperLib::ptSubject);
		AddPath(c,clipperB,ClipperLib::ptClip);
		ClipperLib::Paths results;
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
		static ClipperLib::cInt scale = getWorldScale<REAL>( );
		static long double inverseScale = 1.0L/scale; 
		VtoCScaler scaler(scale);

		ClipperLib::Clipper c;
		for (typename VVA::const_iterator iter = vOfVa.begin( ); iter != vOfVa.end( ); ++iter) {
			const typename VVA::value_type & vectorA = *iter;
			ClipperLib::Path clipperA(vectorA.size( ));
			std::transform(vectorA.begin( ),vectorA.end( ),clipperA.begin( ),scaler);
			AddPath(c,clipperA,ClipperLib::ptSubject);
		}

		ClipperLib::Path clipperB(b.size());

		std::transform(b.begin( ),b.end( ),clipperB.begin( ),scaler);

		AddPath(c,clipperB,ClipperLib::ptClip);
		ClipperLib::Paths results;
		c.Execute(ClipperLib::ctIntersection,results,ClipperLib::pftEvenOdd,ClipperLib::pftEvenOdd);
		transformClipperResults(result,results,inverseScale);
	}

	template <class REAL,class VVA, class VVB>
	void intersectionsManyMany(Volume<REAL,2> &result, const VVA &vOfVa,const VVB &vOfVb) {
		static ClipperLib::cInt scale = getWorldScale<REAL>( );
		static long double inverseScale = 1.0L/scale; 
		//typedef TPoint<REAL,2> POINT;

		VtoCScaler scaler(scale);

		ClipperLib::Clipper c;

		for (typename VVA::const_iterator iter = vOfVa.begin( ); iter != vOfVa.end( ); ++iter) {
			const typename VVA::value_type & vectorA = *iter;
			ClipperLib::Path clipperA(vectorA.size( ));
			std::transform(vectorA.begin( ),vectorA.end( ),clipperA.begin( ),scaler);
			AddPath(c,clipperA,ClipperLib::ptSubject);
		}

		for (typename VVB::const_iterator iter = vOfVb.begin( ); iter != vOfVb.end( ); ++iter) {
			const typename VVB::value_type & vectorB = *iter;
			ClipperLib::Path clipperB(vectorB.size( ));
			std::transform(vectorB.begin( ),vectorB.end( ),clipperB.begin( ),scaler);
			AddPath(c,clipperB,ClipperLib::ptSubject);
		}

		ClipperLib::Paths results;
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