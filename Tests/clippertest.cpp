#include <iostream>
#include <fstream>
#include <cassert>

#include <vector>
#include <algorithm>
#include <MBridge/MBPolygon.h>
#include <MBridge/Figure.h>
#include <MBridge/FronTierAdapt.h>
#include "gtest/gtest.h"
#include "clipper.hpp"
#include "Angle.h"
#include "intersection.h"

template <class T>
void populate(matlabBridge::TPolygon<T> & mpoly, const ClipperLib::Polygon & poly) {
	typedef ClipperLib::Polygon cprPgon;
	for (cprPgon::const_iterator iter = poly.begin( );iter != poly.end( ); ++iter) {
		//mpoly.add(static_cast<double>(iter->X),static_cast<double>(iter->Y));
		mpoly.add(iter->X,iter->Y);
	}
}

ClipperLib::Polygon build(int count, long long *x,long long *y) {
	ClipperLib::Polygon rval;
	for (int i = 0; i < count; i++) {
		rval.push_back(ClipperLib::IntPoint(x[i],y[i]));
	}
	//rval.push_back(ClipperLib::IntPoint(x[0],y[0]));
	return rval;
}

TEST(clipper,largenum){
	using namespace ClipperLib;
	using ClipperLib::Polygon;
	std::cout << "max value is " << std::numeric_limits<long long>::max( ) << std::endl;
	/*
	Polygon1.x = [4387414506071, 4157630003224, 3891577094948, 3591914093955, 3261635133897, -3870280929772]
	Polygon1.y = [2108535817006, 2483323859773, 2833299351411, 3154965452493, 3445108181668, -440]

	Polygon2.x = [5717460464435, 3518437208883, 3518437208883, 5717460464435]
	Polygon2.y = [4398046511104, 4398046511104, 2199023255552, 2199023255552]
	*/
	long64 x1[] = {4387414506071, 4157630003224, 3891577094948, 3591914093955, 3261635133897, -3870280929772};
	long64 y1[] = {2108535817006, 2483323859773, 2833299351411, 3154965452493, 3445108181668, -440};

	long64 x2[] =  {5717460464435, 3518437208883, 3518437208883, 5717460464435};
	long64 y2[]  = {4398046511104, 4398046511104, 2199023255552, 2199023255552};
	ClipperLib::Polygon p1 = build(sizeof(x1)/sizeof(x1[0]),x1,y1);
	ClipperLib::Polygon p2 = build(sizeof(x2)/sizeof(x2[0]),x2,y2);
	std::reverse(p2.begin( ), p2.end( ));

	CleanPolygon(p1,p1);
	CleanPolygon(p2,p2);

	Clipper c;
	c.AddPolygon(p1,ptSubject);
	c.AddPolygon(p2,ptClip);
	Polygons results;
	/*
	PolyFillType types[] = {pftPositive,pftNegative,pftNegative,pftEvenOdd};
	const int ntypes = sizeof(types)/sizeof(types[0]);
	for (int i = 0 ; i <ntypes;i++)  {
	for (int j = 0 ; j <ntypes;j++) {
	const bool r = c.Execute(ctIntersection ,results,types[i],types[j]);
	std::cout << "large " << r << ',' << results.size( ) << " i is " << i << ", j is " << j << std::endl;
	}
	}
	*/
	const bool r = c.Execute(ctIntersection ,results,pftEvenOdd,pftEvenOdd);
	std::ofstream script("clipperLarge.m");
	matlabBridge::TPolygon<long long> b1("g+-");
	matlabBridge::TPolygon<long long> b2("b+-");
	populate(b1,p1);
	populate(b2,p2);
	script << matlabBridge::FigureName("clipperLarge") << b1 << b2;
	for (Polygons::const_iterator iter = results.begin( ); iter != results.end( );++iter) {
		matlabBridge::TPolygon<long long> a("r+-");
		populate(a,*iter);
		script << a;
	}
	//reply from Clipper developer
	/*
	ClipperLib::Polygon a;
	a.push_back(IntPoint(3518437208883, 2199023255552));
	a.push_back(IntPoint(3518437208883, 3219513263720));
	a.push_back(IntPoint(3261635133897, 3445108181668));
	a.push_back(IntPoint(682043251292, 2199023255552));
	matlabBridge::TPolygon<long long> b3("r+-");
	populate(b3,a);
	script << b3;
	*/
}

TEST(clipper,small){
	using namespace ClipperLib;
	/*
	Polygon1.x = [4387414506071, 4157630003224, 3891577094948, 3591914093955, 3261635133897, -3870280929772]
	Polygon1.y = [2108535817006, 2483323859773, 2833299351411, 3154965452493, 3445108181668, -440]

	Polygon2.x = [5717460464435, 3518437208883, 3518437208883, 5717460464435]
	Polygon2.y = [4398046511104, 4398046511104, 2199023255552, 2199023255552]
	*/
	long64 x1[] = {0,5,5,0};
	long64 y1[] = {0,0,5,5};

	long64 x2[] =  {-1,6,6,-1};
	long64 y2[]  = {2,2,4,4};
	ClipperLib::Polygon p1 = build(sizeof(x1)/sizeof(x1[0]),x1,y1);
	ClipperLib::Polygon p2 = build(sizeof(x2)/sizeof(x2[0]),x2,y2);

	Clipper c;
	c.AddPolygon(p1,ptSubject);
	c.AddPolygon(p2,ptClip);
	Polygons results;
	ClipperLib::Polygon answer;
	PolyFillType types[] = {pftPositive,pftNegative,pftNegative,pftEvenOdd};
	const int ntypes = sizeof(types)/sizeof(types[0]);
	for (int i = 0 ; i <ntypes;i++)  {
		for (int j = 0 ; j <ntypes;j++) {
			const bool r = c.Execute(ctIntersection ,results,types[i],types[j]);
			if (results.size( ) > 0) {
				std::cout << r << ',' << results.size( ) << "i is " << i << ", j is " << j << std::endl;
				answer = results.front( );
			}
		}
	}
	std::ofstream script("clippersmall.m");
	matlabBridge::TPolygon<long long> b1("g");
	matlabBridge::TPolygon<long long> b2("b");
	matlabBridge::TPolygon<long long> a("r+");
	populate(b1,p1);
	populate(b2,p2);
	populate(a,answer);
	script << matlabBridge::FigureName("clipperFail") << b1 << b2 << a << std::flush;
}
TEST(clipper,request){
	using namespace ClipperLib;
	long long x1[] = {4721014781795280,    4721014781795280,    5438484791397062,    5438484791397062};
	long long y1[] = {1947418597490553,    1879088120385621,    1879088120385621,   1947418597490551};
	long long x2[] = {4814313922692160,     4800114314395577,     4785679378820044,     4771009946699843,     4710004338769085,    -3792559502082542};
	long long y2[] = {1856220285199938,     1887294857627051,     1918260815746161,     1949116377458356,     2071399242770190,              -450359};

	ClipperLib::Polygon p1 = build(sizeof(x1)/sizeof(x1[0]),x1,y1);
	ClipperLib::Polygon p2 = build(sizeof(x2)/sizeof(x2[0]),x2,y2);

	Clipper c;
	c.AddPolygon(p1,ptSubject);
	c.AddPolygon(p2,ptClip);
	Polygons results;
	ClipperLib::Polygon answer;
	PolyFillType types[] = {pftPositive,pftNegative,pftNegative,pftEvenOdd};
	const bool r = c.Execute(ctIntersection ,results,pftEvenOdd,pftEvenOdd);
	assert(r);
	typedef ClipperLib::Polygon::const_iterator PIter;
	if (results.size( ) > 0) {
		answer = results.front( );
		std::ofstream log("results.txt");
		for (PIter iter = answer.begin( ); iter != answer.end( ); ++iter) {
			log << '(' << iter->X << ',' << iter->Y << ") " << std::endl;
		}
	}
	std::ofstream script("clipperReq.m");
	matlabBridge::TPolygon<long long> b1("g+-");
	matlabBridge::TPolygon<long long> b2("b+-");
	matlabBridge::TPolygon<long long> a("r");
	populate(b1,p1);
	populate(b2,p2);
	populate(a,answer);
	b1.close( );
	b2.close( );
	a.close( );
	script << matlabBridge::FigureName("clipperReq") << b1 << b2 << a << std::flush;
}

TEST(clipper,miss){
	using namespace ClipperLib;
	long64 x1[] = {0,5,5,0};
	long64 y1[] = {0,0,5,5};

	long64 x2[] =  {14,16,16,14};
	long64 y2[]  = {2,2,4,4};
	ClipperLib::Polygon p1 = build(sizeof(x1)/sizeof(x1[0]),x1,y1);
	ClipperLib::Polygon p2 = build(sizeof(x2)/sizeof(x2[0]),x2,y2);
	Clipper c;


	c.AddPolygon(p1,ptSubject);
	c.AddPolygon(p2,ptClip);
	Polygons results;
	ClipperLib::Polygon answer;
	PolyFillType types[] = {pftPositive,pftNegative,pftNegative,pftEvenOdd};
	const int ntypes = sizeof(types)/sizeof(types[0]);
	for (int i = 0 ; i <ntypes;i++)  {
		for (int j = 0 ; j <ntypes;j++) {
			const bool r = c.Execute(ctIntersection ,results,types[i],types[j]);
			if (results.size( ) > 0) {
				std::cout << r << ',' << results.size( ) << " i is " << i << ", j is " << j << std::endl;
				answer = results.front( );

			}
		}
	}
	std::ofstream script("clippersmall.m");
	matlabBridge::TPolygon<long long> b1("g");
	matlabBridge::TPolygon<long long> b2("b");
	matlabBridge::TPolygon<long long> a("r+");
	populate(b1,p1);
	populate(b2,p2);
	populate(a,answer);
	script << matlabBridge::FigureName("clipperFail") << b1 << b2 << a << std::flush;
}

namespace {
	using spatial::Point2D;
	using spatial::Angle;

	std::vector<Point2D> ellipse(double centerX, double centerY, double a, double b, const spatial::Angle & angleOfMajor, 
		int increment = 10) {
			std::vector<Point2D> rval; 
			const double axisCos = angleOfMajor.cos( );
			const double axisSin = angleOfMajor.sin( );
			for (int d = 0; d < 360; d+=increment) { 
				spatial::DegAngle sweep(d);
				double x = centerX + a * sweep.cos( ) * axisCos - b * sweep.sin( ) * axisSin; 
				double y = centerY + a * sweep.cos( ) * axisSin - b * sweep.sin( ) * axisCos; 
				rval.push_back(Point2D(x,y));
			}
			return rval;
	}
}

TEST(clipper,intersection){
#ifdef TIMEIT
	LARGE_INTEGER tps;
	QueryPerformanceFrequency(&tps);
	std::cout << "The system does " << tps.QuadPart << " ticks per second" << std::endl;
#endif
	typedef std::vector<Point2D> OurPolyType;
	OurPolyType a;
	OurPolyType b;
	spatial::Volume<double,2>  vol; 
	for (int i = 20; i > 0; --i) {
		a = ellipse(0,0,0.8,0.4,spatial::DegAngle(0),i);
		b = ellipse(0,0,0.8,0.4,spatial::DegAngle(90),i);
		spatial::intersections<double>(vol,a,b);
	}
	matlabBridge::Polygon pA("g+");
	matlabBridge::Polygon pB("b+");
	frontTierAdapt::copyVectorInto(pA,a);
	frontTierAdapt::copyVectorInto(pB,b);
	std::ofstream script("clipperellipse.m");
	script << matlabBridge::writeDateTime << matlabBridge::FigureName("Ellipse intersection") << pA << pB; 
	spatial::Volume<double,2>::VectorOfVectors vOfV = vol.points( );
	for (spatial::Volume<double,2>::VectorOfVectors::const_iterator vvIter = vOfV.begin( ); vvIter != vOfV.end( );++vvIter) {
		matlabBridge::Polygon pPoly("k",3);
		frontTierAdapt::copyVectorInto(pPoly,*vvIter);
		script << pPoly; 
	}

}

TEST(clipper,size) {
	std::cout << "Clipper integer values range from " << std::numeric_limits<ClipperLib::long64>::min( )
		<< " to " << std::numeric_limits<ClipperLib::long64>::max( ) << std::endl;
}

TEST(clipper,example){
	using namespace ClipperLib;

	ClipperLib::Polygon a; 
	ClipperLib::Polygon b;

	a.push_back(IntPoint(9081931755000444,3852088701037340));
	a.push_back(IntPoint(9223372036854776,4261755592555446));
	a.push_back(IntPoint(8899528943709485,4784327748787467));
	a.push_back(IntPoint(5665663308460032,9168595685557330));
	a.push_back(IntPoint(6920856759147015,4512437454088893));
	a.push_back(IntPoint(6128204351779665,2903238065584625));
	a.push_back(IntPoint(4119204340890290,-4142963787133938));
	a.push_back(IntPoint(8746542643914421,3787478073965549));
	a.push_back(IntPoint(9043878658276244,3593034637283243));
	a.push_back(IntPoint(9081931755000444,3852088701037340));
	b.push_back(IntPoint(6410775356470902,2942958277678163));
	b.push_back(IntPoint(6286170965356830,7405526866037764));
	b.push_back(IntPoint(1298773977681875,9202139602414040));
	b.push_back(IntPoint(-684110545974676,4585850964304698));
	b.push_back(IntPoint(-160563237570624,1552192686524017));
	b.push_back(IntPoint(1159040607476757,-3928434943053781));
	b.push_back(IntPoint(6771193485403109,-2127809313692678));
	b.push_back(IntPoint(6410775356470902,2942958277678163));	

	Clipper c;
	c.AddPolygon(a,ptSubject);
	c.AddPolygon(b,ptClip);
	Polygons results;
	const bool r = c.Execute(ctIntersection ,results,pftEvenOdd,pftEvenOdd);
	assert(r);
	/*
	typedef ClipperLib::Polygon::const_iterator PIter;
	if (results.size( ) > 0) {
		answer = results.front( );
		std::ofstream log("example.txt");
		for (PIter iter = answer.begin( ); iter != answer.end( ); ++iter) {
			log << '(' << iter->X << ',' << iter->Y << ") " << std::endl;
		}
	}
	*/

	std::ofstream script("clipperExample.m");
	matlabBridge::TPolygon<long long> b1("g+-");
	matlabBridge::TPolygon<long long> b2("b+-");
	populate(b1,a);
	populate(b2,b);
	b1.close( );
	b2.close( );
	script << matlabBridge::FigureName("clipperReq") << b1 << b2; 
	for (ClipperLib::Polygons::const_iterator pIter = results.begin( ); pIter != results.end( );++pIter) {
		matlabBridge::TPolygon<long long> ans("r");
		ClipperLib::Polygon answer = *pIter;
		populate(ans,answer);
		ans.close( );
		script << ans;
	}
	script << std::flush;
}
/**
* intersection precision testing
*/
TEST(algo,intersectp) {
	typedef std::vector<Point2D> TestPoly;
	const double centerx = 1.0/7;
	const double centery = 2.0/13; 
	spatial::SVector<double,2> down(0,-1); 
	spatial::SVector<double,2> right(0.5,0); 
	Point2D topLeft(centerx - 0.5, 1);
	Point2D topRight = spatial::displacement(topLeft,right * 2);
	Point2D topCenter = spatial::displacement(topLeft,right);

	Point2D bottomLeft = spatial::displacement(topLeft, down);
	Point2D bottomCenter = spatial::displacement(bottomLeft,right);
	Point2D bottomRight = spatial::displacement(bottomLeft,right *2);



	TestPoly box;
	box.push_back(bottomCenter);
	box.push_back(bottomLeft);
	box.push_back(topLeft);
	box.push_back(topCenter);

	TestPoly box2;
	box2.push_back(bottomCenter);
	box2.push_back(topCenter);
	box2.push_back(topRight);
	box2.push_back(bottomRight);

	//std::vector<Point2D> e = ellipse(double centerX, double centerY, double a, double b, const spatial::Angle & angleOfMajor, 
	TestPoly e = ellipse(centerx, centery, 1, 0.5, spatial::DegAngle(0),1);
	spatial::Volume<double,2> result;
	spatial::intersections<double,TestPoly,TestPoly>(result,box,e);
	ASSERT_TRUE(result.volume( ) > 0);

	spatial::Volume<double,2> result2;
	spatial::intersections<double,TestPoly,TestPoly>(result2,box2,e);
	ASSERT_TRUE(result2.volume( ) > 0);

	std::ofstream script("intersectp.m");
	script << matlabBridge::FigureName("Intersect p") << matlabBridge::clearFigure;

	matlabBridge::Polygon pbox("g",3);
	matlabBridge::Polygon pbox2("y",3);
	matlabBridge::Polygon pe("m",3);
	frontTierAdapt::copyVectorInto(pbox,box);
	frontTierAdapt::copyVectorInto(pbox2,box2);
	frontTierAdapt::copyVectorInto(pe,e);
	script << pbox << pbox2 << pe;
	spatial::Volume<double,2>::VectorOfVectors vOfV = result.points( );
	for (spatial::Volume<double,2>::VectorOfVectors::const_iterator vvIter = vOfV.begin( ); vvIter != vOfV.end( );++vvIter) {
		matlabBridge::Polygon pPoly("k",3);
		frontTierAdapt::copyVectorInto(pPoly,*vvIter);
		for (spatial::Volume<double,2>::PointVector::const_iterator vIter = vvIter->begin( ); vIter != vvIter->end( );++vIter) {
			const Point2D &p = *vIter;
			//std::cout << p << " dist " << spatial::distance(p,bottomCenter) << std::endl;
			if (spatial::distance(p,bottomCenter)  < 0.01) {
				std::cout << "hit " << &bottomCenter << ", " << &p << std::endl;
			}
		}
		script << pPoly; 
	}
	//std::cout << "second verse" << std::endl;
	vOfV = result2.points( );
	for (spatial::Volume<double,2>::VectorOfVectors::const_iterator vvIter = vOfV.begin( ); vvIter != vOfV.end( );++vvIter) {
		matlabBridge::Polygon pPoly("b",3);
		frontTierAdapt::copyVectorInto(pPoly,*vvIter);
		for (spatial::Volume<double,2>::PointVector::const_iterator vIter = vvIter->begin( ); vIter != vvIter->end( );++vIter) {
			const Point2D &p = *vIter;
			//std::cout << p << " dist " << spatial::distance(p,bottomCenter) << std::endl;
			if (spatial::distance(p,bottomCenter)  == 0) {
				std::cout << "hit " << &bottomCenter << ", " << &p << std::endl;
			}
		}
		script << pPoly; 
		script << pPoly; 
	}
}

