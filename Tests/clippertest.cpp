#include <iostream>
#include <fstream>
#include <cassert>

#include <vector>
#include <algorithm>
#include <Volume.h>
#include <MovingBoundaryTypes.h>
#include <MBridge/MBPolygon.h>
#include <MBridge/Figure.h>
#include <MBridge/FronTierAdapt.h>
#include "gtest/gtest.h"
#include "clipper.hpp"
#include "Angle.h"
#include "intersection.h"
//these were names used in previous version of clipper
typedef ClipperLib::Path Polygon;
typedef ClipperLib::Paths Polygons;

template <class T>
void populate(matlabBridge::TPolygon<T> & mpoly, const ClipperLib::Path & poly) {
	typedef ClipperLib::Path cprPgon;
	for (cprPgon::const_iterator iter = poly.begin( );iter != poly.end( ); ++iter) {
		//mpoly.add(static_cast<double>(iter->X),static_cast<double>(iter->Y));
		mpoly.add(iter->X,iter->Y);
	}
}

ClipperLib::Path build(int count, long long *x,long long *y) {
	ClipperLib::Path rval;
	for (int i = 0; i < count; i++) {
		rval.push_back(ClipperLib::IntPoint(x[i],y[i]));
	}
	//rval.push_back(ClipperLib::IntPoint(x[0],y[0]));
	return rval;
}

TEST(clipper,largenum){
	using namespace ClipperLib;
	using ClipperLib::Path;
	std::cout << "max value is " << std::numeric_limits<long long>::max( ) << std::endl;
	/*
	Polygon1.x = [4387414506071, 4157630003224, 3891577094948, 3591914093955, 3261635133897, -3870280929772]
	Polygon1.y = [2108535817006, 2483323859773, 2833299351411, 3154965452493, 3445108181668, -440]

	Polygon2.x = [5717460464435, 3518437208883, 3518437208883, 5717460464435]
	Polygon2.y = [4398046511104, 4398046511104, 2199023255552, 2199023255552]
	*/
	cInt x1[] = {4387414506071, 4157630003224, 3891577094948, 3591914093955, 3261635133897, -3870280929772};
	cInt y1[] = {2108535817006, 2483323859773, 2833299351411, 3154965452493, 3445108181668, -440};

	cInt x2[] =  {5717460464435, 3518437208883, 3518437208883, 5717460464435};
	cInt y2[]  = {4398046511104, 4398046511104, 2199023255552, 2199023255552};
	ClipperLib::Path p1 = build(sizeof(x1)/sizeof(x1[0]),x1,y1);
	ClipperLib::Path p2 = build(sizeof(x2)/sizeof(x2[0]),x2,y2);
	std::reverse(p2.begin( ), p2.end( ));

	CleanPolygon(p1,p1);
	CleanPolygon(p2,p2);

	Clipper c;
	c.AddPath(p1,ptSubject,false);
	c.AddPath(p2,ptClip,false);
	Paths results;
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
	ClipperLib::Path a;
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
	cInt x1[] = {0,5,5,0};
	cInt y1[] = {0,0,5,5};

	cInt x2[] =  {-1,6,6,-1};
	cInt y2[]  = {2,2,4,4};
	ClipperLib::Path p1 = build(sizeof(x1)/sizeof(x1[0]),x1,y1);
	ClipperLib::Path p2 = build(sizeof(x2)/sizeof(x2[0]),x2,y2);

	Clipper c;
	c.AddPath(p1,ptSubject,false);
	c.AddPath(p2,ptClip,false);
	Polygons results;
	ClipperLib::Path answer;
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

	ClipperLib::Path p1 = build(sizeof(x1)/sizeof(x1[0]),x1,y1);
	ClipperLib::Path p2 = build(sizeof(x2)/sizeof(x2[0]),x2,y2);

	Clipper c;
	c.AddPath(p1,ptSubject,false);
	c.AddPath(p2,ptClip,false);
	Polygons results;
	ClipperLib::Path answer;
	PolyFillType types[] = {pftPositive,pftNegative,pftNegative,pftEvenOdd};
	const bool r = c.Execute(ctIntersection ,results,pftEvenOdd,pftEvenOdd);
	assert(r);
	typedef ClipperLib::Path::const_iterator PIter;
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
	cInt x1[] = {0,5,5,0};
	cInt y1[] = {0,0,5,5};

	cInt x2[] =  {14,16,16,14};
	cInt y2[]  = {2,2,4,4};
	ClipperLib::Path p1 = build(sizeof(x1)/sizeof(x1[0]),x1,y1);
	ClipperLib::Path p2 = build(sizeof(x2)/sizeof(x2[0]),x2,y2);
	Clipper c;


	c.AddPath(p1,ptSubject,false);
	c.AddPath(p2,ptClip,false);
	Polygons results;
	ClipperLib::Path answer;
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

#ifdef FIXUP
TEST(clipper,intersection){
#ifdef TIMEIT
	LARGE_INTEGER tps;
	QueryPerformanceFrequency(&tps);
	std::cout << "The system does " << tps.QuadPart << " ticks per second" << std::endl;
#endif
	typedef std::vector<Point2D> OurPolyType;
	OurPolyType a;
	OurPolyType b;
	spatial::Volume<double,double,2>  vol; 
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
	spatial::Volume<double,double,2>::VectorOfVectors vOfV = vol.points( );
	for (spatial::Volume<double,double,2>::VectorOfVectors::const_iterator vvIter = vOfV.begin( ); vvIter != vOfV.end( );++vvIter) {
		matlabBridge::Polygon pPoly("k",3);
		frontTierAdapt::copyVectorInto(pPoly,*vvIter);
		script << pPoly; 
	}

}
#endif

TEST(clipper,size) {
	std::cout << "Clipper integer values range from " << std::numeric_limits<ClipperLib::cInt>::min( )
		<< " to " << std::numeric_limits<ClipperLib::cInt>::max( ) << std::endl;
}

TEST(clipper,example){
	using namespace ClipperLib;

	ClipperLib::Path a; 
	ClipperLib::Path b;

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
	c.AddPath(a,ptSubject,false);
	c.AddPath(b,ptClip,false);
	Polygons results;
	const bool r = c.Execute(ctIntersection ,results,pftEvenOdd,pftEvenOdd);
	assert(r);
	/*
	typedef ClipperLib::Path::const_iterator PIter;
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
	for (ClipperLib::Paths::const_iterator pIter = results.begin( ); pIter != results.end( );++pIter) {
		matlabBridge::TPolygon<long long> ans("r");
		ClipperLib::Path answer = *pIter;
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
#ifdef HOLD_FOR_NOW
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
	spatial::Volume<double,double,2> result;
	spatial::intersections<double,double,TestPoly,TestPoly>(result,box,e);
	ASSERT_TRUE(result.volume( ) > 0);

	spatial::Volume<double,double,2> result2;
	spatial::intersections<double,double,TestPoly,TestPoly>(result2,box2,e);
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
	spatial::Volume<double,double,2>::VectorOfVectors vOfV = result.points( );
	for (spatial::Volume<double,double,2>::VectorOfVectors::const_iterator vvIter = vOfV.begin( ); vvIter != vOfV.end( );++vvIter) {
		matlabBridge::Polygon pPoly("k",3);
		frontTierAdapt::copyVectorInto(pPoly,*vvIter);
		for (spatial::Volume<double,double,2>::PointVector::const_iterator vIter = vvIter->begin( ); vIter != vvIter->end( );++vIter) {
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
	for (spatial::Volume<double,double,2>::VectorOfVectors::const_iterator vvIter = vOfV.begin( ); vvIter != vOfV.end( );++vvIter) {
		matlabBridge::Polygon pPoly("b",3);
		frontTierAdapt::copyVectorInto(pPoly,*vvIter);
		for (spatial::Volume<double,double,2>::PointVector::const_iterator vIter = vvIter->begin( ); vIter != vvIter->end( );++vIter) {
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
namespace {
	void scaledown(ClipperLib::Path &p) {
		for (ClipperLib::Path::iterator iter = p.begin( ); iter != p.end( );++iter) {
			iter->X /=2;
			iter->Y /=2;
		}
	}
}

TEST(clipper,testcase) {
	using namespace ClipperLib;
	Polygon polyA;
	Polygon polyB;
	polyA.push_back(IntPoint(-350111673235712000,-7939286986110536704));
	polyA.push_back(IntPoint(-350111672573059776,-1163274266659200768));
	polyA.push_back(IntPoint(-485638770871588672,-1163274266659200768));
	polyA.push_back(IntPoint(-621165869170117632,-1298801360662762496));
	polyA.push_back(IntPoint(-3553422551835379712,-7163314728386196480));
	polyA.push_back(IntPoint(-350111673235712000,-7939286986110536704));
	polyB.push_back(IntPoint(-805645495940844032,-1042562189840885120));
	polyB.push_back(IntPoint(-789525366217924864,-1054885693040449280));
	polyB.push_back(IntPoint(-773128044900516096,-1066836971753736832));
	polyB.push_back(IntPoint(-756665441390834688,-1078699097885569280));
	polyB.push_back(IntPoint(-739925597552984576,-1090166104233594880));
	polyB.push_back(IntPoint(-723065516277786112,-1101454671896804864));
	polyB.push_back(IntPoint(-706034715454333440,-1112484903150015232));
	polyB.push_back(IntPoint(-688774538806552832,-1123152437475880320));
	polyB.push_back(IntPoint(-671441569765006720,-1133701281089901312));
	polyB.push_back(IntPoint(-653889015211007232,-1143880904975027584));
	polyB.push_back(IntPoint(-636171541082062848,-1153769412850584320));
	polyB.push_back(IntPoint(-618368350621474560,-1163503951353955584));
	polyB.push_back(IntPoint(-600354419700158080,-1172843426363947264));
	polyB.push_back(IntPoint(-582209437029484032,-1181923164888783616));
	polyB.push_back(IntPoint(-563971396586663680,-1190816160411563264));
	polyB.push_back(IntPoint(-545549721624207360,-1199323180587737344));
	polyB.push_back(IntPoint(-527003583301815872,-1207552812216577536));
	polyB.push_back(IntPoint(-508374006747220352,-1215593338859950848));
	polyB.push_back(IntPoint(-489588472884072128,-1223262882315161600));
	polyB.push_back(IntPoint(-470673541875198656,-1230606558074297088));
	polyB.push_back(IntPoint(-451675208810608640,-1237730259058597632));
	polyB.push_back(IntPoint(-432579580717049536,-1244592072012137472));
	polyB.push_back(IntPoint(-413340554655909120,-1251040668979537152));
	polyB.push_back(IntPoint(-394025555686812736,-1257254396514333184));
	polyB.push_back(IntPoint(-374630934454637568,-1263217142076553472));
	polyB.push_back(IntPoint(-355128110187412608,-1268817797957962752));
	polyB.push_back(IntPoint(-335546876593472704,-1274134253721124096));
	polyB.push_back(IntPoint(-315884037673904832,-1279140791922908416));
	polyB.push_back(IntPoint(-296160575948242560,-1283906561352555264));
	polyB.push_back(IntPoint(-276342167197082976,-1288260386148198144));
	polyB.push_back(IntPoint(-256468620099854912,-1292350221693581568));
	polyB.push_back(IntPoint(-236544455318338336,-1296188105170435328));
	polyB.push_back(IntPoint(-216560182733396544,-1299702011983161600));
	polyB.push_back(IntPoint(-196510915370628000,-1302824185163809792));
	polyB.push_back(IntPoint(-176424446516299744,-1305690758577683200));
	polyB.push_back(IntPoint(-156300607924843904,-1308285004132608256));
	polyB.push_back(IntPoint(-136135915764860672,-1310546130133814528));
	polyB.push_back(IntPoint(-115934816217608272,-1312449180680685312));
	polyB.push_back(IntPoint(-95710253516958480,-1314077297079773696));
	polyB.push_back(IntPoint(-75464922749251728,-1315429545826308352));
	polyB.push_back(IntPoint(-55198946235745320,-1316438355349572096));
	polyB.push_back(IntPoint(-34919135455860688,-1317096605851951104));
	polyB.push_back(IntPoint(-14632721738018770,-1317477843267614720));
	polyB.push_back(IntPoint(5657482380678871,-1317581851126844672));
	polyB.push_back(IntPoint(25947090697894924,-1317338345409913856));
	polyB.push_back(IntPoint(46228960128899896,-1316747704996026880));
	polyB.push_back(IntPoint(66500396707274432,-1315880110543367680));
	polyB.push_back(IntPoint(86758615856882752,-1314736055124691456));
	polyB.push_back(IntPoint(106994475598381776,-1313240233871131904));
	polyB.push_back(IntPoint(127201552785642560,-1311403010815586048));
	polyB.push_back(IntPoint(147381322484617024,-1309291032801204224));
	polyB.push_back(IntPoint(167531176928224256,-1306905519578282752));
	polyB.push_back(IntPoint(187635286823819584,-1304157380934391808));
	polyB.push_back(IntPoint(207691246544198016,-1301082362929003776));
	polyB.push_back(IntPoint(227707023029217120,-1297757115066176512));
	polyB.push_back(IntPoint(247669789298808384,-1294123327987265536));
	polyB.push_back(IntPoint(267562597641278784,-1290122868394923776));
	polyB.push_back(IntPoint(287392471003872384,-1285825636568967936));
	polyB.push_back(IntPoint(307163293626026752,-1281264858900788992));
	polyB.push_back(IntPoint(326869814300975616,-1276429515729096448));
	polyB.push_back(IntPoint(346487453211124800,-1271246104524616192));
	polyB.push_back(IntPoint(366003662509548992,-1265694179892965632));
	polyB.push_back(IntPoint(385462037122170880,-1259943564254672128));
	polyB.push_back(IntPoint(404818372442808512,-1253855889521635328));
	polyB.push_back(IntPoint(424073699681275712,-1247458079555856896));
	polyB.push_back(IntPoint(443230732653420800,-1240772996099539456));
	polyB.push_back(IntPoint(462304420157055360,-1233850428244045568));
	polyB.push_back(IntPoint(481227521333248640,-1226526749825900800));
	polyB.push_back(IntPoint(500078604966003520,-1219021037143934208));
	polyB.push_back(IntPoint(518790697299496704,-1211174659400328192));
	polyB.push_back(IntPoint(537362132566560320,-1202999740879542272));
	polyB.push_back(IntPoint(555830310897337856,-1194596417893439488));
	polyB.push_back(IntPoint(574167507517329792,-1185910729804709632));
	polyB.push_back(IntPoint(592348555089614336,-1176900901289441280));
	polyB.push_back(IntPoint(610407921517729152,-1167651621210917632));
	polyB.push_back(IntPoint(628323896673680384,-1158127419398810368));
	polyB.push_back(IntPoint(646066738240875776,-1148282670858516224));
	polyB.push_back(IntPoint(663697009872836992,-1138239303335232256));
	polyB.push_back(IntPoint(681147608193621504,-1127886361771856512));
	polyB.push_back(IntPoint(698400779077170176,-1117206680355105792));
	polyB.push_back(IntPoint(715544670482684928,-1106354706303699456));
	polyB.push_back(IntPoint(732510232652253440,-1095223794347766784));
	polyB.push_back(IntPoint(749286404996775040,-1083809964358603392));
	polyB.push_back(IntPoint(765865205435924608,-1072112528622453888));
	polyB.push_back(IntPoint(782277157271260672,-1060180414909668992));
	polyB.push_back(IntPoint(798499652782176768,-1047994559359855104));
	polyB.push_back(IntPoint(814592462742915968,-1035635394005036160));
	polyB.push_back(IntPoint(830423237770981760,-1022943350096340352));
	polyB.push_back(IntPoint(846089761350096896,-1010048831311192448));
	polyB.push_back(IntPoint(861545171675356544,-996901871522674432));
	polyB.push_back(IntPoint(876773850422140928,-983493288712640000));
	polyB.push_back(IntPoint(891842639579587840,-969904921904532992));
	polyB.push_back(IntPoint(906632501525968000,-956013664991341312));
	polyB.push_back(IntPoint(921291340166629760,-941984124608123648));
	polyB.push_back(IntPoint(935641382416905216,-927638702539557376));
	polyB.push_back(IntPoint(949865393203038592,-913168612736861952));
	polyB.push_back(IntPoint(963774739944013056,-898395267934403200));
	polyB.push_back(IntPoint(977537128893267584,-883485495264830848));
	polyB.push_back(IntPoint(991005177205717760,-868308660245481088));
	polyB.push_back(IntPoint(1004276744433486208,-852960164806553600));
	polyB.push_back(IntPoint(1017293409090974464,-837394665860800128));
	polyB.push_back(IntPoint(1030051790699884800,-821616478404009088));
	polyB.push_back(IntPoint(1042549435831144576,-805632413464334336));
	polyB.push_back(IntPoint(1054829456639982336,-789479129289444992));
	polyB.push_back(IntPoint(1066841121825282304,-773127195234658432));
	polyB.push_back(IntPoint(1078700790584652416,-756662823880027648));
	polyB.push_back(IntPoint(1090198926857846400,-739944382400888832));
	polyB.push_back(IntPoint(1101435396756425728,-723049554523671936));
	polyB.push_back(IntPoint(1112477044431005312,-706025761840175360));
	polyB.push_back(IntPoint(1123154134193374208,-688771772904675328));
	polyB.push_back(IntPoint(1133689512744737152,-671430861327426944));
	polyB.push_back(IntPoint(1143901625987814528,-653897248434652800));
	polyB.push_back(IntPoint(1153770941487631360,-636168760454980736));
	polyB.push_back(IntPoint(1163500271258406144,-618362778387040640));
	polyB.push_back(IntPoint(1172867881628127744,-600363843169159040));
	polyB.push_back(IntPoint(1181924417227397888,-582206885445977984));
	polyB.push_back(IntPoint(1190816031752896256,-563968143614174848));
	polyB.push_back(IntPoint(1199347230428483840,-545558011417184384));
	polyB.push_back(IntPoint(1207553813162096896,-527001286114086464));
	polyB.push_back(IntPoint(1215574467316312320,-508363452625243200));
	polyB.push_back(IntPoint(1223281750221859072,-489593313525738752));
	polyB.push_back(IntPoint(1230607473396858368,-470671156916540160));
	polyB.push_back(IntPoint(1237727936823826432,-451671660669501120));
	polyB.push_back(IntPoint(1244580930054931712,-432572865302393664));
	polyB.push_back(IntPoint(1251085026009581568,-413352942114825024));
	polyB.push_back(IntPoint(1257255051726035200,-394023445927359168));
	polyB.push_back(IntPoint(1263217751839594240,-374628811113788672));
	polyB.push_back(IntPoint(1268818407721003264,-355125986846563648));
	polyB.push_back(IntPoint(1274135957870563840,-335544840316473600));
	polyB.push_back(IntPoint(1279141315538462464,-315881870655226688));
	polyB.push_back(IntPoint(1283907084968111360,-296158408929564928));
	polyB.push_back(IntPoint(1288260863899356928,-276339989607353504));
	polyB.push_back(IntPoint(1292350653580420352,-256466432951721376));
	polyB.push_back(IntPoint(1296173959222694656,-236539683621785888));
	polyB.push_back(IntPoint(1299687404570960128,-216555109508611200));
	polyB.push_back(IntPoint(1302867232232098816,-196515057487656160));
	polyB.push_back(IntPoint(1305691057567375872,-176422201385933376));
	polyB.push_back(IntPoint(1308285256526435840,-156298357079990528));
	polyB.push_back(IntPoint(1310546382527642368,-136133664920007296));
	polyB.push_back(IntPoint(1312449386478683648,-115932560635115440));
	polyB.push_back(IntPoint(1314077456281970688,-95707994167525888));
	polyB.push_back(IntPoint(1315429658432724992,-75462660598727792));
	polyB.push_back(IntPoint(1316438467955986688,-55196684085221288));
	polyB.push_back(IntPoint(1317096671862603264,-34916871466507360));
	polyB.push_back(IntPoint(1317477862682511104,-14630456869753312));
	polyB.push_back(IntPoint(1317581823945986560,5659747169057355));
	polyB.push_back(IntPoint(1317338318229058560,25949355486273440));
	polyB.push_back(IntPoint(1316747631219415552,46231223878490704));
	polyB.push_back(IntPoint(1315879990170989824,66502658457855040));
	polyB.push_back(IntPoint(1314735888156530944,86760874645680240));
	polyB.push_back(IntPoint(1313240066902971648,106996734387179232));
	polyB.push_back(IntPoint(1311402797251620096,127203807646088480));
	polyB.push_back(IntPoint(1309290772641403136,147383572445077984));
	polyB.push_back(IntPoint(1306905212822609664,167533421010699328));
	polyB.push_back(IntPoint(1304157074178718720,187637530906294656));
	polyB.push_back(IntPoint(1301089189374872320,207694534675683968));
	polyB.push_back(IntPoint(1297736621068670208,227705692773802880));
	polyB.push_back(IntPoint(1294122870477125632,247672064327795840));
	polyB.push_back(IntPoint(1290122410884784384,267564872670266368));
	polyB.push_back(IntPoint(1285825131318390784,287394735908787840));
	polyB.push_back(IntPoint(1281276733877125120,307168584642277568));
	polyB.push_back(IntPoint(1276477689323345408,326883976397732416));
	polyB.push_back(IntPoint(1271192252013009664,346469290741652800));
	polyB.push_back(IntPoint(1265695132005383936,366000391084841600));
	polyB.push_back(IntPoint(1259944586462022912,385458786917592128));
	polyB.push_back(IntPoint(1253856911728986112,404815122238229760));
	polyB.push_back(IntPoint(1247488657633124864,424080161884296192));
	polyB.push_back(IntPoint(1240774232379317504,443227326343121792));
	polyB.push_back(IntPoint(1233851664523825152,462301013846756864));
	polyB.push_back(IntPoint(1226528060656519424,481224143011086336));
	polyB.push_back(IntPoint(1218986312702365440,500060332856069312));
	polyB.push_back(IntPoint(1211176000333166592,518787651024806784));
	polyB.push_back(IntPoint(1203001081812380416,537359086291870272));
	polyB.push_back(IntPoint(1194637834001157376,555845736162080320));
	polyB.push_back(IntPoint(1185912256086542336,574164427613800832));
	polyB.push_back(IntPoint(1176902427571275264,592345475186086144));
	polyB.push_back(IntPoint(1167694588094235136,610426319881294464));
	polyB.push_back(IntPoint(1158129142339177216,628320791479289216));
	polyB.push_back(IntPoint(1148284393798883328,646063633046484480));
	polyB.push_back(IntPoint(1138228420514119936,663686353174728064));
	polyB.push_back(IntPoint(1127889661032288512,681145637929233408));
	polyB.push_back(IntPoint(1117230733280597504,698411638041721856));
	polyB.push_back(IntPoint(1106319572876118528,715517739818968192));
	polyB.push_back(IntPoint(1095222419046307968,732505402329676928));
	polyB.push_back(IntPoint(1083809662062408960,749281887918477312));
	polyB.push_back(IntPoint(1072125094171804800,765870200016344192));
	polyB.push_back(IntPoint(1060230393484296576,782309287468353536));
	polyB.push_back(IntPoint(1047986699806312704,798489072966322432));
	polyB.push_back(IntPoint(1035637096992472704,814589222376828416));
	polyB.push_back(IntPoint(1022912100322412800,830393570943895168));
	polyB.push_back(IntPoint(1010060358725556480,846095197418885376));
	polyB.push_back(IntPoint(996882556780983680,861524239988338944));
	polyB.push_back(IntPoint(983513651031659904,876787752530671232));
	polyB.push_back(IntPoint(969894643730893312,891828732659218048));
	polyB.push_back(IntPoint(956040397056219392,906653261585549440));
	polyB.push_back(IntPoint(941980244244016256,921282693270853888));
	polyB.push_back(IntPoint(927669503184327296,935667317096947712));
	polyB.push_back(IntPoint(913168665752790272,949860130803980928));
	polyB.push_back(IntPoint(898427940958141056,963804040230537728));
	polyB.push_back(IntPoint(883487125883186816,977533207153152768));
	polyB.push_back(IntPoint(868323261280084224,991015544086286848));
	polyB.push_back(IntPoint(852961071874183552,1004271887373839872));
	polyB.push_back(IntPoint(837364121095777280,1017249948870407808));
	polyB.push_back(IntPoint(821614367255978368,1030043414182275584));
	polyB.push_back(IntPoint(805614701726000768,1042520993697804288));
	polyB.push_back(IntPoint(789514552315494784,1054870596511644032));
	polyB.push_back(IntPoint(773145836493013504,1066860719342378112));
	polyB.push_back(IntPoint(756649693914866944,1078675077046567168));
	polyB.push_back(IntPoint(739955296793893504,1090208092771052672));
	polyB.push_back(IntPoint(723036165484448256,1101408177555235840));
	polyB.push_back(IntPoint(706040208358451200,1112492444895805056));
	polyB.push_back(IntPoint(688787250676371584,1123172216479555584));
	polyB.push_back(IntPoint(671420194410158592,1133663748759164544));
	polyB.push_back(IntPoint(653906963704727424,1143910774349745792));
	polyB.push_back(IntPoint(636173445447028992,1153772038467536128));
	polyB.push_back(IntPoint(618357799362834944,1163482680958597888));
	polyB.push_back(IntPoint(600377027630348160,1172885105349854208));
	polyB.push_back(IntPoint(582211023465738112,1181924672397023488));
	polyB.push_back(IntPoint(563963837520470848,1190797917489014528));
	polyB.push_back(IntPoint(545569883963094016,1199363940129079296));
	polyB.push_back(IntPoint(527007025260000384,1207557932386435328));
	polyB.push_back(IntPoint(508354873932967168,1215544058091070464));
	polyB.push_back(IntPoint(489596443429323392,1223279504947883008));
	polyB.push_back(IntPoint(470679475932218432,1230619521392939008));
	polyB.push_back(IntPoint(451668988141633920,1237710431706155520));
	polyB.push_back(IntPoint(432574425002465088,1244573863890054400));
	polyB.push_back(IntPoint(413359960207026880,1251094709727462400));
	polyB.push_back(IntPoint(394025543419410560,1257250294064401664));
	polyB.push_back(IntPoint(374623078736060992,1263185592973043968));
	polyB.push_back(IntPoint(355140368622064576,1268855822890269696));
	polyB.push_back(IntPoint(335544481871483136,1274121177739634432));
	polyB.push_back(IntPoint(315894252890381568,1279152765395871744));
	polyB.push_back(IntPoint(296159746070639552,1283869849919725568));
	polyB.push_back(IntPoint(276355462413740448,1288287750952716288));
	polyB.push_back(IntPoint(256476253880432416,1292352340949957632));
	polyB.push_back(IntPoint(236545027412265472,1296150843141403904));
	polyB.push_back(IntPoint(216564146191681312,1299682590625898240));
	polyB.push_back(IntPoint(196523518839778592,1302860759287625984));
	polyB.push_back(IntPoint(176431128728013824,1305688937416884992));
	polyB.push_back(IntPoint(156304586168528896,1308259248220301568));
	polyB.push_back(IntPoint(136144249636679232,1310556023670188800));
	polyB.push_back(IntPoint(115943582902676768,1312469283768028928));
	polyB.push_back(IntPoint(95716892411374880,1314073865297491968));
	polyB.push_back(IntPoint(75470419589062576,1315402717943710720));
	polyB.push_back(IntPoint(55206911014503728,1316455109559125248));
	polyB.push_back(IntPoint(34926608907058252,1317116053680033792));
	polyB.push_back(IntPoint(14639553061629352,1317474052813219584));
	polyB.push_back(IntPoint(-5650334228032682,1317554842062771200));
	polyB.push_back(IntPoint(-25940233680405664,1317358401766244352));
	polyB.push_back(IntPoint(-46222683753873640,1316767030717325312));
	polyB.push_back(IntPoint(-66493313705998752,1315876138895030528));
	polyB.push_back(IntPoint(-86749753502803392,1314708687363462144));
	polyB.push_back(IntPoint(-106989255753323152,1313265368265636608));
	polyB.push_back(IntPoint(-127196391106116304,1311421681325259264));
	polyB.push_back(IntPoint(-147373836771411040,1309285989131621376));
	polyB.push_back(IntPoint(-167520338816471712,1306876541131052032));
	polyB.push_back(IntPoint(-187633561558004096,1304196007247438080));
	polyB.push_back(IntPoint(-207689135410248480,1301115080029332736));
	polyB.push_back(IntPoint(-227694880233947488,1297728945240577024));
	polyB.push_back(IntPoint(-247656107097808448,1294090391156378880));
	polyB.push_back(IntPoint(-267562780233227040,1290159505075630080));
	polyB.push_back(IntPoint(-287387842975536512,1285837025885596160));
	polyB.push_back(IntPoint(-307152630787361344,1281250557064366592));
	polyB.push_back(IntPoint(-326864165384761024,1276436737063115264));
	polyB.push_back(IntPoint(-346474842703090112,1271226472934981376));
	polyB.push_back(IntPoint(-365997221309858304,1265696498688712704));
	polyB.push_back(IntPoint(-385447065834072640,1259918551863224832));
	polyB.push_back(IntPoint(-404822455705243712,1253892040455799808));
	polyB.push_back(IntPoint(-424079659113215872,1247498399773504256));
	polyB.push_back(IntPoint(-443218886594357632,1240760945053276672));
	polyB.push_back(IntPoint(-462286973667508096,1233825236553459712));
	polyB.push_back(IntPoint(-481230790072642112,1226554794977544192));
	polyB.push_back(IntPoint(-500062514938148416,1219001267864972288));
	polyB.push_back(IntPoint(-518768426005149184,1211141088299578880));
	polyB.push_back(IntPoint(-537370332208817280,1203035745229100288));
	polyB.push_back(IntPoint(-555842757986481408,1194640510395352064));
	polyB.push_back(IntPoint(-574145102951952768,1185881207783436544));
	polyB.push_back(IntPoint(-592360464297086592,1176940955451067648));
	polyB.push_back(IntPoint(-610423118083546624,1167696736765288192));
	polyB.push_back(IntPoint(-628299099251895296,1158097251813298944));
	polyB.push_back(IntPoint(-646080368169063936,1148322080387919872));
	polyB.push_back(IntPoint(-663687911260119552,1138238716272885120));
	polyB.push_back(IntPoint(-681116862330200576,1127849969857249664));
	polyB.push_back(IntPoint(-698412908493947008,1117239866756818560));
	polyB.push_back(IntPoint(-715515138311040000,1106322226078200320));
	polyB.push_back(IntPoint(-732502631975200128,1095225742856775808));
	polyB.push_back(IntPoint(-749284266259951232,1083819913159131520));
	polyB.push_back(IntPoint(-765840534747210752,1072090227218203776));
	polyB.push_back(IntPoint(-782304530628585344,1060230027375102464));
	polyB.push_back(IntPoint(-798483268239338624,1047984346229416960));
	polyB.push_back(IntPoint(-814588434547346304,1035642120367227136));
	polyB.push_back(IntPoint(-830411531515206272,1022940033144045184));
	polyB.push_back(IntPoint(-846081345352063744,1010049828046585088));
	polyB.push_back(IntPoint(-861540138537999872,996906934020082688));
	polyB.push_back(IntPoint(-876772454621750656,983502228484178432));
	polyB.push_back(IntPoint(-891844912942968832,969918061309615744));
	polyB.push_back(IntPoint(-906638691802610176,956030321457716352));
	polyB.push_back(IntPoint(-921273350403871360,941976271577265408));
	polyB.push_back(IntPoint(-935655677057619456,927662775123739136));
	polyB.push_back(IntPoint(-949833340113547392,913148030122348672));
	polyB.push_back(IntPoint(-963797603099154688,898426384155643008));
	polyB.push_back(IntPoint(-977503686919320576,883465128240524672));
	polyB.push_back(IntPoint(-991016908609801856,868328661927390848));
	polyB.push_back(IntPoint(-1004256198418711296,852952263274196736));
	polyB.push_back(IntPoint(-1017262058905250816,837378197072615040));
	polyB.push_back(IntPoint(-1030060467263122432,821632802365908224));
	polyB.push_back(IntPoint(-1042524621047329920,805622033236947200));
	polyB.push_back(IntPoint(-1054883257204766208,789528822282638848));
	polyB.push_back(IntPoint(-1066831595479058048,773128832189848064));
	polyB.push_back(IntPoint(-1078686907098720768,756662033344937984));
	polyB.push_back(IntPoint(-1090188607630336256,739946010745256576));
	polyB.push_back(IntPoint(-1101428370259707136,723053314945368704));
	polyB.push_back(IntPoint(-1112480050613537152,706036713844534784));
	polyB.push_back(IntPoint(-1123159732030287872,688783542960986240));
	polyB.push_back(IntPoint(-1133690829162655744,671440204498571904));
	polyB.push_back(IntPoint(-1143905192196793984,653907844526891904));
	polyB.push_back(IntPoint(-1153770315527169280,636176550641420160));
	polyB.push_back(IntPoint(-1163502822866408448,618372593562573568));
	polyB.push_back(IntPoint(-1172876911171238912,600376650432149376));
	polyB.push_back(IntPoint(-1181923146115190016,582214103369266432));
	polyB.push_back(IntPoint(-1190816039796752128,563976308069655616));
	polyB.push_back(IntPoint(-1199357693345881088,545570666126303808));
	polyB.push_back(IntPoint(-1207556591453597440,527010071534690176));
	polyB.push_back(IntPoint(-1215583103085052416,508374788503505088));
	polyB.push_back(IntPoint(-1223278194117264640,489599821751485696));
	polyB.push_back(IntPoint(-1230618210562319616,470682854254380288));
	polyB.push_back(IntPoint(-1237709195426376960,451672394451932736));
	polyB.push_back(IntPoint(-1244590528728240384,432584027567810368));
	polyB.push_back(IntPoint(-1251069675807598848,413355191050114816));
	polyB.push_back(IntPoint(-1257249271857051392,394028793623987456));
	polyB.push_back(IntPoint(-1263184640860627200,374626350160766720));
	polyB.push_back(IntPoint(-1268854870777850880,355143640046769728));
	polyB.push_back(IntPoint(-1274149472258974720,335555908829155264));
	polyB.push_back(IntPoint(-1279124825321682176,315885035697745600));
	polyB.push_back(IntPoint(-1283870355170303744,296157481165722240));
	polyB.push_back(IntPoint(-1288288256203293952,276353197508823072));
	polyB.push_back(IntPoint(-1292352798460097792,256473978851442976));
	polyB.push_back(IntPoint(-1296161743012133632,236544631104622528));
	polyB.push_back(IntPoint(-1299681336364739584,216561739938114688));
	polyB.push_back(IntPoint(-1302846914838374144,196519120008886784));
	polyB.push_back(IntPoint(-1305689244172558336,176428884645536768));
	polyB.push_back(IntPoint(-1308259508380103168,156302336208066016));
	polyB.push_back(IntPoint(-1310556237234155008,136141994776231376));
	polyB.push_back(IntPoint(-1312469497331994880,115941328042228912));
	polyB.push_back(IntPoint(-1314074032265652480,95714633622575440));
	polyB.push_back(IntPoint(-1315402838316088576,75468157838479984));
	polyB.push_back(IntPoint(-1316455183335736576,55204647264910928));
	polyB.push_back(IntPoint(-1317116127456645120,34924345157465448));
	polyB.push_back(IntPoint(-1317474079994076416,14637288273248866));
	polyB.push_back(IntPoint(-1317554822647875840,-5652599096300146));
	polyB.push_back(IntPoint(-1317358335755591168,-25942497669760952));
	polyB.push_back(IntPoint(-1316766964706674432,-46224947743229000));
	polyB.push_back(IntPoint(-1315876026288614400,-66495575856524712));
	polyB.push_back(IntPoint(-1314708528161265408,-86752012852237984));
	polyB.push_back(IntPoint(-1313265162467637760,-106991511335817952));
	polyB.push_back(IntPoint(-1311421475527260928,-127198646688611168));
	polyB.push_back(IntPoint(-1309285736737793792,-147376087616266464));
	polyB.push_back(IntPoint(-1306876242141359360,-167522583946840128));
	polyB.push_back(IntPoint(-1304194419854964992,-187635613382726624));
	polyB.push_back(IntPoint(-1301098414716456960,-207689098189177376));
	polyB.push_back(IntPoint(-1297768058080866304,-227704077651851840));
	polyB.push_back(IntPoint(-1294089959269538816,-247658294245943744));
	polyB.push_back(IntPoint(-1290159073188791808,-267564967381362720));
	polyB.push_back(IntPoint(-1285836548134435840,-287390020565267616));
	polyB.push_back(IntPoint(-1281250033448811264,-307154797806041088));
	polyB.push_back(IntPoint(-1276410282591717376,-326859631859639040));
	polyB.push_back(IntPoint(-1271250295234879232,-346483557903476992));
	polyB.push_back(IntPoint(-1265695888925671424,-365999344650709056));
	polyB.push_back(IntPoint(-1259917896651522048,-385449175593528000));
	polyB.push_back(IntPoint(-1253888922890785536,-404823749025723904));
	polyB.push_back(IntPoint(-1247453376628586496,-424067221815408000));
	polyB.push_back(IntPoint(-1240767192200484096,-443223844093770944));
	polyB.push_back(IntPoint(-1233824321230897664,-462289358626168192));
	polyB.push_back(IntPoint(-1226553879654982400,-481233175031302208));
	polyB.push_back(IntPoint(-1219015831784185344,-500071440810627776));
	polyB.push_back(IntPoint(-1211140087354058752,-518770723192879872));
	polyB.push_back(IntPoint(-1203034744283580672,-537372629396548288));
	polyB.push_back(IntPoint(-1194581210417058048,-555818303496221056));
	polyB.push_back(IntPoint(-1185879955444822272,-574147654535460608));
	polyB.push_back(IntPoint(-1176939703112452096,-592363015880593792));
	polyB.push_back(IntPoint(-1167637430493475328,-610395717512616704));
	polyB.push_back(IntPoint(-1158095723176251136,-628301879878978560));
	polyB.push_back(IntPoint(-1148320551750872192,-646083148796147328));
	polyB.push_back(IntPoint(-1138234324466299392,-663689330681455488));
	polyB.push_back(IntPoint(-1127848273139755008,-681119628232079360));
	polyB.push_back(IntPoint(-1117238170039323904,-698415674395825536));
	polyB.push_back(IntPoint(-1106365518242819712,-715546732024255872));
	polyB.push_back(IntPoint(-1095215347769248128,-732499045984257408));
	polyB.push_back(IntPoint(-1083806729936940544,-749279121665152640));
	polyB.push_back(IntPoint(-1072087109690130944,-765842128339496832));
	polyB.push_back(IntPoint(-1060224576845303168,-782304443340905472));
	polyB.push_back(IntPoint(-1048016597861527296,-798511215204171648));
	polyB.push_back(IntPoint(-1035630344876225024,-814582543827606656));
	polyB.push_back(IntPoint(-1022959292055407616,-830430419342219520));
	polyB.push_back(IntPoint(-1010026881428562176,-846065430070269952));
	polyB.push_back(IntPoint(-996914620557745920,-861550293596542336));
	polyB.push_back(IntPoint(-983470739792860288,-876748052352838016));
	polyB.push_back(IntPoint(-969916994648317312,-891847994289686656));
	polyB.push_back(IntPoint(-955992689599765632,-906607363433852160));
	polyB.push_back(IntPoint(-941969195877843456,-921271104134772224));
	polyB.push_back(IntPoint(-927621243049996800,-935619056962618752));
	polyB.push_back(IntPoint(-913137303382067456,-949827593269779712));
	polyB.push_back(IntPoint(-898383095388123520,-963757447797794816));
	polyB.push_back(IntPoint(-883452953021188736,-977496391668348928));
	polyB.push_back(IntPoint(-868303426406228096,-990995256214777216));
	polyB.push_back(IntPoint(-852940806849919488,-1004249535790368256));
	polyB.push_back(IntPoint(-837398005472809984,-1017293493612283520));
	polyB.push_back(IntPoint(-821624237135682048,-1030056918296435200));
	polyB.push_back(IntPoint(-805645495940844032,-1042562189840885120));
	int scaleCount = 0;
	Polygons results;
	for (;;) {
		Clipper c;
		bool goodAdd = c.AddPath(polyA,ptSubject,true);
		goodAdd = c.AddPath(polyB,ptClip,true);
		const bool r = c.Execute(ctIntersection ,results,pftEvenOdd,pftEvenOdd);
		assert(r);
		if (results.size( ) > 0) {
			break;
		}
		scaledown(polyA);
		scaledown(polyB);
		++scaleCount;
		std::cout << "scaled down " << scaleCount << std::endl;
	}
	/*
	typedef ClipperLib::Path::const_iterator PIter;
	if (results.size( ) > 0) {
		answer = results.front( );
		std::ofstream log("example.txt");
		for (PIter iter = answer.begin( ); iter != answer.end( ); ++iter) {
			log << '(' << iter->X << ',' << iter->Y << ") " << std::endl;
		}
	}
	*/

	std::ofstream script("clipperExample2.m");
	matlabBridge::TPolygon<long long> b1("g+-");
	matlabBridge::TPolygon<long long> b2("b+-");
	populate(b1,polyA);
	populate(b2,polyB);
	b1.close( );
	b2.close( );
	script << matlabBridge::FigureName("clipperReq2") << b1 << b2; 
	for (ClipperLib::Paths::const_iterator pIter = results.begin( ); pIter != results.end( );++pIter) {
		matlabBridge::TPolygon<long long> ans("r");
		ClipperLib::Path answer = *pIter;
		populate(ans,answer);
		ans.close( );
		script << ans;
	}
	script << std::flush;
#endif
}
