#include "gtest/gtest.h"
#include <limits>
#include <random>
#include <fstream>
#include <vcellutil.h>
#include <NumericConvert.h>
#include <MPoint.h>
#include <vcellstring.h>
#include <Mesh.h>
#include <persistvector.h>
#include <Volume.h>
using namespace vcell_util; 
namespace {
	double dx = 2.0 / 3 - 0.0001;
	double dy = 2.0 / 7 + 0.0001;
	void binaryOpen(std::ofstream &out, const char *name) {
		using std::ios;
		out.open(name,ios::trunc|ios::binary);
		out.exceptions(ios::badbit|ios::failbit|ios::eofbit);
	}

	void binaryOpen(std::ifstream &in, const char *name) {
		using std::ios;
		in.open(name, ios::binary);
		in.exceptions(ios::badbit|ios::failbit);
	}
}

TEST(vcellutil,multiply) {
	ASSERT_TRUE(validMultiply(3,4));
	ASSERT_FALSE(validMultiply<char>(127,127));
}

TEST(persist,tcheck) {
	ASSERT_TRUE(typeid(spatial::TPoint<double,2>) == typeid(const spatial::TPoint<double,2>)); 
	std::cout << "dtt " << vcell_persist::getTypeToken(typeid(double)) << std::endl;
}

TEST(persist,prim) {
	const short five = 5;
	const long seven = 7; 
	const double pi =3.14159;
	{
		std::ofstream out;
		binaryOpen(out,"prim.dat");
		vcell_persist::binaryWrite(out,five);
		vcell_persist::binaryWrite(out,seven);
		vcell_persist::binaryWrite(out,pi);
	}
	{
		std::ifstream in;
		binaryOpen(in,"prim.dat");
		short f;
		long s;
		double p;
		vcell_persist::binaryRead(in,f);
		vcell_persist::binaryRead(in,s);
		vcell_persist::binaryRead(in,p);
		ASSERT_TRUE(f == five);
		ASSERT_TRUE(s == seven);
		ASSERT_TRUE(p == pi);
	}

}
TEST(persist,TPoint) {

	{
		std::ofstream out;
		binaryOpen(out,"tpoint.dat");
		spatial::TPoint<double,2> d(dx,dy);
		d.registerType( );
		d.persist(out);
		spatial::TPoint<int,3> e(7,24,25);
		e.registerType( );
		e.persist(out);
		spatial::TPoint<int,3> other(e); 
	}

	{
		std::ifstream in;
		binaryOpen(in,"tpoint.dat");
		spatial::TPoint<double,2> d(in);
		spatial::TPoint<int,3> e(in);
		ASSERT_TRUE(d(spatial::cX) == dx);
		ASSERT_TRUE(d(spatial::cY) == dy);
	}

}

TEST(persist,TPointVector) {
	std::minstd_rand g;
	std::uniform_int_distribution<short> sd(2,30);
	using std::ios;
	typedef spatial::TPoint<double,2> DPoint;
	const int vecSize = sd(g);
	DPoint::registerType( );

	std::vector<DPoint> vec;
	for (int i = 0; i < vecSize; i++) {
		DPoint d(dx + i ,dy -i);
		vec.push_back(d);
	}
	{
		std::ofstream out;
		binaryOpen(out,"tpointvec.dat");
		vcell_persist::persistVector<DPoint>(out,vec);
	}

	{
		std::ifstream in;
		binaryOpen(in,"tpointvec.dat");
		in.exceptions(ios::badbit|ios::failbit);
		std::vector<DPoint> vec2;
		vcell_persist::readVector(in,vec2);
		for (int i = 0; i < vecSize; i++) {
			const DPoint &back = vec2[i];
			ASSERT_TRUE(back(spatial::cX) == dx + i); 
			ASSERT_TRUE(back(spatial::cY) == dy - i); 
		}
	}
}
namespace {
	struct PromiscousME : public spatial::MeshElement<double, 2> {
		 PromiscousME ( const size_t *n, const double *values)  
			 :spatial::MeshElement<double, 2>(n,values) {} 
		void setForTesting(spatial::SurfacePosition sp) {
			setPos(sp);
		}
	};

}
TEST(persist,MPoint) {
	spatial::MeshElement<double,2>::registerType( );
	size_t i[2] = {5,7};
	double r[2] = {3.4, 8.5};
	spatial::SurfacePosition sp = spatial::boundarySurface;
	{
		std::ofstream out;
		binaryOpen(out,"mpoint.dat");
		spatial::MPoint<double,2> a(i,r);
		a.persist(out);
		PromiscousME b(i,r);
		b.setForTesting(sp);
		b.persist(out);
	}

	{
		std::ifstream in;
		binaryOpen(in,"mpoint.dat");
		spatial::MPoint<double,2> d(in);
		ASSERT_TRUE(d(spatial::cX) == r[0]); 
		ASSERT_TRUE(d(spatial::cY) == r[1]);
		ASSERT_TRUE(d.indexOf(0) == i[0]);
		ASSERT_TRUE(d.indexOf(1) == i[1]);
		spatial::MeshElement<double,2> f(in);
		ASSERT_TRUE(f(spatial::cX) == r[0]); 
		ASSERT_TRUE(f(spatial::cY) == r[1]);
		ASSERT_TRUE(f.indexOf(0) == i[0]);
		ASSERT_TRUE(f.indexOf(1) == i[1]);
		ASSERT_TRUE(f.mPos( ) == sp);
	}
}
TEST(persist,Mesh) {
	std::array<short,2> origin = {{2,3}};
	std::array<short,2> sizes = {{5,6}};
	std::array<size_t,2> npoints = {{11,17}};
	std::array<short,2> intervals; 
	size_t cv = 0;
	{
		std::ofstream out;
		binaryOpen(out,"mesh.dat");
		spatial::MeshDef<short,2> md(origin,sizes,npoints);
		md.registerType( );
		md.persist(out);
		intervals[0] = md.interval(spatial::cX); 
		intervals[1] = md.interval(spatial::cY); 
		cv = md.checkvalue( );
	}
	{
		std::ifstream in;
		binaryOpen(in,"mesh.dat");
		spatial::MeshDef<short,2> back(in); 
		ASSERT_TRUE(back.startCorner(spatial::cX) == origin[0]) ; 
		ASSERT_TRUE(back.startCorner(spatial::cY) == origin[1]) ; 

		ASSERT_TRUE(back.interval(spatial::cX) == intervals[0]) ; 
		ASSERT_TRUE(back.interval(spatial::cY) == intervals[1]) ; 

		ASSERT_TRUE(back.numCells(spatial::cX) == npoints[0]) ;
		ASSERT_TRUE(back.numCells(spatial::cY) == npoints[1]) ;
		ASSERT_TRUE(back.checkvalue( ) == cv);
	}
}
TEST(persist,volume) {
	using spatial::Volume;
	typedef spatial::TPoint<double,2> Point;
	typedef Volume<double,double,2> VType;
	VType::registerType( );
	VType empty(0);
	VType single(1); 
	for (int i = 0 ; i < 3; i++) {
		single.add(Point(i,2*i));
	}
	single.close( );
	double vol = single.volume( );
	{
		std::ofstream out;
		binaryOpen(out,"volume.dat");
		empty.persist(out);
		single.persist(out);
	}
	std::ifstream in;
	binaryOpen(in,"volume.dat");
	VType eback(in);
	VType sback(in);
	ASSERT_TRUE(sback.volume( ) == vol);
}

/*
TEST(persist,rback) {
	const std::type_info & ti = typeid(spatial::TPoint<double,2>);
	std::cout << ti.name( ) << std::endl;
	std::ifstream in("tpoint.dat");
	spatial::TPoint<double,2> d(in);
	spatial::TPoint<int,3> e(in);
}
*/
TEST(persist,npc) {
	std::string x("me");
	x.push_back(03);
	std::string pretty = vcell_util::convertNonPrintable(x);
	std::cout << x << ", " << pretty << std::endl; 
}

