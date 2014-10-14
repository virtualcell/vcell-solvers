#include "gtest/gtest.h"
#include <limits>
#include <fstream>
#include <vcellutil.h>
#include <NumericConvert.h>
#include <MPoint.h>
#include <vcellstring.h>
#include <typeinfo>
#include <persist.h>
using namespace vcell_util; 
namespace {
	double dx = 2.0 / 3 - 0.0001;
	double dy = 2.0 / 7 + 0.0001;
}

TEST(vcellutil,multiply) {
	ASSERT_TRUE(validMultiply(3,4));
	ASSERT_FALSE(validMultiply<char>(127,127));
}

TEST(persist,tcheck) {
	ASSERT_TRUE(typeid(spatial::TPoint<double,2>) == typeid(const spatial::TPoint<double,2>)); 
	std::cout << "dtt " << vcell_persist::getTypeToken(typeid(double)) << std::endl;
}

TEST(persist,TPoint) {

	{
		std::ofstream out("tpoint2.dat");
		spatial::TPoint<double,2> d(dx,dy);
		d.registerType( );
		d.persist(out);
		spatial::TPoint<int,3> e(7,24,25);
		e.registerType( );
		e.persist(out);
		spatial::TPoint<int,3> other(e); 
	}

	{
		std::ifstream in("tpoint2.dat");
		spatial::TPoint<double,2> d(in);
		spatial::TPoint<int,3> e(in);
		ASSERT_TRUE(d(spatial::cX) == dx);
		ASSERT_TRUE(d(spatial::cY) == dy);
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
		std::ofstream out("mpoint.dat");
		spatial::MPoint<double,2> a(i,r);
		a.persist(out);
		PromiscousME b(i,r);
		b.setForTesting(sp);
		b.persist(out);
	}

	{
		std::ifstream in("mpoint.dat");
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

