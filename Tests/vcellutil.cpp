#include "gtest/gtest.h"
#include <limits>
#include <fstream>
#include <vcellutil.h>
#include <NumericConvert.h>
#include <TPoint.h>
using namespace vcell_util; 
namespace {
	double dx = 2.0 / 3 - 0.0001;
	double dy = 2.0 / 7 + 0.0001;
}

TEST(vcellutil,multiply) {
	ASSERT_TRUE(validMultiply(3,4));
	ASSERT_FALSE(validMultiply<char>(127,127));

}

TEST(persist,TPoint) {
	{
	std::ofstream out("tpoint2.dat");
	spatial::TPoint<double,2> d(dx,dy);
	d.persist(out);
	spatial::TPoint<int,3> e(7,24,25);
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

TEST(persist,TPointR) {
	std::ifstream in("tpoint.dat");
	spatial::TPoint<double,2> d(in);
	spatial::TPoint<int,3> e(in);
	std::cout << d << std::endl;
	std::cout << e<< std::endl;

}
