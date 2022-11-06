#include "gtest/gtest.h"
#include <boost/math/common_factor.hpp>
#include <boost/logic/tribool.hpp>
#include <boost/logic/tribool_io.hpp>
using std::cout;
using std::endl;

TEST(boost,lcm) {
	int answer = boost::math::lcm(6,4);
	ASSERT_TRUE(answer == 12);
}

TEST(boost,tribool) {
	using boost::logic::tribool;
	tribool a = true;
	tribool b = false;
	tribool c = boost::logic::indeterminate;
	bool ba = (bool)a;
	bool bb = (bool)b;
	bool bc = (bool)c;
	std::cout << a  << ',' << b  << ',' << c  << std::endl; 
	ASSERT_TRUE(a);
    ASSERT_FALSE(b);
}
