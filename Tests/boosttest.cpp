#include "gtest/gtest.h"
#include <boost/math/common_factor.hpp>
using std::cout;
using std::endl;

TEST(boost,lcm) {
	int answer = boost::math::lcm(6,4);
	ASSERT_TRUE(answer == 12);
}
