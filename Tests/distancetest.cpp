#include "gtest/gtest.h"
#include <Distance.h>
using std::cout;
using std::endl;
using namespace spatial;

TEST(distance,basic) {
	long long biggest = std::numeric_limits<long>::max( );
	long okay = DefaultDistancePolicy<long>::convert(biggest);
	ASSERT_TRUE(okay == biggest);
	long long tooBig = biggest + 1;
	ASSERT_THROW(DefaultDistancePolicy<long>::convert(tooBig),std::domain_error);
	cout << okay << endl;
}
