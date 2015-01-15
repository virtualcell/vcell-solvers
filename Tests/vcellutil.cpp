#include "gtest/gtest.h"
#include <limits>
#include <random>
#include <fstream>
#include <vcellutil.h>
#include <NumericConvert.h>
#include <MPoint.h>
#include <vcellstring.h>
#include <Mesh.h>
#include <persistcontainer.h>
#include <Volume.h>
#include <Segment.h>
#include <SVector.h>
#include <boundaryProviders.h>
#include <vcarray.h>
#include <NoChangeSentinel.h>
#include "mockpoint.inc"
using namespace vcell_util; 
namespace {
	double dx = 2.0 / 3 - 0.0001;
	double dy = 2.0 / 7 + 0.0001;
	double dz = 14.0 / 5 + 0.0301;
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
TEST(vcellutil,digits) {
	using namespace std;
	cout << setfill('0') << setw(8) << fixed << setprecision(5) << 1.23<< endl;
	std::cout << std::numeric_limits<int>::is_integer << std::endl;
	for (int i = 0; i < 103; i++) {
		//std::cout << i << ' ' << vcell_util::numberDigits(i) << std::endl;
	}
	for (double d = 0; d < 11; d += 0.3) {
	 //std::cout << d << ' ' << vcell_util::numberDigits(d) << std::endl;
	}
}

TEST(vcellutil,multiply) {
	ASSERT_TRUE(validMultiply(3,4));
	ASSERT_FALSE(validMultiply<char>(127,127));
}
TEST(vcellutil,vcarray) {
	using vcell_util::vcarray;
	vcarray<double,3> cat;
	vcarray<double,cat.ArraySize> dog;
	static_assert(cat.ArraySize == 3, "size");
	cat[0] = dog[0] = 3;
}
TEST(vcellutil,print) {
	const int spec = 12;
	const double lvalue = 123456789012;
	std::cout << std::setw(spec) << std::setprecision(spec) << lvalue << std::endl;
	std::cout << std::setprecision(spec) << lvalue << std::endl;
	std::cout << std::setw(spec) << lvalue << std::endl;
	std::cout << lvalue << std::endl;
}
TEST(vcellutil,nochange) {
	int x = 3;
	float y = 3.4f;
	double z = 7.1;
	
	auto nc1 = makeSentinel("x",x);
	auto nc2 = makeSentinel("y",y);
	auto nc3 = makeSentinel("z",12,z);
	x = 4;
	z = 7.11;
}
