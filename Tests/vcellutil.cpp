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
