#include <stdexcept>
#include "gtest/gtest.h"
#include <VCellChrono.h>

using namespace vcell_util;
namespace {

	void demo(const std::chrono::seconds & s) {
		using namespace std::chrono;
		std::cout << HMS<seconds>(s) << std::endl;
		std::cout << HMS<seconds,HMSFormat::FIXED>(s).describe( ) << ' ' << HMS<seconds,HMSFormat::FIXED>(s) << std::endl;
		std::cout << HMS<seconds,HMSFormat::FIXED|HMSFormat::ALL>(s).describe( ) << ' ' << HMS<seconds,HMSFormat::FIXED|HMSFormat::ALL>(s) << std::endl;
		std::cout << HMS<seconds,HMSFormat::ALL>(s).describe( ) << ' ' << HMS<seconds,HMSFormat::ALL>(s) << std::endl;
	}

}

TEST(chrono,hms) {
	using namespace std::chrono;
	seconds s(3661);
	demo(s);
	seconds two(125);
	demo(two);
	seconds three(37);
	demo(three);

	milliseconds ms(4357);
	std::cout << HMS<milliseconds,1>(ms) << std::endl;
	nanoseconds ns(1);
	std::cout << HMS<nanoseconds,2>(ns) << std::endl;
	

}
