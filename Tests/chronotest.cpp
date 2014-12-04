#include <stdexcept>
#include "gtest/gtest.h"
#include <VCellChrono.h>

using namespace vcell_util;
namespace {
	void demo(const std::chrono::seconds & s) {
		using namespace std::chrono;
		std::cout << HMS<seconds>(s) << std::endl;
		std::cout << HMS<seconds>(s, HMSFormat::FIXED).describe( ) << ' ' << HMS<seconds>(s,HMSFormat::FIXED) << std::endl;
		std::cout << HMS<seconds>(s, HMSFormat::FIXED||HMSFormat::ALL).describe( ) << ' ' << HMS<seconds>(s,HMSFormat::FIXED|HMSFormat::ALL) << std::endl;
		std::cout << HMS<seconds>(s, HMSFormat::ALL).describe( ) << ' ' << HMS<seconds>(s,HMSFormat::ALL) << std::endl << std::endl;
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
	std::cout << HMS<milliseconds>(ms) << std::endl;
	nanoseconds ns(1);
	std::cout << HMS<nanoseconds>(ns) << std::endl;
	

}
