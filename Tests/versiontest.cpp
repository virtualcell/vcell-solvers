#include <iostream>
#include <version.h>
#include "gtest/gtest.h"
TEST(version,basic) {
	const vcell_util::Version & version = vcell_util::Version::get( );
	std::cout << version.svn  << ' ' << version.compileDate << ' ' << version.compileTime << std::endl;

}
