#include "gtest/gtest.h"
#include <vcellutil.h>
#include <NumericConvert.h>
#include <limits>
using namespace vcell_util; 

TEST(vcellutil,multiply) {
	ASSERT_TRUE(validMultiply(3,4));
	ASSERT_FALSE(validMultiply<char>(127,127));

}
