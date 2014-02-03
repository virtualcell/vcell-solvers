#include <random>
#include "gtest/gtest.h"
#include <World.h> 

using moving_boundary::Universe;
using moving_boundary::World;
TEST(universe, basic) {
	Universe<2> &universe = Universe<2>::get( );
	universe.destroy( ); //for testing
	std::array<spatial::GeoLimit,2> limits;
	limits[0] = spatial::GeoLimit(-5,5);
	limits[1] = spatial::GeoLimit(-5,5);
	universe.init(limits);
}

TEST(universe, doubleinit) {
	Universe<2> &universe = Universe<2>::get( );
	universe.destroy( ); //for testing
	std::array<spatial::GeoLimit,2> limits;
	limits[0] = spatial::GeoLimit(-5,5);
	limits[1] = spatial::GeoLimit(-5,5);
	universe.init(limits);
	ASSERT_THROW( universe.init(limits), std::logic_error);
}

TEST(universe, badvalues) {
	ASSERT_THROW( spatial::GeoLimit(5,0), std::domain_error);
}

#ifdef REMOVED
TEST(universe, returnLimits) {
	Universe<2> &universe = Universe<2>::get( );
	const int xLow = 3;
	const int yLow = -4;
	const int xHigh = 5;
	const int yHigh = -2;
	std::array<spatial::GeoLimit,2> limits;
	limits[0] = spatial::GeoLimit(xLow,xHigh);
	//limits[spatial::cY] = spatial::GeoLimit(yLow,yHigh);
	limits[1] = spatial::GeoLimit(yLow,yHigh);
	universe.destroy( ); //for testing
	universe.init(limits);
	ASSERT_TRUE(universe.limitFor(0).low == xLow);
	ASSERT_TRUE(universe.limitFor(0).high == xHigh);
	ASSERT_TRUE(universe.limitFor(spatial::cX).low == xLow);
	ASSERT_TRUE(universe.limitFor(1).low == yLow);
	ASSERT_TRUE(universe.limitFor(1).high == yHigh);
	ASSERT_TRUE(universe.limitFor(spatial::cY).low == yLow);
	ASSERT_TRUE(universe.limits( ) == limits);
	ASSERT_TRUE(universe.limitFor(0) == limits[0]);
	ASSERT_THROW(universe.limitFor(2) == limits[0],std::domain_error);
}

TEST(universe, minAndMax) { 
	Universe<3> &universe = Universe<3>::get( );
	universe.destroy( ); //for testing
	std::array<spatial::GeoLimit,3> limits;
	limits[0] = spatial::GeoLimit(-5,8);
	limits[1] = spatial::GeoLimit(-9,5);
	limits[2] = spatial::GeoLimit(-4,5);
	universe.init(limits);
	ASSERT_TRUE(universe.maxValue( ) == 8);
	ASSERT_TRUE(universe.minValue( ) == -9);
	ASSERT_TRUE(universe.maxAbsolute( ) == 9);
}
#endif

namespace {
	const int16_t int16_tMax = 30000;
	moving_boundary::WorldMax<int16_t> ms(int16_tMax);
}

TEST(universe, diag) { 
	Universe<2> &universe = Universe<2>::get( );
	universe.destroy( ); //for testing
	std::array<spatial::GeoLimit,2> limits;
	const int16_t xlow = -1;
	const int16_t xhigh= 2; //delta is 3
	limits[0] = spatial::GeoLimit(xlow,xhigh);
	limits[1] = spatial::GeoLimit(-2.5,1.5); //4
	const int16_t maxInWorld = 5;
	universe.destroy( ); //for testing
	universe.init(limits);
	World<double,2> & dworld = World<double,2>::get( );

	ASSERT_TRUE(dworld.diagonal( ) == maxInWorld);
	World<int16_t,2> & sworld = World<int16_t,2>::get( );
	ASSERT_TRUE(sworld.diagonal( ) == int16_tMax);

	const double zeroPoint = (xlow + xhigh)/2.0;
	const double testx = 1;
	const int16_t expected =  static_cast<int16_t>(int16_tMax * (testx - zeroPoint) / maxInWorld );
	spatial::TPoint<double,2> ref(testx,0);
	spatial::TPoint<int16_t,2> spoint = sworld.toWorld(ref);
	ASSERT_TRUE(spoint(spatial::cX) == expected);
	spatial::TPoint<double,2> back = sworld.toProblemDomain(spoint);
	ASSERT_TRUE(ref == back);
	moving_boundary::WorldToPDPointConverter<int16_t,2> pconv = sworld.pointConverter( );
	spatial::TPoint<double,2> back2 = pconv(spoint);
	ASSERT_TRUE(ref == back2);

	//moving_boundary::XtoPDConverter<int16_t,2> xconv1 =  sworld.xconverter( );
	moving_boundary::WorldToPDCoordinateConverter<int16_t,2,spatial::cX> xconv1 = sworld.coordConverter<spatial::cX>( );
	World<int16_t,2>::XConverter xconv = sworld.coordConverter<spatial::cX>( );
	double xPD = xconv(expected);
	ASSERT_TRUE(xPD == testx);

	//should not compile
	//World<int16_t,2>::ZConverter zconv = sworld.coordConverter<spatial::cZ>( );
}

#ifdef SPECIAL_TEST
//locking will mess up other test, so run this individually
TEST(universe, lock) { 
	Universe<2> &universe = Universe<2>::get( );
	universe.destroy( ); //for testing
	std::array<spatial::GeoLimit,2> limits;
	limits[0] = spatial::GeoLimit(-1,2); //3
	limits[1] = spatial::GeoLimit(-2.5,1.5); //4
	std::array<unsigned int16_t, 2> nnodes = {3, 4}; 
	universe.init(limits,nnodes,true);
	ASSERT_THROW(universe.destroy( ),std::logic_error);
}
#endif 

TEST(universe, intervals) { 
	std::default_random_engine gen;
	std::uniform_int_distribution<int> rdims(3, 90);
	int16_t nx = static_cast<int16_t>(rdims(gen)); 
	int16_t ny =  static_cast<int16_t>(rdims(gen)); 
	for (int c = 0 ; c < 100; ++c) {
		Universe<2> &universe = Universe<2>::get( );
		universe.destroy( ); //for testing
		std::array<spatial::GeoLimit,2> limits;
		limits[0] = spatial::GeoLimit(-1,2); //3
		limits[1] = spatial::GeoLimit(-2.5,1.5); //4
		std::array<uint16_t, 2> nnodes = {nx, ny}; 
		universe.init(limits,nnodes,false);
		World<int32_t,2> & lworld = World<int32_t,2>::get( );
		double dscale = lworld.theScale( );
		long scale = static_cast<long>(dscale);
		ASSERT_TRUE(scale == dscale);
		ASSERT_TRUE(scale % nx == 0);
		ASSERT_TRUE(scale % ny == 0);
	}
}