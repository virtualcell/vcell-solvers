#include <vector>
#include <fstream>
#include <boundaryProviders.h>
#include "gtest/gtest.h"
#include <boundaryProviders.h>
#include "MBridge/MBPolygon.h"
#include "MBridge/Figure.h"
#include "MBridge/FronTierAdapt.h"
TEST(boundary,circle) {
	std::ofstream d("circle.m");
	d << matlabBridge::FigureName("Circle");
	std::unique_ptr<spatial::FrontProvider<moving_boundary::CoordinateType> > circle( moving_boundary::circleFront(1,2,0.5,.01,0.7) );
	matlabBridge::Polygon c("k+-",1);
	frontTierAdapt::copyVectorInto(c,circle->retrieveFront( ));
	matlabBridge::Polygon c2("r+-",1);
	circle->propagateTo(1);
	frontTierAdapt::copyVectorInto(c2,circle->retrieveFront( ));
	d << c << c2;
}
