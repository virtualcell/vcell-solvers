#pragma warning ( disable: 4996 )
#include "gtest/gtest.h"
#include <MovingBoundarySetup.h>
#include <MovingBoundaryParabolicProblem.h>
#include <fstream>
#include <vcellxml.h>
using namespace spatial;
using namespace moving_boundary; 
namespace {
	const char * const setupXml = 
"<?xml version='1.0' encoding='UTF-8'?>"
"<MovingBoundarySetup>"
"  <problem>"
"    <specialFront>"
"      <circle>"
"        <originx>0</originx>"
"        <originy>0</originy>"
"        <radius>1</radius>"
"        <velocityx>1</velocityx>"
"        <thetaIncrement>.01</thetaIncrement>"
"      </circle>"
"    </specialFront>"
"    <xLimits>"
"      <low>-1.5</low>"
"      <high>2.1</high>"
"    </xLimits>"
"    <yLimits>"
"      <low>-1.5</low>"
"      <high>2.1</high>"
"    </yLimits>"
"    <numNodesX>19</numNodesX>"
"    <numNodesY>19</numNodesY>"
"    <frontToNodeRatio>5</frontToNodeRatio>"
"    <maxTime>1</maxTime>"
"    <numberTimeSteps>70</numberTimeSteps>"
"    <diffusionConstant>0.25</diffusionConstant>"
"    <levelFunction>x^2 + y^2 - 1</levelFunction>"
"    <advectVelocityFunctionX>1</advectVelocityFunctionX>"
"    <advectVelocityFunctionY>0</advectVelocityFunctionY>"
"    <frontVelocityFunctionX>1</frontVelocityFunctionX>"
"    <frontVelocityFunctionY>0</frontVelocityFunctionY>"
"    <concentrationFunction>x/(x*x+y*y)^0.5*j1(1.841183781340659*(x*x+y*y)^0.5)+j1(1.841183781340659)</concentrationFunction>"
"    <hardTime>true</hardTime>"
"  </problem>"
"</MovingBoundarySetup>";

}
TEST(persist,lifetime) {
	tinyxml2::XMLDocument doc;
	tinyxml2::XMLError rcode = doc.Parse(setupXml);
	ASSERT_TRUE(rcode == tinyxml2::XML_SUCCESS);
	MovingBoundarySetup dupe; 
	{
		MovingBoundarySetup mbs = MovingBoundarySetup::setupProblem(*doc.RootElement( ));
		dupe = mbs;
	}
	std::cout << dupe.advectVelocityFunctionStrX << std::endl;
}
TEST(persist,movingBoundarySetup) {
	moving_boundary::Universe<2>::get( ).destroy( );
	tinyxml2::XMLDocument doc;
	tinyxml2::XMLError rcode = doc.Parse(setupXml);
	ASSERT_TRUE(rcode == tinyxml2::XML_SUCCESS);
	MovingBoundarySetup::registerType( );
	MovingBoundarySetup mbs = MovingBoundarySetup::setupProblem(*doc.RootElement( ));
	//MeshElementNeighbor::registerType( );
	const char * const filename = "MovingBoundarySetup.dat";
	{
		std::ofstream out(filename, std::ios::binary|std::ios::trunc);
		vcell_persist::WriteFormatter wf(out, 1);
		mbs.persist(out);
	}
	std::ifstream in(filename,std::ios::binary);
	vcell_persist::ReadFormatter wf(in, 1);
	MovingBoundarySetup back(in); 
	ASSERT_TRUE(back.concentrationFunctionStr == mbs.concentrationFunctionStr);
	ASSERT_TRUE(back.diffusionConstant == mbs.diffusionConstant);
	moving_boundary::Universe<2>::get( ).destroy( );
}

TEST(persist,movingBoundaryProblem) {
	tinyxml2::XMLDocument doc;
	tinyxml2::XMLError rcode = doc.Parse(setupXml);
	ASSERT_TRUE(rcode == tinyxml2::XML_SUCCESS);
	MovingBoundarySetup::registerType( );
	MovingBoundaryParabolicProblem::registerType( );

	MovingBoundarySetup mbs = MovingBoundarySetup::setupProblem(*doc.RootElement( ));
	MovingBoundaryParabolicProblem mbpp(mbs);
	const char * const filename = "MovingBoundaryProblem.dat";
	{
		std::ofstream out(filename, std::ios::binary|std::ios::trunc);
		vcell_persist::WriteFormatter wf(out, 3);
		mbs.persist(out);
		mbpp.persist(out);
	}
	std::ifstream in(filename,std::ios::binary);
	vcell_persist::ReadFormatter wf(in, 3);
	MovingBoundarySetup back(in); 
	MovingBoundaryParabolicProblem p2(back, in);
	moving_boundary::Universe<2>::get( ).destroy( );
}
