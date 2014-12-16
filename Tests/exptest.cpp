#include <iostream>
#include <cassert>

#include <vector>
#include <fstream>
#include <algorithm>
#include <stdexcept>
#include <VCellException.h>
#include "Expression.h"
#include "SimpleSymbolTable.h"
#include "gtest/gtest.h"
#include "vcellxml.h"
#include <boost/lexical_cast.hpp>
#include <SExpression.h>
namespace {
}
TEST(expression,base) {
	std::string syms[] = {"a","b","c"};
	VCell::Expression exp("(a + b) * c");
	SimpleSymbolTable symTb(syms,sizeof(syms)/sizeof(syms[0]));
	exp.bindExpression(&symTb);
	double v[] = {2,3,4};
	const double r= exp.evaluateVector(v);
	std::cout << exp.infix( ) << " = " << r << std::endl;
	ASSERT_TRUE(r == 20);
}
TEST(sexpression,ops) {
	std::string syms[] = {"a","b","c"};
	SimpleSymbolTable symTb(syms,sizeof(syms)/sizeof(syms[0]));
	moving_boundary::SExpression exp("(a + b) * c",symTb);
	moving_boundary::SExpression copy(exp); 
	std::array<double,3> arr = {2,3,4};
	double r= copy.evaluate(arr);
	moving_boundary::SExpression alone("1");
	alone = exp;
	double r2= alone.evaluate(arr);
	ASSERT_TRUE(r == 20);
	ASSERT_TRUE(r2 == 20);
	std::cout << exp.infix( ) << " = " << r << std::endl;
}
TEST(sexpression,base) {
	std::string syms[] = {"a","b","c"};
	SimpleSymbolTable symTb(syms,sizeof(syms)/sizeof(syms[0]));
	moving_boundary::SExpression exp("(a + b) * c",symTb);
	std::array<double,3> arr = {2,3,4};
	double r= exp.evaluate(arr);
	std::vector<double> vec;
	vec.resize(arr.size( ));
	std::copy(arr.begin( ),arr.end( ),vec.begin( ));
	double r2= exp.evaluate(vec);
	ASSERT_TRUE(r == 20);
	ASSERT_TRUE(r2 == 20);
	std::cout << exp.infix( ) << " = " << r << std::endl;
}

TEST(sexpression,badsize) {
	std::string syms[] = {"a","b","c"};
	SimpleSymbolTable symTb(syms,sizeof(syms)/sizeof(syms[0]));
	moving_boundary::SExpression exp("(a + b) * c",symTb);
	std::array<double,2> arr = {2,3};
	ASSERT_THROW(double r= exp.evaluate(arr),std::domain_error);
	std::vector<double> vec;
	vec.resize(arr.size( ));
	std::copy(arr.begin( ),arr.end( ),vec.begin( ));
	ASSERT_THROW(double r= exp.evaluate(vec),std::domain_error);
}

TEST(expression,construct) {
	VCell::Expression a; 
	{
		VCell::Expression exp("(ax + b) * c");
		a = exp;
	}
	std::string syms[] = {"ax","b","c"};
	SimpleSymbolTable symTb(syms,sizeof(syms)/sizeof(syms[0]));
	a.bindExpression(&symTb);
	double v[] = {5,6,8};
	const double r= a.evaluateVector(v);
	std::cout << a.infix( ) << " = " << r << std::endl;
	ASSERT_TRUE(r == 88);
}
using namespace vcell_xml;
namespace {

	double toDouble(const tinyxml2::XMLElement &node, const char * name) {
		return convertChildElement<double>(node,name);
	}

	void parseXMLElement(const tinyxml2::XMLElement &element,double &min, double &max, double &step) {
		min = convertChildElement<double>(element,"min");
		max = toDouble(element,"max");
		min = toDouble(element,"min");
	}


}
TEST(vcellxml,errorPrint) {
	for (int i = 0; i < 30; i++) {
		tinyxml2::XMLError e = (tinyxml2::XMLError)i;
		std::cout << e << std::endl;
	}
}
TEST(expression,parse) {
	bool interactive = false;
	if (!interactive) {
		return;
	}

	const char * const XML_FILENAME = "expressioncontrol.xml";
	using std::cout;
	using std::cin;
	using std::endl;
	using std::getline;

	tinyxml2::XMLDocument doc;
	doc.LoadFile(XML_FILENAME);
	if (doc.ErrorID( ) == tinyxml2::XML_ERROR_FILE_NOT_FOUND) {
		std::cout << "expression.parse requires " << XML_FILENAME << " file to run, attempting to generate " << std::endl;
		std::ofstream xmlfile(XML_FILENAME);
		const char newline = '\n';
		xmlfile << "<?xml version=\"1.0\"?>" << newline
			<< "<parameters>" << newline
			<< "<x>" << newline
			<< "    <min>0</min>" << newline
			<< "    <max>10</max>" << newline
			<< "    <step>1</step>" << newline
			<< "</x>" << newline
			<< "<y>" << newline
			<< "    <min>0</min>" << newline
			<< "    <max>10</max>" << newline
			<< "    <step>1</step>" << newline
			<< "</y>" << newline
			<< "<t>" << newline
			<< "    <min>0</min>" << newline
			<< "    <max>10</max>" << newline
			<< "    <step>1</step>" << newline
			<< "</t>" << newline
			<< "</parameters>" << newline;
	}
	doc.LoadFile(XML_FILENAME);
	std::cout << "error code " << doc.LoadFile(XML_FILENAME) << std::endl;
	if (doc.ErrorID( ) != tinyxml2::XML_NO_ERROR) {
		VCELL_EXCEPTION(runtime_error,"Unable to open " << XML_FILENAME);
	}
	const tinyxml2::XMLElement *root = doc.RootElement( );
	const tinyxml2::XMLElement & xElmnt = get(*root,"x"); 
	const tinyxml2::XMLElement & yElmnt = get(*root,"y"); 
	const tinyxml2::XMLElement & tElmnt = get(*root,"t"); 
	double xmin = 0;
	double xmax = 0;
	double xstep = 1;
	parseXMLElement(xElmnt,xmin,xmax,xstep);
	double ymin = 0;
	double ymax = 0;
	double ystep = 1;
	parseXMLElement(yElmnt,ymin,ymax,ystep);
	double tmin = 0;
	double tmax = 0;
	double tstep = 1;
	parseXMLElement(tElmnt,tmin,tmax,tstep);


	std::string syms[] = {"x","y","t","e"};
	SimpleSymbolTable symTb(syms,sizeof(syms)/sizeof(syms[0]));
	double v[] = {0,0,0, 2.71828};
	std::string input;
	cout << "Enter expressions to evaluate. Press RETURN to exit" << endl;
	for (;;) {
		cout << "Expression: ";
		getline(cin,input);
		if (input.size( ) == 0) {
			break;
		}
		VCell::Expression exp(input,symTb);
		for (double x = xmin; x <=xmax; x += xstep)
			for (double y = ymin; y <=ymax; y += ystep)
				for (double t = tmin; t <=tmax; t += tstep) {
					v[0] = x;
					v[1] = y;
					v[2] = t;
					const double r= exp.evaluateVector(v);
					cout << "f(" << x << ',' << y << ',' << t << ") = " << r << endl; 
				}
	}

}
