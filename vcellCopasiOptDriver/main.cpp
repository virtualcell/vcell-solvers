
#include <stdio.h>
#include <iostream>
#include <OptResultSet.h>
#include "CopasiOptDriver.h"

#include <string>
using namespace std;

int main(int argc, char *argv[])
{
	string fileName = "D:\\COPASI\\copasiOptxml.txt"; 
	FILE* fp = fopen(fileName.c_str(), "r");
	char buffer[8192];
	int len = fread(buffer, 1, 8192, fp);
	buffer[len] = 0;
	string s(buffer);
	string resultSetXML;
	CopasiOptDriver copasiOptDriver;
	copasiOptDriver.run(s, resultSetXML, 0);

	cout << "the end of program";
	      //return new COPASIOptResults(objFuncValue, numEvaluation, paramNames, paramVals);
}