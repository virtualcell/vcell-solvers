#include "PdeOptTesting.h"
#include "NonspatialOptTesting.h"
#include "OptXmlReader2.h"
#include "OptSolverCFSQP.h"
#include "OptSolver2.h"
#include "OptResultSet.h"
#include <iostream>

int main(int argc, char *argv[])
{
	std::cout.precision(30);
	OptXmlReader2 optXmlReader;
	const char* filename1 = "c:\\developer\\eclipse\\workspace\\numerics\\OptStandalone2\\test1.xml";
	const char* filename2 = "c:\\developer\\eclipse\\workspace\\numerics\\OptStandalone2\\test2.xml";
	const char* filename3 = "c:\\developer\\eclipse\\workspace\\numerics\\OptStandalone2\\test3.xml";
	const char* filename4 = "c:\\developer\\eclipse\\workspace\\numerics\\OptStandalone2\\test4.xml";
	const char* filename5 = "c:\\developer\\eclipse\\workspace\\numerics\\OptStandalone2\\test5.xml";
	const char* filename6 = "c:\\developer\\eclipse\\workspace\\numerics\\OptStandalone2\\test6.xml";
	OptProblemDescription* optProb = optXmlReader.readOptProblemDescription(filename6);
	OptSolverCFSQP* cfsqpOptSolver = new OptSolverCFSQP(optProb);
	OptResultSet* optResultSet = cfsqpOptSolver->solve();
	optResultSet->show();
	optResultSet->show();

	//runExplitOptTest1();
	//runOdeOptTesting();
	//runExplitOptTest2();
	//runOdeOptTesting();
//	runPdeOptTesting();
//	runVFRAPOptTesting();
}





