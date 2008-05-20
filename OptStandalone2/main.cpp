#include "PdeOptTesting.h"
#include "NonspatialOptTesting.h"
#include "OptXmlReader2.h"
#include "OptSolverCFSQP.h"
#include "OptSolver2.h"
#include "OptResultSet.h"
#include <Exception.h>
using namespace VCell;

#include <iostream>
using namespace std;

int main(int argc, char *argv[])
{
	try {
		cout.precision(30);
		OptXmlReader2 optXmlReader;
		const char* filename = "test10.xml";
		OptProblemDescription* optProb = optXmlReader.readOptProblemDescription(filename);
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
	} catch (string& errMsg) {
		cout << "Exception : " << errMsg << endl;
	} catch (Exception& ex) {
		cout << "Exception : " << ex.getMessage() << endl;
	} catch (const char* errMsg) {
		cout << "Exception : " << errMsg << endl;
	} catch (...) {
		cout << "Exception!" << endl;
	}
}





