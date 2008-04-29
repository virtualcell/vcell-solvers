#include "OptXmlReader.h"
#include "CFSQPSolver.h"
#include "OdeOptJob.h"
#include "OdeMultiShootingOptJob.h"
#include "OdeResultSet.h"
#include "math.h"

#include <iostream>

int main(int argc, char *argv[])
{
	std::cout.precision(30);
	OptXmlReader optXmlReader;
	const char* filename1 = "c:\\developer\\eclipse\\workspace\\numerics\\OptStandalone2\\test1.xml";
	const char* filename2 = "c:\\developer\\eclipse\\workspace\\numerics\\OptStandalone2\\test2.xml";
	const char* filename3 = "c:\\developer\\eclipse\\workspace\\numerics\\OptStandalone2\\test3.xml";
	const char* filename4 = "c:\\developer\\eclipse\\workspace\\numerics\\OptStandalone2\\test4.xml";
	const char* filename5 = "c:\\developer\\eclipse\\workspace\\numerics\\OptStandalone2\\test5.xml";
	const char* filename6 = "c:\\developer\\eclipse\\workspace\\numerics\\OptStandalone2\\test6.xml";
	OdeOptJob* odeOptJob = optXmlReader.readOdeOptJob(filename6);
	//OdeOptJob* odeOptJob = new OdeOptJob(NUM_PARAMS, paramNames, LB, UB, initialGuess, scaleFactors, 0, 0, 0, 0, 0, refData, columnExpressions, (char*)input.c_str(), 0);
	CFSQPOptSolver* cfsqpOptSolver = new CFSQPOptSolver(odeOptJob);
	OptSolverResultSet* optSolverResultSet = cfsqpOptSolver->solve();
	optSolverResultSet->show();
	delete odeOptJob;
	delete cfsqpOptSolver;
}





