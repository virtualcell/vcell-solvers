#include "smoldyn.h"
#include <SimCommand.h>


/**********************************************************/
/******************** command declarations ****************/
/**********************************************************/
#include <VCELL/SimulationMessaging.h>
#include "VCellSmoldynOutput.h"

#include "smoldynfuncs.h"
namespace {
	const double  reportIntervalSeconds = 30;
	time_t lastTime = 0; // the static variable to allow sending out progress every two seconds

}

VCellSmoldynOutput* vcellSmoldynOutput = NULL;
enum CMDcode cmdVCellPrintProgress(simptr sim, cmdptr cmd, char *line2) {
	SimulationMessaging::create();
	if(line2 && !strcmp(line2,"cmdtype")) {
		return CMDobserve;
	}
	double progress = (sim->time - sim->tmin) / (sim->tmax - sim->tmin);
	time_t currentTime = time(0) ;
	double duration = difftime(currentTime,lastTime) ;
	if (duration >= reportIntervalSeconds)
	{
		SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_PROGRESS, progress, sim->time));
		lastTime = currentTime;
	}
	//fprintf(stdout, "[[[progress:%lg%%]]]",  progress * 100.0);
	if (SimulationMessaging::getInstVar()->isStopRequested()) {
		throw -1;
	}
	return CMDok;
}

#include <iostream>
#include <sstream>
using std::stringstream;
using std::endl;
enum CMDcode cmdVCellWriteOutput(simptr sim, cmdptr cmd, char *line2) {
	static stringstream vcellOutputInput;
	static bool firstTime = true;
	if(line2 && !strcmp(line2,"cmdtype")) {
		return CMDobserve;
	}
	if (vcellSmoldynOutput == NULL) {
		vcellSmoldynOutput = new VCellSmoldynOutput(sim);
	}

	string token;
	stringstream ss(line2);
	ss >> token;
	if (token == "begin") {
	} else if (token == "end") {
		if (firstTime) {
			string input = vcellOutputInput.str();
			vcellSmoldynOutput->parseInput(input);
			firstTime = false;
		}
		vcellSmoldynOutput->write();
	} else {
		if (firstTime) {
			vcellOutputInput << line2 << endl;
		}
	}	

	return CMDok;
}

enum CMDcode cmdVCellDataProcess(simptr sim,cmdptr cmd,char *line2) {
	static bool dataProcessFirstTime = true;
	static stringstream dataProcessInput;
	static string dataProcName;
	if(line2 && !strcmp(line2,"cmdtype")) {
		return CMDobserve;
	}	
	if (dataProcessFirstTime) {
		string token;
		stringstream ss(line2);
		ss >> token;
		if (token == "begin") {
			ss >> dataProcName;
		} else if (token == "end") {
			dataProcessFirstTime = false;
		} else {
			dataProcessInput << line2 << endl;
		}
	}
	return CMDok;
}

//extern int taskID;
int loadJMS(simptr sim,ParseFilePtr *pfpptr,char *line2,char *erstr) {

	char word[STRCHAR];
	ParseFilePtr pfp = *pfpptr;
	int done = 0, pfpcode;
	bool firstline2 = (line2 != NULL);
	while(!done) {
		if (firstline2) {
			strcpy(word,"name");
			pfpcode=1;
			firstline2 = false;
		} else {
			pfpcode=Parse_ReadLine(&pfp,word,&line2,erstr);
		}
		*pfpptr=pfp;
		CHECKS(pfpcode!=3,erstr);

		if(pfpcode==0);	// already taken care of

		else if(pfpcode==2) { // end reading
			done = 1;
		} else if(pfpcode==3) {	// error
			CHECKS(0,"SMOLDYN BUG: parsing error");
		} else if(!strcmp(word,"end_jms")) {  // end_jms
			CHECKS(!line2,"unexpected text following end_jms");
			break;
		} else if(!line2) {															// just word
			CHECKS(0,"missing jms parameters");
		}
	}
	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_STARTING, "setting up simulation"));
	return 0;

failure:		// failure
	return 1;
}

