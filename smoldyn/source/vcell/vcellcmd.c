#include "smoldyn.h"
#include <SimCommand.h>

/**********************************************************/
/******************** command declarations ****************/
/**********************************************************/
#ifdef VCELL
// vcell command
enum CMDcode cmdVCellPrintProgress(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdVCellWriteOutput(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdVCellDataProcess(simptr sim,cmdptr cmd,char *line2);


#include <VCELL/SimulationMessaging.h>
#include "VCellSmoldynOutput.h"

static VCellSmoldynOutput* vcellSmoldynOutput = 0;
enum CMDcode cmdVCellPrintProgress(simptr sim, cmdptr cmd, char *line2) {
	if(line2 && !strcmp(line2,"cmdtype")) {
		return CMDobserve;
	}
	double progress = (sim->time - sim->tmin) / (sim->tmax - sim->tmin);
	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_PROGRESS, progress, sim->time));
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
	if (vcellSmoldynOutput == 0) {
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
#ifndef VCELL_HYBRID 
			if (vcellSmoldynOutput == 0) {
				vcellSmoldynOutput = new VCellSmoldynOutput(sim);
			}
			string input = dataProcessInput.str();
			vcellSmoldynOutput->parseDataProcessingInput(dataProcName, input);
#endif
			dataProcessFirstTime = false;
		} else {
			dataProcessInput << line2 << endl;
		}
	}
	return CMDok;
}
#endif
