/*
 * VCellNFSim.cpp
 *
 *  Created on: Mar 13, 2015
 *      Author: gweatherby
 */

#include <iostream>
#include <cstring>
#include <stdexcept>
#include <VCELL/SimulationMessaging.h>
#include <VCellException.h>
#include <vcellxml.h>
#include "JMSHolder.h"
#include "NFMonitor.h"
#include <OstreamSpy.h>

int NFsimMain(int argc, char **argv);

namespace {
	vcell::JMSHolder getXML(const char * const filename);
	void nfsimExit(int returnCode, std::string& errorMsg);
	void printUsage();
	int parseInteger(const char * input);
	const int unsetTaskId = -1;
	/**
	 * pretty print boolean on / off
	 */
	inline const char * onOrOff(bool b) {
		return b ? "on" : "off";
	}
}

#if !defined(SVNVERSION)
#error SVNVERSION version not defined
#endif
#define VCELLSVNQ(x) #x
#define VCELLSVNQUOTE(x) VCELLSVNQ(x)

#ifdef USE_MESSAGING
int main(int argc, char **argv) {

	std::string errorMessage = "Exception: ";
	vcell::JMSHolder holder; //we need to keep these strings in memory until end of program
	try {
		std::cout
				<< "NFsim messaging version $URL$"VCELLSVNQUOTE(SVNVERSION)
				<< std::endl;

		int taskId = unsetTaskId;
		const char * inputFilename = 0;
		const int penultimate = argc - 1;
		for (int i = 0; i < penultimate; i++) {
			if (strcmp(argv[i], "-tid") == 0) {
				taskId = parseInteger(argv[i + 1]);
			}
			if (strcmp(argv[i], "-xml") == 0) {
				inputFilename = argv[i + 1];
			}
		}
		bool isMessaging = (taskId != unsetTaskId);
		std::cout << "VCell messaging is " << onOrOff( isMessaging ) << std::endl;
		if (isMessaging) {
			if (inputFilename != 0) {
				holder = getXML(inputFilename);
				SimulationMessaging::create(holder.broker.c_str(),
						holder.jmsUser.c_str(), holder.pw.c_str(),
						holder.queue.c_str(), holder.topic.c_str(),
						holder.vcellUser.c_str(), holder.simKey,
						holder.jobIndex, taskId);
				SimulationMessaging::getInstVar()->start(); // start the thread
			} else {
				VCELL_EXCEPTION(invalid_argument,
						"no filename found with taskId = " << taskId);
			}
		}
		else {
				SimulationMessaging::create( );
		}
		int ecode = -100;
		{
			using vcell_nfsim::NFMonitor;
			using vcell_nfsim::NFMonitorCout;
			using vcell_nfsim::NFMonitorCerr;
			NFMonitor monitor;
			NFMonitorCout monitorCout(monitor);
			vcell_util::OStreamSpy<NFMonitorCout> coutToMonitor(std::cout, monitorCout);
			NFMonitorCerr monitorCerr(monitor);
			vcell_util::OStreamSpy<NFMonitorCerr> cerrToMonitor(std::cerr, monitorCerr);
			if (isMessaging) {
				monitor.reportStart();
			}
			ecode = NFsimMain(argc, argv);
			if (isMessaging && ecode == 0) {
				monitor.reportComplete();
			}
		} //we want coutToMonitor destroyed, in case messaging going to cout
		nfsimExit(ecode, errorMessage);
	} catch (const std::exception &e) {
		errorMessage += "caught exception ";
		errorMessage += e.what();
		nfsimExit(-1, errorMessage);
	} catch (const char * msg) {
		errorMessage += "caught exception ";
		errorMessage += msg;
		nfsimExit(-1, errorMessage);
	} catch (...) {
		errorMessage += "caught unknown exception ";
		nfsimExit(-1, errorMessage);
	}
}
#else
int main(int argc, char **argv) {
	std::cout
	<< "NFsim (no messaging) version $URL$"VCELLSVNQUOTE(SVNVERSION)
	<< std::endl;
	return NFsimMain(argc,argv);
}
#endif

#ifdef USE_MESSAGING
#define MESSAGING_ONLY(x) x
#else
#define MESSAGING_ONLY(x)
#endif

namespace {

	void nfsimExit(int returnCode, std::string& errorMsg) {
		if (SimulationMessaging::getInstVar() == 0) {
			if (returnCode != 0) {
				std::cerr << errorMsg << std::endl;
			}
		} else if (!SimulationMessaging::getInstVar()->isStopRequested()) {
			if (returnCode != 0) {
				SimulationMessaging::getInstVar()->setWorkerEvent(
						new WorkerEvent(JOB_FAILURE, errorMsg.c_str()));
			}
			MESSAGING_ONLY(
					SimulationMessaging::getInstVar()->waitUntilFinished()
					;
		)
	}
}

void printUsage() {
	std::cout << "Arguments : [NFsim args] "MESSAGING_ONLY("[-tid taskID]")
			<< std::endl;
}

int parseInteger(const char * input) {
	if (input != 0) {
		const int eos = strlen(input);
		for (int i = 0; i < eos; i++) {
			const char c = input[i];
			if ((c < '0') || (c > '9')) {
				VCELL_EXCEPTION(domain_error,
						"invalid character " << c << " in integer");
			}
		}
		return atoi(input);
	}
}
/*
 # JMS_Paramters
 JMS_PARAM_BEGIN
 JMS_BROKER tcp://code:2507
 JMS_USER serverUser cbittech
 JMS_QUEUE workerEventDev
 JMS_TOPIC serviceControlDev
 VCELL_USER fgao
 SIMULATION_KEY 36230826
 JOB_INDEX 0
 JMS_PARAM_END
 */

vcell::JMSHolder getXML(const char * const filename) {
	vcell::JMSHolder pkg;

	using tinyxml2::XMLElement;
	tinyxml2::XMLDocument doc;
	doc.LoadFile(filename);
	if (doc.Error()) {
		VCELL_EXCEPTION(runtime_error,
				"Error " << doc.ErrorID( ) << " reading " << filename);
	}
	const XMLElement & root = *doc.RootElement();
	if (std::string(root.Name()) != std::string("sbml")) {
		VCELL_EXCEPTION(invalid_argument,
				"Error, root element name " << root.Name( ) << " is not 'sbml'");
	}
	const XMLElement *jms = root.FirstChildElement("jms");
	if (jms == 0) {
		VCELL_EXCEPTION(invalid_argument, "No 'jms' element in XML");
	}
	using vcell_xml::convertChildElement;
	pkg.broker = convertChildElement<std::string>(*jms, "broker");
	pkg.jmsUser = convertChildElement<std::string>(*jms, "jmsUser");
	pkg.pw = convertChildElement<std::string>(*jms, "pw");
	pkg.queue = convertChildElement<std::string>(*jms, "queue");
	pkg.topic = convertChildElement<std::string>(*jms, "topic");
	pkg.vcellUser = convertChildElement<std::string>(*jms, "vcellUser");
	pkg.simKey = convertChildElement<unsigned long>(*jms, "simKey");
	pkg.jobIndex = convertChildElement<unsigned int>(*jms, "jobIndex");
	return pkg;
}
}
