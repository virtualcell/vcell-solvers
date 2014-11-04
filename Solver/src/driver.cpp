#include <MPoint.h>
#include <iostream>
#include <vcellxml.h>
#include <vhdf5/file.h>
#include <ProblemPackage.h>
#include <HDF5Client.h>
#include <Logger.h>
#include <boundaryProviders.h>
#include <StateClient.h>
#include <MBridge/MatlabDebug.h>
#include <tclap/CmdLine.h>
using tinyxml2::XMLElement;

namespace {
	/**
	* constants
	*/
	const char * const XML_ROOT_NAME = "MovingBoundarySetup";

	/**
	* usings and typedefs
	*/
	using moving_boundary::MovingBoundaryElementClient;
	using moving_boundary::MovingBoundarySetup;
	using moving_boundary::MovingBoundaryParabolicProblem;

	/**
	* forward declarations of functions in file
	*/
	void setupTrace(const XMLElement &root); 
	MovingBoundaryElementClient *setupReportClient(const XMLElement &root, const std::string & filename, MovingBoundaryParabolicProblem &mbpp); 
	void setupMatlabDebug(const XMLElement &root); 
	void setupHeartbeat(const XMLElement &root,moving_boundary::MovingBoundaryParabolicProblem & mbpp);

	/**
	* shared variables 
	*/
	std::auto_ptr<vcell_util::FileDest> traceFileDestination; 
}

#if !defined(SVNVERSION)
#error SVNVERSION version not defined
#endif

#define VCELLSVNQ(x) #x
#define VCELLSVNQUOTE(x) VCELLSVNQ(x)

int main(int argc, char *argv[])
{
	std::cout 
		<< "MovingBoundarySolver version $URL$" VCELLSVNQUOTE(SVNVERSION)
		<< std::endl; 

	namespace tclap = TCLAP;
	std::string filename;
	std::string outname;
	std::string restorename;
	bool parseOnly;
	try {
		using namespace tclap;
		CmdLine cmd("Moving boundary solve",' ',"1");
		cmd.setExceptionHandling(false);
		ValueArg<std::string> config("c","config","Input XML file name",true,"","string",cmd);
		ValueArg<std::string> output("o","output","HDF5 output file name",false,"","string",cmd);
		ValueArg<std::string> restore("r","restore","stored state file name",false,"","string",cmd);
		ValueArg<std::string> ignore("i","ignore","ignore me",false,"","string",cmd);
		SwitchArg ponly("p","parseonly","Parse XML without running simulation",cmd);
		cmd.parse(argc,argv);
		filename = config.getValue( );
		outname = output.getValue( );
		restorename = restore.getValue( );
		parseOnly = ponly.getValue( );
	} catch(tclap::ArgException  &ae) {
		std::cerr << "error " << ae.error( ) << " arg " << ae.argId( ) << std::endl;
		return 3;
	} catch (tclap::ExitException &ee) {
		return ee.getExitStatus( );
	}

	if (filename.empty( )) {
		//std::cerr  << opts << std::endl;
		//std::cerr << "Usage: " << argv[0] << " [xml input file] <hdf5 output file name|'parseonly'> " << std::endl; 
		return 1; 
	}
	moving_boundary::MovingBoundaryParabolicProblem problem;
	//moving_boundary::ProblemPackage package;
	std::unique_ptr<moving_boundary::MovingBoundaryElementClient> reportClient;
	std::unique_ptr<moving_boundary::MovingBoundaryTimeClient> persistClient;
	//const char * const filename = argv[1];
	//const char * outname = argv[2];
	if (parseOnly) {
		std::cout <<  "parse XML only mode" << std::endl;
	}
	try {
		tinyxml2::XMLDocument doc;
		doc.LoadFile(filename.c_str( ));
		if (doc.ErrorID( ) != tinyxml2::XML_SUCCESS) {
			std::cerr <<  "Error " << doc.ErrorID( ) << " loading  " << filename << std::endl;
			return 2; 
		}
		const tinyxml2::XMLElement & root = *doc.RootElement( );
		if (!strcmp(root.Name( ),XML_ROOT_NAME) == 0) {
			std::cerr <<  "Invalid XML root identifier " << root.Name( ) << ", " << XML_ROOT_NAME << " expected" << std::endl;
			return 3; 
		}
		setupTrace(root);
		setupMatlabDebug(root);

		using moving_boundary::MovingBoundarySetup;
		if (restorename.empty( )) {
			auto mbs = MovingBoundarySetup::setupProblem(root);
			problem = moving_boundary::MovingBoundaryParabolicProblem(mbs);
		}
		else {
			try {
				problem = moving_boundary::StateClient::restore(restorename);
			}
			catch (std::exception & e) {
				std::cerr <<  argv[0] << " caught exception " << e.what( ) << " reading " << restorename << std::endl; 
				return 6;
			}
		}
		reportClient.reset( setupReportClient(root, outname, problem) ); 
		problem.add(*reportClient);
		setupHeartbeat(root,problem);

		persistClient.reset( new moving_boundary::StateClient(problem,"sim",0.1,0.01) );

	}
	catch (std::exception & e) {
		std::cerr <<  argv[0] << " caught exception " << e.what( ) << " reading " << filename << std::endl; 
		return 4;
	}
	catch (...) {
		std::cerr <<  argv[0] << " caught unknown exception" << " reading " << filename << std::endl; 
		return 5;
	}
	if (parseOnly) {
		std::cout <<  filename << " validated" << std::endl;
		return 0;
	}

	try {
		problem.run( );
	}
	catch (std::exception & e) {
		std::cerr <<  argv[0] << " caught exception " << e.what( ) << " reading " << filename << std::endl; 
		return 6;
	}
	catch (...) {
		std::cerr <<  argv[0] << " caught unknown exception" << " reading " << filename << std::endl; 
		return 4;
	}
	std::cout << "MovingBoundary " << filename << " complete " << std::endl;
}

namespace {


	moving_boundary::MovingBoundaryElementClient *setupReportClient(const XMLElement &root, const std::string & filename , moving_boundary::MovingBoundaryParabolicProblem &mbpp) {
		try {
			const moving_boundary::MovingBoundarySetup & mbs = mbpp.setup( );
			const XMLElement & report = vcell_xml::get(root,"report");
			H5::H5File output;
			if (!filename.empty( )) {
				output = vcellH5::VH5File(filename.c_str( ),H5F_ACC_TRUNC|H5F_ACC_RDWR);
			}
			else {
				std::string filename = vcell_xml::convertChildElement<std::string>(report,"outputFilename");
				bool deleteExisting = vcell_xml::convertChildElementWithDefault<bool>(report,"deleteExisting",false);
				if (deleteExisting) {
					unlink(filename.c_str( ));
				}
				output = vcellH5::VH5File(filename.c_str( ),H5F_ACC_TRUNC|H5F_ACC_RDWR);
			}
			const char *datasetName = nullptr;
			std::pair<bool,std::string> dnq = vcell_xml::queryElement<std::string>(report,"datasetName");
			if (dnq.first) {
				datasetName = dnq.second.c_str( );
			}

			std::vector<moving_boundary::TimeReport *> timeReports;

			std::pair<bool,unsigned int> nr = vcell_xml::queryElement<unsigned int>(report,"numberReports");
			if (nr.first) {
				std::cerr << "numberReports XML element deprecated" << std::endl;
				const double startTime = vcell_xml::convertChildElementWithDefault<double>(report,"startTime",0);
				unsigned int nts = mbpp.numberTimeSteps( );
				unsigned int rs = nts /  nr.second;
				if (startTime >0) { //if not beginning, scale based on fraction of time reported
					const double end = mbpp.endTime( );
					unsigned int scaled = static_cast<unsigned int>(nts * (end -startTime) / end);
					rs = scaled / nr.second; 
				}
				if (rs < 1) {
					rs = 1;
				}
				timeReports.push_back( new moving_boundary::TimeReportStep(startTime,rs));
			}

			const XMLElement * timeReport = report.FirstChildElement("timeReport"); 
			while(timeReport != nullptr) {
				const int NOT_THERE = -1;
				const double startTime = vcell_xml::convertChildElement<double>(*timeReport,"startTime");
				const long step = vcell_xml::convertChildElementWithDefault<long>(*timeReport,"step", NOT_THERE);
				const double interval = vcell_xml::convertChildElementWithDefault<double>(*timeReport,"interval",NOT_THERE);
				bool quiet = timeReport->FirstChildElement("quiet") != nullptr;
				int nsubs = 0;


				if (step != NOT_THERE) {
					nsubs++;
					timeReports.push_back( new moving_boundary::TimeReportStep(startTime,step) );
				}
				if (interval != NOT_THERE) {
					nsubs++;
					timeReports.push_back( new moving_boundary::TimeReportInterval(startTime,interval) );
				}
				if (quiet) {
					nsubs++;
					timeReports.push_back( new moving_boundary::TimeReportQuiet(startTime) );
				}
				if (nsubs != 1) {
					throw std::invalid_argument("XML error exactly one of <step>, <interval>, or <quiet> must be specified per timeReport element");
				}
				timeReport = timeReport->NextSiblingElement("timeReport");
			}

			moving_boundary::World<moving_boundary::CoordinateType,2> &world = moving_boundary::World<moving_boundary::CoordinateType,2>::get( );
			moving_boundary::NHDF5Client<> *client =  new moving_boundary::NHDF5Client<>(output,world,mbpp,datasetName, timeReports);
			client->addInitial(mbs);
			const XMLElement * const annotateSection = report.FirstChildElement("annotation"); 
			if (annotateSection != nullptr) {
				const XMLElement *annotateElement = annotateSection->FirstChildElement( );
				while (annotateElement != nullptr) {
					const char *const name = annotateElement->Name( );
					const std::string value = vcell_xml::convertElement<std::string>(*annotateElement);
					client->annotate(name,value);
					annotateElement = annotateElement->NextSiblingElement( );
				}
			}
			return client;
		} 
		catch (H5::Exception &h5e) {
			std::string h5Msg = h5e.getDetailMsg( );
			h5Msg += " ";
			h5Msg += h5e.getFuncName( );
			throw new std::runtime_error(h5Msg);
		}
	}
	void setupTrace(const XMLElement &root) {
		const tinyxml2::XMLElement *trace = root.FirstChildElement("trace");
		if (trace != nullptr) {
			using vcell_xml::convertChildElementWithDefault;
			std::string tracefilename = convertChildElementWithDefault<std::string>(*trace,"traceFilename", "trace.txt");
			std::string levelStr = convertChildElementWithDefault<std::string>(*trace,"level","fatal");

			using vcell_util::Logger;
			Logger & logger = Logger::get( );
			traceFileDestination.reset(new vcell_util::FileDest(tracefilename.c_str( )) );
			logger.setDestination(*traceFileDestination);
			Logger::Level level = Logger::readLevel(levelStr.c_str( ));
			logger.set(level);
			//set specific level keys
			const char * const KEYSET = "keyset";
			const tinyxml2::XMLElement *keySet = trace->FirstChildElement(KEYSET);
			while (keySet != nullptr) {
				using vcell_xml::convertChildElement;
				bool enabled = convertChildElement<bool>(*keySet,"enabled");
				std::string key = convertChildElement<std::string>(*keySet,"key");
				Logger::Key k = Logger::readKey(key.c_str( ));
				logger.set(k,enabled);
				keySet = keySet->NextSiblingElement(KEYSET);
			}
		}
	}

	void setupMatlabDebug(const XMLElement &root) {
		const tinyxml2::XMLElement *trace = root.FirstChildElement("matlabDebug");
		if (trace != nullptr) {
			const char * const TOKEN = "token";
			const tinyxml2::XMLElement *token = trace->FirstChildElement(TOKEN);
			while (token != nullptr) {
				using vcell_xml::convertChildElement;
				const std::string spec = vcell_xml::convertElement<std::string>(*token);
				matlabBridge::MatLabDebug::activate(spec);
				token = token->NextSiblingElement(TOKEN);
			}
		}
	}
	void setupHeartbeat(const XMLElement &root,moving_boundary::MovingBoundaryParabolicProblem & mbpp) {
		const tinyxml2::XMLElement *hb = root.FirstChildElement("heartbeat");
		if (hb != nullptr) {
			using vcell_xml::convertChildElementWithDefault;
			size_t heartbeat = convertChildElementWithDefault<size_t>(*hb,"generations",0);
			std::string symbol = convertChildElementWithDefault<std::string>(*hb,"symbol",".");
			mbpp.setHeartbeat(heartbeat,symbol);
		}
	}
}


