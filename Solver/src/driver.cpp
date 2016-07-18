#include <MPoint.h>
#include <iostream>
#include <vcellxml.h>
#include <vhdf5/file.h>
#include <Logger.h>
#include <boundaryProviders.h>
#include <StateClient.h>
#include <MBridge/MatlabDebug.h>
#include <tclap/CmdLine.h>
#include <ReportClient.h>
#include <TextReportClient.h>
#include <version.h>
#include <Timer.h>
/**
* usings and typedefs
*/
using tinyxml2::XMLElement;
using moving_boundary::MovingBoundaryElementClient;
using moving_boundary::MovingBoundarySetup;

namespace {
	/**
	* constants
	*/
	const char * const XML_ROOT_NAME = "MovingBoundarySetup";


	/**
	* forward declarations of functions in file
	*/
	void setupTrace(const XMLElement &root); 
	void setupMatlabDebug(const XMLElement &root); 
	void setupProgress(const XMLElement &root,moving_boundary::MovingBoundaryParabolicProblem & mbpp); 

	/**
	* shared variables 
	*/
	std::auto_ptr<vcell_util::FileDest> traceFileDestination; 
}

int main(int argc, char *argv[])
{
	const vcell_util::Version & version = vcell_util::Version::get( );
	std::cout 
		<< "MovingBoundarySolver version $URL$" 
		<< "revision " << version.svn << ' ' << version.compileDate << std::endl; 

	std::string filename;
	std::string outname;
	std::string restorename;
	bool parseOnly;
	bool configPresent;
	int Nx = -1;
	namespace tclap = TCLAP;
	try {
		using namespace TCLAP; 
		CmdLine cmd("Moving boundary solve",' ',version.svn);
		cmd.setExceptionHandling(false);
		ValueArg<std::string> config("c","config","Input XML file name",false,"","string",cmd);
		ValueArg<std::string> output("o","output","HDF5 output file name",false,"","string",cmd);
		ValueArg<std::string> restore("r","restore","stored state file name",false,"","string",cmd);
		MultiArg<std::string> ignore("i","ignore","ignore me",false,"string",cmd);
		ValueArg<int> paramNx("n", "Nx", "Nx", false, -1, "int", cmd);
		SwitchArg ponly("p","parseonly","Parse XML without running simulation",cmd);
		cmd.parse(argc,argv);
		filename = config.getValue( );
		outname = output.getValue( );
		restorename = restore.getValue( );
		parseOnly = ponly.getValue( );
		Nx = paramNx.getValue();
		configPresent = config.isSet( );
		if (!configPresent && !restore.isSet( ) ) { 
			std::cerr << "error, either -" << config.getName( ) << " or -" << restore.getName( )  
				<< " must be set " << std::endl;
			std::cerr << "-h for full list of command line options" << std::endl;
			return 4;
		}
	} catch(tclap::ArgException  &ae) {
		std::cerr << "error " << ae.error( ) << " arg " << ae.argId( ) << std::endl;
		return 3;
	} catch (tclap::ExitException &ee) {
		return ee.getExitStatus( );
	}

	moving_boundary::MovingBoundaryParabolicProblem problem;
	//moving_boundary::ProblemPackage package;

	//const char * const filename = argv[1];
	//const char * outname = argv[2];
	if (parseOnly) {
		std::cout <<  "parse XML only mode" << std::endl;
	}
	try {
		tinyxml2::XMLDocument doc;
		if (configPresent) {
			doc.LoadFile(filename.c_str( ));
			if (doc.ErrorID( ) != tinyxml2::XML_SUCCESS) {
				std::cerr <<  "Error " << doc.ErrorID( ) << " loading " << filename << std::endl;
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
				auto mbs = MovingBoundarySetup::setupProblem(root, Nx);
				problem = moving_boundary::MovingBoundaryParabolicProblem(mbs);
				moving_boundary::ReportClient::setup(root, outname, problem);
			}
		}
		if (!restorename.empty( )) {

			try {
				using moving_boundary::StateClient;
				StateClient::ProblemState pState =  StateClient::restore(restorename);
				problem = pState.problem;
			}
			catch (std::exception & e) {
				std::cerr <<  argv[0] << " caught exception " << e.what( ) << " reading " << restorename << std::endl; 
				return 6;
			}
		}

		if (configPresent) {
			setupProgress(*doc.RootElement( ),problem);
		}
	}
	catch (std::exception & e) {
		std::cerr <<  argv[0] << " caught exception " << e.what( ) << " reading " << filename << std::endl; 
		return 4;
	}
	catch (H5::Exception & e) {
		std::cerr <<  argv[0] << " caught H5 exception" << e.getCDetailMsg( ) << " reading " << filename << std::endl; 
		return 7;
	}
	catch (...) {
		std::cerr <<  argv[0] << " caught unknown exception reading " << filename << std::endl; 
		return 5;
	}
	if (parseOnly) {
		std::cout <<  filename << " validated" << std::endl;
		return 0;
	}

	int returnCode = 0;
	try {
		problem.run( );
		std::cout << "MovingBoundary input " << filename << ", output " << problem.getOutputFiles() << " finished" << std::endl;
	}
	catch (std::exception & e) {
		std::cerr <<  argv[0] << " caught exception while running " << filename << ": " << e.what( ) << std::endl;
		returnCode = 6;
	}
	catch (const std::string & e) {
		std::cerr <<  argv[0] << " caught exception while running " << filename << ": " << e << std::endl;
		returnCode = 6;
	}
	catch (...) {
		std::cerr <<  argv[0] << " caught unknown exception while running " << filename << std::endl;
		returnCode = 4;
	}
	return returnCode;
}

namespace {

	void setupTrace(const XMLElement &root) {
		const tinyxml2::XMLElement *trace = root.FirstChildElement("trace");
		if (trace != nullptr) {
			using vcell_xml::convertChildElementWithDefault;
			std::string tracefilename = convertChildElementWithDefault<std::string>(*trace,"traceFilename", "");
			std::string levelStr = convertChildElementWithDefault<std::string>(*trace,"level","fatal");

			using vcell_util::Logger;
			Logger & logger = Logger::get( );
			if (tracefilename.empty())
			{
				vcell_util::StdoutDest* stdoutDest = new vcell_util::StdoutDest();
				logger.setDestination(*stdoutDest);
			}
			else
			{
				traceFileDestination.reset(new vcell_util::FileDest(tracefilename.c_str( )) );
				logger.setDestination(*traceFileDestination);
			}
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
			if (token != nullptr) {
				std::ofstream *out = new std::ofstream("mdebug.m");
				matlabBridge::MatLabDebug::setDebug(*out);
			}
			while (token != nullptr) {
				using vcell_xml::convertChildElement;
				const std::string spec = vcell_xml::convertElement<std::string>(*token);
				matlabBridge::MatLabDebug::activate(spec);
				token = token->NextSiblingElement(TOKEN);
			}
		}
	}
	void setupProgress(const XMLElement &root,moving_boundary::MovingBoundaryParabolicProblem & mbpp) {
		const tinyxml2::XMLElement *prog = root.FirstChildElement("progress");
		if (prog != nullptr) {
			const unsigned short percent = vcell_xml::convertChildElement<unsigned short>(*prog,"percent");
			const bool estimateProgress =vcell_xml::convertChildElementWithDefault<bool>(*prog,"estimateProgress",false);
			mbpp.reportProgress(percent,estimateProgress);
		}
	}
}


