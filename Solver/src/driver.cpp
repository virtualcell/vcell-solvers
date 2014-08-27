#include <MPoint.h>
#include <iostream>
#include <vcellxml.h>
#include <vhdf5/file.h>
#include <MovingBoundaryParabolicProblem.h>
#include <HDF5Client.h>
#include <Logger.h>
#include <boundaryProviders.h>
#include <MBridge/MatlabDebug.h>
using tinyxml2::XMLElement;

namespace {
	/**
	* constants
	*/
	const char * const XML_ROOT_NAME = "vcellfrontiersetup";
	/**
	* usings and typedefs
	*/
	using moving_boundary::MovingBoundaryClient;
	using moving_boundary::MovingBoundarySetup;
	using moving_boundary::MovingBoundaryParabolicProblem;

	/**
	* forward declarations of functions in file
	*/
	MovingBoundarySetup setupProblem(const XMLElement &root); 
	void setupTrace(const XMLElement &root); 
	MovingBoundaryClient *setupClient(const XMLElement &root, const char *filename, MovingBoundaryParabolicProblem &mbpp, const moving_boundary::MovingBoundarySetup &); 
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
	if (argc < 2) {
		std::cerr << "Usage: " << argv[0] << " [xml input file] <hdf5 output file name> " << std::endl; 
		return 1; 
	}
	moving_boundary::MovingBoundaryParabolicProblem mbpp;
	std::auto_ptr<moving_boundary::MovingBoundaryClient> client;
	const char * const filename = argv[1];
	try {
		tinyxml2::XMLDocument doc;
		doc.LoadFile(filename);
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

		moving_boundary::MovingBoundarySetup mbs = setupProblem(root);
		mbpp = moving_boundary::MovingBoundaryParabolicProblem(mbs);
		client = std::auto_ptr<moving_boundary::MovingBoundaryClient>( setupClient(root, argv[2], mbpp, mbs) ); 
		setupHeartbeat(root,mbpp);
	}
	catch (std::exception & e) {
		std::cerr <<  argv[0] << " caught exception " << e.what( ) << " reading " << filename << std::endl; 
		return 4;
	}
	catch (...) {
		std::cerr <<  argv[0] << " caught unknown exception" << " reading " << filename << std::endl; 
		return 5;
	}

	try {
		mbpp.run(*client);
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

	bool convertTrueOrFalse(const char *v) {
		std::string in(v);
		std::transform(in.begin( ),in.end( ),in.begin( ), ::tolower);
		if (in.compare("true") == 0) {
			return true;
		}
		if (in.compare("false") == 0) {
			return false;
		}
		VCELL_EXCEPTION(domain_error,"invalid boolean string " << v << ", must be 'true' or 'false'");
	}

	void readLimits(const tinyxml2::XMLElement &element, spatial::GeoLimit & limits) {
		double low = vcell_xml::convertChildElement<double>(element,"low");
		double high = vcell_xml::convertChildElement<double>(element,"high");
		limits = spatial::GeoLimit(low,high);
	}
	moving_boundary::MovingBoundarySetup setupProblem(const XMLElement &root) {
		using vcell_xml::convertChildElement;

		moving_boundary::MovingBoundarySetup mbSetup; 
		const XMLElement & prob = vcell_xml::get(root,"problem");

		std::array<spatial::GeoLimit,2> limits;
		const tinyxml2::XMLElement & xlimits = vcell_xml::get(prob,"xLimits"); 
		readLimits(xlimits,limits[0]);
		const tinyxml2::XMLElement & ylimits = vcell_xml::get(prob,"yLimits"); 
		readLimits(ylimits,limits[1]);
		std::array<unsigned short,2> numNodes;
		numNodes[0]  = convertChildElement<unsigned short>(prob,"numNodesX");
		numNodes[1]  = convertChildElement<unsigned short>(prob,"numNodesY");

		moving_boundary::Universe<2>::get( ).init(limits,numNodes, true);


		mbSetup.frontToNodeRatio = convertChildElement<unsigned int>(prob,"frontToNodeRatio");
		mbSetup.maxTime = convertChildElement<double>(prob,"maxTime");
		mbSetup.diffusionConstant = convertChildElement<double>(prob,"diffusionConstant");
		mbSetup.advectVelocityFunctionStrX = convertChildElement<std::string>(prob,"advectVelocityFunctionX");
		mbSetup.advectVelocityFunctionStrY = convertChildElement<std::string>(prob,"advectVelocityFunctionY");
		mbSetup.concentrationFunctionStr = convertChildElement<std::string>(prob,"concentrationFunction");

		mbSetup.numberTimeSteps = vcell_xml::convertChildElementWithDefault<unsigned int>(prob,"numberTimeSteps",0);
		mbSetup.timeStep = vcell_xml::convertChildElementWithDefault<double>(prob,"timeStep",0);
		mbSetup.hardTime = convertTrueOrFalse(vcell_xml::convertChildElementWithDefault<const char *>(prob,"hardTime","false"));

		using vcell_xml::convertChildElementWithDefault;
		mbSetup.frontVelocityFunctionStrX = 
			convertChildElementWithDefault<std::string>(prob,"frontVelocityFunctionX", mbSetup.advectVelocityFunctionStrX);
		mbSetup.frontVelocityFunctionStrY = 
			convertChildElementWithDefault<std::string>(prob,"frontVelocityFunctionY", mbSetup.advectVelocityFunctionStrY);

		std::pair<bool,std::string> lvq = vcell_xml::queryElement<std::string>(prob,"levelFunction");
		if (lvq.first) {
			 mbSetup.levelFunctionStr = lvq.second;
		}
		const tinyxml2::XMLElement *altFront = prob.FirstChildElement("specialFront");
		if (altFront != nullptr) {
			mbSetup.alternateFrontProvider = moving_boundary::frontFromXML(*altFront);
			std::cout << mbSetup.alternateFrontProvider->describe( ) << std::endl;
		}
		return mbSetup; 
	}

	moving_boundary::MovingBoundaryClient *setupClient(const XMLElement &root, const char *filename, moving_boundary::MovingBoundaryParabolicProblem &mbpp,
		const moving_boundary::MovingBoundarySetup & mbs) {
		try {
			const XMLElement & report = vcell_xml::get(root,"report");
			H5::H5File output;
			if (filename != nullptr) {
				output = vcellH5::VH5File(filename,H5F_ACC_TRUNC|H5F_ACC_RDWR);
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
			Logger::Level level = Logger::read(levelStr.c_str( ));
			logger.set(level);
			//set specific level keys
			const char * const KEYSET = "keyset";
			const tinyxml2::XMLElement *keySet = trace->FirstChildElement(KEYSET);
			while (keySet != nullptr) {
				using vcell_xml::convertChildElement;
				levelStr = convertChildElement<std::string>(*keySet,"level");
				level = Logger::read(levelStr.c_str( ));
				std::string key = convertChildElement<std::string>(*keySet,"key");
				logger.set(level,key.c_str( ));
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


