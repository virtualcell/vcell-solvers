#include <MPoint.h>
#include <iostream>
#include <vcellxml.h>
#include <vhdf5/file.h>
#include <MovingBoundaryParabolicProblem.h>
#include <HDF5Client.h>
#include <Logger.h>
#include <boundaryProviders.h>
using tinyxml2::XMLElement;

namespace {
	const char * const XML_ROOT_NAME = "vcellfrontiersetup";

	typedef std::pair<int,H5::H5File> PReturn; 
	spatial::MovingBoundarySetup setupProblem(const XMLElement &root); 
	void setupTrace(const XMLElement &root); 
	spatial::MovingBoundaryClient *setupClient(const XMLElement &root, const char *filename, spatial::MovingBoundaryParabolicProblem &mbpp,
		const spatial::MovingBoundarySetup &); 
	void runSimulation(H5::H5File & output);

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
		<< "FrontTierSolver version $URL$" VCELLSVNQUOTE(SVNVERSION) 
		<< std::endl; 
	if (argc < 2) {
		std::cerr << "Usage: " << argv[0] << " [xml input file] <hdf5 output file name> " << std::endl; 
		return 1; 
	}
	spatial::MovingBoundaryParabolicProblem mbpp;
	std::auto_ptr<spatial::MovingBoundaryClient> client;
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

		spatial::MovingBoundarySetup mbs = setupProblem(root);
		mbpp = spatial::MovingBoundaryParabolicProblem(mbs);
		client = std::auto_ptr<spatial::MovingBoundaryClient>( setupClient(root, argv[2], mbpp, mbs) ); 
	}
	catch (std::exception & e) {
		std::cerr <<  argv[0] << " caught exception " << e.what( ) << " reading " << filename << std::endl; 
		return 4;
	}
	catch (...) {
		std::cerr <<  argv[0] << " caught unknown exception" << " reading " << filename << std::endl; 
		return 4;
	}

	try {
		mbpp.run(*client);
	}
	catch (std::exception & e) {
		std::cerr <<  argv[0] << " caught exception " << e.what( ) << " reading " << filename << std::endl; 
	}
	catch (...) {
		std::cerr <<  argv[0] << " caught unknown exception" << " reading " << filename << std::endl; 
	}
}

namespace {

	void readLimits(const tinyxml2::XMLElement &element, spatial::GeoLimit & limits) {
		limits.low = vcell_xml::convertChildElement<double>(element,"low");
		limits.high = vcell_xml::convertChildElement<double>(element,"high");
	}
	spatial::MovingBoundarySetup setupProblem(const XMLElement &root) {
		spatial::MovingBoundarySetup mbSetup; 
		const XMLElement & prob = vcell_xml::get(root,"problem");

		std::array<spatial::GeoLimit,2> limits;
		const tinyxml2::XMLElement & xlimits = vcell_xml::get(prob,"xLimits"); 
		readLimits(xlimits,limits[0]);
		const tinyxml2::XMLElement & ylimits = vcell_xml::get(prob,"yLimits"); 
		readLimits(ylimits,limits[1]);
		spatial::World<double,2>::get( ).init(limits,true);

		using vcell_xml::convertChildElement;

		mbSetup.numNodesX = convertChildElement<unsigned int>(prob,"numNodesX");
		mbSetup.numNodesY = convertChildElement<unsigned int>(prob,"numNodesY");
		mbSetup.frontToNodeRatio = convertChildElement<unsigned int>(prob,"frontToNodeRatio");
		mbSetup.maxTime = convertChildElement<double>(prob,"maxTime");
		mbSetup.numberTimeSteps = convertChildElement<unsigned int>(prob,"numberTimeSteps");
		mbSetup.diffusionConstant = convertChildElement<double>(prob,"diffusionConstant");
		mbSetup.advectVelocityFunctionStrX = convertChildElement<std::string>(prob,"advectVelocityFunctionX");
		mbSetup.advectVelocityFunctionStrY = convertChildElement<std::string>(prob,"advectVelocityFunctionY");
		mbSetup.concentrationFunctionStr = convertChildElement<std::string>(prob,"concentrationFunction");

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
			mbSetup.alternateFrontProvider = spatial::frontFromXML(*altFront);
			std::cout << mbSetup.alternateFrontProvider->describe( ) << std::endl;
		}
		return mbSetup; 
	}

	spatial::MovingBoundaryClient *setupClient(const XMLElement &root, const char *filename, spatial::MovingBoundaryParabolicProblem &mbpp,
		const spatial::MovingBoundarySetup & mbs) {
		try {
			const XMLElement & report = vcell_xml::get(root,"report");
			unsigned int numberReports = vcell_xml::convertChildElement<unsigned int>(report,"numberReports");
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
			const double startTime = vcell_xml::convertChildElementWithDefault<double>(report,"startTime",0);

			spatial::NHDF5Client<> *client =  new spatial::NHDF5Client< >(output,mbpp,numberReports, datasetName, startTime);
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
			const tinyxml2::XMLElement *keySet = trace->FirstChildElement("keyset");
			while (keySet != nullptr) {
				using vcell_xml::convertChildElement;
				levelStr = convertChildElement<std::string>(*keySet,"level");
				level = Logger::read(levelStr.c_str( ));
				std::string key = convertChildElement<std::string>(*keySet,"key");
				logger.set(level,key.c_str( ));
				keySet = keySet->NextSiblingElement("keyset");
			}
		}
	}
	/*
	void runSimulation(H5::H5File & output) {
	spatial::MovingBoundaryParabolicProblem mbpp(mbSetup);
	mbpp.run(client);
	}
	*/
}
