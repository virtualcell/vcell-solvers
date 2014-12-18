#include <MovingBoundarySetup.h>
#include <boundaryProviders.h>
#include <persistcontainer.h>
#include <vcellxml.h>

using namespace moving_boundary;

void MovingBoundarySetup::registerType( ) {
	vcell_persist::Registrar::reg<MovingBoundarySetup>("Moving Boundary Setup");
}

MovingBoundarySetup::MovingBoundarySetup(std::istream &is)
	:alternateFrontProvider(nullptr) {
	vcell_persist::Token::check<MovingBoundarySetup>(is);
	vcell_persist::binaryRead(is,frontToNodeRatio);
	vcell_persist::binaryRead(is,maxTime);
	vcell_persist::binaryRead(is,numberTimeSteps);
	vcell_persist::binaryRead(is,timeStep);
	vcell_persist::binaryRead(is,hardTime);
	vcell_persist::StdString<>::restore(is,levelFunctionStr);
	vcell_persist::StdString<>::restore(is,advectVelocityFunctionStrX);
	vcell_persist::StdString<>::restore(is,advectVelocityFunctionStrY);
	vcell_persist::StdString<>::restore(is,frontVelocityFunctionStrX);
	vcell_persist::StdString<>::restore(is,frontVelocityFunctionStrY);
	//PWORK vcell_persist::restoreStrings<unsigned short>(is, concentrationFunctionStrings);
	vcell_persist::binaryRead(is,diffusionConstant);
	/*
	bool hasProvider;
	vcell_persist::binaryRead(is,hasProvider);
	if (hasProvider) {
		alternateFrontProvider = restoreFrontProvider(is);
	}
	*/
}
MovingBoundarySetup::~MovingBoundarySetup() {
	speciesSpecs.resize(0);
}


void MovingBoundarySetup::persist(std::ostream &os) const {
	vcell_persist::Token::insert<MovingBoundarySetup>(os);
	vcell_persist::binaryWrite(os,frontToNodeRatio);
	vcell_persist::binaryWrite(os,maxTime);
	vcell_persist::binaryWrite(os,numberTimeSteps);
	vcell_persist::binaryWrite(os,timeStep);
	vcell_persist::binaryWrite(os,hardTime);
	vcell_persist::StdString<>::save(os,levelFunctionStr);
	vcell_persist::StdString<>::save(os,advectVelocityFunctionStrX);
	vcell_persist::StdString<>::save(os,advectVelocityFunctionStrY);
	vcell_persist::StdString<>::save(os,frontVelocityFunctionStrX);
	vcell_persist::StdString<>::save(os,frontVelocityFunctionStrY);
	//PWORK vcell_persist::saveStrings<unsigned short>(os,concentrationFunctionStrings);
	vcell_persist::binaryWrite(os,diffusionConstant);
	throw new std::domain_error("not updated for species spec");
	/*
	const bool hasProvider = ( alternateFrontProvider != nullptr );
	vcell_persist::binaryWrite(os,hasProvider);
	if (hasProvider) {
		alternateFrontProvider->persist(os);
	}
	*/
}
using tinyxml2::XMLElement;
	//MovingBoundarySetup setupProblem(const XMLElement &root); 

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
}

moving_boundary::MovingBoundarySetup MovingBoundarySetup::setupProblem(const XMLElement &root) {
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

	moving_boundary::Universe<2>::get( ).init(limits,numNodes);


	mbSetup.frontToNodeRatio = convertChildElement<unsigned int>(prob,"frontToNodeRatio");
	mbSetup.maxTime = convertChildElement<double>(prob,"maxTime");
	mbSetup.diffusionConstant = convertChildElement<double>(prob,"diffusionConstant");
	mbSetup.advectVelocityFunctionStrX = convertChildElement<std::string>(prob,"advectVelocityFunctionX");
	mbSetup.advectVelocityFunctionStrY = convertChildElement<std::string>(prob,"advectVelocityFunctionY");
	//mbSetup.concentrationFunctionStr = convertChildElement<std::string>(prob,"concentrationFunction");

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
		mbSetup.alternateFrontProvider = ::frontFromXML(*altFront);
		std::cout << mbSetup.alternateFrontProvider->describe( ) << std::endl;
	}
	const XMLElement & physiologyElement = vcell_xml::get(prob,"physiology");
	const XMLElement *spE = physiologyElement.FirstChildElement("species");
	size_t sNumber = 0;
	std::ostringstream defaultNameS;
	std::string name;
	while (spE != nullptr) {
		//get name, or design default
		const char * n  = spE->Attribute("name");
		if (n != nullptr) {
			name = n;
		}
		else {
			defaultNameS.rdbuf( )->pubseekpos(0); 
			defaultNameS << 'u' << sNumber << std::ends;
			name = defaultNameS.str( );
		}
		++sNumber;

		//get other terms 
		std::string source = vcell_xml::convertChildElement<std::string>(*spE,"source");
		std::string init = vcell_xml::convertChildElementWithDefault<std::string>(*spE,"source", source);
		mbSetup.speciesSpecs.push_back(SpeciesSpecification(name,init,source));

		spE = spE->NextSiblingElement("species");
	}

	return mbSetup; 
}

