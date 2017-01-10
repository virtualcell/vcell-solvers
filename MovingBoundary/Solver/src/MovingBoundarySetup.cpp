#include <MovingBoundarySetup.h>
#include <Physiology.h>
#include <VolumeSubdomain.h>
#include <PointSubdomain.h>
#include <boundaryProviders.h>
#include <persistcontainer.h>
#include <vcellxml.h>
#include <Logger.h>
#include <SimulationMessaging.h>

using namespace moving_boundary;

//MovingBoundarySetup
void MovingBoundarySetup::registerType( ) {
	vcell_persist::Registrar::reg<MovingBoundarySetup>("Moving Boundary Setup");
}

MovingBoundarySetup::MovingBoundarySetup(std::istream &is)
	:alternateFrontProvider(nullptr) {
	vcell_persist::Token::check<MovingBoundarySetup>(is);
	vcell_persist::binaryRead(is,frontToNodeRatio);
	vcell_persist::binaryRead(is,maxTime);
	vcell_persist::binaryRead(is,frontTimeStep);
	vcell_persist::binaryRead(is,solverTimeStep);
	vcell_persist::binaryRead(is,hardTime);
	vcell_persist::StdString<>::restore(is,levelFunctionStr);
	vcell_persist::StdString<>::restore(is,frontVelocityFunctionStrX);
	vcell_persist::StdString<>::restore(is,frontVelocityFunctionStrY);
	vcell_persist::binaryRead(is,diffusionConstant);
	bool hasProvider;
	vcell_persist::binaryRead(is,hasProvider);
	if (hasProvider) {
		alternateFrontProvider = restoreFrontProvider(is);
	}
}
MovingBoundarySetup::~MovingBoundarySetup() {
}


void MovingBoundarySetup::persist(std::ostream &os) const {
	vcell_persist::Token::insert<MovingBoundarySetup>(os);
	vcell_persist::binaryWrite(os,frontToNodeRatio);
	vcell_persist::binaryWrite(os,maxTime);
	vcell_persist::binaryWrite(os,frontTimeStep);
	vcell_persist::binaryWrite(os,solverTimeStep);
	vcell_persist::binaryWrite(os,hardTime);
	vcell_persist::StdString<>::save(os,levelFunctionStr);
	vcell_persist::StdString<>::save(os,frontVelocityFunctionStrX);
	vcell_persist::StdString<>::save(os,frontVelocityFunctionStrY);
	vcell_persist::binaryWrite(os,diffusionConstant);
	const bool hasProvider = ( alternateFrontProvider != nullptr );
	vcell_persist::binaryWrite(os,hasProvider);
	if (hasProvider) {
		alternateFrontProvider->persist(os);
	}
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
namespace {
	/**
	* set value to default if it is zero
	* @param value to set
	* @param  deflat default value
	* @return true if substituion made
	*/
	void useDefaultIfZero(std::string & value, const std::string & deflat) {
		if (value == "0") {
			value = deflat;
		}
	}
}

moving_boundary::MovingBoundarySetup MovingBoundarySetup::setupProblem(const XMLElement &root, int taskId, int paramNx) {
	using vcell_xml::convertChildElement;

#ifdef USE_MESSAGING
	if (taskId >= 0)
	{
		const XMLElement* jms = root.FirstChildElement("jms");
		if (jms != nullptr)
		{
			char *broker = new char[256];
			char *smqusername = new char[256];
			char *password = new char[256];
			char *qname = new char[256];
			char *tname = new char[256];
			char *vcusername = new char[256];
			int simKey, jobIndex;

			std::string str = vcell_xml::convertChildElement<std::string>(*jms, "broker");
			std::strcpy(broker, str.c_str());

			str = vcell_xml::convertChildElement<std::string>(*jms,"jmsUser");
			std::strcpy(smqusername, str.c_str());

			str = vcell_xml::convertChildElement<std::string>(*jms,"pw");
			std::strcpy(password, str.c_str());

			str = vcell_xml::convertChildElement<std::string>(*jms,"queue");
			std::strcpy(qname, str.c_str());

			str = vcell_xml::convertChildElement<std::string>(*jms,"topic");
			std::strcpy(tname, str.c_str());

			str = vcell_xml::convertChildElement<std::string>(*jms,"vcellUser");
			std::strcpy(vcusername, str.c_str());

			simKey = vcell_xml::convertChildElement<unsigned int>(*jms, "simKey");
			jobIndex = vcell_xml::convertChildElement<unsigned int>(*jms, "jobIndex");

			SimulationMessaging::create(broker, smqusername, password, qname, tname, vcusername, simKey, jobIndex, taskId);
		}
	}
#endif

	moving_boundary::MovingBoundarySetup mbSetup; 
	const XMLElement & prob = vcell_xml::get(root,"problem");

	std::array<spatial::GeoLimit,2> limits;
	const tinyxml2::XMLElement & xlimits = vcell_xml::get(prob,"xLimits"); 
	readLimits(xlimits,limits[0]);
	mbSetup.extentX = CoordVect(limits[0].low(), limits[0].high());

	const tinyxml2::XMLElement & ylimits = vcell_xml::get(prob,"yLimits"); 
	readLimits(ylimits,limits[1]);
	mbSetup.extentY = CoordVect(limits[1].low(), limits[1].high());

	std::array<unsigned short,2> numNodes;
	if (paramNx == -1)
	{
		numNodes[0]  = convertChildElement<unsigned short>(prob,"numNodesX");
		numNodes[1]  = convertChildElement<unsigned short>(prob,"numNodesY");
	}
	else
	{
		numNodes[0] = paramNx;
		numNodes[1] = paramNx;
	}
	mbSetup.Nx = IndexVect(numNodes[0], numNodes[1]);

	moving_boundary::Universe<2>::get( ).init(limits,numNodes);


	mbSetup.frontToNodeRatio = convertChildElement<unsigned int>(prob,"frontToNodeRatio");
	mbSetup.maxTime = convertChildElement<double>(prob,"maxTime");
	std::pair<bool,std::string> outputPair = vcell_xml::queryElement<std::string>(prob,"outputTimeStep");
	if (outputPair.first)
	{
		mbSetup.outputTimeStep = outputPair.second;
	}

	std::pair<bool,std::string> tsPair = vcell_xml::queryElement<std::string>(prob,"timeStep");
	if (tsPair.first) {
		const std::string & legacyTS = tsPair.second; 
		mbSetup.frontTimeStep = vcell_xml::convertChildElementWithDefault<std::string>(prob,"frontTimeStep","0");
		mbSetup.solverTimeStep = vcell_xml::convertChildElementWithDefault<std::string>(prob,"solverTimeStep","0");
		useDefaultIfZero(mbSetup.frontTimeStep,legacyTS);
		useDefaultIfZero(mbSetup.solverTimeStep,mbSetup.frontTimeStep);
		VCELL_LOG(warn,"legacy timeTime replaced by frontTimeStep and solverTimeStep");
	}
	else {
		mbSetup.frontTimeStep = vcell_xml::convertChildElement<std::string>(prob,"frontTimeStep");
		mbSetup.solverTimeStep = vcell_xml::convertChildElement<std::string>(prob,"solverTimeStep");
	}

	mbSetup.hardTime = convertTrueOrFalse(vcell_xml::convertChildElementWithDefault<const char *>(prob,"hardTime","false"));

	using vcell_xml::convertChildElementWithDefault;
	mbSetup.frontVelocityFunctionStrX = convertChildElement<std::string>(prob,"frontVelocityFunctionX");
	mbSetup.frontVelocityFunctionStrY = convertChildElement<std::string>(prob,"frontVelocityFunctionY");

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

	mbSetup.physiology = new Physiology();
	const XMLElement* subdomainElement = physiologyElement.FirstChildElement("subdomain");
	while (subdomainElement != nullptr)
	{
		Subdomain* subdomain = nullptr;
		string domainType = subdomainElement->Attribute("type");
		string domainName = subdomainElement->Attribute("name");
		if (domainType == "volume")
		{
			subdomain = new VolumeSubdomain(domainName, mbSetup.physiology);
		}
		else if (domainType == "point")
		{
			string posX = vcell_xml::convertChildElement<std::string>(*subdomainElement,"positionX");
			string posY = vcell_xml::convertChildElement<std::string>(*subdomainElement,"positionY");
			subdomain = new PointSubdomain(domainName, mbSetup.physiology, posX, posY);
		}
		mbSetup.physiology->addSubdomain(subdomain);

		const XMLElement *spE = subdomainElement->FirstChildElement("species");
		std::ostringstream defaultNameS;
		while (spE != nullptr) {
			//get name, or design default
			string name  = spE->Attribute("name");
			string type  = spE->Attribute("type");
			Variable* s = nullptr;

			if (type == "volume")
			{
				s = new VolumeVariable(name);
			}
			else if (type == "point")
			{
				s = new PointVariable(name);
			}

			std::string init = vcell_xml::convertChildElement<std::string>(*spE,"initial");
			std::string source = vcell_xml::convertChildElement<std::string>(*spE,"source");
			s->setExpression(expr_initial, init);
			s->setExpression(expr_source, source);

			// PDE
			std::pair<bool,std::string> dq = vcell_xml::queryElement<std::string>(*spE,"diffusion");
			if (dq.first) {
				const std::string & diffusion = dq.second;
				s->setExpression(expr_diffusion, diffusion);
			}
			std::pair<bool,std::string> axq = vcell_xml::queryElement<std::string>(*spE,"advectVelocityFunctionX");
			if (axq.first)
			{
				s->setExpression(expr_advection_x, axq.second);
			}
			std::pair<bool,std::string> ayq = vcell_xml::queryElement<std::string>(*spE,"advectVelocityFunctionY");
			if (ayq.first)
			{
				s->setExpression(expr_advection_y, ayq.second);
			}
			subdomain->addVariable(s);
			mbSetup.physiology->addVariable(s);

			spE = spE->NextSiblingElement("species");
		}
		subdomainElement = subdomainElement->NextSiblingElement("subdomain");
	}

	return mbSetup; 
}

