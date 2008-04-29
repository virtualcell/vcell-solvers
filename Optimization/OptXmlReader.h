#ifndef OPTXMLREADER_H
#define OPTXMLREADER_H

class TiXmlElement;
class OdeOptJob;
class OdeResultSet;

class OptXmlReader
{
public:
	OptXmlReader();
	OdeOptJob* parseOdeOptJob(const char* xmlText);
	OdeOptJob* readOdeOptJob(const char* xmlFile);
private:
	OdeOptJob* parseOdeOptJob(TiXmlElement* rootNode);
	OdeResultSet* parseOdeResultSet(TiXmlElement* dataNode);
};

#endif