#ifndef OPTXMLPARSER
#define OPTXMLPARSER

#include <string>
#include <vector>
using namespace std;

#include <expat.h>

struct OptParameter
{
  std::string name;
  double iniVal;
  string upperbound;
  string lowerbound;
  string scale;
};

struct OptMethodParameter
{
  std::string name;
  double value;
  std::string dataType;
};

struct OptInfo
{
	std::string mathModelSBMLFile;
	std::string experimentalDataFile;
	int numOptimizationRuns;
	int expDataLastRow;
	std::string independentVarName;
	std::vector< std::string > dependentVarNames;
	std::string methodName;
	std::vector<OptMethodParameter> methodParameters;
	std::vector<OptParameter> optParameters;
};



class OptXMLParser
{
public:
	OptXMLParser();
    static void onStartElement(void *data, const char *el, const char **attr);
    static void onEndElement(void *data, const char *el);
    static void onCharacterData(void *data, const XML_Char *s, int len);
	OptInfo& getOptInfo(){
		return optInfo;
	}
	void parse(istream &inputstream);
private:
	OptInfo optInfo;
	string currentElement;
};

#endif // optXMLParser
