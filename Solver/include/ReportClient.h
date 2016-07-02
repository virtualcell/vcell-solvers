#ifndef ReportClient_h
#define ReportClient_h
#include <MovingBoundarySetup.h>
namespace tinyxml2 {
	class XMLElement;
}
namespace moving_boundary {
	class MovingBoundaryParabolicProblem;
	struct ReportClient : public MovingBoundaryElementClient {
		 virtual std::string getXML( ) const = 0;
		 virtual std::string outputName( ) const = 0;
		 /**
		 * @param root document root containing problem section
		 * @param filename optional override of file name in XML, may be empty string
		 * @param mbpp problem to report on
		 */
		 static void setup(const tinyxml2::XMLElement &root, const std::string & filename, MovingBoundaryParabolicProblem &mbpp);
	};



}

#endif
