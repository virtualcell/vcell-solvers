#ifndef TextReportClient_h
#define TextReportClient_h
#include <MovingBoundarySetup.h>
namespace tinyxml2 {
	class XMLElement;
}
namespace moving_boundary {
	class MovingBoundaryParabolicProblem;
	struct TextReportClient : public MovingBoundaryElementClient {
		 /**
		 * @param root document root containing problem section
		 * @param filename optional override of file name in XML, may be empty string
		 * @param mbpp problem to report on
		 */
		 static TextReportClient *setup(const tinyxml2::XMLElement &root, MovingBoundaryParabolicProblem &mbpp);
	};



}

#endif
