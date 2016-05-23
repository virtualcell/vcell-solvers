#include <MPoint.h>
#include <iomanip>
#include <MeshElementNode.h>
#include <MovingBoundaryParabolicProblem.h>
#include <World.h>
#include <vcellxml.h>
#include <TextReportClient.h>
#include <vcellxml.h>
#pragma GCC diagnostic ignored "-Winvalid-offsetof"
using moving_boundary::World ;
using moving_boundary::CoordinateType;
using moving_boundary::Volume2DClass;
using std::endl;
namespace {
	struct TRC : public moving_boundary::TextReportClient {
		typedef World<moving_boundary::CoordinateType,2> WorldType;
		TRC(const std::string & filename , moving_boundary::MovingBoundaryParabolicProblem &mbpp, int precision, int width)
			:output(filename),
			problem(mbpp),
			pointConverter(getPointConverter( )),
			outputPrecision(precision),
			outputWidth(width),
			count(-1)
		{
				output.precision(outputPrecision);
				output.setf(std::ios_base::fixed, std::ios_base::floatfield); //fixed
			}

		virtual void time (double t, unsigned int generationCount, bool last, const moving_boundary::GeometryInfo< moving_boundary::CoordinateType > &geometryInfo) {
			if (t == 0) {
				output << "MovingBoundarySolver output" << endl << "precision:  " << outputPrecision << endl
						<< "width:  " << outputWidth << endl;
			}
			count = generationCount;
			output << "gen: " << generationCount << endl
				 << "time: " << t << endl;
			if (last) {
				output << "last iteration" << endl;
			}
			output << "nodes adjusted: " << geometryInfo.nodesAdjusted << endl
					<< "front: " << endl;
			auto front = geometryInfo.boundary;
			for (auto iter = front.begin( ); iter != front.end( ); ++ iter) {
				auto simPoint = *iter;
				auto pdPoint = pointConverter(simPoint);
				output << simPoint << "; (" << pdPoint << ')' << endl;
			}
			output << endl << endl;
		}

		virtual void element (const moving_boundary::MeshElementNode &e) {
			auto pdPoint = pointConverter(e);
			output << e.indexInfo() << " (" << pdPoint << ") u: " << std::setw(outputWidth) << e.concentration(0)
					<< " v: " << std::setw(outputWidth) << e.volumePD()
					<< " m: " << std::setw(outputWidth) << e.mass(0)
					 << endl;
		}

		virtual void iterationComplete ( ) {
			output << "end of generation " << count << endl;

		}

		virtual void simulationComplete () {
			output << "end of simulation " << count << endl;

		}
		/**
		 * a very long line of code that simply calls Singleton method
		 */
		static inline moving_boundary::WorldToPDPointConverter<moving_boundary::CoordinateType,2> getPointConverter( ) {
			moving_boundary::World<moving_boundary::CoordinateType,2> &world = moving_boundary::World<moving_boundary::CoordinateType,2>::get( );
			return world.pointConverter();
		}

		std::ofstream output;
		const moving_boundary::MovingBoundaryParabolicProblem &problem;
		moving_boundary::WorldToPDPointConverter<moving_boundary::CoordinateType,2> pointConverter;
		const int outputPrecision;
		const int outputWidth;
		int count;
	};
	
}

using moving_boundary::TextReportClient;
TextReportClient * TextReportClient::setup(const tinyxml2::XMLElement &root,moving_boundary::MovingBoundaryParabolicProblem &mbpp) {
	const tinyxml2::XMLElement *tr = vcell_xml::query(root, "textReport");
	if (tr != nullptr) {
		std::string filename = vcell_xml::convertChildElement<std::string>(*tr,"outputFilename");
		const int p = vcell_xml::convertChildElementWithDefault<unsigned int>(*tr, "precision", 6);
		const int w = vcell_xml::convertChildElementWithDefault<unsigned int>(*tr, "width", 6);
		return new TRC(filename, mbpp, p, w);
	}
	return nullptr;
}
