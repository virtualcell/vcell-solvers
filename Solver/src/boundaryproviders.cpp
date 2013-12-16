#include <boundaryProviders.h>
#include <vcellxml.h>
#include <fstream> //temp
#include <iomanip> //temp

namespace { 
	const double Pi = 3.14159265358979323846264338327950288419716939937;
	class FixedBoundary : public spatial::FrontProvider {
	protected:
		const double xVel;
		double time;
		std::vector<spatial::Point2D> baseFront; 
		FixedBoundary(double xvelocity) 
			:xVel(xvelocity),
			time(0) {}
	public:

		virtual bool propagateTo(double time_) {
			time = time_;
			return true;
		}
		/**
		* get current front 
		*/
		virtual std::vector<spatial::Point2D> retrieveFront( ) {
			std::vector<spatial::Point2D> rval(baseFront.size( ));
			std::transform(baseFront.begin( ),baseFront.end( ),rval.begin( ),*this);
			return rval;
		}

		/**
		* make self a transform operator
		*/
		spatial::Point2D operator( )(spatial::Point2D & in) {
			spatial::Point2D rval(in);
			rval(spatial::cX) += xVel *time;
			return rval;
		}
		std::string describe( ) const {
			std::ostringstream os;
			os << "Fixed boundary front with " << baseFront.size( ) << " points and x velocity " << xVel << std::ends;
			return os.str( ); 
		}

	protected:
		/**
		* close polygon
		*/
		void close( ) {
			baseFront.push_back(baseFront.front( ));
		}

	};

	struct Circle : public FixedBoundary {
		/**
		* @param originx
		* @param originy
		* @param radius
		* @param step (degrees to step to create circle)
		* @param velocityx 
		*/
		Circle(double originx, double originy, double radius, double step, double velocityx)
			:FixedBoundary(velocityx),
			x(originx),
			y(originy),
			r(radius),
			s(step)
		{
				for (double theta = 0; theta < 2 *Pi ; theta += step) {
					spatial::Point2D t(originx + radius * cos(theta), originy + radius * sin(theta) );
					baseFront.push_back(t);
				}
				close( );
		}

		std::string describe( ) const {
			//temporary supplemental info
			std::ofstream pointsfile("circle.txt");
			for (std::vector<spatial::Point2D>::const_iterator iter = baseFront.begin( ); iter != baseFront.end( ); ++iter) {
				pointsfile << std::setprecision(40) << iter->get(spatial::cX) << ',' << std::setprecision(40) << iter->get(spatial::cY) << std::endl;
			}

			std::ostringstream os;
			os << "Circle at " << x << ',' << y << " of radius " << r << " theta step " << s << " with " 
				 << baseFront.size( ) << " points and x velocity " << xVel << std::endl;
			return os.str( ); 
			return std::string( );
		}

		~Circle( ) {
		}
	private:
		const double x; 
		const double y; 
		const double r;
		const double s; 
	};

	spatial::FrontProvider *frontFromXMLCircle(const tinyxml2::XMLElement &node) {
		double theta = vcell_xml::convertChildElement<double>(node,"thetaIncrement");
		double vx = vcell_xml::convertChildElement<double>(node,"velocityx");

		double x = vcell_xml::convertChildElementWithDefault<double>(node,"originx",0);
		double y = vcell_xml::convertChildElementWithDefault<double>(node,"originy",0);
		double radius = vcell_xml::convertChildElementWithDefault<double>(node,"radius",1);
		return new Circle(x,y,radius,theta,vx);
	}
	
}

/**
* @param originx
* @param originy
* @param radius
* @param step (degrees to step to create circle)
* @param velocityx 
*/
spatial::FrontProvider *spatial::circleFront(double originx, double originy, double radius, double step, double velocityx) {
	return new Circle(originx,originy,radius,step,velocityx);
}
spatial::FrontProvider *spatial::frontFromXML(const tinyxml2::XMLElement &node) {
	const tinyxml2::XMLElement *c = node.FirstChildElement("circle");
	if (c != nullptr) {
		return frontFromXMLCircle(*c);
	}


	throw std::invalid_argument("unrecognized XML");
}