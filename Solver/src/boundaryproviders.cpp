#include <boundaryProviders.h>
#include <MovingBoundaryCollections.h>
#include <vcellxml.h>
#include <World.h>

namespace { 
	const double Pi = 3.14159265358979323846264338327950288419716939937;
	class FixedBoundary : public spatial::FrontProvider<moving_boundary::CoordinateType> {
		typedef spatial::TPoint<moving_boundary::CoordinateType,2> FixedBoundaryPoint;
		typedef moving_boundary::World<moving_boundary::CoordinateType,2> WorldType; 
	protected:
		const WorldType & world;
		const double xVel;
		double time;
		moving_boundary::FrontType baseFront;
		FixedBoundary(double xvelocity) 
			:world(WorldType::get( )),
			xVel(xvelocity),
			time(0) { }


	public:

		virtual bool propagateTo(double time_) {
			time = time_;
			return true;
		}
		/**
		* get current front 
		*/
		virtual std::vector<FixedBoundaryPoint> retrieveFront( ) {
			std::vector<FixedBoundaryPoint> rval(baseFront.size( ));
			std::transform(baseFront.begin( ),baseFront.end( ),rval.begin( ),*this);
			return rval;
		}

		/**
		* make self a transform operator
		*/
		moving_boundary::FrontPointType operator( )(moving_boundary::FrontPointType & in) {
			moving_boundary::FrontPointType rval(in);
			rval(spatial::cX) += static_cast<moving_boundary::CoordinateType>(xVel *time + 0.5);
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
				moving_boundary::FrontPointType fP = world.toWorld(t);
				baseFront.push_back(fP);
			}
			close( );
		}

		std::string describe( ) const {
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

	spatial::FrontProvider<moving_boundary::CoordinateType> *frontFromXMLCircle(const tinyxml2::XMLElement &node) {
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
spatial::FrontProvider<moving_boundary::CoordinateType> * moving_boundary::circleFront(double originx, double originy, double radius, double step, double velocityx) {
	return new Circle(originx,originy,radius,step,velocityx);
}
spatial::FrontProvider<moving_boundary::CoordinateType> * moving_boundary::frontFromXML(const tinyxml2::XMLElement &node) {
	const tinyxml2::XMLElement *c = node.FirstChildElement("circle");
	if (c != nullptr) {
		return frontFromXMLCircle(*c);
	}
	throw std::invalid_argument("unrecognized XML");
}