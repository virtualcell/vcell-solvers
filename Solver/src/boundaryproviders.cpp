#include <iomanip>
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
			xVel(xvelocity * world.theScale( ) ),
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
		moving_boundary::FrontPointType operator( )(const moving_boundary::FrontPointType & in) {
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
			os << "Circle at " << x << ',' << y << " of radius " << r << " theta step " << std::setprecision(12) << s << " with " 
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

	struct Rectangle : public FixedBoundary {
		Rectangle(double originx, double originy, double width, double height, double velocityx)
			:FixedBoundary(velocityx),
			x(originx),
			y(originy),
			w(width),
			h(height) 
		{
			spatial::Point2D cursor(originx,originy); 
			using spatial::cX;
			using spatial::cY;
			baseFront.push_back(world.toWorld(cursor) );
			cursor(cX) += w;
			baseFront.push_back(world.toWorld(cursor) );
			cursor(cX) += h;
			baseFront.push_back(world.toWorld(cursor) );
			cursor(cX) -= w;
			baseFront.push_back(world.toWorld(cursor) );
			close( );
		}

		std::string describe( ) const {
			std::ostringstream os;
			os << "Rectangle corner at " << x << ',' << y << " width " << w << " height " << h 
				<< "and x velocity " << xVel << std::endl;
			return os.str( ); 
			return std::string( );
		}

	private:
		const double x; 
		const double y; 
		const double w;
		const double h; 
	};

	/**
	* base class / factory for alternate front providers
	*/
	struct Builder {
		Builder(Builder *sub) {
			builders[bIndex++] = sub;
		}
		virtual ~Builder( ) {}
		/**
		* return provider correct XML present
		* @return nullptr if top level node doesn't match
		*/
		virtual spatial::FrontProvider<moving_boundary::CoordinateType> * build(const tinyxml2::XMLElement &node) = 0; 
		static spatial::FrontProvider<moving_boundary::CoordinateType> * construct( const tinyxml2::XMLElement &node) {
			for (size_t i = 0; i < builders.size( ) ; i++) {
				Builder * b = builders[i];
				if (b == nullptr) { //no match
					return nullptr;
				}
				spatial::FrontProvider<moving_boundary::CoordinateType> * fp = b->build(node); 
				if (fp != nullptr) {
					return fp;
				}
			}
			return nullptr;
		}
	protected:
		static std::array<Builder*,10> builders;
		static size_t bIndex;
	};
	std::array<Builder*,10> Builder::builders;
	size_t Builder::bIndex;


	struct CircleBuild : public Builder {
		CircleBuild( )
			:Builder(this) {}
		virtual spatial::FrontProvider<moving_boundary::CoordinateType> * build(const tinyxml2::XMLElement &root) {
			const tinyxml2::XMLElement *n = root.FirstChildElement("circle");
			if (n != nullptr) {
				const tinyxml2::XMLElement &node = *n; 
				double theta = vcell_xml::convertChildElement<double>(node,"thetaIncrement");
				double vx = vcell_xml::convertChildElement<double>(node,"velocityx");

				double x = vcell_xml::convertChildElementWithDefault<double>(node,"originx",0);
				double y = vcell_xml::convertChildElementWithDefault<double>(node,"originy",0);
				double radius = vcell_xml::convertChildElementWithDefault<double>(node,"radius",1);
				return new Circle(x,y,radius,theta,vx);
			}
			return nullptr;
		}
	} Cb;

	struct RectangleBuild : public Builder {
		RectangleBuild( )
			:Builder(this) {}
		virtual spatial::FrontProvider<moving_boundary::CoordinateType> * build(const tinyxml2::XMLElement &root) {
			const tinyxml2::XMLElement *n = root.FirstChildElement("rectangle");
			if (n != nullptr) {
				const tinyxml2::XMLElement &node = *n; 
				double width = vcell_xml::convertChildElement<double>(node,"width");
				double height = vcell_xml::convertChildElement<double>(node,"height");
				double vx = vcell_xml::convertChildElement<double>(node,"velocityx");

				double x = vcell_xml::convertChildElementWithDefault<double>(node,"originx",0);
				double y = vcell_xml::convertChildElementWithDefault<double>(node,"originy",0);
				double radius = vcell_xml::convertChildElementWithDefault<double>(node,"radius",1);
				return new Rectangle(x,y,width,height,vx); 
			}
			return nullptr;
		}
	} Rb;

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
	spatial::FrontProvider<moving_boundary::CoordinateType> * fp = Builder::construct(node); 
	if (fp != nullptr) {
		return fp;
	}
	VCELL_EXCEPTION(invalid_argument,"Invalid XML for alternate front");
}