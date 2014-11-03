#ifndef MovingBoundarySetup_h
#define MovingBoundarySetup_h
//#include <stdexcept>
//#include <iostream>
//#include <MovingBoundaryTypes.h>
#include <VCellFront.h> 
namespace tinyxml2 {
	class XMLElement;
}

namespace moving_boundary {
	struct MeshElementSpecies; 


	struct MovingBoundarySetup {
		unsigned int frontToNodeRatio;
		double maxTime;
		/**
		* set desired number of time steps
		* set this or #timeStep
		*/
		unsigned int numberTimeSteps;

		/**
		* set time step increment
		* set this or #numberTimeSteps;
		*/
		double timeStep;

		/**
		* if true, fail simluation if time set by #numberTimeSteps or #timeStep not numerically stable. If false, time
		* step may be adjusted
		*/
		bool hardTime;

#ifdef OLD_FUNCTION_POINTER_IMPLEMENTATION
		/**
		* if zero, #levelFunctionStr function is in levelFunctionStr
		*/
		spatial::FronTierLevelFunction levelFunction;
		/**
		* if zero, #levelFunctionStr function is in advectVelocityFunctionStrX, Y
		*/
		spatial::FronTierVelocityFunction velocityFunction;
		/**
		* if zero, #levelFunctionStr function is in concentrationFunctionStr
		*/
		ConcentrationFunction concentrationFunction; 
#endif
		//double diffusionCoefficient;
		std::string levelFunctionStr;
		/**
		* velocity used for advection
		*/
		std::string advectVelocityFunctionStrX;
		/**
		* velocity used for advection
		*/
		std::string advectVelocityFunctionStrY;
		/**
		* velocity used for front; optional, default is #advectVelocityFunctionStrX 
		*/
		std::string frontVelocityFunctionStrX;
		/**
		* velocity used for front; optional, default is #advectVelocityFunctionStrY
		*/
		std::string frontVelocityFunctionStrY;

		std::string concentrationFunctionStr;
		double diffusionConstant;
		/**
		* provide alternate to frontier; for testing / validation
		* Must be heap allocated; will be deleted upon simulation completion
		*/
		spatial::FrontProvider<moving_boundary::CoordinateType> *alternateFrontProvider;

		MovingBoundarySetup( ) 
			:
			frontToNodeRatio(5),
			maxTime( ),
			numberTimeSteps( ),
			timeStep(0),
			hardTime(false),
#ifdef OLD_FUNCTION_POINTER_IMPLEMENTATION
			levelFunction( ),
			velocityFunction( ),
			concentrationFunction( ),
#endif
			//diffusionCoefficient( ),
			levelFunctionStr( ),
			advectVelocityFunctionStrX( ),
			advectVelocityFunctionStrY( ),
			concentrationFunctionStr( ),
			diffusionConstant(0),
			alternateFrontProvider(nullptr)
		{}

		explicit MovingBoundarySetup(std::istream &is) ;
		void persist(std::ostream &os) const;
		static void registerType( );

		/**
		* @param XMLElement containing "problem" XML child
		* @param lockUniverse lock universe from future changes -- settable to false for unit testing
		*/
		static MovingBoundarySetup setupProblem(const tinyxml2::XMLElement &root);

	};

	/**
	* information about geometry of simulation at a given time
	*/
	template <class T>
	struct GeometryInfo {
		GeometryInfo(const std::vector<spatial::TPoint<T,2> > & boundary, bool adjusted_)
			:boundary(boundary),
			nodesAdjusted(adjusted_) {};

		/**
		* current control front
		*/
		const std::vector<spatial::TPoint<T,2> > & boundary;
		/**
		* have nodes changed since last time?
		*/
		const bool nodesAdjusted;
	};

	struct MovingBoundaryTimeClient {
		virtual ~MovingBoundaryTimeClient( ) {}
		/**
		* time of simulation
		* @param t current time
		* @param generationCount count of generations
		* @param last is this last time increment of sim?
		* @param geometryInfo current geometry information. Reference is not valid after return of function call.
		*/
		virtual void time(double t, unsigned int generationCount, bool last, const GeometryInfo<moving_boundary::CoordinateType> & geometryInfo) = 0; 
		/**
		* simulation has finished executing
		*/
		virtual void simulationComplete( ) = 0;
	};

	struct MovingBoundaryElementClient : public MovingBoundaryTimeClient {

		/**
		* state of inside / boundary nodes
		*/
		virtual void element(const MeshElementSpecies &e) = 0;
		/**
		* notify client they've received all elements
		*/
		virtual void iterationComplete( ) = 0;
	};
}

#endif
