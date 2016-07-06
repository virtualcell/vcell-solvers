#ifndef MovingBoundarySetup_h
#define MovingBoundarySetup_h
//#include <stdexcept>
//#include <iostream>
//#include <MovingBoundaryTypes.h>
#include <VCellFront.h> 
namespace tinyxml2 {
	class XMLElement;
}

#include <CoordVect.h>
#include <IndexVect.h>

namespace moving_boundary {
	struct MeshElementNode; 

	/**
	* POD for species initialization
	*/
	struct SpeciesSpecification  : public vcell_persist::Persistent {
		std::string name;
		std::string initialConcentrationStr;
		std::string sourceExpressionStr;
		std::string diffusionExpressionStr;
		
		/**
		* default to allow storage in vector
		*/
		SpeciesSpecification( )
			:name( ),
			initialConcentrationStr( ),
			sourceExpressionStr( ),
			diffusionExpressionStr( ) {}
		/**
		* @param n name
		* @param i initial concentration 
		* @param s source (reaction) 
		* @param d diffusion  
		*/
		SpeciesSpecification(const std::string & n, const std::string & i, const std::string & s,const std::string  &d)
			:name(n),
			initialConcentrationStr(i),
			sourceExpressionStr(s),
			diffusionExpressionStr(d) {}


		explicit SpeciesSpecification(std::istream &is) ;
		void persist(std::ostream &os) const;
		static void registerType( );
	};

	struct MovingBoundarySetup {
		IndexVect Nx;
		CoordVect extentX;
		CoordVect extentY;

		unsigned int frontToNodeRatio;
		double maxTime;
		/**
		* front time step increment
		*/
		std::string frontTimeStep;
		/**
		* explicit solver time step 
		*/
		std::string solverTimeStep;

		/**
		* if true, fail simluation if time set by #numberTimeSteps or #timeStep not numerically stable. If false, time
		* step may be adjusted
		*/
		bool hardTime;

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

		std::vector<SpeciesSpecification> speciesSpecs;
		double diffusionConstant;
		/**
		* provide alternate to frontier; for testing / validation
		* Must be heap allocated; will be deleted upon simulation completion
		*/
		spatial::FrontProvider<moving_boundary::CoordinateType> * alternateFrontProvider;

		MovingBoundarySetup( ) 
			:
			frontToNodeRatio(5),
			maxTime( ),
			frontTimeStep(),
			solverTimeStep(),
			hardTime(false),
			//diffusionCoefficient( ),
			levelFunctionStr( ),
			advectVelocityFunctionStrX( ),
			advectVelocityFunctionStrY( ),
			frontVelocityFunctionStrX( ),
			frontVelocityFunctionStrY( ),
			speciesSpecs( ),
			diffusionConstant(0),
			alternateFrontProvider(nullptr)
		{}

		MovingBoundarySetup(const MovingBoundarySetup &rhs)
			:
			Nx(rhs.Nx),
			extentX(rhs.extentX),
			extentY(rhs.extentY),
			frontToNodeRatio(rhs.frontToNodeRatio),
			maxTime(rhs.maxTime),
			frontTimeStep(rhs.frontTimeStep),
			solverTimeStep(rhs.solverTimeStep),
			hardTime(rhs.hardTime),
			//diffusionCoefficient( ),
			levelFunctionStr(rhs.levelFunctionStr),
			advectVelocityFunctionStrX(rhs.advectVelocityFunctionStrX),
			advectVelocityFunctionStrY(rhs.advectVelocityFunctionStrY),
			frontVelocityFunctionStrX(rhs.frontVelocityFunctionStrX),
			frontVelocityFunctionStrY(rhs.frontVelocityFunctionStrY),
			speciesSpecs(rhs.speciesSpecs),
			diffusionConstant(rhs.diffusionConstant),
			alternateFrontProvider(rhs.alternateFrontProvider) {
		}

	MovingBoundarySetup &operator= (const MovingBoundarySetup &rhs) {
		  Nx = rhs.Nx;
		  extentX = rhs.extentX;
		  extentY = rhs.extentY;
			frontToNodeRatio = rhs.frontToNodeRatio;
			maxTime = rhs.maxTime;
			frontTimeStep = rhs.frontTimeStep;
			solverTimeStep = rhs.solverTimeStep;
			hardTime = rhs.hardTime;
			//diffusionCoefficient =  ;
			levelFunctionStr = rhs.levelFunctionStr;
			advectVelocityFunctionStrX = rhs.advectVelocityFunctionStrX;
			advectVelocityFunctionStrY = rhs.advectVelocityFunctionStrY;
			frontVelocityFunctionStrX = rhs.frontVelocityFunctionStrX;
			frontVelocityFunctionStrY = rhs.frontVelocityFunctionStrY;
			speciesSpecs = rhs.speciesSpecs,
			diffusionConstant = rhs.diffusionConstant;
			alternateFrontProvider = rhs.alternateFrontProvider;
			return *this;
		}
		explicit MovingBoundarySetup(std::istream &is) ;
		~MovingBoundarySetup( );
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
		 * return the name of the output if any.
		 */
		virtual std::string outputName( ) const = 0;
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
		virtual void element(const MeshElementNode &e) = 0;
		/**
		* notify client they've received all elements
		*/
		virtual void iterationComplete( ) = 0;
	};
}

#endif
