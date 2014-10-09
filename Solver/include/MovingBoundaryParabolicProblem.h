#ifndef MovingBoundaryParabolicProblem_h
#define MovingBoundaryParabolicProblem_h
#include <stdexcept>
#include <iostream>
#include <MovingBoundaryTypes.h>
#include <VCellFront.h> 

namespace spatial {
	template<class REAL, int N>
	struct MeshDef;
}
namespace moving_boundary {
	struct MeshElementSpecies; 

	typedef double (*ConcentrationFunction)(double x, double y);

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
			levelFunction( ),
			velocityFunction( ),
			concentrationFunction( ),
			//diffusionCoefficient( ),
			levelFunctionStr( ),
			advectVelocityFunctionStrX( ),
			advectVelocityFunctionStrY( ),
			concentrationFunctionStr( ),
			diffusionConstant(0),
			alternateFrontProvider(nullptr)
		{}
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

	struct MovingBoundaryClient {

		virtual ~MovingBoundaryClient( ) {}
		/**
		* time of simulation
		* @param t current time
		* @param generationCount count of generations
		* @param last is this last time increment of sim?
		* @param geometryInfo current geometry information. Reference is not valid after return of function call.
		*/
		virtual void time(double t, unsigned int generationCoount, bool last, const GeometryInfo<moving_boundary::CoordinateType> & geometryInfo) = 0; 
		/**
		* state of inside / boundary nodes
		*/
		virtual void element(const MeshElementSpecies &e) = 0;
		/**
		* notify client they've received all elements
		*/
		virtual void iterationComplete( ) = 0;
		/**
		* simulation has finished executing
		*/
		virtual void simulationComplete( ) = 0;
	};

	struct MovingBoundaryParabolicProblemImpl;

	class MovingBoundaryParabolicProblem { 
	public:
		/**
		* initialiation constructor
		*/
		MovingBoundaryParabolicProblem(const MovingBoundarySetup &mbs);
		/**
		* default constructor to allow placeholder variables
		*/
		MovingBoundaryParabolicProblem( )
			:impl(nullptr) {}
		/**
		* copy constructor -- transfers ownership impl from rhs to this
		*/
		MovingBoundaryParabolicProblem(MovingBoundaryParabolicProblem & rhs) 
			:impl(rhs.impl) {
				rhs.impl = nullptr;
		}
		~MovingBoundaryParabolicProblem( );

		/**
		* assignment operator -- transfers ownership impl from rhs to this
		*/
		MovingBoundaryParabolicProblem & operator=(const MovingBoundaryParabolicProblem & rhs) 
		{ 
			impl = rhs.impl;
			const_cast<MovingBoundaryParabolicProblem &>(rhs).impl = nullptr;
			return *this;
		}

		/**
		* set heartbeat output to std::cout
		* @param numGen output after every numGen generations
		* @param symbol symbol to output (newline not implied)
		* default is no heartbeart
		*/
		void setHeartbeat(size_t numGen, const std::string &symbol);
		/**
		* run the simulation
		* @throws #TimeStepTooBig
		*/
		void run(MovingBoundaryClient &client);
		void plotPolygons(std::ostream &os) const ;
		void plotAreas(std::ostream &os) const;
		const spatial::MeshDef<moving_boundary::CoordinateType,2> & meshDef( ) const;
		double baseTimeStep( ) const;
		unsigned int numberTimeSteps( ) const; 
		double endTime( ) const;
		/**
		* setup used for problem
		*/
		const MovingBoundarySetup &setup( ) const;

	private:
		MovingBoundaryParabolicProblemImpl *impl;
	};

	struct TimeStepTooBig : public std::invalid_argument {
		TimeStepTooBig(const char *what)
			:std::invalid_argument(what) {}
		TimeStepTooBig(const std::string & what)
			:std::invalid_argument(what) {}
	};
}
#endif
