#ifndef MovingBoundaryParabolicProblem_h
#define MovingBoundaryParabolicProblem_h
#include <stdexcept>
#include <iostream>
#include <MovingBoundarySetup.h>
#include <MovingBoundaryTypes.h>

namespace spatial {
	template<class REAL, int N>
	struct MeshDef;
}
namespace moving_boundary {

	struct MovingBoundaryParabolicProblemImpl;

//	typedef double (*ConcentrationFunction)(double x, double y);

	class MovingBoundaryParabolicProblem { 
	public:
		/**
		* initialiation constructor
		*/
		explicit MovingBoundaryParabolicProblem(const MovingBoundarySetup &mbs);
		/**
		* default constructor to allow placeholder variables
		*/
		MovingBoundaryParabolicProblem( )
			:sImpl( ) {}
		/**
		* copy constructor -- XXXX transfers ownership impl from rhs to this
		* copy constructor -- shares pointer 
		*/
		MovingBoundaryParabolicProblem(const MovingBoundaryParabolicProblem & rhs) 
			:sImpl(rhs.sImpl) {
		}
		/**
		* stored instance constructor 
		*/
		MovingBoundaryParabolicProblem(const MovingBoundarySetup &mbs, std::istream &is);

		~MovingBoundaryParabolicProblem( );

		/**
		* assignment operator -- XXXX transfers ownership impl from rhs to this
		* shares pointer 
		*/
		MovingBoundaryParabolicProblem & operator=(const MovingBoundaryParabolicProblem & rhs) 
		{ 
			sImpl = rhs.sImpl;
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
		* add a client. Caller responsible for ensuring reference remains valid
		*/
		void add(MovingBoundaryTimeClient &client);
		/**
		* add a client. Caller responsible for ensuring reference remains valid
		*/
		void add(MovingBoundaryElementClient &client);
		/**
		* run the simulation
		* @throws #TimeStepTooBig
		*/
		void run( );
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

		/**
		* register any polymorphic types present
		*/
		void registerInstanceType( ) const;

		static void registerType( );
		/**
		* save this. Note #MovingBoundarySetup must be saved separately by client
		*/
		void persist(std::ostream &os) const;

		std::string frontDescription( ) const;

	private:
		//MovingBoundaryParabolicProblemImpl *impl;
		std::shared_ptr<MovingBoundaryParabolicProblemImpl> sImpl;
	};

	struct TimeStepTooBig : public std::invalid_argument {
		TimeStepTooBig(const char *what)
			:std::invalid_argument(what) {}
		TimeStepTooBig(const std::string & what)
			:std::invalid_argument(what) {}
	};

}
#endif
