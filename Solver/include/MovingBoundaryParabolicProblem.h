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
			:impl(nullptr) {}
		/**
		* copy constructor -- transfers ownership impl from rhs to this
		*/
		MovingBoundaryParabolicProblem(MovingBoundaryParabolicProblem & rhs) 
			:impl(rhs.impl) {
				rhs.impl = nullptr;
		}
		/**
		* stored instance constructor 
		*/
		MovingBoundaryParabolicProblem(const MovingBoundarySetup &mbs, std::istream &is);

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

		static void registerType( );
		/**
		* save this. Note #MovingBoundarySetup must be saved separately by client
		*/
		void persist(std::ostream &os) const;

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
