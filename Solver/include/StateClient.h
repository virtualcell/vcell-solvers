#ifndef StateClient_h
#define StateClient_h
#include <MovingBoundaryParabolicProblem.h>
namespace moving_boundary {

	struct StateClient : public MovingBoundaryTimeClient {
		StateClient(MovingBoundaryParabolicProblem & problem, const std::string & name, double startTime, double increment);


		virtual void time(double t, unsigned int generationCount, bool last, const GeometryInfo<moving_boundary::CoordinateType> & geometryInfo); 
		virtual void simulationComplete( );
	private:
		MovingBoundaryParabolicProblem & problem;
		std::string baseName;
		double startTime;
		double lastTimeSaved;
		double timeIncrement; 
		/**
		* number digits for output filenames
		*/
		unsigned char requiredDigits;
		/**
		* how many digits after decimal
		*/
		static const unsigned char afterDecimal = 5;
	};
}
#endif
