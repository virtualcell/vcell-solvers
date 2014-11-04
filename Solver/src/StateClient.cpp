#include <StateClient.h>
#include <limits>
#include <fstream>
#include <iomanip>
#include <NumericConvert.h>
using namespace moving_boundary;
namespace {
	const double NEVER_SAVED = std::numeric_limits<double>::min( );

}

/**
* create, add to prb
* @param prb problem to solve; this automatically added to it
* @param name base name of output files 
* @param startTime_ when to start recording state files
* @param increment how often after start to record 
*/
StateClient::StateClient(MovingBoundaryParabolicProblem & prb, const std::string & name, double startTime_, double increment)
	:problem(prb),
	baseName(name),
	startTime(startTime_),
	timeIncrement(increment),
	lastTimeSaved(NEVER_SAVED),
	requiredDigits( ){
		registerTypes( );
		problem.registerInstanceType( );
		problem.add(*this);
		auto endInt = static_cast<unsigned long>(problem.endTime( ));
		requiredDigits = vcell_util::numberDigits(endInt) + afterDecimal + 1;
}

/**
* use same types on save and restore
*/
void StateClient::registerTypes( ) {
		MovingBoundarySetup::registerType( );
		MovingBoundaryParabolicProblem::registerType( );
		Universe<2>::registerType( );
		World<CoordinateType,2>::registerType( );
}

void StateClient::time(double t, unsigned int generationCount, bool last, const GeometryInfo<moving_boundary::CoordinateType> & geometryInfo){
	if (t > startTime) {
		if (lastTimeSaved == NEVER_SAVED || t - lastTimeSaved >= timeIncrement) { 
			std::ostringstream oss;
			oss << baseName << std::setfill('0') << std::setw(requiredDigits) << std::fixed << std::setprecision(afterDecimal) << t << std::ends; 
			std::string filename(oss.str());
			std::replace(filename.begin( ), filename.end( ),'.','-');
			filename += ".dat";
			std::ofstream out(filename, std::ios::binary|std::ios::trunc);
			vcell_persist::WriteFormatter wf(out, MB_VERSION, true);
			problem.setup( ).persist(out);
			Universe<2>::get( ).persist(out);
			World<CoordinateType,2>::get( ).persist(out);
			problem.persist(out);
			lastTimeSaved = t;
		}
	}
}

void StateClient::simulationComplete( ) {}

MovingBoundaryParabolicProblem StateClient::restore(const std::string & filename) {
		registerTypes( );
		MovingBoundarySetup::registerType( );
		MovingBoundaryParabolicProblem::registerType( );
		std::ifstream in(filename,std::ios::binary);
		vcell_persist::ReadFormatter rf(in, MB_VERSION);
		MovingBoundarySetup mbs(in);
		Universe<2>::get( ).restore(in);
		World<CoordinateType,2>::get( ).restore(in);
		MovingBoundaryParabolicProblem mbpp(mbs,in);
		return mbpp;
}
