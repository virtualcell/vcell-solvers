#include <Timer.h>
#include <iomanip>
#include <ctime>
#include <Logger.h>

using vcell_util::Timer;
Timer::Timer( ) {}
Timer::~Timer( )  {
}

void Timer::start( ) {
	startTime = system_clock::now();
}

void Timer::stop( ) {
	stopTime = system_clock::now();
}

void Timer::show( ) const {
	std::time_t t0 = system_clock::to_time_t(startTime);
	std::time_t t1 = system_clock::to_time_t(stopTime);
	VCELL_LOG_ALWAYS("\tStarted at\t" << std::put_time(std::localtime(&t0), "%F %T"));
	VCELL_LOG_ALWAYS("\tStopped at\t" << std::put_time(std::localtime(&t1), "%F %T"));
	VCELL_LOG_ALWAYS("\tElapsed(s)\t" << elapsedInS());
	VCELL_LOG_ALWAYS("\tElapsed(ms)\t" << elapsedInMs());
}

double Timer::elapsedInMs( ) const {
	 return duration_cast<milliseconds>(stopTime - startTime).count();
}

double Timer::elapsedInS( ) const {
	 return duration_cast<seconds>(stopTime - startTime).count();
}

double Timer::elapsed( ) const {
	 return elapsedInS();
}

void Timer::showNow( ) {
	stop();
	show();
}

