#include <Timer.h>

#ifdef WIN32
#	define WIN32_LEAN_AND_MEAN
#	include <Windows.h>

struct vcell_util::TimerImpl {
	TimerImpl( ) 
		:startTime(),
		stopTime(),
		freq(getFreq( )) {}

	static LARGE_INTEGER getFreq( ) {
		LARGE_INTEGER f; 
		QueryPerformanceFrequency(&f);
		return f;
	}

	void start( ) {
		QueryPerformanceCounter(&startTime);
	}
	void stop( ) {
		QueryPerformanceCounter(&stopTime);
	}
	double elapsed( ) const {
		double delta = static_cast<double>(stopTime.QuadPart - startTime.QuadPart);
		return delta/freq.QuadPart;
	}
	LARGE_INTEGER startTime;
	LARGE_INTEGER stopTime; 
	const LARGE_INTEGER freq;
};
#elif defined(__MACH__)
struct vcell_util::TimerImpl {

	void start( ) {
	}
	void stop( ) {
	}
	double elapsed( ) const {
	    return 0; //placeholder
	}
};
#elif defined(__GNUG__)
#include <time.h>
struct vcell_util::TimerImpl {
	TimerImpl( ) 
		:startTime(),
		stopTime(){}

	void start( ) {
	  clock_gettime(CLOCK_REALTIME,&startTime);
	}
	void stop( ) {
	  clock_gettime(CLOCK_REALTIME,&stopTime);
	}
	double elapsed( ) const {
	  const time_t seconds = stopTime.tv_sec - startTime.tv_sec;
	  const long nano = stopTime.tv_nsec - startTime.tv_nsec;
	  const double rval = static_cast<double>(seconds + nano*1e-9);
	  return rval;
	}
	timespec startTime;
	timespec stopTime; 
};
#endif

using vcell_util::Timer;
Timer::Timer( )
	:impl(new vcell_util::TimerImpl) {}
Timer::~Timer( )  {
	delete impl;
}
void Timer::start( ) {
	impl->start( );
}
void Timer::stop( ) {
	impl->stop( );
}
double Timer::elapsed( ) const {
	return impl->elapsed( );
}
#if 0
#ifdef __MACH__
#include <mach/clock.h>
#include <mach/mach.h>
#endif



struct timespec ts;

#ifdef __MACH__ // OS X does not have clock_gettime, use clock_get_time
clock_serv_t cclock;
mach_timespec_t mts;
host_get_clock_service(mach_host_self(), CALENDAR_CLOCK, &cclock);
clock_get_time(cclock, &mts);
mach_port_deallocate(mach_task_self(), cclock);
ts.tv_sec = mts.tv_sec;
ts.tv_nsec = mts.tv_nsec;

#else
clock_gettime(CLOCK_REALTIME, &ts);
#endif

#endif
