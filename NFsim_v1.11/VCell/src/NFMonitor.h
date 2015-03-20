#ifndef NFMonitor_h
#define NFMonitor_h
#include <array>
#include <ctime>
namespace vcell_nfsim {
	struct NFMonitor {
		typedef unsigned int timeUnit;
		/**
		 * don't send progress more than this time
		 */
		static const timeUnit UPDATE_SECONDS = 2;

		NFMonitor();
		/**
		 * begin simulation
		 */
		void reportStart( );
		void reportComplete( );
		/**
		 * receives each char written to stream (cout)
		 */
		void intercept(char);
	private:
		void endOfLine();
		void parseStartToken();
		void parseTimeToken();
		void parseFailToken();
		void process(char);
		timeUnit timeDiff(clock_t end, clock_t start);

		static const size_t bufferSize = 1024;
		char buffer[bufferSize];
		size_t cursor;
		bool active;
		const char *startOfTimeData;
		/**
		 * how long is sim going to run?
		 */
		double totalSimTime;
		/**
		 * last message time
		 */
		std::clock_t lastUpdate;
		/**
		 * last percent sent
		 */
		double lastFraction;
		/**
		 * flag to only check sim time set once
		 */
		bool checkSimTime;
		bool endMessageReceived;
		/**
		 * last fail message parsed
		 */
		std::string failMessage;
	};

}
#endif
