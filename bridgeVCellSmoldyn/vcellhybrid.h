/*
 * vcellhybrid.h
 *
 *  Created on: Oct 20, 2015
 *      Author: gweatherby
 */

#ifndef BRIDGEVCELLSMOLDYN_VCELLHYBRID_H_
#define BRIDGEVCELLSMOLDYN_VCELLHYBRID_H_
#include <cassert>
struct simstruct;
class SimTool;
typedef simstruct * simptr;

/**
* manage hybrid / standlone smolydn status
*/
class vcellhybrid {
	public:
		static simptr smoldynInit(SimTool* simTool, string& fileName);
		static void smoldynOneStep(simptr sim);
		static void smoldynEnd(simptr sim);
		static bool isHybrid( ) {
			return bHybrid;
		}
		static void setHybrid( ) {
			bHybrid = true;
		}
		static void setTaskId(int t) {
			assert(!bHybrid);
			taskID = t;
		}
		static int getTaskId( ) {
			return taskID;
		}
		static bool isMessaging( ) {
			return !bHybrid && (taskID >= 0) ;
		}
	private:
		static bool bHybrid ;
		static int taskID;
};

#endif /* BRIDGEVCELLSMOLDYN_VCELLHYBRID_H_ */
