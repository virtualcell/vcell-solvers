/*
 * vcellhybrid.h
 *
 *  Created on: Oct 20, 2015
 *      Author: gweatherby
 */

#ifndef BRIDGEVCELLSMOLDYN_VCELLHYBRID_H_
#define BRIDGEVCELLSMOLDYN_VCELLHYBRID_H_
struct simstruct;
class SimTool;
typedef simstruct * simptr;

struct vcellhybrid {
		static simptr smoldynInit(SimTool* simTool, string& fileName);
		static void smoldynOneStep(simptr sim);
		static void smoldynEnd(simptr sim);
		static bool isHybrid( ) {
			return bHybrid;
		}
		static void setHybrid( ) {
			bHybrid = true;
		}
	private:
		static bool bHybrid ;
};





#endif /* BRIDGEVCELLSMOLDYN_VCELLHYBRID_H_ */
