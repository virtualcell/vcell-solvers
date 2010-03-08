/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef SCHEDULER_H
#define SCHEDULER_H

class Simulation;

class Scheduler
{
public:
	Scheduler(Simulation *Asim);
	virtual void iterate()=0;
	virtual void initValues();
	void solveFastSystem(int startVolIndex, int VolSize, int startMemIndex, int MemSize);
	bool hasFastSystem() { return bHasFastSystem; }
	//void resetFirstTime() { bFirstTime = true; }
protected:
	Simulation *sim;
	bool    bFirstTime;
	bool    bHasFastSystem;
};

#endif
