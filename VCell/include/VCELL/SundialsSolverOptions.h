/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef SUNDIALSSOLVEROPTIONS_H
#define SUNDIALSSOLVEROPTIONS_H

struct SundialsSolverOptions {
	double relTol, absTol;
	double maxStep;
	int maxOrder;

	SundialsSolverOptions() {
		relTol = 1e-7;
		absTol = 1e-9;
		maxStep = 0.1;
		maxOrder = 5;
	}
};
#endif
