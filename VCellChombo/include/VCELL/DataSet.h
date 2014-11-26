/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef DATASET_H
#define DATASET_H

#include <stdio.h>

typedef unsigned int uint32;

typedef enum {
	endian_not_set = -1,
	little_endian,
	big_endian
} Endian;

class SimulationExpression;

class DataSet
{
public:
	static void readDoubles(FILE *fp, double *data, int length);

	static bool isBigEndian();

#ifndef CH_MPI
	static void write(SimulationExpression *sim, char* filename);
#endif
	
private:
	static Endian endian;
};

#endif
