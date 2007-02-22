#ifndef GIBSON_H
#define GIBSON_H

#include <fstream>
#include "IndexedTree.h"
#include "StochModel.h"

/* This class defines Gibson method which is also called Next Reaction Method.
 * The Gibson method uses only a single random number per simulation event and 
 * takes time proportional to the logarithm of the number of reactions. The
 * better performance is also acheived by utilizing a dependency graph and an
 * indexed priority queue. The latter is implemented as an indexed tree in our
 * package. 
 * The class takes model information from input file and outputs the result as
 * plots(single trial) or histograms(multiple trials).
 * Reference method descriptions in Gibson.cpp.
 *
 * @Author: Tracy LI, Boris Slepchenko
 * @version: 1.0 Beta
 * @CCAM,UCHC. June 2,2006
 */
class Gibson : public StochModel{
public:
	Gibson();
	Gibson(char*, char*);
	~Gibson();
	int core();
	void march();
	double getRandomUniform();
private:
	IndexedTree *Tree; //the data structure(binary tree) to store all the processes and make each parent smaller than it's children.
	long** histogram; //the variable used to store the percentage of the ocurrance of a variable at certain value.
	double* currvals;//array of variable values to be used by expression parser. variables are stored in vector listOfVars.
	ofstream outfile; //the output file stream where the results are saved.
	char* outfilename;//the output file name.
	bool flag_savePeriod;//the flag for using save period.
} ;

#endif