#include <iostream>
#include <string>
#include <fstream>
#include "Gibson.h"
using namespace std;

/* This file is the entrance of the Virtual Cell stochastic simulation package.
 * It parses the commandline arguments to load different simulators. Four parameters
 * are required for the command. The Usage is: 
 * VCellStoch gibson[gillespie] input_filename output_filename. 
 *
 * @Author: Tracy LI
 * @version:1.0 Beta
 * @CCAM,UCHC. May 26,2006
 */
int main(int argc, char *argv[])
{
	if(argc == 4)
	{
		string s2(argv[1]);
				
		if (s2.compare("gibson")==0)
		{
			Gibson *gb=new Gibson(argv[2],argv[3]); // e.g 
//			Gibson *gb = new Gibson("c:/sim.txt","c:/sim_out.txt");
	   		gb->march();
			delete gb;
			return 0;
		}
		else if (s2.compare("gillespie")==0)
		{
			cout << "Gillespie method is under development.";
			return 0;
		}
	}
	cerr << "Wrong Command! Usage: VCellStoch gibson[gillespie] input_filename output_filename";
	return -1;
}//end of main()
