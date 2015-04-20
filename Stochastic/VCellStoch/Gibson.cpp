//#define DEBUG //enable it when debug is needed.
#include "Gibson.h"

#ifdef DEBUG
#include <Windows.h>
#endif
#include <iostream>
#include <sstream>
#include <string>
#include <iomanip>
#include <vector>
#include <stdint.h>
#include <math.h>
#include <stdlib.h>
using namespace std;

#include <assert.h>
#include <sys/types.h>
#include <sys/timeb.h>
#include <time.h>
#include "Jump.h"
#include "StochVar.h"
#include "IndexedTree.h"

#ifdef USE_MESSAGING
#include <VCELL/SimulationMessaging.h>
#endif
#include <limits>
const double double_infinity = numeric_limits<double>::infinity();
const double EPSILON = 1E-12;
/*
 *Empty constructor of Gibson. It will use the defalt settings in StochModel.
 */
Gibson::Gibson():StochModel()
{
	Tree=NULL;
	currvals=NULL;
}//end of constructor Gibson()

/*
 *This constructor of Gibson reads control information, variables and processes
 *from the input file and updates the attributes' values defined in StochModel().
 *Input para: srting, the input file(name), where the model info. is read.
 *            string, the output file(name), where the results are saved.
 */
Gibson::Gibson(char* arg_infilename, char* arg_outfilename):StochModel()
{
	Tree=NULL;
	currvals=NULL;

	outfilename=arg_outfilename;

	ifstream infile;
	string instring;
	infile.open(arg_infilename);
	if (!infile) {
		stringstream ss;
		ss << "Unable to open file " << arg_infilename;
		string errStr = ss.str();   // call system to stop
		throw errStr;
	}
	flag_savePeriod=false;
	while(infile >> instring)
	{
		//load control info.
		if (instring == "STARTING_TIME")
		{
			infile >> STARTING_TIME;
		}
		else if (instring == "ENDING_TIME")
		{
			infile >> ENDING_TIME;
		}
		else if (instring == "SAVE_PERIOD")
		{
			infile >> SAVE_PERIOD;
			flag_savePeriod=true;
		}
		else if (instring == "MAX_ITERATION")
		{
			infile >> MAX_ITERATION;
		}
		else if (instring == "TOLERANCE")
		{
			infile >> TOLERANCE;
		}
		else if (instring == "SAMPLE_INTERVAL")
		{
			infile >> SAMPLE_INTERVAL;
		}
		else if (instring == "MAX_SAVE_POINTS")
		{
			infile >> MAX_SAVE_POINTS;
		}
		else if (instring == "NUM_TRIAL")
		{
			infile >> NUM_TRIAL;
		}
		else if (instring == "SEED")
		{
			infile >> SEED;
		}
		//load listofvars
		else if (instring == "TotalVars")
		{
			int varCount;
			infile >> varCount;
			string name;
			double amount;
			for(int i=0;i<varCount;i++)
			{
				infile >> name;
				infile >> amount;
				listOfVarNames.push_back(name);
				listOfIniValues.push_back((uint64_t)amount);
				StochVar *var=new StochVar((uint64_t)amount);
				listOfVars.push_back(var);
			}
		}
		//load listOfProcesses
		else if (instring == "TotalProcesses")
		{
			int pCount;
			infile >> pCount;
			string name;
			for(int i=0;i<pCount;i++)
			{
				infile >> name;
				listOfProcessNames.push_back(name);
				Jump *jp=new Jump();
				listOfProcesses.push_back(jp);
			}
		}
		//load process description to set up processes
		else if (instring == "TotalDescriptions")
		{
			int dCount,idx;
			infile >> dCount;
			string name,str;
			for(int i=0;i<dCount;i++)//loop through each process description
			{
				infile >> name >> name;// "process name"
				//find the process in listOfProcesses using it's name
				idx=getProcessIndex(name);
				//set the process name index, so that we get find its name later.
				listOfProcesses[idx]->setNameIndex(idx);
				infile >> str;// "Propensity"
				//read expression from fstream
				char exp[2000];
				infile.getline(exp,2000);//get expression string
				string* names=NULL;
				//bind probExpression with symboltable including variables and "t"(put at last)
				int lenNames=listOfVarNames.size();
				names=new string[lenNames+1];
				for (int k=0;k<lenNames;k++)
					names[k]=listOfVarNames.at(k);
				names[lenNames]="t";
				listOfProcesses[idx]->setProbabilityExpression(exp,names,(lenNames+1));
				delete[] names;
				infile >> str;//"Effect"
				if(str=="Effect")
				{
					int numEffects;
					infile >> numEffects;
					string varName;
					string oper;
					double v;
					for(int j=0;j<numEffects;j++)
					{
						infile >> varName;
						infile >> oper;
						infile >> v;
						int idx2=getVarIndex(varName);
						StochVarContext *temp=new StochVarContext(listOfVars[idx2], oper, (int)v);
						listOfProcesses[idx]->addVarContext(temp);
					}
				}
				//set up dependency
				infile >> str;//"DependentProcesses"
				if(str=="DependentProcesses")
				{
					int numDependents;
					infile >> numDependents;
					string processName;
					for(int j=0;j<numDependents;j++)
					{
						infile >> processName;
						int idx3=getProcessIndex(processName);
						listOfProcesses[idx]->addDependentJump(listOfProcesses[idx3]);
					}
				}
			}//end of for loop for process description
		}
	}
    //setup IndexedTree
	Tree=new IndexedTree();
	for(int i=0;i<listOfProcesses.size();i++)
	{
		Tree->addProcess(listOfProcesses[i]);
	}
	infile.close();
	//initialization of the double array currvals
	currvals=new double[listOfIniValues.size()+1];
#ifdef DEBUG
	cout << "-------------------control information----------------"<<endl;
	cout << "starting time:"<<STARTING_TIME <<endl;
	cout << "ending time:"<< ENDING_TIME <<endl;
	cout << "save period:"<< SAVE_PERIOD <<endl;
	cout << "max iteration:"<< MAX_ITERATION <<endl;
	cout << "tolerance:"<< TOLERANCE <<endl;
	cout << "number of trial:"<< NUM_TRIAL <<endl;
	cout << "------------------model information------------------"<<endl;
	cout << "size of vars:" << listOfVars.size() << endl <<endl;
	for (int k=0;k<listOfVarNames.size();k++)
		cout << "Var No."<<k<<" name is:"<< listOfVarNames.at(k)<<endl;
	for (int k=0;k<listOfVars.size();k++)
		cout << "Var No."<<k<<" value is:"<< *listOfVars.at(k)->getCurr()<<endl;
	for (int k=0;k<listOfIniValues.size();k++)
		cout << "Var No."<<k<<" Ini value is:"<< listOfIniValues.at(k)<<endl<<endl;
	cout << "size of processes:" << listOfProcesses.size() << endl << endl;
	for (int k=0;k<listOfProcessNames.size();k++)
		cout << "Process No."<<k<<" name is:"<< listOfProcessNames.at(k)<<endl;
	int lenVars = listOfIniValues.size();
	for(int k=0;k<lenVars;k++)
	{
		currvals[k]=listOfIniValues[k];
	}
	currvals[lenVars] = STARTING_TIME; //starting point
	for (int k=0;k<listOfProcesses.size();k++)
	{
		listOfProcesses.at(k)->getProbabilityRate(currvals);
		cout << "Propensity is:" << listOfProcesses.at(k)->getOldProbabilityRate()<<endl;
	}
	for (int k=0;k<listOfProcesses.size();k++)
		cout << "Process No." <<k<<" varContext size is:" << listOfProcesses.at(k)->getNumVars()<<endl;
	for (int k=0;k<listOfProcesses.size();k++)
		cout << "Process No." <<k<<" dependentProcess size is:" << listOfProcesses.at(k)->getNumDependentJumps()<<endl;
#endif
}// end of constructor Gibson(infilename,outfilename)

//Destructor
Gibson::~Gibson()
{
	//delete indexedTree
	delete Tree;
	//delete variables
	for(int i=0;i<listOfVars.size();i++)
		delete listOfVars[i];
	listOfVars.clear();
	//delete proccesses
	for(int i=0;i<listOfProcesses.size();i++)
		delete listOfProcesses[i];
	listOfProcesses.clear();
	//delete vectors
	listOfVarNames.clear();
	listOfIniValues.clear();
	listOfProcessNames.clear();
	//delete currvals
	delete[] currvals;
}//end of destructor ~Gibson()

/*
 *The method is the core function of Gibson method, it does one run for Gibson simulation.
 *The loop will end either by ending_time or max_iteration.
 *For single trial, the values of variables against time will be output at each save_period.
 *For multiple trial, the results at the ending_time after each trial willl be stored in a the file.
 */
int Gibson::core()
{
	double outputTimer = STARTING_TIME;//time counter used for save output by save_period
	double simtime = STARTING_TIME;//time calculated for next reaction
	double* lastStepVals = new double[listOfIniValues.size()];//to remember the last step values, used for save output by save_period
	double p, r; //temp variables used for propability and random number
	int saveIntervalCount = SAMPLE_INTERVAL; //sampling counter, for default output time spec, keep every
	int savedSampleCount = 1; //for default output time spec, keep at most, value is 1 because the first time step is written in march()
	int iterationCounter=0;//counter used for termination of the loop when max_iteration is reached
	int i; //loop variable
	double prog = 0.19; // to control the output of progress, start from >0.19....>0.99
    clock_t oldTime = clock(); // to control the output of data, output data every 2 seconds.
	int varLen = listOfIniValues.size(); //variables' length
	//reset the indexed tree
	for(i=0;i<Tree->getSize();i++)
	{
		Jump *jump = Tree->getProcess(i);
		jump->setNode(i);

		//get current values for evaluating the probability expression & also reset last setp values
		for(int k=0;k<varLen;k++)
		{
			currvals[k]=listOfIniValues[k];
			lastStepVals[k]=*listOfVars.at(k)->getCurr();
		}
		currvals[varLen] = simtime;
		p = jump->getProbabilityRate(currvals);
		//amended Oct 11th, 2007. Stop the simulation and send error message back if
		//anyone of the propensity functions is negative.
		if(p < 0){
			stringstream ss;
			ss <<  "at time point " << simtime << ", propensity of jump process "<< listOfProcessNames.at(jump->getNameIndex()) <<" evaluated to a negative value (" << p << "). Simulation abort!" << endl << jump->getProbabilityRateEvaluationSummary(currvals);
			string errStr = ss.str();
			throw errStr;
		}
		//amended May 17th,2007 we can not take the first time random number to be 0.
		//Otherwise, there is a situation that no previous random number to be reused.
		do
		{
			r = getRandomUniform();
		}
		while(r <= 0);
		jump->setLogRand(-log(r));
		if(!(p>0)){
			jump->setTime(double_infinity);
		}
		else{
			jump->setTime(jump->getLogRand()/p);
		}
#ifdef DEBUG
		cout<<"Initial r & P:" << r <<"\t" <<p <<endl;
#endif
	}
	Tree->build();
	//the while loop does one trial for simulation and ends by ending_time.
	while(simtime < ENDING_TIME)
	{
#ifdef USE_MESSAGING
		if (SimulationMessaging::getInstVar()->isStopRequested()) {
			break;
		}
#endif
		//check if maximum save points is reached, then quit the simulation (if not using uniformOutputTimeSpec)
		/* frm suggestion
		if(!flag_savePeriod && savedSampleCount > MAX_SAVE_POINTS)
		{
			string errStr = "Simulation exceeded maximum saved time points. Patial results may be available. \nYou can increase the save interval (\"keep every\") or increase the maximum number of saved time points (\"keep at most\").";
			throw errStr;
		}
		*/
		//save last step variables' values
		for(i = 0;i<varLen;i++){
			lastStepVals[i]=*listOfVars.at(i)->getCurr();
		}
	    //get next reaction with shortest absolute time
		Jump* event = Tree->getProcess(0);
		//update time
		simtime = event->getTime();
		if(simtime > ENDING_TIME)
		{
			//simulation time exceed ending time. Before we quit the simulation
			//output data between the last step simulation time and ending time if we have SAVE_PERIOD on.
			if((NUM_TRIAL ==1) && (flag_savePeriod))
			{	//use EPSILON here to make sure that a double 0.99999999999995(usually happen in C) is not regarded as a number that smaller than 1. It should be 1.
				while((outputTimer+SAVE_PERIOD+EPSILON) < ENDING_TIME)
				{
					outfile << outputTimer+SAVE_PERIOD << "\t";
					for(i=0;i<varLen;i++){
						outfile<< lastStepVals[i] << "\t";
					}
					outfile << endl;
					outputTimer = outputTimer + SAVE_PERIOD;
				}
			}
			break;
		}
		//increase output save points by 1, if not using uniformOutputTimeSpec
		if(!flag_savePeriod){
			savedSampleCount ++;
		}
		//update affected variables
		int numVars = event->getNumVars();
		for(i = 0;i<numVars;i++){
			event->getVar(i)->updateCurr();
		}
		//get current values for evaluate the probability expression
		for(int k=0;k<varLen;k++)
		{
			currvals[k]=*listOfVars.at(k)->getCurr();
		}
		currvals[varLen] = simtime;
		//update the jump that occured
		double r = getRandomUniform();
		p = event->getProbabilityRate(currvals);
		//amended Oct 11th, 2007. Stop the simulation and send error message back if
		//anyone of the propensity functions is negative.
		if(p < 0){
			stringstream ss;
			ss << "at time point " << simtime << ", propensity of jump process "<< listOfProcessNames.at(event->getNameIndex()) <<" evaluated to a negative value (" << p << "). Simulation abort!" << endl << event->getProbabilityRateEvaluationSummary(currvals);
			string errStr = ss.str();
			throw errStr;
		}
		//amended May 17th. The previous sentence will cause the time of a process stuck in double_infinity when r<=0
		if(r>0)
		{
			event->setLogRand(-log(r));
			if(p>0)
				Tree->updateTree(event, (event->getLogRand())/p+simtime);
			else
				Tree->updateTree(event, double_infinity);
		}
		else
		{
			Tree->updateTree(event, double_infinity);
		}
		//update dependent jumps
		int numDependentJumps = event->getNumDependentJumps();
		for(i=0;i<numDependentJumps;i++)
		{
			Jump *dJump = event->getDependent(i);
			double p_old = dJump->getOldProbabilityRate();
			double p_new = dJump->getProbabilityRate(currvals);
			//amended Oct 11th, 2007. Stop the simulation and send error message back if
			//anyone of the propensity functions is negative.
			if(p_new < 0){
				stringstream ss;
				ss << "at time point " << simtime << ", propensity of jump process "<< listOfProcessNames.at(dJump->getNameIndex()) <<" evaluated to a negative value (" << p << "). Simulation abort!" << endl << dJump->getProbabilityRateEvaluationSummary(currvals);
				string errStr = ss.str();
				throw errStr;
			}
			double tau = dJump->getTime();
			//amended May 17th. to make sure that tau is a finite double
			if(tau != double_infinity && (-tau != double_infinity) && tau == tau){
				dJump->setLogRand(p_old*(tau-simtime));
			}
			if(!((p_new)>0)){
				Tree->updateTree(dJump, double_infinity);
			}
			else{
				Tree->updateTree(dJump, (dJump->getLogRand())/p_new+simtime);
			}
		}
#ifdef DEBUG
		int treeLen = Tree->getSize();
		bool infAll=true;
		for(i=0; i<treeLen; i++)
		{
			if(Tree->getProcess(i)->getTime() != double_infinity)
				infAll=false;
		}
		if(infAll)
			cout << "all times are set infinity.";
#endif
		if(NUM_TRIAL ==1)
		{
			if(saveIntervalCount == SAMPLE_INTERVAL)
			{
				//output the result to file if the num_trial is one and the outputTimer reaches the new save period
				if(flag_savePeriod)
				{
					while((outputTimer+SAVE_PERIOD) < simtime)
					{
						outfile << outputTimer+SAVE_PERIOD << "\t";
						for(i=0;i<varLen;i++){
							outfile<< lastStepVals[i] << "\t";
						}
						outfile << endl;
						outputTimer = outputTimer + SAVE_PERIOD;
					}
					if(outputTimer+SAVE_PERIOD <= simtime + EPSILON)
					{
						outfile << outputTimer+SAVE_PERIOD << "\t";
						for(i=0;i<varLen;i++){
							outfile<< *listOfVars.at(i)->getCurr() << "\t";
						outfile << endl;
						}
						outputTimer = outputTimer + SAVE_PERIOD;
					}
				}
				else //output according to simulation time
				{
					outfile << simtime << "\t";
					for(i=0;i<varLen;i++){
						outfile<< *listOfVars.at(i)->getCurr()<< "\t";
					}
					outfile << endl;
				}
				// output data message every two seconds to Java program (useful for long simulation, user can view results during simulation)
				clock_t currentTime = clock();
				double duration = (double)(currentTime - oldTime) / CLOCKS_PER_SEC;
				if (duration >= 2)
				{
					double percentile = (simtime/ENDING_TIME);
#ifdef USE_MESSAGING
					SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_PROGRESS, percentile, simtime));
					SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_DATA, percentile, simtime));
#else
					printf("[[[progress:%lg%%]]]", percentile * 100);
					printf("[[[data:%lg]]]", simtime);
					fflush(stdout);
#endif
					oldTime = currentTime;
				}
			}
			if(saveIntervalCount == 1)
				saveIntervalCount = SAMPLE_INTERVAL;
			else
				saveIntervalCount--;
			// output simulaiton progress message
//              if((simtime/ENDING_TIME) > prog)
//				{
//					printf("[[[progress:%lg%%]]]", (simtime/ENDING_TIME) * 100.0);
//					fflush(stdout);
//					prog = prog + 0.2;
//				}
		}//if(NUM_TRIAL ==1)
	}//end of while loop
	//output the variable's vals at the ending time point.
	if((simtime > ENDING_TIME) && (NUM_TRIAL == 1) && (flag_savePeriod))
	{
		outfile << ENDING_TIME ;
		for(i=0;i<listOfVars.size();i++)
		{
			outfile<< "\t" << *listOfVars.at(i)->getCurr();
		}
		outfile << endl;
	}
	//to save the (last-step)values of varaibles after one run of simulation
	if(NUM_TRIAL >1)
	{
		for(i=0;i<listOfVars.size();i++)
		{
			outfile<< "\t" << *listOfVars.at(i)->getCurr();
		}
		outfile << endl;
	}
    //return parameter 0:ends by ending_time  1:ends by max_iteration
	if(iterationCounter< MAX_ITERATION)
	{
		return 0;
	}
	else return 1;
}//end of method core()

/*
 *This method is the control of trials, which will be called for Gibson simulation.
 */
void Gibson::march()
{
#ifdef DEBUG
	// Count performance time in milliseconds for the simulation
	int ntime=0;
	LARGE_INTEGER ntime1,ntime2;
	LARGE_INTEGER freq;
	QueryPerformanceFrequency(&freq);
	QueryPerformanceCounter(&ntime1);
#endif

	//prepare for writing the results to output file
	outfile.open (outfilename);//"c:/gibson_deploy/gibson_deploy/output/gibson_singleTrial.txt"
	outfile << setprecision(10); // set precision to output file
	if(NUM_TRIAL==1)
	{
		srand(SEED);
		//output file header
		outfile << "t:";
		for(int i=0;i<listOfVarNames.size();i++){
			outfile<< listOfVarNames.at(i) << ":";
		}
		outfile <<endl;
		//output initial condition at STARTING_TIME
		outfile << STARTING_TIME << "\t";
		for(int i=0;i<listOfIniValues.size();i++){
			outfile<< listOfIniValues.at(i) << "\t";
		}
		outfile << endl;
		//run the simulation
		core();

	}
	else if (NUM_TRIAL > 1)
	{
		clock_t oldTime = clock(); // use to calculate time, send progress every two seconds
		//output file header
		outfile << "TrialNo:";
		for(int i=0;i<listOfVarNames.size();i++){
			outfile<< listOfVarNames.at(i) << ":";
		}
		outfile <<endl;
		for (long j=SEED;j<NUM_TRIAL+SEED;j++)
		{
#ifdef USE_MESSAGING
		if (SimulationMessaging::getInstVar()->isStopRequested()) {
			break;
		}
#endif

#ifdef DEBUG
			cout << "Trial No. " << j <<endl;
#endif
			srand(j);
			//output trial number.  PS:results after each trial are printed in core() function.
			outfile << j-SEED+1;
			core();
			//reset to initial values before next simulation
			for(int i=0;i<listOfIniValues.size();i++){
				listOfVars[i]->setCurr(listOfIniValues.at(i));
			}
			for(int i=0;i<listOfProcesses.size();i++){
				Tree->setProcess(i,listOfProcesses.at(i));
			}

			// output progress message every two seconds to Java program
			clock_t currentTime = clock();
			double duration = (double)(currentTime - oldTime) / CLOCKS_PER_SEC;
			if (duration >= 2)
			{
				double percentile =  ((j-SEED)*1.0/NUM_TRIAL);
#ifdef USE_MESSAGING
				SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_PROGRESS, percentile, j - SEED));
#else
				printf("[[[progress:%lg%%]]]", percentile * 100);
				//printf("[[[data:%lg]]]", (j-SEED));
				fflush(stdout);
#endif
				oldTime = currentTime;
			}
		}
	}
	else
	{
		string errStr = "Number of trial smaller than 1!";
		throw errStr;
	}
#ifdef DEBUG
	//Count performance time for single trial
	QueryPerformanceCounter(&ntime2);
	//get ntime in millisecs
	ntime = (ntime2.QuadPart-ntime1.QuadPart)/(freq.QuadPart/1000);
	cout << endl << "Total time used(ms): " << ntime;
#endif
	outfile.close();

#ifdef USE_MESSAGING
	if (!SimulationMessaging::getInstVar()->isStopRequested()) {
		SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_COMPLETED, 1, ENDING_TIME));
	}
#endif
}//end of method march()

/*
 * This method generates a random number from uniform distribution.
 * Output: double, the random number generated.
 */
double Gibson::getRandomUniform()
{
#ifdef WIN32
   const long NUM_RAND = RAND_MAX+1;
   const double RAND_TOTAL_MAX = NUM_RAND*NUM_RAND - 1.0;
   long r = (NUM_RAND)*rand()+rand();
   return r / RAND_TOTAL_MAX;
#else
	return ((double)rand()) / RAND_MAX;
#endif
}//end of method getRandomUniform()
