//#define DEBUG //enable it when debug is needed.
#include <limits>
#include <stdexcept>
#include "Gibson.h"
#ifdef __APPLE__
	#include "/usr/local/opt/hdf5@1.8/include/hdf5.h"
#else
	#include <hdf5.h>
#endif
//#undef DEBUG

////Check stats calculation, See Gibson::calcRunningStats below
////Uncomment 'set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")' in .../vcell-solvers/Stochastic/CMakeLists.txt
//#include <random>
//std::default_random_engine generator;
//std::normal_distribution<double> distribution(100.0,1.77);//arg1=mean, arg2=stddev

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
#include <VCellException.h>
const double double_infinity = numeric_limits<double>::infinity();
const double EPSILON = 1E-12;
const string Gibson::MY_T_STR = "t";
/*
 *Empty constructor of Gibson. It will use the defalt settings in StochModel.
 */
Gibson::Gibson()
	:StochModel(),
	savedSampleCount(1),
	lastTime (std::numeric_limits<long>::min( ))
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
Gibson::Gibson(char* arg_infilename, char* arg_outfilename)
	:StochModel(),
	savedSampleCount(1),
	lastTime (std::numeric_limits<long>::min( ))
{
	Tree=NULL;
	currvals=NULL;

	outfilename=arg_outfilename;

	ifstream infile;
	string instring;
	infile.open(arg_infilename);
	if (!infile) {
		VCELL_EXCEPTION(runtime_error, "Unable to open file " << arg_infilename);
	}
	flag_savePeriod=false;
    bMultiButNotHisto = false;
	while(infile >> instring)
	{
		//load control info.
		if (instring == "STARTING_TIME")
		{
			infile >> STARTING_TIME;
		}
        else if (instring == "BMULTIBUTNOTHISTO")
        {
            infile >> bMultiButNotHisto;
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
				//bind probExpression with symboltable including variables and "t"(put at last)
				int lenNames=listOfVarNames.size();
				string names[lenNames+1];
				for (int k=0;k<lenNames;k++)
					names[k]=listOfVarNames.at(k);
				names[lenNames]=MY_T_STR;
				listOfProcesses[idx]->setProbabilityExpression(exp,names,(lenNames+1));
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
	if (NUM_TRIAL > MAX_ALLOWED_POINTS) {
		VCELL_EXCEPTION(invalid_argument,"Stochastic initialization: Server maximum number trials " << NUM_TRIAL << " exceeds limit of " << MAX_ALLOWED_POINTS);
	}
	if (MAX_SAVE_POINTS > MAX_ALLOWED_POINTS) {
		VCELL_EXCEPTION(invalid_argument,"Stochastic initialization: Server save points " << MAX_SAVE_POINTS << " exceeds limit of " << MAX_ALLOWED_POINTS);
	}


	if(NUM_TRIAL > 1 && !bMultiButNotHisto){
		//this must be a gibson 'histogram' sim,
		//java gui not allow setting of MAX_SAVE_POINTS and default is too low
		//makes no sense for 'histogram' sim to have MAX_SAVE_POINTS < NUM_TRIAL
		MAX_SAVE_POINTS = NUM_TRIAL;
	}

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
	double lastStepVals[listOfIniValues.size()];//to remember the last step values, used for save output by save_period
	double p, r; //temp variables used for propability and random number
	int saveIntervalCount = SAMPLE_INTERVAL; //sampling counter, for default output time spec, keep every
	int iterationCounter=0;//counter used for termination of the loop when max_iteration is reached
	int i; //loop variable
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
			VCELL_EXCEPTION(runtime_error,"at time point " << simtime << ", propensity of jump process "<< listOfProcessNames.at(jump->getNameIndex()) <<" evaluated to a negative value (" << p << "). Simulation abort!" << endl << jump->getProbabilityRateEvaluationSummary(currvals) );
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
			if((NUM_TRIAL ==1) && (flag_savePeriod))//SingleTrajectory_OutputInterval
			{	//use EPSILON here to make sure that a double 0.99999999999995(usually happen in C) is not regarded as a number that smaller than 1. It should be 1.
				while((outputTimer+SAVE_PERIOD+EPSILON) < ENDING_TIME)
				{
                    if(bMultiButNotHisto) {//Accumulate data mode
                        calcRunningStats(lastStepVals,listOfIniValues.size(),outputTimer + SAVE_PERIOD);
                    }else {
                        accumOrSaveInit(varLen, outputTimer + SAVE_PERIOD, true);
                        for (i = 0; i < varLen; i++) {
                            outfile << lastStepVals[i] << "\t";
                        }
                    }
//                    outfile << outputTimer+SAVE_PERIOD << "\t";
//					for(i=0;i<varLen;i++){
//						outfile<< lastStepVals[i] << "\t";
//					}
					savedSampleCount = finalizeSampleRow(savedSampleCount,simtime);//outfile << endl;
					outputTimer = outputTimer + SAVE_PERIOD;
				}
			}
			break;
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
			VCELL_EXCEPTION(runtime_error,"at time point " << simtime << ", propensity of jump process "<< listOfProcessNames.at(event->getNameIndex()) <<" evaluated to a negative value (" << p << "). Simulation abort!" << endl << event->getProbabilityRateEvaluationSummary(currvals) );
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
				VCELL_EXCEPTION(runtime_error, "at time point " << simtime << ", propensity of jump process "<< listOfProcessNames.at(dJump->getNameIndex()) <<" evaluated to a negative value (" << p << "). Simulation abort!" << endl << dJump->getProbabilityRateEvaluationSummary(currvals) );
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
		if(NUM_TRIAL ==1)//SingleTrajectory
		{
			if(saveIntervalCount == SAMPLE_INTERVAL)
			{
				//output the result to file if the num_trial is one and the outputTimer reaches the new save period
				if(flag_savePeriod)//OutputInterval
				{
					while((outputTimer+SAVE_PERIOD) < simtime)
					{
                        if(bMultiButNotHisto) {//Accumulate data mode
                            calcRunningStats(lastStepVals,listOfIniValues.size(),outputTimer + SAVE_PERIOD);
                        }else {
                            accumOrSaveInit(varLen, outputTimer + SAVE_PERIOD, true);
                            for (i = 0; i < varLen; i++) {
                                outfile << lastStepVals[i] << "\t";
                            }
                        }
						savedSampleCount = finalizeSampleRow(savedSampleCount,simtime);//outfile << endl;
						outputTimer = outputTimer + SAVE_PERIOD;
					}
					if(outputTimer+SAVE_PERIOD <= simtime + EPSILON)
					{
						outfile << outputTimer+SAVE_PERIOD << "\t";
						for(i=0;i<varLen;i++){
							outfile<< *listOfVars.at(i)->getCurr() << "\t";
							savedSampleCount = finalizeSampleRow(savedSampleCount,simtime);//outfile << endl;
						}
						outputTimer = outputTimer + SAVE_PERIOD;
					}
				}
				else //KeepEvery
				{
					outfile << simtime << "\t";
					for(i=0;i<varLen;i++){
						outfile<< *listOfVars.at(i)->getCurr()<< "\t";
					}
					savedSampleCount = finalizeSampleRow(savedSampleCount,simtime);//outfile << endl;
				}
			}
			if(saveIntervalCount == 1)
				saveIntervalCount = SAMPLE_INTERVAL;
			else
				saveIntervalCount--;
		}//if(NUM_TRIAL ==1)
//		else{
//            frmprint(simtime, lastStepVals,sizeof(lastStepVals)/sizeof(lastStepVals[0]));
//        }
	}//end of while loop
//    frmprint(simtime, lastStepVals,sizeof(lastStepVals)/sizeof(lastStepVals[0]));
	//output the variable's vals at the ending time point.
	if((simtime > ENDING_TIME) && (NUM_TRIAL == 1) && (flag_savePeriod))//SingleTrajectory_OutputInterval
	{
        if(bMultiButNotHisto) {//Accumulate data mode
            calcRunningStats(lastStepVals,listOfIniValues.size(),ENDING_TIME);
        }else {
            accumOrSaveInit(listOfVars.size(), ENDING_TIME, false);
            for (i = 0; i < listOfVars.size(); i++) {
                outfile << "\t" << *listOfVars.at(i)->getCurr();
            }
        }
		savedSampleCount = finalizeSampleRow(savedSampleCount,simtime);//outfile << endl;
	}
	//to save the (last-step)values of variables after one run of simulation
	if(NUM_TRIAL >1)//Histogram
	{
		for(i=0;i<listOfVars.size();i++)
		{
			outfile<< "\t" << *listOfVars.at(i)->getCurr();
		}
		savedSampleCount = finalizeSampleRow(savedSampleCount,simtime);//outfile << endl;
	}
    //return parameter 0:ends by ending_time  1:ends by max_iteration
	if(iterationCounter< MAX_ITERATION)
	{
		return 0;
	}
	else return 1;
}

void Gibson::accumOrSaveInit(int varLen,double timePoint,bool bAddTab) {
    if(bAddTab) {
        outfile << timePoint << "\t";
    }else{
        outfile << timePoint;
    }
}
//end of method core()
//void Gibson::frmprint(double simtime, const double *lastStepVals,int sizelsv) {
//    cout << "t:";
//    for(int i=0; i < listOfVarNames.size(); i++){
//        cout << listOfVarNames.at(i) << ":";
//    }
//    cout << endl;
//    cout << simtime <<":" << ENDING_TIME <<" ";
//    for(int row = 0; row < sizelsv; row++) {
//        cout << lastStepVals[row] << " ";
//    }
//    cout <<endl;
//}


int Gibson::finalizeSampleRow(int savedSampleCount,double simtime){
    if(!bMultiButNotHisto) {
        outfile << endl;
    }
//	cout << "savedSampleCount=" << savedSampleCount << endl;
	if(savedSampleCount > MAX_SAVE_POINTS) {
		VCELL_EXCEPTION(runtime_error,
			"Simulation exceeded maximum saved time points " << MAX_SAVE_POINTS 
			<< ". Partial results may be available. \nYou can increase the save interval (\"keep every\") or increase the maximum number of saved time points (\"keep at most\").");
	}
	//progress no more than every 2 seconds
	if (difftime(time(NULL),lastTime) > 2.0){
		lastTime = time(NULL);
        if(bMultiButNotHisto) {
            double percentile =  static_cast<double>(currMultiNonHistoIter)  / static_cast<double>(numMultiNonHisto);
#ifdef USE_MESSAGING
            SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_PROGRESS, percentile, currMultiNonHistoIter));
#else
            printf("[[[progress:%lg%%]]]", percentile * 100);
            fflush(stdout);
#endif
        } else if(NUM_TRIAL > 1){//histogram
			double percentile =  static_cast<double>(savedSampleCount)  / NUM_TRIAL;
#ifdef USE_MESSAGING
			SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_PROGRESS, percentile, savedSampleCount));
#else
			printf("[[[progress:%lg%%]]]", percentile * 100);
			fflush(stdout);
#endif
		}else{//single_trajectory
			double percentile = (simtime/ENDING_TIME);
#ifdef USE_MESSAGING
			SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_PROGRESS, percentile, simtime) );
			SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_DATA, percentile, simtime) );
#else
			printf("[[[progress:%lg%%]]]", percentile * 100);
			printf("[[[data:%lg]]]", simtime);
			fflush(stdout);
#endif
		}
	}

	return savedSampleCount+1;
}


template <typename T>
void Gibson::calcRunningStats(T valuesFor1Time1Iteration[],int numVars,double timePoint){
    if(mean.size() < savedSampleCount){
        timePoints.push_back(timePoint);
        mean.push_back(vector< double>(listOfIniValues.size(),0));
        M2.push_back(vector< double>(listOfIniValues.size(),0));
        variance.push_back(vector< double>(listOfIniValues.size(),0));
        statMin.push_back(vector< double>(listOfIniValues.size(),std::numeric_limits<double>::max()));
        statMax.push_back(vector< double>(listOfIniValues.size(),0));
    }
   for (int i = 0; i < numVars; i++) {
        //mean[time][var]
       double currVarVal = valuesFor1Time1Iteration[i];
//// Check stats caluclation, save stats into first variable, uncomment random library at beginning of file to set expected mean,stddev
//       if(i==0){
//           currVarVal = distribution(generator);
//       }
       double delta = currVarVal - mean.data()[savedSampleCount - 1].data()[i];
        mean.data()[savedSampleCount-1].data()[i] += delta / (currMultiNonHistoIter + 1);
        M2.data()[savedSampleCount-1].data()[i] += delta * (currVarVal - mean.data()[savedSampleCount - 1].data()[i]);
        variance.data()[savedSampleCount-1].data()[i] = M2.data()[savedSampleCount-1].data()[i] / (currMultiNonHistoIter + 1);
        statMin.data()[savedSampleCount-1].data()[i] = std::min(static_cast<double>(currVarVal), statMin.data()[savedSampleCount - 1].data()[i]) ;
        statMax.data()[savedSampleCount-1].data()[i] = std::max(static_cast<double>(currVarVal), statMax.data()[savedSampleCount - 1].data()[i]) ;
    }
}

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
    if(bMultiButNotHisto)
    {
//        char thebyte[1];
//        thebyte[0]=49;//progress char "1"
//        string ofProg(outfilename);
//        ofProg.append("_progress");
//        ofstream outfileProg;
//        outfileProg.open (ofProg.c_str(),ios::out | ios::app | ios::binary);

        numMultiNonHisto = NUM_TRIAL;
        NUM_TRIAL = 1;//set to 1 to use single trajectory logic in 'core'

        //output file header description for time and variable names
        outfile << "t:";
        for (int i = 0; i < listOfVarNames.size(); i++) {
            outfile << listOfVarNames.at(i) << ":";
        }
        outfile << endl;
        //
        //output STARTING_TIME(time0) and initial condition at STARTING_TIME
        outfile << STARTING_TIME << "\t";

        for (int i = 0; i < listOfIniValues.size(); i++) {
            outfile << listOfIniValues.at(i) << "\t";
        }
        outfile << endl;
        //Execute NUM_TRIALS of core and accumulate the results
        for (currMultiNonHistoIter=0; currMultiNonHistoIter < numMultiNonHisto; currMultiNonHistoIter++)
        {
            srand(currMultiNonHistoIter+SEED);
            //run the simulation
            core();
            //reset to initial values before next simulation
            savedSampleCount = 1;
            for(int i=0;i<listOfIniValues.size();i++){
                listOfVars[i]->setCurr(listOfIniValues.at(i));
            }
            for(int i=0;i<listOfProcesses.size();i++){
                Tree->setProcess(i,listOfProcesses.at(i));
            }
//            outfileProg.write((char *)&thebyte,1);
//            outfileProg.flush();
        }
//        outfileProg.close();

        //Calc and save averages of accumulated data
        for (int i = 0; i < mean.size(); ++i) {
            //timepoint
            outfile << timePoints.data()[i] << "\t";
            for (int j = 0; j < mean.data()[i].size(); ++j) {
                outfile << mean.data()[i].data()[j] << "\t";
            }
            outfile << endl;
        }

        //
        //Create HDF5 file
        //
        string ofhdf5(outfilename);
        ofhdf5.append("_hdf5");
        try{
            hid_t file; //file handle
            file = H5Fcreate(ofhdf5.c_str(), H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);

            hid_t dataspace,datatype,dataset; /* general data structure handles */
            herr_t status;
            hid_t doubleDataType;
            doubleDataType = H5Tcopy (H5T_NATIVE_DOUBLE);

            //
            //Save varnames to hdf5 file
            //
            hid_t varLenStr;   /* variable length string datatype */
            int rank = 1; //num dimensions
            hsize_t varNamesDim[rank]; /*container for size of VarNames  1-d array */
            string varName("VarNames");


            varNamesDim[0] = listOfVarNames.size();//set size of dims in container
            dataspace = H5Screate_simple(rank, varNamesDim, NULL);
            varLenStr = H5Tcopy (H5T_C_S1);
            H5Tset_size (varLenStr, H5T_VARIABLE);
            dataset = H5Dcreate1(file, varName.c_str(), varLenStr, dataspace, H5P_DEFAULT);
            //For variable-names, create vector of pointers to c-style strings
            std::vector<const char*> chars;
            for (int i=0;i < listOfVarNames.size();i++) {
                chars.push_back(listOfVarNames[i].c_str());
            }
            status = H5Dwrite(dataset, varLenStr, H5S_ALL, H5S_ALL, H5P_DEFAULT, chars.data());

            H5Sclose(dataspace);
            H5Tclose(varLenStr);
            H5Dclose(dataset);

            //
            //Save times
            //
            rank = 1;
            hsize_t timesDim[rank];
            string timeName("SimTimes");


            timePoints.insert(timePoints.begin(),STARTING_TIME);
            timesDim[0] = timePoints.size();//add 1 to include time 0
            dataspace = H5Screate_simple(rank, timesDim, NULL);
            dataset = H5Dcreate1(file, timeName.c_str(), doubleDataType, dataspace, H5P_DEFAULT);
            status = H5Dwrite(dataset, doubleDataType, H5S_ALL, H5S_ALL, H5P_DEFAULT, timePoints.data());

            H5Sclose(dataspace);
            //H5Tclose(varLenStr);
            H5Dclose(dataset);

            //
            //Save Stats for all times and variables
            //
            rank = 2;
            hsize_t meanDim[rank];
            meanDim[0] = timePoints.size();
            meanDim[1] = listOfVarNames.size();
            dataspace = H5Screate_simple(rank, meanDim, NULL);
            string statsNames[4];
            int varianceIndex = 3;
            statsNames[0]="StatMean";
            statsNames[1]="StatMin";
            statsNames[2]="StatMax";
            statsNames[varianceIndex]="StatStdDev";//converted to stddev during write to file
            vector< vector<double> > statTypes[4];
            statTypes[0] = mean;
            statTypes[1] = statMin;
            statTypes[2] = statMax;
            statTypes[varianceIndex] = variance;
            for (int k=0;k<4;k++) {
                dataset = H5Dcreate1(file, statsNames[k].c_str(), doubleDataType, dataspace, H5P_DEFAULT);
                double allData[meanDim[0]][meanDim[1]];
                for (int i = 0; i < meanDim[0]; ++i) {
                    for (int j = 0; j < meanDim[1]; ++j) {
                        if (i == 0) {
                            if (k == varianceIndex) {
                                allData[i][j] = 0;//no variance at starttime over all trials
                            } else {
                                allData[i][j] = listOfIniValues[j];//mean,min,max have ini vals at starttime over all trials
                            }
                        } else {
                            allData[i][j] = statTypes[k][i - 1][j];
                        }
                        if(k==varianceIndex){//turn variance into stddev
                            allData[i][j] = sqrt(allData[i][j]);
                        }
                    }
                }
                status = H5Dwrite(dataset, doubleDataType, H5S_ALL, H5S_ALL, H5P_DEFAULT, allData);
               H5Dclose(dataset);
            }
            H5Sclose(dataspace);

            H5Fclose(file);
        } catch (const std::exception& ex) {
            std::cout << " Error writing HDF5 file " << ofhdf5 << " " << ex.what() << "'\n";
        } catch (const std::string& ex) {
            std::cout << " Error writing HDF5 file " << ofhdf5 << " " << ex << "'\n";
        } catch (...) {
            std::cout << " Error writing HDF5 file " << ofhdf5 << "" << "unknown exception" << "'\n";
        }
    }
	else if(NUM_TRIAL==1)
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
			outfile << j-SEED+1;//this expression should evaluate equal to 'savedSampleCount'
			core();//this will save 1 row of data (
			//reset to initial values before next simulation
			for(int i=0;i<listOfIniValues.size();i++){
				listOfVars[i]->setCurr(listOfIniValues.at(i));
			}
			for(int i=0;i<listOfProcesses.size();i++){
				Tree->setProcess(i,listOfProcesses.at(i));
			}
		}
	}
	else
	{
		VCELL_EXCEPTION(invalid_argument, "Number of trial smaller than 1!");
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
