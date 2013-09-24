/* Steven Andrews, started 10/22/2001.
 This is the entry point for the Smoldyn program.  See documentation
 called Smoldyn_doc1.pdf and Smoldyn_doc2.pdf.
 Copyright 2003-2011 by Steven Andrews.  This work is distributed under the terms
 of the Gnu General Public License (GPL). */


#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <VCELL/SimulationMessaging.h>
#include "opengl2.h"
#include "smoldyn.h"
#include "random2.h"
#include "smoldynfuncs.h"
#include "SimpleValueProvider.h"
#include "SimpleMesh.h"
#include <iostream>

/* ***************************************************************** */
/* ********************** main() segment *************************** */
/* ***************************************************************** */
#if !defined(SVNVERSION)
#error SVNVERSION version not defined
#endif
#define VCELLSVNQ(x) #x
#define VCELLSVNQUOTE(x) VCELLSVNQ(x)


int taskID = -1;

/* main */
int main(int argc,char **argv) {
    	std::cout 
	    << "Smoldyn solver version $URL$"VCELLSVNQUOTE(SVNVERSION) 
	    << std::endl; 
	const int errMsgLen = 2048;
	char errorMsg[errMsgLen];
	int exitCode = 0;
	  simptr sim;
	try {
		simSetThrowing(10);
		SimulationMessaging::create();
	
	  //clear the errormesage
	  memset(errorMsg, 0, errMsgLen * sizeof(char));

	  int i,er,pflag,qflag,wflag,tflag,Vflag,oflag;
	  char root[STRCHAR],fname[STRCHAR],flags[STRCHAR],*cptr;

		for(i=0;i<STRCHAR;i++) root[i]=fname[i]=flags[i]='\0';
		er=0;
		if(argc<=1) {
			fprintf(stderr,"Welcome to Smoldyn version %s.\n\n",VERSION);
			fprintf(stderr,"Enter name of configuration file: ");
			fgets(root,STRCHAR,stdin);
			if(strchr(root,'\n')) *(strchr(root,'\n'))='\0';
			fprintf(stderr,"Enter runtime flags (q=quiet, p=parameters only), or '-'=none: ");
			fgets(flags,STRCHAR,stdin);
			if(strchr(flags,'\n')) *(strchr(flags,'\n'))='\0'; }
		if(argc>1) {
			strncpy(root,argv[1],STRCHAR-1);
			root[STRCHAR-1]='\0';
			argc--;
			argv++; }
		er=Parse_CmdLineArg(&argc,argv,NULL);
		if(er) {
			if(er==1) fprintf(stderr,"Out of memory");
			else fprintf(stderr,"Follow command line '--define' options with key=replacement\n");
			return 0; }
		if(argc>1) {
			if(argv[1][0]=='-') {
				// -tid is always at the end of argument list
				// and it is used in conjunction with PBS
				// should not be used when called from command line manually
				if (strcmp(argv[1], "-tid")) {
					strncpy(flags,argv[1],STRCHAR-1);
					flags[STRCHAR-1]='\0';
					strcpy(SimFlags,flags);
					argc--;
					argv++; }
			}
			else {
				fprintf(stderr,"Command line format: smoldyn [config_file] [-options] [-OpenGL_options]\n");
				return 0; }}

		cptr=strrpbrk(root,":\\/");
		if(cptr) cptr++;
		else cptr=root;
		strcpy(fname,cptr);
		*cptr='\0';

		oflag=strchr(flags,'o')?1:0;
		pflag=strchr(flags,'p')?1:0;
		qflag=strchr(flags,'q')?1:0;
		Vflag=strchr(flags,'V')?1:0;
		if(!strcmp(fname,"-V")) Vflag=1;
		wflag=strchr(flags,'w')?1:0;
		tflag=strchr(flags,'t')?1:0;

		if(Vflag) {
			simLog(NULL,4,"%s\n",VERSION);
			return 0; }
		sim=NULL;

		if (argc > 1) {
			if (!strcmp(argv[1], "-tid")) {
				argc --;
				argv ++;

				sscanf(argv[1], "%d", &taskID);
				argc --;
				argv ++;
			}
		}

		er=simInitAndLoad(root,fname,&sim,flags, NULL, NULL);
		if(!er) {
			if(!tflag && sim->graphss && sim->graphss->graphics!=0)
				gl2glutInit(&argc,argv);
			er=simUpdateAndDisplay(sim); }
		if(!oflag && !pflag && !er)
		{
			wflag = 1; //always overwrite smoldyn output for VCell
			er=scmdopenfiles((cmdssptr) sim->cmds,wflag);
		}
		if(pflag || er) {
			simLog(sim,4,"Simulation skipped\n"); }
		else {
			fflush(stdout);
			fflush(stderr);
			if(tflag || !sim->graphss || sim->graphss->graphics==0) {
				er=smolsimulate(sim);
				endsimulate(sim,er);
				SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_COMPLETED, 1.0, sim->time));}
			else {
				smolsimulategl(sim); }}
		simfree(sim);
		simfuncfree(); }

	catch (const char* errmsg) {
		fprintf(stderr, "%s\n", errmsg);
		exitCode = 1; }
	catch (int stop) {
	// stopped by user
	}
	catch (...) {
		fprintf(stderr, "unknown error\n");
		exitCode = 1; }

	if (SimulationMessaging::getInstVar() == NULL) {
		if (exitCode != 0) {
			simLog(sim,10, "%s\n", errorMsg);
		}
	} else if (!SimulationMessaging::getInstVar()->isStopRequested()) {
		if (exitCode != 0) {
			SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_FAILURE, errorMsg));
		}
#ifdef USE_MESSAGING
		SimulationMessaging::getInstVar()->waitUntilFinished();
#endif
	}
	delete SimulationMessaging::getInstVar();

	return exitCode; }

