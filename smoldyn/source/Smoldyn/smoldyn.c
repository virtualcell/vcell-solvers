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
#include "smoldyn_config.h"
#include "opengl2.h"
#include "smoldyn.h"

int main(int argc,char *argv[]);
void RenderScene(void);
void TimerFunction(int er);
void smolsimulategl(simptr sim);

simptr Sim;


/* RenderScene is the rendering call-back function for OpenGL. */
void RenderScene(void) {
	RenderSim(Sim);
	return; }


/* TimerFunction is the call-back function for OpenGL that runs the simulation.
er is positive if the simulation should quit due to a simulation error or normal
ending, er is negative if the simulation has been over, and er is 0 if the
simulation is proceeding normally.  This also looks at the state defined in my
opengl2 library; if it is 0, the simulation is continuing, if it is 1, the
simulation is in pause mode, and if it is 2, the user told the simulation to
quit.  This function runs one simulation time step, posts graphics redisplay
flags, and saves TIFF files as appropriate. */
#ifdef __gl_h_

void TimerFunction(int er) {
	static int it=0;
	static int oldstate=0;
	unsigned int delay;
	int qflag;
	simptr sim;
	graphicsssptr graphss;

	sim=Sim;
	graphss=sim->graphss;
	qflag=strchr(sim->flags,'q')?1:0;
	delay=graphss->graphicdelay;

	if(gl2State(-1)==0 && oldstate==1) {							// leave pause state
		oldstate=0;
		sim->clockstt=time(NULL);
		if(!qflag) printf("Simulation running\n"); }

	if(er==0 && gl2State(-1)==0) {										// normal run mode
		if(!(it%graphss->graphicit)) glutPostRedisplay();
		if(graphss->tiffit>0 && it>0 && !((it-1)%graphss->tiffit)) gl2SetKeyPush('T');
		er=simulatetimestep(sim);
		it++; }
	else if(er>0 || (er==0 && gl2State(-1)==2)) {			// stop the simulation
		if(oldstate==0) sim->elapsedtime+=difftime(time(NULL),sim->clockstt);
		endsimulate(sim,er);
		er=-1; }
	else if(gl2State(-1)==1 && oldstate==0) {					// enter pause state
		sim->elapsedtime+=difftime(time(NULL),sim->clockstt);
		oldstate=1;
		delay=20;
		if(!qflag) printf("Simulation paused at simulation time: %g\n",sim->time); }
	else {																						// still in pause state or simulation is over
		glutPostRedisplay();
		delay=20; }

	glutTimerFunc(delay,TimerFunction,er);
	return; }

#else

void TimerFunction(int er) {
	return; }

#endif


/* smolsimulategl initiates the simulation using OpenGL graphics.  It does all
OpenGL initializations, registers OpenGL call-back functions, sets the global
variables to their proper values, and then hands control over to OpenGL.  This
function returns as the program quits. */
#ifdef __gl_h_

void smolsimulategl(simptr sim) {
	int dim,qflag,lt,er;
	wallptr *wlist;
	graphicsssptr graphss;
	GLenum gllightnum;

	graphss=sim->graphss;
	qflag=strchr(sim->flags,'q')?1:0;
	gl2SetOptionInt("Fix2DAspect",1);
	gl2SetOptionVoid("FreeFunc",&simfree);
	gl2SetOptionVoid("FreePointer",(void*)sim);
	if(!qflag) printf("Starting simulation\n");
	dim=sim->dim;
	wlist=sim->wlist;
	//char* wname=sim->filename;
	char* wname="Particle View";
	if(dim==1) gl2Initialize(wname,(float)wlist[0]->pos,(float)wlist[1]->pos,0,0,0,0);
	else if(dim==2) gl2Initialize(wname,(float)wlist[0]->pos,(float)wlist[1]->pos,(float)wlist[2]->pos,(float)wlist[3]->pos,0,0);
	else {
		gl2Initialize(wname,(float)wlist[0]->pos,(float)wlist[1]->pos,(float)wlist[2]->pos,(float)wlist[3]->pos,(float)wlist[4]->pos,(float)wlist[5]->pos);
		if(sim->srfss) {
			glEnable(GL_BLEND);
			glBlendFunc(GL_SRC_ALPHA,GL_ONE_MINUS_SRC_ALPHA);}}
	glClearColor((GLclampf)graphss->backcolor[0],(GLclampf)graphss->backcolor[1],(GLclampf)graphss->backcolor[2],(GLclampf)graphss->backcolor[3]);

	if(graphss->graphics>=3) {
		glEnable(GL_LIGHTING);
		glLightModelfv(GL_LIGHT_MODEL_AMBIENT,graphss->ambiroom);
		glLightModeli(GL_LIGHT_MODEL_TWO_SIDE,1);
		for(lt=0;lt<MAXLIGHTS;lt++)
			if(graphss->lightstate[lt]==LPon) {
				if(lt==0) gllightnum=GL_LIGHT0;
				else if(lt==1) gllightnum=GL_LIGHT1;
				else if(lt==2) gllightnum=GL_LIGHT2;
				else if(lt==3) gllightnum=GL_LIGHT3;
				else if(lt==4) gllightnum=GL_LIGHT4;
				else if(lt==5) gllightnum=GL_LIGHT5;
				else if(lt==6) gllightnum=GL_LIGHT6;
				else gllightnum=GL_LIGHT7;
				glLightfv(gllightnum,GL_AMBIENT,graphss->ambilight[lt]);
				glLightfv(gllightnum,GL_DIFFUSE,graphss->difflight[lt]);
				glLightfv(gllightnum,GL_SPECULAR,graphss->speclight[lt]);
				glLightfv(gllightnum,GL_POSITION,graphss->lightpos[lt]);
				glEnable(gllightnum); }
		glEnable(GL_COLOR_MATERIAL);
		glColorMaterial(GL_FRONT_AND_BACK,GL_AMBIENT_AND_DIFFUSE); }

	glutDisplayFunc(RenderScene);
	glutTimerFunc(0,TimerFunction,0);
	sim->clockstt=time(NULL);
	Sim=sim;
	er=simdocommands(sim);
	if(er) endsimulate(sim,er);
	glutMainLoop();
	return; }

#else

void smolsimulategl(simptr sim) {
	fprintf(stderr,"Graphics are unavailable, so performing non-graphics simulation.\n");
	smolsimulate(sim);
	return; }

#endif



/* ***************************************************************** */
/* ********************** main() segment *************************** */
/* ***************************************************************** */

/* main is a simple routine that provides an entry point to the program.  It
checks the command line arguments, prints a greeting, inputs the configuration
file name from the user, and then calls setupsim to load the configuration
file and set up all the structures.  If all goes well, it calls simulate or
simulategl to run the simulation. */

#include <VCELL/SimulationMessaging.h>
int taskID = -1;

int main(int argc,char *argv[]) {
	char errorMsg[2048];
	int returnCode = 0;

	try {
		SimulationMessaging::create();

  simptr sim;
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
		if(er==1) throw("Out of memory");
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
		printf("%s\n",VERSION);
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

	er=setupsim(root,fname,&sim,flags);
	if(!oflag && !pflag && !er) er=scmdopenfiles(sim->cmds,wflag);
	if(pflag || er) {
    if(!qflag) printf("Simulation skipped\n"); }
  else {
		fflush(stdout);
		fflush(stderr);
    if(tflag || !sim->graphss || sim->graphss->graphics==0) {
    	er=smolsimulate(sim);
    	endsimulate(sim,er);
    	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_COMPLETED, 1.0, sim->time));	}
		else {
			gl2glutInit(&argc,argv);
			smolsimulategl(sim); }}
	simfree(sim);
	} catch (const char *exStr){
		strcpy(errorMsg, exStr);
		returnCode = 1;
	} catch (int stop) {
		// stopped by user
	}

	if (SimulationMessaging::getInstVar() == NULL) {
		if (returnCode != 0) {
			fprintf(stderr, "%s\n", errorMsg);
		}
	} else if (!SimulationMessaging::getInstVar()->isStopRequested()) {
		if (returnCode != 0) {
			SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_FAILURE, errorMsg));
		}
#ifdef USE_MESSAGING
		SimulationMessaging::getInstVar()->waitUntilFinished();
#endif
	}
	delete SimulationMessaging::getInstVar();

//	cptr=getenv("OS");
//	if(cptr!=NULL && strstr(cptr,"Windows")) {			// true for a Windows system
//		printf("Press ENTER to close window\n");
//		getchar(); }
  return returnCode; }

