/* Steven Andrews, started 10/22/01.
This is a library of functions for the Smoldyn program.  See documentation
called Smoldyn_doc1.doc and Smoldyn_doc2.doc.
Copyright 2003-2007 by Steven Andrews.  This work is distributed under the terms
of the Gnu Lesser General Public License (LGPL). */

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <time.h>
#include "smoldyn_config.h"
#include "math2.h"
#include "opengl2.h"
#include "random2.h"
#include "Rn.h"
#include "smoldyn.h"
#include "Zn.h"

#define CHECK(A) if(!(A)) goto failure; else (void)0
#define CHECKS(A,B) if(!(A)) {strncpy(erstr,B,STRCHAR-1);erstr[STRCHAR-1]='\0';goto failure;} else (void)0


/******************************************************************************/
/***************************** Simulation structure ***************************/
/******************************************************************************/


/******************************************************************************/
/********************************* enumerated types ***************************/
/******************************************************************************/


/* simstring2ss.  Returns the enumerated simulation structure type that
corresponds to the string input.  Returns SSnone if input is ÒnoneÓ or if it is
not recognized. */
enum SmolStruct simstring2ss(char *string) {
	enum SmolStruct ans;

	if(!strcmp(string,"molecule")) ans=SSmolec;
	else if(!strcmp(string,"wall")) ans=SSwall;
	else if(!strcmp(string,"reaction")) ans=SSrxn;
	else if(!strcmp(string,"surface")) ans=SSsurf;
	else if(!strcmp(string,"box")) ans=SSbox;
	else if(!strcmp(string,"compartment")) ans=SScmpt;
	else if(!strcmp(string,"port")) ans=SSport;
	else if(!strcmp(string,"command")) ans=SScmd;
	else if(!strcmp(string,"simulation")) ans=SSsim;
	else if(!strcmp(string,"check")) ans=SScheck;
	else if(!strcmp(string,"all")) ans=SSall;
	else ans=SSnone;
	return ans; }


/* simss2string.  Returns the string that corresponds to the enumerated
simulation structure input in string, which needs to be pre-allocated.  The
address of the string is returned to allow for function nesting. */
char *simss2string(enum SmolStruct ss,char *string) {
	if(ss==SSmolec) strcpy(string,"molecule");
	else if(ss==SSwall) strcpy(string,"wall");
	else if(ss==SSrxn) strcpy(string,"reaction");
	else if(ss==SSsurf) strcpy(string,"surface");
	else if(ss==SSbox) strcpy(string,"box");
	else if(ss==SScmpt) strcpy(string,"compartment");
	else if(ss==SSport) strcpy(string,"port");
	else if(ss==SScmd) strcpy(string,"command");
	else if(ss==SSsim) strcpy(string,"simulation");
	else if(ss==SScheck) strcpy(string,"check");
	else if(ss==SSall) strcpy(string,"all");
	else strcpy(string,"none");
	return string; }


/* simsc2string.  Returns the string that corresponds to the enumerated
structure condition input in string, which needs to be pre-allocated.  The
address of the string is returned to allow for function nesting. */
char *simsc2string(enum StructCond sc,char *string) {
	if(sc==SCinit) strcpy(string,"not initialized");
	else if(sc==SClists) strcpy(string,"lists need updating");
	else if(sc==SCparams) strcpy(string,"parameters need updating");
	else if(sc==SCok) strcpy(string,"ok");
	else strcpy(string,"none");
	return string; }


/******************************************************************************/
/****************************** low level utilities ***************************/
/******************************************************************************/

/* Simsetrandseed.  This sets the random number seed to randseed in the
simulation structure and for the random number generator. */
void Simsetrandseed(simptr sim,long int randseed) {
	if(!sim) return;
	sim->randseed=randseed;
	randomize(randseed);
	return; }


/******************************************************************************/
/******************************* memory management ****************************/
/******************************************************************************/

/* simalloc.  Allocates a simulation structure.  Walls are allocated and
inialized.  The box superstructure is allocated and initialized, although the
list of boxes is left as NULL.  The molecule superstructure is left as NULL.
The commands superstructure is allocated, but the queue of commands and the
output file lists are left as NULLs.  fileroot is required for the command
superstructure.  simalloc also sets the random number generator seed. */
simptr simalloc(char *fileroot) {
  simptr sim;
  int order;
	enum EventType et;

	sim=NULL;
	CHECK(sim=(simptr) malloc(sizeof(struct simstruct)));
	sim->condition=SCinit;
	sim->filepath=NULL;
	sim->filename=NULL;
	sim->flags=NULL;
	sim->clockstt=time(NULL);
	sim->elapsedtime=0;
	sim->randseed=randomize(-1);
	for(et=(EventType)0;et<ETMAX;et=(EventType)(et+1)) sim->eventcount[et]=0;
	sim->dim=0;
	sim->accur=10;
	sim->time=0;
	sim->tmin=0;
	sim->tmax=10;
	sim->dt=1;
	for(order=0;order<MAXORDER;order++) sim->rxnss[order]=NULL;
	sim->mols=NULL;
	sim->wlist=NULL;
	sim->srfss=NULL;
	sim->boxs=NULL;
	sim->cmptss=NULL;
	sim->portss=NULL;
	sim->mzrss=NULL;
	sim->cmds=NULL;
	sim->graphss=NULL;
	sim->threads = NULL;//?????????? new line

	disablesimthreading(sim);//?????? new line
	
	CHECK(sim->filepath=EmptyString());
	CHECK(sim->filename=EmptyString());
	CHECK(sim->flags=EmptyString());
	CHECK(sim->cmds=scmdssalloc(&docommand,(void*)sim,fileroot));
	return sim;

 failure:
	simfree(sim);
	return NULL; }



/* simfree frees a simulation strucutre, including every part of everything in
it. */
void simfree(simptr sim) {
	int dim,order;

	if(!sim) return;
	dim=sim->dim;

	graphssfree(sim->graphss);
	scmdssfree(sim->cmds);
	mzrssfree(sim->mzrss);
	portssfree(sim->portss);
	compartssfree(sim->cmptss);
	boxssfree(sim->boxs);
	surfacessfree(sim->srfss);
	wallsfree(sim->wlist,dim);
	molssfree(sim->mols);
	threadssfree(sim->threads);//????????? new line
	for(order=0;order<MAXORDER;order++) rxnssfree(sim->rxnss[order]);
	free(sim->flags);
	free(sim->filename);
	free(sim->filepath);
	free(sim);
	return; }


/******************************************************************************/
/***************************** data structure output **************************/
/******************************************************************************/

/* simoutput prints out the overall simulation parameters, including simulation
time information, graphics information, the number of dimensions, what the
molecule types are, the output files, and the accuracy. */
void simoutput(simptr sim) {
	printf("SIMULATION PARAMETERS\n");
	if(!sim) {
		printf(" No simulation parameters\n\n");
		return; }
	printf(" file: %s%s\n",sim->filepath,sim->filename);
	printf(" starting clock time: %s",ctime(&sim->clockstt));
	printf(" %i dimensions\n",sim->dim);
	printf(" Accuracy level: %g\n",sim->accur);
	printf(" Random number seed: %li\n",sim->randseed);
	
	if (sim->threading) {				//?????????????? new code
		printf(" Using threading with %d threads\n", sim->threads->nthreads); }
	else {
		printf(" Running in single-threaded mode.\n"); }
	
	printf(" Time from %g to %g step %g\n",sim->tmin,sim->tmax,sim->dt);
	if(sim->time!=sim->tmin) printf(" Current time: %g\n",sim->time);
	printf("\n");
	return; }


/* writesim.  Writes all information about the simulation structure, but not its
substructures, to the file fptr using a format that can be read by Smoldyn.
This allows a simulation state to be saved. */
void writesim(simptr sim,FILE *fptr) {
	fprintf(fptr,"# General simulation parameters\n");
	fprintf(fptr,"# Configuration file: %s%s\n",sim->filepath,sim->filename);
	fprintf(fptr,"dim %i\n",sim->dim);
	fprintf(fptr,"# rand_seed for prior simulation was %li\n",sim->randseed);
	fprintf(fptr,"rand_seed %li  # this is a new random number\n",(long int)randULI());
	fprintf(fptr,"time_start %g\n",sim->tmin);
	fprintf(fptr,"time_stop %g\n",sim->tmax);
	fprintf(fptr,"time_step %g\n",sim->dt);
	fprintf(fptr,"time_now %g\n",sim->time);
	fprintf(fptr,"accuracy %g\n",sim->accur);
	if(sim->boxs->mpbox) fprintf(fptr,"molperbox %g\n",sim->boxs->mpbox);
	else if(sim->boxs->boxsize) fprintf(fptr,"boxsize %g\n",sim->boxs->boxsize);
	fprintf(fptr,"\n");
	return; }


/* checksimparams checks that the simulation parameters, including parameters of
sub-structures, have reasonable values.  If values seem to be too small or too
large, a warning is displayed to the standard output, although this does not
affect continuation of the program. */
void checksimparams(simptr sim) {
	int warn,error,warndiff;
	char string[STRCHAR];

	printf("PARAMETER CHECK\n");
	warn=error=0;

	error+=checkmolparams(sim,&warndiff);warn+=warndiff;
	error+=checkboxparams(sim,&warndiff);warn+=warndiff;
	error+=checkwallparams(sim,&warndiff);warn+=warndiff;
	error+=checkrxnparams(sim,&warndiff);warn+=warndiff;
	error+=checksurfaceparams(sim,&warndiff);warn+=warndiff;
	error+=checkcompartparams(sim,&warndiff);warn+=warndiff;
	error+=checkportparams(sim,&warndiff);warn+=warndiff;
	error+=mzrCheckParams(sim,&warndiff);warn+=warndiff;
	error+=checkgraphicsparams(sim,&warndiff);warn+=warndiff;

	if(sim->condition!=SCok) {
		warn++;
		printf(" WARNING: simulation structure %s\n",simsc2string(sim->condition,string)); }

	if(error>0) printf(" %i total errors\n",error);
	else printf(" No errors\n");
	if(warn>0) printf(" %i total warnings\n",warn);
	else printf(" No warnings\n");
	printf("\n");
	return; }



/******************************************************************************/
/******************************* structure set up *****************************/
/******************************************************************************/


int getnumberofthreads(simptr sim) { //???????? new function
#ifndef THREADING
	return 1;
#else
	if (!sim->threads) return 0;
	else return sim->threads->nthreads;
#endif
	}


void setnumberofthreads(simptr sim,int newNumber) { //??????????? new function
#ifndef THREADING
	return;
#else
	if (sim->threads && sim->threads->nthreads==newNumber) return;
	else {
		threadssfree(sim->threads);
		sim->threads=alloc_threadss(newNumber); }
#endif
	}


/* simsetcondition.  Sets the simulation structure condition to cond, if
appropriate.  Set upgrade to 1 if this is an upgrade, to 0 if this is a
downgrade, or to 2 to set the condition independent of its current value. */
void simsetcondition(simptr sim,enum StructCond cond,int upgrade) {
	if(!sim) return;
	if(upgrade==0 && sim->condition>cond) sim->condition=cond;
	else if(upgrade==1 && sim->condition<cond) sim->condition=cond;
	else if(upgrade==2) sim->condition=cond;
	return; }


/* simsetdim.  Sets the simulation dimensionality.  Returns 0 for success, 2 if
it had already been set (itÕs only allowed to be set once), or 3 if the
requested dimensionality is not between 1 and 3. */
int simsetdim(simptr sim,int dim) {
	if(sim->dim!=0) return 2;
	if(dim<1 || dim>DIMMAX) return 3;
	sim->dim=dim;
	return 0; }


/* simsettime.  Sets the appropriate simulation time parameter to time.  Enter
code as 0 to set the current time, 1 to set the starting time, 2 to set the
stopping time, or 3 to set the time step.  Returns 0 for success, 1 if an
illegal code was entered, or 2 if a negative or zero time step was entered. */
int simsettime(simptr sim,double time,int code) {
	int er;

	er=0;
	if(code==0) sim->time=time;
	else if(code==1) sim->tmin=time;
	else if(code==2) sim->tmax=time;
	else if(code==3) {
		if(time>0) {
			sim->dt=time;
			molsetcondition(sim->mols,SCparams,0);
			rxnsetcondition(sim,-1,SCparams,0);
			surfsetcondition(sim->srfss,SCparams,0); }
		else er=2; }
	else er=1;
	return er; }


/* simreadstring.  Reads and processes one line of text from the configuration
file, or some other source.  The first word of the line should be sent in as
word (terminated by a Ô\0Õ) and the rest sent in as line2.  I donÕt think that
this function changes either the contents of word or line2, but this should be
verified if itÕs important.  If this function is successful, it returns 0 and it
does not change the contents of erstr; if not, it returns 1 and it writes an
error message to erstr. */
int simreadstring(simptr sim,char *word,char *line2,char *erstr) {
	char nm[STRCHAR],nm1[STRCHAR],shapenm[STRCHAR],ch,rname[STRCHAR];
	int er,dim,i,nmol,d,i1,s,c,ll,order,more,rctident[MAXORDER],nprod,prdident[MAXPRODUCT],r,ord,prd,itct,prt,lt;
	double flt1,flt2,flt3,v1[DIMMAX*DIMMAX],v2[4],poslo[DIMMAX],poshi[DIMMAX];
	enum MolecState ms,rctstate[MAXORDER],prdstate[MAXPRODUCT];
	enum PanelShape ps;
	enum RevParam rpart;
	enum LightParam ltparam;
	rxnptr rxn;
	compartptr cmpt;
	surfaceptr srf;
	graphicsssptr graphss;
	long int li1;

	dim=sim->dim;

	// space and time

	if(!strcmp(word,"dim")) {											// dim
		itct=sscanf(line2,"%i",&d);
		CHECKS(itct==1,"dim needs to be an integer");
		er=simsetdim(sim,d);
		CHECKS(er!=2,"dim can only be entered once");
		CHECKS(er!=3,"dim has to be between 1 and 3");
		dim=sim->dim;
		CHECKS(!strnword(line2,2),"unexpected text following dim"); }

	else if(!strcmp(word,"boundaries")) {					// boundaries
		CHECKS(dim>0,"need to enter dim before boundaries");
		itct=sscanf(line2,"%i %lg %lg",&d,&flt1,&flt2);
		CHECKS(itct==3,"boundaries format: dimension position position [type]");
		CHECKS(d>=0 && d<dim,"boundaries dimension needs to be between 0 and dim-1");
		CHECKS(flt1<flt2,"the first boundary position needs to be smaller than the second one");
		line2=strnword(line2,4);
		ch='t';
		if(line2) {
			itct=sscanf(line2,"%c",&ch);
			CHECKS(itct==1,"boundary type character could not be read");
			CHECKS(ch=='r' || ch=='p' || ch=='a' || ch=='t',"boundaries type needs to be r, p, a, or t");
			line2=strnword(line2,2); }
		er=walladd(sim,d,0,flt1,ch);
		CHECKS(er!=1,"out of memory adding wall");
		CHECKS(er!=2,"SMOLDYN BUG: adding wall");
		er=walladd(sim,d,1,flt2,ch);
		CHECKS(er!=1,"out of memory adding wall");
		CHECKS(er!=2,"SMOLDYN BUG: adding wall");
		CHECKS(!line2,"unexpected text following boundaries"); }

	else if(!strcmp(word,"low_wall")) {						// low_wall
		CHECKS(dim>0,"need to enter dim before low_wall");
		itct=sscanf(line2,"%i %lg %c",&d,&flt1,&ch);
		CHECKS(itct==3,"low_wall format: dimension position type");
		CHECKS(d>=0 && d<dim,"low_wall dimension needs to be between 0 and dim-1");
		CHECKS(ch=='r' || ch=='p' || ch=='a' || ch=='t',"low_wall type needs to be r, p, a, or t");
		er=walladd(sim,d,0,flt1,ch);
		CHECKS(er!=1,"out of memory adding wall");
		CHECKS(er!=2,"SMOLDYN BUG: adding wall");
		CHECKS(!strnword(line2,4),"unexpected text following low_wall"); }

	else if(!strcmp(word,"high_wall")) {					// high_wall
		CHECKS(dim>0,"need to enter dim before high_wall");
		itct=sscanf(line2,"%i %lg %c",&d,&flt1,&ch);
		CHECKS(itct==3,"high_wall format: dimension position type");
		CHECKS(d>=0 && d<dim,"high_wall dimension needs to be between 0 and dim-1");
		CHECKS(ch=='r' || ch=='p' || ch=='a' || ch=='t',"high_wall type needs to be r, p, a, or t");
		er=walladd(sim,d,1,flt1,ch);
		CHECKS(er!=1,"out of memory adding wall");
		CHECKS(er!=2,"SMOLDYN BUG: adding wall");
		CHECKS(!strnword(line2,4),"unexpected text following high_wall"); }

	else if(!strcmp(word,"time_start")) {					// time_start
		itct=sscanf(line2,"%lg",&flt1);
		CHECKS(itct==1,"time_start needs to be a number");
		simsettime(sim,flt1,0);
		simsettime(sim,flt1,1);
		CHECKS(!strnword(line2,2),"unexpected text following time_start"); }

	else if(!strcmp(word,"time_stop")) {					// time_stop
		itct=sscanf(line2,"%lg",&flt1);
		CHECKS(itct==1,"time_stop needs to be a number");
		simsettime(sim,flt1,2);
		CHECKS(!strnword(line2,2),"unexpected text following time_stop"); }

	else if(!strcmp(word,"time_step")) {					// time_step
		itct=sscanf(line2,"%lg",&flt1);
		CHECKS(itct==1,"time_step needs to be a number");
		er=simsettime(sim,flt1,3);
		CHECKS(!er,"time step must be >0");
		CHECKS(!strnword(line2,2),"unexpected text following time_step"); }

	else if(!strcmp(word,"time_now")) {						// time_now
		itct=sscanf(line2,"%lg",&flt1);
		CHECKS(itct==1,"time_now needs to be a number");
		simsettime(sim,flt1,0);
		CHECKS(!strnword(line2,2),"unexpected text following time_now"); }

	// molecules

	else if(!strcmp(word,"max_species") || !strcmp(word,"max_names")) {		// max_species, max_names
		itct=sscanf(line2,"%i",&i1);
		CHECKS(itct==1,"max_species needs to be an integer");
		er=molsetmaxspecies(sim,i1);
		CHECKS(er!=1,"out of memory");
		CHECKS(er!=2,"max_species can only be entered once, and not after species");
		CHECKS(er!=3,"max_species needs to be at least 0");
		CHECKS(!strnword(line2,2),"unexpected text following max_species"); }

	else if(!strcmp(word,"species") || !strcmp(word,"names") || !strcmp(word,"name")) {// species, names, name
		if(!sim->mols) {
			i1=wordcount(line2);
			er=molsetmaxspecies(sim,i1);
			CHECKS(er!=1,"out of memory");
			CHECKS(er==0,"SMOLDYN BUG: species"); }
		while(line2) {
			itct=sscanf(line2,"%s",nm);
			CHECKS(itct==1,"failed to read species name");
			er=moladdspecies(sim,nm);
			CHECKS(er!=-2,"SMOLDYN BUG: add species");
			CHECKS(er!=-3,"more species are entered than were allocated with max_species");
			CHECKS(er!=-4,"'empty' is not a permitted species name");
			CHECKS(er!=-5,"this species has already been declared");
			line2=strnword(line2,2); }}

	else if(!strcmp(word,"max_mol")) {						// max_mol
		itct=sscanf(line2,"%i",&i1);
		CHECKS(itct==1,"max_mol needs to be an integer");
		er=molsetmaxmol(sim,i1);
		CHECKS(er!=1,"out of memory");
		CHECKS(er!=2,"need to enter dim before max_mol");
		CHECKS(er!=3,"need to enter max_species before max_mol");
		CHECKS(er!=4,"max_mol needs to be >0");
		CHECKS(!strnword(line2,2),"unexpected text following max_mol"); }

	else if(!strcmp(word,"difc")) {								// difc
		CHECKS(sim->mols,"need to enter species before difc");
		i=readmolname(sim,line2,&ms);
		CHECKS(i!=0,"empty molecules cannot diffuse");
		CHECKS(i!=-1,"difc format: name[(state)] value");
		CHECKS(i!=-2,"mismatched or improper parentheses around molecule state");
		CHECKS(i!=-3,"cannot read molecule state value");
		CHECKS(i!=-4,"molecule name not recognized");
		CHECKS(line2=strnword(line2,2),"missing data for difc");
		itct=sscanf(line2,"%lg",&flt1);
		CHECKS(itct==1,"diffusion coefficient value cannot be read");
		CHECKS(flt1>=0,"diffusion coefficient needs to be >=0");
		molsetdifc(sim,i,ms,flt1);
		CHECKS(!strnword(line2,2),"unexpected text following difc"); }

	else if(!strcmp(word,"difm")) {								// difm
		CHECKS(sim->mols,"need to enter species before difm");
		i=readmolname(sim,line2,&ms);
		CHECKS(i!=0,"empty molecules not permitted");
		CHECKS(i!=-1,"difm format: name[(state)] values");
		CHECKS(i!=-2,"mismatched or improper parentheses around molecule state");
		CHECKS(i!=-3,"cannot read molecule state value");
		CHECKS(i!=-4,"molecule name not recognized");
		CHECKS(line2=strnword(line2,2),"missing matrix in difm");
		itct=strreadnd(line2,dim*dim,v1,NULL);
		CHECKS(itct==dim*dim,"incomplete matrix in difm");
		CHECKS(molsetdifm(sim,i,ms,v1)==0,"out of memory in difm");
		CHECKS(!strnword(line2,dim*dim+1),"unexpected text following difm"); }

	else if(!strcmp(word,"drift")) {							// drift
		CHECKS(sim->mols,"need to enter species before drift");
		i=readmolname(sim,line2,&ms);
		CHECKS(i!=0,"empty molecules not permitted");
		CHECKS(i!=-1,"drift format: name[(state)] values");
		CHECKS(i!=-2,"mismatched or improper parentheses around molecule state");
		CHECKS(i!=-3,"cannot read molecule state value");
		CHECKS(i!=-4,"molecule name not recognized");
		CHECKS(line2=strnword(line2,2),"missing vector in drift");
		itct=strreadnd(line2,dim,v1,NULL);
		CHECKS(itct==dim,"incomplete vector in drift");
		CHECKS(molsetdrift(sim,i,ms,v1)==0,"out of memory in drift");
		CHECKS(!strnword(line2,dim+1),"unexpected text following drift"); }

	else if(!strcmp(word,"mol")) {								// mol
		CHECKS(sim->mols,"need to enter species before mol");
		CHECKS(sim->mols->maxd>0,"need to enter max_mol before mol");
		itct=sscanf(line2,"%i %s",&nmol,nm);
		CHECKS(itct==2,"mol format: number name position_vector");
		CHECKS(nmol>=0,"number of molecules added needs to be >=0");
		i=stringfind(sim->mols->spname,sim->mols->nspecies,nm);
		CHECKS(i>0,"name not recognized for mol");
		CHECKS(line2=strnword(line2,2),"insufficient data in mol command");
		if(sim->wlist)
			systemcorners(sim,poslo,poshi);
		else
			for(d=0;d<dim;d++) {poslo[d]=0;poshi[d]=1;}
		for(d=0;d<dim;d++) {
			CHECKS(line2=strnword(line2,2),"insufficient position data for mol");
			if(line2[0]=='u') {
				CHECKS(sim->wlist,"need to enter boundaries before using 'u' in a mol statement"); }
			else {
				itct=sscanf(line2,"%lg-%lg",&flt1,&flt2);
				if(itct==2) {
					poslo[d]=flt1;
					poshi[d]=flt2; }
				else {
					itct=sscanf(line2,"%lg",&flt1);
					CHECKS(itct==1,"cannot read position value for mol");
					poslo[d]=poshi[d]=flt1; }}}
		CHECKS(addmol(sim,nmol,i,poslo,poshi,0)==0,"more molecules assigned than allocated");
		CHECKS(!strnword(line2,2),"unexpected text following mol"); }

	else if(!strcmp(word,"surface_mol")) {				// surface_mol
		CHECKS(sim->mols,"need to enter species before surface_mol");
		CHECKS(sim->mols->maxd>0,"need to enter max_mol before surface_mol");
		CHECKS(sim->srfss,"surfaces need to be defined before surface_mol statement");
		CHECKS(sim->srfss->nsrf>0,"at least one surface needs to be defined before surface_mol");
		itct=sscanf(line2,"%i",&nmol);
		CHECKS(itct==1,"surface_mol format: nmol species(state) surface panel_shape panel_name [position]");
		CHECKS(nmol>=0,"in surface_mol, the number of molecules needs to be at least 0");
		CHECKS(line2=strnword(line2,2),"surface_mol format: nmol species(state) surface panel_shape panel_name [position]");
		i=readmolname(sim,line2,&ms);
		CHECKS(i!=0,"empty molecules not permitted");
		CHECKS(i!=-1,"error reading molecule name");
		CHECKS(i!=-2,"mismatched or improper parentheses around molecule state");
		CHECKS(i!=-3,"cannot read molecule state value");
		CHECKS(i!=-4,"molecule name not recognized");
		CHECKS(ms<MSMAX && ms!=MSsoln,"state needs to be front, back, up, or down");
		CHECKS(line2=strnword(line2,2),"surface_mol format: nmol species(state) surface panel_shape panel_name [position]");
		itct=sscanf(line2,"%s %s %s",nm,shapenm,nm1);
		CHECKS(itct==3,"surface_mol format: nmol species(state) surface panel_shape panel_name [position]");
		if(!strcmp(nm,"all")) s=-1;
		else {
			s=stringfind(sim->srfss->snames,sim->srfss->nsrf,nm);
			CHECKS(s>=0,"surface name in surface_mol is not recognized"); }
		ps=surfstring2ps(shapenm);
		CHECKS(ps!=PSnone,"in surface_mol, panel shape name not recognized");
		line2=strnword(line2,4);
		if(line2) {
			itct=strreadnd(line2,dim,v1,NULL);
			CHECKS(itct==dim,"in surface_mol, not enough position values are given");
			CHECKS(s>=0 && ps!=PSall && strcmp(nm1,"all"),"in surface_mol, use of coordinates requires that a specific panel be specified");
			er=addsurfmol(sim,nmol,i,ms,v1,NULL,s,ps,nm1);
			CHECKS(er!=1,"in surface_mol, unable to allocate temporary storage space");
			CHECKS(er!=2,"in surface_mol, panel name not recognized");
			CHECKS(er!=3,"not enough available molecules");
			line2=strnword(line2,dim+1); }
		else {
			er=addsurfmol(sim,nmol,i,ms,NULL,NULL,s,ps,nm1);
			CHECKS(er!=1,"in surface_mol, unable to allocate temporary storage space");
			CHECKS(er!=2,"in surface_mol, no panels match the given description");
			CHECKS(er!=3,"not enough available molecules"); }
		CHECKS(!line2,"unexpected text following surface_mol"); }

	else if(!strcmp(word,"compartment_mol")) {		// compartment_mol
		CHECKS(sim->mols,"need to enter species before compartment_mol");
		CHECKS(sim->mols->maxd>0,"need to enter max_mol before compartment_mol");
		CHECKS(sim->cmptss,"compartments need to be defined before compartment_mol statement");
		CHECKS(sim->cmptss->ncmpt>0,"at least one compartment needs to be defined before compartment_mol");
		itct=sscanf(line2,"%i",&nmol);
		CHECKS(itct==1,"compartment_mol format: nmol species compartment");
		CHECKS(nmol>=0,"the number of molecules needs to be at least 0");
		CHECKS(line2=strnword(line2,2),"compartment_mol format: nmol species compartment");
		i=readmolname(sim,line2,&ms);
		CHECKS(i!=0,"empty molecules not permitted");
		CHECKS(i!=-1,"error reading molecule name");
		CHECKS(i!=-2,"mismatched or improper parentheses around molecule state");
		CHECKS(i!=-3,"cannot read molecule state value");
		CHECKS(i!=-4,"molecule name not recognized");
		CHECKS(ms==MSsoln,"state needs to be solution");
		CHECKS(line2=strnword(line2,2),"compartment_mol format: nmol species compartment");
		itct=sscanf(line2,"%s",nm);
		CHECKS(itct==1,"compartment_mol format: nmol species compartment");
		c=stringfind(sim->cmptss->cnames,sim->cmptss->ncmpt,nm);
		CHECKS(c>=0,"compartment name is not recognized");
		er=addcompartmol(sim,nmol,i,sim->cmptss->cmptlist[c]);
		CHECKS(er!=2,"compartment volume is zero or nearly zero");
		CHECKS(er!=3,"not enough allocated molecules");
		CHECKS(!strnword(line2,2),"unexpected text following compartment_mol"); }

	else if(!strcmp(word,"molecule_lists")) {			// molecule_lists
		CHECKS(sim->mols,"need to enter max_species before molecule_lists");
		while(line2) {
			itct=sscanf(line2,"%s",nm);
			CHECKS(itct==1,"unable to read molecule list name");
			er=addmollist(sim,nm,MLTsystem);
			CHECKS(er!=-1,"SMOLDYN BUG: out of memory");
			CHECKS(er!=-2,"molecule list name has already been used");
			CHECKS(er!=-3,"SMOLDYN BUG: illegal addmollist inputs");
			line2=strnword(line2,2); }
		CHECKS(!line2,"unexpected text following molecule_lists"); }

	else if(!strcmp(word,"mol_list")) {						// mol_list
		CHECKS(sim->mols,"need to enter species before mol_list");
		CHECKS(sim->mols->nlist>0,"need to enter molecule_lists before mol_list");
		i=readmolname(sim,line2,&ms);
		CHECKS(i!=0,"empty molecules not permitted");
		CHECKS(i!=-1,"mol_list format: name[(state)] list_name");
		CHECKS(i!=-2,"mismatched or improper parentheses around molecule state");
		CHECKS(i!=-3,"cannot read molecule state value");
		if(i==-4) {
			ms=MSall;
			itct=sscanf(line2,"%s",nm);
			CHECKS(itct==1,"mol_list format: name[(state)] list_name");
			if(!strcmp(nm,"diffusing")) i=-6;
			else if(!strcmp(nm,"fixed")) i=-7;
			else CHECKS(0,"molecule name not recognized"); }
		CHECKS(line2=strnword(line2,2),"missing list name for mol_list");
		itct=sscanf(line2,"%s",nm);
		CHECKS(itct==1,"mol_list format: name[(state)] list_name");
		ll=stringfind(sim->mols->listname,sim->mols->nlist,nm);
		CHECKS(ll>=0,"molecule list name is not recognized");
		molsetlistlookup(sim,i,ms,ll);
		CHECKS(!strnword(line2,2),"unexpected text following mol_list"); }

	// graphics

	else if(!strcmp(word,"graphics")) {						// graphics
		itct=sscanf(line2,"%s",nm);
		CHECKS(itct==1,"missing graphics parameter");
		if(!sim->graphss) {
			CHECKS(sim->graphss=graphssalloc(),"SMOLDYN BUG: out of memory"); }
		graphss=sim->graphss;
		if(!strcmp(nm,"none")) graphss->graphics=0;
		else if(!strcmp(nm,"opengl")) graphss->graphics=1;
		else if(!strcmp(nm,"opengl_good")) graphss->graphics=2;
		else if(!strcmp(nm,"opengl_better")) graphss->graphics=3;
		else CHECKS(0,"graphics method not recognized");
		CHECKS(!strnword(line2,2),"unexpected text following graphics"); }

	else if(!strcmp(word,"graphic_iter")) {				// graphic_iter
		CHECKS(sim->graphss,"graphics needs to be entered before graphic_iter");
		graphss=sim->graphss;
		itct=sscanf(line2,"%i",&i1);
		CHECKS(itct==1,"graphic_iter need to be a number");
		CHECKS(i1>0,"graphic_iter needs to be >=1");
		graphss->graphicit=i1;
		CHECKS(!strnword(line2,2),"unexpected text following graphic_iter"); }

	else if(!strcmp(word,"graphic_delay")) {			// graphic_delay
		CHECKS(sim->graphss,"graphics needs to be entered before graphic_delay");
		graphss=sim->graphss;
		itct=sscanf(line2,"%i",&i1);
		CHECKS(itct==1,"graphic_delay need to be a number");
		CHECKS(i1>=0,"graphic_delay needs to be >=0");
		graphss->graphicdelay=i1;
		CHECKS(!strnword(line2,2),"unexpected text following graphic_delay"); }

	else if(!strcmp(word,"frame_thickness")) {		// frame_thickness
		CHECKS(sim->graphss,"graphics needs to be entered before frame_thickness");
		graphss=sim->graphss;
		itct=sscanf(line2,"%lg",&flt1);
		CHECKS(itct==1,"frame_thickness needs to be a number");
		CHECKS(flt1>=0,"frame_thickness needs to be ³0");
		graphss->framepts=flt1;
		CHECKS(!strnword(line2,2),"unexpected text following frame_thickness"); }

	else if(!strcmp(word,"frame_color")) {				// frame_color
		CHECKS(sim->graphss,"graphics needs to be entered before frame_color");
		graphss=sim->graphss;
		itct=sscanf(line2,"%lg %lg %lg",&flt1,&flt2,&flt3);
		CHECKS(itct==3,"frame_color format: red green blue [alpha]");
		CHECKS(flt1>=0 && flt1<=1,"frame_color values need to between 0 and 1");
		CHECKS(flt2>=0 && flt2<=1,"frame_color values need to between 0 and 1");
		CHECKS(flt3>=0 && flt3<=1,"frame_color values need to between 0 and 1");
		graphss->framecolor[0]=flt1;
		graphss->framecolor[1]=flt2;
		graphss->framecolor[2]=flt3;
		if((line2=strnword(line2,4))!=NULL) {
			itct=sscanf(line2,"%lg",&flt1);
			CHECKS(itct==1,"frame_color format: red green blue [alpha]");
			CHECKS(flt1>=0 && flt1<=1,"frame_color alpha value needs to be between 0 and 1");
			graphss->framecolor[3]=flt1;
			line2=strnword(line2,2); }
		CHECKS(!line2,"unexpected text following frame_color"); }

	else if(!strcmp(word,"grid_thickness")) {		// grid_thickness
		CHECKS(sim->graphss,"graphics needs to be entered before grid_thickness");
		graphss=sim->graphss;
		itct=sscanf(line2,"%lg",&flt1);
		CHECKS(itct==1,"grid_thickness needs to be a number");
		CHECKS(flt1>=0,"grid_thickness needs to be ³0");
		graphss->gridpts=flt1;
		CHECKS(!strnword(line2,2),"unexpected text following grid_thickness"); }

	else if(!strcmp(word,"grid_color")) {					// grid_color
		CHECKS(sim->graphss,"graphics needs to be entered before grid_color");
		graphss=sim->graphss;
		itct=sscanf(line2,"%lg %lg %lg",&flt1,&flt2,&flt3);
		CHECKS(itct==3,"grid_color format: red green blue [alpha]");
		CHECKS(flt1>=0 && flt1<=1,"grid_color values need to between 0 and 1");
		CHECKS(flt2>=0 && flt2<=1,"grid_color values need to between 0 and 1");
		CHECKS(flt3>=0 && flt3<=1,"grid_color values need to between 0 and 1");
		graphss->gridcolor[0]=flt1;
		graphss->gridcolor[1]=flt2;
		graphss->gridcolor[2]=flt3;
		if((line2=strnword(line2,4))!=NULL) {
			itct=sscanf(line2,"%lg",&flt1);
			CHECKS(itct==1,"grid_color format: red green blue [alpha]");
			CHECKS(flt1>=0 && flt1<=1,"grid_color alpha value needs to be between 0 and 1");
			graphss->gridcolor[3]=flt1;
			line2=strnword(line2,2); }
		CHECKS(!line2,"unexpected text following grid_color"); }

	else if(!strcmp(word,"background_color")) {		// background_color
		CHECKS(sim->graphss,"graphics needs to be entered before background_color");
		graphss=sim->graphss;
		itct=sscanf(line2,"%lg %lg %lg",&flt1,&flt2,&flt3);
		CHECKS(itct==3,"background_color format: red green blue [alpha]");
		CHECKS(flt1>=0 && flt1<=1,"background_color values need to between 0 and 1");
		CHECKS(flt2>=0 && flt2<=1,"background_color values need to between 0 and 1");
		CHECKS(flt3>=0 && flt3<=1,"background_color values need to between 0 and 1");
		graphss->backcolor[0]=flt1;
		graphss->backcolor[1]=flt2;
		graphss->backcolor[2]=flt3;
		if((line2=strnword(line2,4))!=NULL) {
			itct=sscanf(line2,"%lg",&flt1);
			CHECKS(itct==1,"background_color format: red green blue [alpha]");
			CHECKS(flt1>=0 && flt1<=1,"background_color alpha value needs to be between 0 and 1");
			graphss->backcolor[3]=flt1;
			line2=strnword(line2,2); }
		CHECKS(!line2,"unexpected text following background_color"); }

	else if(!strcmp(word,"light")) {							// light
		CHECKS(sim->graphss,"graphics needs to be entered before light");
		graphss=sim->graphss;
		itct=sscanf(line2,"%s",nm);
		CHECKS(itct==1,"light format: light_number parameter values");
		if(!strcmp(nm,"global") || !strcmp(nm,"room")) lt=-1;
		else {
			itct=sscanf(line2,"%i",&lt);
			CHECKS(lt>=0 && lt<MAXLIGHTS,"light number out of bounds"); }
		line2=strnword(line2,2);
		CHECKS(line2,"light format: light_number parameter values");
		itct=sscanf(line2,"%s",nm);
		CHECKS(itct==1,"unable to read lighting parameter");
		ltparam=graphicsstring2lp(nm);
		CHECKS(ltparam!=LPnone,"unrecognized light parameter");
		line2=strnword(line2,2);
		if(ltparam==LPon || ltparam==LPoff || ltparam==LPauto) v2[0]=0;
		else {
			CHECKS(line2,"light format: light_number parameter values");
			itct=sscanf(line2,"%lg %lg %lg",&v2[0],&v2[1],&v2[2]);
			CHECKS(itct==3,"light is missing one or more values");
			v2[3]=1;
			line2=strnword(line2,4);
			if(ltparam!=LPposition) {
				if(line2) {
					itct=sscanf(line2,"%lg",&v2[3]);
					CHECKS(itct==1,"failed to read alpha light value");
					line2=strnword(line2,2); }
				for(i1=0;i1<4;i1++)
					CHECKS(v2[i1]>=0 && v2[i1]<=1,"light color values need to be between 0 and 1"); }}
		graphicssetlight(graphss,lt,ltparam,v2);
		CHECKS(!line2,"unexpected text following light"); }

	else if(!strcmp(word,"display_size")) {				// display_size
		CHECKS(sim->mols,"need to enter species before display_size");
		i=readmolname(sim,line2,&ms);
		CHECKS(i!=0,"empty molecules not permitted");
		CHECKS(i!=-1,"display_size format: name[(state)] value");
		CHECKS(i!=-2,"mismatched or improper parentheses around molecule state");
		CHECKS(i!=-3,"cannot read molecule state value");
		CHECKS(i!=-4,"molecule name not recognized");
		CHECKS(line2=strnword(line2,2),"missing data for display_size");
		itct=sscanf(line2,"%lg",&flt1);
		CHECKS(itct==1,"display_size format: name [state] size");
		CHECKS(flt1>=0,"display_size value needs to be >=0");
		molsetdisplaysize(sim,i,ms,flt1);
		CHECKS(!strnword(line2,2),"unexpected text following display_size"); }

	else if(!strcmp(word,"color")) {							// color
		CHECKS(sim->mols,"need to enter species before color");
		i=readmolname(sim,line2,&ms);
		CHECKS(i!=0,"empty molecules not permitted");
		CHECKS(i!=-1,"color format: name[(state)] red green blue");
		CHECKS(i!=-2,"mismatched or improper parentheses around molecule state");
		CHECKS(i!=-3,"cannot read molecule state value");
		CHECKS(i!=-4,"molecule name not recognized");
		CHECKS(line2=strnword(line2,2),"missing data in color");
		itct=strreadnd(line2,3,v1,NULL);
		CHECKS(itct==3,"color format: name red green blue");
		CHECKS(v1[0]>=0 && v1[0]<=1,"in color, red color value needs to be between 0 and 1");
		CHECKS(v1[1]>=0 && v1[1]<=1,"in color, green color value needs to be between 0 and 1");
		CHECKS(v1[2]>=0 && v1[2]<=1,"in color, blue color value needs to be between 0 and 1");
		molsetcolor(sim,i,ms,v1);
		CHECKS(!strnword(line2,4),"unexpected text following color"); }

	else if(!strcmp(word,"tiff_iter")) {				// tiff_iter
		CHECKS(sim->graphss,"graphics needs to be entered before tiff_iter");
		graphss=sim->graphss;
		itct=sscanf(line2,"%i",&i1);
		CHECKS(itct==1,"tiff_iter needs to be a number");
		CHECKS(i1>=1,"tiff_iter has to be at least 1");
		graphss->tiffit=i1;
		CHECKS(!strnword(line2,2),"unexpected text following tiff_iter"); }

	else if(!strcmp(word,"tiff_name")) {					// tiff_name
		itct=sscanf(line2,"%s",nm);
		CHECKS(itct==1,"tiff_name needs to be a string");
		strcpy(nm1,sim->filepath);
		strncat(nm1,nm,STRCHAR-1-strlen(nm1));
		gl2SetOptionStr("TiffName",nm1);
		CHECKS(!strnword(line2,2),"unexpected text following tiff_name"); }

	else if(!strcmp(word,"tiff_min")) {					// tiff_min
		itct=sscanf(line2,"%i",&i1);
		CHECKS(itct==1,"tiff_min needs to be a number");
		CHECKS(i1>=0,"tiff_min has to be at least 0");
		gl2SetOptionInt("TiffNumber",i1);
		CHECKS(!strnword(line2,2),"unexpected text following tiff_min"); }

	else if(!strcmp(word,"tiff_max")) {					// tiff_max
		itct=sscanf(line2,"%i",&i1);
		CHECKS(itct==1,"tiff_max needs to be a number");
		CHECKS(i1>=0,"tiff_max has to be at least 0");
		gl2SetOptionInt("TiffNumMax",i1);
		CHECKS(!strnword(line2,2),"unexpected text following tiff_max"); }

	// about runtime commands

	else if(!strcmp(word,"output_root")) {				// output_root
		er=scmdsetfroot(sim->cmds,line2);
		CHECKS(er!=-1,"SMOLDYN BUG: scmdsetfroot"); }

	else if(!strcmp(word,"output_files")) {				// output_files
		er=scmdsetfnames(sim->cmds,line2);
		CHECKS(er!=1,"out of memory in output_files");
		CHECKS(er!=2,"error reading file name");
		CHECKS(er!=3,"output_files can only be entered once");
		CHECKS(er!=4,"BUG: variable cmds became NULL"); }

	else if(!strcmp(word,"output_file_number")) {	// output_file_number
		itct=sscanf(line2,"%s %i",nm,&i1);
		CHECKS(itct==2,"format for output_file_number: filename number");
		CHECKS(i1>=0,"output_file_number needs to be ³0");
		er=scmdsetfsuffix(sim->cmds,nm,i1);
		CHECKS(!er,"error setting output_file_number");
		CHECKS(!strnword(line2,3),"unexpected text following output_file_number"); }

	else if(!strcmp(word,"cmd")) {								// cmd
		er=scmdstr2cmd(sim->cmds,line2,sim->tmin,sim->tmax,sim->dt);
		CHECKS(er!=1,"out of memory in cmd");
		CHECKS(er!=2,"BUG: no command superstructure for cmd");
		CHECKS(er!=3,"cmd format: type [on off dt] string");
		CHECKS(er!=4,"command string is missing");
		CHECKS(er!=5,"cmd time step needs to be >0");
		CHECKS(er!=6,"command timing type character not recognized");
		CHECKS(er!=7,"insertion of command in queue failed");
		CHECKS(er!=8,"cmd time multiplier needs to be >1"); }

	else if(!strcmp(word,"max_cmd")) {						// max_cmd
		itct=sscanf(line2,"%i",&i1);
		CHECKS(itct==1,"max_cmd needs to be an integer");
		CHECKS(i1>=0,"max_cmd needs to be >=0");
		er=scmdqalloc(sim->cmds,i1);
		CHECKS(er!=1,"insufficient memory");
		CHECKS(er!=2,"SMOLDYN BUG: scmdqalloc");
		CHECKS(er!=3,"max_cmd can only be called once");
		CHECKS(!strnword(line2,2),"unexpected text following max_cmd"); }

	// surfaces

	else if(!strcmp(word,"max_surface")) {				// max_surface
		CHECKS(sim->mols,"need to enter max_species before max_surface");
		CHECKS(dim>0,"need to enter dim before max_surface");
		CHECKS(dim<=3,"surfaces cannot be entered for systems with more than 3 dimensions");
		CHECKS(!sim->srfss,"max_surface can only be entered once");
		itct=sscanf(line2,"%i",&i1);
		CHECKS(itct==1,"max_surface needs to be a number");
		CHECKS(i1>=0,"max_surface must be at least 0");
		if(i1) {
			sim->srfss=surfacessalloc(i1,sim->mols->maxspecies,dim);
			CHECKS(sim->srfss,"memory allocation failure at max_surface");
			sim->srfss->sim=sim;
			surfsetcondition(sim->srfss,SCinit,0); }
		CHECKS(!strnword(line2,2),"unexpected text following max_surface"); }

	else if(!strcmp(word,"new_surface")) {				// new_surface
		CHECKS(sim->srfss,"max_surface has to be entered before new_surface");
		er=surfreadstring(sim,-1,"name",line2,erstr);
		CHECKS(er>=0,erstr); }

	else if(!strcmp(word,"surface")) {						// surface
		CHECKS(sim->srfss,"max_surface has to be entered before surface");
		itct=sscanf(line2,"%s %s",nm,nm1);
		CHECKS(itct==2,"surface format: surface_name statement_name statement_text");
		line2=strnword(line2,3);
		CHECKS(line2,"surface format: surface_name statement_name statement_text");
		s=stringfind(sim->srfss->snames,sim->srfss->nsrf,nm);
		CHECKS(s>=0,"surface is unrecognized");
		er=surfreadstring(sim,s,nm1,line2,erstr);
		CHECKS(er>=0,erstr); }

	// compartments

	else if(!strcmp(word,"max_compartment")) {		// max_compartment
		CHECKS(!sim->cmptss,"max_compartment can only be entered once");
		itct=sscanf(line2,"%i",&i1);
		CHECKS(itct==1,"max_compartment needs to be a number");
		CHECKS(i1>=0,"max_compartment must be at least 0");
		if(i1) {
			sim->cmptss=compartssalloc(i1);
			CHECKS(sim->cmptss,"memory allocation failure");
			sim->cmptss->sim=sim;
			compartsetcondition(sim->cmptss,SCinit,0); }
		CHECKS(!strnword(line2,2),"unexpected text following max_compartment"); }

	else if(!strcmp(word,"new_compartment")) {		// new_compartment
		CHECKS(sim->cmptss,"max_compartment has to be entered before new_compartment");
		er=cmptreadstring(sim,-1,"name",line2,erstr);
		CHECKS(er>=0,erstr); }

	else if(!strcmp(word,"compartment")) {				// compartment
		CHECKS(sim->cmptss,"max_compartment has to be entered before compartment");
		itct=sscanf(line2,"%s %s",nm,nm1);
		CHECKS(itct==2,"compartment format: compart_name compart_name compart_text");
		line2=strnword(line2,3);
		CHECKS(line2,"compartment format: compart_name compart_name compart_text");
		c=stringfind(sim->cmptss->cnames,sim->cmptss->ncmpt,nm);
		CHECKS(c>=0,"compartment is unrecognized");
		er=cmptreadstring(sim,c,nm1,line2,erstr);
		CHECKS(er>=0,erstr); }

	// ports

	else if(!strcmp(word,"max_port")) {						// max_port
		CHECKS(!sim->portss,"max_port can only be entered once");
		itct=sscanf(line2,"%i",&i1);
		CHECKS(itct==1,"max_port needs to be a number");
		CHECKS(i1>=0,"max_port must be at least 0");
		if(i1) {
			sim->portss=portssalloc(i1);
			CHECKS(sim->portss,"memory allocation failure");
			sim->portss->sim=sim;
			portsetcondition(sim->portss,SCinit,0); }
		CHECKS(!strnword(line2,2),"unexpected text following max_port"); }

	else if(!strcmp(word,"new_port")) {					// new_port
		CHECKS(sim->portss,"max_port has to be entered before new_port");
		er=portreadstring(sim,-1,"name",line2,erstr);
		CHECKS(er>=0,erstr); }

	else if(!strcmp(word,"port")) {							// port
		CHECKS(sim->portss,"max_port has to be entered before port");
		itct=sscanf(line2,"%s %s",nm,nm1);
		CHECKS(itct==2,"port format: port_name port_name port_text");
		line2=strnword(line2,3);
		CHECKS(line2,"port format: port_name port_name port_text");
		prt=stringfind(sim->portss->portnames,sim->portss->nport,nm);
		CHECKS(prt>=0,"port is unrecognized");
		er=portreadstring(sim,prt,nm1,line2,erstr);
		CHECKS(er>=0,erstr); }

	// moleculizer

	else if(!strcmp(word,"reference_difc")) {							// reference_difc
		CHECKS(sim->mzrss,"need to enter start_rules before reference_difc");
		CHECKS(sim->mols,"need to enter species before reference_difc");
		itct=sscanf(line2,"%s",nm);
		CHECKS(itct==1,"reference_difc needs to be a string");
		i=stringfind(sim->mols->spname,sim->mols->nspecies,nm);
		CHECKS(i>0,"species name not recognized");
		mzrSetValue(sim->mzrss,"refspecies",i);
		CHECKS(!strnword(line2,2),"unexpected text following reference_difc"); }

	else if(!strcmp(word,"max_network_species")) {						// max_network_species
		CHECKS(sim->mzrss,"need to enter start_rules before max_network_species");
		itct=sscanf(line2,"%i",&i1);
		CHECKS(itct==1,"max_network_species needs to be an integer");
		CHECKS(i1>0,"max_network_species needs to be >0");
		mzrSetValue(sim->mzrss,"maxNetworkSpecies",i1);
		CHECKS(!strnword(line2,2),"unexpected text following max_network_species"); }

	else if(!strcmp(word,"default_state")) {									// default_state
		CHECKS(sim->mzrss,"need to enter start_rules before default_state");
		CHECKS(sim->mols,"need to enter max_species before default_state");
		itct=sscanf(line2,"%s",nm);
		i=readmolname(sim,nm,&ms);
		CHECKS(i!=0,"empty molecules not permitted");
		CHECKS(i!=-1,"default_state format: name[(state)]");
		CHECKS(i!=-2,"mismatched or improper parentheses around molecule state");
		CHECKS(i!=-3,"cannot read molecule state value");
		CHECKS(i!=-4,"molecule name not recognized");
		CHECKS(i!=-5,"all is not a permitted state");
		CHECKS(mzrSetDefaultState(sim,i,ms),"out of memory setting default state");
		CHECKS(!strnword(line2,2),"unexpected text following default_state"); }

	else if(!strcmp(word,"species_class_display_size")) {				// species_class_display_size
		CHECKS(sim->mzrss,"need to enter start_rules before species_class_display_size");
		CHECKS(sim->mols,"need to enter max_species before species_class_display_size");
		itct=sscanf(line2,"%s %lg",nm,&flt1);
		CHECKS(itct==2,"format: name[(state)] value");
		i=mzrReadStreamName(nm,nm1,&ms);
		CHECKS(i!=-1,"format: name[(state)] value");
		CHECKS(i!=-2,"mismatched or improper parentheses around molecule state");
		CHECKS(i!=-3,"cannot read molecule state");
		CHECKS(i!=-5,"'all' is not permitted");
		CHECKS(flt1>=0,"display_size value needs to be >=0");
		mzrSetStreamDisplay(sim->mzrss,nm1,ms,flt1,NULL);
		CHECKS(!strnword(line2,3),"unexpected text following species_class_display_size"); }

	else if(!strcmp(word,"species_class_color")) {						// species_class_color
		CHECKS(sim->mzrss,"need to enter start_rules before species_class_color");
		CHECKS(sim->mols,"need to enter max_species before species_class_color");
		itct=sscanf(line2,"%s %lg %lg %lg",nm,&v1[0],&v1[1],&v1[2]);
		CHECKS(itct==4,"format: name[(state)] red green blue");
		i=mzrReadStreamName(nm,nm1,&ms);
		CHECKS(i!=-1,"format: name[(state)] value");
		CHECKS(i!=-2,"mismatched or improper parentheses around molecule state");
		CHECKS(i!=-3,"cannot read molecule state");
		CHECKS(i!=-5,"'all' is not permitted");
		CHECKS(v1[0]>=0 && v1[0]<=1,"red color value needs to be between 0 and 1");
		CHECKS(v1[1]>=0 && v1[1]<=1,"green color value needs to be between 0 and 1");
		CHECKS(v1[2]>=0 && v1[2]<=1,"blue color value needs to be between 0 and 1");
		mzrSetStreamDisplay(sim->mzrss,nm1,ms,-1,v1);
		CHECKS(!strnword(line2,5),"unexpected text following display_size"); }

	else if(!strcmp(word,"species_class_difc")) {								// species_class_difc
		CHECKS(sim->mzrss,"need to enter start_rules before species_class_difc");
		CHECKS(sim->mols,"need to enter max_species before species_class_difc");
		itct=sscanf(line2,"%s %lg",nm,&flt1);
		CHECKS(itct==2,"format: name[(state)] value");
		i=mzrReadStreamName(nm,nm1,&ms);
		CHECKS(i!=-1,"format: name[(state)] value");
		CHECKS(i!=-2,"mismatched or improper parentheses around molecule state");
		CHECKS(i!=-3,"cannot read molecule state");
		CHECKS(i!=-5,"'all' is not permitted");
		CHECKS(flt1>=0,"difc value needs to be >=0");
		mzrSetStreamDifc(sim->mzrss,nm1,ms,flt1);
		CHECKS(!strnword(line2,3),"unexpected text following species_class_difc"); }

	// reactions

	else if(!strcmp(word,"reaction") || !strcmp(word,"reaction_cmpt") || !strcmp(word,"reaction_surface")) {	// reaction, reaction_cmpt, reaction_surface
		CHECKS(sim->mols,"need to enter species before reaction");
		cmpt=NULL;
		srf=NULL;
		if(!strcmp(word,"reaction_cmpt")) {
			itct=sscanf(line2,"%s",nm);
			CHECKS(itct==1,"failed to read reaction compartment");
			CHECKS(sim->cmptss,"no compartments defined");
			c=stringfind(sim->cmptss->cnames,sim->cmptss->ncmpt,nm);
			CHECKS(c>=0,"compartment name not recognized");
			cmpt=sim->cmptss->cmptlist[c];
			line2=strnword(line2,2);
			CHECKS(line2,"missing reaction name"); }
		else if(!strcmp(word,"reaction_surface")) {
			itct=sscanf(line2,"%s",nm);
			CHECKS(itct==1,"failed to read reaction surface");
			CHECKS(sim->srfss,"no surfaces defined");
			s=stringfind(sim->srfss->snames,sim->srfss->nsrf,nm);
			CHECKS(s>=0,"surface name not recognized");
			srf=sim->srfss->srflist[s];
			line2=strnword(line2,2);
			CHECKS(line2,"missing reaction name"); }
		itct=sscanf(line2,"%s",rname);
		CHECKS(itct==1,"failed to read reaction name");
		for(order=0;order<MAXORDER;order++)
			if(sim->rxnss[order]) {
				CHECKS(stringfind(sim->rxnss[order]->rname,sim->rxnss[order]->totrxn,rname)<0,"reaction name has already been used"); }
		CHECKS(line2=strnword(line2,2),"missing first reactant");
		order=0;
		more=1;
		while(more) {			// reactant list
			CHECKS((itct=sscanf(line2,"%s %s",nm,nm1))==2,"failed to read reactant");
			CHECKS(strcmp(nm,"->"),"missing reaction name");
			CHECKS(strcmp(nm,"+"),"missing reaction name");
			if(strcmp(nm,"0")) {
				CHECKS(order+1<MAXORDER,"exceeded allowed reaction order");
				i=readmolname(sim,nm,&ms);
				CHECKS(i!=0,"empty molecules not permitted");
				CHECKS(i!=-1,"failed to read reactant species name");
				CHECKS(i!=-2,"mismatched or improper parentheses around reactant state");
				CHECKS(i!=-3,"cannot read reactant state");
				CHECKS(i!=-4,"reactant name not recognized");
				CHECKS(i!=-5,"'all' is not allowed as a reactant name");
				rctident[order]=i;
				rctstate[order]=ms;
				order++; }
			if(!strcmp(nm1,"->")) more=0;
			else CHECKS(!strcmp(nm1,"+"),"illegal symbol in reactant list");
			CHECKS(line2=strnword(line2,3),"missing information in reaction"); }
		nprod=0;
		more=1;
		while(more) {			// product list
			CHECKS((itct=sscanf(line2,"%s %s",nm,nm1))>=1,"failed to read product");
			if(strcmp(nm,"0")) {
				CHECKS(nprod+1<MAXPRODUCT,"exceeded allowed number of reaction products");
				i=readmolname(sim,nm,&ms);
				CHECKS(i!=0,"empty molecules not permitted");
				CHECKS(i!=-1,"failed to read product species name");
				CHECKS(i!=-2,"mismatched or improper parentheses around product state");
				CHECKS(i!=-3,"cannot read product state");
				CHECKS(i!=-4,"product name not recognized");
				CHECKS(i!=-5,"'all' is not allowed as a product name");
				CHECKS(ms!=MSall,"'all' is not allowed as a product state");
				prdident[nprod]=i;
				prdstate[nprod]=ms;
				nprod++; }
			if(itct==1) {
				more=0;
				line2=NULL; }
			else if(strcmp(nm1,"+")) {
				more=0;
				CHECKS(line2=strnword(line2,2),"SMOLDYN BUG: error in reading product list"); }
			else
				CHECKS(line2=strnword(line2,3),"incomplete product list"); }
		rxn=RxnAddReaction(sim,rname,order,rctident,rctstate,nprod,prdident,prdstate,cmpt,srf);
		CHECKS(rxn,"out of memory trying to create reaction structure");
		if(line2) {
			CHECKS((itct=sscanf(line2,"%lg",&flt1))==1,"failed to read reaction rate");
			er=RxnSetValue(sim,"rate",rxn,flt1);
			CHECKS(er!=4,"reaction rate value must be non-negative");
			line2=strnword(line2,2); }
		CHECKS(!line2,"unexpected text following reaction"); }

	else if(!strcmp(word,"reaction_rate")) {				// reaction_rate
		itct=sscanf(line2,"%s %lg",rname,&flt1);
		CHECKS(itct==2,"reaction_rate format: rname rate");
		r=readrxnname(sim,rname,&order,&rxn);
		CHECKS(r>=0,"unrecognized reaction name");
		er=RxnSetValue(sim,"rate",rxn,flt1);
		CHECKS(er!=4,"reaction rate value must be non-negative");
		CHECKS(!strnword(line2,3),"unexpected text following reaction"); }

	else if(!strcmp(word,"confspread_radius")) {		// confspread_radius
		itct=sscanf(line2,"%s %lg",rname,&flt1);
		CHECKS(itct==2,"confspread_radius format: rname radius");
		r=readrxnname(sim,rname,&order,&rxn);
		CHECKS(r>=0,"unrecognized reaction name");
		CHECKS(order==2,"conformational spread is only allowed for second order reactions");
		er=RxnSetValue(sim,"confspreadrad",rxn,flt1);
		CHECKS(er!=4,"confspread radius value must be non-negative");
		CHECKS(!strnword(line2,3),"unexpected text following confspread_radius"); }

	else if(!strcmp(word,"binding_radius")) {		// binding_radius
		itct=sscanf(line2,"%s %lg",rname,&flt1);
		CHECKS(itct==2,"binding_radius format: rname radius");
		r=readrxnname(sim,rname,&order,&rxn);
		CHECKS(r>=0,"unrecognized reaction name");
		CHECKS(order==2,"binding radii are only allowed for second order reactions");
		er=RxnSetValue(sim,"bindrad",rxn,flt1);
		CHECKS(er!=4,"binding radius value must be non-negative");
		CHECKS(!strnword(line2,3),"unexpected text following binding_radius"); }

	else if(!strcmp(word,"reaction_probability")) {		// reaction_probability
		itct=sscanf(line2,"%s %lg",rname,&flt1);
		CHECKS(itct==2,"reaction_probability format: rname value");
		r=readrxnname(sim,rname,&order,&rxn);
		CHECKS(r>=0,"unrecognized reaction name");
		CHECKS(order>0,"probability is not allowed for order 0 reactions");
		er=RxnSetValue(sim,"prob",rxn,flt1);
		CHECKS(er!=4,"probability value must be between 0 and 1");
		CHECKS(!strnword(line2,3),"unexpected text following reaction_probability"); }

	else if(!strcmp(word,"reaction_production")) {		// reaction_production
		itct=sscanf(line2,"%s %lg",rname,&flt1);
		CHECKS(itct==2,"reaction_production format: rname value");
		r=readrxnname(sim,rname,&order,&rxn);
		CHECKS(r>=0,"unrecognized reaction name");
		CHECKS(order==0,"production is only allowed for order 0 reactions");
		er=RxnSetValue(sim,"prob",rxn,flt1);
		CHECKS(er!=4,"production value must be between 0 and 1");
		CHECKS(!strnword(line2,3),"unexpected text following reaction_production"); }

	else if(!strcmp(word,"reaction_permit")) {			// reaction_permit
		itct=sscanf(line2,"%s",rname);
		CHECKS(itct==1,"missing reaction name");
		r=readrxnname(sim,rname,&order,&rxn);
		CHECKS(r>=0,"unrecognized reaction name");
		CHECKS(order>0,"reaction_permit is not allowed for order 0 reactions");
		for(ord=0;ord<order;ord++) {
			CHECKS(line2=strnword(line2,2),"missing state term in reaction_permit");
			itct=sscanf(line2,"%s",nm1);
			rctstate[ord]=molstring2ms(nm1); }
		RxnSetPermit(sim,rxn,order,rctstate,1);
		CHECKS(!strnword(line2,3),"unexpected text following reaction_permit"); }

	else if(!strcmp(word,"reaction_forbid")) {			// reaction_forbid
		itct=sscanf(line2,"%s",rname);
		CHECKS(itct==1,"missing reaction name");
		r=readrxnname(sim,rname,&order,&rxn);
		CHECKS(r>=0,"unrecognized reaction name");
		CHECKS(order>0,"reaction_forbid is not allowed for order 0 reactions");
		for(ord=0;ord<order;ord++) {
			CHECKS(line2=strnword(line2,2),"missing state term in reaction_forbid");
			itct=sscanf(line2,"%s",nm1);
			rctstate[ord]=molstring2ms(nm1); }
		RxnSetPermit(sim,rxn,order,rctstate,0);
		CHECKS(!strnword(line2,3),"unexpected text following reaction_forbid"); }

	else if(!strcmp(word,"product_placement")) {				// product_placement
		itct=sscanf(line2,"%s %s",rname,nm1);
		CHECKS(itct==2,"product_placement format: rname type parameters");
		r=readrxnname(sim,rname,&order,&rxn);
		CHECKS(r>=0,"unrecognized reaction name");
		CHECKS(rxn->rparamt==RPnone,"product_placement can only be entered once");
		rpart=rxnstring2rp(nm1);
		CHECKS(!(rpart==RPbounce && (order!=2 || rxn->nprod!=2)),"bounce can only be used with two reactants and two products");
		flt1=0;
		prd=0;
		for(d=0;d<sim->dim;d++) v1[prd]=0;
		line2=strnword(line2,3);
		if(rpart==RPirrev);
		else if(rpart==RPpgem || rpart==RPbounce || rpart==RPpgemmax || rpart==RPpgemmaxw || rpart==RPratio || rpart==RPunbindrad || rpart==RPpgem2 || rpart==RPpgemmax2 || rpart==RPratio2) {
			CHECKS(line2,"missing parameter in product_placement");
			itct=sscanf(line2,"%lg",&flt1);
			CHECKS(itct==1,"error reading parameter in product_placement");
			line2=strnword(line2,2); }
		else if(rpart==RPoffset || rpart==RPfixed) {
			CHECKS(line2,"missing parameters in product_placement");
			itct=sscanf(line2,"%s",nm1);
			CHECKS(itct==1,"format for product_param: rname type parameters");
			CHECKS((i=stringfind(sim->mols->spname,sim->mols->nspecies,nm1))>=0,"unknown molecule in product_placement");
			for(prd=0;prd<rxn->nprod && rxn->prdident[prd]!=i;prd++);
			CHECKS(prd<rxn->nprod,"molecule in product_placement is not a product of this reaction");
			CHECKS(line2=strnword(line2,2),"position vector missing for product_placement");
			itct=strreadnd(line2,dim,v1,NULL);
			CHECKS(itct==dim,"insufficient data for position vector for product_placement");
			line2=strnword(line2,dim+1); }
		else CHECKS(0,"unrecognized or not permitted product placement parameter");
		i1=RxnSetRevparam(sim,rxn,rpart,flt1,prd,v1,dim);
		CHECKS(i1!=2,"reversible parameter value is out of bounds");
		CHECKS(!line2,"unexpected text following product_placement"); }

	// optimizing runtime

	else if(!strcmp(word,"rand_seed")) {					// rand_seed
		itct=sscanf(line2,"%li",&li1);
		CHECKS(itct==1,"rand_seed needs to be an integer");
		Simsetrandseed(sim,li1);
		CHECKS(!strnword(line2,2),"unexpected text following rand_seed"); }

	else if(!strcmp(word,"accuracy")) {						// accuracy
		itct=sscanf(line2,"%lg",&flt1);
		CHECKS(itct==1,"accuracy needs to be a number");
		sim->accur=flt1;
		CHECKS(!strnword(line2,2),"unexpected text following accuracy"); }

	else if(!strcmp(word,"molperbox")) {					// molperbox
		itct=sscanf(line2,"%lg",&flt1);
		CHECKS(itct==1,"molperbox needs to be a number");
		er=boxsetsize(sim,"molperbox",flt1);
		CHECKS(er!=1,"out of memory");
		CHECKS(er!=2,"molperbox needs to be >0");
		CHECKS(er!=3,"need to enter dim before molperbox");
		CHECKS(!strnword(line2,2),"unexpected text following molperbox"); }

	else if(!strcmp(word,"boxsize")) {						// boxsize, got[5]
		itct=sscanf(line2,"%lg",&flt1);
		CHECKS(itct==1,"boxsize needs to be a number");
		er=boxsetsize(sim,"boxsize",flt1);
		CHECKS(er!=1,"out of memory");
		CHECKS(er!=2,"boxsize needs to be >0");
		CHECKS(er!=3,"need to enter dim before boxsize");
		CHECKS(!strnword(line2,2),"unexpected text following boxsize"); }

	else if(!strcmp(word,"gauss_table_size")) {		// gauss_table_size
		itct=sscanf(line2,"%i",&i1);
		CHECKS(itct==1,"gauss_table_size needs to be an integer");
		er=molssetgausstable(sim->mols,i1);
		CHECKS(er!=1,"out of memory");
		CHECKS(er!=2,"need to enter max_species before gauss_table_size");
		CHECKS(er!=3,"gauss_table_size needs to be an integer power of two");
		CHECKS(!strnword(line2,2),"unexpected text following gauss_table_size"); }

	else if(!strcmp(word,"epsilon")) {						// epsilon
		itct=sscanf(line2,"%lg",&flt1);
		CHECKS(itct==1,"epsilon format: value");
		er=surfsetepsilon(sim,flt1);
		CHECKS(er!=2,"need to enter max_surface before epsilon");
		CHECKS(er!=3,"epsilon value needs to be at least 0");
		CHECKS(!strnword(line2,2),"unexpected text following epsilon"); }

	else if(!strcmp(word,"neighbor_dist")) {			// neighbor_dist
		itct=sscanf(line2,"%lg",&flt1);
		CHECKS(itct==1,"neighbor_dist format: value");
		er=surfsetneighdist(sim,flt1);
		CHECKS(er!=2,"need to enter max_surface before neigh_dist");
		CHECKS(er!=3,"neighdist value needs to be at least 0");
		CHECKS(!strnword(line2,2),"unexpected text following neighbor_dist"); }

	else {																				// unknown word
		CHECKS(0,"syntax error: statement not recognized"); }

	return 0;

 failure:
	return 1; }

#include <VCELL/SimulationMessaging.h>
extern int taskID;
int loadJMS(simptr sim,ParseFilePtr *pfpptr,char *line2,char *erstr) {

	char word[STRCHAR];
	ParseFilePtr pfp = *pfpptr;
	int done = 0, pfpcode;
	bool firstline2 = (line2 != NULL);
	while(!done) {
		if (firstline2) {
			strcpy(word,"name");
			pfpcode=1;
			firstline2 = false;
		} else {
			pfpcode=Parse_ReadLine(&pfp,word,&line2,erstr);
		}
		*pfpptr=pfp;
		CHECKS(pfpcode!=3,erstr);

		if(pfpcode==0);	// already taken care of

		else if(pfpcode==2) { // end reading
			done = 1;
		} else if(pfpcode==3) {	// error
			CHECKS(0,"SMOLDYN BUG: parsing error");
		} else if(!strcmp(word,"end_jms")) {  // end_jms
			CHECKS(!line2,"unexpected text following end_jms");
			break;
		} else if(!line2) {															// just word
			CHECKS(0,"missing jms parameters");
		} else {
#ifdef USE_MESSAGING
			if (taskID >= 0) {
				printf("-------taskID=%d\n", taskID);
				char *jmsBroker = new char[64];
				char *jmsUser = new char[64];
				char* jmsPwd = new char[64];
				char* jmsQueue = new char[64];
				char* jmsTopic = new char[64];
				char* vcellUser = new char[64];
				int simKey, jobIndex;
				sscanf(line2, "%s%s%s%s%s%s%d%d", jmsBroker, jmsUser, jmsPwd, jmsQueue, jmsTopic, vcellUser, &simKey, &jobIndex);
				SimulationMessaging::create(jmsBroker, jmsUser, jmsPwd, jmsQueue, jmsTopic, vcellUser, simKey, jobIndex, taskID);
				SimulationMessaging::getInstVar()->start(); // start the thread
			} else {
				SimulationMessaging::create();
			}
#else
			SimulationMessaging::create();
#endif
		}
	}
	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_STARTING, "setting up simulation"));
	return 0;

failure:		// failure
	return 1;
}

/* loadsim loads all simulation parameters from a configuration file, using a
format described above.  fileroot is sent in as the root of the filename,
including all colons, slashes, or backslashes; if the configuration file is in
the same directory as Smoldyn, fileroot should be an empty string.  filename is
sent in as just the file name and any extension.  erstr is sent in as an empty
string of size STRCHAR and is returned with an error message if an error occurs.
smptr is sent in as a pointer to the variable that will point to the simstruct;
it is returned pointing to a pointer to an initiallized simstruct.  This
routine calls loadrxn to load in any reactions.  The following things are set up
after this routine is completed: all molecule elements except box; all molecule
superstructure elements; all wall elements; box superstructure element mpbox,
but no other elements; no boxes are allocated or set up; all reaction structure
elements except rate2 and the product template position vectors (pos in each
product); the command superstructure, including all of its elements; and all
simulation structure elements except for sub-elements that have already been
listed.  If the configuration file loads successfully, the routine returns 0.
If the file could not be found, it returns 10 and an error message.  If an error
was caught during file loading, the return value is 10 plus the line number of
the file with an error, along with an error message.  If there is an error, all
structures are freed automatically. */
int loadsim(simptr sim,char *fileroot,char *filename,char *erstr,char *flags) {
	int done,pfpcode,er;
	char word[STRCHAR],*line2;
	ParseFilePtr pfp;

	if(!sim || !fileroot || !filename || !erstr || !flags) return 0;
	strncpy(sim->filepath,fileroot,STRCHAR);
	strncpy(sim->filename,filename,STRCHAR);
	strncpy(sim->flags,flags,STRCHAR);
	done=0;
	pfp=Parse_Start(fileroot,filename,erstr);
	CHECKS(pfp,erstr);

	while(!done) {
		if(pfp->lctr==0 && !strchr(flags,'q'))
			printf(" Reading file: '%s'\n",pfp->fname);
		pfpcode=Parse_ReadLine(&pfp,word,&line2,erstr);
		CHECKS(pfpcode!=3,erstr);

		if(pfpcode==0);																// already taken care of

		else if(pfpcode==2) {													// end reading
			done=1; }

		else if(pfpcode==3) {													// error
			CHECKS(0,"SMOLDYN BUG: parsing error"); }

		else if(!strcmp(word,"start_jms")) {			// jms settings
			CHECKS(!loadJMS(sim,&pfp,line2,erstr),erstr); }

		else if(!strcmp(word,"start_reaction")) {			// start_reaction
			CHECKS(sim->mols,"need to enter max_species before reactions");
			CHECKS(!loadrxn(sim,&pfp,line2,erstr),erstr); }

		else if(!strcmp(word,"start_surface")) {			// start_surface
			CHECKS(sim->srfss,"need to enter max_surface before start_surface");
			CHECKS(!loadsurface(sim,&pfp,line2,erstr),erstr); }

		else if(!strcmp(word,"start_compartment")) {	// start_compartment
			CHECKS(sim->cmptss,"need to enter max_compartment before start_compartment");
			CHECKS(!loadcompart(sim,&pfp,line2,erstr),erstr); }

		else if(!strcmp(word,"start_port")) {					// start_port
			CHECKS(sim->portss,"need to enter max_port before start_port");
			CHECKS(!loadport(sim,&pfp,line2,erstr),erstr); }

		else if(!strcmp(word,"start_rules")) {				// start_rules
			CHECKS(!mzrssreadrules(sim,&pfp,erstr),erstr); }

		// Threading ??????????? more new code
		else if(!strcmp(word,"enable_threading")) {

		    int ans = 0;
		    printf("In enable...\n");
		    ans = enablesimthreading( sim );
		    printf("In enable...\n");
				CHECKS(ans!=2,"some error occured in enable_threading");
		    
		    if(line2)
		    {
			int itct;
			int i1;
			itct=sscanf(line2,"%i",&i1);
			CHECKS( itct == 1, "The optional paramater must be an integer.");
			CHECKS( i1 >= 0, "Number of threads must be >= 0.");
			// CHECKS( !line2, "Apparent extra parameters in enable_my_threading.");

			if(i1 == 1 || i1 == 0) {
			    disablesimthreading(sim);
			}
			else
			{
			    // Read off the value
			    setnumberofthreads( sim, i1);
			}
		    }
		}

		else if (!strcmp(word, "disable_threading")) {
		    CHECKS( !line2, "Extra parameters given to disable_my_threading.");
		    disablesimthreading(sim);
		}

//????????????????? end of new code

		else if(!line2) {															// just word
			CHECKS(0,"unknown word or missing parameter"); }

		else {
			er=simreadstring(sim,word,line2,erstr);
			CHECKS(!er,erstr); }}

	return 0;

 failure:																					// failure
	if(!done)
		er=10+Parse_ReadFailure(pfp,erstr);
	else er=10;
	return er; }


/* simupdate.  Updates all parts of the simulation structure.  This is called on
start up by setupsim, and may be called at anytime afterwards.  It returns 0 for
success or 1 for failure.  In the latter case, a string that describes the error
should be returned in erstr. */
int simupdate(simptr sim,char *erstr) {
	int er,qflag;
	static int recurs=0;

	if(sim->condition==SCok) {
		return 0; }
	if(recurs>10) {
		recurs=0;
		return 2; }
	recurs++;

	qflag=strchr(sim->flags,'q')?1:0;

	if(sim->condition==SCinit && !qflag && sim->mols) printf(" setting up molecules\n");
	er=setupmols(sim);
	CHECKS(er!=1,"out of memory setting up molecules");

	if(sim->condition==SCinit && !qflag) printf(" setting up virtual boxes\n");
	er=setupboxes(sim);
	CHECKS(er!=1,"out of memory setting up spatial partitions");
	CHECKS(er!=2,"simulation boundaries are undefined");

	er=molsort(sim);
	CHECKS(er!=1,"out of memory during molecule sorting\n");

	if(sim->condition==SCinit && !qflag && sim->cmptss) printf(" setting up compartments\n");
	er=setupcomparts(sim);
	CHECKS(er!=1,"out of memory setting up compartments");

	if(sim->condition==SCinit && !qflag && (sim->rxnss[0] || sim->rxnss[1] || sim->rxnss[2])) printf(" setting up reactions\n");
	er=setuprxns(sim);
	CHECKS(er!=1,"out of memory setting up reactions");
	CHECKS(er!=2,"SMOLDYN BUG: setuprxns");
	CHECKS(er!=4,"failed to set up reactions");

	if(sim->condition==SCinit && !qflag && sim->srfss) printf(" setting up surfaces\n");
	er=setupsurfaces(sim);
	CHECKS(er!=1,"out of memory setting up surfaces");

	if(sim->condition==SCinit && !qflag && sim->portss) printf(" setting up ports\n");
	er=setupports(sim);
	CHECKS(er!=1,"out of memory setting up ports");

	if(sim->condition==SCinit && !qflag && sim->mzrss) printf(" setting up moleculizer\n");
	CHECKS(!mzrsetupmoleculizer(sim,erstr),erstr);

	if(sim->mols && sim->mols->condition!=SCok) simupdate(sim,erstr);
	if(sim->boxs && sim->boxs->condition!=SCok) simupdate(sim,erstr);
	if(sim->cmptss && sim->cmptss->condition!=SCok) simupdate(sim,erstr);
	if(sim->rxnss[0] && sim->rxnss[0]->condition!=SCok) simupdate(sim,erstr);
	if(sim->rxnss[1] && sim->rxnss[1]->condition!=SCok) simupdate(sim,erstr);
	if(sim->rxnss[2] && sim->rxnss[2]->condition!=SCok) simupdate(sim,erstr);
	if(sim->srfss && sim->srfss->condition!=SCok) simupdate(sim,erstr);
	if(sim->portss && sim->portss->condition!=SCok) simupdate(sim,erstr);
	if(sim->mzrss && sim->mzrss->condition!=SCok) simupdate(sim,erstr);

	simsetcondition(sim,SCok,1);
	recurs=0;

	return 0;

 failure:
	return 1; }



/* setupsim sets up and loads values for all the structures as well as global
variables.  This routine calls the other initialization routines, so they do not
have to be called from elsewhere.  It also displays the status to stdout and
calls output routines for each structure, allowing verification of the
initiallization.  Normally, send in fileroot and filename with strings for the path and
name of the input file and send in smptr (pointer to a simulation structure)
pointing to a NULL.  flags is a string of command-line flags.  This returns 0
for correct operation and 1 for an error.  If it succeeds, smptr is returned
pointing to a fully set up simulation structure.  Otherwise, smptr is set to
NULL and an error messages is displayed on stderr.  In the alternate use, send
in fileroot and name as NULL and send in smptr pointing to a partially set up
simulation structure; it should be set up to the same extent that it is after it
is returned from loadsim.  With this alternate input, this function will finish
setting up the simulation structure. */
int setupsim(char *fileroot,char *filename,simptr *smptr,char *flags) {
	simptr sim;
	int er,order,qflag,wflag,vflag;
	char errorstr[STRCHAR],erstr[STRCHAR];

	sim=*smptr;
	if(!sim) {
		qflag=strchr(flags,'q')?1:0;
		if(!qflag) {
			printf("--------------------------------------------------------------\n");
			printf("Running Smoldyn %s\n",VERSION);
			printf("\nCONFIGURATION FILE\n");
			printf(" Root: '%s'\n",fileroot);
			printf(" Name: '%s'\n",filename); }
		sim=simalloc(fileroot);
		CHECKS(sim,"out of memory");
		er=loadsim(sim,fileroot,filename,errorstr,flags);
		if(er) {
			fprintf(stderr,"\nError reading file in line %i\n",er-10);
			simfree(sim);
			sim=NULL;
			CHECKS(0,errorstr); }
		if(!qflag) printf(" Loaded file successfully\n"); }

	qflag=strchr(sim->flags,'q')?1:0;
	wflag=strchr(sim->flags,'w')?1:0;
	vflag=strchr(sim->flags,'v')?1:0;

	er=simupdate(sim,erstr);
	CHECKS(!er,erstr);

	if(!qflag) printf("\n");
	if(!qflag) simoutput(sim);
	if(!qflag) graphssoutput(sim);
	if(!qflag) walloutput(sim);
	if(!qflag) molssoutput(sim);
	if(!qflag) surfaceoutput(sim);
	if(!qflag) scmdoutput(sim->cmds);
	if(!qflag) boxssoutput(sim);
	if(vflag) boxoutput(sim->boxs,0,20,sim->dim);
	for(order=0;order<MAXORDER;order++)
		if(!qflag) rxnoutput(sim,order);
	if(!qflag) compartoutput(sim);
	if(!qflag) portoutput(sim);
	if(!qflag) mzrssoutput(sim);
	if(!wflag) checksimparams(sim);

	*smptr=sim;
	return 0;

 failure:
	fprintf(stderr,"%s",erstr);
	fprintf(stderr,"\n");
	if(!*smptr) simfree(sim);
	return 1; }



/* enablesimthreading.  In versions of Smoldyn compiled with threading support, this
   enables it.  0 = Sucess, 1 = Unknown Error, 2 = No Support for Threading.  */
int enablesimthreading(simptr sim) {	//????????????? new function
#ifndef THREADING
	return 2;
#else
if (!sim->threads) sim->threads = alloc_threadss( 2 );
	sim->threading = 1;
	sim->thediffusionfunction = diffuse_unitary;
	sim->thechecksurfaceboundfunction = checksurfacebound;
	sim->thechecksurfacecollisionsfunction = checksurfaces_unitary;
	sim->thereassignmolstoboxesfunction = reassignmolecs;
	sim->dozeroaryreactionsfunction = zeroreact;
	sim->dounaryreactionsfunction= unireact_unitary;
	sim->dobimolecularreactionsfunction = bireact_unitary;
	sim->checkwallsfunction = checkwalls_unitary;
	//    sim->thechecksurfacecollisionsfunction = checksurfaces_threaded;
	sim->dobimolecularreactionsfunction = bireact_threaded; 
	/* sim->thediffusionfunction = diffuse_threaded; */
	/*     sim->thechecksurfaceboundfunction = checksurfacebound_threaded; */
	/*     sim->thereassignmolstoboxesfunction = reassignmolecs; */
	/*     sim->dozeroaryreactionsfunction = zeroreact; */
	/*     sim->dounaryreactionsfunction= unireact_threaded; */
	/*     sim->checkwallsfunction = checkwalls_threaded; */
	return 0;
#endif
	}



int disablesimthreading(simptr sim) {
	sim->threading = 0;
	sim->thediffusionfunction = diffuse_unitary;
	sim->thechecksurfaceboundfunction = checksurfacebound;
	sim->thechecksurfacecollisionsfunction = checksurfaces_unitary;
	sim->thereassignmolstoboxesfunction = reassignmolecs;
	sim->dozeroaryreactionsfunction = zeroreact;
	sim->dounaryreactionsfunction= unireact_unitary;
	sim->dobimolecularreactionsfunction = bireact_unitary;
	sim->checkwallsfunction = checkwalls_unitary;
return 0; }
//?????????????? end of new code


/******************************************************************************/
/************************** core simulation functions *************************/
/******************************************************************************/


/* simulatetimestep runs the simulation over one time step.  If an error is
encountered at any step, or a command tells the simulation to stop, or the
simulation time becomes greater than or equal to the requested maximum time, the
function returns an error code to indicate that the simulation should stop;
otherwise it returns 0 to indicate that the simulation should continue.  Error
codes are 1 for simulation completed normally, 2 for error with assignmolecs, 3
for error with zeroreact, 4 for error with unireact, 5 for error with bireact, 6
for error with molsort, or 7 for terminate instruction from docommand (e.g. stop
command).  Errors 2 and 6 arise from insufficient memory when boxes were being
exanded and errors 3, 4, and 5 arise from too few molecules being allocated
initially. */
int simulatetimestep(simptr sim) {			//?????????? this function requires some cleaning up
	int er,ll;
	static int firstrun=1;
	enum CMDcode ccode;
	char erstr[STRCHAR];

	if(firstrun) {
		ccode=scmdexecute(sim->cmds,sim->time,sim->dt,-1,0);
		er=simupdate(sim,erstr);
		if(er) {
			fprintf(stderr,"%s",erstr);
			return 8; }
		er=molsort(sim);														// sort live and dead
		if(er) return 6;
		if(ccode==CMDstop || ccode==CMDabort) return 7;
		else if(ccode==CMDpause) return 0;
		firstrun=0; }

	// diffuse(sim);																// diffuse
	er=(*sim->thediffusionfunction)(sim);
	if(er) return 9;

	if(sim->srfss) {
		for(ll=0;ll<sim->srfss->nmollist;ll++)
			if(sim->srfss->srfmollist[ll] & SMLsrfbound)
				(*sim->thechecksurfaceboundfunction)(sim,ll); }  // surface-bound molecule actions

	if(sim->srfss) {
		for(ll=0;ll<sim->srfss->nmollist;ll++) {
			if(sim->srfss->srfmollist[ll] & SMLdiffuse) {
		    (*sim->thechecksurfacecollisionsfunction)(sim, ll, 0); }}}
	else {
		for(ll=0;ll<sim->mols->nlist;ll++)
			if(sim->mols->diffuselist[ll])					// checkwalls(sim,ll,0,NULL); 
				(*sim->checkwallsfunction)(sim, ll, 0, NULL); }				// walls

	// er=reassignmolecs(sim,1,0);									// assign to boxes (diffusing molecs., not reborn)
	er = (*sim->thereassignmolstoboxesfunction)(sim, 1, 0);
	if(er) return 2;

	er = (*sim->dozeroaryreactionsfunction)(sim);
	if(er) return 3;

	er = (*sim->dounaryreactionsfunction)(sim);
	if(er) return 4;

	er = (*sim->dobimolecularreactionsfunction)(sim, 0);
	if(er) return 5;

	er = (*sim->dobimolecularreactionsfunction)(sim, 1);
	if(er) return 5;

	er=molsort(sim);														// sort live and dead
	if(er) return 6;

	if(sim->srfss) {
		for(ll=0;ll<sim->srfss->nmollist;ll++)
			if(sim->srfss->srfmollist[ll] & SMLreact)
				(*sim->thechecksurfacecollisionsfunction)(sim, ll, 1); }			// surfaces again, reborn molecs. only
	else {
		for(ll=0;ll<sim->mols->nlist;ll++)							// checkwalls(sim,ll,1,NULL); 
		    (*sim->checkwallsfunction)(sim, ll, 1, NULL); }						// walls again

	// er=reassignmolecs(sim,0,1);									// assign again (all, reborn)
	er = (*sim->thereassignmolstoboxesfunction)(sim, 0, 1);
	if(er) return 2;

	sim->time+=sim->dt;													// --- end of time step ---

	ccode=scmdexecute(sim->cmds,sim->time,sim->dt,-1,0);		// commands
	er=simupdate(sim,erstr);
	if(er) {
		fprintf(stderr,"%s",erstr);
		return 8; }
	er=molsort(sim);														// sort live and dead
	if(er) return 6;
	if(ccode==CMDstop || ccode==CMDabort) return 7;
	else if(ccode==CMDpause) return 0;
	if(sim->time>=sim->tmax) return 1;

	return 0; }


/* endsimulate takes care of things that should happen when the simulation is
complete.  This includes executing any commands that are supposed to happen
after the simulation, displaying numbers of simulation events that occurred, and
calculating the execution time.  er is a code to tell why the simulation is
ending, which has the same values as those returned by simulatetimestep.  If
graphics are used, this routine just returns to where it was called from (which
is TimerFunction); otherwise, it frees the simulation structure and then returns
(to smolsimulate and then main). */
void endsimulate(simptr sim,int er) {
	int qflag,tflag,*eventcount;

	gl2State(2);
	qflag=strchr(sim->flags,'q')?1:0;
	tflag=strchr(sim->flags,'t')?1:0;
	scmdpop(sim->cmds,sim->tmax);
	scmdexecute(sim->cmds,sim->time,sim->dt,-1,1);
	if(!qflag) {
		printf("\n");
		if(er==1) printf("Simulation complete\n");
		else if(er==2) printf("Simulation terminated during molecule assignment\n  Out of memory\n");
		else if(er==3) printf("Simulation terminated during order 0 reaction\n  Not enough molecules allocated\n");
		else if(er==4) printf("Simulation terminated during order 1 reaction\n  Not enough molecules allocated\n");
		else if(er==5) printf("Simulation terminated during order 2 reaction\n  Not enough molecules allocated\n");
		else if(er==6) printf("Simulation terminated during molecule sorting\n  Out of memory\n");
		else if(er==7) printf("Simulation stopped by a runtime command\n");
		else if(er==8) printf("Simulation terminated during simulation state updating\n  Out of memory\n");
		else if(er==9) printf("Simulation terminated during diffusion\n  Out of memory\n");
		else printf("Simulation stopped by user\n");
		printf("Current simulation time: %f\n",sim->time);

		eventcount=sim->eventcount;
		if(eventcount[ETwall]) printf("%i wall interactions\n",eventcount[ETwall]);
		if(eventcount[ETsurf]) printf("%i surface interactions\n",eventcount[ETsurf]);
		if(eventcount[ETrxn0]) printf("%i zeroth order reactions\n",eventcount[ETrxn0]);
		if(eventcount[ETrxn1]) printf("%i unimolecular reactions\n",eventcount[ETrxn1]);
		if(eventcount[ETrxn2intra]) printf("%i intrabox bimolecular reactions\n",eventcount[ETrxn2intra]);
		if(eventcount[ETrxn2inter]) printf("%i interbox bimolecular reactions\n",eventcount[ETrxn2inter]);
		if(eventcount[ETrxn2wrap]) printf("%i wrap-around bimolecular reactions\n",eventcount[ETrxn2wrap]);
		if(eventcount[ETimport]) printf("%i imported molecules\n",eventcount[ETimport]);
		if(eventcount[ETexport]) printf("%i exported molecules\n",eventcount[ETexport]);
		if(sim->mzrss) printf("%i species generated\n",mzrNumberOfSpecies(sim->mzrss));
		if(sim->mzrss) printf("%i reactions generated\n",mzrNumberOfReactions(sim->mzrss));

		printf("total execution time: %g seconds\n",sim->elapsedtime); }

	if(sim->graphss && sim->graphss->graphics>0 && !tflag)
		fprintf(stderr,"\nPress 'Q' or command-q to quit.\a\n");
	return; }


/* smolsimulate runs the simulation without graphics.  It does essentially
nothing other than running simulatetimestep until the simulation terminates.  At
the end, it calls endsimulate and returns. */
int smolsimulate(simptr sim) {
	int er,qflag;

	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_STARTING, "starting simulation"));

	er=0;
	//qflag=strchr(sim->flags,'q')?1:0;
	//if(!qflag) printf("Starting simulation\n");
	sim->clockstt=time(NULL);
	while((er=simulatetimestep(sim))==0);
	sim->elapsedtime+=difftime(time(NULL),sim->clockstt);
	return er; }






