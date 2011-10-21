/* Steven Andrews, started 10/22/2001.
 This is a library of functions for the Smoldyn program.  See documentation
 called Smoldyn_doc1.pdf and Smoldyn_doc2.pdf.
 Copyright 2003-2011 by Steven Andrews.  This work is distributed under the terms
 of the Gnu General Public License (GPL). */

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
#include <fstream>
#include <sstream>
#include <string>
#include <iostream>
extern "C"
{
#include "zlib.h"
}

#define CHECK(A) if(!(A)) {printfException("Unknown solver error.");goto failure;} else (void)0
#define CHECKS(A,B) if(!(A)) {strncpy(erstr,B,STRCHAR-1);erstr[STRCHAR-1]='\0'; printfException("%s", B); goto failure;} else (void)0

/******************************************************************************/
/***************************** Simulation structure ***************************/
/******************************************************************************/


/******************************************************************************/
/********************************* enumerated types ***************************/
/******************************************************************************/


/* simstring2ss */
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


/* simss2string */
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


/* simsc2string */
char *simsc2string(enum StructCond sc,char *string) {
	if(sc==SCinit) strcpy(string,"not initialized");
	else if(sc==SClists) strcpy(string,"lists need updating");
	else if(sc==SCparams) strcpy(string,"parameters need updating");
	else if(sc==SCok) strcpy(string,"fully updated");
	else strcpy(string,"none");
	return string; }


/******************************************************************************/
/****************************** low level utilities ***************************/
/******************************************************************************/

/* Simsetrandseed */
void Simsetrandseed(simptr sim,long int randseed) {
	if(!sim) return;
	sim->randseed=randomize(randseed);
	return; }


/******************************************************************************/
/******************************* memory management ****************************/
/******************************************************************************/

/* simalloc */
simptr simalloc(const char *fileroot) {
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
	Simsetrandseed(sim,-1);
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
	sim->threads=NULL;
	simsetpthreads(sim,0);

	CHECK(sim->filepath=EmptyString());
	CHECK(sim->filename=EmptyString());
	CHECK(sim->flags=EmptyString());
	CHECK(sim->cmds=scmdssalloc(&docommand,(void*)sim,fileroot));
	return sim;

 failure:
	simfree(sim);
	return NULL; }


/* simfree */
void simfree(simptr sim) {
	int dim,order;

	if(!sim) return;
	dim=sim->dim;

	threadssfree(sim->threads);
	graphssfree(sim->graphss);
	scmdssfree(sim->cmds);
	mzrssfree(sim->mzrss);
	portssfree(sim->portss);
	compartssfree(sim->cmptss);
	boxssfree(sim->boxs);
	surfacessfree(sim->srfss);
	wallsfree(sim->wlist,dim);
	molssfree(sim->mols);
	for(order=0;order<MAXORDER;order++) rxnssfree(sim->rxnss[order]);
	free(sim->flags);
	free(sim->filename);
	free(sim->filepath);
	free(sim);
	return; }


/******************************************************************************/
/***************************** data structure output **************************/
/******************************************************************************/

/* simoutput */
void simoutput(simptr sim) {
	int vflag;

	vflag=strchr(sim->flags,'v')?1:0;
	
	printf("SIMULATION PARAMETERS\n");
	if(!sim) {
		printf(" No simulation parameters\n\n");
		return; }
	if(sim->filename[0]!='\0')
		printf(" file: %s%s\n",sim->filepath,sim->filename);
	printf(" starting clock time: %s",ctime(&sim->clockstt));
	printf(" %i dimensions\n",sim->dim);
	if(sim->accur<10 || vflag) printf(" Accuracy level: %g\n",sim->accur);
	printf(" Random number seed: %li\n",sim->randseed);

	if(sim->threads) printf(" Using threading with %d threads\n",sim->threads->nthreads);
	else if(vflag) printf(" Running in single-threaded mode\n");
	
	printf(" Time from %g to %g step %g\n",sim->tmin,sim->tmax,sim->dt);
	if(sim->time!=sim->tmin) printf(" Current time: %g\n",sim->time);
	printf("\n");
	return; }


/* simsystemoutput */
void simsystemoutput(simptr sim) {
	int order;

	simoutput(sim);
	graphssoutput(sim);
	walloutput(sim);
	molssoutput(sim);
	surfaceoutput(sim);
	scmdoutput(sim->cmds);
	boxssoutput(sim);
	for(order=0;order<MAXORDER;order++)
		rxnoutput(sim,order);
	compartoutput(sim);
	portoutput(sim);
	mzrssoutput(sim);
	return; }


/* writesim */
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

#include <stdarg.h>
void printfException(const char* format, ...) {
	char message[4000];
	va_list arguments;
	va_start(arguments, format);
	vsprintf(message, format, arguments);
	va_end(arguments);
	printf("%s", message);
	throw message;
}

/* checksimparams */
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



/* simsetpthreads */
int simsetpthreads(simptr sim,int number) {
#ifndef THREADING
	number=0;
#endif

	if(number<=0) {											// unthreaded operation
		sim->diffusefn=&diffuse;
		sim->surfaceboundfn=&checksurfacebound;
		sim->surfacecollisionsfn=&checksurfaces;
		sim->assignmols2boxesfn=&reassignmolecs;
		sim->zeroreactfn=&zeroreact;
		sim->unimolreactfn=&unireact;
		sim->bimolreactfn=&bireact;
		sim->checkwallsfn=&checkwalls;
		number=0; }

	else if(sim->threads && sim->threads->nthreads==number);	// still threaded

	else {																// start threaded or change threaded
		if(sim->threads) threadssfree(sim->threads);
		sim->threads=alloc_threadss(number);
		if(!sim->threads) {
			simsetpthreads(sim,0);
			return -2; }
		sim->diffusefn=&diffuse_threaded;
		sim->surfaceboundfn=&checksurfacebound;
		sim->surfacecollisionsfn=&checksurfaces_threaded;
		sim->assignmols2boxesfn=&reassignmolecs;
		sim->zeroreactfn=&zeroreact;
		sim->unimolreactfn=&unireact_threaded;
		sim->bimolreactfn=&bireact_threaded;
		sim->checkwallsfn=&checkwalls_threaded; }

	return number; }


/* simsetcondition */
void simsetcondition(simptr sim,enum StructCond cond,int upgrade) {
	if(!sim) return;
	if(upgrade==0 && sim->condition>cond) sim->condition=cond;
	else if(upgrade==1 && sim->condition<cond) sim->condition=cond;
	else if(upgrade==2) sim->condition=cond;
	return; }


/* simsetdim */
int simsetdim(simptr sim,int dim) {
	if(sim->dim!=0) return 2;
	if(dim<1 || dim>DIMMAX) return 3;
	sim->dim=dim;
	return 0; }


/* simsettime */
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


/* simreadstring */
int simreadstring(simptr sim,const char *word,char *line2,char *erstr) {
	char nm[STRCHAR],nm1[STRCHAR],shapenm[STRCHAR],ch,rname[STRCHAR],errstring[STRCHAR];
	int er,dim,i,nmol,d,i1,s,c,ll,order,more,rctident[MAXORDER],nprod,prdident[MAXPRODUCT],r,ord,prd,itct,prt,lt;
	double flt1,flt2,v1[DIMMAX*DIMMAX],v2[4],poslo[DIMMAX],poshi[DIMMAX];
	enum MolecState ms,rctstate[MAXORDER],prdstate[MAXPRODUCT];
	enum PanelShape ps;
	enum RevParam rpart;
	enum LightParam ltparam;
	rxnptr rxn;
	compartptr cmpt;
	surfaceptr srf;
	portptr port;
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
		er=molenablemols(sim,i1);
		CHECKS(er!=1,"out of memory");
		CHECKS(er!=2,"cannot decrease the number of allocated species");
		CHECKS(!strnword(line2,2),"unexpected text following max_species"); }

	else if(!strcmp(word,"species") || !strcmp(word,"names") || !strcmp(word,"name")) {// species, names, name
		while(line2) {
			itct=sscanf(line2,"%s",nm);
			CHECKS(itct==1,"failed to read species name");
			er=moladdspecies(sim,nm);
			CHECKS(er!=-4,"'empty' is not a permitted species name");
			CHECKS(er!=-5,"this species has already been declared");
			line2=strnword(line2,2); }}

	else if(!strcmp(word,"max_mol")) {						// max_mol
		itct=sscanf(line2,"%i",&i1);
		CHECKS(itct==1,"max_mol needs to be an integer");
		er=molsetmaxmol(sim,i1);
		CHECKS(er!=1,"out of memory");
		CHECKS(er!=5,"more molecule already exist than are requested with max_mol");
		CHECKS(!strnword(line2,2),"unexpected text following max_mol"); }

	else if(!strcmp(word,"difc")) {								// difc
		CHECKS(sim->mols,"need to enter species before difc");
		i=readmolname(sim,line2,&ms);
		CHECKS(i!=0,"empty molecules cannot diffuse");
		CHECKS(i!=-1,"difc format: name[(state)] value");
		CHECKS(i!=-2,"mismatched or improper parentheses around molecule state");
		CHECKS(i!=-3,"cannot read molecule state value");
		CHECKS(i!=-4,"molecule name not recognized");
		CHECKS(ms<MSMAX || ms==MSall,"invalid state");
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
		CHECKS(ms<MSMAX || ms==MSall,"invalid state");
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
		CHECKS(ms<MSMAX || ms==MSall,"invalid state");
		CHECKS(line2=strnword(line2,2),"missing vector in drift");
		itct=strreadnd(line2,dim,v1,NULL);
		CHECKS(itct==dim,"incomplete vector in drift");
		CHECKS(molsetdrift(sim,i,ms,v1)==0,"out of memory in drift");
		CHECKS(!strnword(line2,dim+1),"unexpected text following drift"); }

	else if(!strcmp(word,"mol")) {								// mol
		CHECKS(sim->mols,"need to enter species before mol");
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
		er=addmol(sim,nmol,i,poslo,poshi,0);
		CHECKS(!er,"more molecules assigned than permitted with max_mol");
		CHECKS(!strnword(line2,2),"unexpected text following mol"); }

	else if(!strcmp(word,"surface_mol")) {				// surface_mol
		CHECKS(sim->mols,"need to enter species before surface_mol");
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
			CHECKS(er!=3,"not enough molecules permitted by max_mol");
			line2=strnword(line2,dim+1); }
		else {
			er=addsurfmol(sim,nmol,i,ms,NULL,NULL,s,ps,nm1);
			CHECKS(er!=1,"in surface_mol, unable to allocate temporary storage space");
			CHECKS(er!=2,"in surface_mol, no panels match the given description");
			CHECKS(er!=3,"not enough molecules permitted by max_mol"); }
		CHECKS(!line2,"unexpected text following surface_mol"); }

	else if(!strcmp(word,"compartment_mol")) {		// compartment_mol
		CHECKS(sim->mols,"need to enter species before compartment_mol");
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
		CHECKS(er!=3,"not enough molecules permitted by max_mol");
		CHECKS(!strnword(line2,2),"unexpected text following compartment_mol"); }

	else if(!strcmp(word,"molecule_lists")) {			// molecule_lists
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
		CHECKS(sim->mols && sim->mols->nlist>0,"need to enter molecule_lists before mol_list");
		i=readmolname(sim,line2,&ms);
		CHECKS(i!=0,"empty molecules not permitted");
		CHECKS(i!=-1,"mol_list format: name[(state)] list_name");
		CHECKS(i!=-2,"mismatched or improper parentheses around molecule state");
		CHECKS(i!=-3,"cannot read molecule state value");
		CHECKS(ms<MSMAX || ms==MSall,"invalid state");
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
		er=graphicsreadcolor(&line2,v2);
		CHECKS(er!=3,"color values need to be between 0 and 1");
		CHECKS(er!=4,"color name not recognized");
		CHECKS(er!=6,"alpha values need to be between 0 and 1");
		CHECKS(er==0,"format is either 3 numbers or color name, and then optional alpha value");
		graphss->framecolor[0]=v2[0];
		graphss->framecolor[1]=v2[1];
		graphss->framecolor[2]=v2[2];
		graphss->framecolor[3]=v2[3];
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
		er=graphicsreadcolor(&line2,v2);
		CHECKS(er!=3,"color values need to be between 0 and 1");
		CHECKS(er!=4,"color name not recognized");
		CHECKS(er!=6,"alpha values need to be between 0 and 1");
		CHECKS(er==0,"format is either 3 numbers or color name, and then optional alpha value");
		graphss->gridcolor[0]=v2[0];
		graphss->gridcolor[1]=v2[1];
		graphss->gridcolor[2]=v2[2];
		graphss->gridcolor[3]=v2[3];
		CHECKS(!line2,"unexpected text following grid_color"); }

	else if(!strcmp(word,"background_color")) {		// background_color
		CHECKS(sim->graphss,"graphics needs to be entered before background_color");
		graphss=sim->graphss;
		er=graphicsreadcolor(&line2,v2);
		CHECKS(er!=3,"color values need to be between 0 and 1");
		CHECKS(er!=4,"color name not recognized");
		CHECKS(er!=6,"alpha values need to be between 0 and 1");
		CHECKS(er==0,"format is either 3 numbers or color name, and then optional alpha value");
		graphss->backcolor[0]=v2[0];
		graphss->backcolor[1]=v2[1];
		graphss->backcolor[2]=v2[2];
		graphss->backcolor[3]=v2[3];
		CHECKS(!line2,"unexpected text following background_color"); }
	
	else if(!strcmp(word,"text_color")) {		// text_color
		CHECKS(sim->graphss,"graphics needs to be entered before text_color");
		graphss=sim->graphss;
		er=graphicsreadcolor(&line2,v2);
		CHECKS(er!=3,"color values need to be between 0 and 1");
		CHECKS(er!=4,"color name not recognized");
		CHECKS(er!=6,"alpha values need to be between 0 and 1");
		CHECKS(er==0,"format is either 3 numbers or color name, and then optional alpha value");
		graphss->textcolor[0]=v2[0];
		graphss->textcolor[1]=v2[1];
		graphss->textcolor[2]=v2[2];
		graphss->textcolor[3]=v2[3];
		CHECKS(!line2,"unexpected text following textground_color"); }

	else if(!strcmp(word,"text_display")) {				// text_display
		CHECKS(sim->graphss,"graphics needs to be entered before text_display");
		while(line2) {
			itct=sscanf(line2,"%s",nm);
			CHECKS(itct==1,"error reading text_display item");
			er=graphicssettextitem(sim,nm);
			CHECKS(er!=1,"out of memory");
			CHECKS(er!=2,"unrecognized text display item (check that species have been defined)");
			line2=strnword(line2,2); }
		CHECKS(!line2,"unexpected text following text_display"); }

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
		CHECKS(ms<MSMAX || ms==MSall,"invalid state");
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
		CHECKS(ms<MSMAX || ms==MSall,"invalid state");
		line2=strnword(line2,2);
		er=graphicsreadcolor(&line2,v2);
		CHECKS(er!=3,"color values need to be between 0 and 1");
		CHECKS(er!=4,"color name not recognized");
		CHECKS(er!=6,"alpha values need to be between 0 and 1");
		CHECKS(er==0,"format is either 3 numbers or color name, and then optional alpha value");
		molsetcolor(sim,i,ms,v2);
		CHECKS(!line2,"unexpected text following color"); }

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
		er=scmdsetfnames(sim->cmds,line2,0);
		CHECKS(er!=1,"out of memory in output_files");
		CHECKS(er!=2,"error reading file name");
		CHECKS(er!=4,"BUG: variable cmds became NULL"); }
	
	else if(!strcmp(word,"append_files")) {				// append_files
		er=scmdsetfnames(sim->cmds,line2,1);
		CHECKS(er!=1,"out of memory");
		CHECKS(er!=2,"error reading file name");
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
		CHECKS(dim>0,"need to enter dim before max_surface");
		itct=sscanf(line2,"%i",&i1);
		CHECKS(itct==1,"max_surface needs to be a number");
		CHECKS(i1>0,"max_surface must be greater than 0");
		CHECKS(surfenablesurfaces(sim,i1)==0,"failed to enable surfaces");
		CHECKS(!strnword(line2,2),"unexpected text following max_surface"); }

	else if(!strcmp(word,"new_surface")) {				// new_surface
		CHECKS(dim>0,"need to enter dim before new_surface");
		srf=surfreadstring(sim,NULL,"name",line2,errstring);
		CHECKS(srf!=NULL,errstring); }

	else if(!strcmp(word,"surface")) {						// surface
		CHECKS(dim>0,"need to enter dim before surface");
		CHECKS(sim->srfss,"individual surfaces need to be defined before using surface");
		itct=sscanf(line2,"%s %s",nm,nm1);
		CHECKS(itct==2,"surface format: surface_name statement_name statement_text");
		line2=strnword(line2,3);
		CHECKS(line2,"surface format: surface_name statement_name statement_text");
		s=stringfind(sim->srfss->snames,sim->srfss->nsrf,nm);
		CHECKS(s>=0,"surface is unrecognized");
		srf=sim->srfss->srflist[s];
		srf=surfreadstring(sim,srf,nm1,line2,errstring);
		CHECKS(srf!=NULL,errstring); }

	// compartments

	else if(!strcmp(word,"max_compartment")) {		// max_compartment
		itct=sscanf(line2,"%i",&i1);
		CHECKS(itct==1,"max_compartment needs to be a number");
		CHECKS(i1>=0,"max_compartment must be at least 0");
		CHECKS(!compartenablecomparts(sim,i1),"failed to enable compartments");
		CHECKS(!strnword(line2,2),"unexpected text following max_compartment"); }

	else if(!strcmp(word,"new_compartment")) {		// new_compartment
		cmpt=compartreadstring(sim,NULL,"name",line2,errstring);
		CHECKS(cmpt!=NULL,errstring); }

	else if(!strcmp(word,"compartment")) {				// compartment
		CHECKS(sim->cmptss,"individual compartments need to be defined before using compartment");
		itct=sscanf(line2,"%s %s",nm,nm1);
		CHECKS(itct==2,"compartment format: compart_name statement_name statement_text");
		line2=strnword(line2,3);
		CHECKS(line2,"compartment format: compart_name statement_name statement_text");
		c=stringfind(sim->cmptss->cnames,sim->cmptss->ncmpt,nm);
		CHECKS(c>=0,"compartment is unrecognized");
		cmpt=sim->cmptss->cmptlist[c];
		cmpt=compartreadstring(sim,cmpt,nm1,line2,errstring);
		CHECKS(cmpt!=NULL,errstring); }

	// ports

	else if(!strcmp(word,"max_port")) {						// max_port
		itct=sscanf(line2,"%i",&i1);
		CHECKS(itct==1,"max_port needs to be a number");
		CHECKS(i1>=0,"max_port must be at least 0");
		CHECKS(portenableports(sim,i1),"failed to enable ports");
		CHECKS(!strnword(line2,2),"unexpected text following max_port"); }

	else if(!strcmp(word,"new_port")) {					// new_port
		port=portreadstring(sim,NULL,"name",line2,errstring);
		CHECKS(port!=NULL,errstring); }

	else if(!strcmp(word,"port")) {							// port
		CHECKS(sim->portss,"individual ports need to be defined before using port");
		itct=sscanf(line2,"%s %s",nm,nm1);
		CHECKS(itct==2,"port format: port_name statement_name statement_text");
		line2=strnword(line2,3);
		CHECKS(line2,"port format: port_name statement_name statement_text");
		prt=stringfind(sim->portss->portnames,sim->portss->nport,nm);
		CHECKS(prt>=0,"port is unrecognized");
		port=sim->portss->portlist[prt];
		port=portreadstring(sim,port,nm1,line2,errstring);
		CHECKS(port!=NULL,errstring); }

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
	
	else if(!strcmp(word,"expand_network")) {						// expand_network
		CHECKS(sim->mzrss,"need to enter start_rules before expand_network");
		itct=sscanf(line2,"%i",&i1);
		CHECKS(itct==1,"expand_network needs to be an integer");
		CHECKS(i1==0 || i1==1,"expand_network needs to be 0 or 1");
		mzrSetValue(sim->mzrss,"expandall",i1);
		CHECKS(!strnword(line2,2),"unexpected text following expand_network"); }
	
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
		CHECKS(ms<MSMAX || ms==MSall,"invalid state");
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
		itct=sscanf(line2,"%s",nm);
		CHECKS(itct==4,"format: name[(state)] color");
		i=mzrReadStreamName(nm,nm1,&ms);
		CHECKS(i!=-1,"format: name[(state)] value");
		CHECKS(i!=-2,"mismatched or improper parentheses around molecule state");
		CHECKS(i!=-3,"cannot read molecule state");
		CHECKS(i!=-5,"'all' is not permitted");
		line2=strnword(line2,2);
		er=graphicsreadcolor(&line2,v2);
		CHECKS(er!=3,"color values need to be between 0 and 1");
		CHECKS(er!=4,"color name not recognized");
		CHECKS(er!=6,"alpha values need to be between 0 and 1");
		CHECKS(er==0,"format is either 3 numbers or color name, and then optional alpha value");
		mzrSetStreamDisplay(sim->mzrss,nm1,ms,-1,v2);
		CHECKS(!line2,"unexpected text following display_size"); }

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
				CHECKS(ms<MSMAX1 || ms==MSall,"invalid state");
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
				CHECKS(ms<MSMAX1,"invalid product state");
				CHECKS(!(order==0 && srf==NULL && ms!=MSsoln),"use reaction_surface for order 0 reactions with surface-bound products");
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
		er=molssetgausstable(sim,i1);
		CHECKS(er!=1,"out of memory");
		CHECKS(er!=2,"need to enter max_species before gauss_table_size");
		CHECKS(er!=3,"gauss_table_size needs to be an integer power of two");
		CHECKS(!strnword(line2,2),"unexpected text following gauss_table_size"); }

	else if(!strcmp(word,"epsilon")) {						// epsilon
		CHECKS(dim>0,"need to enter dim before epsilon");
		itct=sscanf(line2,"%lg",&flt1);
		CHECKS(itct==1,"epsilon format: value");
		er=surfsetepsilon(sim,flt1);
		CHECKS(er!=2,"out of memory");
		CHECKS(er!=3,"epsilon value needs to be at least 0");
		CHECKS(!strnword(line2,2),"unexpected text following epsilon"); }
	
	else if(!strcmp(word,"margin")) {						// margin
		CHECKS(dim>0,"need to enter dim before margin");
		itct=sscanf(line2,"%lg",&flt1);
		CHECKS(itct==1,"margin format: value");
		er=surfsetmargin(sim,flt1);
		CHECKS(er!=2,"out of memory");
		CHECKS(er!=3,"margin value needs to be at least 0");
		CHECKS(!strnword(line2,2),"unexpected text following margin"); }
	
	else if(!strcmp(word,"neighbor_dist")) {			// neighbor_dist
		CHECKS(dim>0,"need to enter dim before neighbor_dist");
		itct=sscanf(line2,"%lg",&flt1);
		CHECKS(itct==1,"neighbor_dist format: value");
		er=surfsetneighdist(sim,flt1);
		CHECKS(er!=2,"out of memory");
		CHECKS(er!=3,"neighdist value needs to be at least 0");
		CHECKS(!strnword(line2,2),"unexpected text following neighbor_dist"); }

	else if(!strcmp(word,"pthreads")) {						// pthreads
		itct=sscanf(line2,"%i",&i1);
		CHECKS(itct==1,"pthreads format: number_of_threads");
		CHECKS(i1>=0,"number of threads must be >= 0");
		er=simsetpthreads(sim,i1);
		CHECKS(er!=-2,"out of memory allocating threading memory");
		CHECKS(er==i1,"pthreads are not available");
		CHECKS(!strnword(line2,2),"unexpected text following pthreads"); }

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
			}
#endif
		}
	}
	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_STARTING, "setting up simulation"));
	return 0;

failure:		// failure
	return 1;
}

unsigned char fromHex(const char* src) {
	char chs[5];
	chs[0] = '0';
	chs[1] = 'x';
	chs[2] = src[0];
	chs[3] = src[1];
	chs[4] = 0;
	int v;
	sscanf(chs, "%x", &v);
	return (unsigned char)v;
}

int loadHighResVolumeSamples(simptr sim,ParseFilePtr *pfpptr,char *line2,char *erstr) {
	if (line2 == 0) {
		return 0;
	}
	using namespace std;
	char fileName[128];
	strcpy(fileName, line2);

	char word[STRCHAR];
	ParseFilePtr pfp = *pfpptr;
	int pfpcode=Parse_ReadLine(&pfp,word,&line2,erstr);
	*pfpptr=pfp;

	VolumeSamplesPtr volumeSamplesPtr = new VolumeSamples;
	sim->volumeSamplesPtr = volumeSamplesPtr;
	volumeSamplesPtr->num[0] = volumeSamplesPtr->num[1] = volumeSamplesPtr->num[2] = 1;

	ifstream ifs(fileName);
	string line, nextToken;
	while (!ifs.eof()) {		
		getline(ifs, line);
		istringstream lineInput(line);

		nextToken = "";
		lineInput >> nextToken;
		if (nextToken.size() == 0 || nextToken[0] == '#') {
			continue;
		}
		if (nextToken == "Size") {
			lineInput >> volumeSamplesPtr->size[0] >> volumeSamplesPtr->size[1] >> volumeSamplesPtr->size[2];
		} else if (nextToken == "Origin") {
			lineInput >> volumeSamplesPtr->originX >> volumeSamplesPtr->originY >> volumeSamplesPtr->originZ;
		} else if (nextToken == "CompartmentHighResPixelMap") {
			int count;
			int pixel;
			lineInput >> count;
			volumeSamplesPtr->compartmentIDPairPtr = new CompartmentIdentifierPair[count];
			for (int i = 0; i < count; i ++) {
				getline(ifs, line);
				istringstream lineInput1(line);
				lineInput1 >> volumeSamplesPtr->compartmentIDPairPtr[i].name >> pixel;
				volumeSamplesPtr->compartmentIDPairPtr[i].pixel = pixel;
			}
			volumeSamplesPtr->nCmptIDPair = count;
		} else if (nextToken == "VolumeSamples") {
			lineInput >> volumeSamplesPtr->num[0] >> volumeSamplesPtr->num[1] >> volumeSamplesPtr->num[2];
			break;
		}
	}

	long numVolume = volumeSamplesPtr->num[0] * volumeSamplesPtr->num[1] * volumeSamplesPtr->num[2];

	getline(ifs, line);
	int compressed_len = line.size();
	if (compressed_len <= 1) {
		throw "CartesianMesh::readGeometryFile() : invalid compressed volume";
	}

	const char* compressed_hex = line.c_str();
	//volumeSamples compressed, changed from byte to short
	unsigned char* bytes_from_compressed = new unsigned char[compressed_len+1];
	memset(bytes_from_compressed, 0, (compressed_len+1) * sizeof(unsigned char));
	for (int i = 0, j = 0; i < compressed_len; i += 2, j ++) {
		bytes_from_compressed[j] = fromHex(compressed_hex + i);
	}

	volumeSamplesPtr->volsamples = new unsigned char[numVolume];
	memset(volumeSamplesPtr->volsamples, 0, numVolume * sizeof(unsigned char));

	unsigned long inflated_len = numVolume;
	int retVal = uncompress(volumeSamplesPtr->volsamples, &inflated_len, bytes_from_compressed, compressed_len);
	
	if (inflated_len = numVolume) {
		/*for (unsigned long i = 0; i < inflated_len; i ++) {		
			if (volumeSamplesPtr->volsamples[i] == 6) {
				cout << "volume sample  at " << i<< " is " << ((int)volumeSamplesPtr->volsamples[i])<< endl;
			}
			else if (volumeSamplesPtr->volsamples[i] == 16) {
				cout << "volume sample  at " << i<< " is " << ((int)volumeSamplesPtr->volsamples[i])<< endl;
			}
		}*/
	} else {
		throw "loadHighResVolumeSamples : unexpected number of volume samples";
	}
	return 0;

failure:		// failure
	return 1;
}

/* loadsim */
int loadsim(simptr sim,const char *fileroot,const char *filename,char *erstr,const char *flags) {
	int done,pfpcode,er;
	char word[STRCHAR],*line2,errstring[STRCHAR];
	ParseFilePtr pfp;

	if(!sim || !fileroot || !filename || !erstr || !flags) return 0;
	strncpy(sim->filepath,fileroot,STRCHAR);
	strncpy(sim->filename,filename,STRCHAR);
	strncpy(sim->flags,flags,STRCHAR);
	done=0;
	pfp=Parse_Start(fileroot,filename,errstring);
	CHECKS(pfp,errstring);
	er=Parse_CmdLineArg(NULL,NULL,pfp);
	CHECKS(!er,"out of memory");
	sim->volumeSamplesPtr = NULL;//initialize the volumesample to null

	while(!done) {
		if(pfp->lctr==0 && !strchr(flags,'q'))
			printf(" Reading file: '%s'\n",pfp->fname);
		pfpcode=Parse_ReadLine(&pfp,word,&line2,errstring);
		CHECKS(pfpcode!=3,errstring);

		if(pfpcode==0);																// already taken care of

		else if(pfpcode==2) {													// end reading
			done=1; }

		else if(pfpcode==3) {													// error
			CHECKS(0,"SMOLDYN BUG: parsing error"); }

		else if(!strcmp(word,"start_jms")) {			// jms settings
			CHECKS(!loadJMS(sim,&pfp,line2,errstring),errstring); }

		else if(!strcmp(word,"highResVolumeSamplesFile")) {			//highResVolumeSamplesFile
			CHECKS(!loadHighResVolumeSamples(sim,&pfp,line2,errstring),errstring); }

		else if(!strcmp(word,"start_reaction")) {			// start_reaction
			CHECKS(sim->mols,"need to enter species before reactions");
			CHECKS(!loadrxn(sim,&pfp,line2,errstring),errstring); }

		else if(!strcmp(word,"start_surface")) {			// start_surface
			CHECKS(sim->dim>0,"need to enter dim before start_surface");
			CHECKS(!loadsurface(sim,&pfp,line2,errstring),errstring); }

		else if(!strcmp(word,"start_compartment")) {	// start_compartment
			CHECKS(sim->dim>0,"need to enter dim before start_compartment");
			CHECKS(!loadcompart(sim,&pfp,line2,errstring),errstring); }

		else if(!strcmp(word,"start_port")) {					// start_port
			CHECKS(!loadport(sim,&pfp,line2,errstring),errstring); }

		else if(!strcmp(word,"start_rules")) {				// start_rules
			CHECKS(!mzrssreadrules(sim,&pfp,errstring),errstring); }

		else if(!line2) {															// just word
			CHECKS(0,"unknown word or missing parameter"); }

		else {
			er=simreadstring(sim,word,line2,errstring);
			CHECKS(!er,errstring); }}

	return 0;

 failure:																					// failure
	if(!done)
		er=10+Parse_ReadFailure(pfp,erstr);
	else er=10;
	return er; }


/* simupdate */
int simupdate(simptr sim,char *erstr) {
	int er,qflag;
	static int recurs=0;
	char errstring[STRCHAR];

	if(sim->condition==SCok) {
		return 0; }
	if(recurs>10) {
		recurs=0;
		return 2; }
	recurs++;

	qflag=strchr(sim->flags,'q')?1:0;

	if(sim->condition==SCinit && !qflag && sim->mols) printf(" setting up molecules\n");
	er=molsupdate(sim);
	CHECKS(er!=1,"out of memory setting up molecules");

	if(sim->condition==SCinit && !qflag) printf(" setting up virtual boxes\n");
	er=boxesupdate(sim);
	CHECKS(er!=1,"out of memory setting up spatial partitions");
	CHECKS(er!=3,"simulation dimensions or boundaries are undefined");

	er=molsort(sim);
	CHECKS(er!=1,"out of memory during molecule sorting\n");

	if(sim->condition==SCinit && !qflag && sim->cmptss) printf(" setting up compartments\n");
	er=compartsupdate(sim);
	CHECKS(er!=1,"out of memory setting up compartments");

	if(sim->condition==SCinit && !qflag && (sim->rxnss[0] || sim->rxnss[1] || sim->rxnss[2])) printf(" setting up reactions\n");
	er=rxnsupdate(sim);
	CHECKS(er!=1,"out of memory setting up reactions");
	CHECKS(er!=3,"failed to set up reactions");

	if(sim->condition==SCinit && !qflag && sim->srfss) printf(" setting up surfaces\n");
	er=surfupdate(sim);
	CHECKS(er!=1,"out of memory setting up surfaces");

	if(sim->condition==SCinit && !qflag && sim->portss) printf(" setting up ports\n");
	er=portsupdate(sim);
	CHECKS(er!=1,"out of memory setting up ports");

	if(sim->condition==SCinit && !qflag && sim->mzrss) printf(" setting up moleculizer\n");
	CHECKS(!mzrsetupmoleculizer(sim,errstring),errstring);

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



/* setupsim */
int setupsim(const char *fileroot,const char *filename,simptr *smptr,const char *flags) {
	simptr sim;
	int er,order,qflag,wflag,vflag;
	char errstring[STRCHAR],erstr[STRCHAR];

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
		er=loadsim(sim,fileroot,filename,errstring,flags);

		if(er) {
			printfException("\nError reading file in line %i : %s\n",er-10, errstring);
			simfree(sim);
			sim=NULL;
			CHECKS(0,errstring); }
		if(!qflag) printf(" Loaded file successfully\n"); }

	qflag=strchr(sim->flags,'q')?1:0;
	wflag=strchr(sim->flags,'w')?1:0;
	vflag=strchr(sim->flags,'v')?1:0;

	er=simupdate(sim,errstring);
	CHECKS(!er,errstring);

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
	printfException("%s\n",erstr);
	if(!*smptr) simfree(sim);
	return 1; }


/******************************************************************************/
/************************** core simulation functions *************************/
/******************************************************************************/


/* simdocommands */
int simdocommands(simptr sim) {
	int er;
	enum CMDcode ccode;
	char erstr[STRCHAR];

	ccode=scmdexecute(sim->cmds,sim->time,sim->dt,-1,0);
	er=simupdate(sim,erstr);
	if(er) {
		printfException("%s",erstr);
		return 8; }
	er=molsort(sim);														// sort live and dead
	if(er) return 6;
	if(ccode==CMDstop || ccode==CMDabort) return 7;
	return 0; }


/* simulatetimestep */
int simulatetimestep(simptr sim) {
	int er,ll;
	char errstring[STRCHAR];

	er=simupdate(sim,errstring);										// update any data structure changes
	if(er) return 10;

	er=(*sim->diffusefn)(sim);											// diffuse
	if(er) return 9;

	if(sim->srfss) {																// deal with surface or wall collisions
		for(ll=0;ll<sim->srfss->nmollist;ll++) {
			if(sim->srfss->srfmollist[ll] & SMLdiffuse) {
		    (*sim->surfacecollisionsfn)(sim,ll,0); }}}
	else {
		if(sim->mols)
			for(ll=0;ll<sim->mols->nlist;ll++)
				if(sim->mols->diffuselist[ll])
					(*sim->checkwallsfn)(sim,ll,0,NULL); }

	if(sim->srfss) {																// surface-bound molecule actions
		for(ll=0;ll<sim->srfss->nmollist;ll++)
			if(sim->srfss->srfmollist[ll] & SMLsrfbound)
				(*sim->surfaceboundfn)(sim,ll); }

	er=(*sim->assignmols2boxesfn)(sim,1,0);					// assign to boxes (diffusing molecs., not reborn)
	if(er) return 2;

	er=(*sim->zeroreactfn)(sim);
	if(er) return 3;

	er=(*sim->unimolreactfn)(sim);
	if(er) return 4;

	er=(*sim->bimolreactfn)(sim,0);
	if(er) return 5;

	er=(*sim->bimolreactfn)(sim,1);
	if(er) return 5;

	er=molsort(sim);																// sort live and dead
	if(er) return 6;

	if(sim->srfss) {
		for(ll=0;ll<sim->srfss->nmollist;ll++)
			if(sim->srfss->srfmollist[ll] & SMLreact)
				(*sim->surfacecollisionsfn)(sim,ll,1); }	// surfaces again, reborn molecs. only
	else {
		if(sim->mols)
			for(ll=0;ll<sim->mols->nlist;ll++)
					(*sim->checkwallsfn)(sim,ll,1,NULL); }

	er=(*sim->assignmols2boxesfn)(sim,0,1);					// assign again (all, reborn)
	if(er) return 2;

	sim->time+=sim->dt;													// --- end of time step ---

	er=simdocommands(sim);
	if(er) return er;
	if(sim->time>=sim->tmax) return 1;

	return 0; }


/* endsimulate */
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
		if(eventcount[ETdesorb]) printf("%i desorptions\n",eventcount[ETdesorb]);
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


/* smolsimulate */
int smolsimulate(simptr sim) {
	int er,qflag;

	er=0;
	qflag=strchr(sim->flags,'q')?1:0;
	if(!qflag) printf("Starting simulation\n");
	sim->clockstt=time(NULL);
	er=simdocommands(sim);
	if(!er)
		while((er=simulatetimestep(sim))==0);
	sim->elapsedtime+=difftime(time(NULL),sim->clockstt);
	return er; }






