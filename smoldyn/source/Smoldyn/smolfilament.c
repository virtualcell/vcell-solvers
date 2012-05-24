/* Steven Andrews, started 10/22/2001.
 This is a library of functions for the Smoldyn program.  See documentation
 called Smoldyn_doc1.pdf and Smoldyn_doc2.pdf.
 Copyright 2003-2011 by Steven Andrews.  This work is distributed under the terms
 of the Gnu General Public License (GPL). */

#include <stdio.h>
#include <math.h>
#include "Sphere.h"
#include "smoldyn.h"
#include "smoldynfuncs.h"
#include <string.h>
#include <stdlib.h>


/******************************************************************************/
/********************************** Filaments *********************************/
/******************************************************************************/


/******************************************************************************/
/****************************** Local declarations ****************************/
/******************************************************************************/

// enumerated types


// low level utilities
double filStretchEnergy(filamentptr fil,int i1,int i2);
double filBendEnergy(filamentptr fil,int i1,int i2);

// memory management
filamentptr filalloc(int nmax);
void filfree(filamentptr fil);


// data structure output

// structure set up
void filSetParam(filamentptr fil,char *param,int index,double value);
int filupdateparams(simptr sim);
int filupdatelists(simptr sim);
	


/******************************************************************************/
/********************************* enumerated types ***************************/
/******************************************************************************/







/******************************************************************************/
/****************************** low level utilities ***************************/
/******************************************************************************/


// filStretchEnergy
double filStretchEnergy(filamentptr fil,int i1,int i2) {
	double *pthk,lk,*pl,lstd,energy;
	int i;
	
	pthk=fil->pthk;
	lk=fil->lk;
	pl=fil->pl;
	lstd=fil->lstd;
	energy=0;
	for(i=i1;i<i2;i++)
		energy+=0.5*pthk[i]*lk*(pl[i]-lstd)*(pl[i]-lstd);
	return energy; }


// filBendEnergy
double filBendEnergy(filamentptr fil,int i1,int i2) {
	double *ak,*astd,**pa,*pthk,energy;
	double tk;
	int i;
	
	ak=fil->ak;
	astd=fil->astd;
	pa=fil->pa;
	pthk=fil->pthk;
	energy=0;
	for(i=i1+1;i<=i2;i++) {
		tk=0.5*(pthk[i-1]+pthk[i]);
		energy+=0.5*tk*ak[1]*(pa[i][1]-astd[1])*(pa[i][1]-astd[1]);
		energy+=0.5*tk*ak[2]*(pa[i][2]-astd[2])*(pa[i][2]-astd[2]);
		energy+=0.5*tk*ak[3]*(pa[i][3]-astd[3])*(pa[i][3]-astd[3]); }
	return energy; }




/******************************************************************************/
/******************************* memory management ****************************/
/******************************************************************************/


/* filalloc */
filamentptr filalloc(int nmax) {
	int i;
	filamentptr fil;
	
	fil=NULL;
	CHECKMEM(fil=(filamentptr) malloc(sizeof(struct filamentstruct)));
	fil->fname=NULL;
	fil->nmax=nmax;
	fil->n=0;
	fil->px=NULL;
	fil->pl=NULL;
	fil->pa=NULL;
	fil->pd=NULL;
	fil->po=NULL;
	fil->pthk=NULL;
	fil->lstd=1.0;
	fil->astd[0]=0;
	fil->astd[1]=0;
	fil->astd[2]=0;
	fil->lk=1;
	fil->ak[0]=1;
	fil->ak[1]=1;
	fil->ak[2]=1;
	fil->kT=0;
	fil->surf=' ';
	fil->spar[0]=0;
	fil->spar[1]=0;
	
	CHECKMEM(fil->px=(double**)calloc(nmax+1,sizeof(double*)));
	for(i=0;i<=nmax;i++) fil->px[i]=NULL;
	for(i=0;i<=nmax;i++) CHECK(fil->px[i]=(double*)calloc(3,sizeof(double)));
	for(i=0;i<=nmax;i++) fil->px[i][0]=fil->px[i][1]=fil->px[i][2]=0;
	
	CHECKMEM(fil->pl=(double*)calloc(nmax,sizeof(double)));
	for(i=0;i<nmax;i++) fil->pl[i]=1;
	
	CHECKMEM(fil->pa=(double**)calloc(nmax,sizeof(double*)));
	for(i=0;i<nmax;i++) fil->pa[i]=NULL;
	for(i=0;i<nmax;i++) CHECK(fil->pa[i]=(double*)calloc(3,sizeof(double)));
	for(i=0;i<nmax;i++) fil->pa[i][0]=fil->pa[i][1]=fil->pa[i][2]=0;
	
	CHECKMEM(fil->pd=(double**)calloc(nmax,sizeof(double*)));
	for(i=0;i<nmax;i++) fil->pd[i]=NULL;
	for(i=0;i<nmax;i++) CHECK(fil->pd[i]=(double*)calloc(9,sizeof(double)));
	for(i=0;i<nmax;i++) Sph_One2Dcm(fil->pd[i]);
	
	CHECKMEM(fil->po=(double**)calloc(nmax,sizeof(double*)));
	for(i=0;i<nmax;i++) fil->po[i]=NULL;
	for(i=0;i<nmax;i++) CHECK(fil->po[i]=(double*)calloc(9,sizeof(double)));
	for(i=0;i<nmax;i++) Sph_One2Dcm(fil->po[i]);
	
	CHECKMEM(fil->pthk=(double*)calloc(nmax,sizeof(double)));
	for(i=0;i<nmax;i++) fil->pthk[i]=1;
	
	return fil;
	
failure:
	if(fil) filfree(fil);
	return NULL; }



/* filfree */
void filfree(filamentptr fil) {
	int i,nmax;
	
	if(!fil) return;
	nmax=fil->nmax;
	if(fil->pthk) free(fil->pthk);
	if(fil->po) {
		for(i=0;i<nmax;i++) if(fil->po[i]) free(fil->po[i]);
		free(fil->po); }
	if(fil->pd) {
		for(i=0;i<nmax;i++) if(fil->pd[i]) free(fil->pd[i]);
		free(fil->pd); }
	if(fil->pa) {
		for(i=0;i<nmax;i++) if(fil->pa[i]) free(fil->pa[i]);
		free(fil->pa); }
	if(fil->pl) free(fil->pl);
	if(fil->px) {
		for(i=0;i<nmax+1;i++) if(fil->px[i]) free(fil->px[i]);
		free(fil->px); }
	free(fil);
	return; }


/* filssalloc */
filamentssptr filssalloc(filamentssptr filss,int maxfil) {
	int f,newfilss;
	char **newnames;
	filamentptr *newfillist;
	
	if(maxfil<1) return NULL;
	
	newfilss=0;
	newnames=NULL;
	newfillist=NULL;
	
	if(!filss) {																			// new allocation
		filss=(filamentssptr) malloc(sizeof(struct filamentsuperstruct));
		CHECKMEM(filss);
		newfilss=1;
		filss->condition=SCinit;
		filss->sim=NULL;
		filss->maxfil=0;
		filss->nfil=0;
		filss->fnames=NULL;
		filss->fillist=NULL; }
	else {																						// minor check
		if(maxfil<filss->maxfil) return NULL; }
	
	if(maxfil>filss->maxfil) {											// allocate new fil names and fils
		CHECKMEM(newnames=(char**) calloc(maxfil,sizeof(char*)));
		for(f=0;f<maxfil;f++) newnames[f]=NULL;
		for(f=0;f<filss->maxfil;f++) newnames[f]=filss->fnames[f];
		for(;f<maxfil;f++)
			CHECKMEM(newnames[f]=EmptyString());
		
		CHECKMEM(newfillist=(filamentptr*) calloc(maxfil,sizeof(filamentptr)));	// fil list
		for(f=0;f<maxfil;f++) newfillist[f]=NULL;
		for(f=0;f<filss->maxfil;f++) newfillist[f]=filss->fillist[f];
		for(;f<maxfil;f++) {
			CHECKMEM(newfillist[f]=filalloc(100));		//?? fixed number of monomers for now at 100
			newfillist[f]->filss=filss;
			newfillist[f]->fname=newnames[f]; }}
	
	filss->maxfil=maxfil;
	free(filss->fnames);
	filss->fnames=newnames;
	free(filss->fillist);
	filss->fillist=newfillist;
	
	return filss;
	
failure:
	filssfree(filss);
	simLog(NULL,10,"Unable to allocate memory in filssalloc");
	return NULL; }


/* filssfree */
void filssfree(filamentssptr filss) {
	int f;

	if(!filss) return;
	if(filss->maxfil && filss->fillist)
	for(f=0;f<filss->maxfil;f++) filfree(filss->fillist[f]);
	free(filss->fillist);
	if(filss->maxfil && filss->fnames)
	for(f=0;f<filss->maxfil;f++) free(filss->fnames[f]);
	free(filss->fnames);
	free(filss);
	return; }


/******************************************************************************/
/***************************** data structure output **************************/
/******************************************************************************/


/* filoutput */
void filoutput(filamentptr fil) {
	int i;
	
	simLog(NULL,2," Filament: %s\n",fil->fname);
	if(!fil) {
		simLog(NULL,2," NULL pointer\n");
		return; }
	simLog(NULL,2," allocated size: %i, monomers: %i\n",fil->nmax,fil->n);
	simLog(NULL,2," standard length: %f\n",fil->lstd);
	simLog(NULL,2," standard angles: %f, %f, %f\n",fil->astd[0],fil->astd[1],fil->astd[2]);
	simLog(NULL,2," length force constant: %f\n",fil->lk);
	simLog(NULL,2," bending force constants: %f, %f, %f\n",fil->ak[0],fil->ak[1],fil->ak[2]);
	simLog(NULL,2," kT: %f\n",fil->kT);
	if(fil->surf!=' ')
		simLog(NULL,2," surface: %c, parameters: %f, %f\n",fil->surf,fil->spar[0],fil->spar[1]);
	simLog(NULL,2," stretching energy: %f\n",filStretchEnergy(fil,-1,-1));
	simLog(NULL,2," bending energy: %f\n",filBendEnergy(fil,-1,-1));
	simLog(NULL,2," monomer, thickness, length, angle, positions\n");
	for(i=0;i<fil->n;i++)
		simLog(NULL,2," %i w=%1.3f l=%1.3f x=(%1.3f %1.3f %1.3f) a=(%1.3f %1.3f %1.3f)\n",i,fil->pthk[i],fil->pl[i],fil->px[i][0],fil->px[i][1],fil->px[i][2],fil->pa[i][0],fil->pa[i][1],fil->pa[i][2]);
	return; }


/* filssoutput */
void filssoutput(simptr sim) {
	filamentssptr filss;
	filamentptr fil;
	int f,dim;
	
	filss=sim->filss;
	if(!filss) return;
	simLog(sim,2,"FILAMENT PARAMETERS\n");
	dim=sim->dim;
	simLog(sim,2," Filaments allocated: %i, filaments defined: %i\n",filss->maxfil,filss->nfil);
	for(f=0;f<filss->nfil;f++) {
		fil=filss->fillist[f];
		filoutput(fil); }
	simLog(sim,2,"\n");
	return; }


/* filwrite */
void filwrite(simptr sim,FILE *fptr) {
	filamentssptr filss;
	filamentptr fil;
	int f;
	
	filss=sim->filss;
	if(!filss) return;
	fprintf(fptr,"# filament parameters\n");
	fprintf(fptr,"max_filament %i\n",filss->maxfil);
	for(f=0;f<filss->nfil;f++) {
		fil=filss->fillist[f];
		fprintf(fptr,"start_filament %s\n",fil->fname);
		//TODO: write filwrite
		fprintf(fptr,"end_filament\n\n"); }
	return; }


/* filcheckparams */
int filcheckparams(simptr sim,int *warnptr) {
	int error,warn,f;
	filamentssptr filss;
	filamentptr fil;
	char string[STRCHAR];
	
	error=warn=0;
	filss=sim->filss;
	if(!filss) {
		if(warnptr) *warnptr=warn;
		return 0; }
	
	if(filss->condition!=SCok) {
		warn++;
		simLog(sim,7," WARNING: filament structure %s\n",simsc2string(filss->condition,string)); }
	
	for(f=0;f<filss->nfil;f++) {
		fil=filss->fillist[f];
		//TODO: write checkparams
	}

	if(warnptr) *warnptr=warn;
	return error; }






/******************************************************************************/
/******************************** structure set up ****************************/
/******************************************************************************/


/* filsetcondition */
void filsetcondition(filamentssptr filss,enum StructCond cond,int upgrade) {
	if(!filss) return;
	if(upgrade==0 && filss->condition>cond) filss->condition=cond;
	else if(upgrade==1 && filss->condition<cond) filss->condition=cond;
	else if(upgrade==2) filss->condition=cond;
	if(filss->sim && filss->condition<filss->sim->condition) {
		cond=filss->condition;
		simsetcondition(filss->sim,cond==SCinit?SClists:cond,0); }
	return; }


/* filenablefilaments */
int filenablefilaments(simptr sim,int maxfil) {
	filamentssptr filss;

	if(sim->filss)									// check for redundant function call
		if(maxfil==-1 || sim->filss->maxfil==maxfil)
			return 0;
	filss=filssalloc(sim->filss,maxfil<0?5:maxfil);
	if(!filss) return 1;
	sim->filss=filss;
	filss->sim=sim;
	filsetcondition(sim->filss,SClists,0);
	return 0; }


/* filSetParam */
void filSetParam(filamentptr fil,char *param,int index,double value) {
	if(!strcmp(param,"lstd")) fil->lstd=value;
	else if(!strcmp(param,"astd")) {
		if(index<0) fil->astd[0]=fil->astd[1]=fil->astd[2]=value;
		else fil->astd[index]=value; }
	else if(!strcmp(param,"lk")) fil->lk=value;
	else if(!strcmp(param,"ak")) {
		if(index<0) fil->ak[0]=fil->ak[1]=fil->ak[2]=value;
		else fil->ak[index]=value; }
	else if(!strcmp(param,"kT")) fil->kT=value;
	else if(!strcmp(param,"surf")) fil->surf=(char)value;
	else if(!strcmp(param,"spar")) fil->spar[index]=value;
	return; }



/* filaddfilament */
filamentptr filaddfilament(simptr sim,const char *fnames) {
	int er,f;
	filamentssptr filss;
	filamentptr fil;
	
	if(!sim->filss) {
		er=filenablefilaments(sim,-1);
		if(er) return NULL; }
	filss=sim->filss;
	
	f=stringfind(filss->fnames,filss->nfil,fnames);
	if(f<0) {
		if(filss->nfil==filss->maxfil) {
			er=filenablefilaments(sim,filss->nfil*2+1);
			if(er) return NULL; }
		f=filss->nfil++;
		strncpy(filss->fnames[f],fnames,STRCHAR-1);
		filss->fnames[f][STRCHAR-1]='\0';
		fil=filss->fillist[f]; }
	else
		fil=filss->fillist[f];

	//TODO: filaddfilament

	filsetcondition(filss,SClists,0);
	return fil; }


/* filreadstring */
filamentptr filreadstring(simptr sim,ParseFilePtr pfp,filamentptr fil,const char *word,char *line2) {
	char nm[STRCHAR];
	filamentssptr filss;
	int itct;
	
	filss=sim->filss;
	
	if(!strcmp(word,"name")) {								// name
		itct=sscanf(line2,"%s",nm);
		CHECKS(itct==1,"error reading filament name");
		fil=filaddfilament(sim,nm);
		CHECKS(fil,"failed to add filament");
		CHECKS(!strnword(line2,2),"unexpected text following name"); }

	else {																				// unknown word
		CHECKS(0,"syntax error within fil block: statement not recognized"); }
	
	return fil;
	
failure:
	simParseError(sim,pfp);
	return NULL; }


/* filload */
int filload(simptr sim,ParseFilePtr *pfpptr,char *line2) {
	ParseFilePtr pfp;
	char word[STRCHAR],errstring[STRCHAR];
	int done,pfpcode,firstline2;
	filamentptr fil;

	pfp=*pfpptr;
	done=0;
	fil=NULL;
	firstline2=line2?1:0;

	while(!done) {
		if(pfp->lctr==0)
			simLog(sim,2," Reading file: '%s'\n",pfp->fname);
			if(firstline2) {
				strcpy(word,"name");
				pfpcode=1;
				firstline2=0; }
			else
				pfpcode=Parse_ReadLine(&pfp,word,&line2,errstring);
				*pfpptr=pfp;
				CHECKS(pfpcode!=3,"%s",errstring);
				
				if(pfpcode==0);																// already taken care of
				else if(pfpcode==2) {													// end reading
					done=1; }
				else if(!strcmp(word,"end_filament")) {				// end_filament
					CHECKS(!line2,"unexpected text following end_filament");
					return 0; }
				else if(!line2) {															// just word
					CHECKS(0,"unknown word or missing parameter"); }
				else {
					fil=filreadstring(sim,pfp,fil,word,line2);
					CHECK(fil); }}															// failed but error has already been sent

	CHECKS(0,"end of file encountered before end_filament statement");	// end of file

	failure:																					// failure
	if(ErrorType!=1) simParseError(sim,pfp);
	return 1; }


/* filupdateparams */
int filupdateparams(simptr sim) {
	return 0; }

/* filupdatelists */
int filupdatelists(simptr sim) {
	return 0; }


/* filupdate */
int filsupdate(simptr sim) {
	int er;
	filamentssptr filss;
	
	filss=sim->filss;
	if(filss) {
		if(filss->condition<=SClists) {
			er=filupdatelists(sim);
			if(er) return er;
			filsetcondition(filss,SCparams,1); }
		if(filss->condition==SCparams) {
			er=filupdateparams(sim);
			if(er) return er;
			filsetcondition(filss,SCok,1); }}
	return 0; }








