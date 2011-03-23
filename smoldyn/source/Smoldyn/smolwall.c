/* Steven Andrews, started 10/22/2001.
This is a library of functions for the Smoldyn program.  See documentation
 called Smoldyn_doc1.pdf and Smoldyn_doc2.pdf.
 Copyright 2003-2011 by Steven Andrews.  This work is distributed under the terms
 of the Gnu General Public License (GPL). */

#include <stdio.h>
#include <math.h>
#include "random2.h"
#include "smoldyn.h"

#define CHECK(A) if(!(A)) goto failure
#define CHECKS(A,B) if(!(A)) {strncpy(erstr,B,STRCHAR-1);erstr[STRCHAR-1]='\0';goto failure;} else (void)0


/******************************************************************************/
/************************************ Walls ***********************************/
/******************************************************************************/


/******************************************************************************/
/****************************** low level utilities ***************************/
/******************************************************************************/

/* systemrandpos.  Returns a random point within the system volume, chosen with
a uniform distribution. */
void systemrandpos(simptr sim,double *pos) {
	int d;

	for(d=0;d<sim->dim;d++) pos[d]=unirandOOD(sim->wlist[2*d]->pos,sim->wlist[2*d+1]->pos);
	return; }


/* systemvolume.  Returns the total volume of the system. */
double systemvolume(simptr sim) {
	int d;
	double vol;

	vol=1.0;
	for(d=0;d<sim->dim;d++) vol*=sim->wlist[2*d+1]->pos-sim->wlist[2*d]->pos;
	return vol; }


/* systemcorners.  Returns the low and high corners of the system volume in
poslo and poshi, respectively.  Both results are optional; enter NULL if a
point is unwanted. */
void systemcorners(simptr sim,double *poslo,double *poshi) {
	int d;

	for(d=0;d<sim->dim;d++) {
		if(poslo) poslo[d]=sim->wlist[2*d]->pos;
		if(poshi) poshi[d]=sim->wlist[2*d+1]->pos; }
	return; }


/* systemdiagonal. */
double systemdiagonal(simptr sim) {
	int d;
	double diagonal;

	diagonal=0;
	for(d=0;d<sim->dim;d++)
		diagonal+=(sim->wlist[2*d+1]->pos-sim->wlist[2*d]->pos)*(sim->wlist[2*d+1]->pos-sim->wlist[2*d]->pos);
	return sqrt(diagonal); }


/* posinsystem.  Returns 1 if pos is within the system boundaries (equal to the
edges counts as inside) and 0 if it is outside. */
int posinsystem(simptr sim,double *pos) {
	int d;

	for(d=0;d<sim->dim;d++) {
		if(pos[d]<sim->wlist[2*d]->pos) return 0;
		if(pos[d]>sim->wlist[2*d+1]->pos) return 0; }
	return 1; }


/******************************************************************************/
/******************************* memory management ****************************/
/******************************************************************************/

/* wallalloc allocates and initializes a new wall.  The pointer to the opposite
wall needs to be set. */
wallptr wallalloc(void) {
	wallptr wptr;

	wptr=(wallptr) malloc(sizeof(struct wallstruct));
	if(!wptr) return 0;
	wptr->wdim=0;
	wptr->side=0;
	wptr->pos=0;
	wptr->type='r';
	wptr->opp=NULL;
	return wptr; }


/* wallfree frees a wall. */
void wallfree(wallptr wptr) {
	if(!wptr) return;
	free(wptr);
	return; }


/* wallsalloc allocates an array of pointers to 2*dim walls, allocates each of
the walls, and sets them to default conditions (reflecting walls at 0 and 1 on
each coordinate) with correct pointers in each opp member. */
wallptr *wallsalloc(int dim) {
	int w,d;
	wallptr *wlist;

	if(dim<1) return NULL;
	wlist=(wallptr *) calloc(2*dim,sizeof(wallptr));
	if(!wlist) return 0;
	for(w=0;w<2*dim;w++) wlist[w]=NULL;
	for(w=0;w<2*dim;w++)
		if(!(wlist[w]=wallalloc())) {wallsfree(wlist,dim);return 0;}
	for(d=0;d<dim;d++) {
		wlist[2*d]->wdim=wlist[2*d+1]->wdim=d;
		wlist[2*d]->side=0;
		wlist[2*d+1]->side=1;
		wlist[2*d]->pos=0;
		wlist[2*d+1]->pos=1;
		wlist[2*d]->type=wlist[2*d+1]->type='r';
		wlist[2*d]->opp=wlist[2*d+1];
		wlist[2*d+1]->opp=wlist[2*d]; }
	return wlist; }


/* wallsfree frees an array of 2*dim walls, including the walls. */
void wallsfree(wallptr *wlist,int dim) {
	if(!wlist||dim<1) return;
	for(dim--;dim>=0;dim--) {
		wallfree(wlist[2*dim+1]);
		wallfree(wlist[2*dim]); }
	free(wlist);
	return; }


/******************************************************************************/
/***************************** data structure output **************************/
/******************************************************************************/

/* walloutput prints the wall structure information, including wall dimensions,
positions, and types, as well as the total simulation volume. */
void walloutput(simptr sim) {
	int w,d,dim;
	wallptr *wlist,wptr;
	double vol,poslo[DIMMAX],poshi[DIMMAX];

	dim=sim->dim;
	wlist=sim->wlist;
	printf("WALL PARAMETERS\n");
	if(!wlist) {
		printf(" No walls defined for simulation\n\n");
		return; }
	for(w=0;w<2*dim;w++) {
		wptr=wlist[w];
		printf(" wall %i: dimension %i, at %g, type %c\n",w,wptr->wdim,wptr->pos,wptr->type);
		if(wlist[w+1-2*(w%2)]!=wptr->opp) printf(" ERROR: opposing wall is incorrect\n"); }
	vol=systemvolume(sim);
	if(dim==1) printf(" length: %g\n",vol);
	else if(dim==2) printf(" area: %g\n",vol);
	else printf(" volume: %g\n",vol);
	systemcorners(sim,poslo,poshi);
	printf(" system corners: (%g",poslo[0]);
	for(d=1;d<dim;d++) printf(",%g",poslo[d]);
	printf(") and (%g",poshi[0]);
	for(d=1;d<dim;d++) printf(",%g",poshi[d]);
	printf(")\n");
	printf("\n");
	return; }


/* writewalls.  Writes all information about the walls to the file fptr using a
format that can be read by Smoldyn.  This allows a simulation state to be saved.
*/
void writewalls(simptr sim,FILE *fptr) {
	int d,dim;

	fprintf(fptr,"# Boundary parameters\n");
	dim=sim->dim;
	for(d=0;d<dim;d++) {
		fprintf(fptr,"low_wall %i %g %c\n",d,sim->wlist[2*d]->pos,sim->wlist[2*d]->type);
		fprintf(fptr,"high_wall %i %g %c\n",d,sim->wlist[2*d+1]->pos,sim->wlist[2*d+1]->type); }
	fprintf(fptr,"\n");
	return; }

/* checkwallparams.  Checks some parameters of simulation walls to make sure
that they are reasonable.  Prints warning messages to the display.  Returns the
total number of errors and, if warnptr is not NULL, the number of warnings in
warnptr. */
int checkwallparams(simptr sim,int *warnptr) {
	int d,dim,warn,error;
	wallptr *wlist;
	double lowwall[DIMMAX],highwall[DIMMAX],syslen;

	error=warn=0;
	dim=sim->dim;
	wlist=sim->wlist;

	systemcorners(sim,lowwall,highwall);
	syslen=0;
	for(d=0;d<dim;d++) syslen+=(highwall[d]-lowwall[d])*(highwall[d]-lowwall[d]);
	syslen=sqrt(syslen);
	if(syslen<=0) {error++;printfException(" ERROR: Total system size is zero\n");}

	for(d=0;d<dim;d++)
		if(lowwall[d]>=highwall[d]) {
			printfException(" ERROR: low_wall positions need to be smaller than high_wall positions");
			error++; }

	if(!sim->srfss) {
		for(d=0;d<dim;d++)								// check for asymmetric periodic boundary condition
			if(wlist[2*d]->type=='p'&&wlist[2*d+1]->type!='p') {
				printf(" WARNING: only one wall on dimension %i has a periodic boundary condition\n",d);
				warn++; }}

	if(warnptr) *warnptr=warn;
	return error; }


/******************************************************************************/
/******************************** structure setup *****************************/
/******************************************************************************/


/* walladd.  Adds a wall to the system.  If no walls have been added yet, this
allocates the necessary memory.  d is the dimension that the wall bounds,
highside is 0 if the wall is on the low side of the system and 1 if it is on the
high side of the system, pos is the location of the wall in the d dimension, and
type describes the boundary condition (if there arenÕt any surfaces).  Returns 0
for success, 1 for unable to allocate memory, or 2 if the simulation structure
dim element hasnÕt been set up yet. */
int walladd(simptr sim,int d,int highside,double pos,char type) {
	if(!sim->wlist) {
		if(!sim->dim) return 2;
		sim->wlist=wallsalloc(sim->dim);
		if(!sim->wlist) return 1; }
	d=2*d+highside;
	sim->wlist[d]->pos=pos;
	sim->wlist[d]->type=type;
	if(sim->boxs && sim->boxs->condition>SClists) sim->boxs->condition=SClists;
	return 0; }


/******************************************************************************/
/*************************** core simulation functions ************************/
/******************************************************************************/


/* checkwalls_threaded does the reflection, wrap-around, or absorption of molecules at
walls by checking the current position, relative to the wall positions (as well
as a past position for absorbing walls).  Only molecules in live list ll are
checked.  If reborn is 1, only the newly added molecules are checked; if it's 0,
the full list is checked.  It does not reassign the molecules to boxes or sort
the live and dead ones.  It does not matter if molecules are assigned to the
proper boxes or not.  If bptr is NULL, all diffusing molecules are checked,
otherwise only those in box bptr are checked. */
int checkwalls_threaded(simptr sim,int ll,int reborn,boxptr bptr) {//????? changed name and added error code
	int nmol,w,d,m;
	moleculeptr *mlist;
	double pos2,diff,difi,step,**difstep;
	wallptr wptr;

	if(sim->srfss) return 0;
	if(bptr) {
		nmol=bptr->nmol[ll];
		mlist=bptr->mol[ll]; }
	else {
		nmol=sim->mols->nl[ll];
		mlist=sim->mols->live[ll]; }
	if(!reborn) m=0;
	else if(reborn&&!bptr) m=sim->mols->topl[ll];
	else {m=0;printf("SMOLDYN ERROR: in checkwalls, both bptr and reborn are defined");}

	for(w=0;w<2*sim->dim;w++) {
		wptr=sim->wlist[w];
		d=wptr->wdim;
		if(wptr->type=='r'&&wptr->side==0) {			// reflective
			pos2=2*wptr->pos;
			for(m=0;m<nmol;m++)
				if(mlist[m]->pos[d]<wptr->pos) {
					sim->eventcount[ETwall]++;
					mlist[m]->pos[d]=pos2-mlist[m]->pos[d];}}
		else if(wptr->type=='r') {
			pos2=2*wptr->pos;
			for(m=0;m<nmol;m++)
				if(mlist[m]->pos[d]>wptr->pos) {
					sim->eventcount[ETwall]++;
					mlist[m]->pos[d]=pos2-mlist[m]->pos[d];}}
		else if(wptr->type=='p'&&wptr->side==0) {	// periodic
			pos2=wptr->opp->pos-wptr->pos;
			for(m=0;m<nmol;m++)
				if(mlist[m]->pos[d]<wptr->pos) {
					sim->eventcount[ETwall]++;
					mlist[m]->pos[d]+=pos2;
					mlist[m]->posoffset[d]-=pos2; }}
		else if(wptr->type=='p') {
			pos2=wptr->opp->pos-wptr->pos;
			for(m=0;m<nmol;m++)
				if(mlist[m]->pos[d]>wptr->pos) {
					sim->eventcount[ETwall]++;
					mlist[m]->pos[d]+=pos2;
					mlist[m]->posoffset[d]-=pos2; }}
		else if(wptr->type=='a') {								// absorbing
			difstep=sim->mols->difstep;
			for(m=0;m<nmol;m++) {
				diff=wptr->pos-mlist[m]->pos[d];
				difi=wptr->pos-mlist[m]->posx[d];
				step=difstep[mlist[m]->ident][MSsoln];
				if((!(wptr->side)&&diff>0)||(wptr->side&&diff<0)||coinrandD(exp(-2*difi*diff/step/step))) {
					sim->eventcount[ETwall]++;
					molkill(sim,mlist[m],ll,-1); }}}}
	return 0; }


//?????????? below is a new function, above is unchanged (I think these must be identical)
/* checkwalls does the reflection, wrap-around, or absorption of molecules at
walls by checking the current position, relative to the wall positions (as well
as a past position for absorbing walls).  Only molecules in live list ll are
checked.  If reborn is 1, only the newly added molecules are checked; if it's 0,
the full list is checked.  It does not reassign the molecules to boxes or sort
the live and dead ones.  It does not matter if molecules are assigned to the
proper boxes or not.  If bptr is NULL, all diffusing molecules are checked,
otherwise only those in box bptr are checked. */
int checkwalls(simptr sim,int ll,int reborn,boxptr bptr) {
	int nmol,w,d,m;
	moleculeptr *mlist;
	double pos2,diff,difi,step,**difstep;
	wallptr wptr;

	if(sim->srfss) return 0;
	if(bptr) {
		nmol=bptr->nmol[ll];
		mlist=bptr->mol[ll]; }
	else {
		nmol=sim->mols->nl[ll];
		mlist=sim->mols->live[ll]; }
	if(!reborn) m=0;
	else if(reborn&&!bptr) m=sim->mols->topl[ll];
	else {m=0;printf("SMOLDYN ERROR: in checkwalls, both bptr and reborn are defined");}

	for(w=0;w<2*sim->dim;w++) {
		wptr=sim->wlist[w];
		d=wptr->wdim;
		if(wptr->type=='r'&&wptr->side==0) {			// reflective
			pos2=2*wptr->pos;
			for(m=0;m<nmol;m++)
				if(mlist[m]->pos[d]<wptr->pos) {
					sim->eventcount[ETwall]++;
					mlist[m]->pos[d]=pos2-mlist[m]->pos[d];}}
		else if(wptr->type=='r') {
			pos2=2*wptr->pos;
			for(m=0;m<nmol;m++)
				if(mlist[m]->pos[d]>wptr->pos) {
					sim->eventcount[ETwall]++;
					mlist[m]->pos[d]=pos2-mlist[m]->pos[d];}}
		else if(wptr->type=='p'&&wptr->side==0) {	// periodic
			pos2=wptr->opp->pos-wptr->pos;
			for(m=0;m<nmol;m++)
				if(mlist[m]->pos[d]<wptr->pos) {
					sim->eventcount[ETwall]++;
					mlist[m]->pos[d]+=pos2;
					mlist[m]->posoffset[d]-=pos2; }}
		else if(wptr->type=='p') {
			pos2=wptr->opp->pos-wptr->pos;
			for(m=0;m<nmol;m++)
				if(mlist[m]->pos[d]>wptr->pos) {
					sim->eventcount[ETwall]++;
					mlist[m]->pos[d]+=pos2;
					mlist[m]->posoffset[d]-=pos2; }}
		else if(wptr->type=='a') {								// absorbing
			difstep=sim->mols->difstep;
			for(m=0;m<nmol;m++) {
				diff=wptr->pos-mlist[m]->pos[d];
				difi=wptr->pos-mlist[m]->posx[d];
				step=difstep[mlist[m]->ident][MSsoln];
				if((!(wptr->side)&&diff>0)||(wptr->side&&diff<0)||coinrandD(exp(-2*difi*diff/step/step))) {
					sim->eventcount[ETwall]++;
					molkill(sim,mlist[m],ll,-1); }}}}
	return 0; }



