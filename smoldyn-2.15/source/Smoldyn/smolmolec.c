/* Steven Andrews, started 10/22/01.
This is a library of functions for the Smoldyn program.  See documentation
called Smoldyn_doc1.doc and Smoldyn_doc2.doc.
Copyright 2003-2007 by Steven Andrews.  This work is distributed under the terms
of the Gnu Lesser General Public License (LGPL). */

#include <stdio.h>
#include <limits.h>
#include <math.h>
#include <string.h>
#include <ctype.h>
#include <float.h>
#include "math2.h"
#include "random2.h"
#include "Rn.h"
#include "RnSort.h"
#include "smoldyn.h"
#include "smoldyn_config.h"

#ifdef THREADING
#include <pthread.h>
#endif

#define CHECK(A) if(!(A)) goto failure; else (void)0
#define CHECKS(A,B) if(!(A)) {strncpy(erstr,B,STRCHAR-1);erstr[STRCHAR-1]='\0';goto failure;} else (void)0


/******************************************************************************/
/*********************************** Molecules ********************************/
/******************************************************************************/


/******************************************************************************/
/********************************* enumerated types ***************************/
/******************************************************************************/


/* molstring2ms.  Returns the enumerated molecule state value, given a string
input.  Permitted input strings are “solution”, “front”, “back”, “up”, “down”,
“fsoln”, “bsoln”, and “all”.  Returns MSnone if input is “none” or is not
recognized. */
enum MolecState molstring2ms(char *string) {
	enum MolecState ans;

	if(!strcmp(string,"solution")) ans=MSsoln;
	else if(!strcmp(string,"fsoln")) ans=MSsoln;
	else if(!strcmp(string,"soln")) ans=MSsoln;
	else if(!strcmp(string,"aq")) ans=MSsoln;
	else if(!strcmp(string,"front")) ans=MSfront;
	else if(!strcmp(string,"back")) ans=MSback;
	else if(!strcmp(string,"up")) ans=MSup;
	else if(!strcmp(string,"down")) ans=MSdown;
	else if(!strcmp(string,"bsoln")) ans=MSbsoln;
	else if(!strcmp(string,"all")) ans=MSall;
	else ans=MSnone;
	return ans; }


/* molms2string.  Returns the string that corresponds to the enumerated molecule
state ms in string, which must be pre-allocated.  Also, the address of string
is returned to allow for function nesting. */
char *molms2string(enum MolecState ms,char *string) {
	if(ms==MSsoln) strcpy(string,"solution");
	else if(ms==MSfront) strcpy(string,"front");
	else if(ms==MSback) strcpy(string,"back");
	else if(ms==MSup) strcpy(string,"up");
	else if(ms==MSdown) strcpy(string,"down");
	else if(ms==MSbsoln) strcpy(string,"bsoln");
	else if(ms==MSall) strcpy(string,"all");
	else if(ms==MSsome) strcpy(string,"some");
	else strcpy(string,"none");
	return string; }


/* molstring2mlt.  Returns the enumerated molecule list type, given a string
input.  Permitted input strings are “system” and “port”.  Returns MLTnone for all
other input. */
enum MolListType molstring2mlt(char *string) {
	enum MolListType ans;

	if(!strcmp(string,"system")) ans=MLTsystem;
	else if(!strcmp(string,"port")) ans=MLTport;
	else ans=MLTnone;
	return ans; }


/* molmlt2string.  Returns the string that corresponds to the enumerated molecule
list type mlt.  The string needs to be pre-allocaed; it is returned to allow
function nesting. */
char *molmlt2string(enum MolListType mlt,char *string) {
	if(mlt==MLTsystem) strcpy(string,"system");
	else if(mlt==MLTport) strcpy(string,"port");
	else strcpy(string,"none");
	return string; }


/******************************************************************************/
/****************************** low level utilities ***************************/
/******************************************************************************/


/* readmolname.  Read the first word of string str to parse the molecule name
and state.  If msptr is not NULL, the state is returned in msptr; the name is
returned normally.  The input format for the first word is either just name, in
which case msptr is returned pointing to MSall, or is name(state), in which case
msptr is returned pointing to whatever state is requested; “none” is not a
permitted state.  There are many possible negative return values: -1 implies
that the string cannot be read, -2 implies that the close parenthesis is missing
or in the wrong place, -3 implies that the state cannot be read, -4 implies that
the molecule name is unknown, and -5 implies that the molecule name is “all”. */
int readmolname(simptr sim,char *str,enum MolecState *msptr) {
	char nm[STRCHAR],*pareno,*parenc;
	int itct,i;
	enum MolecState ms;

	if(!str) return -1;
	itct=sscanf(str,"%s",nm);
	if(itct!=1) return -1;	// cannot read name
	pareno=strchr(nm,'(');
	if(pareno) {
		*pareno='\0';
		pareno++;
		parenc=strrchr(pareno,')');
		if(parenc && *(parenc+1)=='\0') *parenc='\0';
		else return -2;				// improper close parenthesis
		ms=molstring2ms(pareno);
		if(ms==MSnone) return -3; }		// cannot read state
	else ms=MSsoln;
	if(!strcmp(nm,"all")) i=-5;		// all
	else {
		i=stringfind(sim->mols->spname,sim->mols->nspecies,nm);
		if(i<0) return -4; }		// unknown molecule name
	if(msptr) *msptr=ms;
	return i; }


/* molpos2string. */
char *molpos2string(simptr sim,moleculeptr mptr,char *string) {
	int d,dim,done,p,tryagain,count;
	char *line2;
	double newpos[DIMMAX],crosspt[DIMMAX],dist;
	boxptr bptr;
	panelptr pnl;

	dim=sim->dim;
	done=0;
	dist=0;
	count=0;

	line2=string;												// write position to string
	for(d=0;d<dim;d++) {
		sprintf(line2," %g",mptr->pos[d]);
		line2+=strlen(line2); }

	if(!sim->srfss) done=1;
	while(!done) {
		line2=string;											// read in written position
		for(d=0;d<dim;d++) {
			sscanf(line2,"%lg",&newpos[d]);
			line2=strnword(line2,2); }

		tryagain=0;
		bptr=pos2box(sim,newpos);
		if(bptr!=pos2box(sim,mptr->pos)) tryagain=1;		// check for same box
		for(p=0;p<bptr->npanel && tryagain==0;p++) {		// check for no panels crossed
			pnl=bptr->panel[p];
			if(mptr->pnl!=pnl && lineXpanel(mptr->pos,newpos,pnl,dim,crosspt,NULL,NULL,NULL,NULL)) tryagain=1; }
		if(!tryagain) done=1;

		if(!done) {
			if(++count>50) {
				printf("WARNING: unable to write %s molecule position (%s) on the correct side of all surfaces\n",sim->mols->spname[mptr->ident],string);
				return string; }

			if(dist==0) {
				for(d=0;d<dim;d++) dist+=(newpos[d]-mptr->pos[d])*(newpos[d]-mptr->pos[d]);
				dist=50*sqrt(dist); }

			line2=string;												// write position to string
			for(d=0;d<dim;d++) {
				sprintf(line2," %g",mptr->pos[d]+unirandCCD(-dist,dist));
				line2+=strlen(line2); }}}
		
		return string; }


/* molchangeident.  Changes the identity or state of a molecule that is
currently in the system.  mptr is a pointer to the molecule and ll is the list
that it is currently listed in (probably equal to mptr->list, but not
necessarily).  If it is known, enter the index of the molecule in the master
list (i.e. not a box list) in m; if it’s unknown set m to -1.  If it is to be
bound to a panel, or if it was on a panel, enter the panel in pnl (both are not
possible since a molecule can’t hop from one panel to another with this
function).  This function sets some parameters of the molecule structure, fixes
the location as needed, and, if appropriate, updates sortl to indicate to
molsort that sorting is needed. */
void molchangeident(simptr sim,moleculeptr mptr,int ll,int m,int i,enum MolecState ms,panelptr pnl) {
	int dim,ll2;
	double epsilon;

	if(i==0) {
		molkill(sim,mptr,ll,m);
		return; }

	dim=sim->dim;
	epsilon=sim->srfss?sim->srfss->epsilon:0;

	mptr->ident=i;
	mptr->mstate=ms;
	if(ms==MSsoln) mptr->pnl=NULL;
	else mptr->pnl=pnl;

	if(ms==MSsoln && !pnl);												// soln -> soln
	else if(ms==MSsoln) {													// surf -> front soln
		fixpt2panel(mptr->posx,pnl,dim,PFfront,epsilon); }
	else if(ms==MSbsoln) {												// surf -> back soln
		mptr->mstate=MSsoln;
		fixpt2panel(mptr->posx,pnl,dim,PFback,epsilon); }
	else if(ms==MSfront)													// any -> front surf
		fixpt2panel(mptr->pos,pnl,dim,PFfront,epsilon);
	else if(ms==MSback)														// any -> back surf
		fixpt2panel(mptr->pos,pnl,dim,PFback,epsilon);
	else																					// any -> up or down
		fixpt2panel(mptr->pos,pnl,dim,PFnone,epsilon);

	ll2=sim->mols->listlookup[i][ms];
	if(ll2!=ll) {
		mptr->list=ll2;
		if(m<0) sim->mols->sortl[ll]=0;
		else if(m<sim->mols->sortl[ll]) sim->mols->sortl[ll]=m; }
	return; }


/* molssetgausstable */
int molssetgausstable(molssptr mols,int size) {
	double *newtable;

	if(!mols) return 2;
	if(mols->ngausstbl>0 && (mols->ngausstbl==size || size==-1)) return 0;
	if(size<1) size=4096;
	else if(!is2ton(size)) return 3;

	newtable=(double*)calloc(size,sizeof(double));
	if(!newtable) return 1;
	randtableD(newtable,size,1);
	randshuffletableD(newtable,size);

	if(mols->gausstbl) free(mols->gausstbl);
	mols->ngausstbl=size;
	mols->gausstbl=newtable;
	return 0; }



/* molssetdifc.  Sets the diffusion coefficient for molecule ident and state ms
to difc.  If ident is negative, this set the diffusion coefficient for all
identities; if ms is MSall, this sets the diffusion coefficient for all states.
Either or both “all” conditions are permitted.  This does not update rms step
sizes or reaction rates. */
void molsetdifc(simptr sim,int ident,enum MolecState ms,double difc) {
	int ilo,ihi,i;
	enum MolecState mslo,mshi;

	if(ms==MSbsoln) ms=MSsoln;
	else if(ms==MSnone) return;
	if(ident>=0) ihi=(ilo=ident)+1;
	else {ilo=1;ihi=sim->mols->nspecies;}
	if(ms!=MSall) mshi=(MolecState)((mslo=ms)+1);
	else {mslo=(MolecState)0;mshi=(MolecState)MSMAX;}

	for(i=ilo;i<ihi;i++)
		for(ms=mslo;ms<mshi;ms=(MolecState)(ms+1))
			sim->mols->difc[i][ms]=difc;
	molsetcondition(sim->mols,SCparams,0);
	return; }


/* molsetdifm.  Sets the diffusion matrix for molecule ident and state ms to
difm.  Any required matrices that were not allocated previously are allocated
here.  If ident is negative, this set the diffusion matrix for all identities;
if ms is MSall, this sets the diffusion matrix for all states.  Either or both
“all” conditions are permitted.  This returns 0 for successful operation and 1
for failure to allcate memory.  This updates the isotropic diffusion coefficient
but does not update rms step sizes or reaction rates. */
int molsetdifm(simptr sim,int ident,enum MolecState ms,double *difm) {
	int ilo,ihi,i,d,dim;
	enum MolecState mslo,mshi;
	double *difmat,dm2[DIMMAX*DIMMAX];

	dim=sim->dim;
	if(ms==MSbsoln) ms=MSsoln;
	else if(ms==MSnone) return 0;
	if(!difm) return 0;
	if(ident>=0) ihi=(ilo=ident)+1;
	else {ilo=1;ihi=sim->mols->nspecies;}
	if(ms!=MSall) mshi=(MolecState)((mslo=ms)+1);
	else {mslo=(MolecState)0;mshi=(MolecState)MSMAX;}

	for(i=ilo;i<ihi;i++)
		for(ms=mslo;ms<mshi;ms=(MolecState)(ms+1)) {
			difmat=sim->mols->difm[i][ms];
			if(!difmat) {
				difmat=(double*)calloc(sim->dim*sim->dim,sizeof(double));
				if(!difmat) return 1;
				sim->mols->difm[i][ms]=difmat; }
			for(d=0;d<sim->dim*sim->dim;d++)
				difmat[d]=difm[d];
			dotMMD(difmat,difmat,dm2,dim,dim,dim);
			sim->mols->difc[i][ms]=traceMD(dm2,dim)/dim; }
	molsetcondition(sim->mols,SCparams,0);
	return 0; }


/* molsetdrift.  Sets the drift vector for molecule ident and state ms to drift.
Any required vectors that were not allocated previously are allocated here.  If
ident is negative, this sets the drift vector for all identities; if ms is
MSall, this sets the drift vector for all states.  Either or both “all”
conditions are permitted.  This returns 0 for successful operation and 1 for
failure to allcate memory. */
int molsetdrift(simptr sim,int ident,enum MolecState ms,double *drift) {
	int ilo,ihi,i,d,dim;
	enum MolecState mslo,mshi;
	double *driftvect;

	dim=sim->dim;
	if(ms==MSbsoln) ms=MSsoln;
	else if(ms==MSnone) return 0;
	if(!drift) return 0;
	if(ident>=0) ihi=(ilo=ident)+1;
	else {ilo=1;ihi=sim->mols->nspecies;}
	if(ms!=MSall) mshi=(MolecState)((mslo=ms)+1);
	else {mslo=(MolecState)0;mshi=(MolecState)MSMAX;}

	for(i=ilo;i<ihi;i++)
		for(ms=mslo;ms<mshi;ms=(MolecState)(ms+1)) {
			driftvect=sim->mols->drift[i][ms];
			if(!driftvect) {
				driftvect=(double*)calloc(sim->dim,sizeof(double));
				if(!driftvect) return 1;
				sim->mols->drift[i][ms]=driftvect; }
			for(d=0;d<sim->dim;d++)
				driftvect[d]=drift[d]; }
	return 0; }


/* molsetdisplaysize.  Sets the display size for molecule ident and state ms to
dsize.  If ident is negative, this set the display size for all identities; if
ms is MSall, this sets the display size for all states.  Either or both “all”
conditions are permitted. */
void molsetdisplaysize(simptr sim,int ident,enum MolecState ms,double dsize) {
	int ilo,ihi,i;
	enum MolecState mslo,mshi;

	if(ms==MSbsoln) ms=MSsoln;
	else if(ms==MSnone) return;
	if(ident>=0) ihi=(ilo=ident)+1;
	else {ilo=1;ihi=sim->mols->nspecies;}
	if(ms!=MSall) mshi=(MolecState)((mslo=ms)+1);
	else {mslo=(MolecState)0;mshi=(MolecState)MSMAX;}

	for(i=ilo;i<ihi;i++)
		for(ms=mslo;ms<mshi;ms=(MolecState)(ms+1))
			sim->mols->display[i][ms]=dsize;
	return; }


/* molsetcolor.  Sets the color for molecule ident and state ms to the
3-dimensional RGB vector color.  If ident is negative, this set the color for
all identities; if ms is MSall, this sets the color for all states.  Either or
both “all” conditions are permitted. */
void molsetcolor(simptr sim,int ident,enum MolecState ms,double *color) {
	int ilo,ihi,i,col;
	enum MolecState mslo,mshi;

	if(ms==MSbsoln) ms=MSsoln;
	else if(ms==MSnone) return;
	if(ident>=0) ihi=(ilo=ident)+1;
	else {ilo=1;ihi=sim->mols->nspecies;}
	if(ms!=MSall) mshi=(MolecState)((mslo=ms)+1);
	else {mslo=(MolecState)0;mshi=(MolecState)MSMAX;}

	for(i=ilo;i<ihi;i++)
		for(ms=mslo;ms<mshi;ms=(MolecState)(ms+1))
			for(col=0;col<3;col++)
				sim->mols->color[i][ms][col]=color[col];
	return; }


/* molsetlistlookup.  Sets the list lookup table value to live list number ll for
molecule ident and state ms.  Various special codes are possible: ident=-5 implies
all identities, ident=-6 implies all diffusing molecules, and ident=-7 implies all
non-diffusing molecules; ms=MSall implies all states. */
void molsetlistlookup(simptr sim,int ident,enum MolecState ms,int ll) {
	int ilo,ihi,i,skip;
	enum MolecState mslo,mshi;

	if(ms==MSbsoln) ms=MSsoln;
	else if(ms==MSnone) return;
	if(ident>=0) ihi=(ilo=ident)+1;
	else {ilo=1;ihi=sim->mols->nspecies;}
	if(ms!=MSall) mshi=(MolecState)((mslo=ms)+1);
	else {mslo=(MolecState)0;mshi=(MolecState)MSMAX;}

	for(i=ilo;i<ihi;i++)
		for(ms=mslo;ms<mshi;ms=(MolecState)(ms+1)) {
			skip=0;
			if(ident==-6 && sim->mols->difc[i][ms]==0) skip=1;
			else if(ident==-7 && sim->mols->difc[i][ms]>0) skip=1;
			if(!skip) sim->mols->listlookup[i][ms]=ll; }
	return; }


/* molsetexist.  Sets the exist element of the molecule superstructure for
identity ident and state ms to exist; “all” inputs are not permitted. */
void molsetexist(simptr sim,int ident,enum MolecState ms,int exist) {
	if(ms==MSnone) return;
	else if(ms==MSbsoln) ms=MSsoln;
	else if(ms==MSall) {
		for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1)) sim->mols->exist[ident][ms]=exist;
		return; }
	sim->mols->exist[ident][ms]=exist;
	return; }


/* molcount.  Counts the number of molecules of type i and state ms currently in
the simulation.  If bptr is NULL, this considers all molecules in the
simulation, and otherwise it only considers those in the listed box.  If max is
-1 it is ignored, and otherwise the counting stops as soon as max is reached.
Either or both of i and ms can be set to “all”; for i enter a negative number
and for ms enter MSall.  All system type molecule lists and the dead list are
checked, meaning that any porting lists are not included.  If bptr is NULL, this
function returns correct molecule counts whether molecule lists have been sorted
since recent changes or not.  It runs fastest if molecule lists have been sorted.
*/
int molcount(simptr sim,int i,enum MolecState ms,boxptr bptr,int max) {
	int count,ll,nmol,top,m,lllo,llhi;
	moleculeptr *mlist;

	if(!sim->mols) return 0;
	if(i<0 || ms==MSall) {lllo=0;llhi=sim->mols->nlist;}
	else llhi=1+(lllo=sim->mols->listlookup[i][ms]);
	if(max<0) max=INT_MAX;

	count=0;
	for(ll=lllo;ll<llhi;ll++)											// count properly sorted molecules
		if(sim->mols->listtype[ll]==MLTsystem) {
			if(bptr) {
				mlist=bptr->mol[ll];
				top=bptr->nmol[ll]; }
			else {
				mlist=sim->mols->live[ll];
				top=sim->mols->sortl[ll]; }

			if(i<0 && ms==MSall) {
				count+=top; }
			else if(i<0) {
				for(m=0;m<top && count<max;m++) if(mlist[m]->mstate==ms) count++; }
			else if(ms==MSall) {
				for(m=0;m<top && count<max;m++) if(mlist[m]->ident==i) count++; }
			else {
				for(m=0;m<top && count<max;m++) if(mlist[m]->ident==i && mlist[m]->mstate==ms) count++; }}

	if(!bptr) {
		mlist=sim->mols->dead;											// count resurrected molecules
		nmol=sim->mols->nd;
		top=sim->mols->topd;

		if(i<0 && ms==MSall) {
			count+=nmol-top; }
		else if(i<0) {
			for(m=top;m<nmol && count<max;m++) if(mlist[m]->mstate==ms) count++; }
		else if(ms==MSall) {
			for(m=top;m<nmol && count<max;m++) if(mlist[m]->ident==i) count++; }
		else {
			for(m=top;m<nmol && count<max;m++) if(mlist[m]->ident==i && mlist[m]->mstate==ms) count++; }

		for(ll=0;ll<sim->mols->nlist;ll++)					// count molecules that need sorting
			if(sim->mols->listtype[ll]==MLTsystem) {
				mlist=sim->mols->live[ll];
				nmol=sim->mols->nl[ll];
				top=sim->mols->sortl[ll];

				if(i<0 && ms==MSall) {
					for(m=top;m<nmol && count<max;m++) if(mlist[m]->ident!=0) count++; }
				else if(i<0) {
					for(m=top;m<nmol && count<max;m++) if(mlist[m]->ident!=0 && mlist[m]->mstate==ms) count++; }
				else if(ms==MSall) {
					for(m=top;m<nmol && count<max;m++) if(mlist[m]->ident==i) count++; }
				else {
					for(m=top;m<nmol && count<max;m++) if(mlist[m]->ident==i && mlist[m]->mstate==ms) count++; }}}

	return count; }


/* MolCalcDifcSum.  Calculates and returns diffusion coefficient sums. */
double MolCalcDifcSum(simptr sim,int i1,enum MolecState ms1,int i2,enum MolecState ms2) {
	double sum;

	sum=0;
	if(i1) {
		if(ms1>=MSMAX) ms1=MSsoln;
		sum+=sim->mols->difc[i1][ms1]; }
	if(i2) {
		if(ms2>=MSMAX) ms2=MSsoln;
		sum+=sim->mols->difc[i2][ms2]; }
	return sum; }


/******************************************************************************/
/****************************** memory management *****************************/
/******************************************************************************/

/* molalloc allocates and initiallizes a new moleculestruct.  The serial number
is set to 0, the list to -1 (dead list), positional vectors to the origin, the
identity to the empty molecule (0), the state to MSsoln, and box and pnl to NULL.
The molecule is returned unless memory could not be allocated, in which case NULL
is returned. */
moleculeptr molalloc(int dim) {
	moleculeptr mptr;
	int d;

	mptr=NULL;
	CHECK(dim>0);
	CHECK(mptr=(moleculeptr) malloc(sizeof(struct moleculestruct)));
	mptr->serno=0;
	mptr->list=-1;
	mptr->pos=NULL;
	mptr->posx=NULL;
	mptr->via=NULL;
	mptr->posoffset=NULL;
	mptr->ident=0;
	mptr->mstate=MSsoln;
	mptr->box=NULL;
	mptr->pnl=NULL;

	CHECK(mptr->pos=(double*)calloc(dim,sizeof(double)));
	CHECK(mptr->posx=(double*)calloc(dim,sizeof(double)));
	CHECK(mptr->via=(double*)calloc(dim,sizeof(double)));
	CHECK(mptr->posoffset=(double*)calloc(dim,sizeof(double)));
	for(d=0;d<dim;d++)
		mptr->pos[d]=mptr->posx[d]=mptr->via[d]=mptr->posoffset[d]=0;
	return mptr;
 failure:
	molfree(mptr);
	return NULL; }


/* molfree frees the space allocated for a moleculestruct, as well as its position
vectors.  The contents of box and pnl are not freed because they are references,
not owned by the molecule structure. */
void molfree(moleculeptr mptr) {
	if(!mptr) return;
	if(mptr->pos) free(mptr->pos);
	if(mptr->posx) free(mptr->posx);
	if(mptr->posoffset) free(mptr->posoffset);
	if(mptr->via) free(mptr->via);
	free(mptr);
	return; }


/* molssalloc allocates and initiallizes a molecule superstructure.  max empty
molecules are created in the dead list, while the live list element is set to NULL.
max must be at least 1.  The molecule boxes are left as NULLs, and need to be set.
Book keeping elements for the lists are set to their initial values.  The Gaussian
table is left empty; it is filled in in setupmols. */
molssptr molssalloc(int maxspecies) {
	molssptr mols;
	int i;
	enum MolecState ms;

	mols=NULL;
	CHECK(maxspecies>0);
	CHECK(mols=(molssptr) malloc(sizeof(struct molsuperstruct)));

	mols->condition=SCinit;
	mols->sim=NULL;
	mols->maxspecies=maxspecies;
	mols->nspecies=1;
	mols->spname=NULL;
	mols->difc=NULL;
	mols->difstep=NULL;
	mols->difm=NULL;
	mols->drift=NULL;
	mols->display=NULL;
	mols->color=NULL;
	mols->exist=NULL;
	mols->dead=NULL;
	mols->maxd=0;
	mols->nd=0;
	mols->topd=0;
	mols->maxlist=0;
	mols->nlist=0;
	mols->listlookup=NULL;
	mols->listname=NULL;
	mols->listtype=NULL;
	mols->live=NULL;
	mols->maxl=NULL;
	mols->nl=NULL;
	mols->topl=NULL;
	mols->sortl=NULL;
	mols->diffuselist=NULL;
	mols->serno=1;
	mols->ngausstbl=0;
	mols->gausstbl=NULL;
	mols->expand=NULL;

	CHECK(mols->spname=(char**) calloc(maxspecies,sizeof(char*)));
	for(i=0;i<maxspecies;i++) mols->spname[i]=NULL;
	for(i=0;i<maxspecies;i++) {
		CHECK(mols->spname[i]=EmptyString()); }

	strncpy(mols->spname[0],"empty",STRCHAR-1);

	CHECK(mols->difc=(double**) calloc(maxspecies,sizeof(double*)));
	for(i=0;i<maxspecies;i++) mols->difc[i]=NULL;
	for(i=0;i<maxspecies;i++) {
		CHECK(mols->difc[i]=(double*) calloc(MSMAX,sizeof(double)));
		for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1)) mols->difc[i][ms]=0; }

	CHECK(mols->difstep=(double**) calloc(maxspecies,sizeof(double*)));
	for(i=0;i<maxspecies;i++) mols->difstep[i]=NULL;
	for(i=0;i<maxspecies;i++) {
		CHECK(mols->difstep[i]=(double*) calloc(MSMAX,sizeof(double)));
		for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1)) mols->difstep[i][ms]=0; }

	CHECK(mols->difm=(double***) calloc(maxspecies,sizeof(double**)));
	for(i=0;i<maxspecies;i++) mols->difm[i]=NULL;
	for(i=0;i<maxspecies;i++) {
		CHECK(mols->difm[i]=(double**) calloc(MSMAX,sizeof(double*)));
		for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1)) mols->difm[i][ms]=NULL; }

	CHECK(mols->drift=(double***) calloc(maxspecies,sizeof(double**)));
	for(i=0;i<maxspecies;i++) mols->drift[i]=NULL;
	for(i=0;i<maxspecies;i++) {
		CHECK(mols->drift[i]=(double**) calloc(MSMAX,sizeof(double*)));
		for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1)) mols->drift[i][ms]=NULL; }

	CHECK(mols->display=(double**) calloc(maxspecies,sizeof(double*)));
	for(i=0;i<maxspecies;i++) mols->display[i]=NULL;
	for(i=0;i<maxspecies;i++) {
		CHECK(mols->display[i]=(double*) calloc(MSMAX,sizeof(double)));
		for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1)) mols->display[i][ms]=3; }

	CHECK(mols->color=(double ***) calloc(maxspecies,sizeof(double **)));
	for(i=0;i<maxspecies;i++) mols->color[i]=NULL;
	for(i=0;i<maxspecies;i++) {
		CHECK(mols->color[i]=(double**) calloc(MSMAX,sizeof(double*)));
		for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1)) mols->color[i][ms]=NULL;
		for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1)) {
			CHECK(mols->color[i][ms]=(double*) calloc(3,sizeof(double)));
			mols->color[i][ms][0]=mols->color[i][ms][1]=mols->color[i][ms][2]=0; }}

	CHECK(mols->exist=(int**) calloc(maxspecies,sizeof(int*)));
	for(i=0;i<maxspecies;i++) mols->exist[i]=NULL;
	for(i=0;i<maxspecies;i++) {
		CHECK(mols->exist[i]=(int*) calloc(MSMAX,sizeof(int)));
		for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1)) mols->exist[i][ms]=0; }

	CHECK(mols->listlookup=(int**) calloc(maxspecies,sizeof(int*)));
	for(i=0;i<maxspecies;i++) mols->listlookup[i]=NULL;
	for(i=0;i<maxspecies;i++) {
		CHECK(mols->listlookup[i]=(int*) calloc(MSMAX,sizeof(int)));
		for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1)) mols->listlookup[i][ms]=-1; }

	CHECK(mols->expand=(int*) calloc(maxspecies,sizeof(int)));
	for(i=0;i<maxspecies;i++) mols->expand[i]=0;

	return mols;

 failure:
	molssfree(mols);
	return NULL; }


/* mollistalloc.  Allocates maxlist new live lists of list type mlt for the
already existing molecule superstructure mols.  This works whether there were
already were live lists or not.  Returns the index of the first live list that
was just added for success or a negative code for failure: -1 for out of memory,
-2 for a negative maxlist input value, or -3 for a NULL mols input.  The maxlist
element of the superstructure is updated.  The nlist element of the
superstructure is unchanged. */
int mollistalloc(molssptr mols,int maxlist,enum MolListType mlt) {
	int *maxl,*nl,*topl,*sortl,*diffuselist,ll,m;
	moleculeptr **live,mptr;
	char **listname;
	enum MolListType *listtype;

	if(maxlist<=0) return -2;
	if(!mols) return -3;
	maxlist+=mols->maxlist;

	listname=NULL;							// allocate new arrays
	listtype=NULL;
	live=NULL;
	maxl=NULL;
	nl=NULL;
	topl=NULL;
	sortl=NULL;
	diffuselist=NULL;

	CHECK(listname=(char**)calloc(maxlist,sizeof(char*)));
	for(ll=0;ll<maxlist;ll++) listname[ll]=NULL;

	CHECK(listtype=(enum MolListType*)calloc(maxlist,sizeof(enum MolListType)));
	for(ll=0;ll<maxlist;ll++) listtype[ll]=MLTnone;

	CHECK(live=(moleculeptr**)calloc(maxlist,sizeof(moleculeptr*)));
	for(ll=0;ll<maxlist;ll++) live[ll]=NULL;

	CHECK(maxl=(int*)calloc(maxlist,sizeof(int)));
	for(ll=0;ll<maxlist;ll++) maxl[ll]=0;

	CHECK(nl=(int*)calloc(maxlist,sizeof(int)));
	for(ll=0;ll<maxlist;ll++) nl[ll]=0;

	CHECK(topl=(int*)calloc(maxlist,sizeof(int)));
	for(ll=0;ll<maxlist;ll++) topl[ll]=0;

	CHECK(sortl=(int*)calloc(maxlist,sizeof(int)));
	for(ll=0;ll<maxlist;ll++) sortl[ll]=0;

	CHECK(diffuselist=(int*)calloc(maxlist,sizeof(int)));
	for(ll=0;ll<maxlist;ll++) diffuselist[ll]=0;

	for(ll=0;ll<mols->maxlist;ll++) {			// copy over existing portions
		listname[ll]=mols->listname[ll];
		listtype[ll]=mols->listtype[ll];
		live[ll]=mols->live[ll];
		maxl[ll]=mols->maxl[ll];
		nl[ll]=mols->nl[ll];
		topl[ll]=mols->topl[ll];
		sortl[ll]=mols->sortl[ll];
		diffuselist[ll]=mols->diffuselist[ll]; }

	for(ll=mols->maxlist;ll<maxlist;ll++) {					// listnames and listtypes
		CHECK(listname[ll]=EmptyString());
		listtype[ll]=mlt; }

	for(ll=mols->maxlist;ll<maxlist;ll++) maxl[ll]=1;			// calculate maxl
	for(m=mols->topd;m<mols->nd;m++) {
		mptr=mols->dead[m];
		if(mptr && mptr->list>=mols->maxlist && mptr->list<maxlist) maxl[mptr->list]++; }
	for(ll=mols->maxlist;ll<maxlist;ll++) {
		maxl[ll]*=2;
		if(maxl[ll]>mols->maxd) maxl[ll]=mols->maxd; }

	for(ll=mols->maxlist;ll<maxlist;ll++) {			// allocate live lists
		CHECK(live[ll]=(moleculeptr*)calloc(maxl[ll],sizeof(moleculeptr)));
		for(m=0;m<maxl[ll];m++) live[ll][m]=NULL; }

	if(mols->maxlist) {										// free any old lists
		free(mols->listname);
		free(mols->listtype);
		free(mols->live);
		free(mols->maxl);
		free(mols->nl);
		free(mols->topl);
		free(mols->sortl);
		free(mols->diffuselist); }
	ll=mols->maxlist;
	mols->maxlist=maxlist;									// store new lists
	mols->listname=listname;
	mols->listtype=listtype;
	mols->live=live;
	mols->maxl=maxl;
	mols->nl=nl;
	mols->topl=topl;
	mols->sortl=sortl;
	mols->diffuselist=diffuselist;
	return ll;

 failure:
	if(listname)
		for(ll=0;ll<maxlist;ll++) free(listname[ll]);
	free(listname);
	free(listtype);
	if(live)
		for(ll=mols->maxlist;ll<maxlist;ll++) free(live[ll]);
	free(live);
	free(maxl);
	free(nl);
	free(topl);
	free(sortl);
	free(diffuselist);
	return -1; }


/* molexpandlist.  Expands molecule list.  If ll is negative, the dead list is
expanded and otherwise live list number ll is expanded.  If nspaces is negative,
the list size is doubled and otherwise nspaces spaces are added to the list.  The
first nmolecs of these spaces are filled with new dead molecules (mptr->list
element set to -1).  Because this shouldn’t normally be called with ll>0 and
nmolecs>0, error code 2 is returned if this happens.  This returns 0 for success,
1 for out of memory during list expansion, 2 for illegal inputs, 3 for more
molecules are being created than will fit in the list even after expansion, and 4
for out of memory during molecule allocation. */
int molexpandlist(molssptr mols,int dim,int ll,int nspaces,int nmolecs) {
	moleculeptr *newlist,*oldlist;
	int m,nold,maxold,maxnew;

	if(!mols || ll>=mols->nlist) return 2;
	if(ll>0 && nmolecs>0) return 2;

	maxold=ll<0?mols->maxd:mols->maxl[ll];
	nold=ll<0?mols->nd:mols->nl[ll];
	oldlist=ll<0?mols->dead:mols->live[ll];

	maxnew=nspaces>0?maxold+nspaces:2*maxold+1;
	if(nold+nmolecs>maxnew) return 3;

	newlist=(moleculeptr*)calloc(maxnew,sizeof(moleculeptr));
	if(!newlist) return 1;
	for(m=0;m<nold;m++) newlist[m]=oldlist[m];
	for(;m<maxnew;m++) newlist[m]=NULL;
	if(ll<0) {
		free(mols->dead);
		mols->dead=newlist;
		mols->maxd=maxnew; }
	else {
		free(mols->live[ll]);
		mols->live[ll]=newlist;
		mols->maxl[ll]=maxnew; }

	if(nmolecs) {
		if(ll<0 && mols->topd!=mols->nd) {
			for(m=mols->topd;m<mols->nd;m++) {
				newlist[m+nmolecs]=newlist[m];
				newlist[m]=NULL; }
			nold=mols->topd; }
		for(m=nold;m<nold+nmolecs;m++) {
			newlist[m]=molalloc(dim);
			if(!newlist[m]) return 4;
			if(ll<0) {
				mols->nd++;
				mols->topd++; }
			else mols->nl[ll]++; }}
	return 0; }


/* molssfree frees both a superstructure of molecules and all the molecules in
all its lists. */
void molssfree(molssptr mols) {
	int m,ll,i,maxspecies;
	enum MolecState ms;

	if(!mols) return;
	maxspecies=mols->maxspecies;

	free(mols->expand);

	free(mols->gausstbl);

	for(ll=0;ll<mols->maxlist;ll++) {
		if(mols->listname) free(mols->listname[ll]);
		if(mols->live && mols->live[ll]) {
			for(m=0;m<mols->nl[ll];m++) molfree(mols->live[ll][m]);
			free(mols->live[ll]); }}
	free(mols->diffuselist);
	free(mols->sortl);
	free(mols->topl);
	free(mols->nl);
	free(mols->maxl);
	free(mols->live);
	free(mols->listtype);
	free(mols->listname);

	if(mols->listlookup) {
		for(i=0;i<maxspecies;i++) free(mols->listlookup[i]);
		free(mols->listlookup); }

	if(mols->exist) {
		for(i=0;i<maxspecies;i++) free(mols->exist[i]);
		free(mols->exist); }

	if(mols->dead) {
		for(m=0;m<mols->nd;m++) molfree(mols->dead[m]);
		free(mols->dead); }

	if(mols->color) {
		for(i=0;i<maxspecies;i++)
			if(mols->color[i]) {
				for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1)) free(mols->color[i][ms]);
				free(mols->color[i]); }
		free(mols->color); }

	if(mols->display) {
		for(i=0;i<maxspecies;i++) free(mols->display[i]);
		free(mols->display); }

	if(mols->drift) {
		for(i=0;i<maxspecies;i++)
			if(mols->drift[i]) {
				for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1)) free(mols->drift[i][ms]);
				free(mols->drift[i]); }
		free(mols->drift); }

	if(mols->difm) {
		for(i=0;i<maxspecies;i++)
			if(mols->difm[i]) {
				for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1)) free(mols->difm[i][ms]);
				free(mols->difm[i]); }
		free(mols->difm); }

	if(mols->difstep) {
		for(i=0;i<maxspecies;i++) free(mols->difstep[i]);
		free(mols->difstep); }

	if(mols->difc) {
		for(i=0;i<maxspecies;i++) free(mols->difc[i]);
		free(mols->difc); }

	if(mols->spname) {
		for(i=0;i<maxspecies;i++) free(mols->spname[i]);
		free(mols->spname); }

	free(mols);
	return; }


/******************************************************************************/
/*************************** data structure output ****************************/
/******************************************************************************/

/* molssoutput prints all the parameters in a molecule superstructure including:
molecule diffusion constants, rms step lengths, colors, and display sizes; and
dead list and live list sizes and indicies. */
void molssoutput(simptr sim) {
	int nspecies,i,ll,same,sum;
	molssptr mols;
	char string[STRCHAR];
	double maxstep;
	enum MolecState ms;

	printf("MOLECULE PARAMETERS\n");
	if(!sim || !sim->mols) {
		printf(" No molecule superstructure defined\n\n");
		return; }
	mols=sim->mols;
	nspecies=mols->nspecies;

	printf(" Next molecule serial number: %li\n",mols->serno);
	if(mols->gausstbl) printf(" Table for Gaussian distributed random numbers has %i values\n",mols->ngausstbl);
	else printf(" Table for Gaussian distributed random numbers has not been set up\n");

	printf(" %i species of %i allocated:\n",mols->nspecies-1,mols->maxspecies-1);
	for(i=1;i<nspecies;i++) printf("%c %s",i==1?' ':',',mols->spname[i]);
	printf("\n");

	maxstep=0;
	for(i=1;i<nspecies;i++) {
		printf(" Molecule %s:\n",mols->spname[i]);
		printf("  states:");
		sum=0;
		for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1))
			if(mols->exist[i][ms]) {
				sum++;
				printf(" %s",molms2string(ms,string)); }
		if(!sum) printf(" none");
		printf("\n");
		same=1;
		for(ms=(MolecState)0;ms<MSMAX && same;ms=(MolecState)(ms+1)) {
			if(mols->difc[i][ms]!=mols->difc[i][MSsoln]) same=0;
			if(mols->difm[i][ms] && !mols->difm[i][MSsoln]) same=0;
			if(!mols->difm[i][ms] && mols->difm[i][MSsoln]) same=0;
			if(mols->drift[i][ms] && !mols->drift[i][MSsoln]) same=0;
			if(!mols->drift[i][ms] && mols->drift[i][MSsoln]) same=0;
			if(mols->listlookup[i][ms]!=mols->listlookup[i][MSsoln]) same=0; }
		if(same) {
			if(mols->difstep[i][MSsoln]>maxstep) maxstep=mols->difstep[i][MSsoln];
			printf("  all states: difc=%g, rms step=%g",mols->difc[i][MSsoln],mols->difstep[i][MSsoln]);
			if(mols->difm[i][MSsoln]) printf(" (anisotropic)");
			if(mols->drift[i][MSsoln]) printf(" (drift)");
			printf(", list=%s, number=%i\n",mols->listname[mols->listlookup[i][MSsoln]],molcount(sim,i,MSall,NULL,-1)); }
		else {
			for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1))
				if(mols->exist[i][ms]) {
					if(mols->difstep[i][ms]>maxstep) maxstep=mols->difstep[i][ms];
					printf("  %s: difc=%g, rms step=%g",molms2string(ms,string),mols->difc[i][ms],mols->difstep[i][ms]);
					if(mols->difm[i][ms]) printf(" (anisotropic)");
					if(mols->drift[i][ms]) printf(" (drift)");
					printf(", list=%s, number=%i\n",mols->listname[mols->listlookup[i][ms]],molcount(sim,i,ms,NULL,-1)); }}

		if(sim->graphss) {
			same=1;
			for(ms=(MolecState)0;ms<MSMAX && same;ms=(MolecState)(ms+1)) {
				if(mols->display[i][ms]!=mols->display[i][MSsoln]) same=0;
				if(mols->color[i][ms][0]!=mols->color[i][MSsoln][0]) same=0;
				if(mols->color[i][ms][1]!=mols->color[i][MSsoln][1]) same=0;
				if(mols->color[i][ms][2]!=mols->color[i][MSsoln][2]) same=0; }
			if(same) {
				printf("  all states:");
				if(mols->display[i][MSsoln])
					printf(" color= %g,%g,%g, size=%g\n",mols->color[i][MSsoln][0],mols->color[i][MSsoln][1],mols->color[i][MSsoln][2],mols->display[i][MSsoln]);
				else printf(" not displayed to graphics\n"); }
			else {
				for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1))
					if(mols->exist[i][ms]) {
						printf("  %s:",molms2string(ms,string));
						if(mols->display[i][ms])
							printf(" color= %g,%g,%g, display size= %g\n",mols->color[i][ms][0],mols->color[i][ms][1],mols->color[i][ms][2],mols->display[i][ms]);
						else printf(" not displayed to graphics\n"); }}}}

	if(mols->dead==NULL) printf(" No dead list allocated\n");
	printf(" Dead list: allocated size=%i, number of molecules=%i",mols->maxd,mols->nd);
	if(mols->topd!=mols->nd) printf(", top value=%i",mols->topd);
	printf("\n");
	printf(" Number of live lists: %i\n",mols->nlist);
	printf(" Live lists:\n");
	for(ll=0;ll<mols->nlist;ll++) {
		if(mols->live[ll]==NULL) printf("  List %i is not allocated\n",ll);
		printf("  %s: type=%s, allocated size=%i, number of molecules=%i",mols->listname[ll],molmlt2string(mols->listtype[ll],string),mols->maxl[ll],mols->nl[ll]);
		if(mols->topl[ll]!=mols->nl[ll] && !mols->topl!=0) printf(", top value=%i",mols->topl[ll]);
		if(mols->sortl[ll]!=mols->nl[ll]) printf(", sort value=%i",mols->sortl[ll]);
		printf("\n"); }

	printf(" Diffusion molecule lists:");
	for(ll=0;ll<mols->nlist;ll++)
		if(mols->diffuselist[ll]) printf(" %s",mols->listname[ll]);
	printf("\n");

	printf(" Overall spatial resolution: %g\n",maxstep);
	printf("\n");
	return; }


/* writemols.  Writes information about the molecule superstructure. */
void writemols(simptr sim,FILE *fptr) {
	int i,d,ll,dim;
	char **spname,string[STRCHAR];
	enum MolecState ms;
	molssptr mols;
	double val0,val1,val2;

	mols=sim->mols;
	if(!mols) return;
	dim=sim->dim;
	spname=mols->spname;
	fprintf(fptr,"# Molecule parameters\n");

	fprintf(fptr,"max_species %i\n",mols->maxspecies-1);
	for(i=1;i<mols->nspecies;i++) fprintf(fptr,"species %s\n",spname[i]);
	fprintf(fptr,"\n");
	fprintf(fptr,"max_mol %i\n",sim->mols->maxd);
	fprintf(fptr,"gauss_table_size %i\n\n",mols->ngausstbl);

	for(ll=0;ll<mols->nlist;ll++)
		if(mols->listtype[ll]==MLTsystem)
			fprintf(fptr,"molecule_lists %s\n",mols->listname[ll]);
	fprintf(fptr,"\n");
	
	for(i=1;i<mols->nspecies;i++) {
		val0=mols->difc[i][0];
		for(ms=(MolecState)1;ms<MSMAX && mols->difc[i][ms]==val0;ms=(MolecState)(ms+1));
		if(ms==MSMAX) fprintf(fptr,"difc %s(all) %g\n",spname[i],val0);
		else {
			for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1))
				if(mols->difc[i][ms]>0)
					fprintf(fptr,"difc %s(%s) %g\n",spname[i],molms2string(ms,string),mols->difc[i][ms]); }
		
		for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1)) {
			if(mols->difm[i][ms]) {
				fprintf(fptr,"difm %s(%s)",spname[i],molms2string(ms,string));
				for(d=0;d<dim*dim;d++)
					fprintf(fptr," %g",mols->difm[i][ms][d]);
				fprintf(fptr,"\n"); }}
		
		for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1)) {
			if(mols->drift[i][ms]) {
				fprintf(fptr,"drift %s(%s)",spname[i],molms2string(ms,string));
				for(d=0;d<dim;d++)
					fprintf(fptr," %g",mols->drift[i][ms][d]);
				fprintf(fptr,"\n"); }}
		
		ll=mols->listlookup[i][0];
		for(ms=(MolecState)1;ms<MSMAX && mols->listlookup[i][ms]==ll;ms=(MolecState)(ms+1));
		if(ms==MSMAX) fprintf(fptr,"mol_list %s(all) %s\n",spname[i],mols->listname[ll]);
		else {
			for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1))
				fprintf(fptr,"mol_list %s(%s) %s\n",spname[i],molms2string(ms,string),mols->listname[mols->listlookup[i][ms]]); }
		
		val0=mols->display[i][0];
		for(ms=(MolecState)1;ms<MSMAX && mols->display[i][ms]==val0;ms=(MolecState)(ms+1));
		if(ms==MSMAX) fprintf(fptr,"display_size %s(all) %g\n",spname[i],val0);
		else {
			for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1))
				fprintf(fptr,"display_size %s(%s) %g\n",spname[i],molms2string(ms,string),mols->display[i][ms]); }
		
		val0=mols->color[i][0][0];
		val1=mols->color[i][0][1];
		val2=mols->color[i][0][2];
		for(ms=(MolecState)1;ms<MSMAX && mols->color[i][ms][0]==val0 && mols->color[i][ms][1]==val1 && mols->color[i][ms][2]==val2;ms=(MolecState)(ms+1));
		if(ms==MSMAX) fprintf(fptr,"color %s(all) %g %g %g\n",spname[i],val0,val1,val2);
		else {
			for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1))
				fprintf(fptr,"color %s(%s) %g %g %g\n",spname[i],molms2string(ms,string),mols->color[i][ms][0],mols->color[i][ms][1],mols->color[i][ms][2]); }
		fprintf(fptr,"\n"); }
	return; }


/* writemolecules.  Writes information about individual molecules. */
void writemolecules(simptr sim,FILE *fptr) {
	int m,ll;
	char **spname,string[STRCHAR];
	molssptr mols;
	moleculeptr mptr;

	mols=sim->mols;
	if(!mols) return;
	spname=mols->spname;
	fprintf(fptr,"# Individual molecules\n");
	
	for(ll=0;ll<mols->nlist;ll++)
		if(mols->listtype[ll]==MLTsystem)
			for(m=0;m<mols->nl[ll];m++) {
				mptr=mols->live[ll][m];
				if(mptr->ident>0) {
					if(mptr->mstate==MSsoln)
						fprintf(fptr,"mol 1 %s",spname[mptr->ident]);
					else {
						fprintf(fptr,"surface_mol 1 %s(%s) %s",spname[mptr->ident],molms2string(mptr->mstate,string),mptr->pnl->srf->sname);
						fprintf(fptr," %s %s",surfps2string(mptr->pnl->ps,string),mptr->pnl->pname); }
					fprintf(fptr,"%s\n",molpos2string(sim,mptr,string)); }}

	return; }


/* checkmolparams.  Checks some parameters in a molecule superstructure and
substructures to make sure that they are legitimate and reasonable.  Prints
error and warning messages to the display.  Returns the total number of errors
and, if warnptr is not NULL, the number of warnings in warnptr. */
int checkmolparams(simptr sim,int *warnptr) {
	int dim,i,nspecies,m,ll,warn,error,sum;
	molssptr mols;
	moleculeptr mptr;
	wallptr *wlist;
	char **spname,string[STRCHAR];
	double m2[DIMMAX*DIMMAX],diag;
	enum MolecState ms;

	error=warn=0;
	mols=sim->mols;
	if(!mols) {
		if(warnptr) *warnptr=warn;
		return 0; }
	dim=sim->dim;
	nspecies=mols->nspecies;
	wlist=sim->wlist;
	spname=mols->spname;

	if(mols->condition!=SCok) {
		warn++;
		printf(" WARNING: molecule structure %s\n",simsc2string(mols->condition,string)); }

	for(ll=0;ll<mols->nlist;ll++) {				// check molecule list sorting
		for(m=0;m<mols->nl[ll];m++) {
			mptr=mols->live[ll][m];
			if(!mptr) {error++;printf(" SMOLDYN BUG: NULL molecule in live list %i at %i\n",ll,m);}
			else if(mptr->list!=ll) {warn++;printf(" WARNING: mis-sorted molecule in live list %i at %i\n",ll,m);}
			else if(!mptr->ident) {warn++;printf(" WARNING: empty molecule in live list %i at %i\n",ll,m);} }
		for(;m<mols->maxl[ll];m++) {
			mptr=mols->live[ll][m];
			if(mptr) {error++;printf(" SMOLDYN BUG: misplaced molecule in live list %i at %i\n",ll,m);} }}

	for(m=0;m<mols->topd;m++) {
		mptr=mols->dead[m];
		if(!mptr) {error++;printf(" SMOLDYN BUG: NULL molecule in dead list at %i\n",m);}
		else if(mptr->list!=-1) {error++;printf(" SMOLDYN BUG: mis-sorted molecule in dead list at %i (species %i, serno %li)\n",m,mptr->ident,mptr->serno);}
		else if(mptr->ident) {error++;printf(" SMOLDYN BUG: live molecule in dead list at %i\n",m);} }
	for(;m<mols->nd;m++) {
		mptr=mols->dead[m];
		if(!mptr) {error++;printf(" SMOLDYN BUG: NULL molecule in resurrected list at %i\n",m);}
		else if(mptr->list==-1) {error++;printf(" SMOLDYN BUG: mis-sorted molecule in resurrected list at %i\n",m);}
		else if(!mptr->ident) {error++;printf(" BUG: dead molecule in resurrected list at %i\n",m);} }
	for(;m<mols->maxd;m++) {
		mptr=mols->dead[m];
		if(mptr) {error++;printf(" SMOLDYN BUG: misplaced molecule in dead list at %i\n",m);} }

	for(ll=0;ll<mols->nlist;ll++)
		for(m=0;m<mols->nl[ll];m++)	{									// check for molecules outside system
			mptr=mols->live[ll][m];
			if(!posinsystem(sim,mptr->pos)) {
				printf(" WARNING: molecule of type '%s' is outside system volume\n",spname[mptr->ident]);
				warn++; }}

	for(i=1;i<nspecies;i++)														// check for asymmetric diffusion matrices
		for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1))
			if(mols->difm[i][ms]) {
				dotMMD(mols->difm[i][ms],mols->difm[i][ms],m2,dim,dim,dim);
				if(!issymmetricMD(m2,dim)) {
					printf(" WARNING: diffusion matrix for molecule %s (%s) is asymmetric\n",spname[i],molms2string(ms,string));
					warn++; }}

	for(i=1;i<nspecies;i++) {													// check for unused molecules
		sum=0;
		for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1)) sum+=mols->exist[i][ms];
		if(!sum) {
			printf(" WARNING: molecule %s is never used\n",spname[i]);
			warn++; }}

	if(sim->graphss && sim->graphss->graphics>1) {		// check for molecules that may not display
		diag=systemdiagonal(sim);
		for(i=1;i<nspecies;i++)
			for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1)) {
				if(mols->display[i][ms]>0.1*diag) {
					printf(" WARNING: very large display size for molecule %s (%s)\n",spname[i],molms2string(ms,string));
					warn++; }
				if(mols->display[i][ms]<0.001*diag) {
					printf(" WARNING: very small display size for molecule %s (%s)\n",spname[i],molms2string(ms,string));
					warn++; }}}

	if(warnptr) *warnptr=warn;
	return error; }


/******************************************************************************/
/********************************* structure set up ***************************/
/******************************************************************************/


/* molsetcondition.  Sets the molecule superstructure condition to cond, if
appropriate.  Set upgrade to 1 if this is an upgrade, to 0 if this is a
downgrade, or to 2 to set the condition independent of its current value. */
void molsetcondition(molssptr mols,enum StructCond cond,int upgrade) {
	if(!mols) return;
	if(upgrade==0 && mols->condition>cond) mols->condition=cond;
	else if(upgrade==1 && mols->condition<cond) mols->condition=cond;
	else if(upgrade==2) mols->condition=cond;
	if(mols->condition<mols->sim->condition) {
		cond=mols->condition;
		simsetcondition(mols->sim,cond==SCinit?SClists:cond,0); }
	return; }



/* addmollist.  Adds a molecule list named nm and of type mlt to the molecule
superstructure, allocating it if needed.  Returns the index of the list for
success, -1 if memory could not be allocated, -2 if the list name has already
been used, or -3 for illegal inputs (mols or nm was NULL). */
int addmollist(simptr sim,char *nm,enum MolListType mlt) {
	int ll,er;
	molssptr mols;

	mols=sim->mols;
	if(!mols || !nm) return -3;
	if(stringfind(mols->listname,mols->nlist,nm)!=-1) return -2;
	if(mols->nlist==mols->maxlist) {
		er=mollistalloc(mols,mols->maxlist+1,mlt);
		if(er<0) return -1; }
	ll=mols->nlist++;
	strcpy(mols->listname[ll],nm);
	boxsetcondition(sim->boxs,SClists,0);
	rxnsetcondition(sim,-1,SClists,0);
	surfsetcondition(sim->srfss,SClists,0);
	portsetcondition(sim->portss,SClists,0);
	return ll; }


/* molsetmaxspecies.  Sets the maximum number of molecular species to max+1.
The additional species is to represent empty molecules.  At present, this
function may only be called once, as the simulation structure is being set up.
It returns 0 for success, 1 for insufficient memory, 2 if it was called before,
or 3 if max is less than 0. */
int molsetmaxspecies(simptr sim,int max) {
	if(sim->mols) return 2;
	if(max<0) return 3;
	if(max==0) return 0;
	max++;											// for species 0, which is empty
	sim->mols=molssalloc(max);
	if(!sim->mols) return 1;
	sim->mols->sim=sim;
	molsetcondition(sim->mols,SCinit,0);
	boxsetcondition(sim->boxs,SClists,0);
	rxnsetcondition(sim,-1,SClists,0);
	surfsetcondition(sim->srfss,SClists,0);
	portsetcondition(sim->portss,SClists,0);
	return 0; }


/* molsetmaxmol.  Sets the total number of molecules that the simulation can
work with, and allocates them, to max molecules.  This works during initial
setup, or later on.  Returns 0 for success, 1 for out of memory, 2 for
simulation dimensionality not set up yet, 3 for no molecule superstructure set
up yet, 4 for a negative max value, or 5 if the requested max value is less
than the current number of allocated molecules. */
int molsetmaxmol(simptr sim,int max) {
	int er;

	if(sim->dim<1) return 2;
	if(!sim->mols) return 3;
	if(max<0) return 4;
	if(max<sim->mols->maxd) return 5;
	if(max==sim->mols->maxd) return 0;
	max-=sim->mols->maxd;
	er=molexpandlist(sim->mols,sim->dim,-1,max,max);
	if(er) return 1;
	return 0; }


/* moladdspecies.  Adds species named nm to the list of species that is in the
molecule superstructure.  Returns a positive value corresponding to the index of a
successfully added species for success, -2 if sim->mols is NULL, -3 if
more species are being added than were allocated, -4 if we are trying to add empty twice, 
or -5 if the species already exists. */
int moladdspecies(simptr sim,char *nm) {
	molssptr mols;
	int found;

	mols=sim->mols;
	if(!mols) return -2;
	if(mols->nspecies==mols->maxspecies) return -3;
	if(!strcmp(nm,"empty")) return -4;

	found=stringfind(mols->spname,mols->nspecies,nm);
	if(found>=0) return -5;

	strncpy(mols->spname[mols->nspecies++],nm,STRCHAR);
	molsetcondition(mols,SClists,0);
	rxnsetcondition(sim,-1,SClists,0);
	surfsetcondition(sim->srfss,SClists,0);
	return mols->nspecies-1; }


/* molsetexpansionflag.  Sets the expansion flag for species i to flag.  A value of zero
means that the molecule should not be expanded, non-zero means that moleculizer 
should expand it. Returns 0 for sucess, 2 for a non-existant molecule superstructure,
or 3 if i is out of bounds.  */
int molsetexpansionflag(simptr sim,int i,int flag) {
	if(!sim->mols) return 2;
	if(sim->mols->maxspecies<=i) return 3;
	sim->mols->expand[i]=flag;
	return 0; }


/* molsettimestep.  Sets the rms step lengths according to the simulation time
step.  This may be called during setup or afterwards. */
void molsettimestep(molssptr mols,double dt) {
	int i;
	enum MolecState ms;

	if(!mols) return;
	for(i=0;i<mols->nspecies;i++)
		for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1))
			mols->difstep[i][ms]=sqrt(2.0*mols->difc[i][ms]*dt);
	return; }


/* molcalcparams.  Calculates the difstep parameter of the molecule
superstructure and also sets the diffuselist set of flags in the molecule
superstructure.  This function should be called during initial setup (this is
called from setupmols), if any diffusion coefficient changes (performed with
molsetdifc), or if any diffusion matrix changes (performed with molsetdifm,
which also updates the diffusion coefficient). */
void molcalcparams(molssptr mols,double dt) {
	int i,ll;
	enum MolecState ms;

	if(!mols) return;
	if(mols->condition<=SCparams) {
		molsettimestep(mols,dt);
		for(ll=0;ll<mols->nlist;ll++) mols->diffuselist[ll]=0;
		for(i=0;i<mols->nspecies;i++)
			for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1)) {
				if(mols->difc[i][ms]>0) mols->diffuselist[mols->listlookup[i][ms]]=1; }
		molsetcondition(mols,SCok,1); }
	return; }


/* setupmols.  This sets up the molecule superstructure.  It is meant to be
called only at program startup.  It sets up the Gaussian table, live lists,
live list lookup numbers, and diffusion step lengths. */
int setupmols(simptr sim) {
	int i,ll,m,ndif,nfix,ok,er;
	enum MolecState ms;
	molssptr mols;
	moleculeptr mptr;

	mols=sim->mols;
	if(!mols) return 0;

	if(mols->condition<=SClists) {
		er=molssetgausstable(mols,-1);				// gaussian lookup table
		if(er) return 1;

		for(i=1;i<mols->nspecies;i++)					// set exist values
			for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1))
				mols->exist[i][ms]=0;
		for(m=mols->topd;m<mols->nd;m++) {
			mptr=mols->dead[m];
			mols->exist[mptr->ident][mptr->mstate]=1; }
		for(ll=0;ll<mols->nlist;ll++)
			for(m=0;m<mols->nl[ll];m++) {
				mptr=mols->live[ll][m];
				mols->exist[mptr->ident][mptr->mstate]=1; }
		for(i=1;i<mols->nspecies;i++) {
			for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1)) {
				if(mols->exist[i][ms]==0 && rxnisprod(sim,i,ms,0)) mols->exist[i][ms]=1;
				if(mols->exist[i][ms]==0 && issurfprod(sim,i,ms)) mols->exist[i][ms]=1; }
			if(mols->exist[i][MSsoln]==0 && rxnisprod(sim,i,MSbsoln,0)) mols->exist[i][MSsoln]=1;
			if(mols->exist[i][MSsoln]==0 && issurfprod(sim,i,MSbsoln)) mols->exist[i][MSsoln]=1; }

		for(ll=0;ll<mols->nlist;ll++)					// create system molecule lists if none yet
			if(mols->listtype[ll]==MLTsystem) ll=mols->nlist+1;
		if(ll==mols->nlist && mols->maxd>0 && mols->nspecies>1) {
			ndif=nfix=0;
			for(i=1;i<mols->nspecies;i++)
				for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1)) {
					if(mols->difc[i][ms]>0) ndif=1;
					else nfix=1; }
			if(ndif) {
				ll=addmollist(sim,"diffuselist",MLTsystem);
				if(ll<0) return 1;
				molsetlistlookup(sim,-6,MSall,ll); }
			if(nfix) {
				ll=addmollist(sim,"fixedlist",MLTsystem);
				if(ll<0) return 1;
				molsetlistlookup(sim,-7,MSall,ll); }}

		ok=1;															// set any list lookup values that weren't done yet
		for(i=1;i<mols->nspecies && ok;i++)
			for(ms=(MolecState)0;ms<MSMAX && ok;ms=(MolecState)(ms+1))
				if(mols->listlookup[i][ms]<0)
					ok=0;
		if(!ok) {
			ll=stringfind(mols->listname,mols->nlist,"unassignedlist");
			if(ll<0) {
				ll=addmollist(sim,"unassignedlist",MLTsystem);
				if(ll<0) return 1; }
			for(i=1;i<mols->nspecies;i++)
				for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1))
					if(mols->listlookup[i][ms]<0)
						molsetlistlookup(sim,i,ms,ll); }

		for(m=mols->topd;m<mols->nd;m++) {		// set molecule list values for molecules in dead list
			mptr=mols->dead[m];
			mptr->list=mols->listlookup[mptr->ident][mptr->mstate]; }

		molsetcondition(mols,SCparams,1); }

	molcalcparams(sim->mols,sim->dt);

	return 0; }


/******************************************************************************/
/*********************** adding and removing molecules ************************/
/******************************************************************************/

/* molkill. Kills a molecule from one of the live lists.  mptr is a pointer to
the molecule and ll is the list that it is currently listed in (probably equal
to mptr->list, but not necessarily).  If it is known, enter the position of the
molecule in the master list (i.e. not a box list) in m; if it’s unknown set m
to -1.  This function resets most parameters of the molecule structure, but
leaves it in the master list and in a box for later sorting by molsort.  The
appropriate sortl index is updated. */
void molkill(simptr sim,moleculeptr mptr,int ll,int m) {
	int dim,d,*sortl;

	dim=sim->dim;
	sortl=sim->mols->sortl;

	mptr->ident=0;
	mptr->mstate=MSsoln;
	mptr->list=-1;
	for(d=0;d<sim->dim;d++) mptr->posoffset[d]=0;
	mptr->pnl=NULL;
	if(m<0) sim->mols->sortl[ll]=0;
	else if(m<sim->mols->sortl[ll]) sim->mols->sortl[ll]=m;
	return; }


/* getnextmol.  Returns a pointer to the next molecule on the dead list so
that its data can be filled in and it can be added to the system.  The molecule
serial number is assigned.  This updates the topd element of the molecule
superstructure.  Returns NULL if there are no more available molecules.  The
intention is that this function should be called anytime that molecules are to
be added to the system. */
moleculeptr getnextmol(molssptr mols) {
	moleculeptr mptr;

	if(mols->topd==0) return NULL;
	mptr=mols->dead[--mols->topd];
	mptr->serno=mols->serno++;
	return mptr; }


/* newestmol.  Returns a pointer to the molecule that was most recently added to
the system, assuming that molsort has not been called in the meantime.  For
example, if 1 molecule is successfully added with addmol, addsurfmol, or
addcompartmol, this will return a pointer to that molecule. */
moleculeptr newestmol(molssptr mols) {
	return mols->dead[mols->topd-1]; }


/* addmol.  Adds nmol molecules of type ident and state MSsoln to the system.
These molecules are not added to surfaces.  Their positions are chosen
randomly within the rectanguloid that is defined by its corners poslo and
poshi.  Set these vectors equal to each other for all molecules at the same
point.  Returns 0 for success or 3 for insufficient molecules in the dead
list. */
int addmol(simptr sim,int nmol,int ident,double *poslo,double *poshi,int sort) {
	int m,d;
	moleculeptr mptr;

	for(m=0;m<nmol;m++) {
		mptr=getnextmol(sim->mols);
		if(!mptr) return 3;
		mptr->ident=ident;
		mptr->mstate=MSsoln;
		mptr->list=sim->mols->listlookup[ident][MSsoln];
		if(poslo==poshi)
			for(d=0;d<sim->dim;d++)
				mptr->posx[d]=mptr->pos[d]=poslo[d];
		else
			for(d=0;d<sim->dim;d++)
				mptr->posx[d]=mptr->pos[d]=unirandOOD(poslo[d],poshi[d]);
		if(sim->boxs && sim->boxs->nbox)
			mptr->box=pos2box(sim,mptr->pos);
		else mptr->box=NULL; }
	if(sort)
		if(molsort(sim)) return 1;
	return 0; }


/* addsurfmol.  Adds nmol surface-bound molecules, all of type ident and
state molecstate, to the system.  If surface and ps are greater than or equal to
zero and pname is the name of a panel on this surface with this shape, then the
molecules are added to the panel thusly identified; they are added at random
positions on this panel if pos is NULL and at pos otherwise (there is no check
that pos is actually on or near the panel).  On the other hand, set surface
and/or ps to a negative number, or pname to “all”, to indicate “all”; in this
case, pos is ignored and molecules are placed randomly on the chosen panels.
Molecules are added to the top of the dead list and the topd element is adjusted
to account for the new molecules.  The molecule pos, posx, ident, mstate, pnl,
and box elements are set up.  The function returns 0 for successful operation, 1
for inability to allocate temporary memory space, 2 for no panels match the
criteria listed, or 3 for insufficient available molecules.  See the surfacearea2
description for more information about the parameter input scheme. */
int addsurfmol(simptr sim,int nmol,int ident,enum MolecState ms,double *pos,panelptr pnl,int surface,enum PanelShape ps,char *pname) {
	int dim,m,d,totpanel,panel;
	moleculeptr mptr;
	int s,slo,shi,pslo,pshi,p,plo,phi,pindex;
	double *areatable,area,mpos[DIMMAX],totarea;
	panelptr *paneltable;
	surfaceptr srf;

	dim=sim->dim;

	if(pnl || (surface>=0 && ps!=PSall && pname && strcmp(pname,"all"))) {			// add to a specific panel
		if(!pnl) {
			srf=sim->srfss->srflist[surface];
			panel=stringfind(srf->pname[ps],srf->npanel[ps],pname);
			if(panel<0) return 2;
			pnl=srf->panels[ps][panel]; }
		for(m=0;m<nmol;m++) {
			mptr=getnextmol(sim->mols);
			if(!mptr) return 3;
			mptr->ident=ident;
			mptr->mstate=ms;
			mptr->list=sim->mols->listlookup[ident][ms];
			mptr->pnl=pnl;
			if(pos)
				for(d=0;d<dim;d++) mpos[d]=pos[d];
			else
				panelrandpos(pnl,mpos,dim);
			if(ms==MSfront) fixpt2panel(mpos,pnl,dim,PFfront,0);
			else if(ms==MSback) fixpt2panel(mpos,pnl,dim,PFback,0);
			for(d=0;d<dim;d++) mptr->pos[d]=mptr->posx[d]=mpos[d];
			if(sim->boxs && sim->boxs->nbox) mptr->box=pos2box(sim,mpos);
			else mptr->box=NULL; }}

	else {
		totarea=surfacearea2(sim,surface,ps,pname,&totpanel);		// create area lookup tables
		if(totpanel<1) return 2;
		areatable=(double*)calloc(totpanel,sizeof(double));
		if(!areatable) return 1;
		paneltable=(panelptr*)calloc(totpanel,sizeof(panelptr));
		if(!paneltable) {free(areatable);return 1; }

		slo=(surface>=0)?surface:0;
		shi=(surface>=0)?surface+1:sim->srfss->nsrf;
		pslo=(ps!=PSall)?ps:0;
		pshi=(ps!=PSall)?ps+1:PSMAX;

		pindex=0;																						// fill in area lookup tables
		area=0;
		for(s=slo;s<shi;s++)
			for(ps=(PanelShape)pslo;ps<pshi;ps=(PanelShape)(ps+1)) {
				srf=sim->srfss->srflist[s];
				if(!pname || !strcmp(pname,"all")) {plo=0;phi=srf->npanel[ps];}
				else if((panel=stringfind(srf->pname[ps],srf->npanel[ps],pname))<0) plo=phi=0;
				else {plo=panel;phi=panel+1;}
				for(p=plo;p<phi;p++) {
					area+=surfacearea2(sim,s,ps,srf->pname[ps][p],NULL);
					areatable[pindex]=area;
					paneltable[pindex]=srf->panels[ps][p];
					pindex++; }}

		for(m=0;m<nmol;m++) {															// place molecules
			mptr=getnextmol(sim->mols);
			if(!mptr) {free(paneltable);free(areatable);return 3;}
			mptr->ident=ident;
			mptr->mstate=ms;
			mptr->list=sim->mols->listlookup[ident][ms];
			pindex=intrandpD(totpanel,areatable);
			pnl=paneltable[pindex];
			mptr->pnl=pnl;
			panelrandpos(pnl,mpos,dim);
			if(ms==MSfront) fixpt2panel(mpos,pnl,dim,PFfront,0);
			else if(ms==MSback) fixpt2panel(mpos,pnl,dim,PFback,0);
			for(d=0;d<dim;d++) mptr->pos[d]=mptr->posx[d]=mpos[d];
			if(sim->boxs && sim->boxs->nbox) mptr->box=pos2box(sim,mpos);
			else mptr->box=NULL; }

		free(paneltable);
		free(areatable); }

	return 0; }


/* addcompartmol.  Adds nmol molecules of type ident and state MSsoln to the
system with random locations that are within compartment cmpt.  Returns 0 for
success, 2 if cmpt->npts is 0, or 3 if there aren’t enough available
molecules. */
int addcompartmol(simptr sim,int nmol,int ident,compartptr cmpt) {
	int d,dim,m,er;
	moleculeptr mptr;

	if(cmpt->npts==0 && cmpt->ncmptl==0) return 2;
	dim=sim->dim;

	for(m=0;m<nmol;m++) {
		mptr=getnextmol(sim->mols);
		if(!mptr) return 3;
		mptr->ident=ident;
		mptr->mstate=MSsoln;
		mptr->list=sim->mols->listlookup[ident][MSsoln];
		er=compartrandpos(sim,mptr->pos,cmpt);
		if(er) return 2;
		for(d=0;d<dim;d++) mptr->posx[d]=mptr->pos[d];
		if(sim->boxs && sim->boxs->nbox) mptr->box=pos2box(sim,mptr->pos);
		else mptr->box=NULL; }
	return 0; }


/******************************************************************************/
/*************************** core simulation functions ************************/
/******************************************************************************/

/* molsort.  Sorts molecules between live and dead lists, and between live
lists.  This also takes care of the live lists within boxes, as well as all
list indicies.  Sorting is based solely on the list element of the molecule
structure.  Molecule ordering in lists is not preserved.  If a molecule is in
the system (in a master live list of type MLTsystem), its box element must
point to a box, and those boxes’ molecule lists must list the respective
molecules.  Resurrected molecules need to have the proper box listed in the
molecule structure, but should not be listed in the box list; this listing is
taken care of here.  The routine returns 0 for normal operation and 1 if
memory could not be allocated. */
int molsort(simptr sim) {
	molssptr mols;
	int nlist,*maxl,*nl,*topl,*sortl,m,ll,ll2;
	moleculeptr *dead,**live,*mlist,mptr;
	enum MolListType *listtype;
	boxptr bptr;

	mols=sim->mols;
	dead=mols->dead;
	nlist=mols->nlist;
	listtype=mols->listtype;
	live=mols->live;
	maxl=mols->maxl;
	nl=mols->nl;
	topl=mols->topl;
	sortl=mols->sortl;

	for(ll=0;ll<nlist;ll++)								// reset topl indicies
		topl[ll]=nl[ll];

	for(ll=0;ll<nlist;ll++) {							// sort live lists
		mlist=live[ll];
		for(m=sortl[ll];m<topl[ll];m++) {
			if(mlist[m]->list!=ll) {
				mptr=mlist[m];
				if(mptr->list==-1) {						// move to dead list
					if(mptr->box) boxremovemol(mptr,ll);
					dead[mols->nd++]=dead[mols->topd];
					dead[mols->topd++]=mptr;
					mlist[m]=NULL; }
				else {													// move to another live list
					ll2=mptr->list;
					bptr=mptr->box;
					if(mptr->box) boxremovemol(mptr,ll);
					if(nl[ll2]==maxl[ll2])
						if(molexpandlist(mols,sim->dim,ll2,-1,0)) {
							printf("out of memory in molsort\n");return 1;}
					live[ll2][nl[ll2]++]=mptr;
					mlist[m]=NULL;
					if(listtype[ll2]==MLTsystem) {
						if(bptr) mptr->box=bptr;
						else mptr->box=pos2box(sim,mptr->pos);
						if(boxaddmol(mptr,ll2)) {
							printf("out of memory in molsort\n");return 1;} }}

				mlist[m]=mlist[--topl[ll]];				// compact original live list
				mlist[topl[ll]]=mlist[--nl[ll]];
				mlist[nl[ll]]=NULL;
				m--; }}}

	for(m=mols->topd;m<mols->nd;m++) {		// move molecules from resurrected to reborn
		mptr=dead[m];
		ll2=mptr->list;
		if(nl[ll2]==maxl[ll2])
			if(molexpandlist(mols,sim->dim,ll2,-1,0)) {
				printf("out of memory in molsort\n");return 1;}
		live[ll2][nl[ll2]++]=mptr;
		dead[m]=NULL;
		if(listtype[ll2]==MLTsystem) {
			if(boxaddmol(mptr,ll2)) {
				printf("out of memory in molsort\n");return 1;} }}
	mols->nd=mols->topd;

	for(ll=0;ll<nlist;ll++)								// reset sortl indicies
		sortl[ll]=nl[ll];

	return 0; }


/* diffuse.  diffuse does the diffusion for all molecules over one time step.
Walls and surfaces are ignored and molecules are not reassigned to the boxes.
If there is a diffusion matrix, it is used for anisotropic diffusion;
otherwise isotropic diffusion is done, using the difstep parameter.  The posx
element is updated to the prior position and pos is updated to the new
position.  Surface-bound molecules are diffused as well. */
int diffuse_unitary(simptr sim) {
	molssptr mols;
	int ll,m,d,nmol,dim,i,ngtablem1;
	enum MolecState ms;
	double flt1;
	double v1[DIMMAX],v2[DIMMAX],**difstep,***difm,***drift,epsilon,neighdist,*gtable,dt;
	moleculeptr *mlist;
	moleculeptr mptr;

	dim=sim->dim;
	mols=sim->mols;
	ngtablem1=mols->ngausstbl-1;
	gtable=mols->gausstbl;
	difstep=mols->difstep;
	difm=mols->difm;
	drift=mols->drift;
	dt=sim->dt;
	flt1=sqrt(2.0*dt);
	epsilon=(sim->srfss)?sim->srfss->epsilon:0;
	neighdist=(sim->srfss)?sim->srfss->neighdist:0;

	for(ll=0;ll<mols->nlist;ll++)
		if(mols->diffuselist[ll]) {
			mlist=mols->live[ll];
			nmol=mols->nl[ll];
			for(m=0;m<nmol;m++) {
				mptr=mlist[m];
				i=mptr->ident;
				ms=mptr->mstate;
				if(!difm[i][ms])
					for(d=0;d<dim;d++) {
						mptr->posx[d]=mptr->pos[d];
						mptr->pos[d]+=difstep[i][ms]*gtable[randULI()&ngtablem1]; }
				else {
					for(d=0;d<dim;d++) {
						mptr->posx[d]=mptr->pos[d];
						v1[d]=flt1*gtable[randULI()&ngtablem1]; }
					dotMVD(difm[i][ms],v1,v2,dim,dim);
					for(d=0;d<dim;d++) mptr->pos[d]+=v2[d]; }
				if(drift[i][ms]) {
					for(d=0;d<dim;d++) mptr->pos[d]+=drift[i][ms][d]*dt; }}}

	return 0; }


//??????????????? new code start
typedef struct ll_threading_struct {
    simptr sim;
    int ll_ndx;
    int min_ndx;
    int max_ndx;
		} *diffuse_struct;


/* Diffuse live list. */
void* diffuseLiveList_threaded(void* ndx_as_void) {	//??????? new function
    diffuse_struct data_struct = (diffuse_struct ) ndx_as_void;
    int livelist_ndx = data_struct->ll_ndx;
    simptr sim = data_struct->sim;
    int min_ndx = data_struct->min_ndx;
    int max_ndx = data_struct->max_ndx;

    int mol_ndx;
    double*** difm;

    moleculeptr ptr_mol = NULL;
    moleculeptr *mol_list = NULL;

    int mol_ident;
    enum MolecState mol_state;
    int dim_ndx;
    int dim = sim->dim;
    double v1[DIMMAX], v2[DIMMAX];
    double flt1;
    double *gtable, **difstep, ***drift, dt;
    int ngtablem1;

    difm=sim->mols->difm;
    difstep=sim->mols->difstep;

    mol_list = sim->mols->live[livelist_ndx];    
//    int num_mols_in_ll = sim->mols->nl[livelist_ndx];
    
    ngtablem1=sim->mols->ngausstbl-1;
    gtable=sim->mols->gausstbl;
    drift=sim->mols->drift;
    dt=sim->dt;
    flt1=sqrt(2.0*dt);

    for(mol_ndx = min_ndx; mol_ndx < max_ndx; mol_ndx++) 
    {
        ptr_mol=mol_list[ mol_ndx ];
        mol_ident=ptr_mol->ident;
        mol_state=ptr_mol->mstate;

        // Normal diffusion
        if( difm[ mol_ident ][ mol_state ])
        {
            for(dim_ndx = 0; dim_ndx != dim; dim_ndx++) 
            {
                ptr_mol->posx[dim_ndx] = ptr_mol->pos[dim_ndx];
                v1[dim_ndx] = flt1 * gtable[ randULI() & ngtablem1 ]; 
            }

            dotMVD( difm[mol_ident][mol_state], v1, v2, dim, dim);

            for(dim_ndx=0; dim_ndx != dim; ++dim_ndx)
            {
                ptr_mol->pos[dim_ndx] += v2[dim_ndx]; 
            }
        }
        else 
        {
            // Anistropic diffusion
            for(dim_ndx = 0; dim_ndx != dim; ++dim_ndx) 
            {
                ptr_mol->posx[dim_ndx] = ptr_mol->pos[dim_ndx];
                ptr_mol->pos[dim_ndx] += difstep[mol_ident][mol_state] * gtable[ randULI() & ngtablem1 ]; 
            }
        }

        // If there is a drift, add it in.
        if( drift[mol_ident][mol_state] ) 
        {
            for(dim_ndx=0; dim_ndx != dim; dim_ndx++) 
            {
                ptr_mol->pos[dim_ndx] += drift[mol_ident][mol_state][dim_ndx] * dt; 
            }
        }
    }
    return NULL;
}





/* diffuse_THREADED.  diffuse does the diffusion for all molecules over one time step.
Walls and surfaces are ignored and molecules are not reassigned to the boxes.
If there is a diffusion matrix, it is used for anisotropic diffusion;
otherwise isotropic diffusion is done, using the difstep parameter.  The posx
element is updated to the prior position and pos is updated to the new
position.  Surface-bound molecules are diffused as well. Returns error codes: 
0 on success, 1 on unknown failure. */
int diffuse_threaded(simptr sim) {//??????????? new function
#ifndef THREADING
    return 2;
#else
	molssptr mols;
	int livelist_ndx, dim,ngtablem1;
	double flt1;
	double **difstep,***difm,***drift,epsilon,neighdist,*gtable,dt;
	stack* current_thread_input_stack;

	dim=sim->dim;
	mols=sim->mols;
	ngtablem1=mols->ngausstbl-1;
	gtable=mols->gausstbl;
	difstep=mols->difstep;
	difm=mols->difm;
	drift=mols->drift;
	dt=sim->dt;
	flt1=sqrt(2.0 * dt);
	epsilon=(sim->srfss)?sim->srfss->epsilon:0;
	neighdist=(sim->srfss)?sim->srfss->neighdist:0;

        int num_threads = getnumberofthreads(sim);
        int current_thread_ndx = 0;

	for(livelist_ndx = 0; livelist_ndx != mols->nlist; ++livelist_ndx)
        {
            if( mols->diffuselist[livelist_ndx] ) 
            {
		
		int per_thread_size = mols->nl[livelist_ndx] / num_threads;
		int currentNdx = 0;

		int thread_ndx;
		for( thread_ndx = 0; thread_ndx != num_threads - 1; ++thread_ndx)
		{
		    clearthreaddata( sim->threads->thread[thread_ndx]);
		    current_thread_input_stack = sim->threads->thread[thread_ndx]->input_stack;
		    push_data_onto_stack(current_thread_input_stack, &sim, sizeof(sim));
		    push_data_onto_stack(current_thread_input_stack, &livelist_ndx, sizeof(livelist_ndx));
		    push_data_onto_stack(current_thread_input_stack, &currentNdx, sizeof(currentNdx));
		    currentNdx += per_thread_size;
		    push_data_onto_stack(current_thread_input_stack, &currentNdx, sizeof(currentNdx));
		    
		    pthread_create(sim->threads->thread[thread_ndx]->thread_id, NULL, diffuseLiveList_threaded, (void*) current_thread_input_stack->stack_data);
		}
		{
		    thread_ndx = num_threads - 1;

		    clearthreaddata( sim->threads->thread[thread_ndx]);
		    current_thread_input_stack = sim->threads->thread[thread_ndx]->input_stack;

		    push_data_onto_stack(current_thread_input_stack, &sim, sizeof(sim));
		    push_data_onto_stack(current_thread_input_stack, &livelist_ndx, sizeof(livelist_ndx));
		    push_data_onto_stack(current_thread_input_stack, &currentNdx, sizeof(currentNdx));
		    push_data_onto_stack(current_thread_input_stack, &mols->nl[livelist_ndx], sizeof(int));

		    pthread_create(sim->threads->thread[thread_ndx]->thread_id, NULL, diffuseLiveList_threaded, (void*) current_thread_input_stack->stack_data);
		}

		int ndx;
		for( ndx = 0; ndx != current_thread_ndx; ++ndx)
		{
		    pthread_join( *((pthread_t*) sim->threads->thread[thread_ndx]->thread_id), NULL);
		}

            }
	    

        }

	return 0; 
#endif
}



