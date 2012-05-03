/* Steven Andrews, started 10/22/2001.
 This is a library of functions for the Smoldyn program.  See documentation
 called Smoldyn_doc1.pdf and Smoldyn_doc2.pdf.
 Copyright 2003-2011 by Steven Andrews.  This work is distributed under the terms
 of the Gnu General Public License (GPL). */

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
#include "smoldynfuncs.h"
#include "smoldynconfigure.h"

#ifdef THREADING
#include <pthread.h>
#endif

/******************************************************************************/
/*********************************** Molecules ********************************/
/******************************************************************************/


/******************************************************************************/
/****************************** Local declarations ****************************/
/******************************************************************************/

// enumerated type functions
enum MolListType molstring2mlt(char *string);
char *molmlt2string(enum MolListType mlt,char *string);

// low level utilities
char *molpos2string(simptr sim,moleculeptr mptr,char *string);

// memory management
moleculeptr molalloc(int dim);
void molfree(moleculeptr mptr);
molssptr molssalloc(molssptr mols,int maxspecies);
int mollistalloc(molssptr mols,int maxlist,enum MolListType mlt);
int molexpandlist(molssptr mols,int dim,int ll,int nspaces,int nmolecs);

// data structure output

// structure setup
int molsetmaxspecies(simptr sim,int max);
int molsupdateparams(molssptr mols,double dt);
int molsupdatelists(simptr sim);

// adding and removing molecules
moleculeptr newestmol(molssptr mols);
int molgetexport(simptr sim,int ident,enum MolecState ms);
int molputimport(simptr sim,int nmol,int ident,enum MolecState ms,panelptr pnl,enum PanelFace face);
int moldummyporter(simptr sim);

// core simulation functions


/******************************************************************************/
/********************************* enumerated types ***************************/
/******************************************************************************/


/* molstring2ms */
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


/* molms2string */
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


/* molstring2mlt */
enum MolListType molstring2mlt(char *string) {
	enum MolListType ans;

	if(!strcmp(string,"system")) ans=MLTsystem;
	else if(!strcmp(string,"port")) ans=MLTport;
	else ans=MLTnone;
	return ans; }


/* molmlt2string */
char *molmlt2string(enum MolListType mlt,char *string) {
	if(mlt==MLTsystem) strcpy(string,"system");
	else if(mlt==MLTport) strcpy(string,"port");
	else strcpy(string,"none");
	return string; }


/******************************************************************************/
/****************************** low level utilities ***************************/
/******************************************************************************/


/* molwildcardname */
int molwildcardname(molssptr mols,char *name,int channel,int itest) {
	static int i[]={-1,-1};									// i is most recent returned index
	static char nm[][STRCHAR]={"\0","\0"};	// nm is name with wildcards
	static unsigned int *flags=NULL;				// flags lists match results for all species
	static int nspecies=0;
	int i2,ch,match;
	unsigned int mask;

	if(!mols) {							// free memory and reset static variables (mode 1)
		for(ch=0;ch<2;ch++) {
			i[ch]=-1;
			nm[ch][0]='\0';
			free(flags);
			flags=NULL;
			nspecies=0; }
		strEnhWildcardMatch(NULL,NULL);
		return 0; }

	if(name) {							// initialize using a new name (mode 2)
		strncpy(nm[channel],name,STRCHAR);
		i[channel]=1;
		if(nspecies==0);
		else if(nspecies!=mols->nspecies) {
			nspecies=0;
			free(flags); }
		else {
			mask=~(1<<channel);
			for(i2=0;i2<nspecies;i2++)
				flags[i2]&=mask; }
		match=strEnhWildcardMatch(name,"test");
		if(match<0) return match-1; }

	if(itest>0) {						// test specific index (mode 4)
		if(!nspecies) {
			nspecies=mols->nspecies;
			flags=(unsigned int*) calloc(nspecies,sizeof(unsigned int));
			if(!flags) {molwildcardname(NULL,NULL,0,0);return -2;}
			for(i2=0;i2<nspecies;i2++) flags[i2]=0; }
		mask=1<<channel;
		if(!(flags[0]&mask)) {
			flags[0]|=mask;
			for(i2=1;i2<nspecies;i2++)
				if(strEnhWildcardMatch(nm[channel],mols->spname[i2]))
					flags[i2]|=mask; }
		return flags[itest]&mask; }

													// return next match (mode 3)
	if(i[channel]<0) return -1;							// all matches already done
	for(i2=i[channel];i2<mols->nspecies && !strEnhWildcardMatch(nm[channel],mols->spname[i2]);i2++);
	if(i2==mols->nspecies) i[channel]=i2=-1;
	else {										// where to start next search
		i[channel]=i2;
		if(!name) i[channel]++; }
	return i2; }


/* readmolname */
int readmolname(simptr sim,char *str,enum MolecState *msptr,int channel) {
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
	else if(strchr(nm,'*') || strchr(nm,'?')) {	// wildcard character
		if(channel<0) i=-4;
		else i=molwildcardname(sim->mols,nm,channel,0);
		if(i>0) i=-6;						// at least one match
		else i=-4; }						// no match
	else {
		i=stringfind(sim->mols->spname,sim->mols->nspecies,nm);
		if(i<0) i=-4; }		// unknown molecule name
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
			if(mptr->pnl!=pnl && lineXpanel(mptr->pos,newpos,pnl,dim,crosspt,NULL,NULL,NULL,NULL,NULL)) tryagain=1; }
		if(!tryagain) done=1;

		if(!done) {
			if(++count>50) {
				simLog(sim,8,"WARNING: unable to write %s molecule position (%s) on the correct side of all surfaces\n",sim->mols->spname[mptr->ident],string);
				return string; }

			if(dist==0) {
				for(d=0;d<dim;d++) dist+=(newpos[d]-mptr->pos[d])*(newpos[d]-mptr->pos[d]);
				dist=50*sqrt(dist); }

			line2=string;												// write position to string
			for(d=0;d<dim;d++) {
				sprintf(line2," %g",mptr->pos[d]+unirandCCD(-dist,dist));
				line2+=strlen(line2); }}}
		
		return string; }


/* molchangeident */
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
	if(ms==MSsoln || ms==MSbsoln) mptr->pnl=NULL;
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
int molssetgausstable(simptr sim,int size) {
	int er;
	molssptr mols;
	double *newtable;

	er=molenablemols(sim,-1);
	if(er) return er;
	mols=sim->mols;

	if(mols->ngausstbl>0 && (mols->ngausstbl==size || size==-1)) return 0;
	if(size<1) size=4096;
	else if(!is2ton(size)) return 3;

	newtable=(double*) calloc(size,sizeof(double));
	CHECKMEM(newtable);
	randtableD(newtable,size,1);
	randshuffletableD(newtable,size);

	if(mols->gausstbl) free(mols->gausstbl);
	mols->ngausstbl=size;
	mols->gausstbl=newtable;
	return 0;
failure:
	simLog(sim,10,"Unable to allocate memory in molssetgausstable");
	return 1; }


/* molsetdifc */
void molsetdifc(simptr sim,int ident,enum MolecState ms,double difc) {
	int i;
	enum MolecState mslo,mshi;

	if(ms==MSbsoln) ms=MSsoln;
	else if(ms==MSnone) return;
	if(ms!=MSall) mshi=MolecState((mslo=ms)+1);
	else {mslo=MolecState(0);mshi=MolecState(MSMAX);}

	i=0;
	while(1) {
		if(ident>0) {						// single species
			if(i==ident) break;
			i=ident; }
		else if(ident==-5) {		// all species
			if(++i==sim->mols->nspecies) break; }
		else if(ident==-6) {		// wildcard selected species
			if((i=molwildcardname(sim->mols,NULL,0,0))<0) break; }

		for(ms=mslo;ms<mshi;ms=MolecState(ms + 1))
			sim->mols->difc[i][ms]=difc; }

	molsetcondition(sim->mols,SCparams,0);
	rxnsetcondition(sim,-1,SCparams,0);
	surfsetcondition(sim->srfss,SCparams,0);
	return; }


/* molsetdifm */
int molsetdifm(simptr sim,int ident,enum MolecState ms,double *difm) {
	int i,d,dim;
	enum MolecState mslo,mshi;
	double *difmat,dm2[DIMMAX*DIMMAX];

	dim=sim->dim;
	if(!difm) return 0;
	if(ms==MSbsoln) ms=MSsoln;
	else if(ms==MSnone) return 0;
	if(ms!=MSall) mshi=MolecState((mslo=ms)+1);
	else {mslo=MolecState(0);mshi=MolecState(MSMAX);}

	i=0;
	while(1) {
		if(ident>0) {						// single species
			if(i==ident) break;
			i=ident; }
		else if(ident==-5) {		// all species
			if(++i==sim->mols->nspecies) break; }
		else if(ident==-6) {		// wildcard selected species
			if((i=molwildcardname(sim->mols,NULL,0,0))<0) break; }

		for(ms=mslo;ms<mshi;ms=MolecState(ms + 1)) {
			difmat=sim->mols->difm[i][ms];
			if(!difmat) {
				difmat=(double*) calloc(sim->dim*sim->dim,sizeof(double));
				CHECKMEM(difmat);
				sim->mols->difm[i][ms]=difmat; }
			for(d=0;d<sim->dim*sim->dim;d++)
				difmat[d]=difm[d];
			dotMMD(difmat,difmat,dm2,dim,dim,dim);
			sim->mols->difc[i][ms]=traceMD(dm2,dim)/dim; }}

	molsetcondition(sim->mols,SCparams,0);
	return 0;
failure:
	simLog(sim,10,"Unable to allocate memory in molsetdifm");
	return 1; }


/* molsetdrift */
int molsetdrift(simptr sim,int ident,enum MolecState ms,double *drift) {
	int i,d,dim;
	enum MolecState mslo,mshi;
	double *driftvect;

	dim=sim->dim;
	if(!drift) return 0;
	if(ms==MSbsoln) ms=MSsoln;
	else if(ms==MSnone) return 0;
	if(ms!=MSall) mshi=MolecState((mslo=ms)+1);
	else {mslo=MolecState(0);mshi=MolecState(MSMAX);}

	i=0;
	while(1) {
		if(ident>0) {						// single species
			if(i==ident) break;
			i=ident; }
		else if(ident==-5) {		// all species
			if(++i==sim->mols->nspecies) break; }
		else if(ident==-6) {		// wildcard selected species
			if((i=molwildcardname(sim->mols,NULL,0,0))<0) break; }
		
		for(ms=mslo;ms<mshi;ms=MolecState(ms + 1)) {
			driftvect=sim->mols->drift[i][ms];
			if(!driftvect) {
				driftvect=(double*) calloc(sim->dim,sizeof(double));
				CHECKMEM(driftvect);
				sim->mols->drift[i][ms]=driftvect; }
			for(d=0;d<sim->dim;d++)
				driftvect[d]=drift[d]; }}

	molsetcondition(sim->mols,SCparams,0);
	return 0;
failure:
	simLog(sim,10,"Unable to allocate memory in molsetdrift");
	return 1; }


/* molsetdisplaysize */
void molsetdisplaysize(simptr sim,int ident,enum MolecState ms,double dsize) {
	int i;
	enum MolecState mslo,mshi;

	if(ms==MSbsoln) ms=MSsoln;
	else if(ms==MSnone) return;
	if(ms!=MSall) mshi=MolecState((mslo=ms)+1);
	else {mslo=MolecState(0);mshi=MolecState(MSMAX);}

	i=0;
	while(1) {
		if(ident>0) {						// single species
			if(i==ident) break;
			i=ident; }
		else if(ident==-5) {		// all species
			if(++i==sim->mols->nspecies) break; }
		else if(ident==-6) {		// wildcard selected species
			if((i=molwildcardname(sim->mols,NULL,0,0))<0) break; }
		
		for(ms=mslo;ms<mshi;ms=MolecState(ms + 1))
			sim->mols->display[i][ms]=dsize; }

	return; }


/* molsetcolor */
void molsetcolor(simptr sim,int ident,enum MolecState ms,double *color) {
	int i,col;
	enum MolecState mslo,mshi;

	if(ms==MSbsoln) ms=MSsoln;
	else if(ms==MSnone) return;
	if(ms!=MSall) mshi=MolecState((mslo=ms)+1);
	else {mslo=MolecState(0);mshi=MolecState(MSMAX);}

	i=0;
	while(1) {
		if(ident>0) {						// single species
			if(i==ident) break;
			i=ident; }
		else if(ident==-5) {		// all species
			if(++i==sim->mols->nspecies) break; }
		else if(ident==-6) {		// wildcard selected species
			if((i=molwildcardname(sim->mols,NULL,0,0))<0) break; }
		
		for(ms=mslo;ms<mshi;ms=MolecState(ms + 1))
			for(col=0;col<3;col++)
				sim->mols->color[i][ms][col]=color[col]; }

	return; }


/* molsetlistlookup */
void molsetlistlookup(simptr sim,int ident,enum MolecState ms,int ll) {
	int i,skip;
	enum MolecState mslo,mshi;

	if(ms==MSbsoln) ms=MSsoln;
	else if(ms==MSnone) return;
	if(ms!=MSall) mshi=MolecState((mslo=ms)+1);
	else {mslo=MolecState(0);mshi=MolecState(MSMAX);}

	i=0;
	while(1) {
		if(ident>0) {						// single species
			if(i==ident) break;
			i=ident; }
		else if(ident==-5 || ident==-7 || ident==-8) {		// all species
			if(++i==sim->mols->nspecies) break; }
		else if(ident==-6) {		// wildcard selected species
			if((i=molwildcardname(sim->mols,NULL,0,0))<0) break; }
		
		for(ms=mslo;ms<mshi;ms=MolecState(ms + 1)) {
			skip=0;
			if(ident==-7 && sim->mols->difc[i][ms]==0) skip=1;
			else if(ident==-8 && sim->mols->difc[i][ms]>0) skip=1;
			if(!skip) sim->mols->listlookup[i][ms]=ll; }}
	return; }


/* molsetexist */
void molsetexist(simptr sim,int ident,enum MolecState ms,int exist) {
	if(ident<=0) return;
	if(ms==MSnone) return;
	else if(ms==MSbsoln) ms=MSsoln;
	else if(ms==MSall) {
		for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1)) sim->mols->exist[ident][ms]=exist;
		return; }
	sim->mols->exist[ident][ms]=exist;
	return; }


/* molcount */
int molcount(simptr sim,int i,enum MolecState ms,boxptr bptr,int max) {
	int count,ll,nmol,top,m,lllo,llhi;
	moleculeptr *mlist;
	molssptr mols;

	mols=sim->mols;
	if(!mols) return 0;
	if(i<0 && i!=-6) i=-5;
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

			if(i==-5 && ms==MSall) {
				count+=top; }
			else if(i==-5) {
				for(m=0;m<top && count<max;m++) if(mlist[m]->mstate==ms) count++; }
			else if(i==-6 && ms==MSall) {
				for(m=0;m<top && count<max;m++) if(molwildcardname(mols,NULL,0,mlist[m]->ident)) count++; }
			else if(i==-6) {
				for(m=0;m<top && count<max;m++) if(molwildcardname(mols,NULL,0,mlist[m]->ident) && mlist[m]->mstate==ms) count++; }
			else if(ms==MSall) {
				for(m=0;m<top && count<max;m++) if(mlist[m]->ident==i) count++; }
			else {
				for(m=0;m<top && count<max;m++) if(mlist[m]->ident==i && mlist[m]->mstate==ms) count++; }}

	if(!bptr) {
		mlist=sim->mols->dead;											// count resurrected molecules
		nmol=sim->mols->nd;
		top=sim->mols->topd;

		if(i==-5 && ms==MSall) {
			count+=nmol-top; }
		else if(i==-5) {
			for(m=top;m<nmol && count<max;m++) if(mlist[m]->mstate==ms) count++; }
		else if(i==-6 && ms==MSall) {
			for(m=top;m<nmol && count<max;m++) if(molwildcardname(mols,NULL,0,mlist[m]->ident)) count++; }
		else if(i==-6) {
			for(m=top;m<nmol && count<max;m++) if(molwildcardname(mols,NULL,0,mlist[m]->ident) && mlist[m]->mstate==ms) count++; }
		else if(ms==MSall) {
			for(m=top;m<nmol && count<max;m++) if(mlist[m]->ident==i) count++; }
		else {
			for(m=top;m<nmol && count<max;m++) if(mlist[m]->ident==i && mlist[m]->mstate==ms) count++; }

		for(ll=0;ll<sim->mols->nlist;ll++)					// count molecules that need sorting
			if(sim->mols->listtype[ll]==MLTsystem) {
				mlist=sim->mols->live[ll];
				nmol=sim->mols->nl[ll];
				top=sim->mols->sortl[ll];

				if(i==-5 && ms==MSall) {
					for(m=top;m<nmol && count<max;m++) if(mlist[m]->ident!=0) count++; }
				else if(i==-5) {
					for(m=top;m<nmol && count<max;m++) if(mlist[m]->ident!=0 && mlist[m]->mstate==ms) count++; }
				else if(i==-6 && ms==MSall) {
					for(m=top;m<nmol && count<max;m++) if(molwildcardname(mols,NULL,0,mlist[m]->ident)) count++; }
				else if(i==-6) {
					for(m=top;m<nmol && count<max;m++) if(molwildcardname(mols,NULL,0,mlist[m]->ident) && mlist[m]->mstate==ms) count++; }
				else if(ms==MSall) {
					for(m=top;m<nmol && count<max;m++) if(mlist[m]->ident==i) count++; }
				else {
					for(m=top;m<nmol && count<max;m++) if(mlist[m]->ident==i && mlist[m]->mstate==ms) count++; }}}

	return count; }


/* MolCalcDifcSum */
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

/* molalloc */
moleculeptr molalloc(int dim) {
	moleculeptr mptr;
	int d;

	mptr=NULL;
	CHECKMEM(mptr=(moleculeptr) malloc(sizeof(struct moleculestruct)));
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

	CHECKMEM(mptr->pos=(double*) calloc(dim,sizeof(double)));
	CHECKMEM(mptr->posx=(double*) calloc(dim,sizeof(double)));
	CHECKMEM(mptr->via=(double*) calloc(dim,sizeof(double)));
	CHECKMEM(mptr->posoffset=(double*) calloc(dim,sizeof(double)));
	for(d=0;d<dim;d++)
		mptr->pos[d]=mptr->posx[d]=mptr->via[d]=mptr->posoffset[d]=0;
	return mptr;
 failure:
	molfree(mptr);
	simLog(NULL,10,"Unable to allocate memory in molalloc");
	return NULL; }


/* molfree */
void molfree(moleculeptr mptr) {
	if(!mptr) return;
	if(mptr->pos) free(mptr->pos);
	if(mptr->posx) free(mptr->posx);
	if(mptr->posoffset) free(mptr->posoffset);
	if(mptr->via) free(mptr->via);
	free(mptr);
	return; }


/* molssalloc */
molssptr molssalloc(molssptr mols,int maxspecies) {
	int newmols,i,**newexist,**newlistlookup,*newexpand;
	enum MolecState ms;
	char **newspname;
	double **newdifc,**newdifstep,***newdifm,***newdrift,**newdisplay,***newcolor;

	if(maxspecies<1) return NULL;
	maxspecies++;

	newmols=0;

	if(!mols) {
		mols=(molssptr) malloc(sizeof(struct molsuperstruct));
		CHECKMEM(mols);
		newmols=1;

		mols->condition=SCinit;
		mols->sim=NULL;
		mols->maxspecies=0;
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
		mols->maxdlimit=-1;
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
		mols->expand=NULL; }

	if(maxspecies>mols->maxspecies) {

		CHECKMEM(newspname=(char**) calloc(maxspecies,sizeof(char*)));
		for(i=0;i<maxspecies;i++) newspname[i]=NULL;
		for(i=0;i<mols->maxspecies;i++) newspname[i]=mols->spname[i];
		for(;i<maxspecies;i++) {
			CHECKMEM(newspname[i]=EmptyString()); }

		strncpy(newspname[0],"empty",STRCHAR-1);

		CHECKMEM(newdifc=(double**) calloc(maxspecies,sizeof(double*)));
		for(i=0;i<maxspecies;i++) newdifc[i]=NULL;
		for(i=0;i<mols->maxspecies;i++) newdifc[i]=mols->difc[i];
		for(;i<maxspecies;i++) {
			CHECKMEM(newdifc[i]=(double*) calloc(MSMAX,sizeof(double)));
			for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1)) newdifc[i][ms]=0; }

		CHECKMEM(newdifstep=(double**) calloc(maxspecies,sizeof(double*)));
		for(i=0;i<maxspecies;i++) newdifstep[i]=NULL;
		for(i=0;i<mols->maxspecies;i++) newdifstep[i]=mols->difstep[i];
		for(;i<maxspecies;i++) {
			CHECKMEM(newdifstep[i]=(double*) calloc(MSMAX,sizeof(double)));
			for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1)) newdifstep[i][ms]=0; }

		CHECKMEM(newdifm=(double***) calloc(maxspecies,sizeof(double**)));
		for(i=0;i<maxspecies;i++) newdifm[i]=NULL;
		for(i=0;i<mols->maxspecies;i++) newdifm[i]=mols->difm[i];
		for(;i<maxspecies;i++) {
			CHECKMEM(newdifm[i]=(double**) calloc(MSMAX,sizeof(double*)));
			for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1)) newdifm[i][ms]=NULL; }

		CHECKMEM(newdrift=(double***) calloc(maxspecies,sizeof(double**)));
		for(i=0;i<maxspecies;i++) newdrift[i]=NULL;
		for(i=0;i<mols->maxspecies;i++) newdrift[i]=mols->drift[i];
		for(;i<maxspecies;i++) {
			CHECKMEM(newdrift[i]=(double**) calloc(MSMAX,sizeof(double*)));
			for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1)) newdrift[i][ms]=NULL; }

		CHECKMEM(newdisplay=(double**) calloc(maxspecies,sizeof(double*)));
		for(i=0;i<maxspecies;i++) newdisplay[i]=NULL;
		for(i=0;i<mols->maxspecies;i++) newdisplay[i]=mols->display[i];
		for(;i<maxspecies;i++) {
			CHECKMEM(newdisplay[i]=(double*) calloc(MSMAX,sizeof(double)));
			for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1)) newdisplay[i][ms]=3; }

		CHECKMEM(newcolor=(double ***) calloc(maxspecies,sizeof(double **)));
		for(i=0;i<maxspecies;i++) newcolor[i]=NULL;
		for(i=0;i<mols->maxspecies;i++) newcolor[i]=mols->color[i];
		for(;i<maxspecies;i++) {
			CHECKMEM(newcolor[i]=(double**) calloc(MSMAX,sizeof(double*)));
			for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1)) newcolor[i][ms]=NULL;
			for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1)) {
				CHECKMEM(newcolor[i][ms]=(double*) calloc(3,sizeof(double)));
				newcolor[i][ms][0]=newcolor[i][ms][1]=newcolor[i][ms][2]=0; }}

		CHECKMEM(newexist=(int**) calloc(maxspecies,sizeof(int*)));
		for(i=0;i<maxspecies;i++) newexist[i]=NULL;
		for(i=0;i<mols->maxspecies;i++) newexist[i]=mols->exist[i];
		for(;i<maxspecies;i++) {
			CHECKMEM(newexist[i]=(int*) calloc(MSMAX,sizeof(int)));
			for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1)) newexist[i][ms]=0; }

		CHECKMEM(newlistlookup=(int**) calloc(maxspecies,sizeof(int*)));
		for(i=0;i<maxspecies;i++) newlistlookup[i]=NULL;
		for(i=0;i<mols->maxspecies;i++) newlistlookup[i]=mols->listlookup[i];
		for(;i<maxspecies;i++) {
			CHECKMEM(newlistlookup[i]=(int*) calloc(MSMAX,sizeof(int)));
			for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1)) newlistlookup[i][ms]=-1; }

		CHECKMEM(newexpand=(int*) calloc(maxspecies,sizeof(int)));
		for(i=0;i<mols->maxspecies;i++) newexpand[i]=mols->expand[i];
		for(;i<maxspecies;i++) newexpand[i]=0;

		mols->maxspecies=maxspecies;
		free(mols->spname);
		mols->spname=newspname;
		free(mols->difc);
		mols->difc=newdifc;
		free(mols->difstep);
		mols->difstep=newdifstep;
		free(mols->difm);
		mols->difm=newdifm;
		free(mols->drift);
		mols->drift=newdrift;
		free(mols->display);
		mols->display=newdisplay;
		free(mols->color);
		mols->color=newcolor;
		free(mols->exist);
		mols->exist=newexist;
		free(mols->listlookup);
		mols->listlookup=newlistlookup;
		free(mols->expand);
		mols->expand=newexpand; }

	return mols;

 failure:
	simLog(NULL,10,"Unable to allocate memory in molssalloc");
	return NULL; }


/* mollistalloc */
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

	CHECKMEM(listname=(char**) calloc(maxlist,sizeof(char*)));
	for(ll=0;ll<maxlist;ll++) listname[ll]=NULL;

	CHECKMEM(listtype=(enum MolListType*) calloc(maxlist,sizeof(enum MolListType)));
	for(ll=0;ll<maxlist;ll++) listtype[ll]=MLTnone;

	CHECKMEM(live=(moleculeptr**) calloc(maxlist,sizeof(moleculeptr*)));
	for(ll=0;ll<maxlist;ll++) live[ll]=NULL;

	CHECKMEM(maxl=(int*) calloc(maxlist,sizeof(int)));
	for(ll=0;ll<maxlist;ll++) maxl[ll]=0;

	CHECKMEM(nl=(int*) calloc(maxlist,sizeof(int)));
	for(ll=0;ll<maxlist;ll++) nl[ll]=0;

	CHECKMEM(topl=(int*) calloc(maxlist,sizeof(int)));
	for(ll=0;ll<maxlist;ll++) topl[ll]=0;

	CHECKMEM(sortl=(int*) calloc(maxlist,sizeof(int)));
	for(ll=0;ll<maxlist;ll++) sortl[ll]=0;

	CHECKMEM(diffuselist=(int*) calloc(maxlist,sizeof(int)));
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
		CHECKMEM(listname[ll]=EmptyString());
		listtype[ll]=mlt; }

	for(ll=mols->maxlist;ll<maxlist;ll++) maxl[ll]=1;			// calculate maxl
	for(m=mols->topd;m<mols->nd;m++) {
		mptr=mols->dead[m];
		if(mptr && mptr->list>=mols->maxlist && mptr->list<maxlist) maxl[mptr->list]++; }
	for(ll=mols->maxlist;ll<maxlist;ll++) {
		maxl[ll]*=2;
		if(maxl[ll]>mols->maxd) maxl[ll]=mols->maxd; }

	for(ll=mols->maxlist;ll<maxlist;ll++) {			// allocate live lists
		CHECKMEM(live[ll]=(moleculeptr*) calloc(maxl[ll],sizeof(moleculeptr)));
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
	simLog(NULL,10,"Unable to allocate memory in mollistalloc");
	return -1; }


/* molexpandlist */
int molexpandlist(molssptr mols,int dim,int ll,int nspaces,int nmolecs) {
	moleculeptr *newlist,*oldlist;
	int m,nold,maxold,maxnew;

	if(!mols || ll>=mols->nlist) return 2;
	if(ll>=0 && nmolecs>0) return 2;							// can't add molecules to live list

	maxold=ll<0?mols->maxd:mols->maxl[ll];				// maxold is previous allocated size
	nold=ll<0?mols->nd:mols->nl[ll];							// nold is previous number of molecules
	oldlist=ll<0?mols->dead:mols->live[ll];				// oldlist is previous list

	maxnew=nspaces>0?maxold+nspaces:2*maxold+1;		// maxnew is new allocated size
	if(nold+nmolecs>maxnew) return 3;

	newlist=(moleculeptr*) calloc(maxnew,sizeof(moleculeptr));
	CHECKMEM(newlist);
	for(m=0;m<maxold;m++) newlist[m]=oldlist[m];
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
		for(m=mols->nd-1;m>=mols->topd;m--) {					// copy resurrected molecules higher on list
			newlist[m+nmolecs]=newlist[m];
			newlist[m]=NULL; }
		for(m=mols->topd;m<mols->topd+nmolecs;m++) {		// create new empty molecules
			newlist[m]=molalloc(dim);
			if(!newlist[m]) return 4; }
		mols->topd+=nmolecs;
		mols->nd+=nmolecs; }
	return 0;
failure:
	simLog(NULL,10,"Unable to allocate memory in molexpandlist");
	return 1; }


/* molssfree */
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
				for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1)) free(mols->color[i][ms]);
				free(mols->color[i]); }
		free(mols->color); }

	if(mols->display) {
		for(i=0;i<maxspecies;i++) free(mols->display[i]);
		free(mols->display); }

	if(mols->drift) {
		for(i=0;i<maxspecies;i++)
			if(mols->drift[i]) {
				for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1)) free(mols->drift[i][ms]);
				free(mols->drift[i]); }
		free(mols->drift); }

	if(mols->difm) {
		for(i=0;i<maxspecies;i++)
			if(mols->difm[i]) {
				for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1)) free(mols->difm[i][ms]);
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

/* molssoutput */
void molssoutput(simptr sim) {
	int nspecies,i,ll,same,sum;
	molssptr mols;
	char string[STRCHAR];
	double maxstep;
	enum MolecState ms;

	simLog(sim,2,"MOLECULE PARAMETERS\n");
	if(!sim || !sim->mols) {
		simLog(sim,2," No molecule superstructure defined\n\n");
		return; }
	mols=sim->mols;
	nspecies=mols->nspecies;

	if(mols->condition!=SCok)
		simLog(sim,7," Molecule superstructure condition: %s\n",simsc2string(mols->condition,string));
	simLog(sim,1," Next molecule serial number: %li\n",mols->serno);
	if(mols->gausstbl) simLog(sim,1," Table for Gaussian distributed random numbers has %i values\n",mols->ngausstbl);
	else simLog(sim,1," Table for Gaussian distributed random numbers has not been set up\n");

	simLog(sim,1," %i species allocated\n",mols->maxspecies-1);
	simLog(sim,2," %i species defined:\n",mols->nspecies-1);
	maxstep=-1;
	for(i=1;i<nspecies;i++) {
		simLog(sim,2," %s:\n",mols->spname[i]);
		simLog(sim,1,"  states used:");
		sum=0;
		for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1))
			if(mols->exist[i][ms]) {
				sum++;
				simLog(sim,1," %s",molms2string(ms,string)); }
		if(!sum) simLog(sim,1," none");
		simLog(sim,1,"\n");

		same=1;
		for(ms=MolecState(0);ms<MSMAX && same;ms=MolecState(ms + 1)) {
			if(mols->difc[i][ms]!=mols->difc[i][MSsoln]) same=0;
			if(mols->difm[i][ms] && !mols->difm[i][MSsoln]) same=0;
			if(!mols->difm[i][ms] && mols->difm[i][MSsoln]) same=0;
			if(mols->drift[i][ms] && !mols->drift[i][MSsoln]) same=0;
			if(!mols->drift[i][ms] && mols->drift[i][MSsoln]) same=0;
			if(mols->listlookup[i][ms]!=mols->listlookup[i][MSsoln]) same=0; }
		if(same) {
			if(mols->difstep[i][MSsoln]>maxstep) maxstep=mols->difstep[i][MSsoln];
			simLog(sim,2,"  all states: difc=%g, rms step=%g",mols->difc[i][MSsoln],mols->difstep[i][MSsoln]);
			if(mols->difm[i][MSsoln]) simLog(sim,2," (anisotropic)");
			if(mols->drift[i][MSsoln]) simLog(sim,2," (drift)");
			if(mols->listname) simLog(sim,2,", list=%s",mols->listname[mols->listlookup[i][MSsoln]]);
			simLog(sim,2,", number=%i\n",molcount(sim,i,MSall,NULL,-1)); }
		else {
			for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1))
				if(mols->exist[i][ms]) {
					if(mols->difstep[i][ms]>maxstep) maxstep=mols->difstep[i][ms];
					simLog(sim,2,"  %s: difc=%g, rms step=%g",molms2string(ms,string),mols->difc[i][ms],mols->difstep[i][ms]);
					if(mols->difm[i][ms]) simLog(sim,2," (anisotropic)");
					if(mols->drift[i][ms]) simLog(sim,2," (drift)");
					if(mols->listname) simLog(sim,2,", list=%s",mols->listname[mols->listlookup[i][ms]]);
					simLog(sim,2,", number=%i\n",molcount(sim,i,ms,NULL,-1)); }}

		if(sim->graphss) {
			same=1;
			for(ms=MolecState(0);ms<MSMAX && same;ms=MolecState(ms + 1)) {
				if(mols->display[i][ms]!=mols->display[i][MSsoln]) same=0;
				if(mols->color[i][ms][0]!=mols->color[i][MSsoln][0]) same=0;
				if(mols->color[i][ms][1]!=mols->color[i][MSsoln][1]) same=0;
				if(mols->color[i][ms][2]!=mols->color[i][MSsoln][2]) same=0; }
			if(same) {
				simLog(sim,2,"  all states:");
				if(mols->display[i][MSsoln])
					simLog(sim,2," color= %g,%g,%g, size=%g\n",mols->color[i][MSsoln][0],mols->color[i][MSsoln][1],mols->color[i][MSsoln][2],mols->display[i][MSsoln]);
				else simLog(sim,2," not displayed to graphics\n"); }
			else {
				for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1))
					if(mols->exist[i][ms]) {
						simLog(sim,2,"  %s:",molms2string(ms,string));
						if(mols->display[i][ms])
							simLog(sim,2," color= %g,%g,%g, display size= %g\n",mols->color[i][ms][0],mols->color[i][ms][1],mols->color[i][ms][2],mols->display[i][ms]);
						else simLog(sim,2," not displayed to graphics\n"); }}}}

	if(mols->dead==NULL) simLog(sim,1," No dead list allocated\n");
	simLog(sim,1," Dead list: allocated size=%i, number of molecules=%i",mols->maxd,mols->nd);
	if(mols->topd!=mols->nd) simLog(sim,1,", top value=%i",mols->topd);
	simLog(sim,1,"\n");
	if(mols->maxdlimit>=0) simLog(sim,1,"  limited to %i molecules\n",mols->maxdlimit);

	simLog(sim,2," %i molecule lists:\n",mols->nlist);
	for(ll=0;ll<mols->nlist;ll++) {
		if(mols->live[ll]==NULL) simLog(sim,1,"  list %i is not allocated\n",ll);
		simLog(sim,1,"  %s: type=%s, allocated size=%i, number of molecules=%i",mols->listname[ll],molmlt2string(mols->listtype[ll],string),mols->maxl[ll],mols->nl[ll]);
		if(mols->topl[ll]!=mols->nl[ll] && !mols->topl!=0) simLog(sim,1,", top value=%i",mols->topl[ll]);
		if(mols->sortl[ll]!=mols->nl[ll]) simLog(sim,1,", sort value=%i",mols->sortl[ll]);
		simLog(sim,1,"\n");
		simLog(sim,2,"%s%s%s",ll==0?"  ":" ",mols->listname[ll],ll==mols->nlist-1?"\n":","); }

	simLog(sim,1," Diffusion molecule lists:");
	for(ll=0;ll<mols->nlist;ll++)
		if(mols->diffuselist[ll]) simLog(sim,1," %s",mols->listname[ll]);
	simLog(sim,1,"\n");

	simLog(sim,2," Overall spatial resolution:");
	if(maxstep==-1 || mols->condition<SCok) simLog(sim,2," not computed\n");
	else simLog(sim,2," %g\n",maxstep);
	simLog(sim,2,"\n");
	return; }


/* writemols */
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
	if(sim->mols->maxdlimit>=0)
		fprintf(fptr,"max_mol %i\n",sim->mols->maxdlimit);
	fprintf(fptr,"gauss_table_size %i\n\n",mols->ngausstbl);

	for(ll=0;ll<mols->nlist;ll++)
		if(mols->listtype[ll]==MLTsystem)
			fprintf(fptr,"molecule_lists %s\n",mols->listname[ll]);
	fprintf(fptr,"\n");
	
	for(i=1;i<mols->nspecies;i++) {
		val0=mols->difc[i][0];
		for(ms=MolecState(1);ms<MSMAX && mols->difc[i][ms]==val0;ms=MolecState(ms + 1));
		if(ms==MSMAX) fprintf(fptr,"difc %s(all) %g\n",spname[i],val0);
		else {
			for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1))
				if(mols->difc[i][ms]>0)
					fprintf(fptr,"difc %s(%s) %g\n",spname[i],molms2string(ms,string),mols->difc[i][ms]); }
		
		for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1)) {
			if(mols->difm[i][ms]) {
				fprintf(fptr,"difm %s(%s)",spname[i],molms2string(ms,string));
				for(d=0;d<dim*dim;d++)
					fprintf(fptr," %g",mols->difm[i][ms][d]);
				fprintf(fptr,"\n"); }}
		
		for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1)) {
			if(mols->drift[i][ms]) {
				fprintf(fptr,"drift %s(%s)",spname[i],molms2string(ms,string));
				for(d=0;d<dim;d++)
					fprintf(fptr," %g",mols->drift[i][ms][d]);
				fprintf(fptr,"\n"); }}
		
		if(mols->nlist) {
			ll=mols->listlookup[i][0];
			for(ms=MolecState(1);ms<MSMAX && mols->listlookup[i][ms]==ll;ms=MolecState(ms + 1));
			if(ms==MSMAX) fprintf(fptr,"mol_list %s(all) %s\n",spname[i],mols->listname[ll]);
			else {
				for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1))
					fprintf(fptr,"mol_list %s(%s) %s\n",spname[i],molms2string(ms,string),mols->listname[mols->listlookup[i][ms]]); }}
		
		val0=mols->display[i][0];
		for(ms=MolecState(1);ms<MSMAX && mols->display[i][ms]==val0;ms=MolecState(ms + 1));
		if(ms==MSMAX) fprintf(fptr,"display_size %s(all) %g\n",spname[i],val0);
		else {
			for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1))
				fprintf(fptr,"display_size %s(%s) %g\n",spname[i],molms2string(ms,string),mols->display[i][ms]); }
		
		val0=mols->color[i][0][0];
		val1=mols->color[i][0][1];
		val2=mols->color[i][0][2];
		for(ms=MolecState(1);ms<MSMAX && mols->color[i][ms][0]==val0 && mols->color[i][ms][1]==val1 && mols->color[i][ms][2]==val2;ms=MolecState(ms + 1));
		if(ms==MSMAX) fprintf(fptr,"color %s(all) %g %g %g\n",spname[i],val0,val1,val2);
		else {
			for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1))
				fprintf(fptr,"color %s(%s) %g %g %g\n",spname[i],molms2string(ms,string),mols->color[i][ms][0],mols->color[i][ms][1],mols->color[i][ms][2]); }
		fprintf(fptr,"\n"); }
	return; }


/* writemolecules */
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


/* checkmolparams */
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
		simLog(sim,7," WARNING: molecule structure %s\n",simsc2string(mols->condition,string)); }

	for(ll=0;ll<mols->nlist;ll++) {				// check molecule list sorting
		for(m=0;m<mols->nl[ll];m++) {
			mptr=mols->live[ll][m];
			if(!mptr) {error++;simLog(sim,10," SMOLDYN BUG: NULL molecule in live list %i at %i\n",ll,m);}
			else if(mptr->list!=ll) {warn++;simLog(sim,9," WARNING: mis-sorted molecule in live list %i at %i\n",ll,m);}
			else if(!mptr->ident) {warn++;simLog(sim,5," WARNING: empty molecule in live list %i at %i\n",ll,m);} }
		for(;m<mols->maxl[ll];m++) {
			mptr=mols->live[ll][m];
			if(mptr) {error++;simLog(sim,10," SMOLDYN BUG: misplaced molecule in live list %i at %i\n",ll,m);} }}

	for(m=0;m<mols->topd;m++) {
		mptr=mols->dead[m];
		if(!mptr) {error++;simLog(sim,10," SMOLDYN BUG: NULL molecule in dead list at %i\n",m);}
		else if(mptr->list!=-1) {error++;simLog(sim,10," SMOLDYN BUG: mis-sorted molecule in dead list at %i (species %i, serno %li)\n",m,mptr->ident,mptr->serno);}
		else if(mptr->ident) {error++;simLog(sim,10," SMOLDYN BUG: live molecule in dead list at %i\n",m);} }
	for(;m<mols->nd;m++) {
		mptr=mols->dead[m];
		if(!mptr) {error++;simLog(sim,10," SMOLDYN BUG: NULL molecule in resurrected list at %i\n",m);}
		else if(mptr->list==-1) {error++;simLog(sim,10," SMOLDYN BUG: mis-sorted molecule in resurrected list at %i\n",m);}
		else if(!mptr->ident) {error++;simLog(sim,10," BUG: dead molecule in resurrected list at %i\n",m);} }
	for(;m<mols->maxd;m++) {
		mptr=mols->dead[m];
		if(mptr) {error++;simLog(sim,10," SMOLDYN BUG: misplaced molecule in dead list at %i\n",m);} }

	for(ll=0;ll<mols->nlist;ll++)
		for(m=0;m<mols->nl[ll];m++)	{									// check for molecules outside system
			mptr=mols->live[ll][m];
			if(!posinsystem(sim,mptr->pos)) {
				simLog(sim,5," WARNING: molecule of type '%s' is outside system volume\n",spname[mptr->ident]);
				warn++; }}

	for(i=1;i<nspecies;i++)														// check for asymmetric diffusion matrices
		for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1))
			if(mols->difm[i][ms]) {
				dotMMD(mols->difm[i][ms],mols->difm[i][ms],m2,dim,dim,dim);
				if(!issymmetricMD(m2,dim)) {
					simLog(sim,5," WARNING: diffusion matrix for molecule %s (%s) is asymmetric\n",spname[i],molms2string(ms,string));
					warn++; }}

	for(i=1;i<nspecies;i++) {													// check for unused molecules
		sum=0;
		for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1)) sum+=mols->exist[i][ms];
		if(!sum) {
			simLog(sim,5," WARNING: molecule %s is never used\n",spname[i]);
			warn++; }}

	if(sim->graphss && sim->graphss->graphics>1) {		// check for molecules that may not display
		diag=systemdiagonal(sim);
		for(i=1;i<nspecies;i++)
			for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1)) {
				if(mols->display[i][ms]>0.1*diag) {
					simLog(sim,5," WARNING: very large display size for molecule %s (%s)\n",spname[i],molms2string(ms,string));
					warn++; }
				if(mols->display[i][ms]<0.001*diag) {
					simLog(sim,5," WARNING: very small display size for molecule %s (%s)\n",spname[i],molms2string(ms,string));
					warn++; }}}

	if(warnptr) *warnptr=warn;
	return error; }


/******************************************************************************/
/********************************* structure set up ***************************/
/******************************************************************************/


/* molenablemols */
int molenablemols(simptr sim,int maxspecies) {
	molssptr mols;
	int er;

	if(sim->mols) {									// check for redundant function call
		if(maxspecies==-1) {
			if(sim->mols->nspecies<sim->mols->maxspecies) return 0; }
		else {
			if(maxspecies==sim->mols->maxspecies) return 0;
			if(maxspecies<sim->mols->maxspecies) return 2; }}

	if(maxspecies<0) maxspecies=sim->mols?sim->mols->maxspecies*2+1:5;	// need to initialize or increase maxspecies
	mols=molssalloc(sim->mols,maxspecies);
	if(!mols) return 1;
	sim->mols=mols;
	mols->sim=sim;
	molsetcondition(sim->mols,SClists,0);
	boxsetcondition(sim->boxs,SClists,0);
	er=rxnexpandmaxspecies(sim,maxspecies+1);
	if(er) return 1;
	er=surfexpandmaxspecies(sim,maxspecies+1);
	if(er) return 1;
	rxnsetcondition(sim,-1,SClists,0);
	surfsetcondition(sim->srfss,SClists,0);
	portsetcondition(sim->portss,SClists,0);
	return 0; }


/* molsetcondition */
void molsetcondition(molssptr mols,enum StructCond cond,int upgrade) {
	if(!mols) return;
	if(upgrade==0 && mols->condition>cond) mols->condition=cond;
	else if(upgrade==1 && mols->condition<cond) mols->condition=cond;
	else if(upgrade==2) mols->condition=cond;
	if(mols->sim && mols->condition<mols->sim->condition) {
		cond=mols->condition;
		simsetcondition(mols->sim,cond==SCinit?SClists:cond,0); }
	return; }



/* addmollist */
int addmollist(simptr sim,const char *nm,enum MolListType mlt) {
	int ll,er;
	molssptr mols;

	if(!sim->mols) {
		er=molenablemols(sim,-1);
		if(er) return -1; }
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


/* molsetmaxspecies */
int molsetmaxspecies(simptr sim,int max) {
	return molenablemols(sim,max); }


/* molsetmaxmol */
int molsetmaxmol(simptr sim,int max) {
	int er;

	if(!sim->mols) {
		er=molenablemols(sim,-1);
		if(er) return er; }
	if(max>=0 && max<sim->mols->maxd) return 5;
	sim->mols->maxdlimit=max;
	return 0; }


/* moladdspecies */
int moladdspecies(simptr sim,const char *nm) {
	molssptr mols;
	int found,er;

	er=molenablemols(sim,-1);
	if(er) return er;
	mols=sim->mols;
	if(!strcmp(nm,"empty")) return -4;
	if(strchr(nm,'?') || strchr(nm,'*')) return -6;

	found=stringfind(mols->spname,mols->nspecies,nm);
	if(found>=0) return -5;

	strncpy(mols->spname[mols->nspecies++],nm,STRCHAR);
	molsetcondition(mols,SClists,0);
	rxnsetcondition(sim,-1,SClists,0);
	surfsetcondition(sim->srfss,SClists,0);
	return mols->nspecies-1; }


/* molsetexpansionflag */
int molsetexpansionflag(simptr sim,int i,int flag) {
	int i2;

	if(!sim->mols) return 2;
	if(i==-1) {
		for(i2=1;i2<sim->mols->nspecies;i2++)
			sim->mols->expand[i2]=flag; }
	else if(i<0 || i>=sim->mols->nspecies) return 3;
	else sim->mols->expand[i]=flag;
	return 0; }


/* molsupdateparams */
int molsupdateparams(molssptr mols,double dt) {
	int i,ll;
	enum MolecState ms;

	for(ll=0;ll<mols->nlist;ll++) mols->diffuselist[ll]=0;		// set diffuselist
	for(i=0;i<mols->nspecies;i++)
		for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1)) {
			if(mols->difc[i][ms]>0 || mols->difm[i][ms] || mols->drift[i][ms])
				mols->diffuselist[mols->listlookup[i][ms]]=1; }

	for(i=0;i<mols->nspecies;i++)					// calculate difstep
		for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1))
			mols->difstep[i][ms]=sqrt(2.0*mols->difc[i][ms]*dt);

	return 0; }


/* molsupdatelists */
int molsupdatelists(simptr sim) {
	int i,ll,m,ndif,nfix,ok,er;
	enum MolecState ms;
	molssptr mols;
	moleculeptr mptr;
	
	mols=sim->mols;

	er=molssetgausstable(sim,-1);				// gaussian lookup table
	if(er) return 1;

	for(i=1;i<mols->nspecies;i++)					// set exist values
		for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1))
			mols->exist[i][ms]=0;
	for(m=mols->topd;m<mols->nd;m++) {
		mptr=mols->dead[m];
		mols->exist[mptr->ident][mptr->mstate]=1; }
	for(ll=0;ll<mols->nlist;ll++)
		for(m=0;m<mols->nl[ll];m++) {
			mptr=mols->live[ll][m];
			mols->exist[mptr->ident][mptr->mstate]=1; }
	for(i=1;i<mols->nspecies;i++) {
		for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1)) {
			if(mols->exist[i][ms]==0 && rxnisprod(sim,i,ms,0)) mols->exist[i][ms]=1;
			if(mols->exist[i][ms]==0 && issurfprod(sim,i,ms)) mols->exist[i][ms]=1; }
		if(mols->exist[i][MSsoln]==0 && rxnisprod(sim,i,MSbsoln,0)) mols->exist[i][MSsoln]=1;
		if(mols->exist[i][MSsoln]==0 && issurfprod(sim,i,MSbsoln)) mols->exist[i][MSsoln]=1; }

	for(ll=0;ll<mols->nlist;ll++)					// create system molecule lists if none yet
		if(mols->listtype[ll]==MLTsystem) ll=mols->nlist+1;
	if(ll==mols->nlist && mols->maxd>0 && mols->nspecies>1) {
		ndif=nfix=0;
		for(i=1;i<mols->nspecies;i++)
			for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1)) {
				if(mols->difc[i][ms]>0) ndif=1;
				else nfix=1; }
		if(ndif) {
			ll=addmollist(sim,"diffuselist",MLTsystem);
			if(ll<0) return 1;
			molsetlistlookup(sim,-7,MSall,ll); }
		if(nfix) {
			ll=addmollist(sim,"fixedlist",MLTsystem);
			if(ll<0) return 1;
			molsetlistlookup(sim,-8,MSall,ll); }}

	ok=1;															// set any list lookup values that weren't done yet
	for(i=1;i<mols->nspecies && ok;i++)
		for(ms=MolecState(0);ms<MSMAX && ok;ms=MolecState(ms + 1))
			if(mols->listlookup[i][ms]<0)
				ok=0;
	if(!ok) {
		ll=stringfind(mols->listname,mols->nlist,"unassignedlist");
		if(ll<0) {
			ll=addmollist(sim,"unassignedlist",MLTsystem);
			if(ll<0) return 1; }
		for(i=1;i<mols->nspecies;i++)
			for(ms=MolecState(0);ms<MSMAX;ms=MolecState(ms + 1))
				if(mols->listlookup[i][ms]<0)
					molsetlistlookup(sim,i,ms,ll); }

	for(m=mols->topd;m<mols->nd;m++) {		// set molecule list values for molecules in dead list
		mptr=mols->dead[m];
		mptr->list=mols->listlookup[mptr->ident][mptr->mstate]; }

	return 0; }


/* molsupdate */
int molsupdate(simptr sim) {
	int er;
	molssptr mols;

	mols=sim->mols;
	if(mols) {
		if(mols->condition<=SClists) {
			er=molsupdatelists(sim);
			if(er) return er;
			molsetcondition(mols,SCparams,1); }
		if(mols->condition==SCparams) {
			er=molsupdateparams(mols,sim->dt);
			if(er) return er;
			molsetcondition(mols,SCok,1); }}
	return 0; }


/******************************************************************************/
/*********************** adding and removing molecules ************************/
/******************************************************************************/


/* molkill */
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


/* getnextmol */
moleculeptr getnextmol(molssptr mols) {
	moleculeptr mptr;
	int er,nmol;

	if(mols->topd==0) {
		if(mols->maxdlimit>=0 && mols->maxd>=mols->maxdlimit) return NULL;
		nmol=mols->maxd+1;
		if(mols->maxdlimit>=0 && mols->maxd+nmol>mols->maxdlimit)
			nmol=mols->maxdlimit-mols->maxd;
		er=molexpandlist(mols,mols->sim->dim,-1,nmol,nmol);
		if(er) return NULL; }
	mptr=mols->dead[--mols->topd];
	mptr->serno=mols->serno++;
	return mptr; }


/* newestmol */
moleculeptr newestmol(molssptr mols) {
	return mols->dead[mols->topd-1]; }


/* addmol */
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
	molsetexist(sim,ident,MSsoln,1);
	if(sort)
		if(molsort(sim)) return 1;
	return 0; }


/* addsurfmol */
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
		areatable=(double*) calloc(totpanel,sizeof(double));
		if(!areatable) return 1;
		paneltable=(panelptr*) calloc(totpanel,sizeof(panelptr));
		if(!paneltable) {free(areatable);return 1; }

		slo=(surface>=0)?surface:0;
		shi=(surface>=0)?surface+1:sim->srfss->nsrf;
		pslo=(ps!=PSall)?ps:0;
		pshi=(ps!=PSall)?ps+1:PSMAX;

		pindex=0;																						// fill in area lookup tables
		area=0;
		for(s=slo;s<shi;s++)
			for(ps=PanelShape(pslo);ps<pshi;ps=PanelShape(ps + 1)) {
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

	molsetexist(sim,ident,ms,1);
	return 0; }


/* addcompartmol */
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
	molsetexist(sim,ident,MSsoln,1);
	return 0; }


/******************************************************************************/
/*************************** core simulation functions ************************/
/******************************************************************************/


/* molsort */
int molsort(simptr sim) {
	molssptr mols;
	int nlist,*maxl,*nl,*topl,*sortl,m,ll,ll2;
	moleculeptr *dead,**live,*mlist,mptr;
	enum MolListType *listtype;
	boxptr bptr;

	if(!sim->mols) return 0;
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
							simLog(sim,10,"out of memory in molsort\n");return 1;}
					live[ll2][nl[ll2]++]=mptr;
					mlist[m]=NULL;
					if(listtype[ll2]==MLTsystem) {
						if(bptr) mptr->box=bptr;
						else mptr->box=pos2box(sim,mptr->pos);
						if(boxaddmol(mptr,ll2)) {
							simLog(sim,10,"out of memory in molsort\n");return 1;} }}

				mlist[m]=mlist[--topl[ll]];				// compact original live list
				mlist[topl[ll]]=mlist[--nl[ll]];
				mlist[nl[ll]]=NULL;
				m--; }}}

	for(m=mols->topd;m<mols->nd;m++) {		// move molecules from resurrected to reborn
		mptr=dead[m];
		ll2=mptr->list;
		if(nl[ll2]==maxl[ll2])
			if(molexpandlist(mols,sim->dim,ll2,-1,0)) {
				simLog(sim,10,"out of memory in molsort\n");return 1;}
		live[ll2][nl[ll2]++]=mptr;
		dead[m]=NULL;
		if(listtype[ll2]==MLTsystem) {
			if(boxaddmol(mptr,ll2)) {
				simLog(sim,10,"out of memory in molsort\n");return 1;} }}
	mols->nd=mols->topd;

	for(ll=0;ll<nlist;ll++)								// reset sortl indicies
		sortl[ll]=nl[ll];

	return 0; }


/* diffuse */
int diffuse(simptr sim) {
	molssptr mols;
	int ll,m,d,nmol,dim,i,ngtablem1;
	enum MolecState ms;
	double flt1;
	double v1[DIMMAX],v2[DIMMAX],**difstep,***difm,***drift,epsilon,margin,neighdist,*gtable,dt;
	moleculeptr *mlist;
	moleculeptr mptr;

	if(!sim->mols) return 0;
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
	margin=(sim->srfss)?sim->srfss->margin:0;
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
					for(d=0;d<dim;d++) mptr->pos[d]+=drift[i][ms][d]*dt; }
				if(mptr->mstate!=MSsoln)
					movemol2closepanel(sim,mptr,dim,epsilon,neighdist,margin); }}

	return 0; }


//?? Start of threading code.  All of this is undocumented.
typedef struct ll_threading_struct {
	simptr sim;
	int ll_ndx;
	int min_ndx;
	int max_ndx;
} *diffuse_struct;


/* Diffuse live list. */
void* diffuseLiveList_threaded(void* ndx_as_void) {
#ifndef THREADING
	return NULL;
#else
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
#endif
}


/* diffuse_threaded */
int diffuse_threaded(simptr sim) {
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
	
	int num_threads = sim->threads->nthreads;
	int current_thread_ndx = 0;
	
	for(livelist_ndx = 0; livelist_ndx != mols->nlist; ++livelist_ndx) {
		if( mols->diffuselist[livelist_ndx] ) {
			
			int per_thread_size = mols->nl[livelist_ndx] / num_threads;
			int currentNdx = 0;
			
			int thread_ndx;
			for( thread_ndx = 0; thread_ndx != num_threads - 1; ++thread_ndx) {
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
				
		    pthread_create(sim->threads->thread[thread_ndx]->thread_id, NULL, diffuseLiveList_threaded, (void*) current_thread_input_stack->stack_data); }
			
			int ndx;
			
			for( ndx = 0; ndx != current_thread_ndx; ++ndx) {
		    pthread_join( *((pthread_t*) sim->threads->thread[thread_ndx]->thread_id), NULL); }}}
	
	return 0; 
#endif
}

//?? End of threading code.

