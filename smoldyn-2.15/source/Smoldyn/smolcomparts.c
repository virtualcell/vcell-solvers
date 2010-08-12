/* Steven Andrews, started 10/22/01.
This is a library of functions for the Smoldyn program.  See documentation
called Smoldyn_doc1.doc and Smoldyn_doc2.doc.
Copyright 2003-2007 by Steven Andrews.  This work is distributed under the terms
of the Gnu Lesser General Public License (LGPL). */

#include <stdio.h>
#include <math.h>
#include <string.h>
#include "random2.h"
#include "RnSort.h"
#include "smoldyn.h"
#include "Zn.h"

#define CHECK(A) if(!(A)) goto failure; else (void)0
#define CHECKS(A,B) if(!(A)) {strncpy(erstr,B,STRCHAR-1);erstr[STRCHAR-1]='\0';goto failure;} else (void)0


/******************************************************************************/
/********************************* Compartments *******************************/
/******************************************************************************/


/******************************************************************************/
/********************************* enumerated types ***************************/
/******************************************************************************/


/* cmptstring2cl.  Converts compartment logic symbol string to an enumerated
compartment logic type.  Input strings can be: “equal”, “equalnot”, “and”, “or”,
“xor”, “andnot”, or “ornot”.  Anything else results in CLnone. */
enum CmptLogic cmptstring2cl(char *string) {
	enum CmptLogic ans;

	if(!strcmp(string,"equal")) ans=CLequal;
	else if(!strcmp(string,"equalnot")) ans=CLequalnot;
	else if(!strcmp(string,"and")) ans=CLand;
	else if(!strcmp(string,"or")) ans=CLor;
	else if(!strcmp(string,"xor")) ans=CLxor;
	else if(!strcmp(string,"andnot")) ans=CLandnot;
	else if(!strcmp(string,"ornot")) ans=CLornot;
	else ans=CLnone;
	return ans; }


/* cmptcl2string.  Converts enumerated compartment logic type to a string, in
string, which must be pre-allocated.  Output strings are “equal”, “equalnot”,
“and”, “or”, “xor”, “andnot”, “ornot” or “none”.  string is returned to allow
for function nesting.
*/
char *cmptcl2string(enum CmptLogic cls,char *string) {
	if(cls==CLequal) strcpy(string,"equal");
	else if(cls==CLequalnot) strcpy(string,"equalnot");
	else if(cls==CLand) strcpy(string,"and");
	else if(cls==CLor) strcpy(string,"or");
	else if(cls==CLxor) strcpy(string,"xor");
	else if(cls==CLandnot) strcpy(string,"andnot");
	else if(cls==CLornot) strcpy(string,"ornot");
	else strcpy(string,"none");
	return string; }


/******************************************************************************/
/****************************** low level utilities ***************************/
/******************************************************************************/

/* posincompart.  Tests if position pos is in compartment cmpt, returning 1 if
so and 0 if not.  This includes composed compartment logic tests.  It does not
use the compartment box list.  This function is quite efficient for surfaces
with few panels, but inefficient if surfaces have lots of panels. */
int posincompart(simptr sim,double *pos,compartptr cmpt) {
	int s,p,k,incmpt,pcross,cl,incmptl;
	enum PanelShape ps;
	surfaceptr srf;
	double crsspt[DIMMAX];
	enum CmptLogic sym;

	incmpt=0;
	for(k=0;k<cmpt->npts&&incmpt==0;k++) {
		pcross=0;
		for(s=0;s<cmpt->nsrf&&!pcross;s++) {
			srf=cmpt->surflist[s];
			for(ps=0;ps<PSMAX&&!pcross;ps++)
				for(p=0;p<srf->npanel[ps]&&!pcross;p++)
					if(lineXpanel(pos,cmpt->points[k],srf->panels[ps][p],sim->dim,crsspt,NULL,NULL,NULL,NULL)) 
						pcross=1; }
		if(pcross==0) incmpt=1; }

	for(cl=0;cl<cmpt->ncmptl;cl++) {
		incmptl=posincompart(sim,pos,cmpt->cmptl[cl]);
		sym=cmpt->clsym[cl];
		if(sym==CLequal) incmpt=incmptl;
		else if(sym==CLequalnot) incmpt=!incmptl;
		else if(sym==CLand) incmpt=incmpt&&incmptl;
		else if(sym==CLor) incmpt=incmpt||incmptl;
		else if(sym==CLxor) incmpt=(incmpt!=incmptl);
		else if(sym==CLandnot) incmpt=incmpt&&!incmptl;
		else if(sym==CLornot) incmpt=incmpt||!incmptl; }

	return incmpt; }


/* compartrandpos.  Returns a random position, in pos, within compartment cmpt.
 Returns 0 and a valid position, unless a point cannot be found, in which case
 this returns 1. */
int compartrandpos(simptr sim,double *pos,compartptr cmpt) {
	static int ptmax=10000;
	int d,dim,i,done,k,bc;
	boxptr bptr;

	if(cmpt->npts==0&&cmpt->ncmptl==0) return 1;
	dim=sim->dim;

	done=0;
	if(cmpt->nbox) {
		bc=intrandpD(cmpt->nbox,cmpt->cumboxvol);
		bptr=cmpt->boxlist[bc];
		for(i=0;i<ptmax&&!done;i++) {
			boxrandpos(sim,pos,bptr);
			if(posincompart(sim,pos,cmpt)) done=1; }}
	else {
		for(i=0;i<ptmax&&!done;i++) {
			for(d=0;d<dim;d++) pos[d]=unirandCCD(sim->wlist[2*d]->pos,sim->wlist[2*d+1]->pos);
			if(posincompart(sim,pos,cmpt)) done=1; }}
	if(!done&&cmpt->npts>0) {
		k=intrand(cmpt->npts);
		for(d=0;d<sim->dim;d++) pos[d]=cmpt->points[k][d];
		done=1; }
	if(!done) return 1;
	return 0; }


/******************************************************************************/
/******************************* memory management ****************************/
/******************************************************************************/

/* compartalloc.  Allocates memory for a compartment.  All arrays are set to
NULL, and not allocated.  Returns the compartment or NULL if unable to allocate
memory. */
compartptr compartalloc(void) {
	compartptr cmpt;

	cmpt=(compartptr)malloc(sizeof(struct compartstruct));
	if(!cmpt) return NULL;
	cmpt->cname=NULL;
	cmpt->nsrf=0;
	cmpt->surflist=NULL;
	cmpt->npts=0;
	cmpt->points=NULL;
	cmpt->ncmptl=0;
	cmpt->cmptl=NULL;
	cmpt->clsym=NULL;
	cmpt->volume=0;
	cmpt->maxbox=0;
	cmpt->nbox=0;
	cmpt->boxlist=NULL;
	cmpt->boxfrac=NULL;
	cmpt->cumboxvol=NULL;
	return cmpt; }


/* compartfree. Frees a compartment, including all of its arrays. */
void compartfree(compartptr cmpt) {
	int k;

	if(!cmpt) return;
	free(cmpt->cumboxvol);
	free(cmpt->boxfrac);
	free(cmpt->boxlist);
	free(cmpt->clsym);
	free(cmpt->cmptl);
	if(cmpt->npts&&cmpt->points)
		for(k=0;k<cmpt->npts;k++) free(cmpt->points[k]);
	free(cmpt->points);
	free(cmpt->surflist);
	free(cmpt);
	return; }


/* compartssalloc.  Allocates a compartment superstructure as well as maxcmpt
compartments.  Space is allocated and initialized for compartment names.
Returns the compartment superstructure or NULL if unable to allocate memory. */
compartssptr compartssalloc(int maxcmpt) {
	compartssptr cmptss;
	int c;

	cmptss=(compartssptr)malloc(sizeof(struct compartsuperstruct));
	if(!cmptss) return NULL;
	cmptss->condition=SCinit;
	cmptss->sim=NULL;
	cmptss->maxcmpt=maxcmpt;
	cmptss->ncmpt=0;
	cmptss->cnames=NULL;
	cmptss->cmptlist=NULL;

	CHECK(cmptss->cnames=(char**)calloc(maxcmpt,sizeof(char*)));
	for(c=0;c<maxcmpt;c++) cmptss->cnames[c]=NULL;
	for(c=0;c<maxcmpt;c++) {
		CHECK(cmptss->cnames[c]=EmptyString()); }

	CHECK(cmptss->cmptlist=(compartptr*)calloc(maxcmpt,sizeof(compartptr)));
	for(c=0;c<maxcmpt;c++) cmptss->cmptlist[c]=NULL;
	for(c=0;c<maxcmpt;c++) {
		CHECK(cmptss->cmptlist[c]=compartalloc());
		cmptss->cmptlist[c]->cmptss=cmptss;
		cmptss->cmptlist[c]->cname=cmptss->cnames[c]; }

	return cmptss;

 failure:
 	compartssfree(cmptss);
 	return NULL; }


/* compartssfree.  Frees a compartment superstructure, including all
compartments and everything within them. */
void compartssfree(compartssptr cmptss) {
	int c;

	if(!cmptss) return;
	if(cmptss->maxcmpt&&cmptss->cmptlist)
		for(c=0;c<cmptss->maxcmpt;c++) compartfree(cmptss->cmptlist[c]);
	free(cmptss->cmptlist);
	if(cmptss->maxcmpt&&cmptss->cnames)
		for(c=0;c<cmptss->maxcmpt;c++) free(cmptss->cnames[c]);
	free(cmptss->cnames);
	free(cmptss);
	return; }


/******************************************************************************/
/***************************** data structure output **************************/
/******************************************************************************/

/* compartoutput.  Displays all important information about all compartments to
stdout. */
void compartoutput(simptr sim) {
	compartssptr cmptss;
	compartptr cmpt;
	int c,dim,s,k,d,cl;
	char string[STRCHAR];

	cmptss=sim->cmptss;
	if(!cmptss) return;
	printf("COMPARTMENT PARAMETERS\n");
	dim=sim->dim;
	printf(" Compartments allocated: %i, compartments defined: %i\n",cmptss->maxcmpt,cmptss->ncmpt);
	for(c=0;c<cmptss->ncmpt;c++) {
		cmpt=cmptss->cmptlist[c];
		printf(" Compartment: %s\n",cmptss->cnames[c]);
		printf("  %i bounding surfaces:\n",cmpt->nsrf);
		for(s=0;s<cmpt->nsrf;s++)
			printf("   %s\n",cmpt->surflist[s]->sname);
		printf("  %i interior-defining points:\n",cmpt->npts);
		for(k=0;k<cmpt->npts;k++) {
			printf("   %i: (",k);
			for(d=0;d<dim-1;d++)
				printf("%g,",cmpt->points[k][d]);
			printf("%g)\n",cmpt->points[k][d]); }
		printf("  %i logically combined compartments\n",cmpt->ncmptl);
		for(cl=0;cl<cmpt->ncmptl;cl++)
			printf("   %s %s\n",cmptcl2string(cmpt->clsym[cl],string),cmpt->cmptl[cl]->cname);
		printf("  volume: %g\n",cmpt->volume);
		printf("  %i virtual boxes listed\n",cmpt->nbox); }
	printf("\n");
	return; }


/* writecomparts.  Prints information about all compartments to file fptr using
a format that allows the compartments to read as a configuration file. */
void writecomparts(simptr sim,FILE *fptr) {
	compartssptr cmptss;
	compartptr cmpt;
	int c,s,k,d,cl;
	char string[STRCHAR];

	cmptss=sim->cmptss;
	if(!cmptss) return;
	fprintf(fptr,"# Compartment parameters\n");
	fprintf(fptr,"max_compartment %i\n",cmptss->maxcmpt);
	for(c=0;c<cmptss->ncmpt;c++) {
		cmpt=cmptss->cmptlist[c];
		fprintf(fptr,"start_compartment %s\n",cmpt->cname);
		for(s=0;s<cmpt->nsrf;s++)
			fprintf(fptr,"surface %s\n",cmpt->surflist[s]->sname);
		for(k=0;k<cmpt->npts;k++) {
			fprintf(fptr,"point");
			for(d=0;d<sim->dim;d++)
				fprintf(fptr," %g",cmpt->points[k][d]);
			fprintf(fptr,"\n"); }
		for(cl=0;cl<cmpt->ncmptl;cl++)
			fprintf(fptr,"compartment %s %s\n",cmptcl2string(cmpt->clsym[cl],string),cmpt->cmptl[cl]->cname);
		fprintf(fptr,"end_compartment\n\n"); }
	return; }


/* checkcompartparams.  This checks a few compartment parameters. */
int checkcompartparams(simptr sim,int *warnptr) {
	int error,warn,c;
	compartssptr cmptss;
	compartptr cmpt;
	char string[STRCHAR];

	error=warn=0;
	cmptss=sim->cmptss;
	if(!cmptss) {
		if(warnptr) *warnptr=warn;
		return 0; }

	if(cmptss->condition!=SCok) {
		warn++;
		printf(" WARNING: compartment structure %s\n",simsc2string(cmptss->condition,string)); }

	for(c=0;c<cmptss->ncmpt;c++) {
		cmpt=cmptss->cmptlist[c];
		if(cmpt->volume<=0) {warn++;printf(" WARNING: compartment %s has 0 volume\n",cmpt->cname);}
		if(cmpt->nbox==0) {warn++;printf(" WARNING: compartment %s overlaps no virtual boxes\n",cmpt->cname);}
		if(cmpt->nbox>0&&cmpt->cumboxvol[cmpt->nbox-1]!=cmpt->volume) {error++;printf(" ERROR: compartment %s box volumes do not add to compartment volume\n",cmpt->cname);} }
	if(warnptr) *warnptr=warn;
	return error; }


/******************************************************************************/
/******************************** structure set up ****************************/
/******************************************************************************/


/* compartsetcondition.  Sets the compartment superstructure condition to cond,
if appropriate.  Set upgrade to 1 if this is an upgrade, to 0 if this is a
downgrade, or to 2 to set the condition independent of its current value. */
void compartsetcondition(compartssptr cmptss,enum StructCond cond,int upgrade) {
	if(!cmptss) return;
	if(upgrade==0 && cmptss->condition>cond) cmptss->condition=cond;
	else if(upgrade==1 && cmptss->condition<cond) cmptss->condition=cond;
	else if(upgrade==2) cmptss->condition=cond;
	if(cmptss->condition<cmptss->sim->condition) {
		cond=cmptss->condition;
		simsetcondition(cmptss->sim,cond==SCinit?SClists:cond,0); }
	return; }


/* compartaddsurf.  Adds surface srf to the compartment cmpt.  This increments
nsrf and appends the surface to srflist.  Returns 0 for success, 1 if memory
could not be allocated, and 2 if the surface was already in the list (in which
case it is not added again). */
int compartaddsurf(compartptr cmpt,surfaceptr srf) {
	int s;
	surfaceptr *newsurflist;

	newsurflist=(surfaceptr*)calloc(cmpt->nsrf+1,sizeof(surfaceptr));
	if(!newsurflist) return 1;
	for(s=0;s<cmpt->nsrf;s++) {
		if(cmpt->surflist[s]==srf) {free(newsurflist);return 2;}
		newsurflist[s]=cmpt->surflist[s]; }
	newsurflist[s]=srf;
	cmpt->nsrf++;
	free(cmpt->surflist);
	cmpt->surflist=newsurflist;
	compartsetcondition(cmpt->cmptss,SCparams,0);
	cmpt->nbox=0;
	cmpt->volume=0;
	return 0; }


/* compartaddpoint.  Adds point point to the compartment cmpt, in a dim
dimensional system.  This increments npts and appends the point to points.
Returns 0 for success and 1 if memory could not be allocated. */
int compartaddpoint(compartptr cmpt,int dim,double *point) {
	int d,k;
	double **newpoints;

	CHECK(newpoints=(double**)calloc(cmpt->npts+1,sizeof(double*)));
	for(k=0;k<cmpt->npts;k++)
		newpoints[k]=cmpt->points[k];
	CHECK(newpoints[k]=(double*)calloc(dim,sizeof(double)));
	for(d=0;d<dim;d++) newpoints[k][d]=point[d];
	cmpt->npts++;
	free(cmpt->points);
	cmpt->points=newpoints;
	compartsetcondition(cmpt->cmptss,SCparams,0);
	cmpt->nbox=0;
	cmpt->volume=0;
	return 0;

 failure:
	if(newpoints) free(newpoints);
	return 1; }


/* compartaddcmptl.  Add logically composed compartment cmptl, which is composed
with symbol sym, to the compartment cmpt.  This increments ncmptl and appends
the new logic compartment to cmptl.  Returns 0 for success, 1 if memory could
not be allocated, or 2 if cmpt and cmptl are the same, which is not allowed. */
int compartaddcmptl(compartptr cmpt,compartptr cmptl,enum CmptLogic sym) {
	int cl;
	compartptr *newcmptl;
	enum CmptLogic *newclsym;

	if(cmpt==cmptl) return 2;
	newcmptl=(compartptr*)calloc(cmpt->ncmptl+1,sizeof(compartptr));
	if(!newcmptl) return 1;
	newclsym=(enum CmptLogic*)calloc(cmpt->ncmptl+1,sizeof(enum CmptLogic));
	if(!newclsym) {free(newcmptl);return 1;}
	for(cl=0;cl<cmpt->ncmptl;cl++) {
		newcmptl[cl]=cmpt->cmptl[cl];
		newclsym[cl]=cmpt->clsym[cl]; }
	newcmptl[cl]=cmptl;
	newclsym[cl]=sym;
	cmpt->ncmptl++;
	free(cmpt->cmptl);
	free(cmpt->clsym);
	cmpt->cmptl=newcmptl;
	cmpt->clsym=newclsym;
	compartsetcondition(cmpt->cmptss,SCparams,0);
	cmpt->nbox=0;
	cmpt->volume=0;
	return 0; }


/* compartupdatebox.  Updates the listing of box bptr in compartment cmpt,
according to the rule that boxes should be listed if any portion of them is
within the compartment and should not be listed if no portion is within the
compartment.  This also updates the cumboxvol and volume structure elements as
needed.  If the fraction of the box within the compartment is known, including
0, enter it in volfrac.  If it is unknown and should be calculated, enter -1 in
volfrac.  If the fraction is unknown and should be unchanged if the box was
already in the compartment and calculated if the box wasn’t in the compartment,
then enter -2 in volfrac.  This returns 0 for no change, 1 for box successfully
added, 2 for box successfully removed, 3 for box was already in listed but
volume was updated, or -1 for failure to allocate memory.  If the volume of the
box within the compartment needs to be calcuated, this calculates it with a
hard-coded value of 100 random trial points.  Memory is allocated as needed.
*/
int compartupdatebox(simptr sim,compartptr cmpt,boxptr bptr,double volfrac) {
	int ptsmax=100;	// number of random points for volume determination
	int bc,max,ptsin,i,bc2;
	double pos[DIMMAX],volfrac2,*newboxfrac,*newcumboxvol,boxvol,vol;
	boxptr *newboxlist;

	newboxlist=NULL;
	newboxfrac=NULL;
	newcumboxvol=NULL;

	for(bc=0;bc<cmpt->nbox && cmpt->boxlist[bc]!=bptr;bc++);	// check for box already in cmpt
	if(bc<cmpt->nbox && volfrac==-2) return 0;				// box is listed and volume ok, so return

	if(volfrac<=0) {																// find actual volume fraction
		ptsin=0;
		for(i=0;i<ptsmax;i++) {
			boxrandpos(sim,pos,bptr);
			if(posincompart(sim,pos,cmpt)) ptsin++; }
		volfrac2=(double)ptsin/(double)ptsmax; }
	else if(volfrac>1) volfrac2=1;
	else volfrac2=volfrac;

	if(volfrac2==0) {
		if(bc==cmpt->nbox) return 0;									// box not listed and 0 volume, so return
		cmpt->nbox--;																	// box was listed, so it needs to be removed
		if(cmpt->nbox==0) {
			cmpt->volume=0;
			return 2; }																	// last box was removed
		cmpt->boxlist[bc]=cmpt->boxlist[cmpt->nbox];
		cmpt->boxfrac[bc]=cmpt->boxfrac[cmpt->nbox];
		boxvol=sim->boxs->boxvol;
		vol=(bc==0)?0:cmpt->cumboxvol[bc-1];
		for(bc2=bc;bc2<cmpt->nbox;bc2++) {
			vol+=boxvol*cmpt->boxfrac[bc2];
			cmpt->cumboxvol[bc2]=vol; }
		cmpt->volume=vol;
		return 2; }

	if(bc<cmpt->nbox) {															// box was listed, so just update volume
		if(cmpt->boxfrac[bc]==volfrac2) return 0;			// volume was ok, so return
		cmpt->boxfrac[bc]=volfrac2;										// volume not ok, so update it
		boxvol=sim->boxs->boxvol;
		vol=(bc==0)?0:cmpt->cumboxvol[bc-1];
		for(bc2=bc;bc2<cmpt->nbox;bc2++) {
			vol+=boxvol*cmpt->boxfrac[bc2];
			cmpt->cumboxvol[bc2]=vol; }
		cmpt->volume=vol;
		return 3; }

	if(cmpt->nbox==cmpt->maxbox) {									// expand box list
		max=cmpt->maxbox>0?cmpt->maxbox*2:1;
		CHECK(newboxlist=(boxptr*)calloc(max,sizeof(boxptr)));
		CHECK(newboxfrac=(double*)calloc(max,sizeof(double)));
		CHECK(newcumboxvol=(double*)calloc(max,sizeof(double)));
		for(bc2=0;bc2<cmpt->nbox;bc2++) {
			newboxlist[bc2]=cmpt->boxlist[bc2];
			newboxfrac[bc2]=cmpt->boxfrac[bc2];
			newcumboxvol[bc2]=cmpt->cumboxvol[bc2]; }
		for(;bc2<max;bc2++) {
			newboxlist[bc2]=NULL;
			newboxfrac[bc2]=0;
			newcumboxvol[bc2]=0; }
		cmpt->maxbox=max;
		free(cmpt->boxlist);
		free(cmpt->boxfrac);
		free(cmpt->cumboxvol);
		cmpt->boxlist=newboxlist;
		cmpt->boxfrac=newboxfrac;
		cmpt->cumboxvol=newcumboxvol; }

	bc=cmpt->nbox++;								// put box into cmpt
	cmpt->boxlist[bc]=bptr;
	cmpt->boxfrac[bc]=volfrac2;
	cmpt->volume+=sim->boxs->boxvol*cmpt->boxfrac[bc];
	cmpt->cumboxvol[bc]=cmpt->volume;
	return 1;

 failure:
 	free(newboxlist);
 	free(newboxfrac);
 	free(newcumboxvol);
 	return -1; }


/* cmptreadstring.  Reads and processes one line of text from the configuration
file, or some other source, for the compartment indexed cmptindex.  If the
compartment index is not known, then set cmptindex to -1.  The first word of the
line should be sent in as word and the rest sent in as line2.  If this function
is successful, it returns the compartment index and it does not change the
contents of erstr; if not, it returns -1 and it writes an error message to
erstr. */
int cmptreadstring(simptr sim,int cmptindex,char *word,char *line2,char *erstr) {
	char nm[STRCHAR],nm1[STRCHAR];
	int s,er,cl,dim,itct;
	double v1[DIMMAX];
	compartptr cmpt;
	enum CmptLogic sym;

	dim=sim->dim;
	if(cmptindex>=0) cmpt=sim->cmptss->cmptlist[cmptindex];
	else cmpt=NULL;

	if(!strcmp(word,"name")) {								// name, got[0]
		itct=sscanf(line2,"%s",nm);
		CHECKS(itct==1,"error reading compartment name");
		cmptindex=stringfind(sim->cmptss->cnames,sim->cmptss->ncmpt,nm);
		if(cmptindex<0) {
			CHECKS(sim->cmptss->ncmpt<sim->cmptss->maxcmpt,"more compartments are being defined than were allocated");
			cmptindex=sim->cmptss->ncmpt++;
			strncpy(sim->cmptss->cnames[cmptindex],nm,STRCHAR-1);
			sim->cmptss->cnames[cmptindex][STRCHAR-1]='\0';
			cmpt=sim->cmptss->cmptlist[cmptindex];
			compartsetcondition(sim->cmptss,SCparams,0); }
		else
			cmpt=sim->cmptss->cmptlist[cmptindex];
		CHECKS(!strnword(line2,2),"unexpected text following name"); }

	else if(!strcmp(word,"surface")) {						// surface
		CHECKS(cmpt,"name has to be entered before surface");
		itct=sscanf(line2,"%s",nm);
		CHECKS(itct==1,"error reading surface name");
		s=stringfind(sim->srfss->snames,sim->srfss->nsrf,nm);
		CHECKS(s>=0,"surface name not recognized");
		er=compartaddsurf(cmpt,sim->srfss->srflist[s]);
		CHECKS(er!=1,"out of memory adding surface to compartment");
		CHECKS(er!=2,"cannot add surface to compartment more than once");
		CHECKS(!strnword(line2,2),"unexpected text following surface"); }

	else if(!strcmp(word,"point")) {							// point
		CHECKS(cmpt,"name has to be entered before point");
		itct=strreadnd(line2,dim,v1,NULL);
		CHECKS(itct==dim,"unable to read all point values");
		er=compartaddpoint(cmpt,dim,v1);
		CHECKS(!er,"out of memory adding point to compartment");
		CHECKS(!strnword(line2,dim+1),"unexpected text following point"); }

	else if(!strcmp(word,"compartment")) {				// compartment
		CHECKS(cmpt,"name has to be entered before compartment");
		itct=sscanf(line2,"%s %s",nm1,nm);
		CHECKS(itct==2,"compartment format: symbol name");
		sym=cmptstring2cl(nm1);
		CHECKS(sym!=CLnone,"unrecognized logic symbol");
		cl=stringfind(sim->cmptss->cnames,sim->cmptss->ncmpt,nm);
		CHECKS(cl>=0,"cmpartment name not recognized");
		er=compartaddcmptl(cmpt,sim->cmptss->cmptlist[cl],sym);
		CHECKS(er!=1,"out of memory adding compartment to compartment");
		CHECKS(er!=2,"cannot a compartment to itself");
		CHECKS(!strnword(line2,3),"unexpected text following compartment"); }

	else {																				// unknown word
		CHECKS(0,"syntax error within compartment block: statement not recognized"); }

	return cmptindex;

 failure:
	return -1; }


/* loadcompart.  Loads a compartment, or information for an already existing
compartment, from an already opened configuration file.  This is used to fill in
basic compartment details.  However, it does not address any of the box
information.  Returns 0 for success and 1 for an error; error messages are
returned in erstr. */
int loadcompart(simptr sim,ParseFilePtr *pfpptr,char *line2,char *erstr) {
	ParseFilePtr pfp;
	char word[STRCHAR];
	int done,pfpcode,firstline2,c;

	pfp=*pfpptr;
	CHECKS(sim->cmptss,"PROGRAM BUG: compartment superstructure not allocated in loadcompart");
	done=0;
	c=0;
	firstline2=line2?1:0;

	while(!done) {
		if(pfp->lctr==0&&!strchr(sim->flags,'q'))
			printf(" Reading file: '%s'\n",pfp->fname);
		if(firstline2) {
			strcpy(word,"name");
			pfpcode=1;
			firstline2=0; }
		else
			pfpcode=Parse_ReadLine(&pfp,word,&line2,erstr);
		*pfpptr=pfp;
		CHECKS(pfpcode!=3,erstr);

		if(pfpcode==0);																// already taken care of
		else if(pfpcode==2) {													// end reading
			done=1; }
		else if(pfpcode==3) {													// error
			CHECKS(0,"SMOLDYN BUG: parsing error"); }
		else if(!strcmp(word,"end_compartment")) {		// end_compartment
			CHECKS(!line2,"unexpected text following end_compartment");
			return 0; }
		else if(!line2) {															// just word
			CHECKS(0,"unknown word or missing parameter"); }
		else {
			c=cmptreadstring(sim,c,word,line2,erstr);
			CHECKS(c>=0,erstr); }}

	CHECKS(0,"end of file encountered before end_compartment statement");	// end of file

 failure:																					// failure
	return 1; }


/* setupcomparts.  Sets up the boxes and volumes portions of all compartments.
Returns 0 for success and 1 for inability to allocate sufficient memory. */
int setupcomparts(simptr sim) {
	boxssptr boxs;
	boxptr bptr;
	compartssptr cmptss;
	compartptr cmpt;
	int b,c,s,p,inbox,er,cl;
	double pos[3];
	surfaceptr srf;
	enum CmptLogic clsym;

	cmptss=sim->cmptss;
	if(!cmptss) return 0;
	if(cmptss->condition<=SCparams) {
		boxs=sim->boxs;
		if(!boxs || !boxs->nbox) return 2;

		for(c=0;c<cmptss->ncmpt;c++) {
			cmpt=cmptss->cmptlist[c];
			if(cmpt->volume==0) {
				cmpt->nbox=0;

				for(b=0;b<boxs->nbox;b++) {
					bptr=boxs->blist[b];
					inbox=0;
					for(p=0;p<bptr->npanel && !inbox;p++) {
						srf=bptr->panel[p]->srf;
						for(s=0;s<cmpt->nsrf && !inbox;s++)
							if(cmpt->surflist[s]==srf) inbox=1; }			// a compartment surface is in the box
					if(!inbox && cmpt->ncmptl==0) {
						boxrandpos(sim,pos,bptr);
						if(posincompart(sim,pos,cmpt)) inbox=2; }		// compartment contains whole box
					if(inbox) {
						er=compartupdatebox(sim,cmpt,bptr,inbox==2?1:-1);
						if(er==-1) return 1; }}

				for(cl=0;cl<cmpt->ncmptl;cl++) {
					clsym=cmpt->clsym[cl];
					if(clsym==CLequal || clsym==CLor || clsym==CLxor)
						for(b=0;b<cmpt->cmptl[cl]->nbox;b++) {
							bptr=cmpt->cmptl[cl]->boxlist[b];
							er=compartupdatebox(sim,cmpt,bptr,-2);
							if(er==-1) return 1; }
					else if(clsym==CLequalnot || CLornot)
						for(b=0;b<boxs->nbox;b++) {
							bptr=boxs->blist[b];
							er=compartupdatebox(sim,cmpt,bptr,-2); }}}}
		compartsetcondition(cmptss,SCok,1); }

	return 0; }



