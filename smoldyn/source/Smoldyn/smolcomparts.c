/* Steven Andrews, started 10/22/2001.
 This is a library of functions for the Smoldyn program.  See documentation
 called Smoldyn_doc1.pdf and Smoldyn_doc2.pdf.
 Copyright 2003-2011 by Steven Andrews.  This work is distributed under the terms
 of the Gnu General Public License (GPL). */

#include <algorithm>
using std::max;
using std::min;

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <fstream>
#include <ostream>
#include <sstream>
#include <iostream>
#include "random2.h"
#include "RnSort.h"
#include "smoldyn.h"
#include "Zn.h"

#define CHECK(A) if(!(A)) {printfException("Unknown solver error.");goto failure;} else (void)0
#define CHECKS(A,B) if(!(A)) {strncpy(erstr,B,STRCHAR-1);erstr[STRCHAR-1]='\0'; printfException("%s", B); goto failure;} else (void)0


/******************************************************************************/
/********************************* Compartments *******************************/
/******************************************************************************/


/******************************************************************************/
/********************************* enumerated types ***************************/
/******************************************************************************/


/* compartstring2cl */
enum CmptLogic compartstring2cl(char *string) {
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


/* compartcl2string */
char *compartcl2string(enum CmptLogic cls,char *string) {
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
//called by posincompart and compartsupdateparams_volumeSample
unsigned char getCompartmentID(char* cmptName, VolumeSamplesPtr vSamplesPtr)
{
	int c;
	for(c=0;c<vSamplesPtr->nCmptIDPair;c++) {
		if(!strcmp(cmptName, vSamplesPtr->compartmentIDPairPtr[c].name))
		{
			return vSamplesPtr->compartmentIDPairPtr[c].pixel;
		}
	}
	return 0; //TODO: if can't find the compartment ID, what should we do?
}

/* posincompart */
int posincompart(simptr sim,double *pos,compartptr cmpt) {
	int s,p,k,incmpt,pcross,cl,incmptl;
	enum PanelShape ps;
	surfaceptr srf;
	double crsspt[DIMMAX];
	enum CmptLogic sym;
	int dim;

	VolumeSamples* volumeSamplePtr;
	const int numNeighbors =3;
	int sampleIdxNeighbors[DIMMAX][numNeighbors];

	incmpt = 0;
	int sameResultCount = 0;
	//for smoldyn simulations with volumeSample
	if(sim->volumeSamplesPtr != NULL)
	{
		volumeSamplePtr = sim->volumeSamplesPtr;
		dim = sim->dim;
		unsigned char cmptID = getCompartmentID(cmpt->cname, sim->volumeSamplesPtr);
		
		for (int idim = 0; idim < dim; idim ++) {
			double h = volumeSamplePtr->size[idim]/volumeSamplePtr->num[idim];
			int idx = floor((pos[idim] - volumeSamplePtr->origin[idim])/h);
			sampleIdxNeighbors[idim][0] = max(0,(idx-1));
			sampleIdxNeighbors[idim][1] = idx;
			sampleIdxNeighbors[idim][2] = min((volumeSamplePtr->num[idim]-1),(idx+1));
		}

		//int numNeighbors =3;
		for (int kk = 0; kk < (dim < 3 ?  1 : numNeighbors); kk ++) {
			for (int jj = 0; jj < (dim < 2 ?  1 : numNeighbors); jj ++) {
				for (int ii = 0; ii < numNeighbors; ii ++) {
					int i = sampleIdxNeighbors[0][ii];
					int j = dim < 2 ?  0 : sampleIdxNeighbors[1][jj];
					int k = dim < 3 ?  0 : sampleIdxNeighbors[2][kk];
					int posInSample = i + j*volumeSamplePtr->num[0] + k*volumeSamplePtr->num[0]*volumeSamplePtr->num[1];
					unsigned char cmptIDFoundInSample = volumeSamplePtr->volsamples[posInSample];
					if(cmptID == cmptIDFoundInSample){
						sameResultCount ++;
					}
				}
			}
		}
		if (sameResultCount == (int)pow((double)numNeighbors, dim)) {
			incmpt = 1;
		}
		
	}
	if((incmpt == 0 && sameResultCount > 0) || sim->volumeSamplesPtr == NULL) //for original smoldyn simulations(without volumeSample)
	{
		for(k=0;k<cmpt->npts&&incmpt==0;k++) {
			pcross=0;
			for(s=0;s<cmpt->nsrf&&!pcross;s++) {
				srf=cmpt->surflist[s];
				for(ps=(PanelShape)0;ps<PSMAX&&!pcross;ps=(PanelShape)(ps+1))
					for(p=0;p<srf->npanel[ps]&&!pcross;p++)
						if(lineXpanel(pos,cmpt->points[k],srf->panels[ps][p],sim->dim,crsspt,NULL,NULL,NULL,NULL,NULL)) 
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
	}
	return incmpt; }


/* compartrandpos */
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

/* compartalloc */
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


/* compartfree */
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


/* compartssalloc */
compartssptr compartssalloc(compartssptr cmptss,int maxcmpt) {
	int c,newcmptss;
	char **newnames;
	compartptr *newcmptlist;

	if(maxcmpt<1) return NULL;

	newcmptss=0;
	newnames=NULL;
	newcmptlist=NULL;

	if(!cmptss) {																			// new allocation
		cmptss=(compartssptr) malloc(sizeof(struct compartsuperstruct));
		if(!cmptss) return NULL;
		newcmptss=1;
		cmptss->condition=SCinit;
		cmptss->sim=NULL;
		cmptss->maxcmpt=0;
		cmptss->ncmpt=0;
		cmptss->cnames=NULL;
		cmptss->cmptlist=NULL; }
	else {																						// minor check
		if(maxcmpt<cmptss->maxcmpt) return NULL; }

	if(maxcmpt>cmptss->maxcmpt) {											// allocate new compartment names and compartments
		CHECK(newnames=(char**) calloc(maxcmpt,sizeof(char*)));
		for(c=0;c<maxcmpt;c++) newnames[c]=NULL;
		for(c=0;c<cmptss->maxcmpt;c++) newnames[c]=cmptss->cnames[c];
		for(;c<maxcmpt;c++)
			CHECK(newnames[c]=EmptyString());

		CHECK(newcmptlist=(compartptr*) calloc(maxcmpt,sizeof(compartptr)));	// compartment list
		for(c=0;c<maxcmpt;c++) newcmptlist[c]=NULL;
		for(c=0;c<cmptss->maxcmpt;c++) newcmptlist[c]=cmptss->cmptlist[c];
		for(;c<maxcmpt;c++) {
			CHECK(newcmptlist[c]=compartalloc());
			newcmptlist[c]->cmptss=cmptss;
			newcmptlist[c]->cname=newnames[c]; }}

	cmptss->maxcmpt=maxcmpt;
	free(cmptss->cnames);
	cmptss->cnames=newnames;
	free(cmptss->cmptlist);
	cmptss->cmptlist=newcmptlist;

	return cmptss;

 failure:
 	compartssfree(cmptss);
 	return NULL; }


/* compartssfree */
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

/* compartoutput */
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
			printf("   %s %s\n",compartcl2string(cmpt->clsym[cl],string),cmpt->cmptl[cl]->cname);
		printf("  volume: %g\n",cmpt->volume);
		printf("  %i virtual boxes listed\n",cmpt->nbox); }
	printf("\n");
	return; }


/* writecomparts */
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
			fprintf(fptr,"compartment %s %s\n",compartcl2string(cmpt->clsym[cl],string),cmpt->cmptl[cl]->cname);
		fprintf(fptr,"end_compartment\n\n"); }
	return; }


/* checkcompartparams */
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
		if(cmpt->nbox>0&&cmpt->cumboxvol[cmpt->nbox-1]!=cmpt->volume) {error++;printfException(" ERROR: compartment %s box volumes do not add to compartment volume\n",cmpt->cname);} }
	if(warnptr) *warnptr=warn;
	return error; }


/******************************************************************************/
/******************************** structure set up ****************************/
/******************************************************************************/


/* compartsetcondition */
void compartsetcondition(compartssptr cmptss,enum StructCond cond,int upgrade) {
	if(!cmptss) return;
	if(upgrade==0 && cmptss->condition>cond) cmptss->condition=cond;
	else if(upgrade==1 && cmptss->condition<cond) cmptss->condition=cond;
	else if(upgrade==2) cmptss->condition=cond;
	if(cmptss->condition<cmptss->sim->condition) {
		cond=cmptss->condition;
		simsetcondition(cmptss->sim,cond==SCinit?SClists:cond,0); }
	return; }


/* compartenablecomparts */
int compartenablecomparts(simptr sim,int maxcmpt) {
	compartssptr cmptss;

	if(sim->cmptss)									// check for redundant function call
		if(maxcmpt==-1 || sim->cmptss->maxcmpt==maxcmpt)
			return 0;
	cmptss=compartssalloc(sim->cmptss,maxcmpt<0?5:maxcmpt);
	if(!cmptss) return 1;
	sim->cmptss=cmptss;
	cmptss->sim=sim;
	compartsetcondition(sim->cmptss,SClists,0);
	return 0; }


/* compartaddcompart */
compartptr compartaddcompart(simptr sim,const char *cmptname) {
	int er,c;
	compartssptr cmptss;
	compartptr cmpt;

	if(!sim->cmptss) {
		er=compartenablecomparts(sim,-1);
		if(er) return NULL; }
	cmptss=sim->cmptss;

	c=stringfind(cmptss->cnames,cmptss->ncmpt,cmptname);
	if(c<0) {
		if(cmptss->ncmpt==cmptss->maxcmpt) {
			er=compartenablecomparts(sim,cmptss->ncmpt*2+1);
			if(er) return NULL; }
		c=cmptss->ncmpt++;
		strncpy(cmptss->cnames[c],cmptname,STRCHAR-1);
		cmptss->cnames[c][STRCHAR-1]='\0';
		cmpt=cmptss->cmptlist[c];
		compartsetcondition(cmptss,SClists,0); }
	else
		cmpt=cmptss->cmptlist[c];

	return cmpt; }


/* compartaddsurf */
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
	cmpt->nbox=0;
	cmpt->volume=0;
	compartsetcondition(cmpt->cmptss,SCparams,0);
	return 0; }


/* compartaddpoint */
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


/* compartaddcmptl */
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

/* compartupdatebox --- smoldyn original version*/
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


/* compartupdatebox, the volume fraction here is the actual volume fraction for the box inside the compartment*/
int compartupdatebox_volumeSample(simptr sim,compartptr cmpt,boxptr bptr,double volfrac) {
	/*int ptsmax=100;*/	// number of random points for volume determination
	int bc,max,bc2;
	double volfrac2,*newboxfrac,*newcumboxvol,boxvol,vol;
	boxptr *newboxlist;

	newboxlist=NULL;
	newboxfrac=NULL;
	newcumboxvol=NULL;

	for(bc=0;bc<cmpt->nbox && cmpt->boxlist[bc]!=bptr;bc++);	// check for box already in cmpt
	if(bc<cmpt->nbox && volfrac==-2) return 0;				// box is listed and volume ok, so return


	//if(volfrac<=0) {										//TODO:when passinging -2, what would we do for logic compartments
	//	ptsin=0;
	//	for(i=0;i<ptsmax;i++) {
	//		boxrandpos(sim,pos,bptr);
	//		if(posincompart(sim,pos,cmpt)) ptsin++; }
	//	volfrac2=(double)ptsin/(double)ptsmax; }
	//else if(volfrac>1) volfrac2=1; //this shouldn't happen
	//else volfrac2=volfrac; //the actual volume faction

	//if(volfrac<0) {										//TODO:when passinging -2, what would we do for logic compartments
	//	ptsin=0;
	//	for(i=0;i<ptsmax;i++) {
	//		boxrandpos(sim,pos,bptr);
	//		if(posincompart(sim,pos,cmpt)) ptsin++; }
	//	volfrac2=(double)ptsin/(double)ptsmax; }
	//else if(volfrac>1) volfrac2=1; //this shouldn't happen
	//else 
	volfrac2=volfrac; //the actual volume faction

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

	//if(bc<cmpt->nbox) {													// box was listed, so just update volume
	//	if(cmpt->boxfrac[bc]==volfrac2) return 0;			// volume was ok, so return
	//	cmpt->boxfrac[bc]=volfrac2;										// volume not ok, so update it
	//	boxvol=sim->boxs->boxvol;
	//	vol=(bc==0)?0:cmpt->cumboxvol[bc-1];
	//	for(bc2=bc;bc2<cmpt->nbox;bc2++) {
	//		vol+=boxvol*cmpt->boxfrac[bc2];
	//		cmpt->cumboxvol[bc2]=vol; }
	//	cmpt->volume=vol;
	//	return 3; }

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


/* compartreadstring */
compartptr compartreadstring(simptr sim,compartptr cmpt,char *word,char *line2,char *erstr) {
	char nm[STRCHAR],nm1[STRCHAR];
	int s,er,cl,dim,itct;
	double v1[DIMMAX];
	enum CmptLogic sym;
	compartssptr cmptss;

	dim=sim->dim;
	cmptss=sim->cmptss;

	if(!strcmp(word,"name")) {								// name, got[0]
		itct=sscanf(line2,"%s",nm);
		CHECKS(itct==1,"error reading compartment name");
		cmpt=compartaddcompart(sim,nm);
		CHECKS(cmpt,"failed to add compartment");
		CHECKS(!strnword(line2,2),"unexpected text following name"); }

	else if(!strcmp(word,"surface")) {						// surface
		CHECKS(cmpt,"name has to be entered before surface");
		CHECKS(sim->srfss,"surfaces need to be entered before compartment surfaces");
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
		sym=compartstring2cl(nm1);
		CHECKS(sym!=CLnone,"unrecognized logic symbol");
		cl=stringfind(sim->cmptss->cnames,sim->cmptss->ncmpt,nm);
		CHECKS(cl>=0,"cmpartment name not recognized");
		er=compartaddcmptl(cmpt,sim->cmptss->cmptlist[cl],sym);
		CHECKS(er!=1,"out of memory adding compartment to compartment");
		CHECKS(er!=2,"cannot a compartment to itself");
		CHECKS(!strnword(line2,3),"unexpected text following compartment"); }

	else {																				// unknown word
		CHECKS(0,"syntax error within compartment block: statement not recognized"); }

	return cmpt;

 failure:
	return NULL; }


/* loadcompart */
int loadcompart(simptr sim,ParseFilePtr *pfpptr,char *line2,char *erstr) {
	ParseFilePtr pfp;
	char word[STRCHAR],errstring[STRCHAR];
	int done,pfpcode,firstline2;
	compartptr cmpt;

	pfp=*pfpptr;
	done=0;
	cmpt=NULL;
	firstline2=line2?1:0;

	while(!done) {
		if(pfp->lctr==0&&!strchr(sim->flags,'q'))
			printf(" Reading file: '%s'\n",pfp->fname);
		if(firstline2) {
			strcpy(word,"name");
			pfpcode=1;
			firstline2=0; }
		else
			pfpcode=Parse_ReadLine(&pfp,word,&line2,errstring);
		*pfpptr=pfp;
		CHECKS(pfpcode!=3,errstring);

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
			cmpt=compartreadstring(sim,cmpt,word,line2,errstring);
			CHECKS(cmpt!=NULL,errstring); }}

	CHECKS(0,"end of file encountered before end_compartment statement");	// end of file

 failure:																					// failure
	return 1; }



/* compartsupdateparams */
int compartsupdateparams(simptr sim) {
	if(sim->volumeSamplesPtr == NULL)
	{
		return compartsupdateparams_smoldyn(sim);
	}
	else
	{
		return compartsupdateparams_volumeSample(sim);
	}
}

int compartsupdateparams_smoldyn(simptr sim) {
	boxssptr boxs;
	boxptr bptr;
	compartssptr cmptss;
	compartptr cmpt;
	int b,c,s,p,inbox,er,cl;
	double pos[3];
	surfaceptr srf;
	enum CmptLogic clsym;

	cmptss=sim->cmptss;
	boxs=sim->boxs;
	if(!boxs || !boxs->nbox) return 2;

	for(c=0;c<cmptss->ncmpt;c++) {
		cmpt=cmptss->cmptlist[c];
		cmpt->nbox=0;
		for(b=0;b<boxs->nbox;b++) {											// find boxes that are in the compartment
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

		for(cl=0;cl<cmpt->ncmptl;cl++) {								// still finding boxes that are in compartment
			clsym=cmpt->clsym[cl];
			if(clsym==CLequal || clsym==CLor || clsym==CLxor)
				for(b=0;b<cmpt->cmptl[cl]->nbox;b++) {
					bptr=cmpt->cmptl[cl]->boxlist[b];
					er=compartupdatebox(sim,cmpt,bptr,-2);
					if(er==-1) return 1; }
			else if(clsym==CLequalnot || CLornot)
				for(b=0;b<boxs->nbox;b++) {
					bptr=boxs->blist[b];
					er=compartupdatebox(sim,cmpt,bptr,-2); }
		}

		using namespace std;
		//write to a file the box volume for each compartment
		stringstream ss;
		ss << "d:\\smoldynCmpt" << cmpt->cname << ".txt";
		ofstream filestr;
		filestr.open (ss.str());
		boxptr* boxlist = cmpt->boxlist;
		double* boxVol = cmpt->boxfrac;
		for(int i=0; i<cmpt->nbox; i++)
		{
			for(b=0;b<sim->boxs->nbox;b++) {
				if (boxlist[i] == sim->boxs->blist[b]) {
					filestr << "box index:" << b << "     " << "volFrac:" << boxVol[i] <<endl;
					break;
				}
			}
		}
		filestr.close();
	}

	return 0; 
}

int compartsupdateparams_volumeSample(simptr sim) {
	//indecies
	int b,c,d,i,j,k;
	//varibles used to check possible boxes and its volFrac in a specific compartment
	double boxLow[3], boxHigh[3], sampleLow[3], sampleHigh[3];

	int dim = sim->dim;
	double* min = sim->boxs->min; // get origin in the space
	double* size = sim->boxs->size; //get box x,y,z size
	VolumeSamples* volumeSample = sim->volumeSamplesPtr;
	compartssptr cmptss = sim->cmptss;
	boxssptr boxs = sim->boxs;
	if(!boxs || !boxs->nbox) return 2;

	double sampleDz = 1;
	if(dim > 2){
		sampleDz = volumeSample->size[2]/volumeSample->num[2];
	}
	double sampleDy = 1;
	if(dim > 1) {
		sampleDy = volumeSample->size[1]/volumeSample->num[1];
	}
	double sampleDx = volumeSample->size[0]/volumeSample->num[0];

	
	for(c=0;c<cmptss->ncmpt;c++) {
		compartptr cmpt=cmptss->cmptlist[c];
		cmpt->nbox=0;
		unsigned char cmptID = getCompartmentID(cmpt->cname, sim->volumeSamplesPtr);
		
		using namespace std;
		stringstream ss;
		ss << "d:\\volumeCmpt" << cmpt->cname << ".txt";
		//write to a file the box volume for each compartment
		ofstream filestr;
		filestr.open (ss.str());
		
		for(b=0;b<boxs->nbox;b++) {											// find boxes that are in the compartment
			boxptr bptr=boxs->blist[b];
			
			//finding box low point and high point's indexes in volume sample values.
			for(d=0;d<dim;d++){
				boxLow[d] = min[d]+size[d]*bptr->indx[d];
				boxHigh[d] = min[d]+size[d]*(bptr->indx[d]+1);
			}
			
			//initialize varibles used for each box
			double insideCmptVol = 0;

			for(k = 0; k < volumeSample->num[2]; k++)
			{
				sampleLow[2] = volumeSample->origin[2]+ k*sampleDz;
				sampleHigh[2] = volumeSample->origin[2]+ (k+1)*sampleDz;
				double intersectZ = std::min<double>(boxHigh[2],sampleHigh[2]) - std::max<double>(boxLow[2], sampleLow[2]);
				if(intersectZ<=0)
				{
					continue;
				}
				for(j = 0; j < volumeSample->num[1]; j++)
				{
					sampleLow[1] = volumeSample->origin[1]+ j*sampleDy;
					sampleHigh[1] = volumeSample->origin[1]+ (j+1)*sampleDy;
					double intersectY = min(boxHigh[1],sampleHigh[1]) - max(boxLow[1], sampleLow[1]);
					if(intersectY<=0)
					{
						continue;
					}
					for(i = 0; i < volumeSample->num[0]; i++)
					{
						sampleLow[0] = volumeSample->origin[0]+ i*sampleDx;
						sampleHigh[0] = volumeSample->origin[0]+ (i+1)*sampleDx;
						double intersectX = min(boxHigh[0],sampleHigh[0]) - max(boxLow[0], sampleLow[0]);
						if(intersectX<=0)
						{
							continue;
						}
						//find intersect volume
						int sampleIdx = i + j*volumeSample->num[0] + k*volumeSample->num[0]*volumeSample->num[1];
						if(volumeSample->volsamples[sampleIdx] == cmptID)
						{
							insideCmptVol += intersectX*intersectY*intersectZ;
						}
					}
				}
			}
			double boxVolfrac = insideCmptVol/((boxs->size[0])*(boxs->size[1])*(boxs->size[2]));
			filestr << "boxIndex " << b << "     " << "volFrac " << boxVolfrac <<endl;
			//if(boxVolfrac <= 1e-8) boxVolfrac = 0; // volume faction is too small, consider it as 0
			int errorCode = compartupdatebox_volumeSample(sim,cmpt,bptr,boxVolfrac);
			if(errorCode==-1) return 1;
		}
		filestr.close();
	}

	return 0; 
}


/* compartsupdatelists */
int compartsupdatelists(simptr sim) {
	return 0; }


/* compartsupdate */
int compartsupdate(simptr sim) {
	int er;
	compartssptr cmptss;

	cmptss=sim->cmptss;
	if(cmptss) {
		if(cmptss->condition<=SClists) {
			er=compartsupdatelists(sim);
			if(er) return er;
			compartsetcondition(cmptss,SCparams,1); }
		if(cmptss->condition==SCparams) {
			er=compartsupdateparams(sim);
			if(er) return er;
			compartsetcondition(cmptss,SCok,1); }}
	return 0; }
