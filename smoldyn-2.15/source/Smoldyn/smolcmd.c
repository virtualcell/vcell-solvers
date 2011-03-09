/* Steven Andrews, 1/16/03.
Library of run-time interpretor commands for Smoldyn program.
See documentation called Smoldyn1_doc.doc and Smoldyn2_doc.doc.
Copyright 2003-2007 by Steven Andrews.  This work is distributed under the terms
of the Gnu Lesser General Public License (LGPL). */

#include <stdio.h>
#include <math.h>
#include <string.h>
#include "Geometry.h"
#include "opengl2.h"
#include "random2.h"
#include "Rn.h"
#include "RnSort.h"
#include "smoldyn.h"
#include "string2.h"
#include "Zn.h"

#include "smoldyn_config.h"


/**********************************************************/
/******************** command declarations ****************/
/**********************************************************/
// vcell command
enum CMDcode cmdVCellPrintProgress(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdVCellWriteOutput(simptr sim,cmdptr cmd,char *line2);

// simulation control
enum CMDcode cmdstop(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdpause(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdbeep(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdkeypress(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdsetrandseed(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdsetgraphics(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdsetgraphic_iter(simptr sim,cmdptr cmd,char *line2);

// file manipulation
enum CMDcode cmdoverwrite(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdincrementfile(simptr sim,cmdptr cmd,char *line2);

// conditional
enum CMDcode cmdifno(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdifless(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdifmore(simptr sim,cmdptr cmd,char *line2);

// system observation
enum CMDcode cmdecho(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdwarnescapee(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdmolcountheader(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdmolcount(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdmolcountinbox(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdmolcountincmpt(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdmolcountincmpts(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdmolcountincmpt2(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdmolcountonsurf(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdmolcountspace(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdspeciesstreamcountheader(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdspeciesstreamcount(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdlistmols(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdlistmols2(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdlistmols3(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdmolpos(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdmolmoments(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdsavesim(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdmeansqrdisp(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdmeansqrdisp2(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmddiagnostics(simptr sim,cmdptr cmd,char *line2);

// system manipulation
enum CMDcode cmdset(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdpointsource(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdvolumesource(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdmovesurfacemol(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdkillmol(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdkillmolprob(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdkillmolinsphere(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdkillmoloutsidesystem(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdfixmolcount(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdfixmolcountonsurf(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdfixmolcountincmpt(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdequilmol(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdreplacexyzmol(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdreplacevolmol(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdreplacecmptmol(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdmodulatemol(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdreact1(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdsetrateint(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdsetsurfcoeff(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdsettimestep(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdporttransport(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdexcludebox(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdexcludesphere(simptr sim,cmdptr cmd,char *line2);
enum CMDcode cmdincludeecoli(simptr sim,cmdptr cmd,char *line2);

// internal functions
void cmdv1free(cmdptr cmd);
void cmdv1v2free(cmdptr cmd);
enum CMDcode conditionalcmdtype(simptr sim,cmdptr cmd,int nparam);
int insideecoli(double *pos,double *ofst,double rad,double length);
void putinecoli(double *pos,double *ofst,double rad,double length);
int molinpanels(simptr sim,int ll,int m,int s,char pshape);


/**********************************************************/
/********************* command processor ******************/
/**********************************************************/

enum CMDcode docommand(void *cmdfnarg,cmdptr cmd,char *line) {
	simptr sim;
	char word[STRCHAR],*line2;
	int itct;

	if(!cmdfnarg) return CMDok;
	sim=(simptr) cmdfnarg;
	if(!line) return CMDok;
	itct=sscanf(line,"%s",word);
	if(itct<=0) return CMDok;
	line2=strnword(line,2);

	// simulation control
	if(!strcmp(word,"stop")) return cmdstop(sim,cmd,line2);
	else if(!strcmp(word,"pause")) return cmdpause(sim,cmd,line2);
	else if(!strcmp(word,"beep")) return cmdbeep(sim,cmd,line2);
	else if(!strcmp(word,"keypress")) return cmdkeypress(sim,cmd,line2);
	else if(!strcmp(word,"setrandseed")) return cmdsetrandseed(sim,cmd,line2);
	else if(!strcmp(word,"setgraphics")) return cmdsetgraphics(sim,cmd,line2);
	else if(!strcmp(word,"setgraphic_iter")) return cmdsetgraphic_iter(sim,cmd,line2);

	// file manipulation
	else if(!strcmp(word,"overwrite")) return cmdoverwrite(sim,cmd,line2);
	else if(!strcmp(word,"incrementfile")) return cmdincrementfile(sim,cmd,line2);

	// conditional
	else if(!strcmp(word,"ifno")) return cmdifno(sim,cmd,line2);
	else if(!strcmp(word,"ifless")) return cmdifless(sim,cmd,line2);
	else if(!strcmp(word,"ifmore")) return cmdifmore(sim,cmd,line2);

	// system observation
	else if(!strcmp(word,"echo")) return cmdecho(sim,cmd,line2);
	else if(!strcmp(word,"warnescapee")) return cmdwarnescapee(sim,cmd,line2);
	else if(!strcmp(word,"molcountheader")) return cmdmolcountheader(sim,cmd,line2);
	else if(!strcmp(word,"molcount")) return cmdmolcount(sim,cmd,line2);
	else if(!strcmp(word,"molcountinbox")) return cmdmolcountinbox(sim,cmd,line2);
	else if(!strcmp(word,"molcountincmpt")) return cmdmolcountincmpt(sim,cmd,line2);
	else if(!strcmp(word,"molcountincmpts")) return cmdmolcountincmpts(sim,cmd,line2);
	else if(!strcmp(word,"molcountincmpt2")) return cmdmolcountincmpt2(sim,cmd,line2);
	else if(!strcmp(word,"molcountonsurf")) return cmdmolcountonsurf(sim,cmd,line2);
	else if(!strcmp(word,"molcountspace")) return cmdmolcountspace(sim,cmd,line2);
	else if(!strcmp(word,"speciesstreamheader")) return cmdspeciesstreamcountheader(sim,cmd,line2);
	else if(!strcmp(word,"speciesstreamcount")) return cmdspeciesstreamcount(sim,cmd,line2);
	else if(!strcmp(word,"listmols")) return cmdlistmols(sim,cmd,line2);
	else if(!strcmp(word,"listmols2")) return cmdlistmols2(sim,cmd,line2);
	else if(!strcmp(word,"listmols3")) return cmdlistmols3(sim,cmd,line2);
	else if(!strcmp(word,"molpos")) return cmdmolpos(sim,cmd,line2);
	else if(!strcmp(word,"molmoments")) return cmdmolmoments(sim,cmd,line2);
	else if(!strcmp(word,"savesim")) return cmdsavesim(sim,cmd,line2);
	else if(!strcmp(word,"meansqrdisp")) return cmdmeansqrdisp(sim,cmd,line2);
	else if(!strcmp(word,"meansqrdisp2")) return cmdmeansqrdisp2(sim,cmd,line2);
	else if(!strcmp(word,"diagnostics")) return cmddiagnostics(sim,cmd,line2);

	// system manipulation
	else if(!strcmp(word,"set")) return cmdset(sim,cmd,line2);
	else if(!strcmp(word,"pointsource")) return cmdpointsource(sim,cmd,line2);
	else if(!strcmp(word,"volumesource")) return cmdvolumesource(sim,cmd,line2);
	else if(!strcmp(word,"movesurfacemol")) return cmdmovesurfacemol(sim,cmd,line2);
	else if(!strcmp(word,"killmol")) return cmdkillmol(sim,cmd,line2);
	else if(!strcmp(word,"killmolprob")) return cmdkillmolprob(sim,cmd,line2);
	else if(!strcmp(word,"killmolinsphere")) return cmdkillmolinsphere(sim,cmd,line2);
	else if(!strcmp(word,"killmoloutsidesystem")) return cmdkillmoloutsidesystem(sim,cmd,line2);
	else if(!strcmp(word,"fixmolcount")) return cmdfixmolcount(sim,cmd,line2);
	else if(!strcmp(word,"fixmolcountonsurf")) return cmdfixmolcountonsurf(sim,cmd,line2);
	else if(!strcmp(word,"fixmolcountincmpt")) return cmdfixmolcountincmpt(sim,cmd,line2);
	else if(!strcmp(word,"equilmol")) return cmdequilmol(sim,cmd,line2);
	else if(!strcmp(word,"replacexyzmol")) return cmdreplacexyzmol(sim,cmd,line2);
	else if(!strcmp(word,"replacevolmol")) return cmdreplacevolmol(sim,cmd,line2);
	else if(!strcmp(word,"replacecmptmol")) return cmdreplacecmptmol(sim,cmd,line2);
	else if(!strcmp(word,"modulatemol")) return cmdmodulatemol(sim,cmd,line2);
	else if(!strcmp(word,"react1")) return cmdreact1(sim,cmd,line2);
	else if(!strcmp(word,"setrateint")) return cmdsetrateint(sim,cmd,line2);
	else if(!strcmp(word,"setsurfcoeff")) return cmdsetsurfcoeff(sim,cmd,line2);
	else if(!strcmp(word,"settimestep")) return cmdsettimestep(sim,cmd,line2);
	else if(!strcmp(word,"porttransport")) return cmdporttransport(sim,cmd,line2);
	else if(!strcmp(word,"excludebox")) return cmdexcludebox(sim,cmd,line2);
	else if(!strcmp(word,"excludesphere")) return cmdexcludesphere(sim,cmd,line2);
	else if(!strcmp(word,"includeecoli")) return cmdincludeecoli(sim,cmd,line2);

	// vcell commands
	else if(!strcmp(word,"vcellPrintProgress")) return cmdVCellPrintProgress(sim,cmd,line2);
	else if(!strcmp(word,"vcellWriteOutput")) return cmdVCellWriteOutput(sim,cmd,line2);

	SCMDCHECK(0,"command not recognized");
	return CMDwarn; }


/**********************************************************/
/****************** simulation control ********************/
/**********************************************************/

enum CMDcode cmdstop(simptr sim,cmdptr cmd,char *line2) {
	if(line2 && !strcmp(line2,"cmdtype")) return CMDcontrol;
	return CMDstop; }


enum CMDcode cmdpause(simptr sim,cmdptr cmd,char *line2) {
	char c;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDcontrol;
	if(!sim->graphss || sim->graphss->graphics==0) {
		fprintf(stderr,"Simulation paused at time %g.  Press enter to continue.",sim->time);
		scanf("%c",&c);
		return CMDok; }
	gl2State(1);
	return CMDpause; }


enum CMDcode cmdbeep(simptr sim,cmdptr cmd,char *line2) {
	if(line2 && !strcmp(line2,"cmdtype")) return CMDcontrol;
	fprintf(stderr,"\7");
	return CMDok; }


enum CMDcode cmdkeypress(simptr sim,cmdptr cmd,char *line2) {
	char c;
	int itct;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDcontrol;
	SCMDCHECK(line2,"missing argument");
	itct=sscanf(line2,"%c",&c);
	SCMDCHECK(itct==1,"cannot read character");
	gl2SetKeyPush((unsigned char) c);
	return CMDok; }


enum CMDcode cmdsetrandseed(simptr sim,cmdptr cmd,char *line2) {
	int itct;
	long int seed;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDcontrol;
	SCMDCHECK(line2,"missing argument");
	itct=sscanf(line2,"%li",&seed);
	SCMDCHECK(itct==1,"cannot read seed");
	if(seed<0) randomize((long int) time(NULL));
	else randomize((long int) seed);
	return CMDok; }


enum CMDcode cmdsetgraphics(simptr sim,cmdptr cmd,char *line2) {
	int itct;
	char str[STRCHAR];

	if(line2 && !strcmp(line2,"cmdtype")) return CMDcontrol;
	if(!sim->graphss || sim->graphss->graphics==0) return CMDok;
	SCMDCHECK(line2,"missing argument");
	itct=sscanf(line2,"%s",str);
	SCMDCHECK(itct==1,"cannot read graphics type");
	if(!strcmp(str,"opengl")) sim->graphss->graphics=1;
	else if(!strcmp(str,"opengl_good")) sim->graphss->graphics=2;
	else SCMDCHECK(0,"unrecognized graphics type");
	return CMDok; }


enum CMDcode cmdsetgraphic_iter(simptr sim,cmdptr cmd,char *line2) {
	int itct,iter;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDcontrol;
	if(!sim->graphss || sim->graphss->graphics==0) return CMDok;
	SCMDCHECK(line2,"missing argument");
	itct=sscanf(line2,"%i",&iter);
	SCMDCHECK(itct==1,"cannot read graphics iterations");
	SCMDCHECK(iter>0,"graphics iterations must be >0");
	sim->graphss->graphicit=iter;
	return CMDok; }


/**********************************************************/
/****************** file manipulation ********************/
/**********************************************************/


enum CMDcode cmdoverwrite(simptr sim,cmdptr cmd,char *line2) {
	if(line2 && !strcmp(line2,"cmdtype")) return CMDcontrol;
	SCMDCHECK(line2,"missing argument");
	SCMDCHECK(scmdoverwrite(sim->cmds,line2),"failed to open file");
	return CMDok; }


enum CMDcode cmdincrementfile(simptr sim,cmdptr cmd,char *line2) {
	if(line2 && !strcmp(line2,"cmdtype")) return CMDcontrol;
	SCMDCHECK(line2,"missing argument");
	SCMDCHECK(scmdincfile(sim->cmds,line2),"failed to increment file");
	return CMDok; }



/**********************************************************/
/********************** conditional ***********************/
/**********************************************************/


enum CMDcode cmdifno(simptr sim,cmdptr cmd,char *line2) {
	int i,count;
	enum MolecState ms;

	if(line2 && !strcmp(line2,"cmdtype")) return conditionalcmdtype(sim,cmd,1);
	i=readmolname(sim,line2,&ms);
	SCMDCHECK(!(i<=0 && i>-5),"cannot read molecule and/or state name");
	count=molcount(sim,i,ms,NULL,1);
	if(count) return CMDok;
	return docommand(sim,cmd,strnword(line2,2)); }


enum CMDcode cmdifless(simptr sim,cmdptr cmd,char *line2) {
	int itct,i,count,min;
	enum MolecState ms;

	if(line2 && !strcmp(line2,"cmdtype")) return conditionalcmdtype(sim,cmd,2);
	i=readmolname(sim,line2,&ms);
	SCMDCHECK(!(i<=0 && i>-5),"cannot read molecule and/or state name");
	SCMDCHECK(line2=strnword(line2,2),"missing value argument");
	itct=sscanf(line2,"%i",&min);
	SCMDCHECK(itct==1,"cannot read value argument");
	count=molcount(sim,i,ms,NULL,min);
	if(count<min) return docommand(sim,cmd,strnword(line2,2));
	return CMDok; }


enum CMDcode cmdifmore(simptr sim,cmdptr cmd,char *line2) {
	int itct,i,count,min;
	enum MolecState ms;

	if(line2 && !strcmp(line2,"cmdtype")) return conditionalcmdtype(sim,cmd,2);
	i=readmolname(sim,line2,&ms);
	SCMDCHECK(!(i<=0 && i>-5),"cannot read molecule and/or state name");
	SCMDCHECK(line2=strnword(line2,2),"missing value argument");
	itct=sscanf(line2,"%i",&min);
	SCMDCHECK(itct==1,"cannot read value argument");
	count=molcount(sim,i,ms,NULL,min+1);
	if(count>min) return docommand(sim,cmd,strnword(line2,2));
	return CMDok; }


/**********************************************************/
/***************** observation commands *******************/
/**********************************************************/


enum CMDcode cmdwarnescapee(simptr sim,cmdptr cmd,char *line2) {
	int i,m,escape,dim,nmol,ll;
	enum MolecState ms;
	FILE *fptr;
	moleculeptr *mlist,mptr;
	double *pos,*posx,*via;
	wallptr *wlist;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDobserve;
	i=readmolname(sim,line2,&ms);
	SCMDCHECK(!(i<0 && i>-5),"cannot read molecule and/or state name");
	line2=strnword(line2,2);
	fptr=scmdgetfptr(sim->cmds,line2);
	SCMDCHECK(fptr,"file name not recognized");

	dim=sim->dim;
	wlist=sim->wlist;
	for(ll=0;ll<sim->mols->nlist;ll++) {
		mlist=sim->mols->live[ll];
		nmol=sim->mols->nl[ll];
		for(m=0;m<nmol;m++) {
			mptr=mlist[m];
			if((mptr->ident>0 && i<0 && (ms==MSall || mptr->mstate==ms)) || (mptr->ident==i && (ms==MSall || mptr->mstate==ms))) {
				pos=mptr->pos;
				escape=!posinsystem(sim,pos);
				if(escape) {
					posx=mptr->posx;
					escape=!posinsystem(sim,posx);
					if(!escape) {
						via=mptr->via;
						if(dim==1) fprintf(fptr,"New escapee: %g #%li %g to %g via %g\n",sim->time,mptr->serno,posx[0],pos[0],via[0]);
						else if(dim==2) fprintf(fptr,"New escapee: %g #%li (%g,%g) to (%g,%g) via (%g,%g)\n",sim->time,mptr->serno,posx[0],posx[1],pos[0],pos[1],via[0],via[1]);
						else fprintf(fptr,"New escapee: %g #%li (%g,%g,%g) to (%g,%g,%g) via (%g,%g,%g)\n",sim->time,mptr->serno,posx[0],posx[1],posx[2],pos[0],pos[1],pos[2],via[0],via[1],via[2]); }}}}}
	return CMDok; }


enum CMDcode cmdecho(simptr sim,cmdptr cmd,char *line2) {
	FILE *fptr;
	char *termqt,str[STRCHAR];

	if(line2 && !strcmp(line2,"cmdtype")) return CMDobserve;
	fptr=scmdgetfptr(sim->cmds,line2);
	SCMDCHECK(fptr,"file name not recognized");
	line2=strnword(line2,2);
	SCMDCHECK(line2=strchr(line2,'"'),"no starting quote on string");
	strncpy(str,line2+1,STRCHAR-1);
	str[STRCHAR-1]='\0';
	SCMDCHECK(termqt=strchr(str,'"'),"no terminal quote on string");
	*termqt='\0';
	strbslash2escseq(str);
	fprintf(fptr,"%s",str);
	return CMDok; }


enum CMDcode cmdmolcountheader(simptr sim,cmdptr cmd,char *line2) {
	FILE *fptr;
	int i;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDobserve;
	fptr=scmdgetfptr(sim->cmds,line2);
	SCMDCHECK(fptr,"file name not recognized");
	SCMDCHECK(sim->mols,"molecules are undefined");
	fprintf(fptr,"time");
	for(i=1;i<sim->mols->nspecies;i++) fprintf(fptr," %s",sim->mols->spname[i]);
	fprintf(fptr,"\n");
	return CMDok; }


enum CMDcode cmdmolcount(simptr sim,cmdptr cmd,char *line2) {
	FILE *fptr;
	int ll,m,*ct,i,nspecies;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDobserve;
	SCMDCHECK(cmd->i1!=-1,"error on setup");					// failed before, don't try again
	fptr=scmdgetfptr(sim->cmds,line2);
	SCMDCHECK(fptr,"file name not recognized");
	SCMDCHECK(sim->mols,"molecules are undefined");

	nspecies=sim->mols->nspecies;
	if(cmd->i1!=nspecies) {														// allocate counter if required
		cmdv1free(cmd);
		cmd->i1=nspecies;
		cmd->freefn=&cmdv1free;
		cmd->v1=calloc(nspecies,sizeof(int));
		if(!cmd->v1) {cmd->i1=-1;return CMDwarn;} }

	ct=(int*)cmd->v1;
	for(i=0;i<nspecies;i++) ct[i]=0;
	for(ll=0;ll<sim->mols->nlist;ll++)
		for(m=0;m<sim->mols->nl[ll];m++) {
			i=sim->mols->live[ll][m]->ident;
			if(i>0) ct[i]++; }
	fprintf(fptr,"%g",sim->time);
	for(i=1;i<nspecies;i++) fprintf(fptr," %i",ct[i]);
	fprintf(fptr,"\n");
	return CMDok; }


enum CMDcode cmdmolcountinbox(simptr sim,cmdptr cmd,char *line2) {
	FILE *fptr;
	int ll,m,*ct,d,dim,itct,i,nspecies;
	double low[3],high[3];
	moleculeptr mptr;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDobserve;
	SCMDCHECK(cmd->i1!=-1,"error on setup");					// failed before, don't try again
	SCMDCHECK(sim->mols,"molecules are undefined");
	dim=sim->dim;
	for(d=0;d<dim;d++) {
		SCMDCHECK(line2,"missing argument");
		itct=sscanf(line2,"%lg %lg",&low[d],&high[d]);
		SCMDCHECK(itct==2,"read failure");
		line2=strnword(line2,3); }
	fptr=scmdgetfptr(sim->cmds,line2);
	SCMDCHECK(fptr,"file name not recognized");

	nspecies=sim->mols->nspecies;
	if(cmd->i1!=nspecies) {														// allocate counter if required
		cmdv1free(cmd);
		cmd->i1=nspecies;
		cmd->freefn=&cmdv1free;
		cmd->v1=calloc(nspecies,sizeof(int));
		if(!cmd->v1) {cmd->i1=-1;return CMDwarn;} }
	
	ct=(int*)cmd->v1;
	for(i=0;i<nspecies;i++) ct[i]=0;
	for(ll=0;ll<sim->mols->nlist;ll++)
		for(m=0;m<sim->mols->nl[ll];m++) {
			mptr=sim->mols->live[ll][m];
			for(d=0;d<dim;d++)
				if(mptr->pos[d]<low[d] || mptr->pos[d]>high[d]) d=dim+1;
			if(d==dim && mptr->ident>0) ct[mptr->ident]++; }
	fprintf(fptr,"%g",sim->time);
	for(i=1;i<nspecies;i++) fprintf(fptr," %i",ct[i]);
	fprintf(fptr,"\n");
	return CMDok; }


enum CMDcode cmdmolcountincmpt(simptr sim,cmdptr cmd,char *line2) {
	FILE *fptr;
	char nm[STRCHAR];
	compartptr cmpt;
	compartssptr cmptss;
	int ll,m,*ct,itct,c,i,nspecies;
	moleculeptr mptr;
	
	if(line2 && !strcmp(line2,"cmdtype")) return CMDobserve;
	SCMDCHECK(cmd->i1!=-1,"error on setup");					// failed before, don't try again
	cmptss=sim->cmptss;
	SCMDCHECK(cmptss,"no compartments defined");
	SCMDCHECK(sim->mols,"molecules are undefined");
	SCMDCHECK(line2,"missing argument");
	itct=sscanf(line2,"%s",nm);
	SCMDCHECK(itct==1,"cannot read argument");
	c=stringfind(cmptss->cnames,cmptss->ncmpt,nm);
	SCMDCHECK(c>=0,"compartment name not recognized");
	cmpt=cmptss->cmptlist[c];
	line2=strnword(line2,2);
	fptr=scmdgetfptr(sim->cmds,line2);
	SCMDCHECK(fptr,"file name not recognized");
	
	nspecies=sim->mols->nspecies;
	if(cmd->i1!=nspecies) {														// allocate counter if required
		cmdv1free(cmd);
		cmd->i1=nspecies;
		cmd->freefn=&cmdv1free;
		cmd->v1=calloc(nspecies,sizeof(int));
		if(!cmd->v1) {cmd->i1=-1;return CMDwarn;} }
	
	ct=(int*)cmd->v1;
	for(i=0;i<nspecies;i++) ct[i]=0;
	for(ll=0;ll<sim->mols->nlist;ll++)
		for(m=0;m<sim->mols->nl[ll];m++) {
			mptr=sim->mols->live[ll][m];
			if(mptr->ident>0 && posincompart(sim,mptr->pos,cmpt) && mptr->mstate==MSsoln) ct[mptr->ident]++; }
	fprintf(fptr,"%g",sim->time);
	for(i=1;i<nspecies;i++) fprintf(fptr," %i",ct[i]);
	fprintf(fptr,"\n");
	return CMDok; }


enum CMDcode cmdmolcountincmpts(simptr sim,cmdptr cmd,char *line2) {
	FILE *fptr;
	char nm[STRCHAR];
	compartptr cmptlist[16];
	compartssptr cmptss;
	int ll,m,*ct,itct,c,i,ic,ncmpt,nspecies;
	moleculeptr mptr;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDobserve;
	SCMDCHECK(cmd->i1!=-1,"error on setup");					// failed before, don't try again
	cmptss=sim->cmptss;
	SCMDCHECK(cmptss,"no compartments defined");
	SCMDCHECK(sim->mols,"molecules are undefined");
	SCMDCHECK(line2,"missing argument");
	ncmpt=wordcount(line2)-1;
	SCMDCHECK(ncmpt>=1,"no compartment or no output file listed");
	for(ic=0;ic<ncmpt;ic++) {
		itct=sscanf(line2,"%s",nm);
		SCMDCHECK(itct==1,"cannot read compartment name");
		c=stringfind(cmptss->cnames,cmptss->ncmpt,nm);
		SCMDCHECK(c>=0,"compartment name not recognized");
		cmptlist[ic]=cmptss->cmptlist[c];
		line2=strnword(line2,2);
		SCMDCHECK(line2,"missing argument"); }
	fptr=scmdgetfptr(sim->cmds,line2);
	SCMDCHECK(fptr,"file name not recognized");

	nspecies=sim->mols->nspecies;
	if(cmd->i1!=nspecies) {														// allocate counter if required
		cmdv1free(cmd);
		cmd->i1=nspecies;
		cmd->freefn=&cmdv1free;
		cmd->v1=calloc(nspecies*ncmpt,sizeof(int));
		if(!cmd->v1) {cmd->i1=-1;return CMDwarn;} }

	ct=(int*)cmd->v1;
	for(i=0;i<nspecies*ncmpt;i++) ct[i]=0;
	for(ll=0;ll<sim->mols->nlist;ll++)
		for(m=0;m<sim->mols->nl[ll];m++) {
			mptr=sim->mols->live[ll][m];
			if(mptr->ident>0 && mptr->mstate==MSsoln)
				for(ic=0;ic<ncmpt;ic++)
					if(posincompart(sim,mptr->pos,cmptlist[ic])) ct[ic*nspecies+mptr->ident]++; }

	fprintf(fptr,"%g",sim->time);
	for(i=1;i<nspecies*ncmpt;i++) 
		if(i%nspecies!=0) fprintf(fptr," %i",ct[i]);
	fprintf(fptr,"\n");
	return CMDok; }


enum CMDcode cmdmolcountincmpt2(simptr sim,cmdptr cmd,char *line2) {
	FILE *fptr;
	char nm[STRCHAR],state[STRCHAR];
	compartptr cmpt;
	compartssptr cmptss;
	int ll,m,*ct,itct,c,i,nspecies;
	moleculeptr mptr;
	enum MolecState ms;
	
	if(line2 && !strcmp(line2,"cmdtype")) return CMDobserve;
	SCMDCHECK(cmd->i1!=-1,"error on setup");					// failed before, don't try again
	cmptss=sim->cmptss;
	SCMDCHECK(cmptss,"no compartments defined");
	SCMDCHECK(sim->mols,"molecules are undefined");
	SCMDCHECK(line2,"missing argument");
	itct=sscanf(line2,"%s %s",nm,state);
	SCMDCHECK(itct==2,"cannot read arguments");
	c=stringfind(cmptss->cnames,cmptss->ncmpt,nm);
	SCMDCHECK(c>=0,"compartment name not recognized");
	ms=molstring2ms(state);
	SCMDCHECK(ms!=MSnone,"molecule state not recognized");
	SCMDCHECK(ms!=MSbsoln,"bsoln molecule state not permitted");
	cmpt=cmptss->cmptlist[c];
	line2=strnword(line2,3);
	fptr=scmdgetfptr(sim->cmds,line2);
	SCMDCHECK(fptr,"file name not recognized");

	nspecies=sim->mols->nspecies;
	if(cmd->i1!=nspecies) {														// allocate counter if required
		cmdv1free(cmd);
		cmd->i1=nspecies;
		cmd->freefn=&cmdv1free;
		cmd->v1=calloc(nspecies,sizeof(int));
		if(!cmd->v1) {cmd->i1=-1;return CMDwarn;} }

	ct=(int*)cmd->v1;
	for(i=0;i<nspecies;i++) ct[i]=0;
	for(ll=0;ll<sim->mols->nlist;ll++)
		for(m=0;m<sim->mols->nl[ll];m++) {
			mptr=sim->mols->live[ll][m];
			if(mptr->ident>0 && posincompart(sim,mptr->pos,cmpt))
				if(ms==MSall || mptr->mstate==ms) ct[mptr->ident]++; }
	fprintf(fptr,"%g",sim->time);
	for(i=1;i<nspecies;i++) fprintf(fptr," %i",ct[i]);
	fprintf(fptr,"\n");
	return CMDok; }


enum CMDcode cmdmolcountonsurf(simptr sim,cmdptr cmd,char *line2) {
	FILE *fptr;
	char nm[STRCHAR];
	surfaceptr srf;
	surfacessptr srfss;
	int ll,m,*ct,itct,s,i,nspecies;
	moleculeptr mptr;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDobserve;
	SCMDCHECK(cmd->i1!=-1,"error on setup");					// failed before, don't try again
	srfss=sim->srfss;
	SCMDCHECK(srfss,"no surfaces defined");
	SCMDCHECK(sim->mols,"molecules are undefined");
	SCMDCHECK(line2,"missing argument");
	itct=sscanf(line2,"%s",nm);
	SCMDCHECK(itct==1,"cannot read argument");
	s=stringfind(srfss->snames,srfss->nsrf,nm);
	SCMDCHECK(s>=0,"surface name not recognized");
	srf=srfss->srflist[s];
	line2=strnword(line2,2);
	fptr=scmdgetfptr(sim->cmds,line2);
	SCMDCHECK(fptr,"file name not recognized");

	nspecies=sim->mols->nspecies;
	if(cmd->i1!=nspecies) {														// allocate counter if required
		cmdv1free(cmd);
		cmd->i1=nspecies;
		cmd->freefn=&cmdv1free;
		cmd->v1=calloc(nspecies,sizeof(int));
		if(!cmd->v1) {cmd->i1=-1;return CMDwarn;} }

	ct=(int*)cmd->v1;
	for(i=0;i<nspecies;i++) ct[i]=0;
	for(ll=0;ll<sim->mols->nlist;ll++)
		for(m=0;m<sim->mols->nl[ll];m++) {
			mptr=sim->mols->live[ll][m];
			if(mptr->ident>0 && mptr->mstate!=MSsoln && mptr->pnl->srf==srf) ct[mptr->ident]++; }
	fprintf(fptr,"%g",sim->time);
	for(i=1;i<nspecies;i++) fprintf(fptr," %i",ct[i]);
	fprintf(fptr,"\n");
	return CMDok; }


enum CMDcode cmdmolcountspace(simptr sim,cmdptr cmd,char *line2) {
	FILE *fptr;
	int dim,i,itct,axis,nbin,ax2,d,*ct,bin,ll,lllo,llhi,nmol,m,average;
	enum MolecState ms;
	double low[DIMMAX],high[DIMMAX],scale;
	moleculeptr *mlist,mptr;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDobserve;
	SCMDCHECK(cmd->i1!=-1,"error on setup");					// failed before, don't try again

	dim=sim->dim;
	SCMDCHECK(line2,"missing arguments");
	i=readmolname(sim,line2,&ms);
	SCMDCHECK(!(i<0 && i>-5),"cannot read molecule and/or state name")
	line2=strnword(line2,2);
	SCMDCHECK(line2,"missing arguments");
	itct=sscanf(line2,"%i",&axis);
	SCMDCHECK(itct==1,"cannot read axis number");
	SCMDCHECK(axis>=0 && axis<dim,"illegal axis value");
	line2=strnword(line2,2);
	SCMDCHECK(line2,"missing arguments");
	itct=sscanf(line2,"%lf %lf %i",&low[axis],&high[axis],&nbin);
	SCMDCHECK(itct==3,"cannot read arguments: low high bins");
	SCMDCHECK(low[axis]<high[axis],"low value needs to be less than high value");
	SCMDCHECK(nbin>0,"bins value needs to be > 0");
	line2=strnword(line2,4);
	ax2=0;
	for(d=0;d<dim-1;d++) {
		if(ax2==axis) ax2++;
		SCMDCHECK(line2,"missing position arguments");
		itct=sscanf(line2,"%lf %lf",&low[ax2],&high[ax2]);
		SCMDCHECK(itct==2,"cannot read (or insufficient) position arguments");
		SCMDCHECK(low[ax2]<=high[ax2],"low value needs to be less than or equal to high value");
		line2=strnword(line2,3);
		ax2++; }
	SCMDCHECK(line2,"missing arguments");
	itct=sscanf(line2,"%i",&average);
	SCMDCHECK(itct==1,"cannot read average number");
	SCMDCHECK(average>=0,"illegal average value");
	line2=strnword(line2,2);
	fptr=scmdgetfptr(sim->cmds,line2);
	SCMDCHECK(fptr,"file name not recognized");

	if(cmd->i1!=nbin) {														// allocate counter if required
		cmdv1free(cmd);
		cmd->i1=nbin;
		cmd->freefn=&cmdv1free;
		cmd->v1=calloc(nbin,sizeof(int));
		if(!cmd->v1) {cmd->i1=-1;return CMDwarn;} }

	ct=(int*)cmd->v1;
	if(average<=1 || cmd->invoke%average==1)
		for(bin=0;bin<nbin;bin++) ct[bin]=0;
	scale=(double)nbin/(high[axis]-low[axis]);

	if(i<0 || ms==MSall) {lllo=0;llhi=sim->mols->nlist;}
	else llhi=1+(lllo=sim->mols->listlookup[i][ms]);
	for(ll=lllo;ll<llhi;ll++) {
		mlist=sim->mols->live[ll];
		nmol=sim->mols->nl[ll];
		for(m=0;m<nmol;m++) {
			mptr=mlist[m];
			if((mptr->ident>0 && i<0 && (ms==MSall || mptr->mstate==ms)) || (mptr->ident==i && (ms==MSall || mptr->mstate==ms))) {
				for(d=0;d<dim;d++)
					if(mptr->pos[d]<=low[d] || mptr->pos[d]>=high[d]) d=dim+1;
				if(d==dim) {
					bin=(int)floor(scale*(mptr->pos[axis]-low[axis]));
					if(bin==nbin) bin--;
					ct[bin]++; }}}}

	if(average<=1) {
		fprintf(fptr,"%g",sim->time);
		for(bin=0;bin<nbin;bin++) fprintf(fptr," %i",ct[bin]);
		fprintf(fptr,"\n"); }
	else if(cmd->invoke%average==0) {
		fprintf(fptr,"%g",sim->time);
		for(bin=0;bin<nbin;bin++) fprintf(fptr," %g",(double)(ct[bin])/(double)average);
		fprintf(fptr,"\n"); }
	return CMDok; }


enum CMDcode cmdspeciesstreamcountheader(simptr sim,cmdptr cmd,char *line2) {
	FILE *fptr;
	int ndx,er;
	char **speciesStreamNames;
	int numNames;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDobserve;
	SCMDCHECK(sim->mzrss,"moleculizer is undefined");
	SCMDCHECK(sim->mzrss->mzr,"moleculizer handle is undefined");

	fptr=scmdgetfptr(sim->cmds,line2);
	SCMDCHECK(fptr,"file name not recognized");

	er=mzrGetSpeciesStreams(sim->mzrss,&speciesStreamNames,&numNames);
	SCMDCHECK(!er,"failed to allocate temporary memory");

	fprintf(fptr,"time");
	for(ndx=0;ndx<numNames;ndx++)
		fprintf(fptr," %s",speciesStreamNames[ndx]);
	fprintf(fptr,"\n");
	mzrFreeSpeciesStreams(speciesStreamNames,numNames);
	return CMDok;	}


enum CMDcode cmdspeciesstreamcount(simptr sim,cmdptr cmd,char *line2) {
	int i,ll,m,ss,er;
	FILE *fptr;
	char** speciesStreamNames;
	int nspecstreams,nspecies;
	char moleculizerName[STRCHAR];
	int *populationArray;
	int *speciesStreamArray;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDobserve;
	SCMDCHECK(cmd->i1!=-1 && cmd->i2!=-1,"error on setup");
	SCMDCHECK(sim->mzrss,"moleculizer is undefined");
	SCMDCHECK(sim->mzrss->mzr,"moleculizer handle is undefined");

	fptr=scmdgetfptr(sim->cmds,line2);
	SCMDCHECK(fptr,"file name not recognized");
	er=mzrGetSpeciesStreams(sim->mzrss,&speciesStreamNames,&nspecstreams);
	SCMDCHECK(!er,"failed to allocate temporary memory");

	nspecies=sim->mols->nspecies;
	if(cmd->i1!=nspecies || cmd->i2!=nspecstreams) {		// allocate counters if required
		cmdv1v2free(cmd);
		cmd->i1=nspecies;
		cmd->i2=nspecstreams;
		cmd->freefn=&cmdv1v2free;
		cmd->v1=calloc(nspecies,sizeof(int));
		if(!cmd->v1) {cmd->i1=-1;return CMDwarn;}
		cmd->v2=calloc(nspecstreams,sizeof(int));
		if(!cmd->v2) {cmd->i2=-1;return CMDwarn;} }

	populationArray=(int*) cmd->v1;
	speciesStreamArray=(int*) cmd->v2;
	for(i=0;i<nspecies;i++) populationArray[i]=0;
	for(i=0;i<nspecstreams;++i) speciesStreamArray[i]=0;

	for(ll=0;ll<sim->mols->nlist;ll++)
		for(m=0;m<sim->mols->nl[ll];m++) {
			i=sim->mols->live[ll][m]->ident;
			if(i>0) populationArray[i]++; }

	for(i=0;i<nspecies;i++) {
		// Convert the name to its moleculizer name, figure out which species streams it is in, increment those counts. 
		if(!mzrSmolName2TagName(sim->mzrss,sim->mols->spname[i],moleculizerName))
			for(ss=0;ss<nspecstreams;ss++)
				if(mzrIsTagNameInStream(sim->mzrss,moleculizerName,speciesStreamNames[ss]))
					speciesStreamArray[ss]+=populationArray[i]; }

	fprintf(fptr,"%g ",sim->time);
	for(ss=0;ss<nspecstreams;ss++)
		fprintf(fptr," %i",speciesStreamArray[ss]);
	fprintf(fptr,"\n");
	mzrFreeSpeciesStreams(speciesStreamNames,nspecstreams);
	return CMDok; }


enum CMDcode cmdlistmols(simptr sim,cmdptr cmd,char *line2) {
	int m,ll;
	static char string[STRCHAR];
	moleculeptr mptr;
	FILE *fptr;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDobserve;
	SCMDCHECK(sim->mols,"molecules are undefined");
	fptr=scmdgetfptr(sim->cmds,line2);
	SCMDCHECK(fptr,"file name not recognized");
	for(ll=0;ll<sim->mols->nlist;ll++)
		for(m=0;m<sim->mols->nl[ll];m++) {
			mptr=sim->mols->live[ll][m];
			if(mptr->ident>0) {
				fprintf(fptr,"%s(%s) ",sim->mols->spname[mptr->ident],molms2string(mptr->mstate,string));
				fprintVD(fptr,mptr->pos,sim->dim); }}
	return CMDok; }


enum CMDcode cmdlistmols2(simptr sim,cmdptr cmd,char *line2) {
	int m,ll,invk;
	moleculeptr mptr;
	FILE *fptr;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDobserve;
	fptr=scmdgetfptr(sim->cmds,line2);
	SCMDCHECK(fptr,"file name not recognized");
	invk=cmd?cmd->invoke:0;
	for(ll=0;ll<sim->mols->nlist;ll++)
		for(m=0;m<sim->mols->nl[ll];m++) {
			mptr=sim->mols->live[ll][m];
			if(mptr->ident>0) {
				fprintf(fptr,"%i %i %i ",invk,mptr->ident,mptr->mstate);
				fprintVD(fptr,mptr->pos,sim->dim); }}
	return CMDok; }


enum CMDcode cmdlistmols3(simptr sim,cmdptr cmd,char *line2) {
	int i,m,ll,dim,invk,lllo,llhi,nmol;
	moleculeptr *mlist,mptr;
	FILE *fptr;
	enum MolecState ms;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDobserve;
	i=readmolname(sim,line2,&ms);
	SCMDCHECK(!(i<0 && i>-5),"cannot read molecule and/or state name")
	line2=strnword(line2,2);
	fptr=scmdgetfptr(sim->cmds,line2);
	SCMDCHECK(fptr,"file name not recognized");
	invk=cmd?cmd->invoke:0;
	dim=sim->dim;

	if(i<0 || ms==MSall) {lllo=0;llhi=sim->mols->nlist;}
	else llhi=1+(lllo=sim->mols->listlookup[i][ms]);
	for(ll=lllo;ll<llhi;ll++) {
		mlist=sim->mols->live[ll];
		nmol=sim->mols->nl[ll];
		for(m=0;m<nmol;m++) {
			mptr=mlist[m];
			if((mptr->ident>0 && i<0 && (ms==MSall || mptr->mstate==ms)) || (mptr->ident==i && (ms==MSall || mptr->mstate==ms))) {
				fprintf(fptr,"%i %i %i ",invk,mptr->ident,mptr->mstate);
				fprintVD(fptr,mptr->pos,sim->dim); }}}
	return CMDok; }


enum CMDcode cmdmolpos(simptr sim,cmdptr cmd,char *line2) {
	int i,d,m,ll,dim,lllo,llhi,nmol;
	moleculeptr *mlist,mptr;
	FILE *fptr;
	enum MolecState ms;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDobserve;
	i=readmolname(sim,line2,&ms);
	SCMDCHECK(!(i<0 && i>-5),"cannot read molecule and/or state name")
	line2=strnword(line2,2);
	fptr=scmdgetfptr(sim->cmds,line2);
	SCMDCHECK(fptr,"file name not recognized");
	dim=sim->dim;

	fprintf(fptr,"%g ",sim->time);
	if(i<0 || ms==MSall) {lllo=0;llhi=sim->mols->nlist;}
	else llhi=1+(lllo=sim->mols->listlookup[i][ms]);
	for(ll=lllo;ll<llhi;ll++) {
		mlist=sim->mols->live[ll];
		nmol=sim->mols->nl[ll];
		for(m=0;m<nmol;m++) {
			mptr=mlist[m];
			if((mptr->ident>0 && i<0 && (ms==MSall || mptr->mstate==ms)) || (mptr->ident==i && (ms==MSall || mptr->mstate==ms))) {
				for(d=0;d<dim;d++)
					fprintf(fptr,"%g ",mptr->pos[d]); }}}
		fprintf(fptr,"\n");
	return CMDok; }


enum CMDcode cmdmolmoments(simptr sim,cmdptr cmd,char *line2) {
	int i,ll,m,ctr,dim,d,d2,nmol;
	double v1[DIMMAX],m1[DIMMAX*DIMMAX];
	FILE *fptr;
	moleculeptr *mlist,mptr;
	enum MolecState ms;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDobserve;
	i=readmolname(sim,line2,&ms);
	SCMDCHECK(i>=0,"cannot read molecule and/or state name; 'all' is not permitted");
	if(ms==MSall) ms=MSsoln;
	line2=strnword(line2,2);
	fptr=scmdgetfptr(sim->cmds,line2);
	SCMDCHECK(fptr,"file name not recognized");
	dim=sim->dim;

	ll=sim->mols->listlookup[i][ms];
	mlist=sim->mols->live[ll];
	nmol=sim->mols->nl[ll];
	ctr=0;
	for(d=0;d<dim;d++) v1[d]=0;
	for(d=0;d<dim*dim;d++) m1[d]=0;
	for(m=0;m<nmol;m++) {
		mptr=mlist[m];
		if(mptr->ident==i && mptr->mstate==ms) {
			ctr++;
			for(d=0;d<dim;d++) v1[d]+=mlist[m]->pos[d]; }}
	for(d=0;d<dim;d++) v1[d]/=1.0*ctr;
	for(m=0;m<nmol;m++) {
		mptr=mlist[m];
		if(mptr->ident==i && mptr->mstate==ms) {
			for(d=0;d<dim;d++)
				for(d2=0;d2<dim;d2++)
					m1[d*dim+d2]+=(mlist[m]->pos[d]-v1[d])*(mlist[m]->pos[d2]-v1[d2]); }}
	fprintf(fptr,"%g %i",sim->time,ctr);
	for(d=0;d<dim;d++) fprintf(fptr," %g",v1[d]);
	for(d=0;d<dim;d++)
		for(d2=0;d2<dim;d2++)
			fprintf(fptr," %g",m1[d*dim+d2]/ctr);
	fprintf(fptr,"\n");
	return CMDok; }


enum CMDcode cmdsavesim(simptr sim,cmdptr cmd,char *line2) {
	FILE *fptr;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDobserve;
	fptr=scmdgetfptr(sim->cmds,line2);
	SCMDCHECK(fptr,"file name not recognized");
	if(line2) strcutwhite(line2,2);

	fprintf(fptr,"# Configuration file automatically created by Smoldyn\n\n");
	writesim(sim,fptr);
	writegraphss(sim,fptr);
	writemols(sim,fptr);
	writewalls(sim,fptr);
	writesurfaces(sim,fptr);
	writecomparts(sim,fptr);
	writereactions(sim,fptr);
	mzrsswrite(sim,fptr);
	scmdwritecommands(sim->cmds,fptr,line2);
	writemolecules(sim,fptr);
	fprintf(fptr,"\nend_file\n");
	return CMDok; }


void cmdmeansqrdispfree(cmdptr cmd) {
	int j;

	if(cmd->v2 && cmd->i1)
		for(j=0;j<cmd->i1;j++)
			free(((double**)(cmd->v2))[j]);
	if(cmd->v2) free(cmd->v2);
	if(cmd->v1) free(cmd->v1);
	return; }


enum CMDcode cmdmeansqrdisp(simptr sim,cmdptr cmd,char *line2) {
	static char dimstr[STRCHAR];
	int i,j,d,itct,ll,dim,ctr,m,msddim,nmol;
	FILE *fptr;
	moleculeptr *mlist;
	double r2,sum,sum4,diff,*pos,**v2;
	long int *v1;
	enum MolecState ms;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDobserve;
	i=readmolname(sim,line2,&ms);
	SCMDCHECK(i>=0,"cannot read molecule and/or state name; 'all' is not permitted");
	if(ms==MSall) ms=MSsoln;
	line2=strnword(line2,2);
	SCMDCHECK(line2,"missing dimension information");
	itct=sscanf(line2,"%s",dimstr);
	SCMDCHECK(itct==1,"cannot read dimension information");
	if(!strcmp(dimstr,"all")) msddim=-1;
	else {
		itct=sscanf(dimstr,"%i",&msddim);
		SCMDCHECK(itct==1,"cannot read dimension");
		SCMDCHECK(msddim>=0 && msddim<sim->dim,"dimension out of range"); }
	line2=strnword(line2,2);
	fptr=scmdgetfptr(sim->cmds,line2);
	SCMDCHECK(fptr,"file name not recognized");

	SCMDCHECK(cmd->i2!=2,"error on setup");					// failed before, don't try again

	ll=sim->mols->listlookup[i][ms];
	dim=sim->dim;
	mlist=sim->mols->live[ll];
	nmol=sim->mols->nl[ll];

	if(!cmd->i2) {										// test for first run
		cmd->i2=1;											// if first run, set up data structures
		ctr=0;
		for(m=0;m<nmol;m++)
			if(mlist[m]->ident==i && mlist[m]->mstate==ms) ctr++;
		cmd->i1=ctr;										// size of arrays
		SCMDCHECK(ctr>0,"no molecules to track");
		cmd->freefn=&cmdmeansqrdispfree;
		cmd->v1=calloc(ctr,sizeof(long int));	// v1 is serial numbers
		if(!cmd->v1) {cmd->i2=2;return CMDwarn;}
		for(j=0;j<ctr;j++) ((long int*)cmd->v1)[j]=0;
		cmd->v2=calloc(ctr,sizeof(double**));	// v2 is positions
		if(!cmd->v2) {cmd->i2=2;return CMDwarn;}
		for(j=0;j<ctr;j++) ((double**)cmd->v2)[j]=NULL;
		for(j=0;j<ctr;j++) {
			((double**)cmd->v2)[j]=(double*)calloc(dim,sizeof(double));
			if(!((double**)cmd->v2)[j]) {cmd->i2=2;return CMDwarn;}
			for(d=0;d<dim;d++) ((double**)cmd->v2)[j][d]=0; }
		ctr=0;
		for(m=0;m<nmol;m++) {
			if(mlist[m]->ident==i && mlist[m]->mstate==ms) {
				((long int*)cmd->v1)[ctr]=mlist[m]->serno;
				for(d=0;d<dim;d++)
					((double**)cmd->v2)[ctr][d]=mlist[m]->posoffset[d]+mlist[m]->pos[d];
				ctr++; }}
		sortVliv((long int*)cmd->v1,(void**)cmd->v2,cmd->i1); }

	ctr=0;														// start of code that is run every invokation
	sum=0;
	sum4=0;
	v1=(long int*)cmd->v1;
	v2=(double**)cmd->v2;
	for(m=0;m<nmol;m++)
		if(mlist[m]->ident==i && mlist[m]->mstate==ms) {
			j=locateVli(v1,mlist[m]->serno,cmd->i1);
			if(j>=0) {
				pos=mlist[m]->pos;
				ctr++;
				if(msddim<0) {
					r2=0;
					for(d=0;d<dim;d++) {
						diff=mlist[m]->posoffset[d]+pos[d]-v2[j][d];
						r2+=diff*diff; }
					sum+=r2;
					sum4+=r2*r2; }
				else {
					diff=mlist[m]->posoffset[msddim]+pos[msddim]-v2[j][msddim];
					sum+=diff*diff;
					sum4+=diff*diff*diff*diff; }}}
	fprintf(fptr,"%g %g %g\n",sim->time,sum/ctr,sum4/ctr);

	return CMDok; }


enum CMDcode cmdmeansqrdisp2(simptr sim,cmdptr cmd,char *line2) {
	static char dimstr[STRCHAR];
	int i,j,d,itct,ll,dim,ctr,m,msddim,nmol,maxmoment,maxmol,mom;
	FILE *fptr;
	moleculeptr *mlist;
	static double sum[17];
	double r2,diff,**v2,*dblptr;
	long int *v1;
	enum MolecState ms;
	char startchar,reportchar;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDobserve;
	i=readmolname(sim,line2,&ms);
	SCMDCHECK(i>=0,"cannot read molecule and/or state name; 'all' is not permitted");
	if(ms==MSall) ms=MSsoln;
	line2=strnword(line2,2);
	SCMDCHECK(line2,"missing dimension information");
	itct=sscanf(line2,"%s",dimstr);
	SCMDCHECK(itct==1,"cannot read dimension information");
	if(!strcmp(dimstr,"all")) msddim=-1;
	else {
		itct=sscanf(dimstr,"%i",&msddim);
		SCMDCHECK(itct==1,"cannot read dimension");
		SCMDCHECK(msddim>=0 && msddim<sim->dim,"dimension out of range"); }
	line2=strnword(line2,2);
	SCMDCHECK(line2,"insufficient arguements");
	itct=sscanf(line2,"%c %c %i %i",&startchar,&reportchar,&maxmol,&maxmoment);
	SCMDCHECK(itct==4,"cannot read start, report, max_mol, or max_moment information");
	SCMDCHECK(maxmol>0,"max_mol has to be at least 1");
	SCMDCHECK(maxmoment>0,"maxmoment has to be at least 1");
	SCMDCHECK(maxmoment<=16,"max_moment cannot exceed 16");
	line2=strnword(line2,5);
	fptr=scmdgetfptr(sim->cmds,line2);
	SCMDCHECK(fptr,"file name not recognized");

	SCMDCHECK(cmd->i2!=2,"error on setup");					// failed before, don't try again

	ll=sim->mols->listlookup[i][ms];
	dim=sim->dim;
	mlist=sim->mols->live[ll];
	nmol=sim->mols->nl[ll];

	if(!cmd->i2) {										// test for first run
		cmd->i2=1;											// if first run, set up data structures
		cmd->i1=maxmol;
		cmd->i3=0;
		cmd->freefn=&cmdmeansqrdispfree;
		cmd->v1=calloc(maxmol,sizeof(long int));	// v1 is serial numbers
		if(!cmd->v1) {cmd->i2=2;return CMDwarn;}
		v1=(long int*)cmd->v1;
		for(j=0;j<maxmol;j++) v1[j]=0;
		cmd->v2=calloc(maxmol,sizeof(double**));	// v2 is positions
		if(!cmd->v2) {cmd->i2=2;return CMDwarn;}
		v2=(double**)cmd->v2;
		for(j=0;j<maxmol;j++) v2[j]=NULL;
		for(j=0;j<maxmol;j++) {
			v2[j]=(double*)calloc(2*dim+1,sizeof(double));
			if(!v2[j]) {cmd->i2=2;return CMDwarn;}
			for(d=0;d<2*dim+1;d++) v2[j][d]=0; }
		ctr=0;
		for(m=0;m<nmol;m++) {
			if(mlist[m]->ident==i && mlist[m]->mstate==ms) {
				SCMDCHECK(ctr<maxmol,"insufficient allocated space");
				v1[ctr]=mlist[m]->serno;
				if(startchar=='c') v2[ctr][0]=0;
				else v2[ctr][0]=2.0;
				for(d=0;d<dim;d++)
					v2[ctr][1+d]=v2[ctr][dim+1+d]=mlist[m]->posoffset[d]+mlist[m]->pos[d];
				ctr++; }}
		cmd->i3=ctr;
		if(cmd->i3>0) sortVliv(v1,(void**)cmd->v2,cmd->i3); }

	v1=(long int*)cmd->v1;						// start of code that is run every invokation
	v2=(double**)cmd->v2;

	for(m=0;m<nmol;m++) {							// update tracking information for all tracked molecules
		if(mlist[m]->ident==i && mlist[m]->mstate==ms) {
			j=locateVli(v1,mlist[m]->serno,cmd->i3);
			if(j>=0) {										// molecule was found
				v2[j][0]+=1.0;
				if(v2[j][0]==3.0) {					// molecule is being tracked and exists, so record current positions
					for(d=0;d<dim;d++)
						v2[j][dim+1+d]=mlist[m]->posoffset[d]+mlist[m]->pos[d]; }}
			else if(startchar!='i') {			// molecule was not found but should be tracked
				if(cmd->i3==cmd->i1) SCMDCHECK(0,"not enough allocated space");
				j=cmd->i3++;					// find empty spot
				v1[j]=mlist[m]->serno;
				v2[j][0]=3.0;
				for(d=0;d<dim;d++)
					v2[j][1+d]=v2[j][dim+1+d]=mlist[m]->posoffset[d]+mlist[m]->pos[d]; }}}
	if(startchar!='i')								// resort lists if appropriate
		if(cmd->i3>0) sortVliv(v1,(void**)cmd->v2,cmd->i3);

	for(mom=0;mom<=maxmoment;mom++)
		sum[mom]=0;
	for(j=0;j<cmd->i3;j++) {					// find moments of all reported results
		if((reportchar=='e' && v2[j][0]==3.0) || (reportchar=='r' && v2[j][0]==2.0)) { // molecule should be recorded
			if(msddim<0) {
				r2=0;
				for(d=0;d<dim;d++) {
					diff=v2[j][dim+1+d]-v2[j][1+d];
					r2+=diff*diff; }
				r2=sqrt(r2);
				for(mom=0;mom<=maxmoment;mom++)
					sum[mom]+=pow(r2,mom); }
			else {
				diff=v2[j][dim+1+msddim]-v2[j][1+msddim];
				for(mom=0;mom<=maxmoment;mom++)
					sum[mom]+=pow(diff,mom); }}}

	if(sum[0]>0) {
		fprintf(fptr,"%g %g",sim->time,sum[0]);					// display results
		for(mom=1;mom<=maxmoment;mom++) {
			fprintf(fptr," %g",sum[mom]/sum[0]); }
		fprintf(fptr,"\n"); }

	for(j=0;j<cmd->i3;j++) {							// stop tracking expired molecules
		if(v2[j][0]==0 || v2[j][0]==2.0) {
			v1[j]=v1[cmd->i3-1];
			v1[cmd->i3-1]=0;
			dblptr=v2[j];
			v2[j]=v2[cmd->i3-1];
			v2[cmd->i3-1]=dblptr;
			v2[cmd->i3-1][0]=0;
			j--;
			cmd->i3--; }
		else
			v2[j][0]-=1.0; }
	if(cmd->i3>0) sortVliv(v1,(void**)cmd->v2,cmd->i3);

	return CMDok; }


enum CMDcode cmddiagnostics(simptr sim,cmdptr cmd,char *line2) {
	int itct,order;
	static char nm[STRCHAR];
	enum SmolStruct ss;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDobserve;
	SCMDCHECK(line2,"missing argument");
	itct=sscanf(line2,"%s",nm);
	SCMDCHECK(itct==1,"read failure");
	ss=simstring2ss(nm);
	SCMDCHECK(ss!=SSnone,"diagnostic type not recognized");

	if(ss==SSsim || ss==SSall) simoutput(sim);
	if(ss==SSwall || ss==SSall) walloutput(sim);
	if(ss==SSmolec || ss==SSall) molssoutput(sim);
	if(ss==SSsurf || ss==SSall) surfaceoutput(sim);
	if(ss==SScmd || ss==SSall) scmdoutput(sim->cmds);
	if(ss==SSbox || ss==SSall) boxssoutput(sim);
	if(ss==SSrxn || ss==SSall)
		for(order=0;order<MAXORDER;order++) rxnoutput(sim,order);
	if(ss==SScmpt || ss==SSall) compartoutput(sim);
	if(ss==SSport || ss==SSall) portoutput(sim);
	if(ss==SSmzr || ss==SSall) mzrssoutput(sim);
	if(ss==SScheck || ss==SSall) checksimparams(sim);
	return CMDok; }


/**********************************************************/
/****************** system manipulation ********************/
/**********************************************************/


enum CMDcode cmdset(simptr sim,cmdptr cmd,char *line2) {
	int er,itct;
	char word[STRCHAR],erstr[STRCHAR];

	if(line2 && !strcmp(line2,"cmdtype")) return CMDmanipulate;
	SCMDCHECK(line2,"missing argument");
	itct=sscanf(line2,"%s",word);
	SCMDCHECK(itct==1,"missing statement");
	line2=strnword(line2,2);
	SCMDCHECK(line2,"missing statement text");
	er=simreadstring(sim,word,line2,erstr);
	SCMDCHECK(!er,erstr);
	return CMDok; }


enum CMDcode cmdpointsource(simptr sim,cmdptr cmd,char *line2) {
	int itct,num,i;
	static char nm[STRCHAR];
	double pos[DIMMAX];

	if(line2 && !strcmp(line2,"cmdtype")) return CMDmanipulate;
	SCMDCHECK(line2,"missing argument");
	SCMDCHECK(sim->mols,"molecules are undefined");
	itct=sscanf(line2,"%s %i",nm,&num);
	SCMDCHECK(itct==2,"read failure");
	SCMDCHECK(num>=0,"number cannot be negative");
	i=stringfind(sim->mols->spname,sim->mols->nspecies,nm);
	SCMDCHECK(i>=1,"name not recognized");
	line2=strnword(line2,3);
	SCMDCHECK(line2,"missing location");
	itct=strreadnd(line2,sim->dim,pos,NULL);
	SCMDCHECK(itct==sim->dim,"insufficient location dimensions");
	SCMDCHECK(addmol(sim,num,i,pos,pos,1)==0,"not enough available molecules");
	return CMDok; }


enum CMDcode cmdvolumesource(simptr sim,cmdptr cmd,char *line2) {
	int itct,num,i,d;
	static char nm[STRCHAR];
	double poslo[DIMMAX],poshi[DIMMAX];
	
	if(line2 && !strcmp(line2,"cmdtype")) return CMDmanipulate;
	SCMDCHECK(line2,"missing argument");
	SCMDCHECK(sim->mols,"molecules are undefined");
	itct=sscanf(line2,"%s %i",nm,&num);
	SCMDCHECK(itct==2,"read failure");
	SCMDCHECK(num>=0,"number cannot be negative");
	i=stringfind(sim->mols->spname,sim->mols->nspecies,nm);
	SCMDCHECK(i>=1,"name not recognized");
	line2=strnword(line2,3);
	SCMDCHECK(line2,"missing location");
	for(d=0;d<sim->dim;d++) {
		SCMDCHECK(line2,"missing argument");
		itct=sscanf(line2,"%lg %lg",&poslo[d],&poshi[d]);
		SCMDCHECK(itct==2,"read failure");
		line2=strnword(line2,3); }
	SCMDCHECK(addmol(sim,num,i,poslo,poshi,1)==0,"not enough available molecules");
	return CMDok; }


enum CMDcode cmdmovesurfacemol(simptr sim,cmdptr cmd,char *line2) {
	int itct,i1,s1,s2,p1,p2,ll,lllo,llhi,m,d,nmol;
	static char nm[STRCHAR],nm2[STRCHAR];
	double prob;
	enum MolecState ms1,ms2;
	enum PanelShape ps1,ps2;
	moleculeptr mptr,*mlist;
	double pos[DIMMAX];
	surfaceptr srf,srf2;
	panelptr pnl2;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDmanipulate;
	SCMDCHECK(line2,"missing arguments");
	SCMDCHECK(sim->mols,"molecules are undefined");
	SCMDCHECK(sim->srfss,"surfaces are undefined");
	itct=sscanf(line2,"%s %lg",nm,&prob);
	SCMDCHECK(itct==2,"failed to read molecule name or probability");
	i1=readmolname(sim,nm,&ms1);
	SCMDCHECK(i1>=0 || i1==-5,"failed to read molecule name");
	SCMDCHECK(ms1==MSfront || ms1==MSback || ms1==MSup || ms1==MSback || ms1==MSall,"failed to read molecule state or illegal state");
	SCMDCHECK(prob>=0 && prob<=1,"probability out of bounds");
	line2=strnword(line2,3);
	SCMDCHECK(line2,"missing originating surface:panel");
	itct=sscanf(line2,"%s %s",nm,nm2);
	SCMDCHECK(itct==2,"failed to read surfaces and panels");
	s1=readsurfacename(sim,nm,&ps1,&p1);
	SCMDCHECK(s1>=0,"failed to read surface1");
	SCMDCHECK(p1>=0 || p1==-1,"failed to read panel1");
	s2=readsurfacename(sim,nm2,&ps2,&p2);
	SCMDCHECK(s2>=0,"failed to read surface2");
	SCMDCHECK(p2>=0 || p2==-1,"failed to read panel2");
	line2=strnword(line2,3);
	if(line2) {
		itct=sscanf(line2,"%s",nm);
		SCMDCHECK(itct==1,"failed to read final state");
		ms2=molstring2ms(nm);
		SCMDCHECK(ms2!=MSnone,"failed to read final state");
		line2=strnword(line2,2); }
	else ms2=MSnone;

	srf=sim->srfss->srflist[s1];
	srf2=sim->srfss->srflist[s2];
	if(p2==-1) pnl2=NULL;
	else pnl2=srf2->panels[ps2][p2];

	if(i1<0 || ms1==MSall) {lllo=0;llhi=sim->mols->nlist;}
	else llhi=1+(lllo=sim->mols->listlookup[i1][ms1]);
	for(ll=lllo;ll<llhi;ll++) {
		mlist=sim->mols->live[ll];
		nmol=sim->mols->nl[ll];
		for(m=0;m<nmol;m++) {
			mptr=mlist[m];
			if((mptr->ident>0 && i1<0 && (ms1==MSall || mptr->mstate==ms1)) || (mptr->ident==i1 && (ms1==MSall || mptr->mstate==ms1)))
				if(mptr->pnl && mptr->pnl->srf==srf && (p1==-1 || mptr->pnl==srf->panels[ps1][p1]))
					if(randCOD()<prob) {
						if(p2==-1) pnl2=surfrandpos(srf2,pos,sim->dim);
						else panelrandpos(pnl2,pos,sim->dim);
						for(d=0;d<sim->dim;d++) {
							mptr->posoffset[d]=mptr->pos[d]-pos[d];
							mptr->posx[d]=mptr->pos[d]=pos[d]; }
						molchangeident(sim,mptr,ll,m,mptr->ident,ms2==MSnone?mptr->mstate:ms2,pnl2); }}}
	return CMDok; }


enum CMDcode cmdkillmol(simptr sim,cmdptr cmd,char *line2) {
	int i,ll,m,lllo,llhi,nmol;
	moleculeptr *mlist,mptr;
	enum MolecState ms;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDmanipulate;
	i=readmolname(sim,line2,&ms);
	SCMDCHECK(!(i<0 && i>-5),"cannot read molecule and/or state name");
	if(i<0 || ms==MSall) {lllo=0;llhi=sim->mols->nlist;}
	else llhi=1+(lllo=sim->mols->listlookup[i][ms]);
	for(ll=lllo;ll<llhi;ll++) {
		mlist=sim->mols->live[ll];
		nmol=sim->mols->nl[ll];
		for(m=0;m<sim->mols->nl[ll];m++) {
			mptr=mlist[m];
			if((i<0 && ms==MSall) || (i<0 && mptr->mstate==ms) || (mptr->ident==i && ms==MSall) || (mptr->ident==i && mptr->mstate==ms))
				molkill(sim,mptr,ll,m); }}
	return CMDok; }


enum CMDcode cmdkillmolprob(simptr sim,cmdptr cmd,char *line2) {
	int itct,i,ll,m,nmol,lllo,llhi;
	moleculeptr *mlist,mptr;
	double prob;
	enum MolecState ms;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDmanipulate;
	i=readmolname(sim,line2,&ms);
	SCMDCHECK(!(i<0 && i>-5),"cannot read molecule and/or state name");
	line2=strnword(line2,2);
	SCMDCHECK(line2,"missing probability value");
	itct=sscanf(line2,"%lg",&prob);
	SCMDCHECK(itct==1,"killmolprob format: name[(state)] probability");
	SCMDCHECK(prob>=0 && prob<=1,"probability needs to be between 0 and 1");
	if(i<0 || ms==MSall) {lllo=0;llhi=sim->mols->nlist;}
	else llhi=1+(lllo=sim->mols->listlookup[i][ms]);
	for(ll=lllo;ll<llhi;ll++) {
		mlist=sim->mols->live[ll];
		nmol=sim->mols->nl[ll];
		for(m=0;m<nmol;m++) {
			mptr=mlist[m];
			if(coinrandD(prob) && ((i<0 && ms==MSall) || (i<0 && mptr->mstate==ms) || (mptr->ident==i && ms==MSall) || (mptr->ident==i && mptr->mstate==ms)))
				molkill(sim,mptr,ll,m); }}
	return CMDok; }


enum CMDcode cmdkillmolinsphere(simptr sim,cmdptr cmd,char *line2) {
	int itct,i,s,ll,m,lllo,llhi,nmol;
	static char nm[STRCHAR];
	moleculeptr *mlist,mptr;
	enum MolecState ms;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDmanipulate;
	if(!sim->srfss) return CMDok;
	i=readmolname(sim,line2,&ms);
	SCMDCHECK(!(i<0 && i>-5),"cannot read molecule and/or state name");
	line2=strnword(line2,2);
	SCMDCHECK(line2,"missing surface name");
	itct=sscanf(line2,"%s",nm);
	SCMDCHECK(itct==1,"cannot read surface name");
	if(!strcmp(nm,"all")) s=-1;
	else {
		s=stringfind(sim->srfss->snames,sim->srfss->nsrf,nm);
		SCMDCHECK(s>=0,"surface not recognized"); }
	if(i<0 || ms==MSall) {lllo=0;llhi=sim->mols->nlist;}
	else llhi=1+(lllo=sim->mols->listlookup[i][ms]);
	for(ll=lllo;ll<llhi;ll++) {
		mlist=sim->mols->live[ll];
		nmol=sim->mols->nl[ll];
		for(m=0;m<nmol;m++) {
			mptr=mlist[m];
			if((i<0 && ms==MSall) || (i<0 && mptr->mstate==ms) || (mptr->ident==i && ms==MSall) || (mptr->ident==i && mptr->mstate==ms))
				if(molinpanels(sim,ll,m,s,'s')) molkill(sim,mptr,ll,m); }}
	return CMDok; }


enum CMDcode cmdkillmoloutsidesystem(simptr sim,cmdptr cmd,char *line2) {
	int i,ll,m,lllo,llhi,nmol;
	moleculeptr *mlist,mptr;
	enum MolecState ms;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDmanipulate;
	if(!sim->srfss) return CMDok;
	i=readmolname(sim,line2,&ms);
	SCMDCHECK(!(i<0 && i>-5),"cannot read molecule and/or state name");
	if(i<0 || ms==MSall) {lllo=0;llhi=sim->mols->nlist;}
	else llhi=1+(lllo=sim->mols->listlookup[i][ms]);
	for(ll=lllo;ll<llhi;ll++) {
		mlist=sim->mols->live[ll];
		nmol=sim->mols->nl[ll];
		for(m=0;m<nmol;m++) {
			mptr=mlist[m];
			if((i<0 && ms==MSall) || (i<0 && mptr->mstate==ms) || (mptr->ident==i && ms==MSall) || (mptr->ident==i && mptr->mstate==ms))
				if(!posinsystem(sim,mptr->pos)) molkill(sim,mptr,ll,m); }}
	return CMDok; }


enum CMDcode cmdfixmolcount(simptr sim,cmdptr cmd,char *line2) {
	int itct,num,i,ll,m,ct,numl;
	static char nm[STRCHAR];
	double pos1[DIMMAX],pos2[DIMMAX];

	if(line2 && !strcmp(line2,"cmdtype")) return CMDmanipulate;
	SCMDCHECK(line2,"missing argument");
	SCMDCHECK(sim->mols,"molecules are undefined");
	itct=sscanf(line2,"%s %i",nm,&num);
	SCMDCHECK(itct==2,"read failure");
	SCMDCHECK(num>=0,"number cannot be negative");
	i=stringfind(sim->mols->spname,sim->mols->nspecies,nm);
	SCMDCHECK(i>=1,"name not recognized");

	ll=sim->mols->listlookup[i][MSsoln];
	numl=sim->mols->nl[ll];
	ct=0;
	for(m=0;m<numl;m++)
		if(sim->mols->live[ll][m]->ident==i) ct++;

	if(ct==num);
	else if(ct<num) {
		systemcorners(sim,pos1,pos2);
		SCMDCHECK(addmol(sim,num-ct,i,pos1,pos2,1)==0,"not enough available molecules"); }
	else {
		num=ct-num;
		for(;num>0;num--) {
			m=intrand(numl);
			while(sim->mols->live[ll][m]->ident!=i) m=(m==numl-1)?0:m+1;
			molkill(sim,sim->mols->live[ll][m],ll,m); }}

	return CMDok; }


enum CMDcode cmdfixmolcountonsurf(simptr sim,cmdptr cmd,char *line2) {
	int itct,num,i,ll,m,ct,numl,s;
	static char nm[STRCHAR];
	enum MolecState ms;
	surfaceptr sptr;
	moleculeptr mptr;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDmanipulate;
	SCMDCHECK(line2,"missing argument");
	i=readmolname(sim,line2,&ms);
	SCMDCHECK(i>0,"failed to read molecule name or state");
	SCMDCHECK(ms!=MSsoln && ms!=MSbsoln,"molecule state needs to be surface-bound");
	line2=strnword(line2,2);
	SCMDCHECK(line2,"fixmolcountonsurf format: species(state) number surface");
	itct=sscanf(line2,"%i %s",&num,nm);
	SCMDCHECK(itct==2,"read failure");
	SCMDCHECK(num>=0,"number cannot be negative");
	SCMDCHECK(sim->srfss,"no surfaces defined");
	s=stringfind(sim->srfss->snames,sim->srfss->nsrf,nm);
	SCMDCHECK(s>=0,"surface not recognized");
	sptr=sim->srfss->srflist[s];

	ll=sim->mols->listlookup[i][ms];
	numl=sim->mols->nl[ll];
	ct=0;
	for(m=0;m<numl;m++) {
		mptr=sim->mols->live[ll][m];
		if(mptr->ident==i && mptr->mstate==ms && mptr->pnl->srf==sptr) ct++; }

	if(ct==num);
	else if(ct<num) {
		SCMDCHECK(addsurfmol(sim,num-ct,i,ms,NULL,NULL,s,PSall,NULL)==0,"not enough available molecules"); }
	else {
		num=ct-num;
		for(;num>0;num--) {
			m=intrand(numl);
			mptr=sim->mols->live[ll][m];
			while(!(mptr->ident==i && mptr->mstate==ms && mptr->pnl->srf==sptr)) {
				m=(m==numl-1)?0:m+1;
				mptr=sim->mols->live[ll][m]; }
			molkill(sim,mptr,ll,m); }}

	return CMDok; }


enum CMDcode cmdfixmolcountincmpt(simptr sim,cmdptr cmd,char *line2) {
	int itct,num,i,ll,m,ct,numl,c;
	static char nm[STRCHAR];
	moleculeptr mptr;
	compartptr cmpt;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDmanipulate;
	SCMDCHECK(line2,"missing argument");
	SCMDCHECK(sim->mols,"molecules are undefined");
	SCMDCHECK(sim->cmptss,"compartments are undefined");
	itct=sscanf(line2,"%s %i",nm,&num);
	SCMDCHECK(itct==2,"read failure");
	SCMDCHECK(num>=0,"number cannot be negative");
	i=stringfind(sim->mols->spname,sim->mols->nspecies,nm);
	SCMDCHECK(i>=1,"molecule name not recognized");
	line2=strnword(line2,3);
	SCMDCHECK(line2,"compartment name missing");
	itct=sscanf(line2,"%s",nm);
	c=stringfind(sim->cmptss->cnames,sim->cmptss->ncmpt,nm);
	SCMDCHECK(c>=0,"compartment not recognized");
	cmpt=sim->cmptss->cmptlist[c];

	ll=sim->mols->listlookup[i][MSsoln];
	numl=sim->mols->nl[ll];
	ct=0;
	for(m=0;m<numl;m++) {
		mptr=sim->mols->live[ll][m];
		if(mptr->ident==i && mptr->mstate==MSsoln && posincompart(sim,mptr->pos,cmpt)) ct++; }

	if(ct==num);
	else if(ct<num) {
		SCMDCHECK(addcompartmol(sim,num-ct,i,cmpt)==0,"not enough available molecules"); }
	else {
		num=ct-num;
		for(;num>0;num--) {
			m=intrand(numl);
			mptr=sim->mols->live[ll][m];
			while(!(mptr->ident==i && mptr->mstate==MSsoln && posincompart(sim,mptr->pos,cmpt))) {
				m=(m==numl-1)?0:m+1;
				mptr=sim->mols->live[ll][m]; }
			molkill(sim,mptr,ll,m); }}

	return CMDok; }


enum CMDcode cmdequilmol(simptr sim,cmdptr cmd,char *line2) {
	int itct,i1,i2,m,ll1,ll2,ll,nmol;
	moleculeptr *mlist,mptr;
	double prob;
	enum MolecState ms1,ms2;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDmanipulate;
	i1=readmolname(sim,line2,&ms1);
	SCMDCHECK(i1>0,"cannot read first molecule and/or state name; 'all' is not permitted");
	if(ms1==MSall) ms1=MSsoln;
	line2=strnword(line2,2);
	i2=readmolname(sim,line2,&ms2);
	SCMDCHECK(i2>0,"cannot read second molecule and/or state name; 'all' is not permitted");
	if(ms2==MSall) ms2=MSsoln;
	SCMDCHECK((ms1==MSsoln && ms2==MSsoln) || (ms1!=MSsoln && ms2!=MSsoln),"cannot equilibrate between solution and surface-bound");
	line2=strnword(line2,2);
	SCMDCHECK(line2,"missing probability argument");
	itct=sscanf(line2,"%lg",&prob);
	SCMDCHECK(itct==1,"failed to read probability");
	SCMDCHECK(prob>=0 && prob<=1,"probability is out of bounds");

	ll1=sim->mols->listlookup[i1][ms1];
	ll2=sim->mols->listlookup[i2][ms2];
	for(ll=ll1;ll!=-1;ll=(ll==ll2?-1:ll2)) {
		mlist=sim->mols->live[ll];
		nmol=sim->mols->nl[ll];
		for(m=0;m<nmol;m++) {
			mptr=mlist[m];
			if((mptr->ident==i1 && mptr->mstate==ms1) || (mptr->ident==i2 && mptr->mstate==ms2)) {
				if(coinrandD(prob))
					molchangeident(sim,mptr,ll,m,i2,ms2,mptr->pnl);
				else
					molchangeident(sim,mptr,ll,m,i1,ms1,mptr->pnl); }}}
	return CMDok; }


enum CMDcode cmdreplacexyzmol(simptr sim,cmdptr cmd,char *line2) {
	int itct,i,m,d,ll;
	moleculeptr *mlist;
	boxptr bptr;
	double pos[DIMMAX];
	enum MolecState ms;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDmanipulate;
	i=readmolname(sim,line2,&ms);
	SCMDCHECK(i>=0,"cannot read molecule and/or state name; 'all' is not permitted");
	SCMDCHECK(ms!=MSall,"molecule state may not be 'all'");
	line2=strnword(line2,2);
	SCMDCHECK(line2,"missing position information");
	itct=strreadnd(line2,sim->dim,pos,NULL);
	SCMDCHECK(itct==sim->dim,"insufficient dimensions entered");
	bptr=pos2box(sim,pos);
	ll=sim->mols->listlookup[i][ms];
	mlist=bptr->mol[ll];
	for(m=0;m<bptr->nmol[ll];m++) {
		for(d=0;d<sim->dim;d++)
			if(mlist[m]->pos[d]!=pos[d]) d=sim->dim+1;
		if(d==sim->dim) {
			molchangeident(sim,mlist[m],ll,-1,i,ms,mlist[m]->pnl);
			m=bptr->nmol[ll]+1; }}
	return CMDok; }


enum CMDcode cmdreplacevolmol(simptr sim,cmdptr cmd,char *line2) {
	int m,itct,dim,d,b,b1,b2,i1,i2,ll;
	double *pos,poslo[DIMMAX],poshi[DIMMAX],frac;
	boxptr bptr1,bptr2,bptr;
	boxssptr boxs;
	moleculeptr *mlist;
	enum MolecState ms1,ms2;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDmanipulate;
	i1=readmolname(sim,line2,&ms1);
	SCMDCHECK(i1>0,"cannot read first molecule and/or state name; 'all' is not permitted");
	SCMDCHECK(ms1!=MSall,"first state may not be 'all'");
	line2=strnword(line2,2);
	i2=readmolname(sim,line2,&ms2);
	SCMDCHECK(i2>=0,"cannot read second molecule and/or state name; 'all' is not permitted");
	SCMDCHECK(ms2!=MSall,"second state may not be 'all'");
	SCMDCHECK((ms1==MSsoln && ms2==MSsoln) || (ms1!=MSsoln && ms2!=MSsoln),"cannot equilibrate between solution and surface-bound");
	line2=strnword(line2,2);
	SCMDCHECK(line2,"missing fraction information");
	itct=sscanf(line2,"%lg",&frac);
	SCMDCHECK(itct==1,"cannot read fraction");
	SCMDCHECK(frac>=0 && frac<=1,"fraction out of bounds");
	line2=strnword(line2,2);
	dim=sim->dim;
	boxs=sim->boxs;
	for(d=0;d<dim;d++) {
		SCMDCHECK(line2,"missing argument");
		itct=sscanf(line2,"%lg %lg",&poslo[d],&poshi[d]);
		SCMDCHECK(itct==2,"read failure");
		line2=strnword(line2,3); }

	bptr1=pos2box(sim,poslo);
	bptr2=pos2box(sim,poshi);
	b1=indx2addZV(bptr1->indx,boxs->side,dim);
	b2=indx2addZV(bptr2->indx,boxs->side,dim);
	ll=sim->mols->listlookup[i1][ms1];
	for(b=b1;b<=b2;b=nextaddZV(b,bptr1->indx,bptr2->indx,boxs->side,dim)) {
		bptr=boxs->blist[b];
		mlist=bptr->mol[ll];
		for(m=0;m<bptr->nmol[ll];m++) {
			if(mlist[m]->ident==i1 && mlist[m]->mstate==ms1) {
				pos=mlist[m]->pos;
				for(d=0;d<dim;d++) if(pos[d]<poslo[d] || pos[d]>poshi[d]) d=dim+1;
				if(d==dim && coinrandD(frac)) {
					molchangeident(sim,mlist[m],ll,-1,i2,ms2,mlist[m]->pnl); }}}}
	return CMDok; }


enum CMDcode cmdreplacecmptmol(simptr sim,cmdptr cmd,char *line2) {
	int m,itct,i1,i2,ll,c,numl;
	char nm[STRCHAR];
	double frac;
	enum MolecState ms1,ms2;
	compartptr cmpt;
	moleculeptr mptr;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDmanipulate;
	i1=readmolname(sim,line2,&ms1);
	SCMDCHECK(i1>0,"cannot read first molecule and/or state name; 'all' is not permitted");
	SCMDCHECK(ms1!=MSall,"first state may not be 'all'");
	line2=strnword(line2,2);
	SCMDCHECK(line2,"missing second species name");
	i2=readmolname(sim,line2,&ms2);
	SCMDCHECK(i2>=0,"cannot read second molecule and/or state name; 'all' is not permitted");
	SCMDCHECK(ms2!=MSall,"second state may not be 'all'");
	SCMDCHECK((ms1==MSsoln && ms2==MSsoln) || (ms1!=MSsoln && ms2!=MSsoln),"cannot equilibrate between solution and surface-bound");
	line2=strnword(line2,2);
	SCMDCHECK(line2,"missing fraction information");
	itct=sscanf(line2,"%lg",&frac);
	SCMDCHECK(itct==1,"cannot read fraction");
	SCMDCHECK(frac>=0 && frac<=1,"fraction out of bounds");
	line2=strnword(line2,2);
	SCMDCHECK(line2,"compartment name missing");
	itct=sscanf(line2,"%s",nm);
	c=stringfind(sim->cmptss->cnames,sim->cmptss->ncmpt,nm);
	SCMDCHECK(c>=0,"compartment not recognized");
	cmpt=sim->cmptss->cmptlist[c];

	ll=sim->mols->listlookup[i1][ms1];
	numl=sim->mols->nl[ll];
	for(m=0;m<numl;m++) {
		mptr=sim->mols->live[ll][m];
		if(mptr->ident==i1 && mptr->mstate==ms1 && posincompart(sim,mptr->pos,cmpt) && coinrandD(frac)) 
			molchangeident(sim,mptr,ll,m,i2,ms2,mptr->pnl); }
	return CMDok; }


enum CMDcode cmdmodulatemol(simptr sim,cmdptr cmd,char *line2) {
	int itct,i1,i2,m,ll1,ll2,nmol,ll;
	moleculeptr *mlist,mptr;
	double freq,shift,prob;
	enum MolecState ms1,ms2;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDmanipulate;
	i1=readmolname(sim,line2,&ms1);
	SCMDCHECK(i1>0,"cannot read first molecule and/or state name; 'all' is not permitted");
	SCMDCHECK(ms1!=MSall,"first state may not be 'all'");
	line2=strnword(line2,2);
	i2=readmolname(sim,line2,&ms2);
	SCMDCHECK(i2>=0,"cannot read second molecule and/or state name; 'all' is not permitted");
	SCMDCHECK(ms1!=MSall,"second state may not be 'all'");
	SCMDCHECK((ms1==MSsoln && ms2==MSsoln) || (ms1!=MSsoln && ms2!=MSsoln),"cannot equilibrate between solution and surface-bound");
	line2=strnword(line2,2);
	SCMDCHECK(line2,"missing frequency and shift");
	itct=sscanf(line2,"%lg %lg",&freq,&shift);
	SCMDCHECK(itct==2,"failure reading frequency or shift");

	ll1=sim->mols->listlookup[i1][ms1];
	ll2=sim->mols->listlookup[i2][ms2];
	prob=0.5*(1.0-cos(freq*sim->time+shift));
	for(ll=ll1;ll!=-1;ll=(ll==ll2?-1:ll2)) {
		mlist=sim->mols->live[ll];
		nmol=sim->mols->nl[ll];
		for(m=0;m<nmol;m++) {
			mptr=mlist[m];
			if((mptr->ident==i1 && mptr->mstate==ms1) || (mptr->ident==i2 && mptr->mstate==ms2)) {
				if(coinrandD(prob))
					molchangeident(sim,mptr,ll,m,i2,ms2,mptr->pnl);
				else
					molchangeident(sim,mptr,ll,m,i1,ms1,mptr->pnl); }}}
	return CMDok; }


enum CMDcode cmdreact1(simptr sim,cmdptr cmd,char *line2) {
	int itct,i,ll,m,r,nmol,lllo,llhi;
	static char rnm[STRCHAR];
	moleculeptr *mlist,mptr;
	enum MolecState ms;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDmanipulate;
	i=readmolname(sim,line2,&ms);
	SCMDCHECK(!(i<=0 && i>-5),"cannot read molecule and/or state name");
	line2=strnword(line2,2);
	SCMDCHECK(line2,"reaction name is missing");
	itct=sscanf(line2,"%s",rnm);
	SCMDCHECK(itct==1,"cannot read reaction name");
	SCMDCHECK(sim->rxnss[1],"no first order reactions defined");
	r=stringfind(sim->rxnss[1]->rname,sim->rxnss[1]->totrxn,rnm);
	SCMDCHECK(r>=0,"reaction not recognized");

	if(i<0) {lllo=0;llhi=sim->mols->nlist;}
	else llhi=1+(lllo=sim->mols->listlookup[i][ms]);
	for(ll=lllo;ll<llhi;ll++) {
		mlist=sim->mols->live[ll];
		nmol=sim->mols->nl[ll];
		for(m=0;m<nmol;m++) {
			mptr=mlist[m];
			if((i<0 && ms==MSall) || (i<0 && mptr->mstate==ms) || (mptr->ident==i && ms==MSall) || (mptr->ident==i && mptr->mstate==ms))
				if(doreact(sim,sim->rxnss[1]->rxn[r],mptr,NULL,ll,m,-1,-1,NULL,NULL)) return CMDwarn; }}
	return CMDok; }


enum CMDcode cmdsetrateint(simptr sim,cmdptr cmd,char *line2) {
	int itct,r,order;
	static char rnm[STRCHAR];
	double rateint;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDmanipulate;
	SCMDCHECK(line2,"missing argument");
	itct=sscanf(line2,"%s %lg",rnm,&rateint);
	SCMDCHECK(itct==2,"read failure");
	r=-1;
	if(sim->rxnss[0]) r=stringfind(sim->rxnss[0]->rname,sim->rxnss[0]->totrxn,rnm);
	if(r>=0) order=0;
	else {
		if(sim->rxnss[1]) r=stringfind(sim->rxnss[1]->rname,sim->rxnss[1]->totrxn,rnm);
		if(r>=0) order=1;
		else {
			if(sim->rxnss[2]) r=stringfind(sim->rxnss[2]->rname,sim->rxnss[2]->totrxn,rnm);
			if(r>=0) order=2;
			else SCMDCHECK(0,"reaction name not recognized"); }}
	SCMDCHECK(rateint>=0,"internal rate cannot be negative");
	if(order<2) sim->rxnss[order]->rxn[r]->prob=rateint;
	else sim->rxnss[order]->rxn[r]->bindrad2=rateint*rateint;
	return CMDok; }


enum CMDcode cmdsetsurfcoeff(simptr sim,cmdptr cmd,char *line2) {
	int itct,s;
	double rate;
	enum MolecState ms1, ms2; 
	char nm[STRCHAR],molname[STRCHAR], ms1string[STRCHAR],ms2string[STRCHAR];
	surfaceptr srf;
	surfacessptr srfss;
	int i;
	
	if(line2 && !strcmp(line2,"cmdtype")) return CMDmanipulate;
	SCMDCHECK(cmd->i1!=2,"error on setup");					// failed before, don't try again
	srfss=sim->srfss;
	SCMDCHECK(srfss,"no surfaces defined");
	SCMDCHECK(line2,"missing argument");
	itct=sscanf(line2,"%s %s %s %s %lg",nm,molname,ms1string,ms2string,&rate);	
	SCMDCHECK(itct==5,"read failure");
	s=stringfind(srfss->snames,srfss->nsrf,nm);	
	SCMDCHECK(s>=0,"surface name not recognized");
	srf=srfss->srflist[s];	
	i=stringfind(sim->mols->spname,sim->mols->nspecies,molname);
	SCMDCHECK(i>=0,"molecule name not recognized");
	ms1=molstring2ms(ms1string);
	ms2=molstring2ms(ms2string);
	SCMDCHECK(ms1!=MSall,"molecule state may not be 'all'");
	SCMDCHECK(ms2!=MSall,"molecule state may not be 'all'");	
	SCMDCHECK(ms1!=MSnone,"molecule state may not be 'none'");
	SCMDCHECK(ms2!=MSnone,"molecule state may not be 'none'");
	SCMDCHECK(rate>=0,"rate may not be negative");
	srf->srfrate[i][ms1][ms2]=rate;
	surfsettimestep(sim);
return CMDok; }


enum CMDcode cmdsettimestep(simptr sim,cmdptr cmd,char *line2) {
	int itct,er;
	double dt;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDmanipulate;
	SCMDCHECK(line2,"missing argument");
	itct=sscanf(line2,"%lg",&dt);
	SCMDCHECK(itct==1,"read failure");
	SCMDCHECK(dt>0,"time step must be >0");

	er=simsettime(sim,dt,3);
	SCMDCHECK(er==0,"error while setting the simulation time step");
	return CMDok; }


enum CMDcode cmdporttransport(simptr sim,cmdptr cmd,char *line2) {
	int itct,prt1,prt2;
	char nm1[STRCHAR],nm2[STRCHAR];
	portptr port1,port2;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDmanipulate;
	SCMDCHECK(line2,"missing argument");
	SCMDCHECK(sim->portss,"no port superstructure is defined");
	itct=sscanf(line2,"%s %s",nm1,nm2);
	SCMDCHECK(itct==2,"porttransport format: port1 port2");
	prt1=stringfind(sim->portss->portnames,sim->portss->nport,nm1);
	SCMDCHECK(prt1>=0,"name of port1 is not recognized");
	prt2=stringfind(sim->portss->portnames,sim->portss->nport,nm2);
	SCMDCHECK(prt2>=0,"name of port2 is not recognized");
	port1=sim->portss->portlist[prt1];
	port2=sim->portss->portlist[prt2];
	porttransport(sim,port1,sim,port2);
	return CMDok; }


enum CMDcode cmdexcludebox(simptr sim,cmdptr cmd,char *line2) {
	int m,itct,dim,d,b,b1,b2;
	double *pos,poslo[DIMMAX],poshi[DIMMAX];
	boxptr bptr1,bptr2,bptr;
	boxssptr boxs;
	moleculeptr *mlist;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDmanipulate;
	dim=sim->dim;
	boxs=sim->boxs;
	for(d=0;d<dim;d++) {
		SCMDCHECK(line2,"missing argument");
		itct=sscanf(line2,"%lg %lg",&poslo[d],&poshi[d]);
		SCMDCHECK(itct==2,"read failure");
		line2=strnword(line2,3); }

	bptr1=pos2box(sim,poslo);
	bptr2=pos2box(sim,poshi);
	b1=indx2addZV(bptr1->indx,boxs->side,dim);
	b2=indx2addZV(bptr2->indx,boxs->side,dim);
	for(b=b1;b<=b2;b=nextaddZV(b,bptr1->indx,bptr2->indx,boxs->side,dim)) {
		bptr=boxs->blist[b];
		mlist=bptr->mol[0];
		for(m=0;m<bptr->nmol[0];m++) {
			pos=mlist[m]->pos;
			for(d=0;d<dim;d++) if(pos[d]<poslo[d] || pos[d]>poshi[d]) d=dim+1;
			if(d==dim) {
				pos=mlist[m]->posx;
				for(d=0;d<dim;d++) if(pos[d]<poslo[d] || pos[d]>poshi[d]) d=dim+1;
				if(d>dim) copyVD(mlist[m]->posx,mlist[m]->pos,dim); }}}
	return CMDok; }


enum CMDcode cmdexcludesphere(simptr sim,cmdptr cmd,char *line2) {
	int m,itct,dim,d,b,b1,b2;
	double *pos,poslo[DIMMAX],poshi[DIMMAX],poscent[DIMMAX],rad,dist;
	boxptr bptr1,bptr2,bptr;
	boxssptr boxs;
	moleculeptr *mlist;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDmanipulate;
	dim=sim->dim;
	boxs=sim->boxs;
	for(d=0;d<dim;d++) {
		SCMDCHECK(line2,"missing center argument");
		itct=sscanf(line2,"%lg",&poscent[d]);
		SCMDCHECK(itct==1,"failure reading center");
		line2=strnword(line2,2); }
	SCMDCHECK(line2,"missing radius");
	itct=sscanf(line2,"%lg",&rad);
	SCMDCHECK(itct==1,"failure reading radius");

	dist=rad*sqrt((double)dim);
	for(d=0;d<dim;d++) {
		poslo[d]=poscent[d]-dist;
		poshi[d]=poscent[d]+dist; }
	rad*=rad;
	bptr1=pos2box(sim,poslo);
	bptr2=pos2box(sim,poshi);
	b1=indx2addZV(bptr1->indx,boxs->side,dim);
	b2=indx2addZV(bptr2->indx,boxs->side,dim);
	for(b=b1;b<=b2;b=nextaddZV(b,bptr1->indx,bptr2->indx,boxs->side,dim)) {
		bptr=boxs->blist[b];
		mlist=bptr->mol[0];
		for(m=0;m<bptr->nmol[0];m++) {
			pos=mlist[m]->pos;
			for(dist=0,d=0;d<dim;d++) if((dist+=(pos[d]-poscent[d])*(pos[d]-poscent[d]))>rad) d=dim+1;
			if(d==dim) {
				pos=mlist[m]->posx;
				for(dist=0,d=0;d<dim;d++) if((dist+=(pos[d]-poscent[d])*(pos[d]-poscent[d]))>rad) d=dim+1;
				if(d>dim) copyVD(mlist[m]->posx,mlist[m]->pos,dim); }}}
	return CMDok; }


enum CMDcode cmdincludeecoli(simptr sim,cmdptr cmd,char *line2) {
	int m,ll,nmol;
	moleculeptr *mlist;
	double rad,length,pos[DIMMAX];
	wallptr *wlist;

	if(line2 && !strcmp(line2,"cmdtype")) return CMDmanipulate;
	SCMDCHECK(sim->dim==3,"system is not 3 dimensional");
	wlist=sim->wlist;
	rad=0.5*(wlist[3]->pos-wlist[2]->pos);
	length=wlist[1]->pos-wlist[0]->pos;
	pos[0]=wlist[0]->pos;
	pos[1]=0.5*(wlist[2]->pos+wlist[3]->pos);
	pos[2]=0.5*(wlist[4]->pos+wlist[5]->pos);
	for(ll=0;ll<sim->mols->nlist;ll++) {
		mlist=sim->mols->live[ll];
		nmol=sim->mols->nl[ll];
		for(m=0;m<nmol;m++)
			if(!insideecoli(mlist[m]->pos,pos,rad,length)) {
				if(insideecoli(mlist[m]->posx,pos,rad,length)) copyVD(mlist[m]->posx,mlist[m]->pos,3);
				else putinecoli(mlist[m]->pos,pos,rad,length); }}
	return CMDok; }



/**********************************************************/
/******************* internal routines ********************/
/**********************************************************/


void cmdv1free(cmdptr cmd) {
	free(cmd->v1);
	return; }


void cmdv1v2free(cmdptr cmd) {
	free(cmd->v1);
	free(cmd->v2);
	return; }


enum CMDcode conditionalcmdtype(simptr sim,cmdptr cmd,int nparam) {
	char string[STRCHAR],*strptr;
	enum CMDcode ans;

	if(!cmd->str) return CMDnone;
	strptr=strnword(cmd->str,nparam+2);
	if(!strptr) return CMDnone;
	strcpy(string,strptr);
	strptr=cmd->str;
	cmd->str=string;
	ans=scmdcmdtype(sim->cmds,cmd);
	cmd->str=strptr;
	return ans; }


int insideecoli(double *pos,double *ofst,double rad,double length) {
	double dist;

	dist=(pos[1]-ofst[1])*(pos[1]-ofst[1])+(pos[2]-ofst[2])*(pos[2]-ofst[2]);
	if(pos[0]-ofst[0]<rad) dist+=(pos[0]-ofst[0]-rad)*(pos[0]-ofst[0]-rad);
	else if(pos[0]-ofst[0]>length-rad) dist+=(pos[0]-ofst[0]-length+rad)*(pos[0]-ofst[0]-length+rad);
	return dist<rad*rad; }


void putinecoli(double *pos,double *ofst,double rad,double length) {
	double dist;

	dist=(pos[1]-ofst[1])*(pos[1]-ofst[1])+(pos[2]-ofst[2])*(pos[2]-ofst[2]);
	if(pos[0]-ofst[0]<rad) {
		dist+=(pos[0]-ofst[0]-rad)*(pos[0]-ofst[0]-rad);
		dist=sqrt(rad*rad/dist);
		pos[0]=ofst[0]+rad+dist*(pos[0]-ofst[0]-rad); }
	else if(pos[0]-ofst[0]>length-rad) {
		dist+=(pos[0]-ofst[0]-length+rad)*(pos[0]-ofst[0]-length+rad);
		dist=sqrt(rad*rad/dist);
		pos[0]=ofst[0]+length-rad+dist*(pos[0]-ofst[0]-length+rad); }
	else
		dist=sqrt(rad*rad/dist);
	pos[1]=ofst[1]+dist*(pos[1]-ofst[1]);
	pos[2]=ofst[2]+dist*(pos[2]-ofst[2]);
	return; }


int molinpanels(simptr sim,int ll,int m,int s,char pshape) {
	int p,ps,dim,npnl;
	double *pos;
	panelptr *pnls,pnl;

	if(s<0) {
		for(s=0;s<sim->srfss->nsrf;s++)
			if(molinpanels(sim,ll,m,s,pshape)) return 1;
		return 0; }

	dim=sim->dim;
	if(pshape=='s') ps=2;
	else return 0;
	pnls=sim->srfss->srflist[s]->panels[ps];
	npnl=sim->srfss->srflist[s]->npanel[ps];
	pos=sim->mols->live[ll][m]->pos;
	if(pshape=='s') {
		for(p=0;p<npnl;p++) {
			pnl=pnls[p];
			if(Geo_PtInSphere(pos,pnl->point[0],pnl->point[1][0],dim)) return 1; }}
	return 0; }


#include <VCELL/SimulationMessaging.h>

enum CMDcode cmdVCellPrintProgress(simptr sim, cmdptr cmd, char *line2) {
	if(line2 && !strcmp(line2,"cmdtype")) {
		return CMDobserve;
	}
	double progress = (sim->time - sim->tmin) / (sim->tmax - sim->tmin);
	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_PROGRESS, progress, sim->time));
	//fprintf(stdout, "[[[progress:%lg%%]]]",  progress * 100.0);
	if (SimulationMessaging::getInstVar()->isStopRequested()) {
		throw -1;
	}
	return CMDok;
}

#include <VCELL/DataSet.h>

#define SIM_FILE_EXT "sim"
#define LOG_FILE_EXT "log"
#define ZIP_FILE_EXT "zip"
#define ZIP_FILE_LIMIT 1E9

typedef int int32;
typedef unsigned int uint32;

int zip32(int filecnt, char* zipfile, ...);
#include <sys/stat.h>

void writeSim(simptr sim, cmdptr cmd, char *line2, char* simFileName, char* zipFileName) {
	static int firstrun = 1;
	static int N[3] = {1,1,1};
	static double extent[3];
	static double origin[3];
	static char **varNames;
	static FileHeader fileHeader;
	static int dimension = 3, varSize = 0, numVars = 0, numBlocks = 0;
	static DataBlock *dataBlock = NULL;
	static double *sol = NULL;
	static int volRegionSize = 0;
	static int solSize = 0;

	molssptr mols = sim->mols;

	if (firstrun) {
		if (line2 == NULL || strlen(line2) == 0) {
			throw "writeOutput : no dimension specified.";
		}
		dimension = sscanf(line2, "%d %d %d %d", &N[0], &N[1], &N[2], &volRegionSize);
		dimension --;
		if (dimension == 0) {
			char errMsg[256];
			sprintf(errMsg, "writeOutput : no dimension specified. %d %d %d", N[0], N[1], N[2]);
			throw errMsg;
		}
		if (dimension == 1) {
			volRegionSize = N[1];
		} else if (dimension == 2) {
			volRegionSize = N[2];
		}
		origin[0] = sim->wlist[0]->pos;
		extent[0] = sim->wlist[1]->pos - origin[0];
		if (dimension > 1) {
			origin[1] = sim->wlist[2]->pos;
			extent[1] = sim->wlist[3]->pos - origin[1];
			if (dimension > 2) {
				origin[2] = sim->wlist[4]->pos;
				extent[2] = sim->wlist[5]->pos - origin[2];
			}
		}
		int i;
		numVars = mols->nspecies - 1;
		varNames = (char**)malloc(numVars * 2 * sizeof(char*));
		for(i = 0; i < numVars; i ++) {
			varNames[i] = (char*)malloc(128 * sizeof(char));
			strcpy(varNames[i], mols->spname[i + 1]);
		}
		for(i = 0; i < numVars; i++) {
			varNames[i + numVars] = (char*)malloc(128 * sizeof(char));
			sprintf(varNames[i + numVars], "%s_totalCount\0", varNames[i]);

		}
		strcpy(fileHeader.magicString, MAGIC_STRING);
		strcpy(fileHeader.versionString, VERSION_STRING);
		varSize = N[0] * N[1] * N[2];
		numBlocks = numVars * 2;

		fileHeader.sizeX = N[0];
		fileHeader.sizeY = N[1];
		fileHeader.sizeZ = N[2];
		fileHeader.numBlocks = numBlocks;
		fileHeader.firstBlockOffset = sizeof(FileHeader);

		dataBlock = (DataBlock*)malloc(numBlocks * sizeof(DataBlock));
		solSize = numVars * (varSize + volRegionSize);
		sol = (double*)malloc(solSize * sizeof(double));

		firstrun = 0;
	}

	if (dimension == 0) {
		throw "dimension == 0";
	}
	FILE* simfp = fopen(simFileName, "wb");
	if (simfp == NULL){
		throw "Cannot open .sim file to write";
	}

	DataSet::writeHeader(simfp, &fileHeader);
	long ftell_pos = ftell(simfp);
	if (ftell_pos != fileHeader.firstBlockOffset){
		char errMsg[256];
		sprintf(errMsg, "DataSet::write() - file offset for first block is incorrect, ftell() says %ld, should be %d", ftell_pos, fileHeader.firstBlockOffset);
		throw errMsg;
	}

	//
	// write data blocks (describing data)
	//
	int blockIndex = 0;
	int dataOffset = fileHeader.firstBlockOffset + numBlocks * sizeof(DataBlock);
	int v;
	for (v = 0; v < numVars; v ++) {
		memset(dataBlock[blockIndex].varName, 0, DATABLOCK_STRING_SIZE * sizeof(char));
		strcpy(dataBlock[blockIndex].varName, varNames[v]);

		dataBlock[blockIndex].varType = VAR_VOLUME;
		dataBlock[blockIndex].size = varSize;
		dataBlock[blockIndex].dataOffset = dataOffset;
		DataSet::writeDataBlock(simfp, dataBlock + blockIndex);
		dataOffset += dataBlock[blockIndex].size * sizeof(double);
		blockIndex ++;
	}
	for (v = numVars; v < numVars * 2; v ++) {
		memset(dataBlock[blockIndex].varName, 0, DATABLOCK_STRING_SIZE * sizeof(char));
		strcpy(dataBlock[blockIndex].varName, varNames[v]);

		dataBlock[blockIndex].varType = VAR_VOLUME_REGION;
		dataBlock[blockIndex].size = volRegionSize;
		dataBlock[blockIndex].dataOffset = dataOffset;
		DataSet::writeDataBlock(simfp, dataBlock + blockIndex);
		dataOffset += dataBlock[blockIndex].size * sizeof(double);
		blockIndex ++;
	}

	memset(sol, 0, solSize * sizeof(double));

	// for variables
	int* totalCounts = new int[numVars];
	memset(totalCounts, 0, numVars * sizeof(int));
	int ll, m;
	moleculeptr mptr;
	for(ll=0;ll<mols->nlist;ll++) {
		for(m=0;m<mols->nl[ll];m++) {
			mptr=mols->live[ll][m];
			int varIndex = mptr->ident - 1;
			if (varIndex >= 0 ) {
				double* coord = mptr->pos;
				int i = 0, j = 0, k = 0;
				i = (int)((coord[0] - origin[0]) * (N[0] - 1)/extent[0] + 0.5);
				if (dimension > 1) {
					j = (int)((coord[1] - origin[1]) * (N[1] - 1)/extent[1] + 0.5);
					if (dimension > 2) {
						k = (int)((coord[2] - origin[2]) * (N[2] - 1)/extent[2] + 0.5);
					}
				}

				int volIndex = k * N[1] * N[0] + j * N[0] + i;
				sol[varIndex * varSize + volIndex] ++;

				totalCounts[varIndex] ++;
			}
		}
	}

	int totalCountOffSet = numVars * varSize;
	// for total count region variable
	for (v = 0; v < numVars; v ++) {
		for (int r = 0; r < volRegionSize; r ++) {
			sol[totalCountOffSet + v * volRegionSize + r] = totalCounts[v];
		}
	}
	delete[] totalCounts;

	//
	// write data
	//
	blockIndex = 0;
	for (v = 0; v < numVars; v ++) {
		ftell_pos = ftell(simfp);
		if (ftell_pos != dataBlock[blockIndex].dataOffset){
			char errMsg[256];
			sprintf(errMsg, "DataSet::write() - offset for data is "
				"incorrect (block %d, var=%s), ftell() says %ld, should be %d", blockIndex, dataBlock[blockIndex].varName,
				ftell_pos, dataBlock[blockIndex].dataOffset);
			throw errMsg;
		}
		DataSet::writeDoubles(simfp, sol + v * varSize, varSize);
		blockIndex ++;
	}
	for (v = 0; v < numVars; v ++) {
		ftell_pos = ftell(simfp);
		if (ftell_pos != dataBlock[blockIndex].dataOffset){
			char errMsg[256];
			sprintf(errMsg, "DataSet::write() - offset for data is "
				"incorrect (block %d, var=%s), ftell() says %ld, should be %d", blockIndex, dataBlock[blockIndex].varName,
				ftell_pos, dataBlock[blockIndex].dataOffset);
			throw errMsg;
		}
		DataSet::writeDoubles(simfp, sol + totalCountOffSet + v * volRegionSize, volRegionSize);
		blockIndex ++;
	}
	fclose(simfp);

	int retcode = zip32(1, zipFileName, simFileName);
	remove(simFileName);
	if (retcode != 0) {
		char errMsg[256];
		sprintf(errMsg, "Writing zip file <%s> failed, return code is %d", zipFileName, retcode);
		throw errMsg;
	}

	if (fabs(sim->tmax - sim->time) < 1e-12) {
		for (int i = 0; i < numVars; i ++) {
			free(varNames[i]);
		}
		free(varNames);
		free(dataBlock);
		free(sol);
	}
}

int getZipCount(char* zipFileName) {
	char* p = strstr(zipFileName, ZIP_FILE_EXT);
	if (p == NULL) {
		return -1;
	}

	char str[3];
	strncpy(str, p - 2, 2 * sizeof(char));
	str[2] = 0;
	return atoi(str);
}

void clearLog(char* baseFileName) {

	FILE *fp;
	char logFileName[256];

	sprintf(logFileName,"%s.%s",baseFileName, LOG_FILE_EXT);
	if ((fp=fopen(logFileName, "r"))==NULL){
		printf("error opening log file <%s>\n", logFileName);
		return;
	}

	char simFileName[128];
	char zipFileName[128];
	int iteration, oldCount=-1, count;
	double time;

	while (!feof(fp)) {
		fscanf(fp,"%4d %s %s %lg\n", &iteration, simFileName, zipFileName, &time);
		count = getZipCount(zipFileName);
		if (oldCount != count && count >= 0) {
			printf("clearLog(), removing zip file %s\n", zipFileName);
			remove(zipFileName);
			oldCount = count;
		}
	}
	fclose(fp);

	printf("clearLog(), removing log file %s\n", logFileName);
	remove(logFileName);
}

enum CMDcode cmdVCellWriteOutput(simptr sim, cmdptr cmd, char *line2) {
	static int simFileCount = 0;
	static int zipFileCount = 0;
	static char baseFileName[256];
	static char baseSimName[256];

	if(line2 && !strcmp(line2,"cmdtype")) {
		return CMDobserve;
	}

	if (simFileCount == 0) {
		char* rootdir = sim->cmds->root;
		char* fname = sim->cmds->fname[0];
		strcpy(baseFileName, rootdir);
		strcat(baseFileName, fname);
		char* p = strrchr(baseFileName, '.');
		*p = '\0';

		strcpy(baseSimName, fname);
		p = strrchr(baseSimName, '.');
		*p = '\0';

		clearLog(baseFileName);
	}

	// write sim file
	char simFileName[256];
	char zipFileName[256];
	sprintf(simFileName, "%s%.4d.%s", baseSimName, simFileCount, SIM_FILE_EXT);
	sprintf(zipFileName, "%s%.2d.%s", baseFileName, zipFileCount, ZIP_FILE_EXT);

	writeSim(sim, cmd, line2, simFileName, zipFileName);

	// write log file
	char logfilename[256];
	sprintf(logfilename, "%s.%s", baseFileName, LOG_FILE_EXT);
	FILE* logfp = NULL;
	if (simFileCount == 0) {
		logfp = fopen(logfilename, "w");
	} else {
		logfp = fopen(logfilename, "a");
	}
	if (logfp == NULL) {
		return CMDabort;
	}
	int iteration = (int)(sim->time/sim->dt + 0.5);
	fprintf(logfp,"%4d %s %s %.15lg\n", iteration, simFileName, zipFileName, sim->time);
//	fprintf(logfp,"%4d %s %.15lg\n", iteration, simfilename, sim->time);
	fclose(logfp);

	// print message
	//fprintf(stdout, "[[[data:%lg]]]",  sim->time);
	simFileCount ++;

	struct stat buf;
	if (stat(zipFileName, &buf) == 0) { // if exists
		if (buf.st_size > ZIP_FILE_LIMIT) {
			zipFileCount ++;
		}
	}
	double progress = (sim->time - sim->tmin) / (sim->tmax - sim->tmin);
	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_DATA, progress, sim->time));
	return CMDok;
}

