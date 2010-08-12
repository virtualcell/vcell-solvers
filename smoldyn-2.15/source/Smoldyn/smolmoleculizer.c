/* Steven Andrews.
 This is a library of functions for the Smoldyn program.  See documentation
 called Smoldyn_doc1.doc and Smoldyn_doc2.doc.
 Copyright 2003-2007 by Steven Andrews.  This work is distributed under the terms
 of the Gnu Lesser General Public License (LGPL). */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <assert.h>
#include <math.h>
#include "smoldyn.h"
#include "string2.h"
#include "uthash.h"

#define CHECK(A) if(!(A)) goto failure; else (void)0
#define CHECKS(A,B) if(!(A)) {strncpy(erstr,B,STRCHAR-1);erstr[STRCHAR-1]='\0';goto failure;} else (void)0

// The ./configure script for compiling smoldyn generates smoldyn_config.h,
// included in smoldyn.h.  It reports whether libmoleculizer is available
// through the LIBMOLECULIZER variable.

#ifdef LIBMOLECULIZER
	#include <libmoleculizer/mzr/libmzr_c_interface.h>
	int addMzrSpeciesArrayToSim(simptr sim,species **species_array,int number_species);
	int addMzrReactionArrayToSim(simptr sim,reaction **reactionlist,int number_reactions);
#endif


/*
New API additions

const char* getErrorMessage(moleculizer* handle);
int getErrorState( moleculizer* handle);
void setErrorMessage( moleculizer* handle, const char* error_message);
void clearErrorState( moleculizer* handle);
*/


/******************************************************************************/
/****************************** low level utilities ***************************/
/******************************************************************************/

/* mzrTagName2SmolName. */
int mzrTagName2SmolName(simptr sim,char *tagname) {
	mzrssptr mzrss;
	int ans,itag;

	mzrss=sim->mzrss;
	itag=stringfind(mzrss->tagname,mzrss->nnamehash,tagname);
	if(itag>=0) {
		ans=stringfind(sim->mols->spname,sim->mols->nspecies,mzrss->smolname[itag]);
		if(ans>=0) return ans;
		else return -2; }
	else {
		ans=stringfind(sim->mols->spname,sim->mols->nspecies,tagname);
		if(ans>=0) return ans;
		else return -1; }
	return -1; }


/* mzrSmolName2TagName */
int mzrSmolName2TagName(mzrssptr mzrss,char *smolname,char *tagname) {
#ifdef LIBMOLECULIZER
	return convertSomeNameToTaggedName(mzrss->mzr,smolname,tagname,STRCHAR);
#else
	return 4;
#endif
}


/* mzrReadStreamName. */
int mzrReadStreamName(char *str,char *streamname,enum MolecState *msptr) {
	char nm[STRCHAR],*pareno,*parenc;
	int itct,i;
	enum MolecState ms;

	if(!str) return -1;
	itct=sscanf(str,"%s",nm);
	if(itct!=1) return -1;			// cannot read name
	pareno=strchr(nm,'(');

	if(pareno) {
		*pareno='\0';
		pareno++;
		parenc=strrchr(pareno,')');
		if(parenc && *(parenc+1)=='\0') *parenc='\0';
		else return -2;						// improper close parenthesis
		ms=molstring2ms(pareno);
		if(ms==MSnone) return -3; }		// cannot read state
	else ms=MSsoln;

	if(!strcmp(nm,"all")) i=-5;		// all
	if(streamname) strncpy(streamname,nm,STRCHAR);
	if(msptr) *msptr=ms;
	return 0; }


/* mzrGetSpeciesStreams */
int mzrGetSpeciesStreams(mzrssptr mzrss,char ***streamnames,int *numnames) {
#ifdef LIBMOLECULIZER
	int ans;

	ans=0;
	getAllSpeciesStreams(mzrss->mzr,streamnames,numnames);
	if(*streamnames==NULL) {
		ans=1;
		*numnames=0; }
	return ans;
#else
	return 2;
#endif
}


/* mzrFreeSpeciesStreams */
void mzrFreeSpeciesStreams(char **streamnames,int numnames) {
#ifdef LIBMOLECULIZER
	freeCharPtrArray(streamnames,numnames);
	return;
#else
	return;
#endif
}


/* mzrIsTagNameInStream */
int mzrIsTagNameInStream(mzrssptr mzrss,char *tagname,char *stream) {
#ifdef LIBMOLECULIZER
	return checkSpeciesTagIsInSpeciesStream(mzrss->mzr,tagname,stream);
#else
	return -1;
#endif
}


/* mzrNumberOfSpecies. */
int mzrNumberOfSpecies(mzrssptr mzrss) {
#ifdef LIBMOLECULIZER
	if(!mzrss || !mzrss->mzr) return 0;
	return getNumberOfSpecies(mzrss->mzr);
#else
	return 0;
#endif
}


/* mzrNumberOfReactions. This function returns the number of reactions that 
 moleculizer has recorded thus far. */
int mzrNumberOfReactions(mzrssptr mzrss) {
#ifdef LIBMOLECULIZER
	if(!mzrss || !mzrss->mzr) return 0;
	return getNumberOfReactions(mzrss->mzr); 
#else
	return 0;
#endif
}


/******************************************************************************/
/****************************** memory management *****************************/
/******************************************************************************/


/* mzrallocrules */
int mzrallocrules(mzrssptr mzrss,int ruleschars) {
	char *newrules;
	int i;

	newrules=NULL;
	if(!mzrss || ruleschars<0) return 2;
	if(ruleschars>0) {
		CHECK(newrules=(char*) calloc(ruleschars,sizeof(char)));
		for(i=0;i<ruleschars;i++) newrules[i]='\0';
		if(mzrss->rules)
			strncpy(newrules,mzrss->rules,ruleschars); }
	free(mzrss->rules);
	mzrss->ruleschars=ruleschars;
	mzrss->rules=newrules;
	return 0;
failure:
	free(newrules);
	return 1; }


/* mzrallocstreams */
int mzrallocstreams(mzrssptr mzrss,int maxstreams) {
	char **newstreamname;
	double **newdisplaysize,***newcolor,**newstrmdifc;
	int strm,c;
	enum MolecState ms;

	newstreamname=NULL;
	newdisplaysize=NULL;
	newcolor=NULL;
	newstrmdifc=NULL;

	if(!mzrss || maxstreams<0) return 2;

	if(maxstreams>0) {
		CHECK(newstreamname=(char**) calloc(maxstreams,sizeof(char*)));			// allocate streamname
		for(strm=0;strm<maxstreams;strm++) newstreamname[strm]=NULL;
		for(strm=0;strm<maxstreams;strm++) {
			CHECK(newstreamname[strm]=EmptyString()); }

		CHECK(newdisplaysize=(double**) calloc(maxstreams,sizeof(double*)));	// allocate displaysize
		for(strm=0;strm<maxstreams;strm++) newdisplaysize[strm]=NULL;
		for(strm=0;strm<maxstreams;strm++) {
			CHECK(newdisplaysize[strm]=(double*) calloc(MSMAX,sizeof(double)));
			for(ms=0;ms<MSMAX;ms++) newdisplaysize[strm][ms]=1.0; }

		CHECK(newcolor=(double***) calloc(maxstreams,sizeof(double**)));		// allocate color
		for(strm=0;strm<maxstreams;strm++) newcolor[strm]=NULL;
		for(strm=0;strm<maxstreams;strm++) {
			CHECK(newcolor[strm]=(double**) calloc(MSMAX,sizeof(double*)));
			for(ms=0;ms<MSMAX;ms++) newcolor[strm][ms]=NULL;
			for(ms=0;ms<MSMAX;ms++) {
				CHECK(newcolor[strm][ms]=(double*) calloc(3,sizeof(double)));
				for(c=0;c<3;c++) newcolor[strm][ms][c]=0; }}

		CHECK(newstrmdifc=(double**) calloc(maxstreams,sizeof(double*)));		// allocate strmdifc
		for(strm=0;strm<maxstreams;strm++) newstrmdifc[strm]=NULL;
		for(strm=0;strm<maxstreams;strm++) {
			CHECK(newstrmdifc[strm]=(double*) calloc(MSMAX,sizeof(double)));
			for(ms=0;ms<MSMAX;ms++) newstrmdifc[strm][ms]=-1.0; }

		for(strm=0;strm<mzrss->nstreams && strm<maxstreams;strm++) {				// copy stuff over
			strcpy(newstreamname[strm],mzrss->streamname[strm]);
			for(ms=0;ms<MSMAX;ms++) {
				newdisplaysize[strm][ms]=mzrss->displaysize[strm][ms];
				for(c=0;c<3;c++)
					newcolor[strm][ms][c]=mzrss->color[strm][ms][c];
				newstrmdifc[strm][ms]=mzrss->strmdifc[strm][ms]; }}}

	mzrfreestreams(mzrss->streamname,mzrss->displaysize,mzrss->color,mzrss->strmdifc,mzrss->maxstreams);
	mzrss->maxstreams=maxstreams;
	if(mzrss->nstreams>maxstreams) mzrss->nstreams=maxstreams;
	mzrss->streamname=newstreamname;
	mzrss->displaysize=newdisplaysize;
	mzrss->color=newcolor;
	mzrss->strmdifc=newstrmdifc;
	
	return 0;

 failure:
	mzrfreestreams(newstreamname,newdisplaysize,newcolor,newstrmdifc,maxstreams);
	return 1; }


/* mzrfreestreams */
void mzrfreestreams(char **streamname,double **displaysize,double ***color,double **strmdifc,int maxstreams) {
	int strm;
	enum MolecState ms;

	if(strmdifc) {
		for(strm=0;strm<maxstreams;strm++)
			free(strmdifc[strm]);
		free(strmdifc); }

	if(color) {
		for(strm=0;strm<maxstreams;strm++)
			if(color[strm]) {
				for(ms=0;ms<MSMAX;ms++)
					free(color[strm][ms]);
				free(color[strm]); }
		free(color); }

	if(displaysize) {
		for(strm=0;strm<maxstreams;strm++)
			free(displaysize[strm]);
		free(displaysize); }

	if(streamname) {
		for(strm=0;strm<maxstreams;strm++)
			free(streamname[strm]);
		free(streamname); }

	return; }


/* mzrallocnamehash. */
int mzrallocnamehash(mzrssptr mzrss,int maxnamehash) {
	char **newtagname,**newsmolname;
	int i;
	
	newtagname=NULL;
	newsmolname=NULL;
	
	if(!mzrss || maxnamehash<0) return 2;
	
	if(maxnamehash>0) {
		CHECK(newtagname=(char**) calloc(maxnamehash,sizeof(char*)));
		for(i=0;i<maxnamehash;i++) newtagname[i]=NULL;
		for(i=0;i<maxnamehash;i++) {
			CHECK(newtagname[i]=EmptyString()); }
		
		CHECK(newsmolname=(char**) calloc(maxnamehash,sizeof(char*)));
		for(i=0;i<maxnamehash;i++) newsmolname[i]=NULL;
		for(i=0;i<maxnamehash;i++) {
			CHECK(newsmolname[i]=EmptyString()); }
		
		for(i=0;i<mzrss->nnamehash && i<maxnamehash;i++) {
			strcpy(newtagname[i],mzrss->tagname[i]);
			strcpy(newsmolname[i],mzrss->smolname[i]); }}
	
	mzrfreenamehash(mzrss->tagname,mzrss->smolname,mzrss->maxnamehash);
	mzrss->maxnamehash=maxnamehash;
	if(mzrss->nnamehash>maxnamehash) mzrss->nnamehash=maxnamehash;
	mzrss->tagname=newtagname;
	mzrss->smolname=newsmolname;
	
	return 0;
	
failure:
	mzrfreenamehash(newtagname,newsmolname,maxnamehash);
	return 1; }


/* mzrfreenamehash */
void mzrfreenamehash(char **tagname,char **smolname,int maxnamehash) {
	int i;
	
	if(smolname) {
		for(i=0;i<maxnamehash;i++)
			free(smolname[i]);
		free(smolname); }
	
	if(tagname) {
		for(i=0;i<maxnamehash;i++)
			free(tagname[i]);
		free(tagname); }
	
	return; }


/* mzrallocrxnhash */
int mzrallocrxnhash(mzrssptr mzrss,int maxrxnhash) {
	char **newmzrrxn,**newsmolrxn;
	int i;

	newmzrrxn=NULL;
	newsmolrxn=NULL;

	if(!mzrss || maxrxnhash<0) return 2;

	if(maxrxnhash>0) {
		CHECK(newmzrrxn=(char**) calloc(maxrxnhash,sizeof(char*)));
		for(i=0;i<maxrxnhash;i++) newmzrrxn[i]=NULL;
		for(i=0;i<maxrxnhash;i++) {
			CHECK(newmzrrxn[i]=EmptyString()); }

		CHECK(newsmolrxn=(char**) calloc(maxrxnhash,sizeof(char*)));
		for(i=0;i<maxrxnhash;i++) newsmolrxn[i]=NULL;
		for(i=0;i<maxrxnhash;i++) {
			CHECK(newsmolrxn[i]=EmptyString()); }

		for(i=0;i<mzrss->nrxnhash && i<maxrxnhash;i++) {
			strcpy(newmzrrxn[i],mzrss->mzrrxn[i]);
			strcpy(newsmolrxn[i],mzrss->smolrxn[i]); }}

	mzrfreerxnhash(mzrss->mzrrxn,mzrss->smolrxn,mzrss->maxrxnhash);
	mzrss->maxrxnhash=maxrxnhash;
	if(mzrss->nrxnhash>maxrxnhash) mzrss->nrxnhash=maxrxnhash;
	mzrss->mzrrxn=newmzrrxn;
	mzrss->smolrxn=newsmolrxn;

	return 0;

 failure:
	mzrfreerxnhash(newmzrrxn,newsmolrxn,maxrxnhash);
	return 1; }


/* mzrfreerxnhash */
void mzrfreerxnhash(char **mzrrxn,char **smolrxn,int maxrxnhash) {
	int i;

	if(smolrxn) {
		for(i=0;i<maxrxnhash;i++)
			free(smolrxn[i]);
		free(smolrxn); }

	if(mzrrxn) {
		for(i=0;i<maxrxnhash;i++)
			free(mzrrxn[i]);
		free(mzrrxn); }

	return; }


/* mzrallocdefaultstate */
int mzrallocdefaultstate(mzrssptr mzrss,int maxspecies) {
	enum MolecState *newdefaultstate;
	int i;

	newdefaultstate=NULL;
	if(!mzrss || maxspecies<0) return 2;
	if(maxspecies>0) {
		CHECK(newdefaultstate=(enum MolecState*) calloc(maxspecies,sizeof(enum MolecState)));
		for(i=0;i<maxspecies;i++) newdefaultstate[i]=MSnone;
		for(i=0;i<mzrss->maxspecies && i<maxspecies;i++)
			newdefaultstate[i]=mzrss->defaultstate[i]; }
	free(mzrss->defaultstate);
	mzrss->maxspecies=maxspecies;
	mzrss->defaultstate=newdefaultstate;
	return 0;
failure:
	free(newdefaultstate);
	return 1; }


/* mzrssalloc */
mzrssptr mzrssalloc(void) {
	mzrssptr mzrss;
	enum MolecState ms;
	
	mzrss=(mzrssptr) malloc(sizeof(struct mzrsuperstruct));
	if(!mzrss) return NULL;
	
	mzrss->condition=SCinit;
	mzrss->sim=NULL;
	mzrss->mzr=NULL;
	mzrss->ruleschars=0;
	mzrss->rules=NULL;
	mzrss->maxstreams=0;
	mzrss->nstreams=0;
	mzrss->streamname=NULL;
	mzrss->displaysize=NULL;
	mzrss->color=NULL;
	mzrss->strmdifc=NULL;
	mzrss->maxNetworkSpecies=-1;
	mzrss->maxnamehash=0;
	mzrss->nnamehash=0;
	mzrss->tagname=NULL;
	mzrss->smolname=NULL;
	mzrss->maxrxnhash=0;
	mzrss->nrxnhash=0;
	mzrss->mzrrxn=NULL;
	mzrss->smolrxn=NULL;
	mzrss->maxspecies=0;
	mzrss->defaultstate=NULL;
	mzrss->refspecies=0;
	mzrss->refmass=0;
	for(ms=0;ms<MSMAX;ms++) mzrss->refdifc[ms]=0;
	return mzrss; }


/* mzrssfree. */
void mzrssfree(mzrssptr mzrss) {
	if(!mzrss) return;
	
#ifdef LIBMOLECULIZER
	if(mzrss->mzr) freeMoleculizerObject(mzrss->mzr);
#endif

	free(mzrss->defaultstate);
	mzrfreerxnhash(mzrss->mzrrxn,mzrss->smolrxn,mzrss->maxrxnhash);
	mzrfreenamehash(mzrss->tagname,mzrss->smolname,mzrss->maxnamehash);
	mzrfreestreams(mzrss->streamname,mzrss->displaysize,mzrss->color,mzrss->strmdifc,mzrss->maxstreams);
	free(mzrss->rules);
	free(mzrss);
	return; }


/******************************************************************************/
/*************************** data structure output ****************************/
/******************************************************************************/


/* mzrCheckParams. */
int mzrCheckParams(simptr sim,int *warnptr) {
	int warn,error,er,numnames,strm;
	mzrssptr mzrss;
	char **streamnames;

	error=warn=0;
	mzrss=sim->mzrss;

	if(!mzrss) {
		if(warnptr) *warnptr=warn;
		return 0; }

	if(mzrss->condition!=SCok) {warn++;printf("WARNING: moleculizer not fully set up\n");}
	if(!mzrss->sim) {error++;printf("BUG: moleculizer sim element undefined\n");}
	if(!mzrss->mzr) {warn++;printf("WARNING: moleculizer rules not fully loaded\n");}
	if(mzrss->nstreams>mzrss->maxstreams) {error++;printf("BUG: moleculizer has more streams defined than allocated\n");}
	if(mzrss->maxNetworkSpecies>=0) {warn++;printf("WARNING: moleculizer network expansion is limited to %i species\n",mzrss->maxNetworkSpecies);}
	if(mzrss->nnamehash>mzrss->maxnamehash) {error++;printf("BUG: moleculizer has more hash names defined than allocated\n");}
	if(!sim->mols) {error++;printf("BUG: moleculizer defined but not Smoldyn molecules\n");}
	else {
		if(mzrss->maxspecies>0 && mzrss->maxspecies!=sim->mols->maxspecies) {
			error++;printf("BUG: moleculizer maxspecies does not match mols version\n");} }

	if(mzrss->mzr) {
		er=mzrGetSpeciesStreams(mzrss,&streamnames,&numnames);
		if(er) {error++;printf("ERROR: failed to allocate memory while looking up species streams\n");}
		else {
			for(strm=0;strm<mzrss->nstreams;strm++)
				if(stringfind(streamnames,numnames,mzrss->streamname[strm])<0) {
					warn++;printf("WARNING: stream %s was mentioned, but not declared in rules file\n",mzrss->streamname[strm]);}
			mzrFreeSpeciesStreams(streamnames,numnames); }}
	
	if(warnptr) *warnptr=warn;
	return error; }


/* mzroutput. */
void mzroutput(mzrssptr mzrss) {
#ifdef LIBMOLECULIZER
	int i,er,i2,nspec,nstrm,strm;
	char **streamarray;
	species **speciesarray;

	if(!mzrss || !mzrss->mzr) return;
	printf("  Modifications: %i\n",getNumModificationDefs(mzrss->mzr));
	printf("  Mols: %i\n",getNumMolDefs(mzrss->mzr));
	printf("  Reaction rules: %i\n",getNumReactionRules(mzrss->mzr));
	printf("  Association reactions: %i\n",getNumDimerDecompReactionRules(mzrss->mzr));
	printf("  Transformation reactions: %i\n",getNumOmniGenReactionRules(mzrss->mzr));
	printf("  Uni-mol-gen reactions: %i\n",getNumUniMolGenReactionRules(mzrss->mzr));

	er=getAllSpecies(mzrss->mzr,&speciesarray,&nspec);
	CHECK(!er);
	getAllSpeciesStreams(mzrss->mzr,&streamarray,&nstrm);

	printf("  Number of species: %i\n",nspec);
	for(i=0;i<nspec;i++) {
		i2=stringfind(mzrss->tagname,mzrss->nnamehash,speciesarray[i]->name);
		CHECK(i2>=0);
		printf("   %s, mass=%g\n",mzrss->smolname[i2],*speciesarray[i]->mass); }

	printf("  Species classes: %i\n",nstrm);
	for(strm=0;strm<nstrm;strm++) {
		printf("   %s\n",streamarray[strm]);
		for(i=0;i<nspec;i++) {
			i2=checkSpeciesTagIsInSpeciesStream(mzrss->mzr,speciesarray[i]->name,streamarray[strm]);
			CHECK(i2>=0);
			if(i2) {
				i2=stringfind(mzrss->tagname,mzrss->nnamehash,speciesarray[i]->name);
				CHECK(i2>=0);
				printf("    %s\n",mzrss->smolname[i2]); }}}

	printf("  Number of generated reactions: %i\n",getNumberOfReactions(mzrss->mzr));

	if(streamarray) freeCharPtrArray(streamarray,nstrm);
	if(speciesarray) freeSpeciesArray(speciesarray,nspec);
	return;
failure:
	printf("BUG: in mzroutput\n");
	return;
#else
	return;
#endif
}


/* mzrssoutput. */
void mzrssoutput(simptr sim) {
	mzrssptr mzrss;
	int strm,same,i,vflag;
	enum MolecState ms;
	char string[STRCHAR];
	double *displaysize,**color,*strmdifc;

	mzrss=sim->mzrss;
	if(!mzrss) return;
	vflag=strchr(sim->flags,'v')?1:0;

	printf("NETWORK GENERATION PARAMETERS\n");
	if(mzrss->mzr) {
		printf(" libmoleculizer parameters:\n");
		mzroutput(sim->mzrss); }
	else printf(" moleculizer object has not been created yet\n");
	if(mzrss->rules)
		printf(" rules file size: %i characters, %i lines\n",(int)strlen(mzrss->rules),symbolcount(mzrss->rules,'\n'));
	else
		printf(" no rules file\n");

	printf(" %i classes used, of %i allocated:\n",mzrss->nstreams,mzrss->maxstreams);
	for(strm=0;strm<mzrss->nstreams;strm++) {							// species stream display size, color, difc
		displaysize=mzrss->displaysize[strm];
		color=mzrss->color[strm];
		same=1;
		for(ms=0;ms<MSMAX && same==1;ms++) {
			if(displaysize[ms]!=displaysize[MSsoln]) same=0;
			if(color[ms][0]!=color[MSsoln][0]) same=0;
			if(color[ms][1]!=color[MSsoln][1]) same=0;
			if(color[ms][2]!=color[MSsoln][2]) same=0; }
		if(same)
			printf("  %s(all): size %g, color (%g,%g,%g)\n",mzrss->streamname[strm],displaysize[MSsoln],color[MSsoln][0],color[MSsoln][1],color[MSsoln][2]);
		else {
			for(ms=0;ms<MSMAX;ms++)
				printf("  %s(%s): size %g, color (%g,%g,%g)\n",mzrss->streamname[strm],molms2string(ms,string),displaysize[ms],color[MSsoln][0],color[MSsoln][1],color[MSsoln][2]); }
		strmdifc=mzrss->strmdifc[strm];
		same=1;
		for(ms=0;ms<MSMAX && same==1;ms++)
			if(strmdifc[ms]!=strmdifc[MSsoln]) same=0;
		if(same)
			printf("  %s(all): difc %g\n",mzrss->streamname[strm],strmdifc[MSsoln]);
		else {
			for(ms=0;ms<MSMAX;ms++)
				printf("  %s(%s): difc %g\n",mzrss->streamname[strm],molms2string(ms,string),strmdifc[ms]); }}

	printf(" %i species in hash of %i allocated\n",mzrss->nnamehash,mzrss->maxnamehash);
	if(vflag) {
		for(i=0;i<mzrss->nnamehash;i++) {
			if(i==10 && mzrss->nnamehash>11) {
				printf("  ...\n");
				i=mzrss->nnamehash-1; }
			printf("  %s ~ %s\n",mzrss->tagname[i],mzrss->smolname[i]); }}
	
	printf(" %i reactions in hash of %i allocated\n",mzrss->nrxnhash,mzrss->maxrxnhash);
	if(vflag) {
		for(i=0;i<mzrss->nrxnhash;i++) {
			if(i==10 && mzrss->nrxnhash>11) {
				printf("  ...\n");
				i=mzrss->nrxnhash-1; }
			printf("  %s ~ %s\n",mzrss->mzrrxn[i],mzrss->smolrxn[i]); }}
	
	printf(" default states:\n");
	if(!mzrss->defaultstate)
		printf("  none declared\n");
	else {
		for(i=0;i<sim->mols->nspecies;i++) {
			if(i==10 && sim->mols->nspecies>11) {
				printf("  ...\n");
				i=sim->mols->nspecies-1; }
			printf("  %s: %s\n",sim->mols->spname[i],molms2string(mzrss->defaultstate[i],string)); }}

	if(mzrss->refspecies>0) {
		printf(" diff. coeff. reference: %s has mass %g\n  and diff. coeffs.",sim->mols->spname[mzrss->refspecies],mzrss->refmass);
		for(ms=0;ms<MSMAX;ms++)
			printf(" %g",mzrss->refdifc[ms]);
		printf("\n"); }

	printf("\n");
	return; }


/* mzrsswrite */
void mzrsswrite(simptr sim,FILE *fptr) {	// ?? This function needs lots of work
	mzrssptr mzrss;

	mzrss=sim->mzrss;
	if(!mzrss) return;
	fprintf(fptr,"# Moleculizer parameters\n");
	fprintf(fptr,"start_rules\n");
	if(mzrss->rules) fprintf(fptr,"%s\n",mzrss->rules);
	fprintf(fptr,"end_rules\n");
	fprintf(fptr,"\n");
	return; }


/******************************************************************************/
/********************************* structure set up ***************************/
/******************************************************************************/


/* mzrsetcondition. */
void mzrsetcondition(mzrssptr mzrss,enum StructCond cond,int upgrade) {
	if(!mzrss) return;
	if(upgrade==0 && mzrss->condition>cond) mzrss->condition=cond;
	else if(upgrade==1 && mzrss->condition<cond) mzrss->condition=cond;
	else if(upgrade==2) mzrss->condition=cond;
	if(mzrss->condition<mzrss->sim->condition) {
		cond=mzrss->condition;
		simsetcondition(mzrss->sim,cond==SCinit?SClists:cond,0); }
	return; }



/* mzrAssignDrawingParameters */
int mzrAssignDrawingParameters(simptr sim,char *tagname,int ident) {
#ifdef LIBMOLECULIZER
	int strm,got;
	enum MolecState ms;
	mzrssptr mzrss;

	if(!sim->mzrss || !sim->mzrss->mzr) return 2;
	mzrss=sim->mzrss;
	got=0;
	for(strm=0;strm<mzrss->nstreams && !got;strm++)
		if(checkSpeciesTagIsInSpeciesStream(mzrss->mzr,tagname,mzrss->streamname[strm])) {
			for(ms=0;ms<MSMAX;ms++) {
				molsetcolor(sim,ident,ms,mzrss->color[strm][ms]);									// call to Smoldyn
				molsetdisplaysize(sim,ident,ms,mzrss->displaysize[strm][ms]); }		// call to Smoldyn
			got=1; }
	
	return !got;
#else
	return 2;
#endif
}


int mzrAssignDiffCoeff(simptr sim,char *tagname,double mass,int ident) {
	mzrssptr mzrss;
	enum MolecState ms;
	int got,strm;

	if(!sim->mzrss || !sim->mols) return 2;
	mzrss=sim->mzrss;
	got=0;
	strm=0;
#ifdef LIBMOLECULIZER
	for(strm=0;strm<mzrss->nstreams && !got;strm++)
		if(checkSpeciesTagIsInSpeciesStream(mzrss->mzr,tagname,mzrss->streamname[strm]))
			got=1;
	strm--;
#endif

	for(ms=0;ms<MSMAX;ms++) {
		if(got && mzrss->strmdifc[strm][ms]>=0)
			molsetdifc(sim,ident,ms,mzrss->strmdifc[strm][ms]);									// call to Smoldyn
		else if(mzrss->refmass>0 && mass>0)
			molsetdifc(sim,ident,ms,mzrss->refdifc[ms]*pow(mzrss->refmass/mass,1.0/3.0));		// call to Smoldyn
		else
			molsetdifc(sim,ident,ms,0); }
	return 0; }


/* mzrSetDefaultState */
int mzrSetDefaultState(simptr sim,int ident,enum MolecState ms) {
	mzrssptr mzrss;
	int er;

	if(!sim->mzrss || !sim->mols) return 2;
	mzrss=sim->mzrss;
	if(!mzrss->defaultstate || mzrss->maxspecies!=sim->mols->maxspecies) {
		er=mzrallocdefaultstate(mzrss,sim->mols->maxspecies);
		if(er) return 1; }
	mzrss->defaultstate[ident]=ms;
	return 0; }


/* mzrAssignProductDefaultState */
int mzrAssignProductDefaultState(mzrssptr mzrss,int *reactants,int nreactants,int *products,int nproducts) {
	int i;
	enum MolecState *defaultstate,ms1,ms2;

	if(!mzrss->defaultstate) return 0;
	defaultstate=mzrss->defaultstate;

	if(nreactants==0) {
		ms1=MSsoln;
		for(i=0;i<nproducts;i++)
			if(defaultstate[products[i]]==MSnone)
				defaultstate[products[i]]=ms1; }
	
	else if(nreactants==1) {
		ms1=defaultstate[reactants[0]];
		for(i=0;i<nproducts;i++)
			if(defaultstate[products[i]]==MSnone)
				defaultstate[products[i]]=ms1; }

	else if(nreactants==2) {
		ms1=defaultstate[reactants[0]];
		ms2=defaultstate[reactants[1]];
		for(i=0;i<nproducts;i++)
			if(defaultstate[products[i]]==MSnone) {
				if(ms1==MSnone)
					defaultstate[products[i]]=ms2;
				else if(ms2==MSnone)
					defaultstate[products[i]]=ms1;
				else if(ms1==MSsoln)
					defaultstate[products[i]]=ms2;
				else if(ms2==MSsoln)
					defaultstate[products[i]]=ms1;
				else
					defaultstate[products[i]]=ms1; }}

	else return 2;

	return 0; }


/* mzrSetStreamDisplay */
int mzrSetStreamDisplay(mzrssptr mzrss,char *streamname,enum MolecState ms,double displaysize,double *color) {
	int strm,er,c;
	enum MolecState mslo,mshi,ms1;

	strm=stringfind(mzrss->streamname,mzrss->nstreams,streamname);
	if(strm<0) {
		if(mzrss->nstreams==mzrss->maxstreams) {
			er=mzrallocstreams(mzrss,mzrss->maxstreams*2+1);
			if(er) return 1; }
		strm=mzrss->nstreams++;
		strncpy(mzrss->streamname[strm],streamname,STRCHAR); }

	if(ms==MSall) {mslo=0;mshi=MSMAX;}
	else if(ms==MSnone) return 0;
	else {mslo=ms;mshi=ms+1;}
	for(ms1=mslo;ms1<mshi;ms1++) {
		if(displaysize>=0) mzrss->displaysize[strm][ms1]=displaysize;
		if(color)
			for(c=0;c<3;c++) mzrss->color[strm][ms1][c]=color[c]; }

return 0; }


/* mzrSetStreamDifc */
int mzrSetStreamDifc(mzrssptr mzrss,char *streamname,enum MolecState ms,double difc) {
	int strm,er;
	enum MolecState mslo,mshi,ms1;

	strm=stringfind(mzrss->streamname,mzrss->nstreams,streamname);
	if(strm<0) {
		if(mzrss->nstreams==mzrss->maxstreams) {
			er=mzrallocstreams(mzrss,mzrss->maxstreams*2+1);
			if(er) return 1; }
		strm=mzrss->nstreams++;
		strncpy(mzrss->streamname[strm],streamname,STRCHAR); }

	if(ms==MSall) {mslo=0;mshi=MSMAX;}
	else if(ms==MSnone) return 0;
	else {mslo=ms;mshi=ms+1;}
	for(ms1=mslo;ms1<mshi;ms1++)
		mzrss->strmdifc[strm][ms1]=difc;

return 0; }


/* mzraddtonamehash */
int mzraddtonamehash(mzrssptr mzrss,char *tagname,char *smolname) {
	int i,er;
	
	if(mzrss->nnamehash==mzrss->maxnamehash) {
		er=mzrallocnamehash(mzrss,mzrss->maxnamehash*2+1);
		if(er) return -1; }
	i=mzrss->nnamehash++;
	strncpy(mzrss->tagname[i],tagname,STRCHAR);
	strncpy(mzrss->smolname[i],smolname,STRCHAR);
	return i; }


/* mzraddtorxnhash */
int mzraddtorxnhash(mzrssptr mzrss,char *mzrrxn,char *smolrxn) {
	int i,er;

	if(mzrss->nrxnhash==mzrss->maxrxnhash) {
		er=mzrallocrxnhash(mzrss,mzrss->maxrxnhash*2+1);
		if(er) return -1; }
	i=mzrss->nrxnhash++;
	strncpy(mzrss->mzrrxn[i],mzrrxn,STRCHAR);
	strncpy(mzrss->smolrxn[i],smolrxn,STRCHAR);
	return i; }


/* mzrNextSmolrxnName */
void mzrNextSmolrxnName(mzrssptr mzrss,char *smolrxn) {
	static int num=0;

	sprintf(smolrxn,"mzr%i",++num);
	return; }


/* mzrMakeNameHash. */
int mzrMakeNameHash(simptr sim) {
#ifdef LIBMOLECULIZER
	mzrssptr mzrss;
	char **names;
	unsigned int numNames;
	char taggedName[STRCHAR];
	int i,i2,er;

	if(!sim->mzrss || !sim->mzrss->mzr || !sim->mols) return 3;
	mzrss=sim->mzrss;
	names=NULL;
	numNames=0;
	er=getExplicitSpeciesList(mzrss->mzr,&names,&numNames);
	if(er) return 1;
	if(mzrss->maxnamehash<numNames) {
		er=mzrallocnamehash(mzrss,numNames);
		if(er) return 1; }
	mzrss->nnamehash=0;

	for(i=0;i<numNames;i++) {
		er=convertUserNameToTaggedName(mzrss->mzr,names[i],taggedName,STRCHAR);
		if(er) return 2;
		i2=stringfind(mzrss->tagname,mzrss->nnamehash,taggedName);
		if(i2>=0) return -1-i2;
		mzraddtonamehash(mzrss,taggedName,names[i]);

		i2=stringfind(sim->mols->spname,sim->mols->nspecies,names[i]);
		if(i2>=0) molsetexpansionflag(sim,i2,1); }
	freeCharPtrArray(names,numNames);
	return 0;
#else
	return 4;
#endif
}


/* mzrssreadrules */
int mzrssreadrules(simptr sim,ParseFilePtr *pfpptr,char *erstr) {
	ParseFilePtr pfp;
	char word[STRCHAR],*line2,*chptr;
	int done,pfpcode,totallength;
	mzrssptr mzrss;

	pfp=*pfpptr;
	if(!sim->mzrss) {
#ifndef LIBMOLECULIZER
		CHECKS(0,"This Smoldyn compile does not include the libmoleculizer module");
#endif
		CHECKS(sim->mzrss=mzrssalloc(),"out of memory allocating moleculizer superstructure");
		sim->mzrss->sim=sim; }
	mzrss=sim->mzrss;
	if(!mzrss->rules) {
		CHECKS(!mzrallocrules(mzrss,256),"out of memory reading rules"); }

	done=0;
	totallength=strlen(mzrss->rules);
	while(!done) {
		if(pfp->lctr==0 && !strchr(sim->flags,'q'))
			printf(" Reading file: '%s'\n",pfp->fname);
		pfpcode=Parse_ReadLine(&pfp,word,&line2,erstr);
		*pfpptr=pfp;
		CHECKS(pfpcode!=3,erstr);

		if(pfpcode==0);																// already taken care of
		else if(pfpcode==2) {													// end reading
			done=1; }
		else if(pfpcode==3) {													// error
			CHECKS(0,"SMOLDYN BUG: parsing error"); }
		else if(!strcmp(word,"end_rules")) {					// end_rules
			CHECKS(!line2,"unexpected text following end_rules");
			return 0; }
		else {																				// a line of the rules block
			if(word[0]=='=' && (chptr=strstr(pfp->line,"Mols"))) {	// text substitute "Mols" for "Molecules" in header
				strncpy(chptr+9,chptr+4,STRCHAR-(chptr-pfp->line)-9);
				strncpy(chptr+3,"ecules",STRCHAR-(chptr-pfp->line)-3);
				pfp->line[STRCHAR-1]='\0'; }

			totallength+=strlen(pfp->line)+1;
			if(totallength>mzrss->ruleschars) {
				CHECKS(!mzrallocrules(mzrss,2*totallength),"out of memory while reading rules file"); }
			strcat(mzrss->rules,pfp->line);
			strcat(mzrss->rules,"\n"); }}
	
	CHECKS(0,"end of file encountered before end_rules");	// end of file
	
failure:																					// failure
	return 1; }


/* mzrssload.  */
int mzrssload(simptr sim,char *erstr) {
#ifdef LIBMOLECULIZER
	int er;
	mzrssptr mzrss;
	const char *errorstring;

	mzrss=sim->mzrss;
	if(mzrss->mzr) freeMoleculizerObject(mzrss->mzr);
	mzrss->mzr=NULL;
	CHECKS(mzrss->mzr=createNewMoleculizerObject(),"out of memory in createNewMoleculizerObject");

	er=loadCommonRulesString(mzrss->mzr,mzrss->rules);
	CHECKS(er!=1,"unknown error reading network generation rules");
	CHECKS(er!=2,"document unparsable error reading network generation rules");
	CHECKS(er!=3,"rules already loaded error reading network generation rules");
	CHECKS(er!=4,"file not found error reading network generation rules");
//	er=getErrorState(mzrss->mzr);	// ?? BUG: This function returns 1 even if there isn't an error
	if(er) {
		errorstring=getErrorMessage(mzrss->mzr);
		if(errorstring) {CHECKS(0,errorstring);}
		else CHECKS(0,"unknown libmoleculizer error"); }

	return 0;
failure:
	clearErrorState(mzrss->mzr);
	freeMoleculizerObject(mzrss->mzr);
	mzrss->mzr=NULL;
	return 1;
#else
	return 0;
#endif
}


/* mzrSetValue */
void mzrSetValue(mzrssptr mzrss,char *item,int i1) {
	if(!mzrss) return;
	if(!strcmp(item,"maxNetworkSpecies"))
		mzrss->maxNetworkSpecies=i1;
	if(!strcmp(item,"refspecies"))
		mzrss->refspecies=i1;
	return; }


/* mzrsetupmoleculizer */
int mzrsetupmoleculizer(simptr sim,char *erstr) {
#ifdef LIBMOLECULIZER
	mzrssptr mzrss;
	species** new_species_array;
	reaction** new_reactions_array;
	int number_species,number_reactions,er,inm,i;
	enum MolecState ms;

	if(!sim->mzrss) return 0;
	mzrss=sim->mzrss;

	if(mzrss->condition==SCinit) {
		CHECKS(!mzrssload(sim,erstr),erstr);
		er=mzrMakeNameHash(sim);
		CHECKS(er!=1,"out of memory in mzrMakeNameHash");
		CHECKS(!(er>1),"BUG: in mzrMakeNameHash");
		if(er<0) {
			sprintf(erstr,"ERROR: species %s has multiple names",mzrss->smolname[-1-er]);
			CHECKS(0,erstr); }
		if(mzrss->refspecies>0) {
			for(ms=0;ms<MSMAX;ms++)
				mzrss->refdifc[ms]=sim->mols->difc[mzrss->refspecies][ms]; }
		mzrsetcondition(mzrss,SClists,1); }

	if(mzrss->condition==SClists) {
		CHECKS(!mzrExpandUnexpandedSpecies(sim),"BUG: in mzrExpandUnexpandedSpecies");

		for(inm=0;inm<mzrss->nnamehash;inm++) {				// set molecule exist element
			i=stringfind(sim->mols->spname,sim->mols->nspecies,mzrss->smolname[inm]);
			if(i>0) molsetexist(sim,i,MSall,1); }

		mzrsetcondition(mzrss,SCparams,1); }

	if(mzrss->condition==SCparams) {
		new_species_array=NULL;
		new_reactions_array=NULL;
		number_species=0;
		number_reactions=0;

		CHECKS(!getDeltaSpecies(mzrss->mzr,&new_species_array,&number_species),"BUG: in getDeltaSpecies");
		CHECKS(!getDeltaReactions(mzrss->mzr,&new_reactions_array,&number_reactions),"BUG: in getDeltaReactions");
		CHECKS(!addMzrSpeciesArrayToSim(sim,new_species_array,number_species),"BUG: in addMzrSpeciesArrayToSim");
		CHECKS(!addMzrReactionArrayToSim(sim,new_reactions_array,number_reactions),"BUG: in addMzrReactionArrayToSim");
		CHECKS(!clearDeltaState(mzrss->mzr),"BUG: in clearDeltaState");

		freeSpeciesArray(new_species_array,(unsigned int)number_species);
		freeReactionArray(new_reactions_array,(unsigned int)number_reactions);
		mzrsetcondition(sim->mzrss,SCok,1); }
	
	return 0;
 failure:
	return 1;
#else
	if(sim->mzrss) mzrsetcondition(sim->mzrss,SCok,1);
	return 0;
#endif
}

/******************************************************************************/
/*************************** core simulation functions ************************/
/******************************************************************************/


/* addMzrSpeciesArrayToSim */
#ifdef LIBMOLECULIZER
int addMzrSpeciesArrayToSim(simptr sim,species **species_array,int number_species) {
	mzrssptr mzrss;
	int speciesNdx,i,code;
	char uniqueID[STRCHAR];

	mzrss=sim->mzrss;
	for(speciesNdx=0;speciesNdx<number_species;speciesNdx++) {
		i=stringfind(mzrss->tagname,mzrss->nnamehash,species_array[speciesNdx]->name);
		if(i<0) {
			code=convertTaggedNameToUniqueID(mzrss->mzr,species_array[speciesNdx]->name,uniqueID,STRCHAR);
			if(code==1 || code==2) return 1;
			if(code==3) uniqueID[STRCHAR-1]='\0';		// truncate too long name and hope for the best
			i=mzraddtonamehash(mzrss,species_array[speciesNdx]->name,uniqueID);
			if(i<0) return 1; }

		code=moladdspecies(sim,mzrss->smolname[i]);																	// call to Smoldyn
		if(code>=0) {
			molsetexpansionflag(sim,code,1);																					// call to Smoldyn
			mzrAssignDiffCoeff(sim,species_array[speciesNdx]->name,*species_array[speciesNdx]->mass,code);
			mzrAssignDrawingParameters(sim,species_array[speciesNdx]->name,code); }
		else if(code==-5) {
			if(!strcmp(mzrss->smolname[i],sim->mols->spname[mzrss->refspecies]))
				mzrss->refmass=*species_array[speciesNdx]->mass; }
		else return 1; }
	return 0; }
#endif


/* addMzrReactionArrayToSim */
#ifdef LIBMOLECULIZER
int addMzrReactionArrayToSim(simptr sim,reaction **reactionlist,int number_reactions) {
	int reactionNdx,numReactants,numProducts;
	int smoldyn_reactants[2],smoldyn_products[MAXPRODUCT];
	int reactNdx,prodNdx,i,er;

	for(reactionNdx=0;reactionNdx<number_reactions;reactionNdx++) {
		if(*reactionlist[reactionNdx]->rate>0) {
			numReactants=reactionlist[reactionNdx]->numberReactants;
			numProducts=reactionlist[reactionNdx]->numberProducts;

			for(reactNdx=0;reactNdx<numReactants;reactNdx++) {
				i=mzrTagName2SmolName(sim,reactionlist[reactionNdx]->reactantVector[reactNdx]->name);
				if(i<0) return 1;
				smoldyn_reactants[reactNdx]=i; }

			for(prodNdx=0;prodNdx<numProducts;++prodNdx) {
				i=mzrTagName2SmolName(sim,reactionlist[reactionNdx]->productVector[prodNdx]->name);
				if(i<0) return 1;
				smoldyn_products[prodNdx]=i; }
			
			er=mzrAddRxn(sim,reactionlist[reactionNdx]->name,numReactants,smoldyn_reactants,smoldyn_products,numProducts,*reactionlist[reactionNdx]->rate);
			if(er) return 1;

			mzrAssignProductDefaultState(sim->mzrss,smoldyn_reactants,numReactants,smoldyn_products,numProducts); }}
	return 0; }
#endif


/* mzrExpandSpecies */
int mzrExpandSpecies(simptr sim,int ident) {
#ifdef LIBMOLECULIZER
	mzrssptr mzrss;
	int er,size1,size2;
	char tagged_name[STRCHAR];

	mzrss=sim->mzrss;
	er=convertSomeNameToTaggedName(mzrss->mzr,sim->mols->spname[ident],tagged_name,STRCHAR);
	if(er) return 1;

	size1=getNumberOfSpecies(mzrss->mzr)*getNumberOfReactions(mzrss->mzr);
	if(mzrss->maxNetworkSpecies<0 || mzrss->maxNetworkSpecies>getNumberOfSpecies(mzrss->mzr))
		expandSpeciesByTag(mzrss->mzr,tagged_name);
	molsetexpansionflag(sim,ident,0);																					// call to Smoldyn
	size2=getNumberOfSpecies(mzrss->mzr)*getNumberOfReactions(mzrss->mzr);

	if(size1!=size2) {						// If the network grew because of expansion. 
		mzrsetcondition(sim->mzrss,SCparams,0); }

	return 0;
#else
	return 2;
#endif
}


/* mzrExpandUnexpandedSpecies. */
int mzrExpandUnexpandedSpecies(simptr sim) {
	int i;
	molssptr mols;

	mols=sim->mols;
	for(i=0;i<mols->nspecies;i++) {
		if(mols->expand[i]) {
			if(mzrExpandSpecies(sim,i)) {
				return 1; }}}
	return 0; }


/* mzrAddRxn */
int mzrAddRxn(simptr sim,char *name,int order,int *reactants,int *products,int nprod,double rate) {
	mzrssptr mzrss;
	enum MolecState rctstate[1],prdstate[MAXPRODUCT],ms;
	rxnptr rxn;
	int rallsoln,pallsoln,i;
	char smolrxn[STRCHAR],errorstring[STRCHAR];

	mzrss=sim->mzrss;
	rallsoln=1;
	for(i=0;i<order;i++) {
		ms=(mzrss->defaultstate)?mzrss->defaultstate[reactants[i]]:MSsoln;
		if(ms==MSnone) ms=MSsoln;
		if(ms!=MSsoln) rallsoln=0;
		rctstate[i]=ms; }

	pallsoln=1;
	for(i=0;i<nprod;i++) {
		ms=(mzrss->defaultstate)?mzrss->defaultstate[products[i]]:MSsoln;
		if(ms==MSnone) ms=MSsoln;
		if(ms!=MSsoln) pallsoln=0;
		prdstate[i]=ms; }

	mzrNextSmolrxnName(mzrss,smolrxn);
	i=mzraddtorxnhash(mzrss,name,smolrxn);
	if(i<0) return 1;
	rxn=RxnAddReactionCheck(sim,smolrxn,order,reactants,rctstate,nprod,products,prdstate,NULL,NULL,errorstring);		// call to Smoldyn
	if(!rxn) {
		fprintf(stderr,"%s\n",errorstring);
		return 1; }
	RxnSetValue(sim,"rate",rxn,rate);

	if(rallsoln && !pallsoln) {				// all reactants in soln but not all products in soln
		for(i=0;i<order;i++) rctstate[i]=MSall;
		RxnSetPermit(sim,rxn,order,rctstate,1);
		for(i=0;i<order;i++) rctstate[i]=MSsoln;
		RxnSetPermit(sim,rxn,order,rctstate,0); }

	return 0; }




