/* Steven Andrews, started 10/22/2001.
 This is an application programming interface for the Smoldyn program.  See
 documentation called Smoldyn_doc1.pdf and Smoldyn_doc2.pdf.
 Copyright 2003-2011 by Steven Andrews.  This work is distributed under the terms
 of the Gnu General Public License (GPL). */

#include <string.h>
#include "string2.h"
#include "smoldyn.h"
#include "libsmoldyn.h"

#define LCHECK(A,B,C,D) if(!(A)) {smolSetError(B,C,D);if(C<ECwarning) goto failure;} else (void)0

enum ErrorCode liberrorcode=ECok;
enum ErrorCode libwarncode=ECok;
char liberrorfunction[STRCHAR]="";
char liberrorstring[STRCHAR]="";
int libdebugmode=0;

/******************************************************************************/
/*********************************** Errors ***********************************/
/******************************************************************************/

/* smolSetError */
void smolSetError(const char *errorfunction,enum ErrorCode errorcode,const char *errorstring) {
	char string[STRCHAR];

	if(errorcode==ECsame) return;
	liberrorcode=errorcode;
	libwarncode=(errorcode>=ECwarning)?errorcode:ECok;
	if(errorstring)
		strncpy(liberrorstring,errorstring,STRCHAR);
	else liberrorstring[0]='\0';
	if(errorfunction)
		strncpy(liberrorfunction,errorfunction,STRCHAR);
	else liberrorfunction[0]='\0';

	if(libdebugmode && liberrorfunction[0]!='\0') {
		if(liberrorcode==ECnotify)
			fprintf(stderr,"Libsmoldyn notification from %s: %s\n",liberrorfunction,liberrorstring);
		else if(liberrorcode==ECwarning)
			fprintf(stderr,"Libsmoldyn warning in %s: %s\n",liberrorfunction,liberrorstring);
		else
			fprintf(stderr,"Libsmoldyn '%s' error in %s: %s\n",smolErrorCodeToString(liberrorcode,string),liberrorfunction,liberrorstring); }
	return; }


/* smolGetError */
enum ErrorCode smolGetError(char *errorfunction,char *errorstring,int clearerror) {
	enum ErrorCode erc;

	erc=liberrorcode;
	if(errorfunction) strcpy(errorfunction,liberrorfunction);
	if(errorstring) strcpy(errorstring,liberrorstring);
	if(clearerror) smolClearError();
	return erc; }


/* smolClearError */
void smolClearError(void) {
	if(libdebugmode && liberrorcode!=ECok)
		fprintf(stderr,"  Libsmoldyn error cleared\n");
	liberrorcode=ECok;
	libwarncode=ECok;
	liberrorfunction[0]='\0';
	liberrorstring[0]='\0';
	return; }


/* smolSetDebugMode */
void smolSetDebugMode(int debugmode) {
	libdebugmode=debugmode;
	return; }


/* smolErrorCodeToString */
char *smolErrorCodeToString(enum ErrorCode erc,char *string) {
	if(erc==ECok) strcpy(string,"ok");
	else if(erc==ECnotify) strcpy(string,"notify");
	else if(erc==ECwarning) strcpy(string,"warning");
	else if(erc==ECerror) strcpy(string,"error");
	else if(erc==ECbug) strcpy(string,"Smoldyn bug");
	else if(erc==ECmissing) strcpy(string,"missing");
	else if(erc==ECbounds) strcpy(string,"bounds");
	else if(erc==ECmemory) strcpy(string,"memory");
	else if(erc==ECsyntax) strcpy(string,"syntax");
	else if(erc==ECnonexist) strcpy(string,"nonexistant");
	else if(erc==ECsame) strcpy(string,"same as before");
	else if(erc==ECall) strcpy(string,"all");
	else strcpy(string,"undefined");
	return string; }


/******************************************************************************/
/******************************** Sim structure *******************************/
/******************************************************************************/

/* smolNewSim */
simptr smolNewSim(int dim,double *lowbounds,double *highbounds) {
	const char *funcname="smolNewSim";
	simptr sim;
	int d,er;

	sim=NULL;
	LCHECK(dim>0,funcname,ECbounds,"dim must be >0");
	LCHECK(dim<=3,funcname,ECbounds,"dim must be <=3");
	LCHECK(lowbounds,funcname,ECmissing,"missing lowbounds");
	LCHECK(highbounds,funcname,ECmissing,"missing highbounds");
	for(d=0;d<dim;d++)
		LCHECK(lowbounds[d]<highbounds[d],funcname,ECbounds,"lowbounds must be < highbounds");

	sim=simalloc(NULL);
	LCHECK(sim,funcname,ECmemory,"allocating sim");
	er=simsetdim(sim,dim);
	LCHECK(!er,funcname,ECbug,"simsetdim bug");
	for(d=0;d<dim;d++) {
		er=walladd(sim,d,0,lowbounds[d],'t');
		LCHECK(!er,funcname,ECmemory,"allocating wall");
		er=walladd(sim,d,1,highbounds[d],'t');
		LCHECK(!er,funcname,ECmemory,"allocating wall"); }

	return sim;
 failure:
	if(sim) simfree(sim);
	return NULL; }


/* smolUpdateSim */
enum ErrorCode smolUpdateSim(simptr sim) {
	const char *funcname="smolUpdateSim";
	int er;
	char erstr[STRCHAR];

	LCHECK(sim,funcname,ECmissing,"missing sim");
	er=simupdate(sim,erstr);
	LCHECK(!er,funcname,ECerror,erstr);
	return ECok;
 failure:
	return liberrorcode; }


/* smolRunTimeStep */
enum ErrorCode smolRunTimeStep(simptr sim) {
	const char *funcname="smolRunTimeStep";
	int er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	sim->clockstt=time(NULL);
	er=simulatetimestep(sim);
	sim->elapsedtime+=difftime(time(NULL),sim->clockstt);
	LCHECK(er!=1,funcname,ECnotify,"Simulation complete");
	LCHECK(er!=2,funcname,ECerror,"Simulation terminated during molecule assignment\n  Out of memory");
	LCHECK(er!=3,funcname,ECerror,"Simulation terminated during order 0 reaction\n  Not enough molecules allocated");
	LCHECK(er!=4,funcname,ECerror,"Simulation terminated during order 1 reaction\n  Not enough molecules allocated");
	LCHECK(er!=5,funcname,ECerror,"Simulation terminated during order 2 reaction\n  Not enough molecules allocated");
	LCHECK(er!=6,funcname,ECerror,"Simulation terminated during molecule sorting\n  Out of memory");
	LCHECK(er!=7,funcname,ECnotify,"Simulation stopped by a runtime command");
	LCHECK(er!=8,funcname,ECerror,"Simulation terminated during simulation state updating\n  Out of memory");
	LCHECK(er!=9,funcname,ECerror,"Simulation terminated during diffusion\n  Out of memory");
	return libwarncode;
 failure:
	return liberrorcode; }


/* smolRunSim */
enum ErrorCode smolRunSim(simptr sim) {
	const char *funcname="smolRunSim";
	int er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	sim->clockstt=time(NULL);
	er=simdocommands(sim);
	LCHECK(er!=6,funcname,ECerror,"Simulation terminated during molecule sorting\n  Out of memory");
	LCHECK(er!=7,funcname,ECnotify,"Simulation stopped by a runtime command");
	LCHECK(er!=8,funcname,ECerror,"Simulation terminated during simulation state updating\n  Out of memory");
	if(!er)
		while((er=simulatetimestep(sim))==0);
	sim->elapsedtime+=difftime(time(NULL),sim->clockstt);
	LCHECK(er!=1,funcname,ECnotify,"Simulation complete");
	LCHECK(er!=2,funcname,ECerror,"Simulation terminated during molecule assignment\n  Out of memory");
	LCHECK(er!=3,funcname,ECerror,"Simulation terminated during order 0 reaction\n  Not enough molecules allocated");
	LCHECK(er!=4,funcname,ECerror,"Simulation terminated during order 1 reaction\n  Not enough molecules allocated");
	LCHECK(er!=5,funcname,ECerror,"Simulation terminated during order 2 reaction\n  Not enough molecules allocated");
	LCHECK(er!=6,funcname,ECerror,"Simulation terminated during molecule sorting\n  Out of memory");
	LCHECK(er!=7,funcname,ECnotify,"Simulation stopped by a runtime command");
	LCHECK(er!=8,funcname,ECerror,"Simulation terminated during simulation state updating\n  Out of memory");
	LCHECK(er!=9,funcname,ECerror,"Simulation terminated during diffusion\n  Out of memory");
	return libwarncode;
 failure:
	return liberrorcode; }


/* smolRunSimUntil */
enum ErrorCode smolRunSimUntil(simptr sim,double pausetime) {
	const char *funcname="smolRunSimUntil";
	int er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	sim->clockstt=time(NULL);
	er=simdocommands(sim);
	LCHECK(er!=6,funcname,ECerror,"Simulation terminated during molecule sorting\n  Out of memory");
	LCHECK(er!=7,funcname,ECnotify,"Simulation stopped by a runtime command");
	LCHECK(er!=8,funcname,ECerror,"Simulation terminated during simulation state updating\n  Out of memory");
	if(!er)
		while((er=simulatetimestep(sim))==0 && sim->time<pausetime);
	sim->elapsedtime+=difftime(time(NULL),sim->clockstt);
	LCHECK(er!=1,funcname,ECnotify,"Simulation complete");
	LCHECK(er!=2,funcname,ECerror,"Simulation terminated during molecule assignment\n  Out of memory");
	LCHECK(er!=3,funcname,ECerror,"Simulation terminated during order 0 reaction\n  Not enough molecules allocated");
	LCHECK(er!=4,funcname,ECerror,"Simulation terminated during order 1 reaction\n  Not enough molecules allocated");
	LCHECK(er!=5,funcname,ECerror,"Simulation terminated during order 2 reaction\n  Not enough molecules allocated");
	LCHECK(er!=6,funcname,ECerror,"Simulation terminated during molecule sorting\n  Out of memory");
	LCHECK(er!=7,funcname,ECnotify,"Simulation stopped by a runtime command");
	LCHECK(er!=8,funcname,ECerror,"Simulation terminated during simulation state updating\n  Out of memory");
	LCHECK(er!=9,funcname,ECerror,"Simulation terminated during diffusion\n  Out of memory");
	return libwarncode;
 failure:
	return liberrorcode; }


/* smolFreeSim */
enum ErrorCode smolFreeSim(simptr sim) {
	simfree(sim);
	return ECok; }


/* smolDisplaySim */
enum ErrorCode smolDisplaySim(simptr sim) {
	simsystemoutput(sim);
	return ECok; }


/******************************************************************************/
/************************** Read configuration file ***************************/
/******************************************************************************/

/* smolPrepareSimFromFile */
simptr smolPrepareSimFromFile(const char *filepath,const char *filename,const char *flags) {
	const char *funcname="smolPrepareSimFromFile";
	int er;
	char emptystring[STRCHAR];
	simptr sim;

	sim=NULL;
	LCHECK(filename,funcname,ECmissing,"missing filename");

	emptystring[0]='\0';
	if(!filepath) filepath=emptystring;
	if(!flags) flags=emptystring;
	er=setupsim(filepath,filename,&sim,flags);
	LCHECK(!er,funcname,ECerror,"setupsim failure");

	return sim;
 failure:
	simfree(sim);
	return NULL; }


/* smolLoadSimFromFile */
enum ErrorCode smolLoadSimFromFile(const char *filepath,const char *filename,simptr *simpointer,const char *flags) {
	const char *funcname="smolLoadSimFromFile";
	int er;
	char emptystring[STRCHAR];
	char erstr[STRCHAR];
	simptr sim;
	
	LCHECK(filename,funcname,ECmissing,"missing filename");
	LCHECK(simpointer,funcname,ECmissing,"missing simpointer");

	emptystring[0]='\0';
	if(!filepath) filepath=emptystring;
	if(!flags) flags=emptystring;

	sim=*simpointer;
	if(!sim) {
		sim=simalloc(filepath);
		LCHECK(sim,funcname,ECmemory,"allocating sim"); }
	er=loadsim(sim,filepath,filename,erstr,flags);
	LCHECK(!er,funcname,ECerror,erstr);

	*simpointer=sim;
	return ECok;
 failure:
	return liberrorcode; }


/* smolReadConfigString */
enum ErrorCode smolReadConfigString(simptr sim,const char *statement,char *parameters) {
	const char *funcname="smolReadConfigString";
	char erstr[STRCHAR];
	int er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(statement,funcname,ECmissing,"missing statement");
	er=simreadstring(sim,statement,parameters,erstr);
	LCHECK(!er,funcname,ECerror,erstr);

	return ECok;
 failure:
	return liberrorcode; }


/******************************************************************************/
/***************************** Simulation settings ****************************/
/******************************************************************************/

/* smolSetSimTimes */
enum ErrorCode smolSetSimTimes(simptr sim,double timestart,double timestop,double timestep) {
	const char *funcname="smolSetSimTimes";

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(timestep>0,funcname,ECbounds,"timestep value");
	simsettime(sim,timestart,0);
	simsettime(sim,timestart,1);
	simsettime(sim,timestop,2);
	simsettime(sim,timestep,3);
	return ECok;
 failure:
	return liberrorcode; }


/* smolSetTimeStart */
enum ErrorCode smolSetTimeStart(simptr sim,double timestart) {
	const char *funcname="smolSetTimeStart";

	LCHECK(sim,funcname,ECmissing,"missing sim");
	simsettime(sim,timestart,1);
	return ECok;
 failure:
	return liberrorcode; }


/* smolSetTimeStop */
enum ErrorCode smolSetTimeStop(simptr sim,double timestop) {
	const char *funcname="smolSetTimeStop";

	LCHECK(sim,funcname,ECmissing,"missing sim");
	simsettime(sim,timestop,2);
	return ECok;
 failure:
	return liberrorcode; }


/* smolSetTimeNow */
enum ErrorCode smolSetTimeNow(simptr sim,double timenow) {
	const char *funcname="smolSetTimeNow";

	LCHECK(sim,funcname,ECmissing,"missing sim");
	simsettime(sim,timenow,0);
	return ECok;
 failure:
	return liberrorcode; }


/* smolSetTimeStep */
enum ErrorCode smolSetTimeStep(simptr sim,double timestep) {
	const char *funcname="smolSetTimeStep";

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(timestep>0,funcname,ECbounds,"timestep is not > 0");
	simsettime(sim,timestep,3);
	return ECok;
 failure:
	return liberrorcode; }


//**enum ErrorCode smolSetRandomSeed(simptr sim,double seed);
//**enum ErrorCode smolSetPartitions(simptr sim,double molperbox,double boxsize);


/******************************************************************************/
/********************************** Graphics **********************************/
/******************************************************************************/

//**enum ErrorCode smolSetGraphicsParams(simptr sim,int type,int timesteps,double delay);
//**enum ErrorCode smolSetTiffParams(simptr sim,int timesteps,const char *tiffname,int lowcount,int highcount);
//**enum ErrorCode smolSetLightParams(simptr sim,int lightindex,double *ambient,double *diffuse,double *specular,double *position);
//**enum ErrorCode smolSetBackgroundStyle(simptr sim,double *color);
//**enum ErrorCode smolSetFrameStyle(simptr sim,double thickness,double *color);
//**enum ErrorCode smolSetGridStyle(simptr sim,double thickness,double *color);
//**enum ErrorCode smolSetTextStyle(simptr sim,double *color);
//**enum ErrorCode smolAddTextDisplay(simptr sim,char *item);

/******************************************************************************/
/***************************** Runtime commands *******************************/
/******************************************************************************/

/* smolSetOutputPath */
enum ErrorCode smolSetOutputPath(simptr sim,const char *path) {
	const char *funcname="smolSetOutputPath";
	int er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(path,funcname,ECmissing,"missing path");
	er=scmdsetfroot(sim->cmds,path);
	LCHECK(!er,funcname,ECbug,"scmdsetfroot bug");
	return ECok;
 failure:
	return liberrorcode; }


/* smolAddOutputFile */
enum ErrorCode smolAddOutputFile(simptr sim,char *filename,int suffix,int append) {
	const char *funcname="smolSetOutputFile";
	int er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(filename,funcname,ECmissing,"missing filename");
	LCHECK(!strchr(filename,' '),funcname,ECwarning,"only first word of filename is used");
	er=scmdsetfnames(sim->cmds,filename,append);
	LCHECK(!er,funcname,ECmemory,"allocating filename");
	if(suffix>=0) {
		er=scmdsetfsuffix(sim->cmds,filename,suffix);
		LCHECK(!er,funcname,ECbug,"scmdsetfsuffix bug"); }

	return libwarncode;
 failure:
	return liberrorcode; }


/* smolAddCommand */
enum ErrorCode smolAddCommand(simptr sim,char type,double on,double off,double step,double multiplier,const char *commandstring) {
	const char *funcname="smolSetCommand";

	//** function not written yet because there is no modularized function in SimCommand.c to wrap. ??
	LCHECK(sim,funcname,ECmissing,"missing sim");
	return ECok;
 failure:
	return liberrorcode; }


/* smolAddCommandFromString */
enum ErrorCode smolAddCommandFromString(simptr sim,char *string) {
	const char *funcname="smolSetCommandFromString";
	int er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(string,funcname,ECmissing,"missing string");
	er=scmdstr2cmd(sim->cmds,string,sim->tmin,sim->tmax,sim->dt);
	LCHECK(er!=1,funcname,ECmemory,"out of memory in cmd");
	LCHECK(er!=2,funcname,ECbug,"BUG: no command superstructure for cmd");
	LCHECK(er!=3,funcname,ECsyntax,"cmd format: type [on off dt] string");
	LCHECK(er!=4,funcname,ECmissing,"command string is missing");
	LCHECK(er!=5,funcname,ECbounds,"cmd time step needs to be >0");
	LCHECK(er!=6,funcname,ECsyntax,"command timing type character not recognized");
	LCHECK(er!=7,funcname,ECerror,"insertion of command in queue failed");
	LCHECK(er!=8,funcname,ECbounds,"cmd time multiplier needs to be >1");
	return ECok;
 failure:
	return liberrorcode; }

	
/******************************************************************************/
/********************************* Molecules **********************************/
/******************************************************************************/

enum ErrorCode smolAddSpecies(simptr sim,const char *species,const char *mollist) {
	const char *funcname="smolAddSpecies";
	int i,ll;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(species,funcname,ECmissing,"missing species");
	if(mollist && mollist[0]!='\0') {
		ll=smolGetMolListIndex(sim,mollist);
		LCHECK(ll>=0,funcname,ECsame,NULL);
		LCHECK(sim->mols->listtype[ll]==MLTsystem,funcname,ECsyntax,"mollist is not a system list"); }
	else ll=-1;
	i=moladdspecies(sim,species);
	LCHECK(i!=-2,funcname,ECbug,"add species bug");
	LCHECK(i!=-3,funcname,ECbug,"more species are entered than are automatically allocated");
	LCHECK(i!=-4,funcname,ECsyntax,"'empty' is not a permitted species name");
	LCHECK(i!=-5,funcname,ECwarning,"this species has already been declared");
	if(mollist && mollist[0]!='\0')
		molsetlistlookup(sim,i,MSall,ll);
	return libwarncode;
 failure:
	return liberrorcode; }


/* smolGetSpeciesIndex */
int smolGetSpeciesIndex(simptr sim,const char *species,enum MolecState *stateptr) {
	const char *funcname=NULL;
	int i;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(species,funcname,ECmissing,"missing speciesname");
	LCHECK(sim->mols,funcname,ECnonexist,"no species defined");
	LCHECK(strcmp(species,"all"),funcname,ECall,"species cannot be 'all'");
	i=stringfind(sim->mols->spname,sim->mols->nspecies,species);
	LCHECK(i>0,funcname,ECnonexist,"species not found");
	return i;
 failure:
	return (int)liberrorcode; }


/* smolGetSpeciesName */
char *smolGetSpeciesName(simptr sim,int speciesindex,char *species) {
	const char *funcname="smolGetSpeciesName";

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(sim->mols,funcname,ECnonexist,"no species defined");
	LCHECK(speciesindex>=0,funcname,ECbounds,"speciesindex < 0");
	LCHECK(speciesindex<sim->mols->nspecies,funcname,ECnonexist,"species doesn't exist");
	LCHECK(species,funcname,ECmissing,"missing species");
	strcpy(species,sim->mols->spname[speciesindex]);
	return species;
 failure:
	return NULL; }


/* smolSetSpeciesMobility */
enum ErrorCode smolSetSpeciesMobility(simptr sim,const char *species,enum MolecState state,double difc,double *drift,double *difmatrix) {
	const char *funcname="smolSetSpeciesMobility";
	int i,er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	i=smolGetSpeciesIndex(sim,species,NULL);
	if(i==(int)ECall) smolClearError();
	else LCHECK(i>0,funcname,ECsame,NULL);
	LCHECK((state>=0 && state<MSMAX) || state==MSall,funcname,ECsyntax,"invalid state");

	if(difc>=0) molsetdifc(sim,i,state,difc);
	if(drift) {
		er=molsetdrift(sim,i,state,drift);
		LCHECK(!er,funcname,ECmemory,"allocating drift"); }
	if(difmatrix) {
		er=molsetdifm(sim,i,state,difmatrix);
		LCHECK(!er,funcname,ECmemory,"allocating difmatrix"); }
	return ECok;
 failure:
	return liberrorcode; }


/* smolAddMolList */
enum ErrorCode smolAddMolList(simptr sim,const char *mollist) {
	const char *funcname="smolAddMolList";
	int ll;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(mollist,funcname,ECmissing,"missing mollist");
	ll=addmollist(sim,mollist,MLTsystem);
	LCHECK(ll!=-1,funcname,ECmemory,"out of memory");
	LCHECK(ll!=-2,funcname,ECwarning,"molecule list name has already been used");
	LCHECK(ll!=-3,funcname,ECbug,"illegal addmollist inputs");
	return libwarncode;
 failure:
	return liberrorcode; }


/* smolGetMolListIndex */
int smolGetMolListIndex(simptr sim,const char *mollist) {
	const char *funcname=NULL;
	int ll;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(mollist,funcname,ECmissing,"missing mollist");
	LCHECK(sim->mols,funcname,ECnonexist,"no molecule lists defined");
	LCHECK(strcmp(mollist,"all"),funcname,ECall,"molecule list cannot be 'all'");
	ll=stringfind(sim->mols->listname,sim->mols->nlist,mollist);
	LCHECK(ll>=0,funcname,ECnonexist,"list name not recognized");
	return ll;
 failure:
	return (int)liberrorcode; }


/* smolGetMolListName */
char *smolGetMolListName(simptr sim,int mollistindex,char *mollist) {
	const char *funcname="smolGetMolListName";

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(mollistindex>=0,funcname,ECbounds,"mollistindex < 0");
	LCHECK(sim->mols,funcname,ECnonexist,"no molecule lists defined");
	LCHECK(mollistindex<sim->mols->nlist,funcname,ECnonexist,"molecule list doesn't exist");
	LCHECK(mollist,funcname,ECmissing,"missing mollist");
	strcpy(mollist,sim->mols->listname[mollistindex]);
	return mollist;
 failure:
	return NULL; }


/* smolSetMolList */
enum ErrorCode smolSetMolList(simptr sim,const char *species,enum MolecState state,const char *mollist) {
	const char *funcname="smolSetMolList";
	int i,ll;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	i=smolGetSpeciesIndex(sim,species,NULL);
	if(i==(int)ECall) smolClearError();
	else LCHECK(i>0,funcname,ECsame,NULL);
	LCHECK((state>=0 && state<MSMAX) || state==MSall,funcname,ECsyntax,"invalid state");
	ll=smolGetMolListIndex(sim,mollist);
	LCHECK(ll>=0,funcname,ECsame,NULL);
	LCHECK(sim->mols->listtype[ll]==MLTsystem,funcname,ECerror,"list is not a system list");
	molsetlistlookup(sim,i,state,ll);
	return ECok;
 failure:
	return liberrorcode; }


/* smolSetMaxMolecules */
enum ErrorCode smolSetMaxMolecules(simptr sim,int maxmolecules) {
	const char *funcname="smolSetMaxMolecules";
	int er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(maxmolecules>0,funcname,ECbounds,"maxmolecules needs to be > 0");
	er=molsetmaxmol(sim,maxmolecules);
	LCHECK(!er,funcname,ECmemory,"out of memory allocating molecules");
	return ECok;
 failure:
	return liberrorcode; }


/* smolAddSolutionMolecules */
enum ErrorCode smolAddSolutionMolecules(simptr sim,const char *species,int number,double *lowposition,double *highposition) {
	const char *funcname="smolAddSolutionMolecules";
	int er,i,d;
	double *low,*high,lowpos[3],highpos[3];

	LCHECK(sim,funcname,ECmissing,"missing sim");
	i=smolGetSpeciesIndex(sim,species,NULL);
	LCHECK(i>0,funcname,ECsame,NULL);
	LCHECK(number>=0,funcname,ECbounds,"number cannot be < 0");
	if(!lowposition) {
		for(d=0;d<sim->dim;d++) lowpos[d]=sim->wlist[d*2]->pos;
		low=lowpos; }
	else
		low=lowposition;
	if(!highposition) {
		for(d=0;d<sim->dim;d++) highpos[d]=sim->wlist[d*2+1]->pos;
		high=highpos; }
	else
		high=highposition;

	er=addmol(sim,number,i,low,high,0);
	LCHECK(!er,funcname,ECmemory,"out of memory adding molecules");
	return ECok;
 failure:
	return liberrorcode; }


/* smolAddCompartmentMolecules */
enum ErrorCode smolAddCompartmentMolecules(simptr sim,const char *species,int number,const char *compartment) {
	const char *funcname="smolAddCompartmentMolecules";
	int i,er,c;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	i=smolGetSpeciesIndex(sim,species,NULL);
	LCHECK(i>0,funcname,ECsame,NULL);
	LCHECK(number>=0,funcname,ECbounds,"number < 0");
	c=smolGetCompartmentIndex(sim,compartment);
	LCHECK(c>=0,funcname,ECsame,NULL);
	er=addcompartmol(sim,number,i,sim->cmptss->cmptlist[c]);
	LCHECK(er!=2,funcname,ECerror,"compartment volume is zero or nearly zero");
	LCHECK(er!=3,funcname,ECmemory,"out of memory adding molecules");
	return ECok;
 failure:
	return liberrorcode; }


/* smolAddSurfaceMolecules */
enum ErrorCode smolAddSurfaceMolecules(simptr sim,const char *species,enum MolecState state,int number,const char *surface,enum PanelShape panelshape,const char *panel,double *position) {
	const char *funcname="smolAddSurfaceMolecules";
	int i,s,p,er;
	panelptr pnl;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	i=smolGetSpeciesIndex(sim,species,NULL);
	LCHECK(i>0,funcname,ECsame,NULL);
	LCHECK(state>=0 && state<MSMAX,funcname,ECsyntax,"invalid state");
	LCHECK(number>=0,funcname,ECbounds,"number < 0");
	s=smolGetSurfaceIndex(sim,surface);
	if(s==(int)ECall) smolClearError();
	else LCHECK(s>=0,funcname,ECsame,NULL);
	LCHECK((panelshape>=0 && panelshape<PSMAX) || panelshape==PSall,funcname,ECnonexist,"invalid panelshape");
	pnl=NULL;
	p=smolGetPanelIndex(sim,surface,NULL,panel);
	if(p==(int)ECall) smolClearError();
	else LCHECK(p>=0,funcname,ECsame,NULL);

	if(p>=0) {
		LCHECK(s>=0,funcname,ECsyntax,"needs to be specific surface");
		LCHECK(panelshape!=PSall,funcname,ECsyntax,"needs to be specific panelshape");
		pnl=sim->srfss->srflist[s]->panels[panelshape][p]; }
	else {
		LCHECK(!position,funcname,ECsyntax,"a panel must be specified if position is entered"); }
	er=addsurfmol(sim,number,i,state,position,pnl,s,panelshape,NULL);
	LCHECK(er!=1,funcname,ECmemory,"unable to allocate temporary storage space");
	LCHECK(er!=2,funcname,ECbug,"panel name not recognized");
	LCHECK(er!=3,funcname,ECmemory,"out of memory adding molecules");

	return ECok;
 failure:
	return liberrorcode; }


/* smolGetMoleculeCount */
int smolGetMoleculeCount(simptr sim,const char *species,enum MolecState state) {
	const char *funcname="smolGetMoleculeCount";
	int i;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	i=smolGetSpeciesIndex(sim,species,NULL);
	if(i==(int)ECall) smolClearError();
	else LCHECK(i>0,funcname,ECsame,NULL);
	return molcount(sim,i,state,NULL,-1);
 failure:
	return (int)liberrorcode; }


//**enum ErrorCode smolSetMoleculeStyle(simptr sim,const char *species,enum MolecState state,double size,double *color);


/******************************************************************************/
/*********************************** Surfaces *********************************/
/******************************************************************************/

/* smolSetBoundaryType */
enum ErrorCode smolSetBoundaryType(simptr sim,int dimension,int highside,char type) {
	const char *funcname="smolSetBoundaryType";
	int er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(dimension>=0 && dimension<sim->dim,funcname,ECbounds,"");
	LCHECK(type=='r' || type=='p' || type=='a' || type=='t',funcname,ECsyntax,"invalid type");
	if(highside>=0)
		er=wallsettype(sim,dimension,highside,type);
	else {
		er=wallsettype(sim,dimension,0,type);
		LCHECK(!er,funcname,ECbug,"bug in wallsettype");
		er=wallsettype(sim,dimension,1,type); }
	LCHECK(!er,funcname,ECbug,"bug in wallsettype");
	return ECok;
 failure:
	return liberrorcode; }


/* smolAddSurface */
enum ErrorCode smolAddSurface(simptr sim,const char *surface) {
	const char *funcname="smolAddSurface";
	int s;
	surfaceptr srf;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	s=smolGetSurfaceIndex(sim,surface);
	if(s==(int)ECnonexist) smolClearError();
	else if(s<0) LCHECK(0,funcname,ECsame,NULL);
	else LCHECK(0,funcname,ECerror,"surface is already in system");
	srf=surfaddsurface(sim,surface);
	LCHECK(srf,funcname,ECmemory,"out of memory adding surface");
	return ECok;
 failure:
	return liberrorcode; }


/* smolGetSurfaceIndex */
int smolGetSurfaceIndex(simptr sim,const char *surface) {
	const char *funcname=NULL;
	int s;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(surface,funcname,ECmissing,"missing surface");
	LCHECK(sim->srfss && sim->srfss->nsrf,funcname,ECnonexist,"no surfaces defined");
	LCHECK(strcmp(surface,"all"),funcname,ECall,"surface cannot be 'all'");
	s=stringfind(sim->srfss->snames,sim->srfss->nsrf,surface);
	LCHECK(s>=0,funcname,ECnonexist,"surface not found");
	return s;
 failure:
	return (int)liberrorcode; }


/* smolGetSurfaceName */
char *smolGetSurfaceName(simptr sim,int surfaceindex,char *surface) {
	const char *funcname="smolGetSurfaceName";

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(surfaceindex>=0,funcname,ECbounds,"invalid surface index");
	LCHECK(surface,funcname,ECmissing,"missing surface");
	LCHECK(sim->srfss && sim->srfss->nsrf,funcname,ECnonexist,"no surfaces defined");
	LCHECK(surfaceindex<sim->srfss->nsrf,funcname,ECnonexist,"surface does not exist");
	strcpy(surface,sim->srfss->snames[surfaceindex]);
	return surface;
 failure:
	return NULL; }


/* smolSetSurfaceAction */
enum ErrorCode smolSetSurfaceAction(simptr sim,const char *surface,enum PanelFace face,const char *species,enum MolecState state,enum SrfAction action) {
	const char *funcname="smolSetSurfaceAction";
	int er,i,s;
	surfaceptr srf;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	s=smolGetSurfaceIndex(sim,surface);
	if(s==(int)ECall) smolClearError();
	else LCHECK(s>=0,funcname,ECsame,NULL);
	LCHECK(face==PFfront || face==PFback || face==PFboth,funcname,ECbounds,"invalid face");
	i=smolGetSpeciesIndex(sim,species,NULL);
	if(i==(int)ECall) smolClearError();
	else LCHECK(i>0,funcname,ECsame,NULL);
	LCHECK((state>=0 && state<MSMAX) || state==MSall,funcname,ECbounds,"invalid state");
	LCHECK(action>=0 && action<=SAmult,funcname,ECbounds,"invalid action");
	if(s>=0) {
		srf=sim->srfss->srflist[s];
		er=surfsetaction(srf,i,state,face,action);
		LCHECK(!er,funcname,ECbug,"bug in surfsetaction"); }
	else {
		for(s=0;s<sim->srfss->nsrf;s++) {
			srf=sim->srfss->srflist[s];
			er=surfsetaction(srf,i,state,face,action);
			LCHECK(!er,funcname,ECbug,"bug in surfsetaction"); }}
	return ECok;
 failure:
	return liberrorcode; }


/* smolSetSurfaceRate */
enum ErrorCode smolSetSurfaceRate(simptr sim,const char *surface,const char *species,enum MolecState state,enum MolecState state1,enum MolecState state2,double rate,const char *newspecies,int isinternal) {
	const char *funcname="smolSetSurfaceRate";
	int er,i,i2,s;
	surfaceptr srf;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	s=smolGetSurfaceIndex(sim,surface);
	if(s==(int)ECall) smolClearError();
	else LCHECK(s>=0,funcname,ECsame,NULL);
	i=smolGetSpeciesIndex(sim,species,NULL);
	if(i==(int)ECall) smolClearError();
	else LCHECK(i>0,funcname,ECsame,NULL);
	LCHECK(state>=0 && state<MSMAX,funcname,ECbounds,"invalid state");
	LCHECK(state1>=0 && state1<MSMAX1,funcname,ECbounds,"invalid state1");
	LCHECK(state==MSsoln || state1==MSsoln || state1==MSbsoln || state1==state,funcname,ECsyntax,"nonsensical state combination");
	LCHECK(state2>=0 && state2<MSMAX1,funcname,ECbounds,"invalid state2");
	LCHECK(state1!=state2,funcname,ECsyntax,"cannot set rate for state1 = state2");
	if(newspecies && newspecies[0]!='\0') {
		i2=smolGetSpeciesIndex(sim,newspecies,NULL);
		LCHECK(i2>0,funcname,ECerror,"invalid newspecies"); }
	else i2=-5;
	LCHECK(rate>=0,funcname,ECbounds,"rate needs to be non-negative");
	LCHECK(!(isinternal && rate>1),funcname,ECbounds,"internal rate needs to be <= 1");

	if(s>=0) {
		srf=sim->srfss->srflist[s];
		er=surfsetrate(srf,i,state,state1,state2,i2,rate,isinternal?2:1);
		LCHECK(!er,funcname,ECerror,"error in surfsetrate"); }
	else {
		for(s=0;s<sim->srfss->nsrf;s++) {
			srf=sim->srfss->srflist[s];
			er=surfsetrate(srf,i,state,state1,state2,i2,rate,isinternal?2:1);
			LCHECK(!er,funcname,ECerror,"error in surfsetrate"); }}
	return ECok;
 failure:
	return liberrorcode; }


/* smolAddPanel */
enum ErrorCode smolAddPanel(simptr sim,const char *surface,enum PanelShape panelshape,const char *panel,const char *axisstring,double *params) {
	const char *funcname="smolAddPanel";
	int er,s;
	surfaceptr srf;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	s=smolGetSurfaceIndex(sim,surface);
	LCHECK(s>=0,funcname,ECsame,NULL);
	LCHECK(panelshape>=0 && panelshape<PSMAX,funcname,ECnonexist,"invalid panel shape");
	if(panelshape==PSrect) {
		LCHECK(axisstring,funcname,ECmissing,"missing axisstring"); }
	LCHECK(params,funcname,ECmissing,"missing params");
	srf=sim->srfss->srflist[s];
	er=surfaddpanel(srf,sim->dim,panelshape,axisstring,params,panel);
	LCHECK(er!=-1,funcname,ECmemory,"out of memory adding panel");
	LCHECK(er!=3,funcname,ECsyntax,"cannot parse axisstring");
	LCHECK(er!=4,funcname,ECbounds,"drawing slices and stacks need to be positive");
	LCHECK(er!=5,funcname,ECbounds,"cylinder ends cannot be at the same location");
	LCHECK(er!=6,funcname,ECbounds,"hemisphere outward pointing vector has zero length");
	LCHECK(er!=7,funcname,ECbounds,"radius needs to be positive");
	LCHECK(er!=8,funcname,ECbounds,"normal vector has zero length");
	LCHECK(er!=9,funcname,ECerror,"panel name was used before for a different panel shape");
	LCHECK(!er,funcname,ECbug,"bug in smolAddPanel");
	return ECok;
 failure:
	return liberrorcode; }


/* smolGetPanelIndex */
int smolGetPanelIndex(simptr sim,const char *surface,enum PanelShape *panelshapeptr,const char *panel) {
	const char *funcname=NULL;
	surfaceptr srf;
	int p,s;
	enum PanelShape ps;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	s=smolGetSurfaceIndex(sim,surface);
	LCHECK(s>=0,funcname,ECsame,NULL);
	LCHECK(panel,funcname,ECmissing,"missing panel name");
	LCHECK(strcmp(panel,"all"),funcname,ECall,"panel cannot be 'all'");
	srf=sim->srfss->srflist[s];
	p=-1;
	for(ps=0;ps<PSMAX && p<0;ps++)
		p=stringfind(srf->pname[ps],srf->npanel[ps],panel);
	LCHECK(p>=0,funcname,ECnonexist,"panel not found");
	if(panelshapeptr) *panelshapeptr=ps;
	return p;
 failure:
	return (int)liberrorcode; }


/* smolGetPanelName */
char *smolGetPanelName(simptr sim,const char *surface,enum PanelShape panelshape,int panelindex,char *panel) {
	const char *funcname="smolGetPanelName";
	surfaceptr srf;
	int s;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	s=smolGetSurfaceIndex(sim,surface);
	LCHECK(s>=0,funcname,ECsame,NULL);
	LCHECK(panelshape>=0 && panelshape<PSMAX,funcname,ECnonexist,"invalid panel shape");
	LCHECK(panelindex>=0,funcname,ECbounds,"invalid panel index");
	LCHECK(panel,funcname,ECmissing,"missing panel name");
	srf=sim->srfss->srflist[s];
	LCHECK(panelindex<srf->npanel[panelshape],funcname,ECnonexist,"no panel exists with this number");
	strcpy(panel,srf->pname[panelshape][panelindex]);
	return panel;
 failure:
	return NULL; }


//**enum ErrorCode smolSetPanelJump(simptr sim,const char *surface,const char *panel1,enum PanelFace face1,const char *panel2,enum PanelFace face2,int isbidirectional);
//**enum ErrorCode smolAddSurfaceUnboundedEmitter(simptr sim,const char *surface,enum PanelFace face,const char *species,double emitamount,double *emitposition);
//**enum ErrorCode smolSetSurfaceSimParams(simptr sim,double epsilon,double margin,double neighbordist);
//**enum ErrorCode smolAddPanelNeighbor(simptr sim,const char *surface1,const char *panel1,const char *surface2,const char *panel2);
//**enum ErrorCode smolSetSurfaceFaceStyle(simptr sim,const char *surface,enum PanelFace face,double *color,double shininess);
//**enum ErrorCode smolSetSurfaceEdgeStyle(simptr sim,const char *surface,enum PanelFace face,double thickness,double *color,int stipplefactor,int stipplepattern);


/******************************************************************************/
/******************************** Compartments ********************************/
/******************************************************************************/

/* smolAddCompartment */
enum ErrorCode smolAddCompartment(simptr sim,const char *compartment) {
	const char *funcname="smolAddCompartment";
	int c;
	compartptr cmpt;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	c=smolGetCompartmentIndex(sim,compartment);
	if(c==(int)ECnonexist) smolClearError();
	else if(c<0) LCHECK(0,funcname,ECsame,NULL);
	else LCHECK(0,funcname,ECerror,"compartment is already in system");
	cmpt=compartaddcompart(sim,compartment);
	LCHECK(cmpt,funcname,ECmemory,"out of memory adding compartment");
	return ECok;
 failure:
	return liberrorcode; }


/* smolGetCompartmentIndex */
int smolGetCompartmentIndex(simptr sim,const char *compartment) {
	const char *funcname=NULL;
	int c;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(compartment,funcname,ECmissing,"missing compartment");
	LCHECK(sim->cmptss && sim->cmptss->ncmpt,funcname,ECnonexist,"no compartments defined");
	LCHECK(strcmp(compartment,"all"),funcname,ECall,"compartment cannot be 'all'");
	c=stringfind(sim->cmptss->cnames,sim->cmptss->ncmpt,compartment);
	LCHECK(c>=0,funcname,ECnonexist,"compartment not found");
	return c;
 failure:
	return (int)liberrorcode; }


/* smolGetCompartmentName */
char *smolGetCompartmentName(simptr sim,int compartmentindex,char *compartment) {
	const char *funcname="smolGetCompartmentName";

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(compartmentindex>=0,funcname,ECbounds,"invalid compartment index");
	LCHECK(compartment,funcname,ECmissing,"missing compartment string");
	LCHECK(sim->cmptss && sim->cmptss->ncmpt,funcname,ECnonexist,"no compartments defined");
	LCHECK(compartmentindex<sim->cmptss->ncmpt,funcname,ECnonexist,"compartment does not exist");
	strcpy(compartment,sim->cmptss->cnames[compartmentindex]);
	return compartment;
 failure:
	return NULL; }


/* smolAddCompartmentSurface */
enum ErrorCode smolAddCompartmentSurface(simptr sim,const char *compartment,const char *surface) {
	const char *funcname="smolAddCompartmentSurface";
	int c,s,er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	c=smolGetCompartmentIndex(sim,compartment);
	LCHECK(c>=0,funcname,ECsame,NULL);
	s=smolGetSurfaceIndex(sim,surface);
	LCHECK(s>=0,funcname,ECsame,NULL);
	er=compartaddsurf(sim->cmptss->cmptlist[c],sim->srfss->srflist[s]);
	LCHECK(!er,funcname,ECmemory,"out of memory in compartaddsurf");
	return ECok;
 failure:
	return liberrorcode; }


/* smolAddCompartmentPoint */
enum ErrorCode smolAddCompartmentPoint(simptr sim,const char *compartment,double *point) {
	const char *funcname="smolAddCompartmentPoint";
	int c,er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	c=smolGetCompartmentIndex(sim,compartment);
	LCHECK(c>=0,funcname,ECsame,NULL);
	LCHECK(point,funcname,ECmissing,"missing point");
	er=compartaddpoint(sim->cmptss->cmptlist[c],sim->dim,point);
	LCHECK(!er,funcname,ECmemory,"out of memory in compartaddsurf");
	return ECok;
 failure:
	return liberrorcode; }


/* smolAddCompartmentLogic */
enum ErrorCode smolAddCompartmentLogic(simptr sim,const char *compartment,enum CmptLogic logic,const char *compartment2) {
	const char *funcname="smolAddCompartmentLogic";
	int c,c2,er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	c=smolGetCompartmentIndex(sim,compartment);
	LCHECK(c>=0,funcname,ECsame,NULL);
	LCHECK(logic>=CLequal && logic<CLnone,funcname,ECsyntax,"invalid logic operation");
	c2=smolGetCompartmentIndex(sim,compartment2);
	LCHECK(c2>=0,funcname,ECerror,"error with compartment2");
	er=compartaddcmptl(sim->cmptss->cmptlist[c],sim->cmptss->cmptlist[c2],logic);
	LCHECK(!er,funcname,ECmemory,"out of memory in compartaddcmpt");
	return ECok;
 failure:
	return liberrorcode; }



/******************************************************************************/
/********************************* Reactions **********************************/
/******************************************************************************/

/* smolAddReaction */
enum ErrorCode smolAddReaction(simptr sim,const char *reaction,const char *reactant1,enum MolecState rstate1,const char *reactant2,enum MolecState rstate2,int nproduct,const char **productspecies,enum MolecState *productstates,double rate) {
	const char *funcname="smolAddReaction";
	rxnptr rxn;
	int order,prd,rctident[MAXORDER],prdident[MAXPRODUCT];
	enum MolecState rctstate[MAXORDER];

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(reaction,funcname,ECmissing,"missing reaction name");
	order=0;
	rctident[0]=rctident[1]=0;
	rctstate[0]=rctstate[1]=MSnone;
	if(reactant1 && reactant1[0]!='\0') {
		rctident[order]=smolGetSpeciesIndex(sim,reactant1,NULL);
		LCHECK(rctident[order]>0,funcname,ECsame,NULL);
		LCHECK(rstate1>=0 && rstate1<MSMAX,funcname,ECbounds,"invalid rstate1");
		rctstate[order]=rstate1;
		order++; }
	if(reactant2 && reactant2[0]!='\0') {
		rctident[order]=smolGetSpeciesIndex(sim,reactant2,NULL);
		LCHECK(rctident[order]>0,funcname,ECsame,NULL);
		LCHECK(rstate2>=0 && rstate2<MSMAX,funcname,ECbounds,"invalid rstate2");
		rctstate[order]=rstate2;
		order++; }
	LCHECK(nproduct>=0,funcname,ECbounds,"invalid nproduct");
	if(nproduct) {
		LCHECK(productspecies,funcname,ECmissing,"missing product species");
		LCHECK(productstates,funcname,ECmissing,"missing product states");
		for(prd=0;prd<nproduct;prd++) {
			prdident[prd]=smolGetSpeciesIndex(sim,productspecies[prd],NULL);
			LCHECK(prdident[prd]>0,funcname,ECsame,NULL); }}
	rxn=RxnAddReaction(sim,reaction,order,rctident,rctstate,nproduct,prdident,productstates,NULL,NULL);
	LCHECK(rxn,funcname,ECmemory,"out of memory allocating reaction");
	return ECok;
 failure:
	return liberrorcode; }


/* smolGetReactionIndex */
int smolGetReactionIndex(simptr sim,int order,const char *reaction) {
	const char *funcname=NULL;
	int r;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(order>=0 && order<3,funcname,ECbounds,"invalid reaction order");
	LCHECK(reaction,funcname,ECmissing,"missing reaction");
	LCHECK(sim->rxnss[order] && sim->rxnss[order]->nrxn,funcname,ECnonexist,"no reactions defined of this order");
	LCHECK(strcmp(reaction,"all"),funcname,ECall,"reaction cannot be 'all'");
	r=stringfind(sim->rxnss[order]->rname,sim->rxnss[order]->totrxn,reaction);
	LCHECK(r>=0,funcname,ECnonexist,"reaction not found");
	return r;
 failure:
	return (int)liberrorcode; }


//**char *smolGetReactionName(simptr sim,int order,int reactionindex,char *reaction);


/* smolSetReactionRate */
enum ErrorCode smolSetReactionRate(simptr sim,int order,const char *reaction,double rate,int isinternal) {
	const char *funcname="smolSetReactionRate";
	int r,er;
	rxnptr rxn;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	r=smolGetReactionIndex(sim,order,reaction);
	LCHECK(r>=0,funcname,ECsame,NULL);
	rxn=sim->rxnss[order]->rxn[r];
	if(!isinternal)
		er=RxnSetValue(sim,"rate",rxn,rate);
	else if(order<2)
		er=RxnSetValue(sim,"prob",rxn,rate);
	else
		er=RxnSetValue(sim,"bindrad",rxn,rate);
	if(er==3) LCHECK(0,funcname,ECwarning,"rate was set previously");
	else LCHECK(!er,funcname,ECbug,"RxnSetValue error");
	return libwarncode;
 failure:
	return liberrorcode; }


//**enum ErrorCode smolSetReactionRegion(simptr sim,const char *reaction,const char *compartment,const char *surface);
//**enum ErrorCode smolSetReactionProducts(simptr sim,const char *reaction,enum RevParam,const char *product,double *parameters);


/******************************************************************************/
/*********************************** Ports ************************************/
/******************************************************************************/

/* smolAddPort */
enum ErrorCode smolAddPort(simptr sim,const char *port,const char *surface,enum PanelFace face) {
	const char *funcname="smolAddPort";
	int s;
	portptr portptr;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(port,funcname,ECmissing,"missing port");
	s=smolGetSurfaceIndex(sim,surface);
	if(s==(int)ECmissing) smolClearError();
	else LCHECK(s>=0,funcname,ECsame,NULL);
	LCHECK(face=PFfront || face==PFback || PFnone,funcname,ECsyntax,"invalid face");
	portptr=portaddport(sim,port,sim->srfss->srflist[s],face);
	LCHECK(portptr,funcname,ECmemory,"out of memory adding port");

	return ECok;
 failure:
	return liberrorcode; }


/* smolGetPortIndex */
int smolGetPortIndex(simptr sim,const char *port) {
	const char *funcname=NULL;
	int p;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(port,funcname,ECmissing,"missing port");
	LCHECK(sim->portss && sim->portss->nport,funcname,ECnonexist,"no ports defined");
	LCHECK(strcmp(port,"all"),funcname,ECall,"port cannot be 'all'");
	p=stringfind(sim->portss->portnames,sim->portss->nport,port);
	LCHECK(p>=0,funcname,ECnonexist,"port not found");
	return p;
 failure:
	return (int)liberrorcode; }


/* smolGetPortName */
char *smolGetPortName(simptr sim,int portindex,char *port) {
	const char *funcname="smolGetPortName";

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(portindex>=0,funcname,ECbounds,"invalid port index");
	LCHECK(port,funcname,ECmissing,"missing port string");
	LCHECK(sim->portss && sim->portss->nport,funcname,ECnonexist,"no ports defined");
	LCHECK(portindex<sim->portss->nport,funcname,ECnonexist,"port does not exist");
	strcpy(port,sim->portss->portnames[portindex]);
	return port;
 failure:
	return NULL; }


//**enum ErrorCode smolAddPortMolecules(simptr sim,const char *port,int nmolec,const char **species,enum MolecState *state,double **positions);
//**enum ErrorCode smolGetPortMolecules(simptr sim,const char *port,int *nmolec,const char **species,enum MolecState *state,double **positions);



