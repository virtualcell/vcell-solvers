/* Steven Andrews, started 10/22/2001.
 This is an application programming interface for the Smoldyn program.  See
 documentation called Smoldyn_doc1.pdf and Smoldyn_doc2.pdf.
 Copyright 2003-2011 by Steven Andrews.  This work is distributed under the terms
 of the Gnu General Public License (GPL). */

#include <string.h>
#include "opengl2.h"
#include "string2.h"
#include "smoldyn.h"
#include "smoldynfuncs.h"
#include "libsmoldyn.h"

#define LCHECK(A,B,C,D) if(!(A)) {smolSetError(B,C,D);if(C<ECwarning) goto failure;} else (void)0
#define LCHECKNT(A,B,C,D) if(!(A)) {smolSetErrorNT(B,C,D);if(C<ECwarning) goto failure;} else (void)0

enum ErrorCode Liberrorcode=ECok;
enum ErrorCode Libwarncode=ECok;
char Liberrorfunction[STRCHAR]="";
char Liberrorstring[STRCHAR]="";
int Libdebugmode=0;
int LibThrowThreshold=0;


/******************************************************************************/
/******************************* Miscellaneous ********************************/
/******************************************************************************/


/* smolGetVersion */
double smolGetVersion(void) {
	return simversionnumber(); }


/******************************************************************************/
/*********************************** Errors ***********************************/
/******************************************************************************/

/* smolSetLogging */
void smolSetLogging(FILE *logfile,void (*logFunction)(simptr,int,const char*, ...)) {
	simSetLogging(logfile,logFunction);
	return; }


/* smolSetThrowing */
smolSetThrowing(int corethreshold,int libthreshold) {
	simSetThrowing(corethreshold);
	LibThrowThreshold=libthreshold;
	return; }


/* smolSetError */
void smolSetError(const char *errorfunction,enum ErrorCode errorcode,const char *errorstring) {
	char string[STRCHAR];
	int severity;

	if(errorcode!=ECsame) {
		Liberrorcode=errorcode;
		Libwarncode=(errorcode>=ECwarning)?errorcode:ECok;
		if(errorstring)
			strncpy(Liberrorstring,errorstring,STRCHAR);
		else Liberrorstring[0]='\0'; }
	if(errorfunction)
		strncpy(Liberrorfunction,errorfunction,STRCHAR);
	else Liberrorfunction[0]='\0';

	severity=-(int)errorcode;
	if(LibThrowThreshold<severity) throw;

	if(Libdebugmode && Liberrorfunction[0]!='\0') {
		if(Liberrorcode==ECnotify)
			fprintf(stderr,"Libsmoldyn notification from %s: %s\n",Liberrorfunction,Liberrorstring);
		else if(Liberrorcode==ECwarning)
			fprintf(stderr,"Libsmoldyn warning in %s: %s\n",Liberrorfunction,Liberrorstring);
		else
			fprintf(stderr,"Libsmoldyn '%s' error in %s: %s\n",smolErrorCodeToString(Liberrorcode,string),Liberrorfunction,Liberrorstring); }
	return; }


/* smolSetErrorNT */
void smolSetErrorNT(const char *errorfunction,enum ErrorCode errorcode,const char *errorstring) {
	char string[STRCHAR];

	if(errorcode!=ECsame) {
		Liberrorcode=errorcode;
		Libwarncode=(errorcode>=ECwarning)?errorcode:ECok;
		if(errorstring)
			strncpy(Liberrorstring,errorstring,STRCHAR);
		else Liberrorstring[0]='\0'; }
	if(errorfunction)
		strncpy(Liberrorfunction,errorfunction,STRCHAR);
	else Liberrorfunction[0]='\0';
	return; }


/* smolGetError */
enum ErrorCode smolGetError(char *errorfunction,char *errorstring,int clearerror) {
	enum ErrorCode erc;

	erc=Liberrorcode;
	if(errorfunction) strcpy(errorfunction,Liberrorfunction);
	if(errorstring) strcpy(errorstring,Liberrorstring);
	if(clearerror) smolClearError();
	return erc; }


/* smolClearError */
void smolClearError(void) {
	if(Libdebugmode && Liberrorcode!=ECok)
		fprintf(stderr,"  Libsmoldyn error cleared\n");
	Liberrorcode=ECok;
	Libwarncode=ECok;
	Liberrorfunction[0]='\0';
	Liberrorstring[0]='\0';
	return; }


/* smolSetDebugMode */
void smolSetDebugMode(int debugmode) {
	Libdebugmode=debugmode;
	return; }


/* smolErrorCodeToString */
char *smolErrorCodeToString(enum ErrorCode erc,char *string) {
	if(erc==ECok) strcpy(string,"ok");
	else if(erc==ECnotify) strcpy(string,"notify");
	else if(erc==ECwarning) strcpy(string,"warning");
	else if(erc==ECnonexist) strcpy(string,"nonexistant");
	else if(erc==ECall) strcpy(string,"all");
	else if(erc==ECmissing) strcpy(string,"missing");
	else if(erc==ECbounds) strcpy(string,"bounds");
	else if(erc==ECsyntax) strcpy(string,"syntax");
	else if(erc==ECerror) strcpy(string,"error");
	else if(erc==ECmemory) strcpy(string,"memory");
	else if(erc==ECbug) strcpy(string,"Smoldyn bug");
	else if(erc==ECsame) strcpy(string,"same as before");
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
	return Liberrorcode; }


/* smolRunTimeStep */
enum ErrorCode smolRunTimeStep(simptr sim) {
	const char *funcname="smolRunTimeStep";
	int er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	simsettime(sim,sim->time+sim->dt/2,4);
	er=smolsimulate(sim);
	LCHECK(er!=1,funcname,ECnotify,"Simulation complete");
	LCHECK(er!=2,funcname,ECerror,"Simulation terminated during molecule assignment\n  Out of memory");
	LCHECK(er!=3,funcname,ECerror,"Simulation terminated during order 0 reaction\n  Not enough molecules allocated");
	LCHECK(er!=4,funcname,ECerror,"Simulation terminated during order 1 reaction\n  Not enough molecules allocated");
	LCHECK(er!=5,funcname,ECerror,"Simulation terminated during order 2 reaction\n  Not enough molecules allocated");
	LCHECK(er!=6,funcname,ECerror,"Simulation terminated during molecule sorting\n  Out of memory");
	LCHECK(er!=7,funcname,ECnotify,"Simulation stopped by a runtime command");
	LCHECK(er!=8,funcname,ECerror,"Simulation terminated during simulation state updating\n  Out of memory");
	LCHECK(er!=9,funcname,ECerror,"Simulation terminated during diffusion\n  Out of memory");
	return Libwarncode;
 failure:
	return Liberrorcode; }


/* smolRunSim */
enum ErrorCode smolRunSim(simptr sim) {
	const char *funcname="smolRunSim";
	int er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	if(sim->graphss && sim->graphss->graphics>0)
		smolsimulategl(sim);
	else {
		er=smolsimulate(sim);
		LCHECK(er!=1,funcname,ECnotify,"Simulation complete");
		LCHECK(er!=2,funcname,ECerror,"Simulation terminated during molecule assignment\n  Out of memory");
		LCHECK(er!=3,funcname,ECerror,"Simulation terminated during order 0 reaction\n  Not enough molecules allocated");
		LCHECK(er!=4,funcname,ECerror,"Simulation terminated during order 1 reaction\n  Not enough molecules allocated");
		LCHECK(er!=5,funcname,ECerror,"Simulation terminated during order 2 reaction\n  Not enough molecules allocated");
		LCHECK(er!=6,funcname,ECerror,"Simulation terminated during molecule sorting\n  Out of memory");
		LCHECK(er!=7,funcname,ECnotify,"Simulation stopped by a runtime command");
		LCHECK(er!=8,funcname,ECerror,"Simulation terminated during simulation state updating\n  Out of memory");
		LCHECK(er!=9,funcname,ECerror,"Simulation terminated during diffusion\n  Out of memory"); }
	return Libwarncode;
 failure:
	return Liberrorcode; }


/* smolRunSimUntil */
enum ErrorCode smolRunSimUntil(simptr sim,double breaktime) {
	const char *funcname="smolRunSimUntil";
	int er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	simsettime(sim,breaktime,4);
	er=smolsimulate(sim);

	LCHECK(er!=1,funcname,ECnotify,"Simulation complete");
	LCHECK(er!=2,funcname,ECerror,"Simulation terminated during molecule assignment\n  Out of memory");
	LCHECK(er!=3,funcname,ECerror,"Simulation terminated during order 0 reaction\n  Not enough molecules allocated");
	LCHECK(er!=4,funcname,ECerror,"Simulation terminated during order 1 reaction\n  Not enough molecules allocated");
	LCHECK(er!=5,funcname,ECerror,"Simulation terminated during order 2 reaction\n  Not enough molecules allocated");
	LCHECK(er!=6,funcname,ECerror,"Simulation terminated during molecule sorting\n  Out of memory");
	LCHECK(er!=7,funcname,ECnotify,"Simulation stopped by a runtime command");
	LCHECK(er!=8,funcname,ECerror,"Simulation terminated during simulation state updating\n  Out of memory");
	LCHECK(er!=9,funcname,ECerror,"Simulation terminated during diffusion\n  Out of memory");
	return Libwarncode;
 failure:
	return Liberrorcode; }


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
	er=simInitAndLoad(filepath,filename,&sim,flags);
	LCHECK(!er,funcname,ECerror,"Failed to initialize and load simulation");
	er=simUpdateAndDisplay(sim);
	LCHECK(!er,funcname,ECerror,"Failed to update simulation");
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
	return Liberrorcode; }


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
	return Liberrorcode; }


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
	return Liberrorcode; }


/* smolSetTimeStart */
enum ErrorCode smolSetTimeStart(simptr sim,double timestart) {
	const char *funcname="smolSetTimeStart";

	LCHECK(sim,funcname,ECmissing,"missing sim");
	simsettime(sim,timestart,1);
	return ECok;
 failure:
	return Liberrorcode; }


/* smolSetTimeStop */
enum ErrorCode smolSetTimeStop(simptr sim,double timestop) {
	const char *funcname="smolSetTimeStop";

	LCHECK(sim,funcname,ECmissing,"missing sim");
	simsettime(sim,timestop,2);
	return ECok;
 failure:
	return Liberrorcode; }


/* smolSetTimeNow */
enum ErrorCode smolSetTimeNow(simptr sim,double timenow) {
	const char *funcname="smolSetTimeNow";

	LCHECK(sim,funcname,ECmissing,"missing sim");
	simsettime(sim,timenow,0);
	return ECok;
 failure:
	return Liberrorcode; }


/* smolSetTimeStep */
enum ErrorCode smolSetTimeStep(simptr sim,double timestep) {
	const char *funcname="smolSetTimeStep";

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(timestep>0,funcname,ECbounds,"timestep is not > 0");
	simsettime(sim,timestep,3);
	return ECok;
 failure:
	return Liberrorcode; }


/* smolSetRandomSeed */
enum ErrorCode smolSetRandomSeed(simptr sim,long int seed) {
	const char *funcname="smolSetRandomSeed";

	LCHECK(sim,funcname,ECmissing,"missing sim");
	Simsetrandseed(sim,seed);
	return ECok;
 failure:
	return Liberrorcode; }


/* smolSetPartitions */
enum ErrorCode smolSetPartitions(simptr sim,char *method,double value) {
	const char *funcname="smolSetPartitions";
	int er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(method,funcname,ECmissing,"missing method string");
	LCHECK(value>0,funcname,ECbounds,"value needs to be > 0");
	er=boxsetsize(sim,method,value);
	LCHECK(er!=1,funcname,ECmemory,"out of memory");
	LCHECK(er!=2,funcname,ECsyntax,"method is not recognized");
	return ECok;
 failure:
	return Liberrorcode; }


/******************************************************************************/
/********************************** Graphics **********************************/
/******************************************************************************/

/* smolSetGraphicsParams */
enum ErrorCode smolSetGraphicsParams(simptr sim,char *method,int timesteps,double delay) {
	const char *funcname="smolSetGraphicsParams";
	int er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	er=graphicsenablegraphics(sim,method);
	LCHECK(er!=1,funcname,ECmemory,"out of memory");
	LCHECK(er!=2,funcname,ECmissing,"missing sim");
	LCHECK(er!=3,funcname,ECsyntax,"graphics method not recognized");
	if(timesteps>0) {
		er=graphicssetiter(sim,timesteps);
		LCHECK(er!=1,funcname,ECmemory,"out of memory enabling graphics");
		LCHECK(er!=2,funcname,ECbug,"BUG: missing parameter");
		LCHECK(er!=3,funcname,ECbug,"BUG: timesteps needs to be >=1"); }
	if(delay>=0) {
		er=graphicssetdelay(sim,delay);
		LCHECK(er!=1,funcname,ECmemory,"out of memory enabling graphics");
		LCHECK(er!=2,funcname,ECbug,"BUG: missing parameter");
		LCHECK(er!=3,funcname,ECbug,"BUG: delay needs to be >=0"); }
	return ECok;
 failure:
	return Liberrorcode; }


/* smolSetTiffParams */
enum ErrorCode smolSetTiffParams(simptr sim,int timesteps,const char *tiffname,int lowcount,int highcount) {
	const char *funcname="smolSetTiffParams";
	char nm1[STRCHAR];
	int er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	if(timesteps>0) {
		er=graphicssettiffiter(sim,timesteps);
		LCHECK(er!=1,funcname,ECmemory,"out of memory enabling graphics");
		LCHECK(er!=2,funcname,ECbug,"BUG: missing parameter");
		LCHECK(er!=3,funcname,ECbug,"BUG: timesteps needs to be >=1"); }
	if(tiffname) {
		strcpy(nm1,sim->filepath);
		strncat(nm1,tiffname,STRCHAR-1-strlen(nm1));
		gl2SetOptionStr("TiffName",nm1); }
	if(lowcount>=0) {
		gl2SetOptionInt("TiffNumber",lowcount); }
	if(highcount>=0) {
		gl2SetOptionInt("TiffNumMax",highcount); }
	return ECok;
 failure:
	return Liberrorcode; }


/* smolSetLightParams */
enum ErrorCode smolSetLightParams(simptr sim,int lightindex,double *ambient,double *diffuse,double *specular,double *position) {
	const char *funcname="smolSetLightParams";
	int c,er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(lightindex>=-1 && lightindex<MAXLIGHTS,funcname,ECbounds,"lightindex out of bounds");
	LCHECK(lightindex>=0 || (!diffuse && !specular && !position),funcname,ECsyntax,"can only set ambient for global light");

	if(ambient) {
		for(c=0;c<4;c++)
			LCHECK(ambient[c]>=0 && ambient[c]<=1,funcname,ECbounds,"ambient light value out of bounds");
		er=graphicssetlight(sim,NULL,lightindex,LPambient,ambient);
		LCHECK(!er,funcname,ECmemory,"out of memory enabling graphics"); }
	if(diffuse) {
		for(c=0;c<4;c++)
			LCHECK(diffuse[c]>=0 && diffuse[c]<=1,funcname,ECbounds,"diffuse light value out of bounds");
		er=graphicssetlight(sim,NULL,lightindex,LPdiffuse,diffuse);
		LCHECK(!er,funcname,ECmemory,"out of memory enabling graphics"); }
	if(specular) {
		for(c=0;c<4;c++)
			LCHECK(specular[c]>=0 && specular[c]<=1,funcname,ECbounds,"specular light value out of bounds");
		er=graphicssetlight(sim,NULL,lightindex,LPspecular,specular);
		LCHECK(!er,funcname,ECmemory,"out of memory enabling graphics"); }
	if(position) {
		er=graphicssetlight(sim,NULL,lightindex,LPposition,position);
		LCHECK(!er,funcname,ECmemory,"out of memory enabling graphics"); }

	return ECok;
 failure:
	return Liberrorcode; }


/* smolSetBackgroundStyle */
enum ErrorCode smolSetBackgroundStyle(simptr sim,double *color) {
	const char *funcname="smolSetBackgroundStyle";
	int c,er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	if(color) {
		for(c=0;c<4;c++)
			LCHECK(color[c]>=0 && color[c]<=1,funcname,ECbounds,"color value out of bounds");
		er=graphicssetbackcolor(sim,color);
		LCHECK(!er,funcname,ECmemory,"out of memory enabling graphics"); }
	return ECok;
 failure:
	return Liberrorcode; }


/* smolSetFrameStyle */
enum ErrorCode smolSetFrameStyle(simptr sim,double thickness,double *color) {
	const char *funcname="smolSetFrameStyle";
	int c,er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	if(thickness>=0) {
		er=graphicssetframethickness(sim,thickness);
		LCHECK(!er,funcname,ECmemory,"out of memory enabling graphics"); }
	if(color) {
		for(c=0;c<4;c++)
			LCHECK(color[c]>=0 && color[c]<=1,funcname,ECbounds,"color value out of bounds");
		er=graphicssetframecolor(sim,color);
		LCHECK(!er,funcname,ECmemory,"out of memory enabling graphics"); }
	return ECok;
 failure:
	return Liberrorcode; }


/* smolSetGridStyle */
enum ErrorCode smolSetGridStyle(simptr sim,double thickness,double *color) {
	const char *funcname="smolSetGridStyle";
	int c,er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	if(thickness>=0) {
		er=graphicssetgridthickness(sim,thickness);
		LCHECK(!er,funcname,ECmemory,"out of memory enabling graphics"); }
	if(color) {
		for(c=0;c<4;c++)
			LCHECK(color[c]>=0 && color[c]<=1,funcname,ECbounds,"color value out of bounds");
		er=graphicssetgridcolor(sim,color);
		LCHECK(!er,funcname,ECmemory,"out of memory enabling graphics"); }
	return ECok;
 failure:
	return Liberrorcode; }


/* smolSetTextStyle */
enum ErrorCode smolSetTextStyle(simptr sim,double *color) {
	const char *funcname="smolSetTextStyle";
	int c,er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	if(color) {
		for(c=0;c<4;c++)
			LCHECK(color[c]>=0 && color[c]<=1,funcname,ECbounds,"color value out of bounds");
		er=graphicssettextcolor(sim,color);
		LCHECK(!er,funcname,ECmemory,"out of memory enabling graphics"); }
	return ECok;
 failure:
	return Liberrorcode; }


/* smolAddTextDisplay */
enum ErrorCode smolAddTextDisplay(simptr sim,char *item) {
	const char *funcname="smolAddTextDisplay";
	int er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	er=graphicssettextitem(sim,item);
	LCHECK(er!=1,funcname,ECmemory,"out of memory adding text display item");
	LCHECK(er!=2,funcname,ECsyntax,"listed item is not recognized or not supported");
	LCHECK(er!=3,funcname,ECwarning,"text display item was already listed");
	return Libwarncode;
 failure:
	return Liberrorcode; }


/******************************************************************************/
/***************************** Runtime commands *******************************/
/******************************************************************************/

/* smolSetOutputPath */
enum ErrorCode smolSetOutputPath(simptr sim,const char *path) {
	const char *funcname="smolSetOutputPath";
	int er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(path,funcname,ECmissing,"missing path");
	er=scmdsetfroot((cmdssptr) sim->cmds,path);
	LCHECK(!er,funcname,ECbug,"scmdsetfroot bug");
	return ECok;
 failure:
	return Liberrorcode; }


/* smolAddOutputFile */
enum ErrorCode smolAddOutputFile(simptr sim,char *filename,int suffix,int append) {
	const char *funcname="smolSetOutputFile";
	int er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(filename,funcname,ECmissing,"missing filename");
	LCHECK(!strchr(filename,' '),funcname,ECwarning,"only first word of filename is used");
	er=scmdsetfnames((cmdssptr) sim->cmds,filename,append);
	LCHECK(!er,funcname,ECmemory,"allocating filename");
	if(suffix>=0) {
		er=scmdsetfsuffix((cmdssptr) sim->cmds,filename,suffix);
		LCHECK(!er,funcname,ECbug,"scmdsetfsuffix bug"); }

	return Libwarncode;
 failure:
	return Liberrorcode; }


/* smolAddCommand */
enum ErrorCode smolAddCommand(simptr sim,char type,double on,double off,double step,double multiplier,const char *commandstring) {
	const char *funcname="smolSetCommand";
	int er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	er=scmdaddcommand((cmdssptr) sim->cmds,type,sim->tmin,sim->tmax,sim->dt,on,off,step,multiplier,commandstring);
	LCHECK(er!=1,funcname,ECmemory,"out of memory creating command");
	LCHECK(er!=2,funcname,ECbug,"missing sim->cmds");
	LCHECK(er!=5,funcname,ECbounds,"step needs to be >0");
	LCHECK(er!=6,funcname,ECsyntax,"command type is not recognized");
	LCHECK(er!=7,funcname,ECmemory,"out of memory adding command to queue");
	LCHECK(er!=8,funcname,ECbounds,"multiplier needs to be >1");
	return ECok;
 failure:
	return Liberrorcode; }


/* smolAddCommandFromString */
enum ErrorCode smolAddCommandFromString(simptr sim,char *string) {
	const char *funcname="smolSetCommandFromString";
	int er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(string,funcname,ECmissing,"missing string");
	er=scmdstr2cmd((cmdssptr) sim->cmds,string,sim->tmin,sim->tmax,sim->dt);
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
	return Liberrorcode; }

	
/******************************************************************************/
/********************************* Molecules **********************************/
/******************************************************************************/

/* smolAddSpecies */
enum ErrorCode smolAddSpecies(simptr sim,const char *species,const char *mollist) {
	const char *funcname="smolAddSpecies";
	int i,ll;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(species,funcname,ECmissing,"missing species");
	if(mollist && mollist[0]!='\0') {
		ll=smolGetMolListIndexNT(sim,mollist);
		LCHECK(ll>=0,funcname,ECsame,NULL);
		LCHECK(sim->mols->listtype[ll]==MLTsystem,funcname,ECsyntax,"mollist is not a system list"); }
	else ll=-1;
	i=moladdspecies(sim,species);
	LCHECK(i!=-2,funcname,ECbug,"add species bug");
	LCHECK(i!=-3,funcname,ECbug,"more species are entered than are automatically allocated");
	LCHECK(i!=-4,funcname,ECsyntax,"'empty' is not a permitted species name");
	LCHECK(i!=-5,funcname,ECwarning,"this species has already been declared");
	LCHECK(i!=-6,funcname,ECsyntax,"'?' and '*' are not allowed in species names");
	if(mollist && mollist[0]!='\0')
		molsetlistlookup(sim,i,MSall,ll);
	return Libwarncode;
 failure:
	return Liberrorcode; }


/* smolGetSpeciesIndex */
int smolGetSpeciesIndex(simptr sim,const char *species) {
	const char *funcname="smolGetSpeciesIndex";
	int i;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(species,funcname,ECmissing,"missing species name");
	LCHECK(sim->mols,funcname,ECnonexist,"no species defined");
	LCHECK(strcmp(species,"all"),funcname,ECall,"species is 'all'");
	i=stringfind(sim->mols->spname,sim->mols->nspecies,species);
	LCHECK(i>0,funcname,ECnonexist,"species not found");
	return i;
 failure:
	return (int)Liberrorcode; }


/* smolGetSpeciesIndexNT */
int smolGetSpeciesIndexNT(simptr sim,const char *species) {
	const char *funcname="smolGetSpeciesIndexNT";
	int i;

	LCHECKNT(sim,funcname,ECmissing,"missing sim");
	LCHECKNT(species,funcname,ECmissing,"missing species name");
	LCHECKNT(sim->mols,funcname,ECnonexist,"no species defined");
	LCHECKNT(strcmp(species,"all"),funcname,ECall,"species cannot be 'all'");
	i=stringfind(sim->mols->spname,sim->mols->nspecies,species);
	LCHECKNT(i>0,funcname,ECnonexist,"species not found");
	return i;
 failure:
	return (int)Liberrorcode; }


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
	i=smolGetSpeciesIndexNT(sim,species);
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
	return Liberrorcode; }


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
	return Libwarncode;
 failure:
	return Liberrorcode; }


/* smolGetMolListIndex */
int smolGetMolListIndex(simptr sim,const char *mollist) {
	const char *funcname="smolGetMolListIndex";
	int ll;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(mollist,funcname,ECmissing,"missing mollist");
	LCHECK(sim->mols,funcname,ECnonexist,"no molecule lists defined");
	LCHECK(strcmp(mollist,"all"),funcname,ECall,"molecule list is 'all'");
	ll=stringfind(sim->mols->listname,sim->mols->nlist,mollist);
	LCHECK(ll>=0,funcname,ECnonexist,"list name not recognized");
	return ll;
 failure:
	return (int)Liberrorcode; }


/* smolGetMolListIndexNT */
int smolGetMolListIndexNT(simptr sim,const char *mollist) {
	const char *funcname="smolGetMolListIndexNT";
	int ll;

	LCHECKNT(sim,funcname,ECmissing,"missing sim");
	LCHECKNT(mollist,funcname,ECmissing,"missing mollist");
	LCHECKNT(sim->mols,funcname,ECnonexist,"no molecule lists defined");
	LCHECKNT(strcmp(mollist,"all"),funcname,ECall,"molecule list cannot be 'all'");
	ll=stringfind(sim->mols->listname,sim->mols->nlist,mollist);
	LCHECKNT(ll>=0,funcname,ECnonexist,"list name not recognized");
	return ll;
 failure:
	return (int)Liberrorcode; }


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
	i=smolGetSpeciesIndexNT(sim,species);
	if(i==(int)ECall) smolClearError();
	else LCHECK(i>0,funcname,ECsame,NULL);
	LCHECK((state>=0 && state<MSMAX) || state==MSall,funcname,ECsyntax,"invalid state");
	ll=smolGetMolListIndexNT(sim,mollist);
	LCHECK(ll>=0,funcname,ECsame,NULL);
	LCHECK(sim->mols->listtype[ll]==MLTsystem,funcname,ECerror,"list is not a system list");
	molsetlistlookup(sim,i,state,ll);
	return ECok;
 failure:
	return Liberrorcode; }


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
	return Liberrorcode; }


/* smolAddSolutionMolecules */
enum ErrorCode smolAddSolutionMolecules(simptr sim,const char *species,int number,double *lowposition,double *highposition) {
	const char *funcname="smolAddSolutionMolecules";
	int er,i,d;
	double *low,*high,lowpos[3],highpos[3];

	LCHECK(sim,funcname,ECmissing,"missing sim");
	i=smolGetSpeciesIndexNT(sim,species);
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
	return Liberrorcode; }


/* smolAddCompartmentMolecules */
enum ErrorCode smolAddCompartmentMolecules(simptr sim,const char *species,int number,const char *compartment) {
	const char *funcname="smolAddCompartmentMolecules";
	int i,er,c;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	i=smolGetSpeciesIndexNT(sim,species);
	LCHECK(i>0,funcname,ECsame,NULL);
	LCHECK(number>=0,funcname,ECbounds,"number < 0");
	c=smolGetCompartmentIndexNT(sim,compartment);
	LCHECK(c>=0,funcname,ECsame,NULL);
	er=addcompartmol(sim,number,i,sim->cmptss->cmptlist[c]);
	LCHECK(er!=2,funcname,ECerror,"compartment volume is zero or nearly zero");
	LCHECK(er!=3,funcname,ECmemory,"out of memory adding molecules");
	return ECok;
 failure:
	return Liberrorcode; }


/* smolAddSurfaceMolecules */
enum ErrorCode smolAddSurfaceMolecules(simptr sim,const char *species,enum MolecState state,int number,const char *surface,enum PanelShape panelshape,const char *panel,double *position) {
	const char *funcname="smolAddSurfaceMolecules";
	int i,s,p,er;
	panelptr pnl;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	i=smolGetSpeciesIndexNT(sim,species);
	LCHECK(i>0,funcname,ECsame,NULL);
	LCHECK(state>=0 && state<MSMAX,funcname,ECsyntax,"invalid state");
	LCHECK(number>=0,funcname,ECbounds,"number < 0");
	s=smolGetSurfaceIndexNT(sim,surface);
	if(s==(int)ECall) smolClearError();
	else LCHECK(s>=0,funcname,ECsame,NULL);
	LCHECK((panelshape>=0 && panelshape<PSMAX) || panelshape==PSall,funcname,ECnonexist,"invalid panelshape");
	pnl=NULL;
	p=smolGetPanelIndexNT(sim,surface,NULL,panel);
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
	return Liberrorcode; }


/* smolGetMoleculeCount */
int smolGetMoleculeCount(simptr sim,const char *species,enum MolecState state) {
	const char *funcname="smolGetMoleculeCount";
	int i;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	i=smolGetSpeciesIndexNT(sim,species);
	if(i==(int)ECall) {i=-5;smolClearError();}
	else LCHECK(i>0,funcname,ECsame,NULL);
	return molcount(sim,i,state,NULL,-1);
 failure:
	return (int)Liberrorcode; }


/* smolSetMoleculeStyle */
enum ErrorCode smolSetMoleculeStyle(simptr sim,const char *species,enum MolecState state,double size,double *color) {
	const char *funcname="smolSetTextStyle";
	int i,c;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	i=smolGetSpeciesIndexNT(sim,species);
	if(i==(int)ECall) smolClearError();
	else LCHECK(i>0,funcname,ECsame,NULL);
	LCHECK((state>=0 && state<MSMAX) || state==MSall,funcname,ECsyntax,"invalid state");

	if(size>0) molsetdisplaysize(sim,i,state,size);
	
	if(color) {
		for(c=0;c<3;c++)
			LCHECK(color[c]>=0 && color[c]<=1,funcname,ECbounds,"color value out of bounds");
		molsetcolor(sim,i,state,color); }
	return ECok;
failure:
	return Liberrorcode; }


/******************************************************************************/
/*********************************** Surfaces *********************************/
/******************************************************************************/

/* smolSetBoundaryType */
enum ErrorCode smolSetBoundaryType(simptr sim,int dimension,int highside,char type) {
	const char *funcname="smolSetBoundaryType";
	int er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(dimension<sim->dim,funcname,ECbounds,"dimension cannot exceed system dimensionality");
	LCHECK(highside<=1,funcname,ECbounds,"highside must be -1, 0, or 1");
	LCHECK(type=='r' || type=='p' || type=='a' || type=='t',funcname,ECsyntax,"invalid type");
	er=wallsettype(sim,dimension,highside,type);
	LCHECK(!er,funcname,ECbug,"bug in wallsettype");
	return ECok;
 failure:
	return Liberrorcode; }


/* smolAddSurface */
enum ErrorCode smolAddSurface(simptr sim,const char *surface) {
	const char *funcname="smolAddSurface";
	int s;
	surfaceptr srf;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	s=smolGetSurfaceIndexNT(sim,surface);
	if(s==(int)ECnonexist) smolClearError();
	else if(s<0) LCHECK(0,funcname,ECsame,NULL);
	else LCHECK(0,funcname,ECerror,"surface is already in system");
	srf=surfaddsurface(sim,surface);
	LCHECK(srf,funcname,ECmemory,"out of memory adding surface");
	return ECok;
 failure:
	return Liberrorcode; }


/* smolGetSurfaceIndex */
int smolGetSurfaceIndex(simptr sim,const char *surface) {
	const char *funcname="smolGetSurfaceIndex";
	int s;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(surface,funcname,ECmissing,"missing surface");
	LCHECK(sim->srfss && sim->srfss->nsrf,funcname,ECnonexist,"no surfaces defined");
	LCHECK(strcmp(surface,"all"),funcname,ECall,"surface cannot be 'all'");
	s=stringfind(sim->srfss->snames,sim->srfss->nsrf,surface);
	LCHECK(s>=0,funcname,ECnonexist,"surface not found");
	return s;
 failure:
	return (int)Liberrorcode; }


/* smolGetSurfaceIndexNT */
int smolGetSurfaceIndexNT(simptr sim,const char *surface) {
	const char *funcname="smolGetSurfaceIndexNT";
	int s;

	LCHECKNT(sim,funcname,ECmissing,"missing sim");
	LCHECKNT(surface,funcname,ECmissing,"missing surface");
	LCHECKNT(sim->srfss && sim->srfss->nsrf,funcname,ECnonexist,"no surfaces defined");
	LCHECKNT(strcmp(surface,"all"),funcname,ECall,"surface cannot be 'all'");
	s=stringfind(sim->srfss->snames,sim->srfss->nsrf,surface);
	LCHECKNT(s>=0,funcname,ECnonexist,"surface not found");
	return s;
 failure:
	return (int)Liberrorcode; }


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
	s=smolGetSurfaceIndexNT(sim,surface);
	if(s==(int)ECall) smolClearError();
	else LCHECK(s>=0,funcname,ECsame,NULL);
	LCHECK(face==PFfront || face==PFback || face==PFboth,funcname,ECbounds,"invalid face");
	i=smolGetSpeciesIndexNT(sim,species);
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
	return Liberrorcode; }


/* smolSetSurfaceRate */
enum ErrorCode smolSetSurfaceRate(simptr sim,const char *surface,const char *species,enum MolecState state,enum MolecState state1,enum MolecState state2,double rate,const char *newspecies,int isinternal) {
	const char *funcname="smolSetSurfaceRate";
	int er,i,i2,s;
	surfaceptr srf;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	s=smolGetSurfaceIndexNT(sim,surface);
	if(s==(int)ECall) smolClearError();
	else LCHECK(s>=0,funcname,ECsame,NULL);
	i=smolGetSpeciesIndexNT(sim,species);
	if(i==(int)ECall) smolClearError();
	else LCHECK(i>0,funcname,ECsame,NULL);
	LCHECK(state>=0 && state<MSMAX,funcname,ECbounds,"invalid state");
	LCHECK(state1>=0 && state1<MSMAX1,funcname,ECbounds,"invalid state1");
	LCHECK(state==MSsoln || state1==MSsoln || state1==MSbsoln || state1==state,funcname,ECsyntax,"nonsensical state combination");
	LCHECK(state2>=0 && state2<MSMAX1,funcname,ECbounds,"invalid state2");
	LCHECK(state1!=state2,funcname,ECsyntax,"cannot set rate for state1 = state2");
	if(newspecies && newspecies[0]!='\0') {
		i2=smolGetSpeciesIndexNT(sim,newspecies);
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
	return Liberrorcode; }


/* smolAddPanel */
enum ErrorCode smolAddPanel(simptr sim,const char *surface,enum PanelShape panelshape,const char *panel,const char *axisstring,double *params) {
	const char *funcname="smolAddPanel";
	int er,s;
	surfaceptr srf;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	s=smolGetSurfaceIndexNT(sim,surface);
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
	return Liberrorcode; }


/* smolGetPanelIndex */
int smolGetPanelIndex(simptr sim,const char *surface,enum PanelShape *panelshapeptr,const char *panel) {
	const char *funcname="smolGetPanelIndex";
	surfaceptr srf;
	int p,s;
	enum PanelShape ps;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	s=smolGetSurfaceIndexNT(sim,surface);
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
	return (int)Liberrorcode; }


/* smolGetPanelIndexNT */
int smolGetPanelIndexNT(simptr sim,const char *surface,enum PanelShape *panelshapeptr,const char *panel) {
	const char *funcname="smolGetPanelIndexNT";
	surfaceptr srf;
	int p,s;
	enum PanelShape ps;

	LCHECKNT(sim,funcname,ECmissing,"missing sim");
	s=smolGetSurfaceIndexNT(sim,surface);
	LCHECKNT(s>=0,funcname,ECsame,NULL);
	LCHECKNT(panel,funcname,ECmissing,"missing panel name");
	LCHECKNT(strcmp(panel,"all"),funcname,ECall,"panel cannot be 'all'");
	srf=sim->srfss->srflist[s];
	p=-1;
	for(ps=0;ps<PSMAX && p<0;ps++)
		p=stringfind(srf->pname[ps],srf->npanel[ps],panel);
	LCHECKNT(p>=0,funcname,ECnonexist,"panel not found");
	if(panelshapeptr) *panelshapeptr=ps;
	return p;
 failure:
	return (int)Liberrorcode; }


/* smolGetPanelName */
char *smolGetPanelName(simptr sim,const char *surface,enum PanelShape panelshape,int panelindex,char *panel) {
	const char *funcname="smolGetPanelName";
	surfaceptr srf;
	int s;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	s=smolGetSurfaceIndexNT(sim,surface);
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


/* smolSetPanelJump */
enum ErrorCode smolSetPanelJump(simptr sim,const char *surface,const char *panel1,enum PanelFace face1,const char *panel2,enum PanelFace face2,int isbidirectional) {
	const char *funcname="smolSetPanelJump";
	int s,p1,p2,er;
	surfaceptr srf;
	enum PanelShape ps1,ps2;
	panelptr pnl1,pnl2;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	s=smolGetSurfaceIndexNT(sim,surface);
	LCHECK(s>=0,funcname,ECsame,NULL);
	p1=smolGetPanelIndexNT(sim,surface,&ps1,panel1);
	LCHECK(p1>=0,funcname,ECsame,NULL);
	p2=smolGetPanelIndexNT(sim,surface,&ps2,panel2);
	LCHECK(p2>=0,funcname,ECsame,NULL);
	LCHECK(ps1==ps2,funcname,ECerror,"origin and destination jump panels need to have the same shape");
	LCHECK(p1!=p2,funcname,ECerror,"origin and destination jump panels cannot be the same panel");
	LCHECK(face1==PFfront || face1==PFback,funcname,ECsyntax,"jumping panel face has to be either front or back");
	LCHECK(face2==PFfront || face2==PFback,funcname,ECsyntax,"jumping panel face has to be either front or back");
	LCHECK(isbidirectional==0 || isbidirectional==1,funcname,ECsyntax,"bidirectional code has to be 0 or 1");

	srf=sim->srfss->srflist[s];
	pnl1=srf->panels[ps1][p1];
	pnl2=srf->panels[ps2][p2];
	er=surfsetjumppanel(srf,pnl1,face1,isbidirectional,pnl2,face2);
	LCHECK(!er,funcname,ECbug,"BUG: error code returned by surfsetjumppanel");
	return ECok;
 failure:
	return Liberrorcode; }


/* smolAddSurfaceUnboundedEmitter */
enum ErrorCode smolAddSurfaceUnboundedEmitter(simptr sim,const char *surface,enum PanelFace face,const char *species,double emitamount,double *emitposition) {
	const char *funcname="smolAddSurfaceUnboundedEmitter";
	int s,i,er;
	surfaceptr srf;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	s=smolGetSurfaceIndexNT(sim,surface);
	LCHECK(s>=0,funcname,ECsame,NULL);
	LCHECK(face==PFfront || face==PFback,funcname,ECsyntax,"jumping panel face has to be either front or back");
	i=smolGetSpeciesIndexNT(sim,species);
	LCHECK(i>0,funcname,ECsame,NULL);

	srf=sim->srfss->srflist[s];
	er=surfaddemitter(srf,face,i,emitamount,emitposition,sim->dim);
	LCHECK(!er,funcname,ECmemory,"out of memory allocating unbounded emitter");
	
	return ECok;
 failure:
	return Liberrorcode; }


/* smolSetSurfaceSimParams */
enum ErrorCode smolSetSurfaceSimParams(simptr sim,const char *parameter,double value) {
	const char *funcname="smolSetSurfaceSimParams";
	int er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(parameter,funcname,ECmissing,"missing parameter name");
	if(!strcmp(parameter,"epsilon")) {
		er=surfsetepsilon(sim,value);
		LCHECK(er!=2,funcname,ECmemory,"out of memory enabling surfaces");
		LCHECK(er!=3,funcname,ECbounds,"epsilon needs to be >0"); }
	else if(!strcmp(parameter,"margin")) {
		er=surfsetmargin(sim,value);
		LCHECK(er!=2,funcname,ECmemory,"out of memory enabling surfaces");
		LCHECK(er!=3,funcname,ECbounds,"margin needs to be >=0"); }
	else if(!strcmp(parameter,"neighbordist")) {
		er=surfsetneighdist(sim,value);
		LCHECK(er!=2,funcname,ECmemory,"out of memory enabling surfaces");
		LCHECK(er!=3,funcname,ECbounds,"neighbor distance needs to be >0"); }
	else
		LCHECK(0,funcname,ECsyntax,"parameter name not recognized");

	return ECok;
 failure:
	return Liberrorcode; }


/* smolAddPanelNeighbor */
enum ErrorCode smolAddPanelNeighbor(simptr sim,const char *surface1,const char *panel1,const char *surface2,const char *panel2,int reciprocal) {
	const char *funcname="smolAddPanelNeighbor";
	int s1,s2,p1,p2,er;
	panelptr pnl1,pnl2;
	enum PanelShape ps1,ps2;
	
	LCHECK(sim,funcname,ECmissing,"missing sim");
	s1=smolGetSurfaceIndexNT(sim,surface1);
	LCHECK(s1>=0,funcname,ECsame,NULL);
	s2=smolGetSurfaceIndexNT(sim,surface2);
	LCHECK(s2>=0,funcname,ECsame,NULL);
	p1=smolGetPanelIndexNT(sim,surface1,&ps1,panel1);
	LCHECK(p1>=0,funcname,ECsame,NULL);
	p2=smolGetPanelIndexNT(sim,surface2,&ps2,panel2);
	LCHECK(p2>=0,funcname,ECsame,NULL);
	LCHECK(!(s1==s2 && p1==p2),funcname,ECerror,"panels cannot be their own neighbors");

	pnl1=sim->srfss->srflist[s1]->panels[ps1][p1];
	pnl2=sim->srfss->srflist[s2]->panels[ps2][p2];
	er=surfsetneighbors(pnl1,&pnl2,1,1);
	LCHECK(!er,funcname,ECmemory,"out of memory adding panel neighbor");
	if(reciprocal) {
		er=surfsetneighbors(pnl2,&pnl1,1,1);
		LCHECK(!er,funcname,ECmemory,"out of memory adding panel neighbor"); }
	
	return ECok;
failure:
	return Liberrorcode; }


/* smolSetSurfaceStyle */
enum ErrorCode smolSetSurfaceStyle(simptr sim,const char *surface,enum PanelFace face,enum DrawMode mode,double thickness,double *color,int stipplefactor,int stipplepattern,double shininess) {
	const char *funcname="smolSetSurfaceFaceStyle";
	int s,c,er;
	surfaceptr srf;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	s=smolGetSurfaceIndexNT(sim,surface);
	if(s==(int)ECall) smolClearError();
	else LCHECK(s>=0,funcname,ECsame,NULL);
	srf=sim->srfss->srflist[s];

	if(mode!=DMnone) {
		LCHECK(mode>=0 && mode<DMnone,funcname,ECsyntax,"mode not recognized");
		er=surfsetdrawmode(srf,face,mode);
		LCHECK(!er,funcname,ECbug,"BUG: error in surfsetdrawmode"); }
	if(thickness>=0) {
		er=surfsetedgepts(srf,thickness);
		LCHECK(!er,funcname,ECbug,"BUG: error in surfsetedgepts"); }
	if(color) {
		for(c=0;c<4;c++)
			LCHECK(color[c]>=0 && color[c]<=1,funcname,ECbounds,"color value out of bounds");
		er=surfsetcolor(srf,face,color);
		LCHECK(!er,funcname,ECbug,"BUG: error in surfsetcolor"); }
	if(stipplefactor>=0) {
		LCHECK(stipplefactor>0,funcname,ECbounds,"stipplefactor needs to be >0");
		er=surfsetstipple(srf,stipplefactor,-1);
		LCHECK(!er,funcname,ECbug,"BUG: error in surfsetstipple"); }
	if(stipplepattern>=0) {
		LCHECK(stipplepattern<=0xFFFF,funcname,ECbounds,"stipplepattern needs to be between 0 and 0xFFFF");
		er=surfsetstipple(srf,-1,stipplepattern);
		LCHECK(!er,funcname,ECbug,"BUG: error in surfsetstipple"); }
	if(shininess>=0) {
		LCHECK(shininess<=128,funcname,ECbounds,"shininess cannot exceed 128");
		er=surfsetshiny(srf,face,shininess);
		LCHECK(!er,funcname,ECbug,"BUG: error in surfsetshiny"); }

	return ECok;
failure:
	return Liberrorcode; }


/******************************************************************************/
/******************************** Compartments ********************************/
/******************************************************************************/

/* smolAddCompartment */
enum ErrorCode smolAddCompartment(simptr sim,const char *compartment) {
	const char *funcname="smolAddCompartment";
	int c;
	compartptr cmpt;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	c=smolGetCompartmentIndexNT(sim,compartment);
	if(c==(int)ECnonexist) smolClearError();
	else if(c<0) LCHECK(0,funcname,ECsame,NULL);
	else LCHECK(0,funcname,ECerror,"compartment is already in system");
	cmpt=compartaddcompart(sim,compartment);
	LCHECK(cmpt,funcname,ECmemory,"out of memory adding compartment");
	return ECok;
 failure:
	return Liberrorcode; }


/* smolGetCompartmentIndex */
int smolGetCompartmentIndex(simptr sim,const char *compartment) {
	const char *funcname="smolGetCompartmentIndex";
	int c;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(compartment,funcname,ECmissing,"missing compartment");
	LCHECK(sim->cmptss && sim->cmptss->ncmpt,funcname,ECnonexist,"no compartments defined");
	LCHECK(strcmp(compartment,"all"),funcname,ECall,"compartment cannot be 'all'");
	c=stringfind(sim->cmptss->cnames,sim->cmptss->ncmpt,compartment);
	LCHECK(c>=0,funcname,ECnonexist,"compartment not found");
	return c;
 failure:
	return (int)Liberrorcode; }


/* smolGetCompartmentIndexNT */
int smolGetCompartmentIndexNT(simptr sim,const char *compartment) {
	const char *funcname="smolGetCompartmentIndexNT";
	int c;

	LCHECKNT(sim,funcname,ECmissing,"missing sim");
	LCHECKNT(compartment,funcname,ECmissing,"missing compartment");
	LCHECKNT(sim->cmptss && sim->cmptss->ncmpt,funcname,ECnonexist,"no compartments defined");
	LCHECKNT(strcmp(compartment,"all"),funcname,ECall,"compartment cannot be 'all'");
	c=stringfind(sim->cmptss->cnames,sim->cmptss->ncmpt,compartment);
	LCHECKNT(c>=0,funcname,ECnonexist,"compartment not found");
	return c;
 failure:
	return (int)Liberrorcode; }


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
	c=smolGetCompartmentIndexNT(sim,compartment);
	LCHECK(c>=0,funcname,ECsame,NULL);
	s=smolGetSurfaceIndexNT(sim,surface);
	LCHECK(s>=0,funcname,ECsame,NULL);
	er=compartaddsurf(sim->cmptss->cmptlist[c],sim->srfss->srflist[s]);
	LCHECK(!er,funcname,ECmemory,"out of memory in compartaddsurf");
	return ECok;
 failure:
	return Liberrorcode; }


/* smolAddCompartmentPoint */
enum ErrorCode smolAddCompartmentPoint(simptr sim,const char *compartment,double *point) {
	const char *funcname="smolAddCompartmentPoint";
	int c,er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	c=smolGetCompartmentIndexNT(sim,compartment);
	LCHECK(c>=0,funcname,ECsame,NULL);
	LCHECK(point,funcname,ECmissing,"missing point");
	er=compartaddpoint(sim->cmptss->cmptlist[c],sim->dim,point);
	LCHECK(!er,funcname,ECmemory,"out of memory in compartaddsurf");
	return ECok;
 failure:
	return Liberrorcode; }


/* smolAddCompartmentLogic */
enum ErrorCode smolAddCompartmentLogic(simptr sim,const char *compartment,enum CmptLogic logic,const char *compartment2) {
	const char *funcname="smolAddCompartmentLogic";
	int c,c2,er;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	c=smolGetCompartmentIndexNT(sim,compartment);
	LCHECK(c>=0,funcname,ECsame,NULL);
	LCHECK(logic>=CLequal && logic<CLnone,funcname,ECsyntax,"invalid logic operation");
	c2=smolGetCompartmentIndexNT(sim,compartment2);
	LCHECK(c2>=0,funcname,ECerror,"error with compartment2");
	er=compartaddcmptl(sim->cmptss->cmptlist[c],sim->cmptss->cmptlist[c2],logic);
	LCHECK(!er,funcname,ECmemory,"out of memory in compartaddcmpt");
	return ECok;
 failure:
	return Liberrorcode; }



/******************************************************************************/
/********************************* Reactions **********************************/
/******************************************************************************/

/* smolAddReaction */
enum ErrorCode smolAddReaction(simptr sim,const char *reaction,const char *reactant1,enum MolecState rstate1,const char *reactant2,enum MolecState rstate2,int nproduct,const char **productspecies,enum MolecState *productstates,double rate) {
	const char *funcname="smolAddReaction";
	rxnptr rxn;
	int order,prd,rctident[MAXORDER],prdident[MAXPRODUCT],er;
	enum MolecState rctstate[MAXORDER];

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(reaction,funcname,ECmissing,"missing reaction name");
	order=0;
	rctident[0]=rctident[1]=0;
	rctstate[0]=rctstate[1]=MSnone;
	if(reactant1 && reactant1[0]!='\0') {
		rctident[order]=smolGetSpeciesIndexNT(sim,reactant1);
		LCHECK(rctident[order]>0,funcname,ECsame,NULL);
		LCHECK(rstate1>=0 && rstate1<MSMAX,funcname,ECbounds,"invalid rstate1");
		rctstate[order]=rstate1;
		order++; }
	if(reactant2 && reactant2[0]!='\0') {
		rctident[order]=smolGetSpeciesIndexNT(sim,reactant2);
		LCHECK(rctident[order]>0,funcname,ECsame,NULL);
		LCHECK(rstate2>=0 && rstate2<MSMAX,funcname,ECbounds,"invalid rstate2");
		rctstate[order]=rstate2;
		order++; }
	LCHECK(nproduct>=0,funcname,ECbounds,"invalid nproduct");
	if(nproduct) {
		LCHECK(productspecies,funcname,ECmissing,"missing product species");
		LCHECK(productstates,funcname,ECmissing,"missing product states");
		for(prd=0;prd<nproduct;prd++) {
			prdident[prd]=smolGetSpeciesIndexNT(sim,productspecies[prd]);
			LCHECK(prdident[prd]>0,funcname,ECsame,NULL); }}
	rxn=RxnAddReaction(sim,reaction,order,rctident,rctstate,nproduct,prdident,productstates,NULL,NULL);
	LCHECK(rxn,funcname,ECmemory,"out of memory allocating reaction");

	if(rate>=0) {
		er=RxnSetValue(sim,"rate",rxn,rate);
		if(er==3) LCHECK(0,funcname,ECwarning,"rate was set previously");
		else LCHECK(!er,funcname,ECbug,"RxnSetValue error"); }

	return Libwarncode;
 failure:
	return Liberrorcode; }


/* smolGetReactionIndex */
int smolGetReactionIndex(simptr sim,int *orderptr,const char *reaction) {
	const char *funcname="smolGetReactionIndex";
	int order,r;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(reaction,funcname,ECmissing,"missing reaction");
	LCHECK(strcmp(reaction,"all"),funcname,ECall,"reaction cannot be 'all'");
	if(orderptr && *orderptr>=0 && *orderptr<MAXORDER) {
		order=*orderptr;
		LCHECK(sim->rxnss[order] && sim->rxnss[order]->totrxn,funcname,ECnonexist,"no reactions defined of this order");
		r=stringfind(sim->rxnss[order]->rname,sim->rxnss[order]->totrxn,reaction);
		LCHECK(r>=0,funcname,ECnonexist,"reaction not found"); }
	else {
		r=-1;
		for(order=0;order<MAXORDER && r<0;order++)
			if(sim->rxnss[order])
				r=stringfind(sim->rxnss[order]->rname,sim->rxnss[order]->totrxn,reaction);
		LCHECK(r>=0,funcname,ECnonexist,"reaction not found");
		if(orderptr) *orderptr=order-1; }
	return r;
 failure:
	return (int)Liberrorcode; }


/* smolGetReactionIndexNT */
int smolGetReactionIndexNT(simptr sim,int *orderptr,const char *reaction) {
	const char *funcname="smolGetReactionIndexNT";
	int order,r;

	LCHECKNT(sim,funcname,ECmissing,"missing sim");
	LCHECKNT(reaction,funcname,ECmissing,"missing reaction");
	LCHECKNT(strcmp(reaction,"all"),funcname,ECall,"reaction cannot be 'all'");
	if(orderptr && *orderptr>=0 && *orderptr<MAXORDER) {
		order=*orderptr;
		LCHECKNT(sim->rxnss[order] && sim->rxnss[order]->totrxn,funcname,ECnonexist,"no reactions defined of this order");
		r=stringfind(sim->rxnss[order]->rname,sim->rxnss[order]->totrxn,reaction);
		LCHECKNT(r>=0,funcname,ECnonexist,"reaction not found"); }
	else {
		r=-1;
		for(order=0;order<MAXORDER && r<0;order++)
			if(sim->rxnss[order])
				r=stringfind(sim->rxnss[order]->rname,sim->rxnss[order]->totrxn,reaction);
		LCHECKNT(r>=0,funcname,ECnonexist,"reaction not found");
		if(orderptr) *orderptr=order-1; }
	return r;
 failure:
	return (int)Liberrorcode; }


/* smolGetReactionName */
char *smolGetReactionName(simptr sim,int order,int reactionindex,char *reaction) {
	const char *funcname="smolGetReactionName";

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(order>=0 && order<MAXORDER,funcname,ECbounds,"invalid reaction order");
	LCHECK(reactionindex>=0,funcname,ECbounds,"invalid reaction name");
	LCHECK(reaction,funcname,ECmissing,"missing reaction");
	LCHECK(sim->rxnss[order] && sim->rxnss[order]->totrxn,funcname,ECnonexist,"no reactions defined of this order");
	LCHECK(reactionindex<sim->rxnss[order]->totrxn,funcname,ECnonexist,"reaction does not exist");
	strcpy(reaction,sim->rxnss[order]->rname[reactionindex]);
	return reaction;
failure:
	return NULL; }


/* smolSetReactionRate */
enum ErrorCode smolSetReactionRate(simptr sim,const char *reaction,double rate,int isinternal) {
	const char *funcname="smolSetReactionRate";
	int r,er,order;
	rxnptr rxn;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	order=-1;
	r=smolGetReactionIndexNT(sim,&order,reaction);
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
	return Libwarncode;
 failure:
	return Liberrorcode; }


/* smolSetReactionRegion */
enum ErrorCode smolSetReactionRegion(simptr sim,const char *reaction,const char *compartment,const char *surface) {
	const char *funcname="smolSetReactionRegion";
	int order,r,c,s;
	rxnptr rxn;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	order=-1;
	r=smolGetReactionIndexNT(sim,&order,reaction);
	LCHECK(r>=0,funcname,ECsame,NULL);
	rxn=sim->rxnss[order]->rxn[r];

	if(compartment && compartment[0]!='\0') {
		c=smolGetCompartmentIndexNT(sim,compartment);
		LCHECK(c>=0,funcname,ECsame,NULL);
		RxnSetCmpt(rxn,sim->cmptss->cmptlist[c]); }
	else if(compartment && compartment[0]=='\0')
		RxnSetCmpt(rxn,NULL);

	if(surface && surface[0]!='\0') {
		s=smolGetSurfaceIndexNT(sim,surface);
		LCHECK(s>=0,funcname,ECsame,NULL);
		RxnSetSurface(rxn,sim->srfss->srflist[s]); }
	else if(surface && surface[0]=='\0')
		RxnSetSurface(rxn,NULL);

	return ECok;
 failure:
	return Liberrorcode; }


/* smolSetReactionProducts */
enum ErrorCode smolSetReactionProducts(simptr sim,const char *reaction,enum RevParam method,double parameter,const char *product,double *position) {
	const char *funcname="smolSetReactionProducts";
	int order,r,done,prd,i,er;
	rxnptr rxn;
	
	LCHECK(sim,funcname,ECmissing,"missing sim");
	order=-1;
	r=smolGetReactionIndexNT(sim,&order,reaction);
	LCHECK(r>=0,funcname,ECsame,NULL);
	rxn=sim->rxnss[order]->rxn[r];

	prd=-1;
	if(product) {
		i=smolGetSpeciesIndexNT(sim,product);
		LCHECK(i>0,funcname,ECsame,NULL);
		done=0;
		for(prd=0;prd<rxn->nprod && !done;prd++)
			if(rxn->prdident[prd]==i) done=1;
		prd--;
		LCHECK(done,funcname,ECerror,"listed product is not a product of the listed reaction"); }

	er=RxnSetRevparam(sim,rxn,method,parameter,prd,position,sim->dim);
	LCHECK(er!=1,funcname,ECwarning,"reaction product parameter was set before");
	LCHECK(er!=2,funcname,ECbounds,"reaction product parameter out of bounds");
	LCHECK(er!=3,funcname,ECnonexist,"invalid reaction product method");
	LCHECK(er!=4,funcname,ECmissing,"missing product name");
	LCHECK(er!=5,funcname,ECmissing,"missing product position");

	return Libwarncode;
failure:
	return Liberrorcode; }



/******************************************************************************/
/*********************************** Ports ************************************/
/******************************************************************************/

/* smolAddPort */
enum ErrorCode smolAddPort(simptr sim,const char *port,const char *surface,enum PanelFace face) {
	const char *funcname="smolAddPort";
	int s;
	portptr simport;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(port,funcname,ECmissing,"missing port");
	s=smolGetSurfaceIndexNT(sim,surface);
	if(s==(int)ECmissing) smolClearError();
	else LCHECK(s>=0,funcname,ECsame,NULL);
	LCHECK(face=PFfront || face==PFback || PFnone,funcname,ECsyntax,"invalid face");
	simport=portaddport(sim,port,sim->srfss->srflist[s],face);
	LCHECK(simport,funcname,ECmemory,"out of memory adding port");

	return ECok;
 failure:
	return Liberrorcode; }


/* smolGetPortIndex */
int smolGetPortIndex(simptr sim,const char *port) {
	const char *funcname="smolGetPortIndex";
	int p;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	LCHECK(port,funcname,ECmissing,"missing port");
	LCHECK(sim->portss && sim->portss->nport,funcname,ECnonexist,"no ports defined");
	LCHECK(strcmp(port,"all"),funcname,ECall,"port cannot be 'all'");
	p=stringfind(sim->portss->portnames,sim->portss->nport,port);
	LCHECK(p>=0,funcname,ECnonexist,"port not found");
	return p;
 failure:
	return (int)Liberrorcode; }


/* smolGetPortIndexNT */
int smolGetPortIndexNT(simptr sim,const char *port) {
	const char *funcname="smolGetPortIndexNT";
	int p;

	LCHECKNT(sim,funcname,ECmissing,"missing sim");
	LCHECKNT(port,funcname,ECmissing,"missing port");
	LCHECKNT(sim->portss && sim->portss->nport,funcname,ECnonexist,"no ports defined");
	LCHECKNT(strcmp(port,"all"),funcname,ECall,"port cannot be 'all'");
	p=stringfind(sim->portss->portnames,sim->portss->nport,port);
	LCHECKNT(p>=0,funcname,ECnonexist,"port not found");
	return p;
 failure:
	return (int)Liberrorcode; }


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


/* smolAddPortMolecules */
enum ErrorCode smolAddPortMolecules(simptr sim,const char *port,int nmolec,const char *species,double **positions) {
	const char *funcname="smolAddPortMolecules";
	int prt,i,er;
	portptr simport;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	prt=smolGetPortIndexNT(sim,port);
	LCHECK(prt>=0,funcname,ECsame,NULL);
	simport=sim->portss->portlist[prt];
	if(nmolec==0) return ECok;
	LCHECK(nmolec>0,funcname,ECbounds,"nmolec cannot be negative");
	i=smolGetSpeciesIndexNT(sim,species);
	LCHECK(i>0,funcname,ECsame,NULL);
	er=portputmols(sim,simport,nmolec,i,NULL,positions);
	LCHECK(er!=1,funcname,ECmemory,"out of memory");
	LCHECK(er!=2,funcname,ECnonexist,"no porting surface defined");
	LCHECK(er!=3,funcname,ECnonexist,"no porting face defined");
	LCHECK(er!=4,funcname,ECnonexist,"no panels on porting surface");

	return ECok;
failure:
	return Liberrorcode; }


/* smolGetPortMolecules */
int smolGetPortMolecules(simptr sim,const char *port,const char *species,enum MolecState state,int remove) {
	const char *funcname="smolGetPortMolecules";
	int prt,i,num;
	portptr simport;

	LCHECK(sim,funcname,ECmissing,"missing sim");
	prt=smolGetPortIndexNT(sim,port);
	LCHECK(prt>=0,funcname,ECsame,NULL);
	simport=sim->portss->portlist[prt];

	i=smolGetSpeciesIndexNT(sim,species);
	if(i==(int)ECall) smolClearError();
	else LCHECK(i>0,funcname,ECsame,NULL);
	LCHECK((state>=0 && state<MSMAX) || state==MSall,funcname,ECsyntax,"invalid state");

	num=portgetmols(sim,simport,i,state,remove);
	return num;
failure:
	return (int)Liberrorcode; }
