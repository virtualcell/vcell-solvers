/* Steven Andrews, started 10/22/01.
This is a library of functions for the Smoldyn program.  See documentation
called Smoldyn_doc1.doc and Smoldyn_doc2.doc.
Copyright 2003-2007 by Steven Andrews.  This work is distributed under the terms
of the Gnu Lesser General Public License (LGPL). */

#include <stdio.h>
#include <math.h>
#include <string.h>
#include "smoldyn.h"
#include "string2.h"
#include "Zn.h"

#define CHECK(A) if(!(A)) goto failure; else (void)0
#define CHECKS(A,B) if(!(A)) {strncpy(erstr,B,STRCHAR-1);erstr[STRCHAR-1]='\0';goto failure;} else (void)0


/******************************************************************************/
/************************************* Ports **********************************/
/******************************************************************************/


/******************************************************************************/
/****************************** low level utilities ***************************/
/******************************************************************************/



/******************************************************************************/
/******************************* memory management ****************************/
/******************************************************************************/

/* portalloc.  Allocates memory for a port.  Pointers are set to NULL and llport
is set to -1.  Returns the port or NULL if unable to allocate memory. */
portptr portalloc(void) {
	portptr port;

	port=(portptr)malloc(sizeof(struct portstruct));
	if(!port) return NULL;
	port->portname=NULL;
	port->srf=NULL;
	port->face=PFnone;
	port->llport=-1;
	return port; }


/* portfree.  Frees a port. */
void portfree(portptr port) {
	if(!port) return;
	free(port);
	return; }


/* portssalloc.  Allocates a port superstructure as well as maxport ports.
Space is allocated and initialized for port names.  Returns the port
superstructure or NULL if unable to allocate memory. */
portssptr portssalloc(int maxport) {
	portssptr portss;
	int prt;

	portss=(portssptr)malloc(sizeof(struct portsuperstruct));
	if(!portss) return NULL;
	portss->condition=SCinit;
	portss->sim=NULL;
	portss->maxport=maxport;
	portss->nport=0;
	portss->portnames=NULL;
	portss->portlist=NULL;

	CHECK(portss->portnames=(char**)calloc(maxport,sizeof(char*)));
	for(prt=0;prt<maxport;prt++) portss->portnames[prt]=NULL;
	for(prt=0;prt<maxport;prt++) {
		CHECK(portss->portnames[prt]=EmptyString()); }

	CHECK(portss->portlist=(portptr*)calloc(maxport,sizeof(portptr)));
	for(prt=0;prt<maxport;prt++) portss->portlist[prt]=NULL;
	for(prt=0;prt<maxport;prt++) {
		CHECK(portss->portlist[prt]=portalloc());
		portss->portlist[prt]->portname=portss->portnames[prt]; }

	return portss;

 failure:
 	portssfree(portss);
 	return NULL; }


/* portssfree.  Frees a port superstructure, including all ports. */
void portssfree(portssptr portss) {
	int prt;

	if(!portss) return;
	if(portss->maxport&&portss->portlist)
		for(prt=0;prt<portss->maxport;prt++) portfree(portss->portlist[prt]);
	free(portss->portlist);
	if(portss->maxport&&portss->portnames)
		for(prt=0;prt<portss->maxport;prt++) free(portss->portnames[prt]);
	free(portss->portnames);
	free(portss);
	return; }


/******************************************************************************/
/***************************** data structure output **************************/
/******************************************************************************/

/* portoutput.  Displays all important information about all ports to stdout. */
void portoutput(simptr sim) {
	portssptr portss;
	portptr port;
	int prt;
	char string[STRCHAR];

	portss=sim->portss;
	if(!portss) return;
	printf("PORT PARAMETERS\n");
	printf(" Ports allocated: %i, ports defined: %i\n",portss->maxport,portss->nport);
	for(prt=0;prt<portss->nport;prt++) {
		port=portss->portlist[prt];
		printf(" Port: %s\n",portss->portnames[prt]);
		if(port->srf) printf("  surface: %s, %s\n",port->srf->sname,surfface2string(port->face,string));
		else printf("  no surface assigned\n");
		if(port->llport>=0) printf("  molecule list: %s\n",sim->mols->listname[port->llport]);
		else printf("  no molecule list assigned"); }
	printf("\n");
	return; }


/* writeports.  Prints information about all ports to file fptr using a format
that allows the ports to read as a configuration file. */
void writeports(simptr sim,FILE *fptr) {
	portssptr portss;
	portptr port;
	int prt;
	char string[STRCHAR];

	portss=sim->portss;
	if(!portss) return;
	fprintf(fptr,"# Port parameters\n");
	fprintf(fptr,"max_port %i\n",portss->maxport);
	for(prt=0;prt<portss->nport;prt++) {
		port=portss->portlist[prt];
		fprintf(fptr,"start_port %s\n",port->portname);
		fprintf(fptr,"surface %s\n",port->srf->sname);
		fprintf(fptr,"face %s\n",surfface2string(port->face,string));
		fprintf(fptr,"end_port\n\n"); }
	return; }


/* checkportparams.  This checks a few port parameters. */
int checkportparams(simptr sim,int *warnptr) {
	int error,warn,prt,er,i;
	portssptr portss;
	portptr port;
	char string[STRCHAR];

	error=warn=0;
	portss=sim->portss;
	if(!portss) {
		if(warnptr) *warnptr=warn;
		return 0; }

	if(portss->condition!=SCok) {
		warn++;
		printf(" WARNING: port structure %s\n",simsc2string(portss->condition,string)); }

	for(prt=0;prt<portss->nport;prt++) {
		port=portss->portlist[prt];					// check for porting surface
		if(!port->srf) {warn++;printf(" WARNING: there is no porting surface assigned to port %s\n",port->portname);}
		if(!(port->face==PFfront||port->face==PFback))
			{error++;printf(" ERROR: no surface face has been assigned to port %s\n",port->portname);}

		if(port->srf->port[port->face]!=port) {error++;printf(" ERROR: port %s is not registered by surface %s\n",port->portname,port->srf->sname);}

		er=1;																// make sure surface action is set to port
		for(i=0;i<sim->mols->nspecies&&er;i++)
			if(port->srf->action[i][port->face==PFfront?MSsoln:MSbsoln]==SAport) er=0;
		if(er) {warn++;printf(" WARNING: port %s is nonfunctional because no molecule actions at the surface %s are set to port\n",port->portname,port->srf->sname);}
		if(!port->llport) {error++;printf(" BUG: port %s has no molecule buffer\n",port->portname);} }

	if(warnptr) *warnptr=warn;
	return error; }


/******************************************************************************/
/******************************** structure set up ****************************/
/******************************************************************************/


/* portsetcondition.  Sets the port superstructure condition to cond, if
appropriate.  Set upgrade to 1 if this is an upgrade, to 0 if this is a
downgrade, or to 2 to set the condition independent of its current value. */
void portsetcondition(portssptr portss,enum StructCond cond,int upgrade) {
	if(!portss) return;
	if(upgrade==0 && portss->condition>cond) portss->condition=cond;
	else if(upgrade==1 && portss->condition<cond) portss->condition=cond;
	else if(upgrade==2) portss->condition=cond;
	if(portss->condition<portss->sim->condition) {
		cond=portss->condition;
		simsetcondition(portss->sim,cond==SCinit?SClists:cond,0); }
	return; }


/* addport.  Adds, or updates, a port to the port superstructure.  If portname
is not the name of an existing port, a new port is defined, the nport element of
the superstructure is incremented, and this name is copied over for the new
port.  Alternatively, if portname has already been defined, it is used to
reference an existing port.    Either way, the port surface is set to srf if srf
is not NULL, the port face is set to face if face is not PFnone, and the address
of the port is returned. */
portptr addport(portssptr portss,char *portname,surfaceptr srf,enum PanelFace face) {
	int prt;
	portptr port;

	if(!portss) return NULL;
	prt=stringfind(portss->portnames,portss->nport,portname);
	if(prt<0) {
		if(portss->nport>=portss->maxport) return NULL;
		prt=portss->nport++;
		strncpy(portss->portnames[prt],portname,STRCHAR-1);
		portss->portnames[prt][STRCHAR-1]='\0';
		port=portss->portlist[prt]; }
	else
		port=portss->portlist[prt];
	if(srf) port->srf=srf;
	if(face!=PFnone) port->face=face;
	if(port->srf&&port->face!=PFnone)
		port->srf->port[port->face]=port;
	portsetcondition(portss,SClists,0);
	return port; }


/* portreadstring.  Reads and processes one line of text from the configuration
file, or some other source, for the port indexed portindex.  If the port index
is not known, then set portindex to -1.  The first word of the line should be
sent in as word and the rest sent in as line2.  If this function is successful,
it returns the port index and it does not change the contents of erstr; if not,
it returns -1 and it writes an error message to erstr. */
int portreadstring(simptr sim,int portindex,char *word,char *line2,char *erstr) {
	char nm[STRCHAR];
	portptr port;
	int itct,s;
	enum PanelFace face;

	if(portindex>=0) port=sim->portss->portlist[portindex];
	else port=NULL;

	if(!strcmp(word,"name")) {								// name, got[0]
		itct=sscanf(line2,"%s",nm);
		CHECKS(itct==1,"error reading port name");
		CHECKS(port=addport(sim->portss,nm,NULL,PFnone),"more ports are being defined than were allocated");
		portindex=stringfind(sim->portss->portnames,sim->portss->nport,nm);
		CHECKS(portindex>=0,"SMOLDYN BUG: portreadstring");
		CHECKS(!strnword(line2,2),"unexpected text following name"); }

	else if(!strcmp(word,"surface")) {						// surface, got[1]
		CHECKS(port,"port name has to be entered before surface");
		itct=sscanf(line2,"%s",nm);
		CHECKS(itct==1,"error reading surface name");
		s=stringfind(sim->srfss->snames,sim->srfss->nsrf,nm);
		CHECKS(s>=0,"surface name not recognized");
		CHECKS(port==addport(sim->portss,port->portname,sim->srfss->srflist[s],PFnone),"SMOLDYN BUG: new port was created when adding surface");
		CHECKS(!strnword(line2,2),"unexpected text following surface"); }

	else if(!strcmp(word,"face")) {								// face, got[2]
		CHECKS(port,"name has to be entered before face");
		itct=sscanf(line2,"%s",nm);
		CHECKS(itct==1,"error reading face name");
		face=surfstring2face(nm);
		CHECKS(face==PFfront||face==PFback,"face needs to be either front or back");
		CHECKS(port==addport(sim->portss,port->portname,NULL,face),"SMOLDYN BUG: new port was created when adding face");
		CHECKS(!strnword(line2,2),"unexpected text following face"); }

	else {																				// unknown word
		CHECKS(0,"syntax error within port block: statement not recognized"); }

	return portindex;

 failure:
	return -1; }


/* loadport.  Loads a port, or information for an already existing port, from an
already opened configuration file.  This is used to fill in basic port details.
However, it does not assign a molecule buffer to the port.  Returns 0 for
success and 1 for an error; error messages are returned in erstr. */
int loadport(simptr sim,ParseFilePtr *pfpptr,char* line2,char *erstr) {
	ParseFilePtr pfp;
	char word[STRCHAR];
	int done,pfpcode,firstline2,prt;

	pfp=*pfpptr;
	CHECKS(sim->portss,"PROGRAM BUG: port superstructure not allocated in loadport");
	done=0;
	prt=-1;
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
		else if(!strcmp(word,"end_port")) {						// end_port
			CHECKS(!line2,"unexpected text following end_port");
			return 0; }
		else if(!line2) {															// just word
			CHECKS(0,"unknown word or missing parameter"); }
		else {
			prt=portreadstring(sim,prt,word,line2,erstr);
			CHECKS(prt>=0,erstr); }}

	CHECKS(0,"end of file encountered before end_port statement");	// end of file

 failure:																					// failure
	return 1; }


/* setupports.  Sets up the molecule buffers for all ports.  Returns 0 for
success, 1 for inability to allocate sufficient memory, or 2 for molecules not
set up sufficiently. */
int setupports(simptr sim) {
	portssptr portss;
	portptr port;
	int prt,ll;

	portss=sim->portss;
	if(!portss) return 0;
	if(portss->condition==SCok) return 0;

	if(sim->mols && sim->mols->condition<=SClists) return 2;

	if(sim->mols) {
		for(prt=0;prt<portss->nport;prt++) {
			port=portss->portlist[prt];									// port molecule list
			if(port->llport<0) {
				ll=addmollist(sim,port->portname,MLTport);
				if(ll<0) return 1;
				port->llport=ll; }}
	portsetcondition(portss,SCok,1); }
	return 0; }


/******************************************************************************/
/*************************** core simulation functions ************************/
/******************************************************************************/


/* portgetmols.  Returns the number of molecules of type ident and state ms that
are in the export buffer of port port.  This also kills those molecules, so that
they will be returned to the dead list at the next sorting.  If ident is -1,
this returns the total number of molecules on the export list, which are not
killed.  The intention is that molecules that are gotten from the export list
with this function are then added to MOOSE or another simulator. */
int portgetmols(simptr sim,portptr port,int ident,enum MolecState ms) {
	int ll,nmol,count,m;
	moleculeptr *mlist;

	ll=port->llport;
	mlist=sim->mols->live[ll];
	nmol=sim->mols->nl[ll];
	if(ident<0) return nmol;
	count=0;
	for(m=0;m<nmol;m++) {
		if(mlist[m]->ident==ident&&(ms==MSall||mlist[m]->mstate==ms)) {
			count++;
			molkill(sim,mlist[m],ll,m); }}
	sim->eventcount[ETexport]+=count;
	return count; }


/* portputmols.  Adds nmol molecules of type ident and state MSsoln to the
simulation system at the porting surface of port port.  Molecules are placed
randomly on the surface.  This returns 0 for success, 1 for insufficient
available molecules, 2 for no porting surface defined, 3 for no porting surface
face defined, and 4 for inability to allocated temporary memory. */
int portputmols(simptr sim,portptr port,int nmol,int ident) {
	moleculeptr mptr;
	int dim,m,d;
	panelptr pnl;

	if(!nmol) return 0;
	if(!port->srf) return 2;
	if(port->face==PFnone) return 3;
	dim=sim->dim;

	for(m=0;m<nmol;m++) {
		mptr=getnextmol(sim->mols);
		if(!mptr) return 1;
		mptr->ident=ident;
		mptr->mstate=MSsoln;
		mptr->list=sim->mols->listlookup[ident][MSsoln];
		pnl=surfrandpos(port->srf,mptr->posx,dim);
		if(!pnl) return 4;
		fixpt2panel(mptr->posx,pnl,dim,port->face,sim->srfss->epsilon);
		for(d=0;d<dim;d++) mptr->pos[d]=mptr->posx[d];
		mptr->box=pos2box(sim,mptr->pos); }
	sim->eventcount[ETimport]+=nmol;
	return 0; }


/* porttransport.  Transports molecules from port1 of simulation structure sim1
to port2 of simulation structure sim2.  sim1 and sim2 may be the same and port1
and port2 may be the same.  This is designed for testing ports or for coupled
Smoldyn simulations that communicate with ports. */
int porttransport(simptr sim1,portptr port1,simptr sim2,portptr port2) {
	int i,nmol,er;

	if(!portgetmols(sim1,port1,-1,MSnone)) return 0;
	er=0;
	for(i=1;i<sim1->mols->nspecies&&!er;i++) {
		nmol=portgetmols(sim1,port1,i,MSall);
		er=portputmols(sim2,port2,nmol,i); }
	return er; }

