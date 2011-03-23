/* Steven Andrews, started 10/22/2001.
 This is an application programming interface for the Smoldyn program.  See
 documentation called Smoldyn_doc1.pdf and Smoldyn_doc2.pdf.
 Copyright 2003-2011 by Steven Andrews.  This work is distributed under the terms
 of the Gnu General Public License (GPL). */

#include "smoldyn.h"
#include "libsmoldyn.h"


/* smolPrepareSimFromFile */
int smolPrepareSimFromFile(char *filepath,char *filename,simptr *simpointer,char *flags) {
	int er;
	char emptystring[256];

	emptystring[0]='\0';
	er=0;
	if(!filepath) filepath=emptystring;
	if(!filename) er=2;
	if(!simpointer) er=3;
	if(!flags) flags=emptystring;
	if(!er)
		er=setupsim(filepath,filename,simpointer,flags);
	return er; }


/* smolLoadSimFromFile */
int smolLoadSimFromFile(char *filepath,char *filename,simptr *simpointer,char *flags,char *erstr) {
	int er;
	char emptystring[256];
	simptr sim;
	
	emptystring[0]='\0';
	er=0;
	if(!filepath) filepath=emptystring;
	if(!filename) er=2;
	if(!simpointer) er=3;
	if(!flags) flags=emptystring;
	if(!erstr) erstr=emptystring;

	sim=*simpointer;
	if(!sim) {
		sim=simalloc(filepath);
		if(!sim) er=1; }

	if(!er)
		er=loadsim(sim,filepath,filename,erstr,flags);

	*simpointer=sim;
	return er; }




