/* Steven Andrews, 1/10/04.
Library for runtime command interpreter used for various simulations.
Copyright 2004-2007 by Steven Andrews.  This work is distributed under the terms
of the Gnu Lesser General Public License (LGPL). */

#ifndef __SimCommand_h__
#define __SimCommand_h__

#include "queue.h"
#include "string2.h"

#define SCMDCHECK(A,B) if(!(A)) {if(cmd) strncpy(cmd->erstr,B,STRCHAR);return CMDwarn;}

enum CMDcode {CMDok,CMDwarn,CMDpause,CMDstop,CMDabort,CMDnone,CMDcontrol,CMDobserve,CMDmanipulate,CMDctrlORobs,CMDall};

typedef struct cmdstruct {
	double on;
	double off;
	double dt;
	double xt;
	Q_LONGLONG oni;
	Q_LONGLONG offi;
	Q_LONGLONG dti;
	Q_LONGLONG invoke;
	char *str;
	char *erstr;
	int i1,i2,i3;
	double f1,f2,f3;
	void *v1,*v2,*v3;
	void (*freefn)(struct cmdstruct*); } *cmdptr;

typedef struct cmdsuperstruct {
	queue cmd;
	queue cmdi;
	enum CMDcode (*cmdfn)(void*,cmdptr,char*);
	void *cmdfnarg;
	int iter;
	int nfile;
	char root[STRCHAR];
	char froot[STRCHAR];
	int *fsuffix;
	char **fname;
	FILE **fptr; } *cmdssptr;

// non-file functions
char *scmdcode2string(enum CMDcode code,char *string);
cmdptr scmdalloc(void);
void scmdfree(cmdptr cmd);
cmdssptr scmdssalloc(enum CMDcode (*cmdfn)(void*,cmdptr,char*),void *cmdfnarg,char *root);
void scmdssfree(cmdssptr cmds);
int scmdqalloc(cmdssptr cmds,int n);
int scmdqalloci(cmdssptr cmds,int n);
int scmdstr2cmd(cmdssptr cmds,char *line2,double tmin,double tmax,double dt);
void scmdpop(cmdssptr cmds,double t);
enum CMDcode scmdexecute(cmdssptr cmds,double time,double simdt,Q_LONGLONG iter,int donow);
enum CMDcode scmdcmdtype(cmdssptr cmds,cmdptr cmd);
int scmdnextcmdtime(cmdssptr cmds,double time,Q_LONGLONG iter,enum CMDcode type,int equalok,double *timeptr,Q_LONGLONG *iterptr);
void scmdoutput(cmdssptr cmds);
void scmdwritecommands(cmdssptr cmds,FILE *fptr,char *filename);

// file functions
int scmdsetfroot(cmdssptr cmds,char *root);
int scmdsetfnames(cmdssptr cmds,char *str);
int scmdsetfsuffix(cmdssptr cmds,char *fname,int i);
int scmdopenfiles(cmdssptr cmds,int overwrite);
FILE *scmdoverwrite(cmdssptr cmds,char *line2);
FILE *scmdincfile(cmdssptr cmds,char *line2);
FILE *scmdgetfptr(cmdssptr cmds,char *line2);

# endif
