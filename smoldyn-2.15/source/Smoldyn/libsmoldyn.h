/* Steven Andrews, started 10/22/2001.
 This is an application programming interface for the Smoldyn program.  See
 documentation called Smoldyn_doc1.pdf and Smoldyn_doc2.pdf.
 Copyright 2003-2011 by Steven Andrews.  This work is distributed under the terms
 of the Gnu General Public License (GPL). */

#ifndef __libsmoldyn_h__
#define __libsmoldyn_h__

#include "smoldyn.h"

int smolPrepareSimFromFile(char *filepath,char *filename,simptr *simpointer,char *flags);
int smolLoadSimFromFile(char *filepath,char *filename,simptr *simpointer,char *flags,char *erstr);

#endif
