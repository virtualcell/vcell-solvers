/**
 * @file    validateSBML.c
 * @brief   Validates an SBML file against the appropriate schema
 * @author  Ben Bornstein
 *
 * $Id: validateSBML.c 8746 2009-01-06 19:06:13Z mhucka $
 * $HeadURL: http://sbml.svn.sourceforge.net/svnroot/sbml/trunk/libsbml/examples/c/validateSBML.c $
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */

#include <stdio.h>

#include <sbml/SBMLTypes.h>
#include "util.h"


int
main (int argc, char *argv[])
{
  const char *filename;

  unsigned long long start, stop;
  unsigned long      size;
  unsigned int       errors = 0;

  SBMLDocument_t *d;


  if (argc != 2)
  {
    printf("Usage: validateSBML filename\n");
    return 2;
  }

  filename = argv[1];

  start = getCurrentMillis();
  d     = readSBML(filename);
  stop  = getCurrentMillis();

  errors  = SBMLDocument_getNumErrors(d);
  errors += SBMLDocument_checkConsistency(d);

  size = getFileSize(filename);

  printf( "\n" );
  printf( "        filename: %s\n" , filename     );
  printf( "       file size: %lu\n", size         );
  printf( "  read time (ms): %lu\n", stop - start );
  printf( "        error(s): %u\n" , errors       );

  if (errors > 0) SBMLDocument_printErrors(d, stdout);
  printf("\n");

  SBMLDocument_free(d);
  return errors;
}
