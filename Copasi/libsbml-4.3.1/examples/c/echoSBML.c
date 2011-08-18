/**
 * @file    echoSBML.c
 * @brief   Echos (and pretty prints) an SBML model.
 * @author  Ben Bornstein
 *
 * $Id: echoSBML.c 8704 2009-01-04 02:26:05Z mhucka $
 * $HeadURL: http://sbml.svn.sourceforge.net/svnroot/sbml/trunk/libsbml/examples/c/echoSBML.c $
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


#include <stdio.h>
#include <sbml/SBMLTypes.h>


int
main (int argc, char *argv[])
{
  if (argc != 3)
  {
    printf("Usage: echoSBML input-filename output-filename\n");
    return 2;
  }

  writeSBML(readSBML(argv[1]), argv[2]);
  return 0;
}
