/**
 * @file    echoSBML.cpp
 * @brief   Echos (and pretty prints) an SBML model.
 * @author  Ben Bornstein
 *
 * $Id: echoSBML.cpp 12338 2010-12-17 18:58:49Z fbergmann $
 * $HeadURL: http://sbml.svn.sourceforge.net/svnroot/sbml/trunk/libsbml/examples/c++/echoSBML.cpp $
 *
 * This file is part of libSBML.  Please visit http://sbml.org for more
 * information about SBML, and the latest version of libSBML.
 */


#include <iostream>
#include <sbml/SBMLTypes.h>

using namespace std;
LIBSBML_CPP_NAMESPACE_USE

int
main (int argc, char* argv[])
{
  if (argc != 3)
  {
    cout << endl << "Usage: echoSBML input-filename output-filename"
         << endl << endl;
    return 2;
  }

//  writeSBML(readSBML(argv[1]), argv[2]);
  SBMLDocument *d = readSBML(argv[1]);
  if ( d->getNumErrors() > 0)
  {
    d->printErrors();
  }
  else
  {
    writeSBML(d, argv[2]);
  }

  return 0;
}
