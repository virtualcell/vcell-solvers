/**
 * @cond doxygen-libsbml-internal
 *
 * @file    ModelingPracticeConstraints.cpp
 * @brief   ModelingPractice check constraints.  
 * @author  Sarah Keating
 *
 * $Id: ModelingPracticeConstraints.cpp 13266 2011-03-21 22:26:50Z mhucka $
 * $HeadURL: http://sbml.svn.sourceforge.net/svnroot/sbml/trunk/libsbml/src/validator/constraints/ModelingPracticeConstraints.cpp $
 *
 * Copyright (C) 2009-2011 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. EMBL European Bioinformatics Institute (EBML-EBI), Hinxton, UK
 *  
 * Copyright (C) 2006-2008 by the California Institute of Technology,
 *     Pasadena, CA, USA 
 *  
 * Copyright (C) 2002-2005 jointly by the following organizations: 
 *     1. California Institute of Technology, Pasadena, CA, USA
 *     2. Japan Science and Technology Agency, Japan
 * 
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation.  A copy of the license agreement is
 * provided in the file named "LICENSE.txt" included with this software
 * distribution.  It is also available online at
 * http://sbml.org/software/libsbml/license.html
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA.
 */



#ifndef AddingConstraintsToValidator
#include <sbml/SBMLTypes.h>
#include <sbml/validator/VConstraint.h>
#endif

#include <sbml/validator/ConstraintMacros.h>

#include "LocalParameterShadowsIdInModel.h"
/** @cond doxygen-ignored */

using namespace std;

/** @endcond */

// Compartment validation

START_CONSTRAINT (80501, Compartment, c)
{
  pre( c.getLevel() > 1);
  pre( c.getSpatialDimensions() != 0 );
  
  //msg =
  //  "It is recommended that the size of a compartment is set.";

  inv( c.isSetSize() == true );
}
END_CONSTRAINT

// Parameters
EXTERN_CONSTRAINT( 81121, LocalParameterShadowsIdInModel             )


START_CONSTRAINT (80701, Parameter, p)
{
  inv(p.isSetUnits() == true);
}
END_CONSTRAINT



/** @endcond */
