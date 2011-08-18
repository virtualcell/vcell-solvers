# Begin CVS Header
#   $Source: /fs/turing/cvs/copasi_dev/copasi/moieties/moieties.pro,v $
#   $Revision: 1.6 $
#   $Name: Build-33 $
#   $Author: shoops $
#   $Date: 2010/07/16 19:00:58 $
# End CVS Header

# Copyright (C) 2010 by Pedro Mendes, Virginia Tech Intellectual 
# Properties, Inc., University of Heidelberg, and The University 
# of Manchester. 
# All rights reserved. 

# Copyright (C) 2008 by Pedro Mendes, Virginia Tech Intellectual
# Properties, Inc., EML Research, gGmbH, University of Heidelberg,
# and The University of Manchester.
# All rights reserved.

######################################################################
# $Revision: 1.6 $ $Author: shoops $ $Date: 2010/07/16 19:00:58 $
######################################################################

LIB = moieties

# Input
HEADERS += CMoietiesMethod.h
HEADERS += CMoietiesProblem.h
HEADERS += CMoietiesTask.h

SOURCES += CMoietiesMethod.cpp
SOURCES += CMoietiesProblem.cpp
SOURCES += CMoietiesTask.cpp

include(../lib.pri)
include(../common.pri)
