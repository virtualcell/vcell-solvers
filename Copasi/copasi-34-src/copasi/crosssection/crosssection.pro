# Begin CVS Header 
#   $Source: /fs/turing/cvs/copasi_dev/copasi/crosssection/crosssection.pro,v $ 
#   $Revision: 1.1.2.3 $ 
#   $Name: Build-33 $ 
#   $Author: shoops $ 
#   $Date: 2010/11/13 16:55:55 $ 
# End CVS Header 

# Copyright (C) 2010 by Pedro Mendes, Virginia Tech Intellectual 
# Properties, Inc., University of Heidelberg, and The University 
# of Manchester. 
# All rights reserved. 

LIB = crosssection
DISTFILES = crosssection.pro

# Input
HEADERS += CCrossSectionMethod.h \
           CCrossSectionProblem.h \
           CCrossSectionTask.h

SOURCES += CCrossSectionMethod.cpp \
           CCrossSectionProblem.cpp \
           CCrossSectionTask.cpp

include(../lib.pri)
include(../common.pri)
include(../srcDistribution.pri)
