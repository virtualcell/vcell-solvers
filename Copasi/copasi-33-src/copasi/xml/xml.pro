# Begin CVS Header
#   $Source: /fs/turing/cvs/copasi_dev/copasi/xml/xml.pro,v $
#   $Revision: 1.14 $
#   $Name: Build-33 $
#   $Author: shoops $
#   $Date: 2010/07/16 19:06:32 $
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
# $Revision: 1.14 $ $Author: shoops $ $Date: 2010/07/16 19:06:32 $
######################################################################

LIB = copasiXML
SRC_TARGET = xml

# Input
HEADERS += CCopasiXML.h \
           CCopasiXMLInterface.h \
           CCopasiXMLVersion.h \
           CExpat.h \
           CFixLocalReactionParameters.h \
           CXMLHandler.h  \
           CCopasiXMLParser.h

SOURCES += CCopasiXML.cpp \
           CCopasiXMLInterface.cpp \
           CExpat.cpp \
           CFixLocalReactionParameters.cpp \
           CCopasiXMLParser.cpp


include(../lib.pri)
include(../common.pri)
