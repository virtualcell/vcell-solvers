# Begin CVS Header 
#   $Source: /fs/turing/cvs/copasi_dev/copasi/Attic/srcDistribution.pri,v $ 
#   $Revision: 1.1.2.2 $ 
#   $Name: Build-33 $ 
#   $Author: shoops $ 
#   $Date: 2010/11/13 16:55:53 $ 
# End CVS Header 

# Copyright (C) 2010 by Pedro Mendes, Virginia Tech Intellectual 
# Properties, Inc., University of Heidelberg, and The University 
# of Manchester. 
# All rights reserved. 

!equals(TEMPLATE, subdirs) {
  # Copy the sources for the tar ball
  src_distribution.depends = Makefile
  src_distribution.commands =   \
    $(CHK_DIR_EXISTS) ../../copasi_src || $(MKDIR) ../../copasi_src; \
    $(CHK_DIR_EXISTS) ../../copasi_src/copasi || \
      $(MKDIR) ../../copasi_src/copasi; \
    $(CHK_DIR_EXISTS) ../../copasi_src/copasi/$$SRC_TARGET || \
      $(MKDIR) ../../copasi_src/copasi/$$SRC_TARGET; \
    $(COPY_FILE) --parents $${SOURCES} $${HEADERS} $${FORMS} $${FORMS3} $${DISTFILES} \
      ../../copasi_src/copasi/$$SRC_TARGET/

  QMAKE_EXTRA_UNIX_TARGETS += src_distribution
}

