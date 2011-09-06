# @file    TestModifierSpeciesReference.rb
# @brief   ModifierSpeciesReference unit tests
#
# @author  Akiya Jouraku (Ruby conversion)
# @author  Ben Bornstein 
#
# $Id: TestModifierSpeciesReference.rb 11522 2010-07-22 00:32:10Z mhucka $
# $HeadURL: http://sbml.svn.sourceforge.net/svnroot/sbml/trunk/libsbml/src/bindings/ruby/test/sbml/TestModifierSpeciesReference.rb $
#
# ====== WARNING ===== WARNING ===== WARNING ===== WARNING ===== WARNING ======
#
# DO NOT EDIT THIS FILE.
#
# This file was generated automatically by converting the file located at
# src/sbml/test/TestModifierSpeciesReference.c
# using the conversion program dev/utilities/translateTests/translateTests.pl.
# Any changes made here will be lost the next time the file is regenerated.
#
# -----------------------------------------------------------------------------
# This file is part of libSBML.  Please visit http://sbml.org for more
# information about SBML, and the latest version of libSBML.
#
# Copyright 2005-2010 California Institute of Technology.
# Copyright 2002-2005 California Institute of Technology and
#                     Japan Science and Technology Corporation.
# 
# This library is free software; you can redistribute it and/or modify it
# under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation.  A copy of the license agreement is provided
# in the file named "LICENSE.txt" included with this software distribution
# and also available online as http://sbml.org/software/libsbml/license.html
# -----------------------------------------------------------------------------
require 'test/unit'
require 'libSBML'

class TestModifierSpeciesReference < Test::Unit::TestCase

  def setup
    @@msr = LibSBML::ModifierSpeciesReference.new(2,4)
    if (@@msr == nil)
    end
  end

  def teardown
    @@msr = nil
  end

  def test_ModifierSpeciesReference_create
    assert( @@msr.getTypeCode() == LibSBML::SBML_MODIFIER_SPECIES_REFERENCE )
    assert( @@msr.getMetaId() == "" )
    assert( @@msr.getNotes() == nil )
    assert( @@msr.getAnnotation() == nil )
    assert( @@msr.getSpecies() == "" )
    assert_equal false, @@msr.isSetSpecies()
    assert_equal true, @@msr.isModifier()
  end

  def test_ModifierSpeciesReference_createWithNS
    xmlns = LibSBML::XMLNamespaces.new()
    xmlns.add( "http://www.sbml.org", "testsbml")
    sbmlns = LibSBML::SBMLNamespaces.new(2,1)
    sbmlns.addNamespaces(xmlns)
    object = LibSBML::ModifierSpeciesReference.new(sbmlns)
    assert( object.getTypeCode() == LibSBML::SBML_MODIFIER_SPECIES_REFERENCE )
    assert( object.getMetaId() == "" )
    assert( object.getNotes() == nil )
    assert( object.getAnnotation() == nil )
    assert( object.getLevel() == 2 )
    assert( object.getVersion() == 1 )
    assert( object.getNamespaces() != nil )
    n = object.getNamespaces()
    assert( n.getLength() == 2 )
    object = nil
  end

  def test_ModifierSpeciesReference_free_NULL
  end

  def test_ModifierSpeciesReference_setSpecies
    species =  "s1";
    @@msr.setSpecies(species)
    s = @@msr.getSpecies()
    assert (( species == s ))
    assert_equal true, @@msr.isSetSpecies()
    if (@@msr.getSpecies() == species)
    end
    s = @@msr.getSpecies()
    @@msr.setSpecies(s)
    s = @@msr.getSpecies()
    assert (( species == s ))
    @@msr.setSpecies("")
    assert_equal false, @@msr.isSetSpecies()
    if (@@msr.getSpecies() != nil)
    end
  end

end