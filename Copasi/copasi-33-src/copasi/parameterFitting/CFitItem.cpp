// Begin CVS Header
//   $Source: /fs/turing/cvs/copasi_dev/copasi/parameterFitting/CFitItem.cpp,v $
//   $Revision: 1.22 $
//   $Name: Build-33 $
//   $Author: shoops $
//   $Date: 2009/07/20 16:06:20 $
// End CVS Header

// Copyright (C) 2008 by Pedro Mendes, Virginia Tech Intellectual
// Properties, Inc., EML Research, gGmbH, University of Heidelberg,
// and The University of Manchester.
// All rights reserved.

// Copyright (C) 2001 - 2007 by Pedro Mendes, Virginia Tech Intellectual
// Properties, Inc. and EML Research, gGmbH.
// All rights reserved.

#include <limits>
#include <math.h>

#include "copasi.h"

#include "CFitItem.h"

#include "report/CKeyFactory.h"
#include "utilities/CCopasiParameterGroup.h"
#include "CopasiDataModel/CCopasiDataModel.h"
#include "copasi/report/CCopasiRootContainer.h"

CFitItem::CFitItem(const CCopasiContainer * pParent,
                   const std::string & name):
    COptItem(pParent, name),
    mpGrpAffectedExperiments(NULL),
    mLocalValue(0),
    mpLocalMethod(new SpecificUpdateMethod<CFitItem, C_FLOAT64>(this, &CFitItem::setLocalValue))
{initializeParameter();}

CFitItem::CFitItem(const CFitItem & src,
                   const CCopasiContainer * pParent):
    COptItem(src, pParent),
    mpGrpAffectedExperiments(NULL),
    mLocalValue(0),
    mpLocalMethod(new SpecificUpdateMethod<CFitItem, C_FLOAT64>(this, &CFitItem::setLocalValue))
{initializeParameter();}

CFitItem::CFitItem(const CCopasiParameterGroup & group,
                   const CCopasiContainer * pParent):
    COptItem(group, pParent),
    mpGrpAffectedExperiments(NULL),
    mLocalValue(0),
    mpLocalMethod(new SpecificUpdateMethod<CFitItem, C_FLOAT64>(this, &CFitItem::setLocalValue))
{initializeParameter();}

CFitItem::~CFitItem()
{pdelete(mpLocalMethod);}

void CFitItem::initializeParameter()
{
  mpGrpAffectedExperiments = assertGroup("Affected Experiments");


  elevateChildren();
}

bool CFitItem::elevateChildren()
{
  // The functionality of SavedValue is no handled more transparently
  // through the StartValue. Therefore, in case we encounter an old file
  // we need to copy its value.
  CCopasiParameter *pSavedValue = getParameter("SavedValue");

  if (pSavedValue)
    {
      setStartValue(*pSavedValue->getValue().pDOUBLE);
      removeParameter("SavedValue");
    }

  mpGrpAffectedExperiments =
    elevate<CCopasiParameterGroup, CCopasiParameterGroup>(mpGrpAffectedExperiments);

  if (!mpGrpAffectedExperiments) return false;


  return true;
}

bool CFitItem::isValid() const
{return COptItem::isValid();}

bool CFitItem::isValid(CCopasiParameterGroup & group)
{
  CFitItem tmp(group);

  return tmp.isValid();
}

bool CFitItem::compile(const std::vector< CCopasiContainer * > listOfContainer)
{
  if (!COptItem::compile(listOfContainer)) return false;

  mLocalValue = *mpParmStartValue;

  return true;
}

C_INT32 CFitItem::checkConstraint() const
{
  if (*mpLowerBound > mLocalValue) return - 1;

  if (mLocalValue > *mpUpperBound) return 1;

  return 0;
}

C_FLOAT64 CFitItem::getConstraintViolation() const
{
  switch (checkConstraint())
    {
      case - 1:
        return *mpLowerBound - mLocalValue;
        break;

      case 1:
        return mLocalValue - *mpUpperBound;
        break;

      default:
        return 0.0;
        break;
    }
}

std::ostream &operator<<(std::ostream &os, const CFitItem & o)
{
  os << * static_cast<const COptItem *>(&o) << std::endl;

  unsigned C_INT32 i, imax = o.mpGrpAffectedExperiments->size();

  os << "    Affected Experiments:" << std::endl << "      ";

  if (imax == 0) os << "all";

  for (i = 0; i < imax; i++)
    {
      if (i) os << ", ";

      os << o.getExperiment(i);
    }


  return os;
}

void CFitItem::setLocalValue(const C_FLOAT64 & value)
{
  mLocalValue = value;
  return;
}

const C_FLOAT64 & CFitItem::getLocalValue() const
{return mLocalValue;}

const C_FLOAT64 * CFitItem::getObjectValue() const
{return & mLocalValue;}

UpdateMethod * CFitItem::getUpdateMethod() const
{return mpLocalMethod;}

bool CFitItem::addExperiment(const std::string & key)
{
  unsigned C_INT32 i, imax = mpGrpAffectedExperiments->size();

  for (i = 0; i < imax; i++)
    if (*mpGrpAffectedExperiments->getValue(i).pKEY == key) return false; // The key already exists.

  return mpGrpAffectedExperiments->addParameter("Experiment Key", CCopasiParameter::KEY, key);
}

const std::string & CFitItem::getExperiment(const unsigned C_INT32 & index) const
{
  static const std::string Empty("");

  if (index < mpGrpAffectedExperiments->size())
    return *mpGrpAffectedExperiments->getValue(index).pKEY;

  return Empty;
}

bool CFitItem::removeExperiment(const unsigned C_INT32 & index)
{return mpGrpAffectedExperiments->removeParameter(index);}

unsigned C_INT32 CFitItem::getExperimentCount() const
{return mpGrpAffectedExperiments->size();}

std::string CFitItem::getExperiments() const
{
  std::string Experiments;
  unsigned C_INT32 i, imax = mpGrpAffectedExperiments->size();
  const CCopasiObject * pObject;

  for (i = 0; i < imax; i++)
    {
      pObject = CCopasiRootContainer::getKeyFactory()->get(*mpGrpAffectedExperiments->getValue(i).pKEY);

      if (i && pObject)
        Experiments += ", ";

      Experiments += pObject->getObjectName();
    }

  return Experiments;
}


bool CFitItem::updateBounds(std::vector<COptItem * >::iterator it)
{
  while (*it != this)
    {
      if (mpLowerObject && (getLowerBound() == (*it)->getObjectCN()))
        mpLowerBound = &static_cast<CFitItem *>(*it)->getLocalValue();

      if (mpUpperObject && (getUpperBound() == (*it)->getObjectCN()))
        mpUpperBound = &static_cast<CFitItem *>(*it)->getLocalValue();

      ++it;
    }

  return true;
}

CFitConstraint::CFitConstraint(const CCopasiContainer * pParent,
                               const std::string & name):
    CFitItem(pParent, name),
    mCheckConstraint(0),
    mConstraintViolation(0.0)
{}

CFitConstraint::CFitConstraint(const CFitConstraint & src,
                               const CCopasiContainer * pParent):
    CFitItem(src, pParent),
    mCheckConstraint(src.mCheckConstraint),
    mConstraintViolation(src.mConstraintViolation)
{}

CFitConstraint::CFitConstraint(const CCopasiParameterGroup & group,
                               const CCopasiContainer * pParent):
    CFitItem(group, pParent),
    mCheckConstraint(0),
    mConstraintViolation(0.0)
{}

CFitConstraint::~CFitConstraint() {}

void CFitConstraint::resetConstraintViolation()
{
  mCheckConstraint = 0;
  mConstraintViolation = 0.0;
}

void CFitConstraint::calculateConstraintViolation()
{
  if (*mpLowerBound > *mpObjectValue) mCheckConstraint = -1;
  else if (*mpObjectValue > *mpUpperBound) mCheckConstraint = 1;
  else mCheckConstraint = 0;

  switch (mCheckConstraint)
    {
      case - 1:
        mConstraintViolation += *mpLowerBound - *mpObjectValue;
        break;

      case 1:
        mConstraintViolation += *mpObjectValue - *mpUpperBound;
        break;

      default:
        break;
    }
}

C_INT32 CFitConstraint::checkConstraint() const
{return mCheckConstraint;}

C_FLOAT64 CFitConstraint::getConstraintViolation() const
{return mConstraintViolation;}
