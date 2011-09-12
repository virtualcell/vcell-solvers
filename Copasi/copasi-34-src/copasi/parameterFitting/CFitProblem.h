// Begin CVS Header
//   $Source: /fs/turing/cvs/copasi_dev/copasi/parameterFitting/CFitProblem.h,v $
//   $Revision: 1.22.4.4 $
//   $Name: Build-33 $
//   $Author: shoops $
//   $Date: 2011/06/01 16:24:41 $
// End CVS Header

// Copyright (C) 2011 - 2010 by Pedro Mendes, Virginia Tech Intellectual
// Properties, Inc., University of Heidelberg, and The University
// of Manchester.
// All rights reserved.

// Copyright (C) 2008 by Pedro Mendes, Virginia Tech Intellectual
// Properties, Inc., EML Research, gGmbH, University of Heidelberg,
// and The University of Manchester.
// All rights reserved.

// Copyright (C) 2001 - 2007 by Pedro Mendes, Virginia Tech Intellectual
// Properties, Inc. and EML Research, gGmbH.
// All rights reserved.

#ifndef COPASI_CFitProblem
#define COPASI_CFitProblem

#include "optimization/COptProblem.h"
#include "utilities/CMatrix.h"

class CExperimentSet;
class CCrossValidationSet;
class CTrajectoryProblem;
class CState;
class CFitConstraint;
class CArrayAnnotation;
template < class CMatrixType > class CCopasiMatrixInterface;

class CFitProblem : public COptProblem
{
public:
  // Operations
  /**
   * Default constructor
   * @param const CCopasiTask::Type & type (default: parameterFitting)
   * @param const CCopasiContainer * pParent (default: NULL)
   */
  CFitProblem(const CCopasiTask::Type & type = CCopasiTask::parameterFitting,
              const CCopasiContainer * pParent = NULL);

  /**
   * Copy constructor.
   * @param const CFitProblem & src
   * @param const CCopasiContainer * pParent (default: NULL)
   */
  CFitProblem(const CFitProblem & src,
              const CCopasiContainer * pParent = NULL);

  /**
   * Destructor
   */
  virtual ~CFitProblem();

  /**
   * This methods must be called to elevate subgroups to
   * derived objects. The default implementation does nothing.
   * @return bool success
   */
  virtual bool elevateChildren();

  /**
   * Set the model of the problem
   * @param CModel * pModel
   * @result bool success
   */
  virtual bool setModel(CModel * pModel);

  /**
   * Set the call back of the problem
   * @param CProcessReport * pCallBack
   * @result bool success
   */
  virtual bool setCallBack(CProcessReport * pCallBack);

  /**
   * Do all necessary initialization so that calls to calculate will
   * be successful. This is called once from CCopasiTask::process()
   * @result bool success
   */
  virtual bool initialize();

  /**
   * Do the calculation based on CalculateVariables and fill
   * CalculateResults with the results.
   * @result bool continue
   */
  virtual bool calculate();

  /**
   * Do all necessary restore procedures so that the
   * model is in the same state as before
   * @param const bool & updateModel
   * @result bool success
   */
  virtual bool restore(const bool & updateModel);

  /**
   * Check whether all functional constraints are fulfilled.
   * @result bool fulfilled
   */
  virtual bool checkFunctionalConstraints();

  /**
   * This is the output method for any object. The default implementation
   * provided with CCopasiObject uses the ostream operator<< of the object
   * to print the object.To override this default behavior one needs to
   * reimplement the virtual print function.
   * @param std::ostream * ostream
   */
  virtual void print(std::ostream * ostream) const;

  /**
   * Output stream operator
   * @param ostream & os
   * @param const CFitProblem & A
   * @return ostream & os
   */
  friend std::ostream &operator<<(std::ostream &os, const CFitProblem & o);

  /**
   * This is the output method for any result of a problem. The default implementation
   * provided with CCopasiProblem. Does only print "Not implemented." To override this
   * default behavior one needs to reimplement the virtual printResult function.
   * @param std::ostream * ostream
   */
  virtual void printResult(std::ostream * ostream) const;

  /**
   * Fetch a new initial state from the model.
   */
  void updateInitialState();

  /**
   * Set residual required
   * @param const bool & required
   * @return bool success
   */
  bool setResidualsRequired(const bool & required);

  /**
   * Retrieve the vector of residuals
   * @return const CVector< C_FLOAT64 > & residuals
   */
  const CVector< C_FLOAT64 > & getResiduals() const;

  /**
   * Calculate the statistics for the problem
   * @param const C_FLOAT64 & factor (Default: 1.0e-003)
   * @param const C_FLOAT64 & resolution (Default: 1.0e-009)
   */
  virtual bool calculateStatistics(const C_FLOAT64 & factor = 1.0e-003,
                                   const C_FLOAT64 & resolution = 1.0e-009);

  /**
   * Retrieve the root mean square of the objective value.
   * @return const C_FLOAT64 & RMS
   */
  const C_FLOAT64 & getRMS() const;

  /**
   * Retrieve the std. deviation of the objective value.
   * @return const C_FLOAT64 & stdDeviation
   */
  const C_FLOAT64 & getStdDeviation() const;

  /**
   * Retrieve the std. deviation  for each solution variable.
   * @return const CVector< C_FLOAT64 > & variableStdDeviations
   */
  const CVector< C_FLOAT64 > & getVariableStdDeviations() const;

  /**
   * Retrieve the Fisher Information Matrix of the solution variables.
   * @return CArrayAnnotation & variableCorrelations
   */
  CArrayAnnotation & getFisherInformation() const;

  /**
   * Retrieve the correlations of the solution variables.
   * @return CArrayAnnotation & variableCorrelations
   */
  CArrayAnnotation & getCorrelations() const;

  /**
   * Retrieve the experiment set.
   * @return const CExperimentSet & experiementSet
   */
  const CExperimentSet & getExperiementSet() const;


private:
  /**
   * Allocates all group parameters and assures that they are
   * properly initialized.
   */
  void initializeParameter();

  virtual bool createObjectiveFunction();

  /**
   * Initialize objects
   */
  void initObjects();


private:
  // Attributes
  /**
   * A pointer to the value of the CCopasiParameter holding the Steady-State CN
   */
  std::string * mpParmSteadyStateCN;

  /**
   * A pointer to the value of the CCopasiParameter holding the Time Course CN
   */
  std::string * mpParmTimeCourseCN;

  /**
   * The experiment set to which the model is fitted.
   */
  CExperimentSet * mpExperimentSet;

  /**
   * Pointer to CSteadyStateTask.  To be used in calculate() to select between
   * trajectory and steady state method
   */
  CSteadyStateTask * mpSteadyState;

  /**
   * Pointer to CTrajectory.  To be used in calculate() to select between
   * trajectory and steady state method
   */
  CTrajectoryTask * mpTrajectory;

  /**
   * Matrix of update methods for items for each experiment.
   */
  CMatrix< UpdateMethod * > mExperimentUpdateMethods;

  /**
   * Matrix of update methods for undoing items which are not used in all experiment.
   */
  CMatrix< UpdateMethod * > mExperimentUndoMethods;

  /**
   * A vector of refresh methods which contains the sequence of refresh methods
   * to update the initial values for each experiment.
   * */
  CVector< std::vector< Refresh * > > mExperimentInitialRefreshes;

  /**
   * Matrix of constraints for each experiment.
   */
  CMatrix< CFitConstraint * > mExperimentConstraints;

  /**
   * A vector of refresh methods which contains the sequence of refresh methods
   * to update the constraint values for each experiment.
   * */
  CVector< std::vector< Refresh * > > mExperimentConstraintRefreshes;

  /**
   * The simulation values for the experiments.
   */
  CVector< C_FLOAT64 > mExperimentDependentValues;


  /**
   * Copy of the trajectory problem so that we can restore the defaults
   */
  CTrajectoryProblem * mpTrajectoryProblem;

  /**
   * Copy of the initial state needed for calculate
   */
  CState * mpInitialState;

  /**
   * Matrix of the residuals.
   */
  CVector< C_FLOAT64 > mResiduals;

  /**
   * The root mean square of the solution
   */
  C_FLOAT64 mRMS;

  /**
   * The standard deviation of the solution
   */
  C_FLOAT64 mSD;

  /**
   * The vector of standard deviations of the parameters
   */
  CVector< C_FLOAT64 > mParameterSD;

  /**
   * The Fisher information or parameter correlation matrix
   */
  CMatrix< C_FLOAT64 > mFisher;
  CCopasiMatrixInterface< CMatrix< C_FLOAT64 > > * mpFisherMatrixInterface;
  CArrayAnnotation * mpFisherMatrix;

  /**
   * The  parameter correlation matrix
   */
  CMatrix< C_FLOAT64 > mCorrelation;
  CCopasiMatrixInterface< CMatrix< C_FLOAT64 > > * mpCorrelationMatrixInterface;
  CArrayAnnotation * mpCorrelationMatrix;
};

#endif  // COPASI_CFitProblem
