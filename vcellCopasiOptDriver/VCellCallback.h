#ifndef VCELLCALLBACK_H
#define VCELLCALLBACK_H

#include "utilities/CProcessReport.h"
#include <jni.h>
#include <time.h>

struct OptSolverCallbacks {
	JNIEnv* jniEnv;
	jobject object_optSolverCallbacks;
	jmethodID mid_OptSolverCallbacks_getStopRequested;
	jmethodID mid_OptSolverCallbacks_setEvaluation;
};

/**
 *  This is used to call the progress bar code
 *  We do not want to call GUI stuff directly from the CModel.
 */
class VCellCallback : public CProcessReport
{
public:
  VCellCallback(OptSolverCallbacks* optSolverCallbacks);

  virtual ~VCellCallback();

  /**
   * Report process on item handle. If the return value is false the calling
   * process must halt execution and return.
   * @param const unsigned C_INT32 & handle
   * @param bool continue
   */
  virtual bool progressItem(const size_t & handle);


  /**
   * Indicate that item handle is finished reporting. The handle of that
   * item is no longer valid after the call. If the return value is false
   * the calling process must halt execution and return.
   * @param const unsigned C_INT32 & handle
   * @param bool continue
   */
  virtual bool finishItem(const size_t & handle);

  void setRunNumber(int runNumber){runNo = runNumber;}

private:
	OptSolverCallbacks* optSolverCallbacks;
	clock_t oldTime; // to control the output of data, output data every 2 seconds.

	int numEvals;
	double objVal;
	double currVal;
	double endVal;
	double itemHandle;
	int runNo;
	void setEvaluation();
	void updateInfo(const size_t & handle);
};

#endif
