#include "copasi.h"
#include <iostream>
#include <limits>
#include "VCellCallback.h"
using namespace std;

static const double progress_interval_in_s = 0.25;

//#define DEBUG
VCellCallback::VCellCallback(OptSolverCallbacks* callbacks)
: CProcessReport(), optSolverCallbacks(callbacks), itemHandle(-1)
{
	oldTime = clock(); // to control the output of data, output data every 2 seconds.
	numEvals = 0;
	objVal = std::numeric_limits<double>::infinity();
	currVal = 0;
	endVal = 0;
}

VCellCallback::~VCellCallback()
{
}

bool VCellCallback::progressItem(const size_t & handle)
{
	static bool bFirstCall = true;
	if (!isValidHandle(handle) || mProcessReportItemList[handle] == NULL) 
	{
	 return false;
	}
	
	if (bFirstCall) {
		for (int i = 0; i < mProcessReportItemList.size(); i ++) {
			CProcessReportItem* reportItem = mProcessReportItemList[i];
			if (reportItem == 0) {
				continue;
			}
			if (reportItem->hasEndValue()) {
				endVal = *reportItem->getEndValue().pUINT;
				break;
			}		
		}
		bFirstCall = false;
	}

	CProcessReportItem* reportItem = mProcessReportItemList[handle]; 

#ifndef DEBUG
	if (optSolverCallbacks != 0) {
#endif
		clock_t currentTime = clock();
		double duration = (double)(currentTime - oldTime) / CLOCKS_PER_SEC;
					
		updateInfo(handle);
		if(duration >= progress_interval_in_s )
		{
			setEvaluation();
			oldTime = currentTime;
		}
		itemHandle = handle;
		
#ifndef DEBUG
		jboolean bStopRequested = optSolverCallbacks->jniEnv->CallBooleanMethod(optSolverCallbacks->object_optSolverCallbacks, 
			optSolverCallbacks->mid_OptSolverCallbacks_getStopRequested);
		return !bStopRequested;
	}
#endif
	return true;
}

bool VCellCallback::finishItem(const size_t & handle)
{
  if (!isValidHandle(handle) || mProcessReportItemList[handle] == NULL) return false;
  CProcessReportItem* reportItem = mProcessReportItemList[handle]; 
	for (int i = 0; i < mProcessReportItemList.size(); i ++) {
		updateInfo(i);
	}
	setEvaluation();
	return CProcessReport::finishItem(handle);
}

void VCellCallback::setEvaluation() {
	if (optSolverCallbacks == 0) {
		return;
	}
	jobject endValue_Double = 0;
	jclass class_Double = optSolverCallbacks->jniEnv->FindClass("java/lang/Double");
	endValue_Double = optSolverCallbacks->jniEnv->AllocObject(class_Double);
	jmethodID mid_Double_constructor = optSolverCallbacks->jniEnv->GetMethodID(class_Double, "<init>", "(D)V");
	optSolverCallbacks->jniEnv->CallVoidMethod(endValue_Double, mid_Double_constructor, endVal); 
	optSolverCallbacks->jniEnv->CallVoidMethod(optSolverCallbacks->object_optSolverCallbacks, optSolverCallbacks->mid_OptSolverCallbacks_setEvaluation, 
		numEvals, objVal, currVal, endValue_Double);
}

void VCellCallback::updateInfo(const size_t & handle){
	CProcessReportItem* reportItem = mProcessReportItemList[handle];
	if (reportItem == 0) {
		return;
	}
	const string& itemName = reportItem->getObjectName();
	if(itemName == "Best Value")
	{
		objVal = *reportItem->getValue().pDOUBLE;
	}
	else if(itemName == "Function Evaluations")
	{
		numEvals = *reportItem->getValue().pUINT;
	}
	//progress for the algos listed below and no progress for Random Search,Nelder,Praxis,truncated Newton
	else if(itemName == "Current Generation" || //for GA,GASR,EP
	   itemName == "Current Iteration" || //for H&J,L-M,
	   itemName == "Iteration Limit") //for PS
	{
		currVal = *reportItem->getValue().pUINT;
		if (reportItem->hasEndValue()) {
			endVal = *reportItem->getEndValue().pUINT;
		}
	}
	else if ( itemName == "Current Temperature")//for SA
	{
		currVal = *reportItem->getValue().pDOUBLE;
	}

}