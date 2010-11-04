// OptSolverLibrary.cpp : Defines the entry point for the application.
//

#include "OptProblemDescription.h"

OptProblemDescription::OptProblemDescription(ParameterDescription *arg_parameterDesc,
				ConstraintDescription *arg_constraintDescription,
				ObjectiveFunction *arg_objectiveFunction,
				bool bComputeProfDists)
{
	parameterDescription = arg_parameterDesc;
	constraintDescription = arg_constraintDescription;
	objectiveFunction = arg_objectiveFunction;
	bComputeProfileDistributions = bComputeProfDists;
}

OptProblemDescription::~OptProblemDescription()
{
}
