#ifndef OPTXML_H
#define OPTXML_H

static const char* OptProblemDescription_Tag = "optProblemDescription";

static const char* ObjectiveFunction_Tag = "objectiveFunction";
static const char* ObjectiveFunctionType_Attr = "type";
static const char* ObjectiveFunctionType_Attr_Explicit = "explicit";
static const char* ObjectiveFunctionType_Attr_ODE = "ode";
static const char* ObjectiveFunctionType_Attr_PDE = "pde";
static const char* Model_Tag = "model";
static const char* ModelType_Attr = "modelType";
static const char* ModelType_Attr_IDA = "ida";
static const char* ModelType_Attr_CVODE = "cvode";
static const char* Data_Tag = "data";
static const char* Variable_Tag = "variable";
static const char* VariableType_Attr = "type";
static const char* VariableType_Attr_Independent = "independent";
static const char* VariableType_Attr_Dependent = "dependent";
static const char* VariableName_Attr = "name";
static const char* VariableDimension_Attr = "dim";
static const char* Row_Tag = "row";
static const char* ModelMapping_Tag = "modelMapping";
static const char* ModelMappingDataColumn_Attr = "dataColumn";
static const char* ModelMappingWeight_Attr = "weight";

static const char* ParameterDescription_Tag = "parameterDescription";
static const char* Parameter_Tag = "parameter";
static const char* ParameterName_Attr = "name";
static const char* ParameterLow_Attr = "low";
static const char* ParameterHigh_Attr = "high";
static const char* ParameterInit_Attr = "init";
static const char* ParameterScale_Attr = "scale";
static const char* ParameterBestValue_Attr = "bestValue";

static const char* ConstraintDescription_Tag = "constraintDescription";
static const char* Constraint_Tag = "constraint";
static const char* ConstraintType_Attr = "constraintType";
static const char* ConstraintType_Attr_LinearEquality = "linearEquality";
static const char* ConstraintType_Attr_LinearInequality = "linearInquality";
static const char* ConstraintType_Attr_NonlinearEquality = "nonlinearEquality";
static const char* ConstraintType_Attr_NonlinearInequality = "nonlinearInequality";

static const char* OptSolverResultSet_Tag = "optSolverResultSet";
static const char* OptSolverResultSetBestObjectiveFunction_Attr = "bestObjectiveFunction";
static const char* OptSolverResultSetNumObjectiveFunctionEvaluations_Attr = "numObjectiveFunctionEvaluations";
static const char* OptSolverResultSetStatus_Attr = "status";
static const char* OptSolverResultSetStatus_Attr_Unknown = "unknown";
static const char* OptSolverResultSetStatus_Attr_NormalTermination = "normalTermination";
static const char* OptSolverResultSetStatus_Attr_NonfeasibleLinear = "nonfeasibleLinear";
static const char* OptSolverResultSetStatus_Attr_NonfeasibleNonlinear = "nonfeasibleNonlinear";
static const char* OptSolverResultSetStatus_Attr_NoSolutionIterations = "noSolutionIterations";
static const char* OptSolverResultSetStatus_Attr_NoSolutionMachinePrecision = "noSolutionMachinePrecision";
static const char* OptSolverResultSetStatus_Attr_FailedConstructingD0 = "failedConstructingD0";
static const char* OptSolverResultSetStatus_Attr_FailedConstructingD1 = "failedConstructingD1";
static const char* OptSolverResultSetStatus_Attr_FailedInconsistentInput = "failedInconsistentInput";
static const char* OptSolverResultSetStatus_Attr_FailedIteratesStalled = "failedIteratesStalled";
static const char* OptSolverResultSetStatus_Attr_FailedPenaltyTooLarge = "failedPenaltyTooLarge";
static const char* OptSolverResultSetStatus_Attr_Failed = "failed";
static const char* OptSolverResultSetStatus_Attr_StoppedByUser = "stoppedByUser";
static const char* OptSolverResultSetStatusMessage_Attr = "statusMessage";


#endif