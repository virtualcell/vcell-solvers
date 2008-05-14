#ifndef OPTPROBLEMDESCRIPTION_H
#define OPTPROBLEMDESCRIPTION_H

class ConstraintDescription;
class ParameterDescription;
class ObjectiveFunction;

class OptProblemDescription {
public:
	OptProblemDescription(ParameterDescription *parameterDesc,
				ConstraintDescription *constraintDesc,
				ObjectiveFunction *objectiveFunct);
	~OptProblemDescription();

	ParameterDescription *getParameterDescription() { return parameterDescription; }
	ConstraintDescription *getConstraintDescription() { return constraintDescription; }
	ObjectiveFunction *getObjectiveFunction() { return objectiveFunction; }

private:
	ParameterDescription *parameterDescription;
	ConstraintDescription *constraintDescription;
	ObjectiveFunction *objectiveFunction;
};

#endif
