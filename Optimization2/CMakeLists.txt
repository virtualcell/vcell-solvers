project(Optimization2)

#************************************************
# to include PdeObjectFunction
# uncomment the following line
# and include PdeObjectFunction.cpp in SRC_FILES
#
# ***********************************************
#add_definitions(INCLUDE_PDE_OPT)

set (SRC_FILES 
	Constraint.cpp
	ElementWeights.cpp
	ExplicitConstraintDescription.cpp
	ExplicitFitObjectiveFunction.cpp
	ExplicitObjectiveFunction.cpp
	MemoryManager.cpp
	OdeObjectiveFunction.cpp
	OdeResultSetOpt.cpp
	OptProblemDescription.cpp
	OptResultSet.cpp
	OptSolver2.cpp
	OptSolverCFSQP.cpp
	OptXmlReader2.cpp
	OptXmlWriter2.cpp
#	PdeObjectiveFunction.cpp
	SimpleParameterDescription.cpp
	SpatialReferenceData.cpp
	TimeWeights.cpp
	VariableWeights.cpp
	VFRAPObjectiveFunction.cpp
)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/../ExpressionParser
	${CMAKE_CURRENT_SOURCE_DIR}/../fsqp
	${CMAKE_CURRENT_SOURCE_DIR}/../IDAWin
	${CMAKE_CURRENT_SOURCE_DIR}/../Optimization2
	${CMAKE_CURRENT_SOURCE_DIR}/../sundials/include 
	${CMAKE_CURRENT_SOURCE_DIR}/../sundials/src
	${CMAKE_CURRENT_SOURCE_DIR}/../tinyxml
)

add_library(vcellOpt STATIC ${SRC_FILES})
