#include <SimpleSymbolTable.h>
#include "StoppedByUserException.h"
#include "VCellSundialsSolver.h"
#include "OdeResultSet.h"
#include <assert.h>
#include <math.h>
#include <sstream>
using std::stringstream;

#include <memory.h>

#ifdef USE_MESSAGING
#include <VCELL/SimulationMessaging.h>
#endif

static void trimString(string& str)
{
	string::size_type pos = str.find_last_not_of(" \r\n");
	if(pos != string::npos) {
		str.erase(pos + 1);
		pos = str.find_first_not_of(" \r\n");
		if(pos != string::npos) {
			str.erase(0, pos);
		}
	}
	else {
		str.erase(str.begin(), str.end());
	}
}

Expression* VCellSundialsSolver::readExpression(istream& inputstream) {
	string exp;
	getline(inputstream, exp);
	trimString(exp);
	if (*(exp.end() - 1) != ';') {
		throw VCell::Exception(BAD_EXPRESSION_MSG);
	}
	return new Expression(exp);
}

VCellSundialsSolver::VCellSundialsSolver() {
	NEQ = 0;
	NPARAM = 0;
	STARTING_TIME = 0.0;
	ENDING_TIME   = 0.0;
	RelativeTolerance = 0.0;
	AbsoluteTolerance = 0.0;
	keepEvery = 0;
	maxTimeStep = 0.0;		

	solver = 0;
	initialConditionSymbolTable = 0;
	initialConditionExpressions = 0;
	values = 0;
	tempRowData = 0;

	variableNames = 0;
	paramNames = 0;
	allSymbols = 0;
	numAllSymbols = 0;
	defaultSymbolTable = 0;

	numDiscontinuities = 0;
	odeDiscontinuities = 0;
	discontinuityValues = 0;
	discontinuitySymbolTable = 0;
	rootsFound = 0;

	odeResultSet = new OdeResultSet();	
	y = 0;

	events = 0;
	numEvents = 0;
}

VCellSundialsSolver::~VCellSundialsSolver() {
	N_VDestroy_Serial(y);

	for (int i = 0; i < NEQ; i ++) {
		delete initialConditionExpressions[i];
	}
	delete[] initialConditionExpressions;
	delete[] variableNames;
	delete[] paramNames;
	delete[] allSymbols;
	delete defaultSymbolTable;

	for (int i = 0; i < numDiscontinuities; i ++) {
		delete odeDiscontinuities[i];
	}
	delete[] odeDiscontinuities;
	delete[] rootsFound;
	delete[] discontinuityValues;
	delete discontinuitySymbolTable;
	
	delete[] values;
	delete[] tempRowData;
	delete initialConditionSymbolTable;
	delete odeResultSet;

	outputTimes.clear();

	for (int i = 0; i < numEvents; i ++) {
		delete events[i];
	}
	delete[] events;
}

void VCellSundialsSolver::updateTempRowData(double currTime) {
	tempRowData[0] = currTime; 
	for (int i = 0; i < NEQ; i++) { 
		tempRowData[i+1] = NV_Ith_S(y,i); 
	} 
}

void VCellSundialsSolver::writeData(double currTime, FILE* outputFile) {
	updateTempRowData(currTime);
	odeResultSet->addRow(tempRowData);
	writeFileData(outputFile);
}

void VCellSundialsSolver::writeFileData(FILE* outputFile) {
	if (outputFile != 0) {
		fprintf(outputFile, "%0.17E", tempRowData[0]); 
		for (int i = 1; i < NEQ+1; i++) { 
			fprintf(outputFile, "\t%0.17E", tempRowData[i]);
		} 
		fprintf(outputFile, "\n");
	}
}

void VCellSundialsSolver::writeFileHeader(FILE* outputFile) {
	if (outputFile != 0) {	
		//  Print header...
		for (int i = 0; i < odeResultSet->getNumColumns(); i++) {
			fprintf(outputFile, "%s:", odeResultSet->getColumnName(i).data());
		}
		fprintf(outputFile, "\n");
	}
}

void VCellSundialsSolver::printProgress(double currTime, double& percentile, double increment, FILE* outputFile) {
	fflush(outputFile);

	if (currTime == STARTING_TIME) { // print 0%
#ifdef USE_MESSAGING
		SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_PROGRESS, percentile, currTime));
#else
		printf("[[[progress:%lg%%]]]", percentile*100.0);
		fflush(stdout);
#endif
	} else {
		double oldp = percentile;
		while (true) {
			double midTime = (percentile + increment) * (ENDING_TIME - STARTING_TIME);
			if (STARTING_TIME + midTime > currTime) { 
				break;
			}
			percentile += increment;
		}
		if ( percentile != oldp) {
#ifdef USE_MESSAGING
			SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_PROGRESS, percentile, currTime));		
			SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_DATA, percentile, currTime));
#else
			printf("[[[progress:%lg%%]]]", percentile*100.0);
			printf("[[[data:%lg]]]", currTime); 
			fflush(stdout);
#endif
		}
	}
}

void VCellSundialsSolver::readInput(istream& inputstream) { 
	try {
		if (solver != 0) {
			throw "readInput should only be called once";
		}
		string name;
		while (!inputstream.eof()) {
			name = "";
			inputstream >> name;
			if (name.empty()) {
				continue;
			}			
			if (name == "SOLVER") {
				inputstream >> name;
				if (name != getSolverName()) {
					throw "Wrong solver";
				}
			} else if (name == "STARTING_TIME") {
				inputstream >> STARTING_TIME;
			} else if (name == "ENDING_TIME") {
				inputstream >> ENDING_TIME;
			} else if (name == "RELATIVE_TOLERANCE") {
				inputstream >> RelativeTolerance;
			} else if (name == "ABSOLUTE_TOLERANCE") {
				inputstream >> AbsoluteTolerance;
			} else if (name == "MAX_TIME_STEP") {
				inputstream >> maxTimeStep;
			} else if (name == "KEEP_EVERY") {
				inputstream >> keepEvery;
			} else if (name == "OUTPUT_TIME_STEP") {
				double outputTimeStep = 0.0;
				inputstream >> outputTimeStep;
				double timePoint = 0.0;
				int count = 1;
				while (STARTING_TIME + count * outputTimeStep < ENDING_TIME + 1E-12 * ENDING_TIME) {
					timePoint = STARTING_TIME + count * outputTimeStep;
					outputTimes.push_back(timePoint);
					count ++;
				}
				ENDING_TIME = outputTimes[outputTimes.size() - 1];
			} else if (name == "OUTPUT_TIMES") {
				int totalNumTimePoints;	
				double timePoint;
				inputstream >> totalNumTimePoints;
				for (int i = 0; i < totalNumTimePoints; i ++) {
					inputstream >> timePoint;
					if (timePoint > STARTING_TIME && timePoint <= ENDING_TIME) {
						outputTimes.push_back(timePoint);
					}
				}
				if (outputTimes[outputTimes.size() - 1] < ENDING_TIME) {
					outputTimes.push_back(ENDING_TIME);
				}
			} else if (name == "DISCONTINUITIES") {
				readDiscontinuities(inputstream);
			} else if (name == "NUM_PARAMETERS") {
				inputstream >> NPARAM;
				paramNames = new string[NPARAM];
				for (int i = 0; i < NPARAM; i ++) {
					inputstream >> paramNames[i];
				}				
			} else if (name == "NUM_EQUATIONS") {
				inputstream >> NEQ;
				variableNames = new string[NEQ];
				initialConditionExpressions = new Expression*[NEQ];
				readEquations(inputstream);				
			} else if (name == "EVENTS") {
				readEvents(inputstream);
			} else {
				string msg = "Unexpected token \"" + name + "\" in the input file!";
				throw VCell::Exception(msg);
			}
		}
		initialize();
	} catch (char* ex) {
		throw VCell::Exception(string("VCellSundialsSolver::readInput() : ") + ex);
	} catch (VCell::Exception& ex) {
		throw VCell::Exception(string("VCellSundialsSolver::readInput() : ") + ex.getMessage());
	} catch (...) {
		throw "VCellSundialsSolver::readInput() : caught unknown exception";
	}
}

void VCellSundialsSolver::readEvents(istream& inputstream) {
	string token;

	inputstream >> numEvents;
	events = new Event*[numEvents];
	for (int i = 0; i < numEvents; i ++) {
		events[i] = new Event();
		while (true) {
			inputstream >> token;
			if (token == "EVENT") {
				inputstream >> events[i]->name;
			} else if (token == "TRIGGER") {
				try {
					events[i]->triggerExpression = readExpression(inputstream);
				} catch (VCell::Exception& ex) {
					throw VCell::Exception(string("trigger expression") + " " + ex.getMessage());
				}
			} else if (token == "DELAY") {
				inputstream >> token;
				events[i]->bUseValuesAtTriggerTime = token == "true";
				try {
					events[i]->delayDurationExpression = readExpression(inputstream);
				} catch (VCell::Exception& ex) {
					throw VCell::Exception(string("delay duration expression") + " " + ex.getMessage());
				}
			} else if (token == "EVENTASSIGNMENTS") {
				inputstream >> events[i]->numEventAssignments;
				events[i]->eventAssignments = new EventAssignment*[events[i]->numEventAssignments];
				for (int j = 0; j < events[i]->numEventAssignments; j ++) {
					EventAssignment* ea = new EventAssignment();					
					inputstream >> ea->varIndex;
					try {
						ea->assignmentExpression = readExpression(inputstream);
					} catch (VCell::Exception& ex) {
						throw VCell::Exception(string("event assignment expression") + " " + ex.getMessage());
					}
					events[i]->eventAssignments[j] = ea;
				}
				break;
			} else {
				string msg = "Unexpected token \"" + token + "\" in the input file!";
				throw VCell::Exception(msg);
			}
		}
	}
}

void VCellSundialsSolver::readDiscontinuities(istream& inputstream) {
	inputstream >> numDiscontinuities;
	odeDiscontinuities = new OdeDiscontinuity*[numDiscontinuities];
	string line;
	string exp;
	for (int i = 0; i < numDiscontinuities; i ++) {
		OdeDiscontinuity* od = new OdeDiscontinuity;
		inputstream >> od->discontinuitySymbol;

		line = "";
		getline(inputstream, line);
		string::size_type pos = line.find(";");
		if (pos == string::npos) {
			string msg = string("discontinuity expression ") + BAD_EXPRESSION_MSG;
			throw VCell::Exception(msg);
		}
		exp = line.substr(0, pos + 1);
		trimString(exp);
		od->discontinuityExpression = new Expression(exp);

		exp = line.substr(pos + 1);
		trimString(exp);
		if (*(exp.end() - 1) != ';') {
			string msg = string("discontinuity root expression ") + BAD_EXPRESSION_MSG;
			throw VCell::Exception(msg);
		}
		od->rootFindingExpression = new Expression(exp);

		odeDiscontinuities[i] = od;
	}
}

void VCellSundialsSolver::initialize() {
	// add parameters to symbol table
	numAllSymbols = 1 + NEQ + NPARAM + numDiscontinuities;
	allSymbols = new string[numAllSymbols]; // t, variables, parameters, discontinuities

	//t
	allSymbols[0] = "t";
	odeResultSet->addColumn("t");
	// variables
	for (int i = 0 ; i < NEQ; i ++) {
		allSymbols[1 + i] = variableNames[i];
		odeResultSet->addColumn(variableNames[i]);
	}
	// parameters
	for (int i = 0 ; i < NPARAM; i ++) {
		allSymbols[1 + NEQ + i] = paramNames[i]; 
	}
	//discontinuities
	for (int i = 0 ; i < numDiscontinuities; i ++) {
		allSymbols[1 + NEQ + NPARAM + i] = odeDiscontinuities[i]->discontinuitySymbol;
	}
	// default symbol table has variables, parameters and discontinuities.
	defaultSymbolTable = new SimpleSymbolTable(allSymbols, numAllSymbols); 

	// initial condition can only be function of parameters.
	initialConditionSymbolTable = new SimpleSymbolTable(allSymbols + 1 + NEQ, NPARAM);
	for (int i = 0; i < NEQ; i ++) {
		initialConditionExpressions[i]->bindExpression(initialConditionSymbolTable);
	}

	if (numDiscontinuities > 0) {
		rootsFound = new int[2*numDiscontinuities];
		discontinuityValues = new double[numDiscontinuities];
		// discontinuities can't be function of discontinuity symbols
		discontinuitySymbolTable = new SimpleSymbolTable(allSymbols, 1 + NEQ + NPARAM);
		for (int i = 0; i < numDiscontinuities; i ++) {
			odeDiscontinuities[i]->discontinuityExpression->bindExpression(discontinuitySymbolTable);
			odeDiscontinuities[i]->rootFindingExpression->bindExpression(discontinuitySymbolTable);
		}
	}

	if (numEvents > 0) {
		for (int i = 0; i < numEvents; i ++) {
			events[i]->bind(defaultSymbolTable);
		}
	}
	try {
		values = new realtype[1 + NEQ + NPARAM + numDiscontinuities];
		tempRowData = new realtype[1 + NEQ];
	} catch (...) {
		throw "Out of Memory";
	}

	y = N_VNew_Serial(NEQ);
	if (y == 0) {
		throw "Out of Memory";
	}
}

bool VCellSundialsSolver::updateDiscontinuities(realtype t, bool bOnRootReturn) {
	if (numDiscontinuities == 0) {
		return false;
	}

	bool bUpdated = false;
	updateTandVariableValues(t, y);
	for (int i = 0; i < numDiscontinuities; i ++) {
		cout << odeDiscontinuities[i]->discontinuitySymbol << " " << odeDiscontinuities[i]->discontinuityExpression->infix() << " " << discontinuityValues[i];
		if (bOnRootReturn) {
			if (rootsFound[2 * i] && rootsFound[2 * i + 1]) {
				cout << " inverted ";
				discontinuityValues[i] = discontinuityValues[i] ? 0.0 : 1.0;
				bUpdated = true;
			} else if (rootsFound[2 * i] || rootsFound[2 * i + 1]) {
				cout << " evaluated ";
				double oldValue = discontinuityValues[i];
				discontinuityValues[i] = odeDiscontinuities[i]->discontinuityExpression->evaluateVector(values);
				if (oldValue != discontinuityValues[i]) {
					bUpdated = true;
				}
			} else {
				cout << " nonroot ";
			}
		} else {
			cout << " evaluated after event execution ";
			double oldValue = discontinuityValues[i];
			discontinuityValues[i] = odeDiscontinuities[i]->discontinuityExpression->evaluateVector(values);
			if (oldValue != discontinuityValues[i]) {
				bUpdated = true;
			}
		}
		cout << discontinuityValues[i] << endl;
	}
	cout << endl;

	// copy discontinuity values to values to evaluate RHS
	if (bUpdated) {
		memcpy(values + 1 + NEQ + NPARAM, discontinuityValues, numDiscontinuities * sizeof(double));
	}
	return bUpdated;
}

void VCellSundialsSolver::initDiscontinuities() {
	if (numDiscontinuities == 0) {
		return;
	}

	updateTandVariableValues(STARTING_TIME, y);
	// init discontinuities	
	for (int i = 0; i < numDiscontinuities; i ++) {
		discontinuityValues[i] = odeDiscontinuities[i]->discontinuityExpression->evaluateVector(values);
	}
	// copy discontinuity values to values to evaluate RHS
	memcpy(values + 1 + NEQ + NPARAM, discontinuityValues, numDiscontinuities * sizeof(double));
}

void VCellSundialsSolver::checkDiscontinuityConsistency() {
	if (numDiscontinuities == 0) {
		return;
	}

	for (int i = 0; i < numDiscontinuities; i ++) {
		double realValue = odeDiscontinuities[i]->discontinuityExpression->evaluateVector(values);
		if (discontinuityValues[i] != realValue) {
			stringstream ss;
			ss << "at time " << values[0] << ", discontinuity " << odeDiscontinuities[i]->discontinuityExpression->infix() << " evaluated to " << (realValue ? "TRUE" : "FALSE") << ", solver assumed " << (discontinuityValues[i] ? "TRUE" : "FALSE") << endl;
			throw ss.str();
		}
	}
}

void VCellSundialsSolver::solveInitialDiscontinuities(double t) {
	cout << "------------------solveInitialDiscontinuities--at-time-" << t << "--------------------------" << endl;
	bool bFoundRoot = false;
	string roots_at_initial_str = "";
	updateTandVariableValues(t, y);
	for (int i = 0; i < numDiscontinuities; i ++) {
		double v = odeDiscontinuities[i]->rootFindingExpression->evaluateVector(values);
		if (fabs(v) < AbsoluteTolerance) {
			roots_at_initial_str += odeDiscontinuities[i]->discontinuityExpression->infix() + "; ";
			bFoundRoot = true;
		}
	}

	if (bFoundRoot) {
		cout << "solveInitialDiscontinuities() : roots found at time " << t << "; " << roots_at_initial_str << endl;
		int count = 0;
		int maxCount = (int)pow(2.0, numDiscontinuities);
		while (count < maxCount) {
			try {
				if (!fixInitialDiscontinuities(t)) {
					break;
				}
			} catch (const char* err) {
				stringstream str;
				str << "found discontinuities at time " << t << " but unable to initialize : " << err << "\nDiscontinuities at time 0 are : " << roots_at_initial_str;
				throw str.str();
			}
			count ++;
		}
		if (count >= maxCount) {
			string str = "found discontinuities at time 0 but unable to initialize due to max iterations.\nDiscontinuities at time 0 are : " + roots_at_initial_str;
			throw str;
		}
	}
	cout << "------------------------------------------------" << endl;
}

void VCellSundialsSolver::printVariableValues(realtype t) {
	updateTandVariableValues(t, y);
	cout << "variable values are" << endl;
	cout << "t " << values[0] << endl;
	for (int i = 0; i < NEQ; i ++) {
		cout << variableNames[i] << " " << values[i + 1] << endl;
	}
	cout << endl;
}

void VCellSundialsSolver::printDiscontinuityValues() {
	cout << endl << "discontinuities values are" << endl;
	for (int i = 0; i < numDiscontinuities; i ++) {
		cout << odeDiscontinuities[i]->discontinuitySymbol << " " << odeDiscontinuities[i]->discontinuityExpression->infix() << " " << discontinuityValues[i] << endl;
	}
	cout << endl;
}

int VCellSundialsSolver::RootFn(realtype t, N_Vector y, realtype *gout) {
	updateTandVariableValues(t, y);
	//cout << "RootFn " << endl;
	//printVariableValues();

	for (int i = 0; i < numDiscontinuities; i ++) {
		double r = odeDiscontinuities[i]->rootFindingExpression->evaluateVector(values);
		if (r == 0) {
			gout[2 * i] = 1e-200;
			gout[2 * i + 1] = -1e-200;
		} else {
			gout[2 * i] = r;
			gout[2 * i + 1] = r;
		}		
		//cout << "gout[" << i << "]=" << gout[i] << endl;
	}	
	return 0;
}

void VCellSundialsSolver::testEventTriggers(realtype Time) {
	if (numEvents == 0) {
		return;
	}
	updateTandVariableValues(Time, y);
	for (int i = 0; i < numEvents; i ++) {
		bool oldTriggerValue = events[i]->triggerValue;
		bool newTriggerValue = events[i]->triggerExpression->evaluateVector(values) != 0.0;
		events[i]->triggerValue = newTriggerValue;
		if (!oldTriggerValue && newTriggerValue) { // triggered
			EventExecution* ee = new EventExecution(events[i]);
			ee->exeTime = Time;
			if (events[i]->hasDelay()) {
				ee->exeTime = Time + events[i]->delayDurationExpression->evaluateVector(values);
			}
			if (events[i]->bUseValuesAtTriggerTime) {
				ee->targetValues = new double[events[i]->numEventAssignments];
				for (int j = 0; j < events[i]->numEventAssignments; j ++) {
					ee->targetValues[j] = events[i]->eventAssignments[j]->assignmentExpression->evaluateVector(values);
				}
			}
			bool bInserted = false;
			for (list<EventExecution*>::iterator iter = eventExeList.begin(); iter != eventExeList.end(); iter ++) {
				if ((*iter)->exeTime > ee->exeTime) {
					eventExeList.insert(iter, ee); // sort them by execution time
					bInserted = true;
					break;
				}
			}
			if (!bInserted) {
				eventExeList.push_back(ee);
			}
		}
	}
}

bool VCellSundialsSolver::executeEvents(realtype Time) {
	if (numEvents == 0) {
		return false;
	}
	testEventTriggers(Time);

	static double epsilon = 1e-15;
	bool bExecuted = false;
	while (eventExeList.size() > 0) {
		list<EventExecution*>::iterator iter = eventExeList.begin();
		EventExecution* ee = *iter;

		if (ee->exeTime > Time + epsilon) { // not time yet
			return bExecuted;
		}

		if (fabs(ee->exeTime - Time) < epsilon) { // execute
			updateTandVariableValues(Time, y);
			double* y_data = NV_DATA_S(y); // assign the values
			for (int i = 0; i < ee->event0->numEventAssignments; i ++) {
				EventAssignment* ea = ee->event0->eventAssignments[i];
				if (ee->event0->bUseValuesAtTriggerTime) {
					y_data[ea->varIndex] = ee->targetValues[i];
				} else {					
					y_data[ea->varIndex] = ea->assignmentExpression->evaluateVector(values);
				}
			}
			cout << endl << "Executed event " << ee->event0->name << " at time " << Time << endl;
			eventExeList.pop_front(); // delete from the list
			delete ee;
			bExecuted = true;
			testEventTriggers(Time); // retest all triggers again.
		} else if (ee->exeTime < Time) {
			stringstream ss;
			ss << "missed Event '" << ee->event0->name << "' with trigger " << ee->event0->triggerExpression->infix() 
				<< ", scheduled time = " << ee->exeTime << ", current time = " << Time << endl;
			throw ss.str();
		}

	}
	return bExecuted;
}

double VCellSundialsSolver::getNextEventTime() {
	if (eventExeList.size() > 0) {
		list<EventExecution*>::iterator iter = eventExeList.begin();
		EventExecution* ee = *iter;
		return ee->exeTime;
	}

	return DBL_MAX;
}

void VCellSundialsSolver::checkStopRequested(double time, long numIterations) {
#ifdef USE_MESSAGING
	if (SimulationMessaging::getInstVar()->isStopRequested()) {
		throw StoppedByUserException("stopped by user");
	}
#endif
}
