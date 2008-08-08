#include "Expression.h"
#include "SimpleSymbolTable.h"
#include "Exception.h"
#include "StoppedByUserException.h"
#include "VCellSundialsSolver.h"
#include "OdeResultSet.h"
#include <assert.h>

char* trim(char* str) {	
	int leftIndex, rightIndex;
	int len = (int)strlen(str);
	for (leftIndex = 0; leftIndex < len; leftIndex ++) { // remove leading spaces
		char c = str[leftIndex];
		if (c != ' ' && c != '\n' && c != '\r') {
			break;
		}
	}
	for (rightIndex = len - 1; rightIndex >= 0; rightIndex --) { // remove trailing spaces and new line and carriage return		
		char c = str[rightIndex];
		if (c != ' ' && c != '\n' && c != '\r') {
			break;
		}
	}

	len = rightIndex - leftIndex + 2;
	if (len <= 0) {
		return 0;
	}

	char* newstr = new char[len];
	memset(newstr, 0, len * sizeof(char));
	strncpy(newstr, str + leftIndex, len - 1);

	return newstr;
}

void trimString(string& str)
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

	numDiscontinuities = 0;
	odeDiscontinuities = 0;
	discontinuityValues = 0;
	discontinuitySymbolTable = 0;
	rootsFound = 0;

	odeResultSet = new OdeResultSet();	
	y = 0;
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

	for (int i = 0; i < numDiscontinuities; i ++) {
		delete odeDiscontinuities[i]->discontinuityExpression;
		delete odeDiscontinuities[i]->rootFindingExpression;
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

void VCellSundialsSolver::printProgress(double currTime, double& percentile, double increment) {
	if (currTime == STARTING_TIME) { // print 0%
		printf("[[[progress:%lg%%]]]", percentile*100.0); 
		fflush(stdout);		
	} else {
		while ((STARTING_TIME + ((percentile + increment) * (ENDING_TIME - STARTING_TIME))) <= currTime) { 
			percentile += increment; 
			printf("[[[progress:%lg%%]]]", percentile*100.0); 
			fflush(stdout); 
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
			if (name == "STARTING_TIME") {
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
			} else {
				string msg = "Unexpected token \"" + name + "\" in the input file!";
				throw Exception(msg);
			}
		}
		initialize();
	} catch (char* ex) {
		throw Exception(string("VCellSundialsSolver::readInput() : ") + ex);
	} catch (Exception& ex) {
		throw Exception(string("VCellSundialsSolver::readInput() : ") + ex.getMessage());
	} catch (...) {
		throw "VCellSundialsSolver::readInput() : caught unknown exception";
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
			throw Exception(msg);
		}
		exp = line.substr(0, pos + 1);
		trimString(exp);
		od->discontinuityExpression = new Expression(exp);

		exp = line.substr(pos + 1);
		trimString(exp);
		if (*(exp.end() - 1) != ';') {
			string msg = string("discontinuity expression ") + BAD_EXPRESSION_MSG;
			throw Exception(msg);
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
	// initial condition can only be function of parameters.
	initialConditionSymbolTable = new SimpleSymbolTable(allSymbols + 1 + NEQ, NPARAM);
	for (int i = 0; i < NEQ; i ++) {
		initialConditionExpressions[i]->bindExpression(initialConditionSymbolTable);			
	}

	rootsFound = new int[numDiscontinuities];
	discontinuityValues = new double[numDiscontinuities];
	// discontinuities can't be function of discontinuity symbols
	discontinuitySymbolTable = new SimpleSymbolTable(allSymbols, 1 + NEQ + NPARAM);
	for (int i = 0; i < numDiscontinuities; i ++) {
		odeDiscontinuities[i]->discontinuityExpression->bindExpression(discontinuitySymbolTable);
		odeDiscontinuities[i]->rootFindingExpression->bindExpression(discontinuitySymbolTable);
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

void VCellSundialsSolver::updateDiscontinuities() {
	cout << endl << "updateDiscontinuities" << endl;
	for (int i = 0; i < numDiscontinuities; i ++) {
		cout << odeDiscontinuities[i]->discontinuitySymbol << " " << odeDiscontinuities[i]->discontinuityExpression->infix() << " " << discontinuityValues[i] << " ";
		if (rootsFound[i]) {
			discontinuityValues[i] = discontinuityValues[i] ? 0.0 : 1.0;
		}
		cout << discontinuityValues[i] << endl;
	}
	// copy discontinuity values to values to evaluate RHS
	memcpy(values + 1 + NEQ + NPARAM, discontinuityValues, numDiscontinuities * sizeof(double));
}

void VCellSundialsSolver::initDiscontinuities() {
	// init discontinuities	
	for (int i = 0; i < numDiscontinuities; i ++) {
		discontinuityValues[i] = odeDiscontinuities[i]->discontinuityExpression->evaluateVector(values);
	}
	// copy discontinuity values to values to evaluate RHS
	memcpy(values + 1 + NEQ + NPARAM, discontinuityValues, numDiscontinuities * sizeof(double));
}

void VCellSundialsSolver::checkDiscontinuityConsistency(realtype t, N_Vector y) {
	if (numDiscontinuities == 0) {
		return;
	}

	values[0] = t;
	memcpy(values + 1, NV_DATA_S(y), NEQ * sizeof(realtype));
	for (int i = 0; i < numDiscontinuities; i ++) {
		double realValue = odeDiscontinuities[i]->discontinuityExpression->evaluateVector(values);
		if (discontinuityValues[i] != realValue) {
			stringstream ss;
			ss << "at time " << t << ", discontinuity " << odeDiscontinuities[i]->discontinuityExpression->infix() << " evaluated to " << (realValue ? "TRUE" : "FALSE") << ", solver assumed " << (discontinuityValues[i] ? "TRUE" : "FALSE") << endl;
			throw ss.str();
		}
	}
}
