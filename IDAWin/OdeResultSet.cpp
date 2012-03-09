#include "OdeResultSet.h"
#include "Expression.h"
#include "SymbolTable.h"
#include "Exception.h"
using namespace VCell;

#include <memory.h>

OdeResultSet::OdeResultSet()
{
	columnWeights = NULL;
	numFunctionColumns = 0;
	numRowsAllocated = 0;
	numRowsUsed = 0;
	numDataColumns = 0;
	rowData = 0;
}

OdeResultSet::~OdeResultSet()
{
	for (int i = 0; i < (int)columns.size(); i ++) {
		delete columns[i].expression;
	}
	columns.clear();
	delete[] rowData;
	delete[] columnWeights;
}

void OdeResultSet::addColumn(const string& aColumn) {
	if (numRowsAllocated != 0) {
		throw Exception("Can't add column when rowData is not empty");
	}
	columns.push_back(Column(aColumn, 0));
	numDataColumns ++;
}

void OdeResultSet::bindFunctionExpression(SymbolTable* symbolTable) {
	for (int i = 0; i < (int)columns.size(); i ++) {
		if (columns[i].expression != 0) {
			columns[i].expression->bindExpression(symbolTable);
		}
	}
}

void OdeResultSet::addFunctionColumn(const string& aColumn, const string& exp) {
	columns.push_back(Column(aColumn, new Expression(exp)));
	numFunctionColumns ++;
}

void OdeResultSet::addRow(double* aRow) {
	if (numRowsAllocated == 0) {
		numRowsAllocated = 500;
		rowData = new double[numRowsAllocated * numDataColumns];
		memset(rowData, 0, numRowsAllocated * numDataColumns * sizeof(double));
	} else if (numRowsAllocated == numRowsUsed) {
		int oldNumRowsAllocated = numRowsAllocated;
		double* oldRowData = rowData;
		numRowsAllocated += 500;		
		rowData = new double[numRowsAllocated * numDataColumns];
		memset(rowData, 0, numRowsAllocated * numDataColumns * sizeof(double));
		memcpy(rowData, oldRowData, oldNumRowsAllocated *  numDataColumns * sizeof(double));
		delete[] oldRowData;
	}
	int index = numRowsUsed * numDataColumns;
	for (int i = 0; i < numDataColumns; i ++, index ++) {	
		rowData[index] = aRow[i];		
	}
	numRowsUsed ++;
}

void OdeResultSet::setColumnWeights(double* weights){
	delete[] columnWeights;
	columnWeights = new double[columns.size()];
	memcpy(columnWeights, weights, columns.size() * sizeof(double));
}

double* OdeResultSet::getRowData(int index) {
	if (index >= numRowsUsed) {
		throw Exception("OdeResultSet::getRowData(int index), row index is out of bounds");
	}
	return rowData + index * numDataColumns;
}

void OdeResultSet::clearData() {
	numRowsUsed = 0;
	memset(rowData, 0, numRowsAllocated * numDataColumns * sizeof(double));
}

int OdeResultSet::findColumn(const string& aColumn) {
	int columnIndex = 0;
	for (vector<Column>::iterator iter = columns.begin(); iter < columns.end(); iter++) {
		if ((*iter).name == aColumn) {
			break;
		}
		columnIndex ++;
	}	
	if (columnIndex == columns.size()) {
		columnIndex = -1;
	}
	return columnIndex;
}

double OdeResultSet::getColumnWeight(int index) {
	if (index >= (int)columns.size()) {
		throw "OdeResultSet::getColumnWeight(int index), column index is out of bounds";
	}
	return columnWeights[index];
}

int OdeResultSet::getNumColumns() {
	return (int)columns.size();
}

string& OdeResultSet::getColumnName(int index) {
	if (index >= (int)columns.size()) {
		throw "OdeResultSet::getColumnName(int index), column index is out of bounds";
	}
	return columns[index].name;
}

int OdeResultSet::getNumRows() {
	return numRowsUsed;
}

Expression* OdeResultSet::getColumnFunctionExpression(int columnIndex) {
	if (columnIndex >= (int)columns.size()) {
		throw "OdeResultSet::getColumnFunctionExpression(), column index is out of bounds";
	}
	return columns[columnIndex].expression;
}

void OdeResultSet::getColumnData(int columnIndex, int numParams, double* paramValues, double* colData) {
	int numCols = getNumColumns();
	if (columnIndex < 0 || columnIndex >= numCols){
		throw "OdeResultSet::getColumnData(int columnIndex), columnIndex out of bounds";
	}
	if (columns[columnIndex].expression == 0) {
		for (int i = 0; i < numRowsUsed; i++){
			colData[i] = rowData[i * numDataColumns + columnIndex];
		}
	} else { // Function
		double* values = new double[numDataColumns + numParams];
		memcpy(values + numDataColumns, paramValues, numParams * sizeof(double));
		for (int i = 0; i < numRowsUsed; i++){
			memcpy(values, rowData + i * numDataColumns, numDataColumns * sizeof(double));
			colData[i] = columns[columnIndex].expression->evaluateVector(values);		
		}
		delete[] values;
	}
}

void OdeResultSet::copyInto(OdeResultSet* otherOdeResultSet) {
	// columns
	if (otherOdeResultSet->columns.size() != columns.size()) {
		for (int i = 0; i < (int)otherOdeResultSet->columns.size(); i ++) {
			delete otherOdeResultSet->columns[i].expression;
		}
		otherOdeResultSet->columns.clear();
		for (int i = 0; i < (int)columns.size(); i ++) {
			if (columns[i].expression == 0) {
				otherOdeResultSet->addColumn(columns[i].name);
			} else {
				otherOdeResultSet->addFunctionColumn(columns[i].name, columns[i].expression->infix());
			}
		}
		if (columnWeights != 0) {
			otherOdeResultSet->setColumnWeights(columnWeights);
		}
	}
	// rows
	if (otherOdeResultSet->numRowsAllocated != numRowsAllocated) {
		delete[] otherOdeResultSet->rowData;
		otherOdeResultSet->rowData = new double[numRowsAllocated * numDataColumns];
		otherOdeResultSet->numRowsAllocated = numRowsAllocated;
	}
	otherOdeResultSet->numRowsUsed = numRowsUsed;
	memcpy(otherOdeResultSet->rowData, rowData, numRowsAllocated * numDataColumns * sizeof(double));
}

void OdeResultSet::addEmptyRows(int numRowsToAdd) {
	if (numRowsAllocated < numRowsUsed + numRowsToAdd) {
		int oldNumRowsAllocated = numRowsAllocated;
		double* oldRowData = rowData;
		numRowsAllocated = numRowsUsed + numRowsToAdd;		
		rowData = new double[numRowsAllocated * numDataColumns];
		memset(rowData, 0, numRowsAllocated * numDataColumns * sizeof(double));
		memcpy(rowData, oldRowData, oldNumRowsAllocated *  numDataColumns * sizeof(double));
		delete[] oldRowData;
	} 
	numRowsUsed += numRowsToAdd;
}
