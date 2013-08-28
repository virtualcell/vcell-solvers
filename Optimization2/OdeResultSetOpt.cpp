#include <cstring> //memcpy
#include "OdeResultSetOpt.h"
#include "Expression.h"
#include "SymbolTable.h"
#include "Exception.h"

using namespace VCell;

OdeResultSetOpt::OdeResultSetOpt()
{
	weights = 0;
	numFunctionColumns = 0;
	numRowsAllocated = 0;
	numRowsUsed = 0;
	numDataColumns = 0;
	rowData = 0;
}

OdeResultSetOpt::OdeResultSetOpt(OdeResultSet* resultSetTemp)
{
	// columns
	vector<Column> columnTemp = resultSetTemp->getColumns();
	for (int i = 0; i < (int)columnTemp.size(); i ++) {
		if (columnTemp[i].expression == 0) {
			addColumn(columnTemp[i].name);
		} else {
			addFunctionColumn(columnTemp[i].name, columnTemp[i].expression->infix());
		}
	}
	// rows
	int numRows = resultSetTemp->getNumRows();
	int numColumns = resultSetTemp->getNumColumns();
	rowData = new double[numRows * numColumns];
	memcpy(rowData, resultSetTemp->getRowData(), numRows * numColumns * sizeof(double));
	// copy weights (in IDAWIN/OdeResultSet the weights are variable weights)
	if(resultSetTemp->getNumColumns() > 0)
	{
		int numVariables = resultSetTemp->getNumColumns() - 1;//exclude "t"
		double* vWeightData = new double[numVariables];
		for(int i=0; i<numVariables; i++)
		{
			vWeightData[i] = resultSetTemp -> getColumnWeight(i+1);
		}
		VariableWeights* vWeights = new VariableWeights(vWeightData, numVariables);

		setWeights(vWeights);
	};
}

OdeResultSetOpt::~OdeResultSetOpt()
{
	for (int i = 0; i < (int)columns.size(); i ++) {
		delete columns[i].expression;
	}
	columns.clear();
	delete weights;
	delete[] rowData;

}

void OdeResultSetOpt::addColumn(const string& aColumn) {
	if (numRowsAllocated != 0) {
		throw Exception("Can't add column when rowData is not empty");
	}
	columns.push_back(Column(aColumn, 0));
	numDataColumns ++;
}

void OdeResultSetOpt::bindFunctionExpression(SymbolTable* symbolTable) {
	for (int i = 0; i < (int)columns.size(); i ++) {
		if (columns[i].expression != 0) {
			columns[i].expression->bindExpression(symbolTable);
		}
	}
}

void OdeResultSetOpt::addFunctionColumn(const string& aColumn, const string& exp) {
	columns.push_back(Column(aColumn, new Expression(exp)));
	numFunctionColumns ++;
}

void OdeResultSetOpt::addRow(double* aRow) {
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

double* OdeResultSetOpt::getRowData(int index) {
	if (index >= numRowsUsed) {
		throw Exception("OdeResultSetOpt::getRowData(int index), row index is out of bounds");
	}
	return rowData + index * numDataColumns;
}

void OdeResultSetOpt::clearData() {
	numRowsUsed = 0;
	memset(rowData, 0, numRowsAllocated * numDataColumns * sizeof(double));
}

int OdeResultSetOpt::findColumn(const string& aColumn) {
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

//void OdeResultSetOpt::setColumnWeights(double* weights){
//	delete[] columnWeights;
//	columnWeights = new double[columns.size()];
//	memcpy(columnWeights, weights, columns.size() * sizeof(double));
//}

//double OdeResultSetOpt::getColumnWeight(int index) {
//	if (index >= (int)columns.size()) {
//		throw "OdeResultSetOpt::getColumnWeight(int index), column index is out of bounds";
//	}
//	return columnWeights[index];
//}

int OdeResultSetOpt::getNumColumns() {
	return (int)columns.size();
}

string& OdeResultSetOpt::getColumnName(int index) {
	if (index >= (int)columns.size()) {
		throw "OdeResultSetOpt::getColumnName(int index), column index is out of bounds";
	}
	return columns[index].name;
}

int OdeResultSetOpt::getNumRows() {
	return numRowsUsed;
}

Expression* OdeResultSetOpt::getColumnFunctionExpression(int columnIndex) {
	if (columnIndex >= (int)columns.size()) {
		throw "OdeResultSetOpt::getColumnFunctionExpression(), column index is out of bounds";
	}
	return columns[columnIndex].expression;
}

void OdeResultSetOpt::getColumnData(int columnIndex, int numParams, double* paramValues, double* colData) {
	int numCols = getNumColumns();
	if (columnIndex < 0 || columnIndex >= numCols){
		throw "OdeResultSetOpt::getColumnData(int columnIndex), columnIndex out of bounds";
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

void OdeResultSetOpt::copyInto(OdeResultSetOpt* otherOdeResultSet) {
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
	}
	// rows
	if (otherOdeResultSet->numRowsAllocated != numRowsAllocated) {
		delete[] otherOdeResultSet->rowData;
		otherOdeResultSet->rowData = new double[numRowsAllocated * numDataColumns];
		otherOdeResultSet->numRowsAllocated = numRowsAllocated;
	}
	otherOdeResultSet->numRowsUsed = numRowsUsed;
	memcpy(otherOdeResultSet->rowData, rowData, numRowsAllocated * numDataColumns * sizeof(double));
	// copy weights
	if(weights->getWeightType() == TIMEWEIGHT)
	{
		int numTWeightData = weights->getNumWeights();
		double* weightData = weights->getWeightData();
		double* newWeightData = new double[numTWeightData];
		memcpy(newWeightData, weightData, numTWeightData * sizeof(double));
		TimeWeights* tWeights = new TimeWeights(newWeightData, numTWeightData);

		otherOdeResultSet->setWeights(tWeights);
	}
	else if(weights->getWeightType() == VARIABLEWEIGHT)
	{
		int numVWeightData = weights->getNumWeights();
		double* weightData = weights->getWeightData();
		double* newWeightData = new double[numVWeightData];
		memcpy(newWeightData, weightData, numVWeightData * sizeof(double));
		VariableWeights* vWeights = new VariableWeights(newWeightData, numVWeightData);

		otherOdeResultSet->setWeights(vWeights);
	}
	else //elementWeight
	{
		int numEWeightData = weights->getNumWeights();
		int numWeightRows = ((ElementWeights*)weights)->getNumWeightRows();
		int numWeightCols = ((ElementWeights*)weights)->getNumWeightColumns();
		double* weightData = weights->getWeightData();
		double* newWeightData = new double[numEWeightData];
		memcpy(newWeightData, weightData, numEWeightData * sizeof(double));
		ElementWeights* eWeights = new ElementWeights(newWeightData, numWeightRows, numWeightCols );

		otherOdeResultSet->setWeights(eWeights);
	};
}

void OdeResultSetOpt::addEmptyRows(int numRowsToAdd) {
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
