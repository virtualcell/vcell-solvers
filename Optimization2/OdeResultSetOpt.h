#ifndef ODERESULTSETOPT_H
#define ODERESULTSETOPT_H

#include <string>
#include <vector>
#include "OdeResultSet.h"
#include "Weights.h"
#include "TimeWeights.h"
#include "VariableWeights.h"
#include "ElementWeights.h"
using namespace std;

namespace VCell {
	class Expression;
}
class SymbolTable;
class OdeResultSet;

class OdeResultSetOpt
{
public:
	OdeResultSetOpt();
	OdeResultSetOpt(OdeResultSet*);
	~OdeResultSetOpt();
	void addColumn(const string& aColumn);
	void addFunctionColumn(const string& aColumn, const string& columnExpression);
	void addRow(double* aRow);
		
	void bindFunctionExpression(SymbolTable* symbolTable);

	double* getRowData(int index);
	const double* getRowData() {
		return rowData;
	}

	int findColumn(const string& aColumn);
	Weights* getWeights(){ return weights; }
	void setWeights(Weights* argWeights){ weights = argWeights;}
	string& getColumnName(int index);	
	void getColumnData(int index, int numParams, double* paramValues, double* colData);

	int getNumColumns();
	int getNumRows();
	int getNumFunctionColumns() { return numFunctionColumns; }
	int getNumDataColumns() { return numDataColumns; }

	VCell::Expression* getColumnFunctionExpression(int columnIndex);
	void clearData();

	void copyInto(OdeResultSetOpt* otherOdeResultSet);
	void addEmptyRows(int numRowsToAdd);

private:
	// 0 : t
	// 1 ~ N : variable names;
	vector<Column> columns;
	double* rowData;
	Weights* weights;
	int numRowsAllocated;
	int numRowsUsed;
	int numFunctionColumns;
	int numDataColumns;
};

#endif
