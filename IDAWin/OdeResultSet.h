#ifndef ODERESULTSET_H
#define ODERESULTSET_H

#include <string>
#include <vector>
using namespace std;

class Expression;
class SymbolTable;

struct Column {
	string name;
	Expression* expression;
	
	Column(string arg_name, Expression* exp) {
		name = arg_name;
		expression = exp;
	}
};

class OdeResultSet
{
public:
	OdeResultSet();
	~OdeResultSet();
	void addColumn(const string& aColumn);
	void addFunctionColumn(const string& aColumn, const string& columnExpression);
	void addRow(double* aRow);
	void setColumnWeights(double* weights);
	
	void bindFunctionExpression(SymbolTable* symbolTable);

	double* getRowData(int index);
	const double* getRowData() {
		return rowData;
	}

	int findColumn(const string& aColumn);
	double getColumnWeight(int index);
	string& getColumnName(int index);	
	void getColumnData(int index, int numParams, double* paramValues, double* colData);

	int getNumColumns();
	int getNumRows();
	int getNumFunctionColumns() { return numFunctionColumns; }
	int getNumDataColumns() { return numDataColumns; }

	Expression* getColumnFunctionExpression(int columnIndex);
	void clearData();

	void copyInto(OdeResultSet* otherOdeResultSet);
	void addEmptyRows(int numRowsToAdd);

private:
	// 0 : t
	// 1 ~ N : variable names;
	vector<Column> columns;
	double* columnWeights;
	double* rowData;
	int numRowsAllocated;
	int numRowsUsed;
	int numFunctionColumns;
	int numDataColumns;
};

#endif
