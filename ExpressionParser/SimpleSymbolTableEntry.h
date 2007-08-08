#ifndef	SIMPLESYMBOLTABLEENTRY_H
#define SIMPLESYMBOLTABLEENTRY_H

#include "SymbolTable.h"

class SimpleSymbolTableEntry : public SymbolTableEntry
{
public:
	SimpleSymbolTableEntry(string nameValue, int indexVal, NameScope* namescopeVal, ValueProxy* proxyVal);
	~SimpleSymbolTableEntry(void);
	double getConstantValue();
	Expression* getExpression();
	int getIndex();
	string getName();    
	NameScope* getNameScope();
	//VCUnitDefinition getUnitDefinition()=0;
	boolean isConstant();	
	void setIndex(int symbolTableIndex);
	void setConstantValue(double v);
	ValueProxy* getValueProxy() { return valueProxy; };

private:
	string name;	
	int index;	
	NameScope* namescope;
	bool bConstant;
	double value;
	ValueProxy* valueProxy;
};
#endif
