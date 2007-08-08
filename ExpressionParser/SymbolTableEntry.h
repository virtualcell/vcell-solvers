#ifndef SYMBOLTABLEENTRY_H
#define SYMBOLTABLEENTRY_H

#include "stdinc.h"
#include "ValueProxy.h"

class Expression;
class NameScope;

class SymbolTableEntry {
public:
	virtual double getConstantValue()=0;
	virtual Expression* getExpression()=0;
	virtual int getIndex()=0;
	virtual string getName()=0;    
	virtual NameScope* getNameScope()=0;
	//VCUnitDefinition getUnitDefinition()=0;
	virtual boolean isConstant()=0;
	virtual ValueProxy* getValueProxy()=0;
};

#endif
