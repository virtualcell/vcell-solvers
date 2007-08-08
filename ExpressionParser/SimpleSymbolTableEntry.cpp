#include "SimpleSymbolTableEntry.h"
#include "ExpressionException.h"

SimpleSymbolTableEntry::SimpleSymbolTableEntry(string nameValue, int indexVal, NameScope* namescopeVal, ValueProxy* proxyVal) : name(nameValue), index(indexVal), namescope(namescopeVal), valueProxy(proxyVal)
{
	bConstant = false;
	value = 0.0;
}

SimpleSymbolTableEntry::~SimpleSymbolTableEntry(void)
{
}

double SimpleSymbolTableEntry::getConstantValue() {
	if (bConstant){
		return value;
	}

	throw ExpressionException("can't evaluate to constant");
}

Expression* SimpleSymbolTableEntry::getExpression() {
	return null;
}

int SimpleSymbolTableEntry::getIndex() {
	return index;
}

string SimpleSymbolTableEntry::getName() {
	return name;
}

NameScope* SimpleSymbolTableEntry::getNameScope() {
	return namescope;
}

boolean SimpleSymbolTableEntry::isConstant() {
	return bConstant;
}

void SimpleSymbolTableEntry::setIndex(int indexVal)
{
	index = indexVal;
}

void SimpleSymbolTableEntry::setConstantValue(double v)
{
	value = v;
	bConstant = true;
}
