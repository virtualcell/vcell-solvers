#ifndef STRUCTURE_H
#define STRUCTURE_H

#include <VCELL/SimTypes.h>
#include <VCELL/Variable.h>
#include <vector>
#include <string>
using std::string;
using std::vector;

class Region;
class FastSystem;

class Structure
{
public:
	Structure(string& Aname);
	virtual ~Structure(void);

	virtual BoundaryType getXmBoundaryType() { return boundaryType[0]; }
	virtual BoundaryType getXpBoundaryType() { return boundaryType[1]; }
	virtual BoundaryType getYmBoundaryType() { return boundaryType[2]; }
	virtual BoundaryType getYpBoundaryType() { return boundaryType[3]; }
	virtual BoundaryType getZmBoundaryType() { return boundaryType[4]; }
	virtual BoundaryType getZpBoundaryType() { return boundaryType[5]; }
	     
	virtual void setXmBoundaryType(BoundaryType bt) { boundaryType[0] = bt; }
	virtual void setXpBoundaryType(BoundaryType bt) { boundaryType[1] = bt; }
	virtual void setYmBoundaryType(BoundaryType bt) { boundaryType[2] = bt; }
	virtual void setYpBoundaryType(BoundaryType bt) { boundaryType[3] = bt; }
	virtual void setZmBoundaryType(BoundaryType bt) { boundaryType[4] = bt; }
	virtual void setZpBoundaryType(BoundaryType bt) { boundaryType[5] = bt; }

	const string& getName() { return name; }

	void addDefinedVariable(Variable* var) {
		definedVariableList.push_back(var);
		if (var->isDiffusing())
		{
			++ pdeVarCount; 
		}
		else
		{
			++ odeVarCount;
		}
	}

	bool isVariableDefined(Variable* var){
		for (int i = 0; i < definedVariableList.size(); i ++) {
			if (var == definedVariableList[i]) {
				return true;
			}
		}
		return false;
	}
	int getNumDefinedVariables() {
		return definedVariableList.size();
	}
	Variable* getDefinedVariable(int i) {
		return definedVariableList[i];
	}

	const BoundaryType* getBoundaryTypes() {
		return boundaryType;
	}
	int getPdeVarCount()
	{
		return pdeVarCount;
	}
protected:
	string  name;
	vector<Variable*> definedVariableList;

private:
	int odeVarCount;
	int pdeVarCount;
	BoundaryType boundaryType[6];
};

#endif
