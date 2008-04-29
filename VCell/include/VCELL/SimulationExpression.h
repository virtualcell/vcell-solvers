/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef SIMULATIONEXPRESSION_H
#define SIMULATIONEXPRESSION_H

#include <VCELL/Simulation.h>
#include <SimpleSymbolTable.h>
#include <ScalarValueProxy.h>

enum VAR_INDEX {VAR_VOLUME_INDEX=0, VAR_MEMBRANE_INDEX, VAR_CONTOUR_INDEX, VAR_VOLUME_REGION_INDEX, VAR_MEMBRANE_REGION_INDEX,
    VAR_CONTOUR_REGION_INDEX};

#define NUM_VAR_INDEX (VAR_CONTOUR_REGION_INDEX + 1)

class FieldData;

class SimulationExpression : public Simulation
{
public:
	SimulationExpression(Mesh *mesh);
	~SimulationExpression();

	bool initSimulation();   // initializes to t=0
	void update();           // copies new to old values 
	void advanceTimeOn();
	void advanceTimeOff();

	void addFieldData(FieldData* fd) {
		fieldDataList.push_back(fd);
	}

	int getNumFields() { return (int)fieldDataList.size(); }
	string* getFieldSymbols();
	void populateFieldValues(double* darray, int index);

	int* getIndices() { return indices; };
	SimpleSymbolTable* getOldSymbolTable() { return oldSymbolTable; };
	void setCurrentCoordinate(WorldCoord& wc) {
		valueProxyX->setValue(wc.x);
		valueProxyY->setValue(wc.y);
		valueProxyZ->setValue(wc.z);
	}

	void addParameter(string& param);
	void setParameterValues(double* paramValues);

protected:
	SimpleSymbolTable* oldSymbolTable;

	int* indices;
	void createSymbolTable();

	ScalarValueProxy* valueProxyTime;
	ScalarValueProxy* valueProxyX;
	ScalarValueProxy* valueProxyY;
	ScalarValueProxy* valueProxyZ;
	vector<FieldData*> fieldDataList;

	vector<string> paramList;
	vector<ScalarValueProxy*> paramValueProxies;
};

#endif
