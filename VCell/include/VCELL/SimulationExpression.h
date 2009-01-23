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
class MembraneVariable;
class MembraneRegionVariable;
class VolumeRegionVariable;

class SimulationExpression : public Simulation
{
public:
	SimulationExpression(Mesh *mesh);
	~SimulationExpression();

	void initSimulation();   // initializes to t=0
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

	bool isVolumeVariableDefinedInRegion(int volVarIndex, int regionIndex) {
		if (volVariableRegionMap[volVarIndex] == 0) {
			return true;
		}
		return volVariableRegionMap[volVarIndex][regionIndex];
	}

	void addParameter(string& param);
	void setParameterValues(double* paramValues);

	// right now bSolveRegion is only applicable for volume variables
	virtual void addVariable(Variable *var, bool* bSolveRegions=0);

	double* getDiscontinuityTimes() { return discontinuityTimes; }
	void setDiscontinuityTimes(double* stopTimes) { discontinuityTimes=stopTimes; }

	int getNumVolVariables() { return (int)volVarList.size(); }
	VolumeVariable* getVolVariable(int i) { return volVarList.at(i); }

	int getNumMemVariables() { return (int)memVarList.size(); }
	MembraneVariable* getMemVariable(int i) { return memVarList.at(i); }

	int getNumVolRegionVariables() { return (int)volRegionVarList.size(); }
	VolumeRegionVariable* getVolRegionVariable(int i) { return volRegionVarList.at(i); }

	int getNumMemRegionVariables() { return (int)memRegionVarList.size(); }
	MembraneRegionVariable* getMemRegionVariable(int i) { return memRegionVarList.at(i); }

	int getNumMemPde() { return numMemPde; }
	int getNumVolPde() { return numVolPde; }

	void setHasTimeDependentDiffusionAdvection() { bHasTimeDependentDiffusionAdvection = true; }
	bool hasTimeDependentDiffusionAdvection() { return bHasTimeDependentDiffusionAdvection; }

	void setPSFFieldDataIndex(int idx) {
		psfFieldDataIndex = idx;
	}

	FieldData* getPSFFieldData() {
		if (psfFieldDataIndex >= 0) {
			return fieldDataList[psfFieldDataIndex];
		}

		return 0;
	}

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

	vector<bool*> volVariableRegionMap;
	double* discontinuityTimes;

	vector<VolumeVariable*> volVarList;
	vector<MembraneVariable*> memVarList;
	vector<VolumeRegionVariable*> volRegionVarList;
	vector<MembraneRegionVariable*> memRegionVarList;

	int numVolPde;
	int numMemPde;
	bool bHasTimeDependentDiffusionAdvection;

	int psfFieldDataIndex;
};

#endif
