/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef SIMULATIONEXPRESSION_H
#define SIMULATIONEXPRESSION_H

#include <VCELL/Simulation.h>
#include <VCELL/SimTypes.h>

enum VAR_INDEX {VAR_VOLUME_INDEX=0, VAR_MEMBRANE_INDEX, VAR_CONTOUR_INDEX, VAR_VOLUME_REGION_INDEX, VAR_MEMBRANE_REGION_INDEX,
    VAR_CONTOUR_REGION_INDEX};

#define NUM_VAR_INDEX (VAR_CONTOUR_REGION_INDEX + 1)

class FieldData;
class MembraneVariable;
class MembraneRegionVariable;
class VolumeRegionVariable;
class RandomVariable;
class RegionSizeVariable;
class SymbolTable;
class ScalarValueProxy;

class SimulationExpression : public Simulation
{
public:
	SimulationExpression(Mesh *mesh);
	~SimulationExpression();

	void resolveReferences(); // create symbol table
	void update();           // copies new to old values 
	void advanceTimeOn();
	void advanceTimeOff();

	void writeData(char *filename, bool bCompress);

	void addFieldData(FieldData* fd) {
		fieldDataList.push_back(fd);
	}
	int getNumFields() { return (int)fieldDataList.size(); }
	string* getFieldSymbols();
	void populateFieldValues(double* darray, int index);

	void addRandomVariable(RandomVariable* rv) {
		randomVarList.push_back(rv);
	}
	RandomVariable* getRandomVariableFromName(char* rvname);
	int getNumRandomVariables() {
		return (int)randomVarList.size();
	}
	RandomVariable* getRandomVariable(int index) {
		return randomVarList[index];
	}
	void populateRandomValues(double* darray, int index);

	int* getIndices() { return indices; };
	SymbolTable* getSymbolTable() { 
		return symbolTable; 
	};
	void setCurrentCoordinate(WorldCoord& wc);

	bool isVolumeVariableDefinedInRegion(int volVarIndex, int regionIndex) {
		if (volVariableRegionMap[volVarIndex] == 0) {
			return true;
		}
		return volVariableRegionMap[volVarIndex][regionIndex];
	}

	void addParameter(string& param);
	void setParameterValues(double* paramValues);
	int getNumParameters() {
		return (int)paramList.size();
	}

	void addSerialScanParameter(string& param);
	void setSerialScanParameterValues(double* serialScanParamValues);
	int getNumSerialScanParameters() {
		return (int)serialScanParamList.size();
	}
	void populateSerialScanParameterValues(double* darray);

	// right now bSolveRegion is only applicable for volume variables
	void addVariable(Variable *var, bool* bSolveRegions=0);

	void addVolumeVariable(VolumeVariable *var, bool* bSolveRegions);
	void addMembraneVariable(MembraneVariable *var);
	void addVolumeRegionVariable(VolumeRegionVariable *var);
	void addMembraneRegionVariable(MembraneRegionVariable *var);

	double* getDiscontinuityTimes() { return discontinuityTimes; }
	void setDiscontinuityTimes(double* stopTimes) { discontinuityTimes=stopTimes; }

	int getNumVolVariables() { return volVarSize; }
	VolumeVariable* getVolVariable(int i) { return volVarList[i]; }

	int getNumMemVariables() { return memVarSize; }
	MembraneVariable* getMemVariable(int i) { return memVarList[i]; }

	int getNumVolRegionVariables() { return volRegionVarSize; }
	VolumeRegionVariable* getVolRegionVariable(int i) { return volRegionVarList[i]; }

	int getNumMemRegionVariables() { return memRegionVarSize; }
	MembraneRegionVariable* getMemRegionVariable(int i) { return memRegionVarList[i]; }

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
	bool isParameter(string& symbol); // can be serial scan parameter or opt parameter
	bool isVariable(string& symbol);

	int getNumRegionSizeVariables() {
		return numRegionSizeVars;
	}
	RegionSizeVariable* getRegionSizeVariable(int index) {
		return regionSizeVarList[index];
	}
	void populateRegionSizeVariableValues(double* darray, bool bVolumeRegion, int regionIndex);

private:
	SymbolTable* symbolTable;

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

	VolumeVariable** volVarList;
	int volVarSize;
	MembraneVariable** memVarList;
	int memVarSize;
	VolumeRegionVariable** volRegionVarList;
	int volRegionVarSize;
	MembraneRegionVariable** memRegionVarList;
	int memRegionVarSize;

	vector<RandomVariable*> randomVarList;

	int numRegionSizeVars;
	RegionSizeVariable** regionSizeVarList;

	int numVolPde;
	int numMemPde;
	bool bHasTimeDependentDiffusionAdvection;

	int psfFieldDataIndex;

	vector<string> serialScanParamList;
	vector<ScalarValueProxy*> serialScanParamValueProxies;

	void reinitConstantValues();
};

#endif
