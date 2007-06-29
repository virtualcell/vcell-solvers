/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef REGION_H
#define REGION_H 

#include <VCELL/SimTypes.h>
#include <vector>
#include <string>
using namespace std;

class Mesh;

class Region
{
public:
	Region(Mesh *mesh);
	~Region();

	void setName(string& Aname);
	void setId(int AnId);

	string getName() { return name; }
	int     getId() { return id; }
	long    getNumElements() { return (int)index.size(); } 
	long    getIndex(long i) { return index[i]; } 
	void addIndex(long i);
	void setIndex(long i, long j) {index[i] = j;}

protected:
	vector<long> index;
	bool wasChanged;
	Mesh *mesh;

private:   
	string name;
	int id;
};

#endif
