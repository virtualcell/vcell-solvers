#ifndef MEMORYMANAGER_H
#define MEMORYMANAGER_H

#include <vector>
using namespace std;

struct ManagedCalloc {
	void* pointer;
	int sizeInBytes;

	ManagedCalloc(void* p, int sib) {
		pointer = p;
		sizeInBytes = sib;
	}
};

class MemoryManager
{
public:	
	~MemoryManager(void);
	static MemoryManager* getInstance();
	static void deleteInstance();
	void* safeCalloc(int numElement, int sizeElement);
	void safeFree(void* p);	
	void freeAll();	
	void setMaxHeapSize(long s);
	long getMaxHeapSize() { return maxHeapSize; }
	long getHeapSize() { return heapSize; }	

private:
	MemoryManager(void);

	static MemoryManager* memoryManager;
	long maxHeapSize;	
	vector<ManagedCalloc*> managedCallocs;
	long heapSize;
};

#endif
