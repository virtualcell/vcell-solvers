#include <cstdio> //sprintf
#include "MemoryManager.h"
extern "C" {
	#include "MemoryUtils.h"
}

#include <stdlib.h>
#ifndef MACOSX
#include <malloc.h>
#endif

#include <iostream>
using namespace std;

void* vc_calloc(int numElement, int sizeElement) {
	return MemoryManager::getInstance()->safeCalloc(numElement, sizeElement);
}

void vc_free(void* p) {
	MemoryManager::getInstance()->safeFree(p);
}

MemoryManager* MemoryManager::memoryManager = NULL;

MemoryManager::MemoryManager(void)
{
	heapSize = 0;
	maxHeapSize = 0;
}

MemoryManager::~MemoryManager(void)
{
}

MemoryManager* MemoryManager::getInstance() {
	if (memoryManager == 0) {
		memoryManager = new MemoryManager();
	}

	return memoryManager;
}

void MemoryManager::deleteInstance() {
	if (memoryManager != 0) {
		memoryManager->freeAll();
	}
	delete memoryManager;
	memoryManager = 0;
}

void* MemoryManager::safeCalloc(int numElement, int sizeElement) {
	if (heapSize + numElement * sizeElement > maxHeapSize) {
		char errMsg[256];
		sprintf(errMsg, "Memory limit exceeded [requested %ld bytes, max %ld bytes]!", heapSize + numElement * sizeElement, maxHeapSize);
		freeAll();
		throw errMsg;
	}
	void* p = calloc(numElement, sizeElement);
	if (!p) {
		char errMsg[256];
		sprintf(errMsg, "Memory allocation failed [requested %ld bytes]!", heapSize + numElement * sizeElement);
		freeAll();
		throw errMsg;
	}
	managedCallocs.push_back(new ManagedCalloc(p, numElement * sizeElement));
	heapSize += numElement * sizeElement;
	return p;
}

void MemoryManager::safeFree(void* p) {
	bool bFound = false;
	for (vector<ManagedCalloc*>::iterator iter = managedCallocs.begin(); iter < managedCallocs.end(); iter ++) {
		ManagedCalloc* managedCalloc = *iter;
		if (managedCalloc->pointer == p) {			
			managedCallocs.erase(iter);			
			heapSize -= managedCalloc->sizeInBytes;
			delete managedCalloc;
			free(p);
			bFound = true;			
			break;
		}
	}
	if (!bFound) {
		cout << "Can't free memory, location [" << p << "] not managed." << endl;
	}
}

void MemoryManager::freeAll() {
	for (vector<ManagedCalloc*>::iterator iter = managedCallocs.begin(); iter < managedCallocs.end(); iter ++) {
		ManagedCalloc* managedCalloc = *iter;
		free(managedCalloc->pointer);
		heapSize -= managedCalloc->sizeInBytes;
		delete managedCalloc;
	}
	if (heapSize != 0) {
		cout << "There is memory leak [" << heapSize << "]!" << endl;
	}
	managedCallocs.clear();
}

void MemoryManager::setMaxHeapSize(long s) {
	maxHeapSize = s;
}
