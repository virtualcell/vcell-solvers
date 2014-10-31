#include <iostream>
#include <cstdlib>
/**
* on GCC, it appears some static initialization is corrupting the heap. This test file 
* is used to track that down.
*/

/*
#include <vcellxml.h>
#include <vhdf5/file.h>
#include <MovingBoundaryParabolicProblem.h>
#include <HDF5Client.h>
#include <Logger.h>
#include <boundaryProviders.h>
#include <MBridge/MatlabDebug.h>
using tinyxml2::XMLElement;
*/
int main(int argc, char *argv[])
{
	size_t s = 256600;
	if (argc > 1) {
		s = std::strtoul(argv[1],nullptr,10);
	}
	std::cout << "allocating " << s << std::endl;
	void * raw = malloc(s);
	free(raw);
	std::cout << "allocation compete" << std::endl;
	return 0;
}
