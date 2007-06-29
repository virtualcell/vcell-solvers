#include "VCellIDASolver.h"
#include "OdeResultSet.h"
#include "Exception.h"
using namespace VCell;
#include <stdio.h>

int main(int argc, char *argv[]) {
	if (argc < 3) {
		fprintf(stderr, "Usage: IDAStandalone input output\n");
		return(1);
	}

	FILE* outputFile = NULL;
	ifstream inputstream(argv[1]);
	try {		
		if (!inputstream.is_open()) {
			throw Exception(string("IDA input file [") + argv[1] + "] doens't exit!");
		}

		// Open the output file...		
		if ((outputFile = fopen(argv[2], "w")) == NULL) {
			throw Exception(string("Could not open output file[") +  argv[2] + "] for writing.");
		}
		VCellIDASolver* idaSolver = new VCellIDASolver(inputstream, true);
		idaSolver->solve(0, outputFile);				

		fclose(outputFile);
		inputstream.close();
		delete idaSolver;
		return 0;
	} catch (Exception& ex) {
		if (outputFile != NULL) {
			fclose(outputFile);
		}
		if (inputstream.is_open()) {
			inputstream.close();
		}
		cerr << "Solver exception:\n\t " << ex.getMessage() << endl;
		return -1;
	} catch (...) {
		if (outputFile != NULL) {
			fclose(outputFile);
		}
		if (inputstream.is_open()) {
			inputstream.close();
		}
		cerr << "Unknown solver exception." << endl;
		return -1;
	}
}
