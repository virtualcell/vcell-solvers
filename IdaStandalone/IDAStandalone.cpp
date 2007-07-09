#include "VCellIDASolver.h"
#include "OdeResultSet.h"
#include "Exception.h"
using namespace VCell;
#include <stdio.h>

#define CLOSE_FILES 		\
		if (outputFile != NULL) {\
			fclose(outputFile);\
		}\
		if (inputstream.is_open()) {\
			inputstream.close();\
		}

int main(int argc, char *argv[]) {
	if (argc < 3) {
		cerr << "Usage: IDAStandalone input output" << endl;
		return 1;
	}

	FILE* outputFile = NULL;
	ifstream inputstream(argv[1]);
	try {		
		if (!inputstream.is_open()) {
			throw Exception(string("input file [") + argv[1] + "] doens't exit!");
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

	} catch (const char* ex) {
		CLOSE_FILES
		cerr << "IDAStandalone failed : " << ex << endl;
		return -1;
	} catch (string& ex) {
		CLOSE_FILES
		cerr << "IDAStandalone failed : " << ex << endl;
		return -1;
	} catch (Exception& ex) {
		CLOSE_FILES
		cerr << "IDAStandalone failed : " << ex.getMessage() << endl;
		return -1;
	} catch (...) {
		CLOSE_FILES
		cerr << "IDAStandalone failed : unknown error." << endl;
		return -1;
	}
}
