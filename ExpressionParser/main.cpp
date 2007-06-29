#include "Exception.h"
#include "ExpressionTest.h"
using namespace VCell;

int main(int argc, char** argv) {
	try {
		if (argc < 2) {
			cout << "Usage: " << argv[0] << " input " << endl;
			exit(1);
		}
		ExpressionTest::testParser(argv[1]);
	} catch (Exception& ex) {
		cout << "Parser:  exception during parse." << endl;
		cout << ex.getMessage() << endl;
		return -1;
	}
	
	return 0;
}