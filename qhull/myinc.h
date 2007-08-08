#include <string>
#include <iostream>
#include <vector>
#include <set>
#include <sstream>
using namespace std;

void my_fprintf(ostream* stream, const char* format, ...);
void my_fprintf_throw(ostream* stream, const char* format, ...);
void my_fputs(const char* arg_string, ostream* stream);
char * my_fgets(char *string, int n, istream* stream );
