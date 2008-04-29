#ifndef ParameterDescription_H
#define ParameterDescription_H

#include <string>
#include <vector>
using namespace std;

class SymbolTable;

class ParameterDescription {
public:
	virtual int getNumParameters() = 0;
	virtual void getUnscaledLimits(double *lower, double *upper) = 0;
	virtual void getScaledLimits(double *lower, double *upper) = 0;
	virtual void getUnscaledInitialGuess(double *x) = 0;
	virtual void getScaledInitialGuess(double *x) = 0;
	virtual const char* getParameterName(int index) = 0;
	virtual SymbolTable* getSymbolTable() = 0;
	virtual void getScales(double *x) = 0;
	virtual void unscaleX(const double* scaled_x, double* unscaled_x) = 0;
	virtual void scaleX(const double* unscaled_x, double* scaled_x) = 0;
};

class SimpleParameterDescription : public ParameterDescription {
public:
	SimpleParameterDescription(int arg_numParameters, 
		vector<string> paramNames, 
		double* arg_UnscaledLB, double* arg_UnscaledUB, double* arg_UnscaledInitialGuess,
		double* scales);

	~SimpleParameterDescription();

	virtual int getNumParameters() { return numParameters; }
	virtual void getUnscaledLimits(double *unscaled_lower, double *unscaled_upper);
	virtual void getScaledLimits(double *scaled_lower, double *scaled_upper);
	virtual void getUnscaledInitialGuess(double *x);
	virtual void getScaledInitialGuess(double *x);
	virtual void getScales(double *x);
	virtual void unscaleX(const double* scaled_x, double* unscaled_x);
	virtual void scaleX(const double* unscaled_x, double* scaled_x);

	virtual const char* getParameterName(int index) { return paramNames[index].c_str(); }
	SymbolTable* getSymbolTable() { return parameterSymbolTable; }

private:
	int numParameters;
	const double* unscaledLB, *unscaledUB, *unscaledInitGuess, *scales;
	vector<string> paramNames;
	SymbolTable* parameterSymbolTable;
};

#endif
