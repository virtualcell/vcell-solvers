#ifndef PARAMETERDESCRIPTION_H
#define PARAMETERDESCRIPTION_H

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

#endif
