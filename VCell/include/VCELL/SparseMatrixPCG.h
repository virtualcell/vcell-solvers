#ifndef SPARSEMATRIXPCG
#define SPARSEMATRIXPCG

#include <VCELL/SimTypes.h>

/*----------------------------------------------------------------------------
	Sparse matrix stored in PCGPAK2 form
	SparseMatrixPCG contains all the diagonal elements and all the non-zero off diagonal elements
	numNonZeros is the number of diagonal elements plus non-zero off diagonal elements

	If SparseMatrixPCG needs to be passed to Fortran, each element of ija must be incremented by 1.
 --------------------------------------------------------------------------------------*/
class SparseMatrixPCG
{
public:
	SparseMatrixPCG(long N, long NonZ, int flg);
	SparseMatrixPCG(SparseMatrixPCG* sm);
	~SparseMatrixPCG();

	void setsa(long index, double value);
	void setija(long index, long value);

	long getN() { return N; }
	long getNumNonZeros() {return numNonZeros;};
	int  getSymmetricFlag() { return symmflag; }    
	double getValue(long i, long j);
	void setValue(long i, long j, double value);
	void show();

	void addNewRow();
	void setCol(long col, double value);
	void setRow(double diag, int numCols, int* cols, double* values);

	void setDiag(long row, double value);
	void close();
	int32* getFortranIJA();
	double* getsa() { return sa; };
	int getColumns(long i, int32*& columns, double*& values);
	void clear();

private:
    long N;
	long numNonZeros;
    int symmflag;
    double *sa;
    int32 *ija;
	int32* fortranIJA;
	long currentRow;
	long currentCol;
	long currentIndex;
};

#endif
