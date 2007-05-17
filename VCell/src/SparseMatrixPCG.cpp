#ifdef WIN32
#include <Windows.h>
#else
#include <UnixDefs.h>
#endif

#include <string.h>
#include <stdio.h>
#include <VCELL/SimTypes.h>
#include <VCELL/SparseMatrixPCG.h>
#include <iostream>
#include <fstream>
using namespace std;

/*----------------------------------------------------------------------------
	SparseMatrixPCG contains all the diagonal elements and all the non-zero off diagonal elements
	numNonZeros is the number of diagonal elements plus non-zero off diagonal elements
 --------------------------------------------------------------------------------------*/
SparseMatrixPCG::SparseMatrixPCG(long arg_N, long arg_numNonZeros, int arg_symmflag)
{
	ASSERTION(arg_N > 0);
	ASSERTION(arg_numNonZeros > 0);

	N = arg_N;
	numNonZeros = arg_numNonZeros;
	symmflag = arg_symmflag;

	try {
		sa = new double[numNonZeros + 1];
		ija = new INT32[numNonZeros + 1];
	} catch (...) {
		throw "Out of memory";
	}	

	clear();
	fortranIJA = NULL;
}

SparseMatrixPCG::SparseMatrixPCG(SparseMatrixPCG* sm)
{
	N = sm->N;
	numNonZeros = sm->numNonZeros;
	symmflag = sm->symmflag;

	try {
		sa = new double[numNonZeros + 1];
		ija = new INT32[numNonZeros + 1];
	} catch (...) {
		throw "Out of memory";
	}

	memcpy(sa, sm->sa, (numNonZeros + 1) * sizeof(double));
	memcpy(ija, sm->ija, (numNonZeros + 1) * sizeof(INT32));
	currentRow = sm->currentRow;
	currentCol = sm->currentCol;
	currentIndex = sm->currentIndex;

	fortranIJA = NULL;
}

SparseMatrixPCG::~SparseMatrixPCG()
{
	delete[] sa;
	delete[] ija;
	delete[] fortranIJA;
}

void SparseMatrixPCG::clear() {
	memset(sa, 0, (numNonZeros + 1) * sizeof(double));
	memset(ija, 0, (numNonZeros + 1) * sizeof(INT32));
	ija[0] = N + 1;
	currentRow = -1;
	currentCol = -1;
	currentIndex = N;	
}

void SparseMatrixPCG::show()
{
	printf("SparseMatrixPCG\n");
	printf("%10s %10s %14s\n", "k", "ija[k]", "sa[k]");
	for (int i = 0; i < ija[N]; i ++){
		printf("%10d %10d %14.10lg\n", i, ija[i], sa[i]);		
	}	
	if (N < 20) {
		printf("\nMatrix:\n");
		for (int i = 0; i < N; i ++){
			for (int j = 0; j < N; j ++){
				printf("%8lg ",getValue(i, j));
			}
			printf("\n");
		}
	}
}

void SparseMatrixPCG::setsa(long index, double value)
{
	assert(index>=0 && index<numNonZeros+1);
	sa[index]=value;
}

void SparseMatrixPCG::setija(long index, long value)
{
	assert(index>=0 && index<numNonZeros+1);
	ija[index]=value;
}

double SparseMatrixPCG::getValue(long i, long j) {
	assert(i >= 0 && j >= 0 && i<N && j<N);
	if (i == j) {
		return sa[i];
	}
	if (ija[i] >= numNonZeros + 1) {
		return 0.0;
	}
	for (INT32 k = ija[i]; k < ija[i + 1]; k ++) {
		if (ija[k] == j) {
			return sa[k];
		}
	}
	return 0.0;
}

void SparseMatrixPCG::setValue(long i, long j, double value) {
	assert(i >= 0 && j >= 0 && i<N && j<N && currentRow == N-1);
	if (i == j) {
		sa[i] = value;
	}
	if (ija[i] >= numNonZeros + 1) {
		ASSERTION(0);
	}
	for (INT32 k = ija[i]; k < ija[i + 1]; k ++) {
		if (ija[k] == j) {
			sa[k]=value;
			return;
		}
	}
	ASSERTION(0);
}

void SparseMatrixPCG::addNewRow() {
	ija[currentRow + 1] = currentIndex + 1;
	currentRow ++;
	currentCol = -1;
	assert(currentRow<N);
}

void SparseMatrixPCG::setDiag(long row, double value) {
	assert(row < N && row >= 0);	
	sa[row] = value;
}

void SparseMatrixPCG::setCol(long col, double value) {
	assert(col < N && col >= 0);	
	if (col == currentRow) {
		sa[currentRow] = value;
	} else {
		assert(col>currentCol);
		currentCol = col;
		currentIndex ++;
		sa[currentIndex] = value;
		ija[currentIndex] = col;
	}
}

void SparseMatrixPCG::setRow(double diag, int numCols, int* cols, double* values) {
	sa[currentRow] = diag;
	if (numCols > 0) {
		memcpy(ija + currentIndex + 1, cols, numCols * sizeof(INT32));
		memcpy(sa + currentIndex + 1, values, numCols * sizeof(double));
		currentIndex += numCols;
		currentCol = cols[numCols - 1];
	}
}

void SparseMatrixPCG::close() {
	assert(currentRow < N);
	ija[currentRow + 1] = currentIndex + 1;
	for (int i = currentRow + 2; i <= N; i ++) {
		ija[i] = ija[currentRow + 1];
	}
	currentRow = N - 1;
}

INT32* SparseMatrixPCG::getFortranIJA() {
	if (fortranIJA == NULL) {
		fortranIJA  = new INT32[numNonZeros + 1];
	}
	for (int i = 0; i < numNonZeros + 1; i ++) {
		fortranIJA[i] = ija[i] + 1;
	}
	return fortranIJA;
}

int SparseMatrixPCG::getColumns(long i, INT32*& columns, double*& values) {
	assert(i >= 0 && i<N);
	columns = ija + ija[i];
	values = sa + ija[i];
	return ija[i+1] - ija[i];
}