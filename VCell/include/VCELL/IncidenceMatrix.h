#ifndef INCIDENCEMATRIX
#define INCIDENCEMATRIX

/*----------------------------------------------------------------------------
	Sparse matrix stored in PCGPAK2 form
	IncidenceMatrix  contains all the diagonal elements and all the non-zero off diagonal elements
	numNonZeros is the number of diagonal elements plus non-zero off diagonal elements

	If IncidenceMatrix  needs to be passed to Fortran, each element of ija must be incremented by 1.
 --------------------------------------------------------------------------------------*/
template<class TYPE> class IncidenceMatrix 
{
public:
    IncidenceMatrix(long N, long NonZ, int flg, int fixedRowSize=-1);
	IncidenceMatrix(IncidenceMatrix * sm);
    ~IncidenceMatrix();

    long getN() { return N; }
	long getNumNonZeros() {return numNonZeros;};
    int  getSymmetricFlag() { return symmflag; }    
    TYPE* getValue(long i, long j); // return the index of the sparsematrix;
	void setValue(long i, long j, TYPE value); // return the index of the sparsematrix;
	void addNewRow();
	void setCol(long col, TYPE value);
	void setDiag(long row, TYPE value);
	void close();
	TYPE* getsa() { return sa; };
	int getColumns(long i, INT32*& columns, TYPE*& values);

private:
	long N;
	long numNonZeros;
	int symmflag;
	TYPE *sa;
	INT32 *ija;
	long currentRow;
	long currentCol;
	long currentIndex;
	long fixedNonZeroesPerRow;
};


/*----------------------------------------------------------------------------
	IncidenceMatrix is actually varied sparse matrix. 
	1. it is template class
	2. we can insert values. To do that, for each row, there are fixed number(should be greater than the maximum non zeros) of 
		elements which might not be used (correpsonding ija is -1).
 --------------------------------------------------------------------------------------*/
template<class TYPE> IncidenceMatrix<TYPE>::IncidenceMatrix(long arg_N, long arg_numNonZeros, int arg_symmflag, int fixedRowSize)
{
	ASSERTION(arg_N > 0);
	ASSERTION(arg_numNonZeros > 0);

	N = arg_N;	
	symmflag = arg_symmflag;

	numNonZeros = arg_numNonZeros;
	fixedNonZeroesPerRow = fixedRowSize;

	sa = new TYPE[numNonZeros + 1];
	assert(sa);
	memset(sa, 0, (numNonZeros + 1) * sizeof(TYPE));
	ija     = new INT32[numNonZeros + 1];
	assert(ija);
	ija[0] = N + 1;	
	for (int i = 1; i < numNonZeros + 1; i ++) {
		if (fixedNonZeroesPerRow != -1 && i < N + 1) {
			ija[i] = ija[i - 1] + fixedNonZeroesPerRow; // first set every number to be an invalid number	
		} else {
			ija[i] = -1; // first set every number to be an invalid number	
		}
	}	
	currentRow = -1;
	currentCol = -1;
	currentIndex = N;
}

template<class TYPE>IncidenceMatrix<TYPE>::IncidenceMatrix(IncidenceMatrix* sm)
{
	N = sm->N;
	numNonZeros = sm->numNonZeros;
	symmflag = sm->symmflag;
	fixedNonZeroesPerRow = sm->fixedNonZeroesPerRow;

	sa      = new TYPE[numNonZeros + 1];
	assert(sa);
	memcpy(sa, sm->sa, (numNonZeros + 1) * sizeof(TYPE));
	ija     = new INT32[numNonZeros + 1];
	assert(ija);
	memcpy(ija, sm->ija, (numNonZeros + 1) * sizeof(INT32));
	currentRow = sm->currentRow;
	currentCol = sm->currentCol;
	currentIndex = sm->currentIndex;
}

template<class TYPE>IncidenceMatrix<TYPE>::~IncidenceMatrix()
{
	delete[] sa;
	delete[] ija;
}

template<class TYPE> TYPE* IncidenceMatrix<TYPE>::getValue(long i, long j) {
	assert(i<N && j<N);
	if (i == j) {
		return sa + i;
	}
	if (ija[i] >= numNonZeros + 1) {
		return 0;
	}
	for (INT32 k = ija[i]; k < ija[i + 1]; k ++) {
		if (ija[k] == j) {
			return sa + k;
		} else if (ija[k] == -1) {
			return 0;
		}
	}
	return 0;
}

template<class TYPE> void IncidenceMatrix<TYPE>::setValue(long i, long j, TYPE value) {
	assert(i >= 0 && j >= 0 && i < N && j < N);
	assert(fixedNonZeroesPerRow != -1 || currentRow == N-1);
	if (i == j) {
		sa[i] = value;
	}
	if (ija[i] >= numNonZeros + 1) {
		ASSERTION(0);
	}

	bool bVacant = false;
	long insPos = -1;
	for (INT32 k = ija[i]; k < ija[i + 1]; k ++) {
		if (ija[k] == -1) {
			bVacant = true;		
			if (insPos == -1) {
				insPos = k;
			}
			break;
		} else if (ija[k] == j) {
			sa[k] = value;
		} else if (ija[k] > j && insPos == -1) {
			insPos = k;
		}
	}

	assert(insPos != -1 && bVacant);

	for (k = ija[i + 1] - 2; k >= insPos; k --) {
		if (ija[k] == -1) {
			continue;
		}
		ija[k + 1] = ija[k];
		sa[k + 1] = sa[k];
	}
	ija[insPos] = j;
	sa[insPos] = value;
}

template<class TYPE> void IncidenceMatrix<TYPE>::addNewRow() {
	//cout << "currentIndex before=" << currentIndex << endl;
	assert(currentIndex < numNonZeros);	
	if (fixedNonZeroesPerRow != -1 && currentRow != -1) {
		ija[currentRow + 1] = ija[currentRow] + fixedNonZeroesPerRow;
		currentIndex = ija[currentRow + 1] - 1;
	} else {
		ija[currentRow + 1] = currentIndex + 1;
	}
	//cout << "currentIndex after=" << currentIndex << endl;

	currentRow ++;
	currentCol = -1;
	assert(currentRow < N);
}

template<class TYPE> void IncidenceMatrix<TYPE>::setDiag(long row, TYPE value) {
	assert(row < N && row >= 0);	
	sa[row] = value;
}

template<class TYPE> void IncidenceMatrix<TYPE>::setCol(long col, TYPE value) {
	assert(col < N && col >= 0);	
	if (col == currentRow) {
		sa[currentRow] = value;
	} else {
		assert(currentIndex < numNonZeros);
		assert(col>currentCol);
		currentCol = col;
		currentIndex ++;
		sa[currentIndex] = value;
		ija[currentIndex] = col;
	}
}

template<class TYPE> void IncidenceMatrix<TYPE>::close() {
	assert(currentRow < N);
	if (fixedNonZeroesPerRow == -1) {
		ija[currentRow + 1] = currentIndex + 1;	
		for (int i = currentRow + 2; i <= N; i ++) {
			ija[i] = ija[currentRow + 1];
		}
	}
	currentRow = N - 1;
}


template<class TYPE> int IncidenceMatrix<TYPE>::getColumns(long i, INT32*& columns, TYPE*& values) {
	assert(i < N && i >= 0);
	columns = ija + ija[i];
	values = sa + ija[i];
	return ija[i+1] - ija[i];
}

#endif