/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#include <math.h>
#include <VCELL/AlgebraicSystem.h>
#include <iostream>
using namespace std;

AlgebraicSystem::AlgebraicSystem(int dim)
{
	dimension = dim;
	tolerance = 0;

	x = new double[dimension];
	varIncrements = new double[dimension];
	matrix = new double*[dimension];
	for(int i=0; i<dimension; i++){
		matrix[i] = new double[dimension+1];
	}
}

bool AlgebraicSystem::solveGauss()
{
	int i, j, k, m;
	double sum, temp;
	for(k=0;k<dimension-1;k++){
		m = k;
		if(matrix[m][k]==0){
			do{
				m = m + 1;
			}while(((m<dimension) && matrix[m][k]==0));
		}
		if(m==dimension){
			return false;
		}else if(m>k){
			for(j=k;j<dimension+1;j++){
				temp = matrix[k][j];
				matrix[k][j] = matrix[m][j];
				matrix[m][j] = temp;
			}
		}
		if (matrix[k][k] == 0) {
			throw "AlgebraicSystem::solveGauss() : zero pivot";
		}
		for(j=k+1;j<dimension+1;j++){
			matrix[k][j] /= matrix[k][k];
		}
		for(i=k+1;i<dimension;i++){
			for(j=k+1;j<dimension+1;j++){
				matrix[i][j] -= matrix[i][k]*matrix[k][j];
			}
		}
	}
	if(matrix[dimension-1][dimension-1]==0){
		return false;
	}else{
		varIncrements[dimension-1] = matrix[dimension-1][dimension]/matrix[dimension-1][dimension-1];
	}
	for(i=1;i<dimension;i++){
		sum = 0;
		for(j=dimension-i;j<dimension;j++){
			sum += matrix[dimension-i-1][j]*varIncrements[j];
		}
		varIncrements[dimension-1-i] = matrix[dimension-1-i][dimension] - sum;
	}
	return true;
  
}

bool AlgebraicSystem::solveSystem()
{
  double diff;
  int count = 0;
  int i = 0;
	do{
		count++;
		//	cout<<"numIterations = "<<count<<endl;
		//            printf("numIterations =%d\n",count);
		//	for(i=0; i<getDimension(); i++){
		//	    cout<<"x("<<i<<")= "<<getX(i)<<endl;
		//            printf("x(%d) = %35.30lg\n",i,getX(i));
		//	}
		diff = 0;
		updateMatrix();
		if(solveGauss()){
			for(i=0; i<dimension; i++){
				x[i] += varIncrements[i];
				if(fabs(x[i])<1e-5){
					diff += fabs(varIncrements[i]);
				}else{
					diff += fabs(varIncrements[i]/x[i]);
				}
			}
		}else{
			cout << "Matrix is singular" << endl;
			return false;
		}
	}while((tolerance < diff)&&(count<100));
	if(tolerance>=diff){
	//     cout << "Matrix is solved, numIterations in solveGauss is "<< count << endl;
		return true;
	}else{
		cout << "Iterations do not converge"<<endl;
		return false; 
	}
}

void AlgebraicSystem::setMatrix(int i, int j, double value)
{
	int dim = getDimension();
	if (i < 0 || j < 0 || i >= dim || j >= dim + 1) {
		throw "AlgebraicSystem::setMatrix() : index out of bound";
	}
	matrix[i][j] = value;   
}
void AlgebraicSystem::setX(int i, double value)
{
	int dim = getDimension();
	if (i < 0 || i >= dim) {
		throw "AlgebraicSystem::setX() : index out of bound";
	}
	x[i] = value;   
}