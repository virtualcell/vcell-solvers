/* Steven Andrews, 12/98.
See documentation called RnSort_doc.doc.
Copyright 2003-2007 by Steven Andrews.  This work is distributed under the terms
of the Gnu Lesser General Public License (LGPL). */

#ifndef __RnSort_h
#define __RnSort_h

void sortV(float *a,float *b,int n);
void sortVdbl(double *a,double *b,int n);
void sortCV(float *a,float *bc,int n);
void sortVliv(long int *a,void **b,int n);
int locateV(float *a,float x,int n);
int locateVdbl(double *a,double x,int n);
int locateVli(long int *a,long int x,int n);
float interpolate1(float *ax,float *ay,int n,int *j,float x);
double interpolate1dbl(double *ax,double *ay,int n,int *j,double x);
float interpolate1Cr(float *ax,float *ayc,int n,int *j,float x);
float interpolate1Ci(float *ax,float *ayc,int n,int *j,float x);
void convertxV(float *ax,float *ay,float *cx,float *cy,int na,int nc);
void convertxCV(float *ax,float *ayc,float *cx,float *cyc,int na,int nc);

void setuphist(float *hist,float *scale,int hn,float low,float high);
void setuphistdbl(double *hist,double *scale,int hn,double low,double high);
int histbin(float value,float *scale,int hn);
int histbindbl(double value,double *scale,int hn);
void data2hist(float *data,int dn,char op,float *hist,float *scale,int hn);
double maxeventrateVD(double *event,double *weight,int n,double sigma,double *tptr);

#endif
