/* Steven Andrews, 12/98.
See documentation called RnSort_doc.doc.
Copyright 2003-2007 by Steven Andrews.  This work is distributed under the terms
of the Gnu Lesser General Public License (LGPL). */

#include <math.h>
#include <float.h>
#include "Rn.h"
#include "RnSort.h"


void sortV(float *a,float *b,int n) {
	int i,j,ir,l;
	float az,bz;
	
	if(!b) b=a;
	for(i=0;i<n-1&&a[i]<a[i+1];i++)
		;
	if(i==n-1) return;
	for(i=0;i<n-1&&a[i]>a[i+1];i++)
		;
	if(i==n-1)	{
		for(i=0;i<n/2;i++)	{
			az=a[i];
			bz=b[i];
			a[i]=a[j=n-i-1];
			b[i]=b[j];
			a[j]=az;
			b[j]=bz; }
		return; }
	l=(n>>1)+1;
	ir=n;
	for(;;)	{
		if(l>1)	{
			az=a[--l-1];
			bz=b[l-1]; }
		else	{
			az=a[ir-1];
			bz=b[ir-1];
			a[ir-1]=a[0];
			b[ir-1]=b[0];
			if(--ir==1)	{
				a[0]=az;
				b[0]=bz;
				return; }}
		i=l;
		j=l<<1;
		while(j<=ir)	{
			if(j<ir&&a[j-1]<a[j])	++j;
			if(az<a[j-1])	{
				a[i-1]=a[j-1];
				b[i-1]=b[j-1];
				j+=(i=j); }
			else
				j=ir+1; }
		a[i-1]=az;
		b[i-1]=bz; }}


void sortVdbl(double *a,double *b,int n) {
	int i,j,ir,l;
	double az,bz;

	if(!b) b=a;
	for(i=0;i<n-1&&a[i]<a[i+1];i++)
		;
	if(i==n-1) return;
	for(i=0;i<n-1&&a[i]>a[i+1];i++)
		;
	if(i==n-1)	{
		for(i=0;i<n/2;i++)	{
			az=a[i];
			bz=b[i];
			a[i]=a[j=n-i-1];
			b[i]=b[j];
			a[j]=az;
			b[j]=bz; }
		return; }
	l=(n>>1)+1;
	ir=n;
	for(;;)	{
		if(l>1)	{
			az=a[--l-1];
			bz=b[l-1]; }
		else	{
			az=a[ir-1];
			bz=b[ir-1];
			a[ir-1]=a[0];
			b[ir-1]=b[0];
			if(--ir==1)	{
				a[0]=az;
				b[0]=bz;
				return; }}
		i=l;
		j=l<<1;
		while(j<=ir)	{
			if(j<ir&&a[j-1]<a[j])	++j;
			if(az<a[j-1])	{
				a[i-1]=a[j-1];
				b[i-1]=b[j-1];
				j+=(i=j); }
			else
				j=ir+1; }
		a[i-1]=az;
		b[i-1]=bz; }}


void sortCV(float *a,float *bc,int n) {
	int i,j,ir,l;
	float az,bzr,bzi;

	for(i=0;i<n-1&&a[i]<a[i+1];i++)
		;
	if(i==n-1) return;
	for(i=0;i<n-1&&a[i]>a[i+1];i++)
		;
	if(i==n-1)	{
		for(i=0;i<n/2;i++)	{
			az=a[i];
			bzr=bc[2*i];
			bzi=bc[2*i+1];
			a[i]=a[j=n-i-1];
			bc[2*i]=bc[2*j];
			bc[2*i+1]=bc[2*j+1];
			a[j]=az;
			bc[2*j]=bzr;
			bc[2*j+1]=bzi; }
		return; }
	l=(n>>1)+1;
	ir=n;
	for(;;)	{
		if(l>1)	{
			az=a[--l-1];
			bzr=bc[2*(l-1)];
			bzi=bc[2*(l-1)+1]; }
		else	{
			az=a[ir-1];
			bzr=bc[2*(ir-1)];
			bzi=bc[2*(ir-1)+1];
			a[ir-1]=a[0];
			bc[2*(ir-1)]=bc[0];
			bc[2*(ir-1)+1]=bc[1];
			if(--ir==1)	{
				a[0]=az;
				bc[0]=bzr;
				bc[1]=bzi;
				return; }}
		i=l;
		j=l<<1;
		while(j<=ir)	{
			if(j<ir&&a[j-1]<a[j])	++j;
			if(az<a[j-1])	{
				a[i-1]=a[j-1];
				bc[2*(i-1)]=bc[2*(j-1)];
				bc[2*(i-1)+1]=bc[2*(j-1)+1];
				j+=(i=j); }
			else
				j=ir+1; }
		a[i-1]=az;
		bc[2*(i-1)]=bzr;
		bc[2*(i-1)+1]=bzi; }}


void sortVliv(long int *a,void **b,int n) {
	int i,j,ir,l;
	long int az;
	void *bz;
	
	for(i=0;i<n-1&&a[i]<a[i+1];i++)
		;
	if(i==n-1) return;
	for(i=0;i<n-1&&a[i]>a[i+1];i++)
		;
	if(i==n-1)	{
		for(i=0;i<n/2;i++)	{
			az=a[i];
			bz=b[i];
			a[i]=a[j=n-i-1];
			b[i]=b[j];
			a[j]=az;
			b[j]=bz; }
		return; }
	l=(n>>1)+1;
	ir=n;
	for(;;)	{
		if(l>1)	{
			az=a[--l-1];
			bz=b[l-1]; }
		else	{
			az=a[ir-1];
			bz=b[ir-1];
			a[ir-1]=a[0];
			b[ir-1]=b[0];
			if(--ir==1)	{
				a[0]=az;
				b[0]=bz;
				return; }}
		i=l;
		j=l<<1;
		while(j<=ir)	{
			if(j<ir&&a[j-1]<a[j])	++j;
			if(az<a[j-1])	{
				a[i-1]=a[j-1];
				b[i-1]=b[j-1];
				j+=(i=j); }
			else
				j=ir+1; }
		a[i-1]=az;
		b[i-1]=bz; }}


int locateV(float *a,float x,int n) {
	int jl,jm,ju,ascnd;

	jl=-1;
	ju=n;
	ascnd=(a[n-1]>=a[0]);
	while(ju-jl>1)	{
		jm=(ju+jl)>>1;
		if(ascnd==(x>=a[jm]))	jl=jm;
		else	ju=jm; }
	return jl; }


int locateVdbl(double *a,double x,int n) {
	int jl,jm,ju,ascnd;

	jl=-1;
	ju=n;
	ascnd=(a[n-1]>=a[0]);
	while(ju-jl>1)	{
		jm=(ju+jl)>>1;
		if(ascnd==(x>=a[jm]))	jl=jm;
		else	ju=jm; }
	return jl; }


int locateVli(long int *a,long int x,int n) {
	int jl,jm,ju,ascnd;

	jl=-1;
	ju=n;
	ascnd=(a[n-1]>=a[0]);
	while(ju-jl>1)	{
		jm=(ju+jl)>>1;
		if(ascnd==(x>=a[jm]))	jl=jm;
		else	ju=jm; }
	if(a[jl]==x) return jl;
	else return -1; }


float interpolate1(float *ax,float *ay,int n,int *j,float x) {
	int i;

	i=*j;
	if(i<-1) i=locateV(ax,x,n);
	else while(i<n-1&&ax[i+1]<=x) i++;
	*j=i;
	if(i>n-2) i=n-2;
	if(i<0) i=0;
	if(n==1||ax[i+1]==ax[i]) return ay[i];
	return (ay[i]*(ax[i+1]-x)+ay[i+1]*(x-ax[i]))/(ax[i+1]-ax[i]); }


double interpolate1dbl(double *ax,double *ay,int n,int *j,double x) {
	int i;

	i=*j;
	if(i<-1) i=locateVdbl(ax,x,n);
	else while(i<n-1&&ax[i+1]<=x) i++;
	*j=i;
	if(i>n-2) i=n-2;
	if(i<0) i=0;
	if(n==1||ax[i+1]==ax[i]) return ay[i];
	return (ay[i]*(ax[i+1]-x)+ay[i+1]*(x-ax[i]))/(ax[i+1]-ax[i]); }


float interpolate1Cr(float *ax,float *ayc,int n,int *j,float x) {
	int i;
	
	i=*j;
	if(i<-1) i=locateV(ax,x,n);
	else while(i<n-1&&ax[i+1]<=x) i++;
	*j=i;
	if(i>n-2) i=n-2;
	if(i<0) i=0;
	if(n==1||ax[i+1]==ax[i]) return ayc[2*i];
	return (ayc[2*i]*(ax[i+1]-x)+ayc[2*(i+1)]*(x-ax[i]))/(ax[i+1]-ax[i]); }


float interpolate1Ci(float *ax,float *ayc,int n,int *j,float x) {
	int i;
	
	i=*j;
	if(i<-1) i=locateV(ax,x,n);
	else while(i<n-1&&ax[i+1]<=x) i++;
	*j=i;
	if(i>n-2) i=n-2;
	if(i<0) i=0;
	if(n==1||ax[i+1]==ax[i]) return ayc[2*i+1];
	return (ayc[2*i+1]*(ax[i+1]-x)+ayc[2*(i+1)+1]*(x-ax[i]))/(ax[i+1]-ax[i]); }


void convertxV(float *ax,float *ay,float *cx,float *cy,int na,int nc) {
	int ia,ic;
	float x,dx;

	if(na==nc) {
		for(ia=0;ia<na&&cx[ia]==ax[ia];ia++)
			;
		if(ia==na) {
			for(ia=0;ia<na;ia++) cy[ia]=ay[ia];
			return; }}
	ia=locateV(ax,cx[0],na);
	if(ia>na-2) ia=na-2;
	if(ia<0) ia=0;
	for(ic=0;ic<nc;ic++) {
		x=cx[ic];
		while(ia<na-2&&ax[ia+1]<=x) ia++;
		dx=ax[ia+1]-ax[ia];
		cy[ic]=dx==0?ay[ia]:(ay[ia]*(ax[ia+1]-x)+ay[ia+1]*(x-ax[ia]))/dx; }
	return; }

	
void convertxCV(float *ax,float *ayc,float *cx,float *cyc,int na,int nc) {
	int ia,ic;
	float x,dx;

	if(na==nc) {
		for(ia=0;ia<na&&cx[ia]==ax[ia];ia++)
			;
		if(ia==na) {
			for(ia=0;ia<2*na;ia++) cyc[ia]=ayc[ia];
			return; }}
	ia=locateV(ax,cx[0],na);
	if(ia>na-2) ia=na-2;
	if(ia<0) ia=0;
	for(ic=0;ic<nc;ic++) {
		x=cx[ic];
		while(ia<na-2&&ax[ia+1]<=x) ia++;
		dx=ax[ia+1]-ax[ia];
		cyc[2*ic]=dx==0?ayc[2*ia]:(ayc[2*ia]*(ax[ia+1]-x)+ayc[2*ia+2]*(x-ax[ia]))/dx;
		cyc[2*ic+1]=dx==0?ayc[2*ia+1]:(ayc[2*ia+1]*(ax[ia+1]-x)+ayc[2*ia+3]*(x-ax[ia]))/dx; }
	return; }
	
	
void setuphist(float *hist,float *scale,int hn,float low,float high) {
	int i;
	float delta;

	for(i=0;i<hn;i++) hist[i]=0;
	delta=(high-low)/(hn-2.0);
	for(i=0;i<hn-1;i++) scale[i]=low+i*delta;
	scale[hn-1]=FLT_MAX;
	return; }


void setuphistdbl(double *hist,double *scale,int hn,double low,double high) {
	int i;
	double delta;

	for(i=0;i<hn;i++) hist[i]=0;
	delta=(high-low)/(hn-2.0);
	for(i=0;i<hn-1;i++) scale[i]=low+i*delta;
	scale[hn-1]=DBL_MAX;
	return; }


int histbin(float value,float *scale,int hn) {
	return 1+locateV(scale,value,hn); }


int histbindbl(double value,double *scale,int hn) {
	return 1+locateVdbl(scale,value,hn); }


void data2hist(float *data,int dn,char op,float *hist,float *scale,int hn) {
	int i,j;
	float delta;

	if(op=='-') delta=-1;
	else delta=1;
	if(op=='=') for(j=0;j<hn;j++) hist[j]=0;
	for(i=0;i<dn;i++) {
		j=1+locateV(scale,data[i],hn);
		hist[j]+=delta; }
	return; }


double maxeventrateVD(double *event,double *weight,int n,double sigma,double *tptr) {
	double xlo,xhi,x,rate,ratemax,xmax,step;
	int i,scan,nstep;

	xlo=minVD(event,n,NULL);
	xhi=maxVD(event,n,NULL);
	ratemax=0;
	xmax=xlo;
	step=sigma;
	nstep=(xhi-xlo)/step;
	for(scan=0;scan<3;scan++) {
		for(x=xlo;x<=xhi;x+=step) {
			rate=0;
			if(weight) for(i=0;i<n;i++) rate+=weight[i]*exp(-(x-event[i])*(x-event[i])/(2*sigma));
			else for(i=0;i<n;i++) rate+=exp(-(x-event[i])*(x-event[i])/(2*sigma));
			if(rate>ratemax) {
				ratemax=rate;
				xmax=x; }}
		if(xmax>xlo) xlo=xmax-step;
		if(xmax<xhi) xhi=xmax+step;
		step=(xhi-xlo)/nstep; }
	if(tptr) *tptr=xmax;
	return ratemax/(sigma*sqrt(2*3.1415926535)); }







