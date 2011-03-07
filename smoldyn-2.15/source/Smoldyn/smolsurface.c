/* Steven Andrews, started 10/22/01.
This is a library of functions for the Smoldyn program.  See documentation
called Smoldyn_doc1.doc and Smoldyn_doc2.doc.
Copyright 2003-2007 by Steven Andrews.  This work is distributed under the terms
of the Gnu Lesser General Public License (LGPL). */

#include <stdio.h>
#include <math.h>
#include <string.h>
#include <float.h>
#include "Geometry.h"
#include "math2.h"
#include "random2.h"
#include "Rn.h"
#include "RnSort.h"
#include "SurfaceParam.h"
#include "smoldyn.h"
#include "Sphere.h"
#include "Zn.h"

#include "smoldyn_config.h"

#ifdef THREADING
#include <pthread.h>
#endif

#define CHECK(A) if(!(A)) goto failure; else (void)0
#define CHECKS(A,B) if(!(A)) {strncpy(erstr,B,STRCHAR-1);erstr[STRCHAR-1]='\0';goto failure;} else (void)0


/******************************************************************************/
/********************************** Surfaces **********************************/
/******************************************************************************/

typedef struct PARAMSET_check_surfaces_on_subset_mols { //??????????? new code
	simptr sim;
	int ll;
	int first_ndx;
	int second_ndx;
	stack* output_stack;
	} PARAMS_check_surfaces_on_subset_mols;


/******************************************************************************/
/****************************** enumerated types ******************************/
/******************************************************************************/


/* surfstring2face.  Converts panel face string to an enumerated panel face
type. */
enum PanelFace surfstring2face(char *string) {
	enum PanelFace ans;

	if(strbegin(string,"front",0)) ans=PFfront;
	else if(strbegin(string,"back",0)) ans=PFback;
	else if(strbegin(string,"all",0) || strbegin(string,"both",0)) ans=PFboth;
	else ans=PFnone;
	return ans; }


/* surfface2string.  Converts enumerated panel face to a string, in string,
which must be pre-allocated.  Output strings are ÒfrontÓ, ÒbackÓ, ÒbothÓ, or
ÒnoneÓ.  string is returned to allow for function nesting. */
char *surfface2string(enum PanelFace face,char *string) {
	if(face==PFfront) strcpy(string,"front");
	else if(face==PFback) strcpy(string,"back");
	else if(face==PFboth) strcpy(string,"both");
	else strcpy(string,"none");
	return string; }


/* surfstring2act.  Converts action string to an enumerated action type. */
enum SrfAction surfstring2act(char *string) {
	enum SrfAction ans;

	if(strbegin(string,"reflect",0)) ans=SAreflect;
	else if(strbegin(string,"transmit",0)) ans=SAtrans;
	else if(strbegin(string,"absorb",0)) ans=SAabsorb;
	else if(strbegin(string,"jump",0)) ans=SAjump;
	else if(!strcmp(string,"periodic")) ans=SAjump;
	else if(!strcmp(string,"port")) ans=SAport;
	else if(strbegin(string,"multiple",0)) ans=SAmult;
	else if(strbegin(string,"no",0)) ans=SAno;
	else ans=SAnone;
	return ans; }


/* surfact2string.  Converts enumerated surface action to a string, in string,
which must be pre-allocated.  Output strings are ÒreflectÓ, ÒtransmitÓ, etc.
string is returned to allow for function nesting. */
char *surfact2string(enum SrfAction act,char *string) {
	if(act==SAreflect) strcpy(string,"reflect");
	else if(act==SAtrans) strcpy(string,"transmit");
	else if(act==SAabsorb) strcpy(string,"absorb");
	else if(act==SAjump) strcpy(string,"jump");
	else if(act==SAport) strcpy(string,"port");
	else if(act==SAmult) strcpy(string,"multiple");
	else if(act==SAno) strcpy(string,"no");
	else if(act==SAadsorb) strcpy(string,"adsorb");
	else if(act==SArevdes) strcpy(string,"revdes");
	else if(act==SAirrevdes) strcpy(string,"irrevdes");
	else if(act==SAflip) strcpy(string,"flip");
	else strcpy(string,"none");
	return string; }


/* surfstring2ps.  Converts panel shape string to an enumerated panel shape type. */
enum PanelShape surfstring2ps(char *string) {
	enum PanelShape ans;

	if(strbegin(string,"rectangle",0)) ans=PSrect;
	else if(strbegin(string,"triangle",0)) ans=PStri;
	else if(strbegin(string,"sphere",0)) ans=PSsph;
	else if(strbegin(string,"cylinder",0)) ans=PScyl;
	else if(strbegin(string,"hemisphere",0)) ans=PShemi;
	else if(strbegin(string,"disk",0)) ans=PSdisk;
	else if(strbegin(string,"all",0)) ans=PSall;
	else ans=PSnone;
	return ans; }


/* surfps2string.  Converts enumerated panel shape to a string.  Output strings
are abbreviated shape names, such as ÒrectÓ to designate a rectangle.  Also,
PSall and PSnone result in the strings ÒallÓ and ÒnoneÓ.  string is returned to
allow for function nesting. */
char *surfps2string(enum PanelShape ps,char *string) {
	if(ps==PSrect) strcpy(string,"rect");
	else if(ps==PStri) strcpy(string,"tri");
	else if(ps==PSsph) strcpy(string,"sph");
	else if(ps==PScyl) strcpy(string,"cyl");
	else if(ps==PShemi) strcpy(string,"hemi");
	else if(ps==PSdisk) strcpy(string,"disk");
	else if(ps==PSall) strcpy(string,"all");
	else strcpy(string,"none");
	return string; }


/* surfstring2dm.  Converts drawing mode string to an enumerated drawing mode
type. */
enum DrawMode surfstring2dm(char *string) {
	enum DrawMode ans;

	if(strbegin(string,"none",0)) ans=DMno;
	else if(!strcmp(string,"ve") || !strcmp(string,"ev")) ans=DMve;
	else if(!strcmp(string,"vf") || !strcmp(string,"fv")) ans=DMvf;
	else if(!strcmp(string,"vef") || !strcmp(string,"vfe") || !strcmp(string,"evf") || !strcmp(string,"efv") || !strcmp(string,"fev") || !strcmp(string,"fve")) ans=DMvef;
	else if(strbegin(string,"vertex",0)) ans=DMvert;
	else if(strbegin(string,"edge",0)) ans=DMedge;
	else if(strbegin(string,"face",0)) ans=DMface;
	else ans=DMnone;
	return ans; }


/* surfdm2string.  Converts enumerated drawing mode to a string.  Output strings
are abbreviated drawing mode names.  string is returned to allow for function
nesting. */
char *surfdm2string(enum DrawMode dm,char *string) {
	if(dm==DMno) strcpy(string,"no");
	else if(dm==DMvert) strcpy(string,"vert");
	else if(dm==DMedge) strcpy(string,"edge");
	else if(dm==DMve) strcpy(string,"ve");
	else if(dm==DMface) strcpy(string,"face");
	else if(dm==DMvf) strcpy(string,"vf");
	else if(dm==DMef) strcpy(string,"ef");
	else if(dm==DMvef) strcpy(string,"vef");
	else strcpy(string,"none");
	return string; }


/******************************************************************************/
/***************************** low level utilities ****************************/
/******************************************************************************/


/* readsurfacename. */
int readsurfacename(simptr sim,char *str,enum PanelShape *psptr,int *pptr) {
	char nm[STRCHAR],*colon;
	int itct,s,p;
	enum PanelShape ps;

	if(!str) return -1;
	if(!sim->srfss || !sim->srfss->nsrf) return -2;

	itct=sscanf(str,"%s",nm);
	if(itct!=1) return -1;	// cannot read name
	colon=strchr(nm,':');
	ps=PSall;
	p=-1;

	if(!strcmp(nm,"all:all") || !strcmp(nm,"all")) {
		s=-5; }
	else if(!colon) {
		s=stringfind(sim->srfss->snames,sim->srfss->nsrf,nm);
		if(s==-1) return -3; }
	else {
		*colon='\0';
		if(!strcmp(nm,"all")) return -4;
		s=stringfind(sim->srfss->snames,sim->srfss->nsrf,nm);
		if(s==-1) return -3;
		if(!strcmp(colon+1,"all"));
		else {
			for(ps=(PanelShape)0;p==-1 && ps<PSMAX;ps=(PanelShape)(ps+1))
				p=stringfind(sim->srfss->srflist[s]->pname[ps],sim->srfss->srflist[s]->npanel[ps],colon+1);
			if(p==-1) {
				ps=PSnone;
				p=-2; }
			else
				ps=(PanelShape)(ps-1); }
		*colon=':'; }

	if(psptr) *psptr=ps;
	if(pptr) *pptr=p;
	return s; }


/* panelpoints.  Returns the number of point elements that need to be allocated
for a panel of shape ps and for system dimensionality dim.  These numbers are
the same as those listed in the table above.  0 is returned for inputs that
don't make sense (e.g. PSall) or for shapes that are not permitted in the
requested dimension. */
int panelpoints(enum PanelShape ps,int dim) {
	int npts;

	if(ps==PSrect) npts=intpower(2,dim-1);
	else if(ps==PStri) npts=dim;
	else if(ps==PSsph) npts=2;
	else if(ps==PScyl && dim>1) npts=3;
	else if(ps==PShemi && dim>1) npts=3;
	else if(ps==PSdisk && dim>1) npts=2;
	else npts=0;
	return npts; }


/* panelmiddle.  Returns the middle of panel pnl in the dim-dimensional vector
middle; dim is the system dimensionality.  For spheres, hemispheres, and
cylinders, the middle point is the actual center location if onpanel is 0, which
is enclosed by the panel but not on it; for these, set onpanel to 1 for middle
to be returned as a point on the panel, although it will no longer be in the
middle.  onpanel is ignored for rectangles, triangles, and disks.  If onpanel is
1: for spheres, the returned point is directly to the positive x direction from
the sphere center; for cylinders, the returned point is as close as possible to
the center point; and for hemispheres, the returned point is the center of the
on-panel locations. */
void panelmiddle(panelptr pnl,double *middle,int dim,int onpanel) {
	enum PanelShape ps;
	double **point,norm[DIMMAX];
	int d;

	ps=pnl->ps;
	point=pnl->point;
	if(ps==PSrect) {
		Geo_RectCenter(point,middle,dim); }
	else if(ps==PStri) {
		Geo_TriCenter(point,middle,dim); }
	else if(ps==PSsph) {
		for(d=0;d<dim;d++)
			middle[d]=point[0][d];
		if(onpanel)
			middle[0]+=point[1][0]; }
	else if(ps==PScyl) {
		Geo_LineCenter(point,middle,dim);
		if(onpanel) {
			if(dim==2) Geo_LineNormal2D(point[0],point[1],middle,norm);
			else Geo_LineNormal3D(point[0],point[1],middle,norm);
			for(d=0;d<dim;d++)
				middle[d]+=norm[d]*point[2][0]; }}
	else if(ps==PShemi) {
		for(d=0;d<dim;d++)
			middle[d]=point[0][d];
		if(onpanel)
			for(d=0;d<dim;d++)
				middle[d]-=point[2][d]*point[1][0]; }
	else if(ps==PSdisk) {
		for(d=0;d<dim;d++)
			middle[d]=point[0][d]; }
	return; }


/* panelarea.  Returns the area of panel pnl; dim is the system dimensionality,
as always. */
double panelarea(panelptr pnl,int dim) {
	enum PanelShape ps;
	double **point,*front,area;
	int d;

	ps=pnl->ps;
	point=pnl->point;
	front=pnl->front;

	if(dim==1) {
		if(ps==PSrect || ps==PStri) area=1;
		else if(ps==PSsph) area=2;
		else area=0; }
	else if(dim==2) {
		if(ps==PSrect) area=fabs(point[1][(int)front[2]]-point[0][(int)front[2]]);
		else if(ps==PStri) area=sqrt((point[1][0]-point[0][0])*(point[1][0]-point[0][0])+(point[1][1]-point[0][1])*(point[1][1]-point[0][1]));
		else if(ps==PSsph) area=2.0*PI*point[1][0];
		else if(ps==PScyl) area=2.0*sqrt((point[1][0]-point[0][0])*(point[1][0]-point[0][0])+(point[1][1]-point[0][1])*(point[1][1]-point[0][1]));
		else if(ps==PShemi) area=PI*point[1][0];
		else if(ps==PSdisk) area=2.0*point[1][0];
		else area=0; }
	else if(dim==3) {
		if(ps==PSrect) {
			d=0;
			while(d==(int)front[1] || d==(int)front[2]) d++;
			area=fabs((point[2][(int)front[2]]-point[0][(int)front[2]])*(point[2][d]-point[0][d])); }
		else if(ps==PStri) area=Geo_TriArea3(point[0],point[1],point[2],front);
		else if(ps==PSsph) area=4.0*PI*point[1][0]*point[1][0];
		else if(ps==PScyl) area=2.0*PI*point[2][0]*sqrt((point[1][0]-point[0][0])*(point[1][0]-point[0][0])+(point[1][1]-point[0][1])*(point[1][1]-point[0][1])+(point[1][2]-point[0][2])*(point[1][2]-point[0][2]));
		else if(ps==PShemi) area=2.0*PI*point[1][0]*point[1][0];
		else if(ps==PSdisk) area=PI*point[1][0]*point[1][0];
		else area=0; }
	else area=0;

	return area; }


/* surfacearea.  Returns the total area of surface srf; dim is the system
dimensionality.    If totpanelptr is not NULL, it is returned with the total
number of panels in the surface. */
double surfacearea(surfaceptr srf,int dim,int *totpanelptr) {
	int ps,p,totpanel;
	double area;

	totpanel=0;
	area=0;
	for(ps=0;ps<PSMAX;ps++)
		for(p=0;p<srf->npanel[ps];p++) {
			totpanel++;
			area+=panelarea(srf->panels[ps][p],dim); }
	if(totpanelptr) *totpanelptr=totpanel;
	return area; }


/* surfacearea2.  Returns the area of one or more panels.  For the area of a
single panel, the inputs are the surface number, the panel shape number, and the
panel name.  The area is returned (number of points for 1-D, line length for
2-D, and area for 3-D).  If totpanelptr is set in as not NULL, it will point to
the integer 1 on return.  For multiple panels, set any or all of the input
integers to negative values, or pname to ÒallÓ, to indicate ÒallÓ, and
totpanelptr will point to the number of panels included in the sum.  For
example, if surface is a positive number, ps is negative, and panel is ÒallÓ,
then the total area of all panels of the specified surface is found.  Or, if
surface and ps are negative and pname is ÒendcapÓ then the area is found for all
panels named ÒendcapÓ, regardless of their surface or shape.  If no panels match
the input description, 0 is returned and totpanelptr is left pointing to a 0. */
double surfacearea2(simptr sim,int surface,enum PanelShape ps,char *pname,int *totpanelptr) {
	int panel,dim,totpanel;
	double area;
	surfaceptr srf;
	int slo,shi,pslo,pshi,plo,phi,s,p;

	dim=sim->dim;
	if(ps==PSnone) {area=0;totpanel=0;}											// ps is none

	else if(surface>=0 && ps!=PSall && pname && strcmp(pname,"all")) {		// specific panel
		srf=sim->srfss->srflist[surface];
		panel=stringfind(srf->pname[ps],srf->npanel[ps],pname);
		if(panel<0) {
			area=0;
			totpanel=0; }
		else {
			area=panelarea(srf->panels[ps][panel],sim->dim);
			totpanel=1; }}

	else {																									// multiple panels
		slo=(surface>=0)?surface:0;
		shi=(surface>=0)?surface+1:sim->srfss->nsrf;
		pslo=(ps!=PSall)?ps:0;
		pshi=(ps!=PSall)?ps+1:PSMAX;

		area=0;
		totpanel=0;
		for(s=slo;s<shi;s++)
			for(ps=(PanelShape)pslo;ps<pshi;ps=(PanelShape)(ps+1)) {
				srf=sim->srfss->srflist[s];
				if(!pname || !strcmp(pname,"all")) {plo=0;phi=srf->npanel[ps];}
				else if((panel=stringfind(srf->pname[ps],srf->npanel[ps],pname))<0) plo=phi=0;
				else {plo=panel;phi=panel+1;}
				for(p=plo;p<phi;p++) {
					area+=surfacearea2(sim,s,ps,srf->pname[ps][p],NULL);
					totpanel++; }}}

	if(totpanelptr) *totpanelptr=totpanel;
	return area; }


/* panelrandpos.  Returns a random position, in pos, on the surface of panel
pnl, in a dim dimensional system.  The result might be on either side of the
panel. */
void panelrandpos(panelptr pnl,double *pos,int dim) {
	int d;
	double **point,*front,flt1,flt2,v1[DIMMAX],dcm[DIMMAX*DIMMAX];
	enum PanelShape ps;

	point=pnl->point;
	front=pnl->front;
	ps=pnl->ps;

	if(dim==1) {
		if(ps==PSrect || ps==PStri)
			pos[0]=point[0][0];
		else if(ps==PSsph)
			pos[0]=point[0][0]+(coinrandD(0.5)?point[1][0]:-point[1][0]);
		else
			pos[0]=0; }

	else if(dim==2) {
		if(ps==PSrect) {
			pos[(int)front[1]]=point[0][(int)front[1]];
			pos[(int)front[2]]=unirandCCD(point[0][(int)front[2]],point[1][(int)front[2]]); }
		else if(ps==PStri) {
			flt1=randCCD();
			pos[0]=point[0][0]+flt1*(point[1][0]-point[0][0]);
			pos[1]=point[0][1]+flt1*(point[1][1]-point[0][1]); }
		else if(ps==PSsph) {
			flt1=unirandCOD(0,2.0*PI);
			pos[0]=point[0][0]+point[1][0]*cos(flt1);
			pos[1]=point[0][1]+point[1][0]*sin(flt1); }
		else if(ps==PScyl) {
			flt1=randCCD();
			flt2=coinrandD(0.5)?1:-1;
			pos[0]=point[0][0]+flt1*(point[1][0]-point[0][0])+flt2*point[2][0]*front[0];
			pos[1]=point[0][1]+flt1*(point[1][1]-point[0][1])+flt2*point[2][0]*front[1]; }
		else if(ps==PShemi) {
			flt1=unirandCCD(0,PI)+atan2(point[2][1],point[2][0])+PI/2.0;
			pos[0]=point[0][0]+point[1][0]*cos(flt1);
			pos[1]=point[0][1]+point[1][0]*sin(flt1); }
		else if(ps==PSdisk) {
			flt1=unirandCCD(-1,1);
			pos[0]=point[0][0]-flt1*point[1][0]*front[1];
			pos[1]=point[0][1]+flt1*point[1][0]*front[0]; }
		else
			pos[0]=pos[1]=0; }

	else if(dim==3) {
		if(ps==PSrect) {
			pos[0]=unirandCCD(point[0][0],point[2][0]);
			pos[1]=unirandCCD(point[0][1],point[2][1]);
			pos[2]=unirandCCD(point[0][2],point[2][2]);
			pos[(int)front[1]]=point[0][(int)front[1]]; }
		else if(ps==PStri) {
			trianglerandCD(point[0],point[1],point[2],dim,pos); }
		else if(ps==PSsph) {
			sphererandCCD(pos,point[1][0],point[1][0]);
			for(d=0;d<dim;d++) pos[d]+=point[0][d]; }
		else if(ps==PScyl) {
			flt1=randCCD();
			flt2=unirandCOD(0,2.0*PI);
			v1[0]=point[1][0]-point[0][0];
			v1[1]=point[1][1]-point[0][1];
			v1[2]=point[1][2]-point[0][2];
			Sph_Newz2Dcm(v1,0,dcm);
			v1[0]=point[2][0]*cos(flt2);
			v1[1]=point[2][0]*sin(flt2);
			v1[2]=0;
			dotMVD(dcm,v1,pos,3,3);
			pos[0]+=point[0][0]+flt1*(point[1][0]-point[0][0]);
			pos[1]+=point[0][1]+flt1*(point[1][1]-point[0][1]);
			pos[2]+=point[0][2]+flt1*(point[1][2]-point[0][2]); }
		else if(ps==PShemi) {
			flt1=thetarandCCD();
			if(flt1<PI/2.0) flt1=PI-flt1;
			flt2=unirandCOD(0,2.0*PI);
			v1[0]=point[1][0]*sin(flt1)*cos(flt2);
			v1[1]=point[1][0]*sin(flt1)*sin(flt2);
			v1[2]=point[1][0]*cos(flt1);
			Sph_Newz2Dcm(point[2],0,dcm);
			dotMVD(dcm,v1,pos,3,3);
			pos[0]+=point[0][0];
			pos[1]+=point[0][1];
			pos[2]+=point[0][2]; }
		else if(ps==PSdisk) {
			flt1=unirandCOD(0,2.0*PI);
			flt2=radrandcircCCD(point[1][0]);
			v1[0]=flt2*cos(flt1);
			v1[1]=flt2*sin(flt1);
			v1[2]=0;
			Sph_Newz2Dcm(front,0,dcm);
			dotMVD(dcm,v1,pos,3,3);
			pos[0]+=point[0][0];
			pos[1]+=point[0][1];
			pos[2]+=point[0][2]; }
		else
			pos[0]=pos[1]=0; }

	return; }


/* surfrandpos.  Returns a random position, in pos, on the surface srf, in a dim
dimensional system.  The result might be on either side of the surface.  The
return value is a pointer to the panel that the point is in, or NULL if the surface
has no panels. */
panelptr surfrandpos(surfaceptr srf,double *pos,int dim) {
	panelptr pnl;

	if(!srf->totpanel) return NULL;
	pnl=srf->paneltable[intrandpD(srf->totpanel,srf->areatable)];
	panelrandpos(pnl,pos,dim);
	return pnl; }


/* issurfprod.  Determines if molecule identity i and state ms is the product of
a surface action, accounting for all surfaces.  Returns 1 if so and 0 if not.
This should work after surfaces have been loaded and either before or after they
have been set up. */
int issurfprod(simptr sim,int i,enum MolecState ms) {
	surfacessptr srfss;
	int s;
	surfaceptr srf;
	enum MolecState ms1;

	srfss=sim->srfss;
	if(srfss) {
		if(i>=srfss->maxspecies) return 0;
		for(s=0;s<srfss->nsrf;s++) {
			srf=srfss->srflist[s];
			for(ms1=(MolecState)0;ms1<MSMAX1;ms1=(MolecState)(ms1+1)) {
				if(srf->srfrate[i][ms1][ms]>0) return 1;
				if(srf->srfprob[i][ms1][ms]>0) return 1; }}}
	return 0; }


/* srfcalcrate.  Calculates the actual rate for the interaction of a molecule of
type i with surface srf, converting from state ms1 to state ms2.  This only
returns a useful answer if the surface action listed for species i and
originating state ms1 is SAmult, because only then are rates relevant.  If the
surface action is not SAmult, this returns -1; also if ms1 equals ms2, this
returns -2, because rates are not meaningful for no change.  For other
conversions, this uses the srf->prob[i][ms1][ms2] structure element to calculate
the actual conversion rate.  This function also accounts for reversible
interactions.  All cases are calculated assuming steady-state behavior, and all
are found using the SurfaceParam.c function surfacerate.  Unless one of the two
previously mentioned negative number codes are returned, this returns a rate,
which has a value between 0 and MAX_DBL. */
double srfcalcrate(simptr sim,surfaceptr srf,int i,enum MolecState ms1,enum MolecState ms2) {
	double rate,prob,probrev,sum,dt,difc;
	enum MolecState ms3;

	if(srf->action[i][ms1]!=SAmult) return -1;
	if(ms1==ms2) return -2;

	prob=srf->srfprob[i][ms1][ms2];
	if(prob<=0) return 0;
	probrev=srf->srfprob[i][ms2][ms1];
	dt=sim->dt;
	difc=sim->mols->difc[i][MSsoln];

	sum=0;
	if(!(ms1==MSsoln || ms1==MSbsoln)) {						// if surface-bound, find sum
		for(ms3=(MolecState)0;ms3<MSMAX1;ms3=(MolecState)(ms3+1)) {
			if(srf->srfprob[i][ms1][ms3]>=0 && ms3!=ms1)
				sum+=srf->srfprob[i][ms1][ms3]; }}

	if(ms1==MSsoln || ms1==MSbsoln) {
		if(ms2==MSsoln || ms2==MSbsoln) {								// solution to solution
			if(probrev<=0)
				rate=surfacerate(prob,0,dt,difc,NULL,SPAirrTrans);
			else
				rate=surfacerate(prob,probrev,dt,difc,NULL,SPArevTrans); }
		else {																				// solution to surface
			if(probrev<=0)
				rate=surfacerate(prob,0,dt,difc,NULL,SPAirrAds);
			else
				rate=surfacerate(prob,probrev,dt,difc,NULL,SPArevAds); }}
	else {
		if(ms2==MSsoln || ms2==MSbsoln) {								// surface to solution
			if(probrev<=0)
				rate=surfacerate(prob,sum,dt,difc,NULL,SPAirrDes);
			else
				rate=surfacerate(prob,probrev,dt,difc,NULL,SPArevDes); }
		else {
			if(probrev<=0)
				rate=surfacerate(prob,sum,dt,difc,NULL,SPAirrFlip);
			else
				rate=surfacerate(prob,probrev,dt,difc,NULL,SPArevFlip); }}

	return rate; }



/******************************************************************************/
/****************************** memory management *****************************/
/******************************************************************************/


/* Allocates maxpanels of shape ps for the surface srf.  The srf element of
the panels are set to srf.  In srf, the correct maxpanel entry is set to
maxpanel, the npanel entry is set to 0, and the proper list of panels are
allocated and cleared.  All points are set to all zeros.  The function returns 1
for success and 0 for failure to allocate memory. */
int panelsalloc(surfaceptr srf,int dim,int maxpanel,enum PanelShape ps) {
	panelptr *pnls,pnl;
	int p,npts,pt,d;
	char **pname,string[STRCHAR];

	npts=panelpoints(ps,dim);
	pname=NULL;
	pnls=NULL;
	CHECK(srf);

	CHECK(pname=(char**) calloc(maxpanel,sizeof(char*)));
	for(p=0;p<maxpanel;p++) pname[p]=NULL;
	for(p=0;p<maxpanel;p++) CHECK(pname[p]=EmptyString());
	for(p=0;p<maxpanel;p++) sprintf(pname[p],"%s%i",surfps2string(ps,string),p);

	CHECK(pnls=(panelptr*) calloc(maxpanel,sizeof(panelptr)));
	for(p=0;p<maxpanel;p++) pnls[p]=NULL;
	for(p=0;p<maxpanel;p++) {
		CHECK(pnls[p]=(panelptr) malloc(sizeof(struct panelstruct)));
		pnl=pnls[p];
		pnl->pname=pname[p];
		pnl->ps=ps;
		pnl->srf=srf;
		pnl->npts=npts;
		pnl->point=NULL;
		pnl->maxneigh=0;
		pnl->nneigh=0;
		pnl->neigh=NULL;
		pnl->emitterabsorb[PFfront]=pnl->emitterabsorb[PFback]=NULL;

		CHECK(pnl->point=(double**) calloc(npts,sizeof(double*)));
		for(pt=0;pt<npts;pt++) pnl->point[pt]=NULL;
		for(pt=0;pt<npts;pt++) {
			CHECK(pnl->point[pt]=(double*) calloc(dim,sizeof(double)));
			for(d=0;d<dim;d++) pnl->point[pt][d]=0; }
		pnl->front[0]=pnl->front[1]=pnl->front[2]=0;
		pnl->jumpp[0]=pnl->jumpp[1]=NULL;
		pnl->jumpf[0]=pnl->jumpf[1]=PFnone; }

	srf->maxpanel[ps]=maxpanel;
	srf->npanel[ps]=0;
	srf->pname[ps]=pname;
	srf->panels[ps]=pnls;

	if(srf->maxemitter[PFfront]) {
		CHECK(emittersalloc(srf,PFfront,srf->srfss->maxspecies)==0); }
	if(srf->maxemitter[PFback]) {
		CHECK(emittersalloc(srf,PFback,srf->srfss->maxspecies)==0); }

	return 1;

 failure:
	if(pname) {
		for(p=0;p<maxpanel;p++) if(pname[p]) free(pname[p]);
		free(pname); }
	if(pnls) {
		for(p=0;p<maxpanel;p++) panelfree(pnls[p]);
		free(pnls); }
	return 0; }


/* Frees a single panel and all of its substructures (but not srf, because
thatÕs a reference and is not owned by the panel).  This is called by
surfacefree and so should not need to be called externally. */
void panelfree(panelptr pnl) {
	int pt;

	if(!pnl) return;
	free(pnl->emitterabsorb[PFback]);
	free(pnl->emitterabsorb[PFfront]);
	free(pnl->neigh);
	if(pnl->npts && pnl->point) {
		for(pt=0;pt<pnl->npts;pt++)
			if(pnl->point[pt]) free(pnl->point[pt]);
		free(pnl->point); }
	free(pnl);
	return; }


/* emittersalloc.  Allocates basic space for emitters (used for concentrations
that match those for unbounded diffusion).  This allocates the srf->maxemitter
and nemitter arrays, as well as the first levels of the emitteramount, and
emitterpos arrays.  It also allocates all of the panel emitterabsorb arrays.
This function may be called multiple times.  On the first call, it will set up
the basics for all existing structures; subsequent times, it will allocates
arrays for any new panels but not do anything else.  Returns 0 for success or 1
for inability to allocate memory; in the latter case, this frees all emitter
stuff for this face of this surface (which can lead to memory leaks, but should
never occur). */
int emittersalloc(surfaceptr srf,enum PanelFace face,int maxspecies) {
	int i1,p;
	enum PanelShape ps;
	panelptr pnl;

	if(!srf->maxemitter[face]) {
		CHECK(srf->maxemitter[face]=(int*) calloc(maxspecies,sizeof(int)));
		for(i1=0;i1<maxspecies;i1++) srf->maxemitter[face][i1]=0;

		CHECK(srf->nemitter[face]=(int*) calloc(maxspecies,sizeof(int)));
		for(i1=0;i1<maxspecies;i1++) srf->nemitter[face][i1]=0;

		CHECK(srf->emitteramount[face]=(double**) calloc(maxspecies,sizeof(double*)));
		for(i1=0;i1<maxspecies;i1++) srf->emitteramount[face][i1]=NULL;

		CHECK(srf->emitterpos[face]=(double***) calloc(maxspecies,sizeof(double**)));
		for(i1=0;i1<maxspecies;i1++) srf->emitterpos[face][i1]=NULL; }

	for(ps=(PanelShape)0;ps<PSMAX;ps=(PanelShape)(ps+1))
		for(p=0;p<srf->maxpanel[ps];p++) {
			pnl=srf->panels[ps][p];
			if(!pnl->emitterabsorb[face]) {
				CHECK(pnl->emitterabsorb[face]=(double*) calloc(maxspecies,sizeof(double)));
				for(i1=0;i1<maxspecies;i1++)
					pnl->emitterabsorb[face][i1]=0; }}

	return 0;

 failure:
	free(srf->maxemitter[face]);
	srf->maxemitter[face]=NULL;
	free(srf->nemitter[face]);
	srf->nemitter[face]=NULL;
	free(srf->emitteramount[face]);
	srf->emitteramount[face]=NULL;
	free(srf->emitterpos[face]);
	srf->emitterpos[face]=NULL;
	for(ps=(PanelShape)0;ps<PSMAX;ps=(PanelShape)(ps+1))
		for(p=0;p<srf->maxpanel[ps];p++) {
			free(srf->panels[ps][p]->emitterabsorb[face]);
			srf->panels[ps][p]->emitterabsorb[face]=NULL; }
	return 1; }



/* surfacealloc.  Allocates a surface structure, and sets all elements to
initial values.  maxspecies is the maximum number of molecular identities, which
is used for allocating action, srfrate, and srfprob.  Colors are set to all 0's
(black), but with alpha values of 1 (opaque); polygon modes are set to 'f' for
face if dim is 3, and to 'e' otherwise; edgepoints is set to 1; action is set to
for reflective (SAreflect) for the solution element and to ÒnoÓ (SAno) for the
surface-bound elements.  The surface rates (srfrate) are all set to 0.  The
surface action probabilities (srfprob) are all set to -1, to indicate that they
have not been calculated yet.  No panels are allocated.  This is called by
surfacessalloc and so should not need to be called externally. */
surfaceptr surfacealloc(int maxspecies,int dim) {
	surfaceptr srf;
	int i;
	enum PanelShape ps;
	enum MolecState ms,ms2;

	srf=(surfaceptr) malloc(sizeof(struct surfacestruct));
	if(!srf) return NULL;
	srf->sname=NULL;
	srf->srfss=NULL;
	srf->action=NULL;
	srf->srfrate=NULL;
	srf->srfprob=NULL;
	srf->srfcumprob=NULL;
	srf->srfnewspec=NULL;
	srf->fcolor[0]=srf->fcolor[1]=srf->fcolor[2]=0;
	srf->bcolor[0]=srf->bcolor[1]=srf->bcolor[2]=0;
	srf->fcolor[3]=srf->bcolor[3]=1;
	srf->edgepts=1;
	srf->edgestipple[0]=1;
	srf->edgestipple[1]=0xFFFF;
	srf->fdrawmode=(dim==3?DMface:DMedge);
	srf->bdrawmode=(dim==3?DMface:DMedge);
	srf->fshiny=0;
	srf->bshiny=0;
	for(ps=(PanelShape)0;ps<PSMAX;ps=(PanelShape)(ps+1)) srf->maxpanel[ps]=0;
	for(ps=(PanelShape)0;ps<PSMAX;ps=(PanelShape)(ps+1)) srf->npanel[ps]=0;
	for(ps=(PanelShape)0;ps<PSMAX;ps=(PanelShape)(ps+1)) srf->pname[ps]=NULL;
	for(ps=(PanelShape)0;ps<PSMAX;ps=(PanelShape)(ps+1)) srf->panels[ps]=NULL;
	srf->port[PFfront]=NULL;
	srf->port[PFback]=NULL;
	srf->totarea=0;
	srf->totpanel=0;
	srf->areatable=NULL;
	srf->paneltable=NULL;
	srf->maxemitter[PFfront]=srf->maxemitter[PFback]=NULL;
	srf->nemitter[PFfront]=srf->nemitter[PFback]=NULL;
	srf->emitteramount[PFfront]=srf->emitteramount[PFback]=NULL;
	srf->emitterpos[PFfront]=srf->emitterpos[PFback]=NULL;

	CHECK(srf->action=(enum SrfAction**) calloc(maxspecies,sizeof(enum SrfAction*)));
	for(i=0;i<maxspecies;i++) srf->action[i]=NULL;
	for(i=0;i<maxspecies;i++) {
		CHECK(srf->action[i]=(enum SrfAction*) calloc(MSMAX1,sizeof(enum SrfAction)));
		for(ms=(MolecState)0;ms<MSMAX1;ms=(MolecState)(ms+1)) {
			if(ms==MSsoln || ms==MSbsoln) srf->action[i][ms]=SAreflect;
			else srf->action[i][ms]=SAno; }}

	CHECK(srf->srfrate=(double***)calloc(maxspecies,sizeof(double**)));
	for(i=0;i<maxspecies;i++) srf->srfrate[i]=NULL;
	for(i=0;i<maxspecies;i++) {
		CHECK(srf->srfrate[i]=(double**)calloc(MSMAX1,sizeof(double*)));
		for(ms=(MolecState)0;ms<MSMAX1;ms=(MolecState)(ms+1)) srf->srfrate[i][ms]=NULL; }
	for(i=0;i<maxspecies;i++)
		for(ms=(MolecState)0;ms<MSMAX1;ms=(MolecState)(ms+1)) {
			CHECK(srf->srfrate[i][ms]=(double*)calloc(MSMAX1,sizeof(double)));
			for(ms2=(MolecState)0;ms2<MSMAX1;ms2=(MolecState)(ms2+1)) srf->srfrate[i][ms][ms2]=-1; }

	CHECK(srf->srfprob=(double***)calloc(maxspecies,sizeof(double**)));
	for(i=0;i<maxspecies;i++) srf->srfprob[i]=NULL;
	for(i=0;i<maxspecies;i++) {
		CHECK(srf->srfprob[i]=(double**)calloc(MSMAX1,sizeof(double*)));
		for(ms=(MolecState)0;ms<MSMAX1;ms=(MolecState)(ms+1)) srf->srfprob[i][ms]=NULL; }
	for(i=0;i<maxspecies;i++)
		for(ms=(MolecState)0;ms<MSMAX1;ms=(MolecState)(ms+1)) {
			CHECK(srf->srfprob[i][ms]=(double*)calloc(MSMAX1,sizeof(double)));
			for(ms2=(MolecState)0;ms2<MSMAX1;ms2=(MolecState)(ms2+1)) srf->srfprob[i][ms][ms2]=-1; }

	CHECK(srf->srfcumprob=(double***)calloc(maxspecies,sizeof(double**)));
	for(i=0;i<maxspecies;i++) srf->srfcumprob[i]=NULL;
	for(i=0;i<maxspecies;i++) {
		CHECK(srf->srfcumprob[i]=(double**)calloc(MSMAX1,sizeof(double*)));
		for(ms=(MolecState)0;ms<MSMAX1;ms=(MolecState)(ms+1)) srf->srfcumprob[i][ms]=NULL; }
	for(i=0;i<maxspecies;i++)
		for(ms=(MolecState)0;ms<MSMAX1;ms=(MolecState)(ms+1)) {
			CHECK(srf->srfcumprob[i][ms]=(double*)calloc(MSMAX1,sizeof(double)));
			for(ms2=(MolecState)0;ms2<MSMAX1;ms2=(MolecState)(ms2+1)) srf->srfcumprob[i][ms][ms2]=-1; }
	
	CHECK(srf->srfnewspec=(int***)calloc(maxspecies,sizeof(int**)));
	for(i=0;i<maxspecies;i++) srf->srfnewspec[i]=NULL;
	for(i=0;i<maxspecies;i++) {
		CHECK(srf->srfnewspec[i]=(int**)calloc(MSMAX1,sizeof(int*)));
		for(ms=(MolecState)0;ms<MSMAX1;ms=(MolecState)(ms+1)) srf->srfnewspec[i][ms]=NULL; }
	for(i=0;i<maxspecies;i++)
		for(ms=(MolecState)0;ms<MSMAX1;ms=(MolecState)(ms+1)) {
			CHECK(srf->srfnewspec[i][ms]=(int*)calloc(MSMAX1,sizeof(int)));
			for(ms2=(MolecState)0;ms2<MSMAX1;ms2=(MolecState)(ms2+1)) srf->srfnewspec[i][ms][ms2]=-1; }
	
	return srf;

 failure:
 	surfacefree(srf,maxspecies);
 	return NULL; }


/* Frees a surface, including all substructures and panels in it.  This is
called by surfacessfree and so should not need to be called externally. */
void surfacefree(surfaceptr srf,int maxspecies) {
	int p,i,emit;
	enum PanelFace face;
	enum PanelShape ps;
	enum MolecState ms;

	if(!srf) return;

	for(face=PFfront;face<=PFback;face=(PanelFace)(face+1)) {
		if(srf->emitterpos[face]) {
			for(i=0;i<maxspecies;i++)
				if(srf->emitterpos[face][i])
					for(emit=0;emit<srf->maxemitter[face][i];emit++)
						free(srf->emitterpos[face][i][emit]); }
		if(srf->emitteramount[face]) {
			for(i=0;i<maxspecies;i++)
				free(srf->emitteramount[face][i]); }
		free(srf->nemitter[face]);
		free(srf->maxemitter[face]); }

	free(srf->paneltable);
	free(srf->areatable);

	for(ps=(PanelShape)0;ps<PSMAX;ps=(PanelShape)(ps+1)) {
		for(p=0;p<srf->maxpanel[ps];p++) {
			if(srf->panels[ps]) panelfree(srf->panels[ps][p]);
			if(srf->pname[ps]) free(srf->pname[ps][p]); }
		free(srf->pname[ps]);
		free(srf->panels[ps]); }

	for(i=0;i<maxspecies;i++) {
		for(ms=(MolecState)0;ms<MSMAX1;ms=(MolecState)(ms+1)) {
			if(srf->srfrate && srf->srfrate[i]) free(srf->srfrate[i][ms]);
			if(srf->srfprob && srf->srfprob[i]) free(srf->srfprob[i][ms]);
			if(srf->srfcumprob && srf->srfcumprob[i]) free(srf->srfcumprob[i][ms]);
			if(srf->srfnewspec && srf->srfnewspec[i]) free(srf->srfnewspec[i][ms]); }
		if(srf->srfrate) free(srf->srfrate[i]);
		if(srf->srfprob) free(srf->srfprob[i]);
		if(srf->srfcumprob) free(srf->srfcumprob[i]);
		if(srf->srfnewspec) free(srf->srfnewspec[i]); }
	free(srf->srfrate);
	free(srf->srfprob);
	free(srf->srfcumprob);
	free(srf->srfnewspec);

	for(i=0;i<maxspecies;i++)
		if(srf->action) free(srf->action[i]);
	free(srf->action);

	free(srf);
	return; }


/* Allocates a surface superstructure for maxsurface surfaces, as well as all of
the surfaces.  Each name is allocated to an empty string of STRCHAR (256)
characters. */
surfacessptr surfacessalloc(int maxsurface,int maxspecies,int dim) {
	surfacessptr srfss;
	int s;

	srfss=(surfacessptr) malloc(sizeof(struct surfacesuperstruct));
	if(!srfss) return NULL;
	srfss->condition=SCinit;
	srfss->sim=NULL;
	srfss->maxspecies=maxspecies;
	srfss->maxsrf=maxsurface;
	srfss->nsrf=0;
	srfss->epsilon=100*DBL_EPSILON;
	srfss->neighdist=-1;
	srfss->snames=NULL;
	srfss->srflist=NULL;
	srfss->maxmollist=0;
	srfss->nmollist=0;
	srfss->srfmollist=NULL;

	CHECK(srfss->snames=(char**) calloc(maxsurface,sizeof(char*)));
	for(s=0;s<maxsurface;s++) srfss->snames[s]=NULL;
	for(s=0;s<maxsurface;s++)
		CHECK(srfss->snames[s]=EmptyString());
	CHECK(srfss->srflist=(surfaceptr*) calloc(maxsurface,sizeof(surfaceptr)));
	for(s=0;s<maxsurface;s++) srfss->srflist[s]=NULL;
	for(s=0;s<maxsurface;s++) {
		CHECK(srfss->srflist[s]=surfacealloc(maxspecies,dim));
		srfss->srflist[s]->srfss=srfss;
		srfss->srflist[s]->sname=srfss->snames[s]; }
	return srfss;

 failure:
 	surfacessfree(srfss);
 	return NULL; }


/* Frees a surface superstructure pointed to by srfss, and all contents in it,
including all of the surfaces and all of their panels. */
void surfacessfree(surfacessptr srfss) {
	int s;

	if(!srfss) return;

	free(srfss->srfmollist);
	if(srfss->srflist) {
		for(s=0;s<srfss->maxsrf;s++) surfacefree(srfss->srflist[s],srfss->maxspecies);
		free(srfss->srflist); }
	if(srfss->snames) {
		for(s=0;s<srfss->maxsrf;s++) free(srfss->snames[s]);
		free(srfss->snames); }
	free(srfss);
	return; }


/******************************************************************************/
/**************************** data structure output ***************************/
/******************************************************************************/

/* surfaceoutput.  Prints out information about all surfaces, including the
surface superstructure, each surface, and panels in the surface. */
void surfaceoutput(simptr sim) {
	int s,p,i,dim,nspecies,jumpfrnt,jumpback,ll,vflag,p2,emit,d;
	surfacessptr srfss;
	surfaceptr srf;
	double **point,*front,prob;
	char **pname,string[STRCHAR];
	panelptr pnl;
	enum SrfAction **action;
	enum MolecState ms,ms2;
	enum PanelFace face;

	vflag=strchr(sim->flags,'v')?1:0;
	printf("SURFACE PARAMETERS\n");
	srfss=sim->srfss;
	dim=sim->dim;
	nspecies=sim->mols?sim->mols->nspecies:0;
	if(!srfss) {
		printf(" No internal surfaces\n\n");
		return; }
	printf(" Surface epsilon and neighbor distances: %g %g\n",srfss->epsilon,srfss->neighdist);

	printf(" Molecule lists checked after diffusion:");
	for(ll=0;ll<srfss->nmollist;ll++)
		if(srfss->srfmollist[ll]&SMLdiffuse) printf(" %s",sim->mols->listname[ll]);
	printf("\n");
	printf(" Molecule lists checked after reactions:");
	for(ll=0;ll<srfss->nmollist;ll++)
		if(srfss->srfmollist[ll]&SMLreact) printf(" %s",sim->mols->listname[ll]);
	printf("\n");
	printf(" Molecule lists checked for surface-bound molecules:");
	for(ll=0;ll<srfss->nmollist;ll++)
		if(srfss->srfmollist[ll]&SMLsrfbound) printf(" %s",sim->mols->listname[ll]);
	printf("\n");

	printf(" Surfaces allocated: %i, surfaces defined: %i\n\n",srfss->maxsrf,srfss->nsrf);
	for(s=0;s<srfss->nsrf;s++) {
		srf=srfss->srflist[s];
		printf(" Surface: %s\n",srfss->snames[s]);
		if(srf->port[PFfront]) printf("  The front of this surface is part of port %s\n",srf->port[PFfront]->portname);
		if(srf->port[PFback]) printf("  The back of this surface is part of port %s\n",srf->port[PFback]->portname);
		printf("  actions for molecules:\n");
		action=srf->action;
		for(i=1;i<nspecies;i++) {
			printf("   %s:",sim->mols->spname[i]);
			printf(" fsoln:%s",surfact2string(action[i][MSsoln],string));
			printf(", bsoln:%s",surfact2string(action[i][MSbsoln],string));
			if(action[i][MSfront]!=SAno) printf(", front:%s",surfact2string(action[i][MSfront],string));
			if(action[i][MSback]!=SAno) printf(", back:%s",surfact2string(action[i][MSback],string));
			if(action[i][MSup]!=SAno) printf(", up:%s",surfact2string(action[i][MSup],string));
			if(action[i][MSdown]!=SAno) printf(", down:%s",surfact2string(action[i][MSdown],string));
			printf("\n");
			for(ms=(MolecState)0;ms<MSMAX1;ms=(MolecState)(ms+1))
				if(action[i][ms]==SAmult)
					for(ms2=(MolecState)0;ms2<MSMAX1;ms2=(MolecState)(ms2+1)) {
						prob=srf->srfprob[i][ms][ms2];
						if(prob>0 && ms!=ms2) {
							printf("    %s -> ",molms2string(ms,string));
							if(srf->srfnewspec[i][ms][ms2]!=-1) printf("%s(",sim->mols->spname[srf->srfnewspec[i][ms][ms2]]);
							printf("%s",molms2string(ms2,string));
							if(srf->srfnewspec[i][ms][ms2]!=-1) printf(")");
							if(srf->srfrate[i][ms][ms2]>=0) printf(", requested rate=%g",srf->srfrate[i][ms][ms2]);
							printf(", actual rate=%g, prob=%g\n",srfcalcrate(sim,srf,i,ms,ms2),prob); }}}

		printf("  front color: %g %g %g %g\n",srf->fcolor[0],srf->fcolor[1],srf->fcolor[2],srf->fcolor[3]);
		printf("  back color: %g %g %g %g\n",srf->bcolor[0],srf->bcolor[1],srf->bcolor[2],srf->bcolor[3]);
		printf("  edge points: %g, polygon modes: %s %s\n",srf->edgepts,surfdm2string(srf->fdrawmode,string),surfdm2string(srf->bdrawmode,string));
		if(srf->edgestipple[1]!=0xFFFF) printf("   edge stippling: %ui %X\n",srf->edgestipple[0],srf->edgestipple[1]);
		if(srf->fshiny!=0) printf("  front shininess: %g\n",srf->fshiny);
		if(srf->bshiny!=0) printf("  back shininess: %g\n",srf->bshiny);

		for(face=PFfront;face<=PFback;face=(PanelFace)(face+1)) {
			if(srf->nemitter[face])
				for(i=0;i<nspecies;i++)
					if(srf->nemitter[face][i]) {
						printf("  %i %s-side emitters defined, of %i allocated:\n",srf->nemitter[face][i],surfface2string(face,string),srf->maxemitter[face][i]);
						for(emit=0;emit<srf->nemitter[face][i];emit++) {
							printf("   %g at (%g",srf->emitteramount[face][i][emit],srf->emitterpos[face][i][emit][0]);
							for(d=1;d<dim;d++)
								printf(",%g",srf->emitterpos[face][i][emit][d]);
							printf(")\n"); }}}

		jumpfrnt=jumpback=0;
		for(i=1;i<nspecies;i++) {
			if(srf->action[i][MSsoln]==SAjump) jumpfrnt=1;
			if(srf->action[i][MSbsoln]==SAjump) jumpback=1; }

		if(srf->maxpanel[PSrect]) {
			pname=srf->pname[PSrect];
			printf("  rectangle panels allocated: %i, defined: %i\n",srf->maxpanel[PSrect],srf->npanel[PSrect]);
			for(p=0;p<srf->npanel[PSrect] && (vflag || p<21);p++) {
				if(!vflag && p==20) {
					printf("   ...\n");
					p=srf->npanel[PSrect]-1; }
				pnl=srf->panels[PSrect][p];
				point=pnl->point;
				front=pnl->front;
				if(dim==1) printf("   %s: %g, facing %c0",pname[p],point[0][0],front[0]==1?'+':'-');
				else if(dim==2) printf("   %s: (%g,%g), (%g,%g), facing: %c%1.0f, length: %g",pname[p],point[0][0],point[0][1],point[1][0],point[1][1],front[0]==1?'+':'-',front[1],Geo_LineLength(point[0],point[1],2));
				else printf("   %s: (%g,%g,%g), (%g,%g,%g), (%g,%g,%g), (%g,%g,%g), facing: %c%1.0f, area: %g",pname[p],point[0][0],point[0][1],point[0][2],point[1][0],point[1][1],point[1][2],point[2][0],point[2][1],point[2][2],point[3][0],point[3][1],point[3][2],front[0]==1?'+':'-',front[1],Geo_QuadArea(point[0],point[1],point[2],point[3],3));
				if(jumpfrnt && pnl->jumpp[PFfront]) printf("; front jump: %s, %s",pnl->jumpp[PFfront]->pname,surfface2string(pnl->jumpf[PFfront],string));
				else if(jumpfrnt) printf("; front jump: NO PANEL");
				if(jumpback && pnl->jumpp[PFback]) printf("; back jump: %s, %s",pnl->jumpp[PFback]->pname,surfface2string(pnl->jumpf[PFback],string));
				else if(jumpback) printf("; back jump: NO PANEL");
				for(face=PFfront;face<=PFback;face=(PanelFace)(face+1)) {
					if(pnl->emitterabsorb[face]) {
						printf("; %s absorb probs.:",surfface2string(face,string));
						for(i=1;i<nspecies;i++)
							printf(" %g",pnl->emitterabsorb[face][i]); }}
				printf("\n");
				if(vflag && pnl->maxneigh) {
					printf("    neighbors allocated: %i, defined: %i",pnl->maxneigh,pnl->nneigh);
					for(p2=0;p2<pnl->nneigh;p2++)
						printf(", %s",pnl->neigh[p2]->pname);
					printf("\n"); }}}
		if(srf->maxpanel[PStri]) {
			pname=srf->pname[PStri];
			printf("  triangle panels allocated: %i, defined: %i\n",srf->maxpanel[PStri],srf->npanel[PStri]);
			for(p=0;p<srf->npanel[PStri] && (vflag || p<21);p++) {
				if(!vflag && p==20) {
					printf("   ...\n");
					p=srf->npanel[PStri]-1; }
				pnl=srf->panels[PStri][p];
				point=pnl->point;
				front=pnl->front;
				if(dim==1) printf("   %s: %g, facing %c0",pname[p],point[0][0],front[0]==1?'+':'-');
				else if(dim==2) printf("   %s: (%g,%g), (%g,%g), facing: (%g,%g), length: %g",pname[p],point[0][0],point[0][1],point[1][0],point[1][1],front[0],front[1],Geo_LineLength(point[0],point[1],2));
				else printf("   %s: (%g,%g,%g), (%g,%g,%g), (%g,%g,%g), facing: (%g,%g,%g), area: %g",pname[p],point[0][0],point[0][1],point[0][2],point[1][0],point[1][1],point[1][2],point[2][0],point[2][1],point[2][2],front[0],front[1],front[2],Geo_TriArea3(point[0],point[1],point[2],front));
				if(jumpfrnt && pnl->jumpp[PFfront]) printf("; front jump: %s, %s",pnl->jumpp[PFfront]->pname,surfface2string(pnl->jumpf[PFfront],string));
				else if(jumpfrnt) printf("; front jump: NO PANEL, face=%s",surfface2string(pnl->jumpf[PFfront],string));
				if(jumpback && pnl->jumpp[PFback]) printf("; back jump: %s, %s",pnl->jumpp[PFback]->pname,surfface2string(pnl->jumpf[PFback],string));
				else if(jumpback) printf("; back jump: NO PANEL, face=%s",surfface2string(pnl->jumpf[PFfront],string));
				for(face=PFfront;face<=PFback;face=(PanelFace)(face+1)) {
					if(pnl->emitterabsorb[face]) {
						printf("; %s absorb probs.:",surfface2string(face,string));
						for(i=1;i<nspecies;i++)
							printf(" %g",pnl->emitterabsorb[face][i]); }}
				printf("\n");
				if(vflag && pnl->maxneigh) {
					printf("    neighbors allocated: %i, defined: %i",pnl->maxneigh,pnl->nneigh);
					for(p2=0;p2<pnl->nneigh;p2++)
						printf(", %s",pnl->neigh[p2]->pname);
					printf("\n"); }}}
		if(srf->maxpanel[PSsph]) {
			pname=srf->pname[PSsph];
			printf("  sphere panels allocated: %i, defined: %i\n",srf->maxpanel[PSsph],srf->npanel[PSsph]);
			for(p=0;p<srf->npanel[PSsph] && (vflag || p<21);p++) {
				if(!vflag && p==20) {
					printf("   ...\n");
					p=srf->npanel[PSsph]-1; }
				pnl=srf->panels[PSsph][p];
				point=pnl->point;
				front=pnl->front;
				if(dim==1) printf("   %s: %g, R=%g, facing: %s",pname[p],point[0][0],point[1][0],front[0]==1?"out":"in");
				else if(dim==2) printf("   %s: (%g,%g), R=%g, facing: %s, draw: %g, length: %g",pname[p],point[0][0],point[0][1],point[1][0],front[0]==1?"out":"in",point[1][1],2.0*PI*point[1][0]);
				else printf("   %s: (%g,%g,%g), R=%g, facing: %s, draw: %g %g, area: %g",pname[p],point[0][0],point[0][1],point[0][2],point[1][0],front[0]==1?"out":"in",point[1][1],point[1][2],4.0*PI*point[1][0]*point[1][0]);
				if(jumpfrnt && pnl->jumpp[PFfront]) printf("; front jump: %s, %s",pnl->jumpp[PFfront]->pname,surfface2string(pnl->jumpf[PFfront],string));
				else if(jumpfrnt) printf("; front jump: NO PANEL, face=%s",surfface2string(pnl->jumpf[PFfront],string));
				if(jumpback && pnl->jumpp[PFback]) printf("; back jump: %s, %s",pnl->jumpp[PFback]->pname,surfface2string(pnl->jumpf[PFback],string));
				else if(jumpback) printf("; back jump: NO PANEL, face=%s",surfface2string(pnl->jumpf[PFfront],string));
				for(face=PFfront;face<=PFback;face=(PanelFace)(face+1)) {
					if(pnl->emitterabsorb[face]) {
						printf("; %s absorb probs.:",surfface2string(face,string));
						for(i=1;i<nspecies;i++)
							printf(" %g",pnl->emitterabsorb[face][i]); }}
				printf("\n");
				if(vflag && pnl->maxneigh) {
					printf("    neighbors allocated: %i, defined: %i",pnl->maxneigh,pnl->nneigh);
					for(p2=0;p2<pnl->nneigh;p2++)
						printf(", %s",pnl->neigh[p2]->pname);
					printf("\n"); }}}
		if(srf->maxpanel[PScyl]) {
			pname=srf->pname[PScyl];
			printf("  cylinder panels allocated: %i, defined: %i\n",srf->maxpanel[PScyl],srf->npanel[PScyl]);
			for(p=0;p<srf->npanel[PScyl] && (vflag || p<21);p++) {
				if(!vflag && p==20) {
					printf("   ...\n");
					p=srf->npanel[PScyl]-1; }
				pnl=srf->panels[PScyl][p];
				point=pnl->point;
				front=pnl->front;
				if(dim==1) printf("   error, cylinders are not implemented in 1-D");
				else if(dim==2) printf("   %s: (%g,%g) to (%g,%g), R=%g, facing: %s, length: %g",pname[p],point[0][0],point[0][1],point[1][0],point[1][1],point[2][0],front[2]==1?"out":"in",2.0*Geo_LineLength(point[0],point[1],2));
				else printf("   %s: (%g,%g,%g) to (%g,%g,%g), R=%g, facing: %s, draw: %g %g, area: %g",pname[p],point[0][0],point[0][1],point[0][2],point[1][0],point[1][1],point[1][2],point[2][0],front[2]==1?"out":"in",point[2][1],point[2][2],2.0*PI*point[2][0]*Geo_LineLength(point[0],point[1],3));
				if(jumpfrnt && pnl->jumpp[PFfront]) printf("; front jump: %s, %s",pnl->jumpp[PFfront]->pname,surfface2string(pnl->jumpf[PFfront],string));
				else if(jumpfrnt) printf("; front jump: NO PANEL, face=%s",surfface2string(pnl->jumpf[PFfront],string));
				if(jumpback && pnl->jumpp[PFback]) printf("; back jump: %s, %s",pnl->jumpp[PFback]->pname,surfface2string(pnl->jumpf[PFback],string));
				else if(jumpback) printf("; back jump: NO PANEL, face=%s",surfface2string(pnl->jumpf[PFfront],string));
				for(face=PFfront;face<=PFback;face=(PanelFace)(face+1)) {
					if(pnl->emitterabsorb[face]) {
						printf("; %s absorb probs.:",surfface2string(face,string));
						for(i=1;i<nspecies;i++)
							printf(" %g",pnl->emitterabsorb[face][i]); }}
				printf("\n");
				if(vflag && pnl->maxneigh) {
					printf("    neighbors allocated: %i, defined: %i",pnl->maxneigh,pnl->nneigh);
					for(p2=0;p2<pnl->nneigh;p2++)
						printf(", %s",pnl->neigh[p2]->pname);
					printf("\n"); }}}
		if(srf->maxpanel[PShemi]) {
			pname=srf->pname[PShemi];
			printf("  hemisphere panels allocated: %i, defined: %i\n",srf->maxpanel[PShemi],srf->npanel[PShemi]);
			for(p=0;p<srf->npanel[PShemi] && (vflag || p<21);p++) {
				if(!vflag && p==20) {
					printf("   ...\n");
					p=srf->npanel[PShemi]-1; }
				pnl=srf->panels[PShemi][p];
				point=pnl->point;
				front=pnl->front;
				if(dim==1) printf("   error, hemispheres are not implemented in 1-D");
				else if(dim==2) printf("   %s: (%g,%g), R=%g, facing: %s, opening: (%g,%g), draw: %g, length: %g",pname[p],point[0][0],point[0][1],point[1][0],front[0]==1?"out":"in",point[2][0],point[2][1],point[1][1],PI*point[1][0]);
				else printf("   %s: (%g,%g,%g), R=%g, facing: %s, opening: (%g,%g,%g), draw: %g %g, area: %g",pname[p],point[0][0],point[0][1],point[0][2],point[1][0],front[0]==1?"out":"in",point[2][0],point[2][1],point[2][2],point[1][1],point[1][2],2.0*PI*point[1][0]*point[1][0]);
				if(jumpfrnt && pnl->jumpp[PFfront]) printf("; front jump: %s, %s",pnl->jumpp[PFfront]->pname,surfface2string(pnl->jumpf[PFfront],string));
				else if(jumpfrnt) printf("; front jump: NO PANEL, face=%s",surfface2string(pnl->jumpf[PFfront],string));
				if(jumpback && pnl->jumpp[PFback]) printf("; back jump: %s, %s",pnl->jumpp[PFback]->pname,surfface2string(pnl->jumpf[PFback],string));
				else if(jumpback) printf("; back jump: NO PANEL, face=%s",surfface2string(pnl->jumpf[PFfront],string));
				for(face=PFfront;face<=PFback;face=(PanelFace)(face+1)) {
					if(pnl->emitterabsorb[face]) {
						printf("; %s absorb probs.:",surfface2string(face,string));
						for(i=1;i<nspecies;i++)
							printf(" %g",pnl->emitterabsorb[face][i]); }}
				printf("\n");
				if(vflag && pnl->maxneigh) {
					printf("    neighbors allocated: %i, defined: %i",pnl->maxneigh,pnl->nneigh);
					for(p2=0;p2<pnl->nneigh;p2++)
						printf(", %s",pnl->neigh[p2]->pname);
					printf("\n"); }}}
		if(srf->maxpanel[PSdisk]) {
			pname=srf->pname[PSdisk];
			printf("  disk panels allocated: %i, defined: %i\n",srf->maxpanel[PSdisk],srf->npanel[PSdisk]);
			for(p=0;p<srf->npanel[PSdisk] && (vflag || p<21);p++) {
				if(!vflag && p==20) {
					printf("   ...\n");
					p=srf->npanel[PSdisk]-1; }
				pnl=srf->panels[PSdisk][p];
				point=pnl->point;
				front=pnl->front;
				if(dim==1) printf("   error, disks are not implemented in 1-D");
				else if(dim==2) printf("   %s: (%g,%g), R=%g, facing: (%g,%g), length: %g",pname[p],point[0][0],point[0][1],point[1][0],front[0],front[1],2.0*point[1][0]);
				else printf("   %s: (%g,%g,%g), R=%g, facing: (%g,%g,%g), draw: %g, area: %g",pname[p],point[0][0],point[0][1],point[0][2],point[1][0],front[0],front[1],front[2],point[1][1],PI*point[1][0]*point[1][0]);
				if(jumpfrnt && pnl->jumpp[PFfront]) printf("; front jump: %s, %s",pnl->jumpp[PFfront]->pname,surfface2string(pnl->jumpf[PFfront],string));
				else if(jumpfrnt) printf("; front jump: NO PANEL, face=%s",surfface2string(pnl->jumpf[PFfront],string));
				if(jumpback && pnl->jumpp[PFback]) printf("; back jump: %s, %s",pnl->jumpp[PFback]->pname,surfface2string(pnl->jumpf[PFback],string));
				else if(jumpback) printf("; back jump: NO PANEL, face=%s",surfface2string(pnl->jumpf[PFfront],string));
				for(face=PFfront;face<=PFback;face=(PanelFace)(face+1)) {
					if(pnl->emitterabsorb[face]) {
						printf("; %s absorb probs.:",surfface2string(face,string));
						for(i=1;i<nspecies;i++)
							printf(" %g",pnl->emitterabsorb[face][i]); }}
				printf("\n");
				if(vflag && pnl->maxneigh) {
					printf("    neighbors allocated: %i, defined: %i",pnl->maxneigh,pnl->nneigh);
					for(p2=0;p2<pnl->nneigh;p2++)
						printf(", %s",pnl->neigh[p2]->pname);
					printf("\n"); }}}

		printf("\n"); }
	return; }


/* writesurfaces.  Writes all information about all surfaces to the file fptr
using a format that can be read by Smoldyn.  This allows a simulation state to
be saved. */
void writesurfaces(simptr sim,FILE *fptr) {
	int s,i,d,d2,j,p,dim,nspecies;
	surfacessptr srfss;
	surfaceptr srf;
	enum MolecState ms1,ms2;
	enum PanelFace face;
	char string[STRCHAR];
	enum PanelShape ps;
	panelptr pnl;
	double **point,*front;

	dim=sim->dim;
	nspecies=sim->mols?sim->mols->nspecies:0;
	fprintf(fptr,"# Surface parameters\n");
	if(!sim->srfss) return;
	srfss=sim->srfss;
	fprintf(fptr,"max_surface %i\n",srfss->maxsrf);
	fprintf(fptr,"epsilon %g\n",srfss->epsilon);
	fprintf(fptr,"neighbor_dist %g\n",srfss->neighdist);
	fprintf(fptr,"\n");
	for(s=0;s<srfss->nsrf;s++) {
		srf=srfss->srflist[s];
		fprintf(fptr,"start_surface %s\n",srfss->snames[s]);
		for(i=1;i<nspecies;i++) {
			fprintf(fptr,"action front %s %s\n",sim->mols->spname[i],surfact2string(srf->action[i][MSsoln],string));
			fprintf(fptr,"action back %s %s\n",sim->mols->spname[i],surfact2string(srf->action[i][MSbsoln],string));
			for(ms1=(MolecState)0;ms1<MSMAX1;ms1=(MolecState)(ms1+1))
				for(ms2=(MolecState)0;ms2<MSMAX1;ms2=(MolecState)(ms2+1))
					if(srf->action[i][ms1]==SAmult && ms1!=ms2) {
						if(srf->srfrate[i][ms1][ms2]>0) {
							fprintf(fptr,"rate %s %s",sim->mols->spname[i],molms2string(ms1,string));
							fprintf(fptr," %s %g",molms2string(ms2,string),srf->srfrate[i][ms1][ms2]);
							if(srf->srfnewspec[i][ms1][ms2]!=-1) fprintf(fptr," %s",sim->mols->spname[srf->srfnewspec[i][ms1][ms2]]);
							fprintf(fptr,"\n"); }
						else if(srf->srfrate[i][ms1][ms2]<0 && srf->srfprob[i][ms1][ms2]>0) {
							fprintf(fptr,"rate_internal %s %s",sim->mols->spname[i],molms2string(ms1,string));
							fprintf(fptr," %s %g",molms2string(ms2,string),srf->srfprob[i][ms1][ms2]);
							if(srf->srfnewspec[i][ms1][ms2]!=-1) fprintf(fptr," %s",sim->mols->spname[srf->srfnewspec[i][ms1][ms2]]);
							fprintf(fptr,"\n"); }}}
		fprintf(fptr,"color front %g %g %g %g\n",srf->fcolor[0],srf->fcolor[1],srf->fcolor[2],srf->fcolor[3]);
		fprintf(fptr,"color back %g %g %g %g\n",srf->bcolor[0],srf->bcolor[1],srf->bcolor[2],srf->bcolor[3]);
		if(srf->fshiny!=0) fprintf(fptr,"shininess front %g\n",srf->fshiny);
		if(srf->bshiny!=0) fprintf(fptr,"shininess back %g\n",srf->bshiny);
		fprintf(fptr,"thickness %g\n",srf->edgepts);
		if(srf->edgestipple[1]!=0xFFFF)
			printf("   stipple %ui 0x%X\n",srf->edgestipple[0],srf->edgestipple[1]);
		fprintf(fptr,"polygon front %s\n",surfdm2string(srf->fdrawmode,string));
		fprintf(fptr,"polygon back %s\n",surfdm2string(srf->bdrawmode,string));
		for(ps=(PanelShape)0;ps<PSMAX;ps=(PanelShape)(ps+1)) {
			if(srf->maxpanel[ps]) fprintf(fptr,"max_panels %s %i\n",surfps2string(ps,string),srf->maxpanel[ps]);
			for(p=0;p<srf->npanel[ps];p++) {
				pnl=srf->panels[ps][p];
				point=pnl->point;
				front=pnl->front;
				fprintf(fptr,"panel %s",surfps2string(ps,string));
				if(ps==PSrect) {
					fprintf(fptr," %c%i",pnl->front[0]>0?'+':'-',(int)(front[1]));
					for(d=0;d<dim;d++) fprintf(fptr," %g",point[0][d]);
					for(d=0;d<dim;d++)
						if(d!=pnl->front[1]) fprintf(fptr," %g",point[dim==2?1:2][d]-point[0][d]); }
				else if(ps==PStri) {
					for(d2=0;d2<dim;d2++)
						for(d=0;d<dim;d++)
							fprintf(fptr," %g",point[d2][d]); }
				else if(ps==PSsph) {
					for(d=0;d<dim;d++) fprintf(fptr," %g",point[0][d]);
					fprintf(fptr," %c%g",front[0]>0?'+':'-',point[1][0]);
					if(dim>1) fprintf(fptr," %g",point[1][1]);
					if(dim>2) fprintf(fptr," %g",point[1][2]); }
				else if(ps==PScyl) {
					for(d=0;d<dim;d++) fprintf(fptr," %g",point[0][d]);
					for(d=0;d<dim;d++) fprintf(fptr," %g",point[1][d]);
					fprintf(fptr," %c%g",front[dim==2?2:0]>0?'+':'-',point[2][0]);
					if(dim>2) fprintf(fptr," %g %g",point[2][1],point[2][2]); }
				else if(ps==PShemi) {
					for(d=0;d<dim;d++) fprintf(fptr," %g",point[0][d]);
					fprintf(fptr," %c%g",front[0]>0?'+':'-',point[1][0]);
					for(d=0;d<dim;d++) fprintf(fptr," %g",point[2][d]);
					if(dim>=2) fprintf(fptr," %g",point[1][1]);
					if(dim>2) fprintf(fptr," %g",point[1][2]); }
				else if(ps==PSdisk) {
					for(d=0;d<dim;d++) fprintf(fptr," %g",point[0][d]);
					fprintf(fptr," %g",point[1][0]);
					for(d=0;d<dim;d++) fprintf(fptr," %g",front[d]);
					if(dim>2) fprintf(fptr," %g",point[1][1]); }
				fprintf(fptr," %s\n",pnl->pname); }}
		for(i=1;i<nspecies;i++)
			for(face=(PanelFace)0;face<2;face=(PanelFace)(face+1))
				if(srf->action[i][face==PFfront?MSsoln:MSbsoln]==SAjump)
					for(ps=(PanelShape)0;ps<PSMAX;ps=(PanelShape)(ps+1))
						for(p=0;p<srf->npanel[ps];p++) {
							pnl=srf->panels[ps][p];
							if(pnl->jumpp[face] && (pnl->jumpf[face]==PFfront || pnl->jumpf[face]==PFback))
								fprintf(fptr,"jump %s %s -> %s %s\n",pnl->pname,surfface2string(face,string),pnl->jumpp[face]->pname,surfface2string(pnl->jumpf[face],string)); }
		for(ps=(PanelShape)0;ps<PSMAX;ps=(PanelShape)(ps+1))
			for(p=0;p<srf->npanel[ps];p++) {
				pnl=srf->panels[ps][p];
				if(pnl->nneigh) {
					fprintf(fptr,"neighbors %s",pnl->pname);
					for(j=0;j<pnl->nneigh;j++) {
						if(pnl->neigh[j]->srf==srf)
							fprintf(fptr," %s",pnl->neigh[j]->pname);
						else
							fprintf(fptr," %s:%s",pnl->neigh[j]->srf->sname,pnl->neigh[j]->pname); }
					fprintf(fptr,"\n"); }}
		fprintf(fptr,"end_surface\n\n"); }
	return; }

void printfException(const char* format, ...);
/* checksurfaceparams.  Checks some surface parameters.  Many more should be
checked as well, although those havenÕt been written yet. */
int checksurfaceparams(simptr sim,int *warnptr) {
	int error,warn;
	int d,c,s,i,dim,nspecies,p,pt,fjump,bjump,fport,bport;
	surfacessptr srfss;
	surfaceptr srf;
	enum MolecState ms,ms1,ms2;
	double syslen,**point,*front,lowwall[3],highwall[3],norm[3],prob;
	enum PanelShape ps;
	enum PanelFace face;
	panelptr pnl;
	char string[STRCHAR];

	error=warn=0;

	dim=sim->dim;
	nspecies=sim->mols?sim->mols->nspecies:0;
	systemcorners(sim,lowwall,highwall);
	syslen=0;
	for(d=0;d<dim;d++) syslen+=(highwall[d]-lowwall[d])*(highwall[d]-lowwall[d]);
	syslen=sqrt(syslen);

	srfss=sim->srfss;
	if(!srfss) {
		if(warnptr) *warnptr=warn;
		return 0; }

	if(srfss->condition!=SCok) {											// condition
		warn++;
		printf(" WARNING: surface structure %s\n",simsc2string(srfss->condition,string)); }

																										// maxspecies
	if(!sim->mols) {warn++;printf(" WARNING: Surfaces may not work because no molecules have been defined\n");}
	else if(sim->mols->maxspecies!=sim->srfss->maxspecies) {error++;printfException(" SMOLDYN BUG: number of molecule species differ between mols and srfss\n");}

																										// nmollist
	if(srfss->nmollist!=sim->mols->nlist) {error++;printfException(" SMOLDYN BUG: mismatch between number of molecule lists in surface and in molecule structures");}

																										// possible superstructure warnings, errors
	if(srfss->nsrf==0) {warn++;printf(" WARNING: Surface superstructure is defined, but no surfaces are defined\n");}
	if(srfss->nsrf>srfss->maxsrf) {error++;printfException(" SMOLDYN BUG: More surfaces are defined than allocated\n");}
	if(srfss->epsilon>0.01*syslen) {warn++;printf(" WARNING: surface epsilon value is large compared to system size\n");}
	if(srfss->neighdist>0.01*syslen) {warn++;printf(" WARNING: surface neighbor distance value is large compared to system size\n");}

	for(s=0;s<srfss->nsrf;s++) {											// surface missing and incorrect elements, not panels
		if(strlen(srfss->snames[s])==0) {error++;printfException(" ERROR: surface %i is unnamed\n",s);}
		if(!srfss->srflist[s]) {error++;printfException(" SMOLDYN BUG: pointer to surface %i is NULL",s);}
		srf=srfss->srflist[s];
		if(srf->sname!=srfss->snames[s]) {error++;printfException(" SMOLDYN BUG: surface %i name pointer does not match surface superstructure pointer\n",s);}
		for(i=1;i<nspecies;i++) {
			if(!srf->action || !srf->action[i]) {error++;printfException(" SMOLDYN BUG: surface %i action element is not allocated properly\n",s);}
			for(ms1=(MolecState)0;ms1<MSMAX1;ms1=(MolecState)(ms1+1)) {
				if(srf->action[i][ms1]<0 || srf->action[i][ms1]>SAno) {error++;printfException(" SMOLDYN BUG: illegal action enumeration for surface %i\n",s);}
				if(!srf->srfrate || !srf->srfrate[i] || !srf->srfrate[i][ms1]) {error++;printfException(" SMOLDYN BUG: surface %i srfrate element is not allocated properly\n",s);}
				if(!srf->srfprob || !srf->srfprob[i] || !srf->srfprob[i][ms1]) {error++;printfException(" SMOLDYN BUG: surface %i srfprob element is not allocated properly\n",s);}
				if(!srf->srfcumprob || !srf->srfcumprob[i] || !srf->srfcumprob[i][ms1]) {error++;printfException(" SMOLDYN BUG: surface %i srfcumprob element is not allocated properly\n",s);}
				if(srf->action[i][ms1]==SAmult) {
					for(ms2=(MolecState)1;ms2<MSMAX1;ms2=(MolecState)(ms2+1)) {
						if(srf->srfprob[i][ms1][ms2]<0) {error++;printfException(" SMOLDYN BUG: surface %i srfprob[%i][%i][0] (transition probability) was never set up\n",s,i,ms1);}
						if(srf->srfprob[i][ms1][ms2]>1) {error++;printfException(" SMOLDYN BUG: surface %i srfprob[%i][%i][0] (transition probability) is greater than 1\n",s,i,ms1);} }}}}
		for(c=0;c<4;c++) {
			if(srf->fcolor[c]<0 || srf->fcolor[c]>1) {error++;printfException(" SMOLDYN BUG: surface %i front color %i value is out of bounds\n",s,c);}
			if(srf->bcolor[c]<0 || srf->bcolor[c]>1) {error++;printfException(" SMOLDYN BUG: surface %i front color %i value is out of bounds\n",s,c);} }
		if(srf->edgepts<0) {error++;printfException(" SMOLDYN BUG: surface %i drawing thickness is negative\n",s);}
		if(srf->fdrawmode<DMno || srf->fdrawmode>DMvef) {error++;printfException(" SMOLDYN BUG: surface %i front drawing mode is out of bounds\n",s);}
		if(srf->bdrawmode<DMno || srf->bdrawmode>DMvef) {error++;printfException(" SMOLDYN BUG: surface %i back drawing mode is out of bounds\n",s);}
		if(srf->fshiny<0 || srf->fshiny>128) {error++;printfException(" SMOLDYN BUG: surface %i front shininess is out of bounds\n",s);}
		if(srf->bshiny<0 || srf->bshiny>128) {error++;printfException(" SMOLDYN BUG: surface %i back shininess is out of bounds\n",s);} }

	for(s=0;s<srfss->nsrf;s++) {											// surface missing and incorrect elements, about panels
		srf=srfss->srflist[s];
		for(ps=(PanelShape)0;ps<PSMAX;ps=(PanelShape)(ps+1)) {
			if(srf->npanel[ps]>srf->maxpanel[ps]) {error++;printfException(" SMOLDYN BUG: surface %i has more panels of shape %i defined than allocated\n",s,ps);}
			if(srf->npanel[ps]>0) {
				if(!srf->pname || !srf->pname[ps]) {error++;printfException(" SMOLDYN BUG: surface %i has no name strings allocated for shape %i\n",s,ps);}
				if(!srf->panels || !srf->panels[ps]) {error++;printfException(" SMOLDYN BUG: surface %i has panels listed but not allocated for shape %i\n",s,ps);} }
			for(p=0;p<srf->npanel[ps];p++) {
				if(strlen(srf->pname[ps][p])==0) {error++;printfException(" ERROR: surface %i, panel shape %i, panel %i is unnamed\n",s,ps,p);}
				if(!srf->panels[ps][p]) {error++;printfException(" SMOLDYN BUG: surface %i, panel shape %i, panel %i pointer is NULL\n",s,ps,p);} }}}

	for(s=0;s<srfss->nsrf;s++) {											// panel missing and incorrect elements
		srf=srfss->srflist[s];
		for(ps=(PanelShape)0;ps<PSMAX;ps=(PanelShape)(ps+1))
			for(p=0;p<srf->npanel[ps];p++) {
				pnl=srf->panels[ps][p];
				if(pnl->pname!=srf->pname[ps][p]) {error++;printfException(" SMOLDYN BUG: surface %i, panel shape %i, panel %i name pointer does not match surface pointer\n",s,ps,p);}
				if(pnl->ps!=ps) {error++;printfException(" SMOLDYN BUG: surface %i, shape %i, panel %i listing of shape does not match actual shape\n",s,ps,p);}
				if(pnl->srf!=srf) {error++;printfException(" SMOLDYN BUG: surface %i, shape %i, panel %i listing of surface does not match actual surface\n",s,ps,p);}
				if(pnl->npts!=panelpoints(ps,dim)) {error++;printfException(" SMOLDYN BUG: surface %i, shape %i, panel %i number of points (npts) is incorrect\n",s,ps,p);}
				for(pt=0;pt<pnl->npts;pt++)
					if(!pnl->point || !pnl->point[pt]) {error++;printfException(" SMOLDYN BUG: surface %i, shape %i, panel %i points element is not fully allocated\n",s,ps,p);} }}

	for(s=0;s<srfss->nsrf;s++) {											// panel points and front
		srf=srfss->srflist[s];
		for(ps=(PanelShape)0;ps<PSMAX;ps=(PanelShape)(ps+1))
			for(p=0;p<srf->npanel[ps];p++) {
				pnl=srf->panels[ps][p];
				point=pnl->point;
				front=pnl->front;
				if(ps==PSrect) {
					if(dim==1) {
						if(point[0][0]<lowwall[0]) {warn++;printf(" WARNING: surface %s, panel %s is entirely outside the system volume\n",srf->sname,pnl->pname);}
						if(point[0][0]>highwall[0]) {warn++;printf(" WARNING: surface %s, panel %s is entirely outside the system volume\n",srf->sname,pnl->pname);}
						if(!(front[0]==-1 || front[0]==1)) {error++;printfException(" SMOLDYN BUG: surface %s, panel %s front vector is set incorrectly\n",srf->sname,pnl->pname);}
						if(front[1]!=0) {error++;printfException(" SMOLDYN BUG: surface %s, panel %s front vector is set incorrectly\n",srf->sname,pnl->pname);} }
					else if(dim==2) {
						Geo_LineNormal(point[0],point[1],norm);
						if(!Geo_LineXaabb2(point[0],point[1],norm,lowwall,highwall)) {warn++;printf(" WARNING: surface %s, panel %s is entirely outside the system volume\n",srf->sname,pnl->pname);}
						if(!(front[0]==-1 || front[0]==1)) {error++;printfException(" SMOLDYN BUG: surface %s, panel %s front vector is set incorrectly\n",srf->sname,pnl->pname);} }}}}
				//?? need to check points and front for other shapes and dimensions

	for(s=0;s<srfss->nsrf;s++) {											// jump surfaces and panels
		srf=srfss->srflist[s];
		fjump=bjump=0;
		for(i=1;i<nspecies;i++) {
			if(srf->action[i][MSsoln]==SAjump) fjump=1;
			if(srf->action[i][MSbsoln]==SAjump) bjump=1; }
		if(fjump)
			for(ps=(PanelShape)0;ps<PSMAX;ps=(PanelShape)(ps+1))
				for(p=0;p<srf->npanel[ps];p++) {
					pnl=srf->panels[ps][p];
					if(!pnl->jumpp[PFfront]) {warn++;printf(" WARNING: front of surface %s has jump action but panel %s has no jump destination\n",srf->sname,pnl->pname);}
					else if(pnl->jumpp[PFfront]==pnl) {warn++;printf(" WARNING: surface %s, panel %s front is a jump type panel that is its own destination\n",srf->sname,pnl->pname);}
					if(pnl->jumpp[PFfront] && !(pnl->jumpf[PFfront]==PFfront || pnl->jumpf[PFfront]==PFback)) {error++;printfException(" SMOLDYN BUG: surface %s, panel %s jumps to an undefined panel face\n",srf->sname,pnl->pname);} }
		if(bjump)
			for(ps=(PanelShape)0;ps<PSMAX;ps=(PanelShape)(ps+1))
				for(p=0;p<srf->npanel[ps];p++) {
					pnl=srf->panels[ps][p];
					if(!pnl->jumpp[PFback]) {warn++;printf(" WARNING: back of surface %s has jump action but panel %s has no jump destination\n",srf->sname,pnl->pname);}
					else if(pnl->jumpp[PFback]==pnl) {warn++;printf(" WARNING: surface %s, panel %s back is a jump type panel that is its own destination\n",srf->sname,pnl->pname);}
					if(pnl->jumpp[PFback] && !(pnl->jumpf[PFfront]==PFfront || pnl->jumpf[PFfront]==PFback)) {error++;printfException(" SMOLDYN BUG: surface %s, panel %s jumps to an undefined panel face\n",srf->sname,pnl->pname);} }}

	for(s=0;s<srfss->nsrf;s++) {											// make sure panel panel neighbors are stored correctly
		srf=srfss->srflist[s];
		for(ps=(PanelShape)0;ps<PSMAX;ps=(PanelShape)(ps+1))
			for(p=0;p<srf->npanel[ps];p++) {
				pnl=srf->panels[ps][p];
				if(pnl->nneigh>0 && !pnl->neigh) {error++;printfException(" SMOLDYN BUG: surface %s, panel %s has %i neighbors, but none listed\n",srf->sname,pnl->pname,pnl->nneigh);} }}

	for(s=0;s<srfss->nsrf;s++) {											// make sure porting actions are linked to ports
		srf=srfss->srflist[s];
		fport=bport=0;
		for(i=0;i<nspecies;i++) {
			if(srf->action[i][MSsoln]==SAport) fport=1;
			if(srf->action[i][MSbsoln]==SAport) bport=1; }
		if(fport && !srf->port[PFfront]) {
			error++;
			printfException(" ERROR: surface %s front face has porting action, but is not linked to a port\n",srf->sname);
			for(i=0;i<nspecies;i++) if(srf->action[i][MSsoln]==SAport) srf->action[i][MSsoln]=SAreflect; }
		if(bport && !srf->port[PFback]) {
			error++;
			printfException(" ERROR: surface %s back face has porting action, but is not linked to a port\n",srf->sname);
			for(i=0;i<nspecies;i++) if(srf->action[i][MSsoln]==SAport) srf->action[i][MSsoln]=SAreflect; }}

	if(sim->mols)																			// check that requested surface rates can be achieved
		for(s=0;s<srfss->nsrf;s++) {
			srf=srfss->srflist[s];
			for(i=1;i<sim->mols->nspecies;i++)
				for(ms=(MolecState)0;ms<MSMAX1;ms=(MolecState)(ms+1))
					if(srf->action[i][ms]==SAmult)
						for(ms2=(MolecState)0;ms2<MSMAX1;ms2=(MolecState)(ms2+1)) {
							prob=srf->srfprob[i][ms][ms2];
							if(prob<0) {
								error++;
								printfException(" SMOLDYN BUG: surface '%s' interaction probability for %s(%s) ->",srf->sname,sim->mols->spname[i],molms2string(ms,string));
								printf(" %s(%s) is negative\n",sim->mols->spname[i],molms2string(ms2,string)); }
							if(prob>0 && ms!=ms2) {
								if(srf->srfrate[i][ms][ms2]>=0)
									if(fabs((srf->srfrate[i][ms][ms2]-srfcalcrate(sim,srf,i,ms,ms2))/srf->srfrate[i][ms][ms2])>0.01) {
										warn++;
										printf(" WARNING: requested surface '%s' rate for %s(%s) ->",srf->sname,sim->mols->spname[i],molms2string(ms,string));
										printf(" %s(%s) cannot be achieved\n",sim->mols->spname[i],molms2string(ms2,string)); }}}}

	if(sim->mols)																			// check that don't have conflicting surface actions and emitters
		for(s=0;s<srfss->nsrf;s++) {
			srf=srfss->srflist[s];
			for(face=PFfront;face<=PFback;face=(PanelFace)(face+1)) {
				ms1=(face==PFfront?MSsoln:MSbsoln);
				for(i=1;i<sim->mols->nspecies;i++)
					if(srf->nemitter[face] && srf->nemitter[face][i])
						if(srf->action[i][ms1]!=SAreflect) {
							warn++;
							printf(" WARNING: surface '%s' %s-side action for %s",srf->sname,surfface2string(face,string),sim->mols->spname[i]);
							printf(" is listed as %s, but molecules will absorb instead because of unbounded emitters\n",surfact2string(srf->action[i][ms1],string)); }}}

	if(sim->mols)																			// check that emitter absorptions aren't pegged to 1
		for(s=0;s<srfss->nsrf;s++) {
			srf=srfss->srflist[s];
			for(face=PFfront;face<=PFback;face=(PanelFace)(face+1))
				for(i=1;i<sim->mols->nspecies;i++)
					if(srf->nemitter[face] && srf->nemitter[face][i])
						for(ps=(PanelShape)0;ps<PSMAX;ps=(PanelShape)(ps+1))
							for(p=0;p<srf->npanel[ps];p++)
								if(srf->panels[ps][p]->emitterabsorb[face][i]==1.0) {
									warn++;
									printf(" WARNING: surface '%s', panel %s, %s-side absorption probability for %s",srf->sname,srf->panels[ps][p]->pname,surfface2string(face,string),sim->mols->spname[i]);
									printf(" cannot be made large enough for accurate effective unbounded diffusion\n"); }}

// check panels that normals are normal and normalized, with proper signs (error)
// check that panels are at least partially inside system volume (warning)
// check winding direction, if possible
// check for asymmetric jumping and that jumping is enabled for jumpable panels
// check that epsilon and neighbor_dist values are reasonable

	if(warnptr) *warnptr=warn;
	return error; }


/******************************************************************************/
/****************************** structure set up ******************************/
/******************************************************************************/


/* surfsetcondition.  Sets the surface superstructure condition to cond, if
appropriate.  Set upgrade to 1 if this is an upgrade, to 0 if this is a
downgrade, or to 2 to set the condition independent of its current value. */
void surfsetcondition(surfacessptr surfss,enum StructCond cond,int upgrade) {
	if(!surfss) return;
	if(upgrade==0 && surfss->condition>cond) surfss->condition=cond;
	else if(upgrade==1 && surfss->condition<cond) surfss->condition=cond;
	else if(upgrade==2) surfss->condition=cond;
	if(surfss->condition<surfss->sim->condition) {
		cond=surfss->condition;
		simsetcondition(surfss->sim,cond==SCinit?SClists:cond,0); }
	return; }


/* surfsetepsilon.  Sets the epsilon value in the surface superstructure.
Returns 0 for success, 2 for no surface superstructure, or 3 for an illegal
requested value (²0). */
int surfsetepsilon(simptr sim,double epsilon) {
	if(!sim->srfss) return 2;
	if(epsilon<=0) return 3;
	sim->srfss->epsilon=epsilon;
	return 0; }


/* surfsetneighdist.  Sets the neighbor distance value in the surface
superstructure.  Returns 0 for success, 2 for no surface superstructure, or 3
for an illegal requested value (²0). */
int surfsetneighdist(simptr sim,double neighdist) {
	if(!sim->srfss) return 2;
	if(neighdist<=0) return 3;
	sim->srfss->neighdist=neighdist;
	return 0; }


/* surfsetemitterabsorption.  Sets emitter absorption probabilities for panels
based on emitter information in the surface structures.  Returns 0 for success
or 1 if one or more of the distances between emitters and a surface panel was
zero (which leads to divide-by-zero errors). */
int surfsetemitterabsorption(simptr sim) {
	surfacessptr srfss;
	surfaceptr srf;
	panelptr pnl;
	int dim,s,i,nspecies,emit,er,p;
	enum PanelFace face;
	enum PanelShape ps;
	double difc,middle[DIMMAX],normal[DIMMAX],numer,denom,amount,*pos,dist,vdiff[DIMMAX],kappa,prob;

	srfss=sim->srfss;
	dim=sim->dim;
	nspecies=sim->mols->nspecies;
	er=0;
	for(s=0;s<srfss->nsrf;s++)
		for(face=PFfront;face<=PFback;face=(PanelFace)(face+1)) {
			srf=srfss->srflist[s];
			if(srf->nemitter[face])
				for(i=1;i<nspecies;i++)
					if(srf->nemitter[face][i]) {
						difc=sim->mols->difc[i][MSsoln];
						for(ps=(PanelShape)0;ps<PSMAX;ps=(PanelShape)(ps+1))
							for(p=0;p<srf->npanel[ps];p++) {
								pnl=srf->panels[ps][p];
								panelmiddle(pnl,middle,dim,1);						// middle position
								panelnormal(pnl,middle,face==PFfront?PFback:PFfront,dim,normal);	// normal vector
								numer=0;
								denom=0;
								for(emit=0;emit<srf->nemitter[face][i];emit++) {
									amount=srf->emitteramount[face][i][emit];
									pos=srf->emitterpos[face][i][emit];
									dist=distanceVVD(middle,pos,dim);
									if(!(dist>0)) er=1;
									denom+=amount/dist;
									sumVD(1.0,middle,-1.0,pos,vdiff,dim);
									numer+=amount*dotVVD(vdiff,normal,dim)/(dist*dist*dist); }
								kappa=difc*numer/denom;
								prob=surfaceprob(kappa,0,sim->dt,difc,NULL,SPAirrAds);
								pnl->emitterabsorb[face][i]=prob; }}}

	return er; }


/* surfsettimestep.  Sets the simulation time step for surface parameters.  This
includes setting the neighbor distance (srf->neighdist) to 3 times the longest
surface-bound diffusion rms step-length, and setting surface interaction
probabilities (srf->prob).  All probabilities are either simply set to 0 or 1 or
are set to an intermediate value with the SurfaceParam.c function srfprob.  The
latter ones account for reversible or competing processes, as appropriate.  They
are cumulative probabilities.  Returns 0 for success or 2 if the molecules
arenÕt adequately set up. */
int surfsettimestep(simptr sim) {
	surfacessptr srfss;
	surfaceptr srf;
	int nspecies,i,s,er;
	double dt,**difc,sum,***srfrate,***srfprob,***srfcumprob,rate,**difstep,difstepmax;
	enum MolecState ms1,ms2;

	srfss=sim->srfss;
	if(!srfss) return 0;
	if(srfss->condition!=SCparams) return 0;
	if(!sim->mols) {
		surfsetcondition(srfss,SCok,1);
		return 0; }
	if(sim->mols->condition<SCok) return 2;

	nspecies=sim->mols->nspecies;
	dt=sim->dt;
	difc=sim->mols->difc;
	difstep=sim->mols->difstep;

	if(srfss->neighdist<0) {												// set neighdist
		difstepmax=0;
		for(i=0;i<nspecies;i++)
			for(ms1=MSfront;ms1<MSMAX;ms1=(MolecState)(ms1+1))
				if(difstep[i][ms1]>difstepmax) difstepmax=difstep[i][ms1];
		surfsetneighdist(sim,3.0*difstepmax); }

	for(s=0;s<srfss->nsrf;s++) {										// set probabilities
		srf=srfss->srflist[s];
		srfrate=srf->srfrate;
		srfprob=srf->srfprob;
		srfcumprob=srf->srfcumprob;
		for(i=0;i<nspecies;i++) {
			for(ms1=(MolecState)0;ms1<MSMAX1;ms1=(MolecState)(ms1+1)) {
				if(srf->action[i][ms1]==SAmult) {

					sum=0;																	// for surface-bound, find total rate for an event
					if(ms1!=MSsoln && ms1!=MSbsoln)
						for(ms2=(MolecState)0;ms2<MSMAX1;ms2=(MolecState)(ms2+1))
							if(ms2!=ms1 && srfrate[i][ms1][ms2]>=0) sum+=srfrate[i][ms1][ms2];

					for(ms2=(MolecState)0;ms2<MSMAX1;ms2=(MolecState)(ms2+1)) {						// record actual probability of each event
						rate=srfrate[i][ms1][ms2];
						if(ms1==ms2) srfprob[i][ms1][ms2]=0;			// same state, so set to 0
						else if(rate==0) srfprob[i][ms1][ms2]=0;	// rate was requested to be 0
						else if(rate>0) {													// non-zero rate was entered
							if(ms1==MSsoln || ms1==MSbsoln) {
								if(ms2==MSsoln || ms2==MSbsoln) {				// solution to solution
									if(srfrate[i][ms2][ms1]<=0)
										srfprob[i][ms1][ms2]=surfaceprob(rate,0,dt,difc[i][MSsoln],NULL,SPAirrTrans);
									else
										srfprob[i][ms1][ms2]=surfaceprob(rate,srfrate[i][ms2][ms1],dt,difc[i][MSsoln],NULL,SPArevTrans); }
								else {																// solution to surface
									if(srfrate[i][ms2][ms1]<=0)
										srfprob[i][ms1][ms2]=surfaceprob(rate,0,dt,difc[i][MSsoln],NULL,SPAirrAds);
									else
										srfprob[i][ms1][ms2]=surfaceprob(rate,srfrate[i][ms2][ms1],dt,difc[i][MSsoln],NULL,SPArevAds); }}
							else {
								if(ms2==MSsoln || ms2==MSbsoln) {				// surface to solution
									if(srfrate[i][ms2][ms1]<=0)
										srfprob[i][ms1][ms2]=surfaceprob(rate,sum,dt,difc[i][MSsoln],NULL,SPAirrDes);
									else
										srfprob[i][ms1][ms2]=surfaceprob(rate,srfrate[i][ms2][ms1],dt,difc[i][MSsoln],NULL,SPArevDes); }
								else {																// surface to surface
									if(srfrate[i][ms2][ms1]<=0)
										srfprob[i][ms1][ms2]=surfaceprob(rate,sum,dt,0,NULL,SPAirrFlip);
									else
										srfprob[i][ms1][ms2]=surfaceprob(rate,srfrate[i][ms2][ms1],dt,0,NULL,SPArevFlip); }}}
						else if(srfprob[i][ms1][ms2]>0)						// only prob was entered
							srfrate[i][ms1][ms2]=-1;
						else if(srfprob[i][ms1][ms2]==0);					// probably neither rate nor prob were entered, but prob already set to 0
						else if(srfprob[i][ms1][ms2]<0) {					// neither rate nor prob were entered
							srfprob[i][ms1][ms2]=0;
							srfrate[i][ms1][ms2]=-2; }}

					sum=0;
					for(ms2=(MolecState)0;ms2<MSMAX1;ms2=(MolecState)(ms2+1))									// find ms1->ms1 probability
						if(ms2!=ms1) sum+=srfprob[i][ms1][ms2];
					if(sum>1.0) {
						srfprob[i][ms1][ms1]=0;
						for(ms2=(MolecState)0;ms2<MSMAX1;ms2=(MolecState)(ms2+1))
							srfprob[i][ms1][ms2]/=sum; }
					else srfprob[i][ms1][ms1]=1.0-sum;

					sum=0;																			// find cumulative probability
					for(ms2=(MolecState)0;ms2<MSMAX1;ms2=(MolecState)(ms2+1)) {
						sum+=srfprob[i][ms1][ms2];
						srfcumprob[i][ms1][ms2]=sum; }}}}}

	er=surfsetemitterabsorption(sim);
	if(er)
		printf("WARNING: an unbounded emitter is at a surface panel, which will cause inaccurate operation\n");

	surfsetcondition(srfss,SCok,1);
	return 0; }


/* surfchangeneighbors.  Adds or removes neighbors to or from a panelÕs list of
neighbors.  pnl is the panel whose neighbor list should be modified, neighlist
is a list of neighboring panels to be added or removed, nneigh is the number of
neighbors that are listed in neighlist, and add is 1 if those listed in
neighlist should be added, or 0 if they should be removed.  If all neighbors
should be removed, send neighlist in as NULL.  This allocates space as needed.
It returns 0 for success or 1 if not enough space could be allocated. */
int surfchangeneighbors(panelptr pnl,panelptr *neighlist,int nneigh,int add) {
	int newnneigh,p,p2;
	panelptr *newneigh;

	newnneigh=pnl->nneigh+nneigh;
	if(add && newnneigh>pnl->maxneigh) {		// need to allocate more space
		newneigh=(panelptr*)calloc(newnneigh,sizeof(panelptr));
		if(!newneigh) return 1;
		for(p=0;p<pnl->nneigh;p++) newneigh[p]=pnl->neigh[p];
		for(;p<newnneigh;p++) newneigh[p]=NULL;
		free(pnl->neigh);
		pnl->maxneigh=newnneigh;
		pnl->neigh=newneigh; }

	if(add) {
		for(p=0;p<nneigh;p++)
			pnl->neigh[pnl->nneigh++]=neighlist[p]; }
	else if(!neighlist) {
		pnl->nneigh=0; }
	else {
		for(p=0;p<nneigh;p++) {
			for(p2=0;p2<pnl->nneigh && pnl->neigh[p2]!=neighlist[p];p2++);
			if(pnl->neigh[p2]==neighlist[p])
				pnl->neigh[p2]=pnl->neigh[--pnl->nneigh]; }}

	return 0; }


/* surfaddemitter.  Adds an emitter to a surface so that it can be used for
simulating unbounded diffusion.  This takes care of any necessary memory
allocating, and also works if there are more panels than there were before.  srf
is the surface that the emitter is being added to, face is the surface face, i
is the species number, amount is the emitter amount, flux, or weighting, pos is
the dim-dimensional position of the emitter, and dim is the system
dimensionality.  Returns 0 for success or 1 if this is unable to allocate
memory.  This does not calculate the panel absorption probabilities, but does
allocate space for them, if needed. */
int surfaddemitter(surfaceptr srf,enum PanelFace face,int i,double amount,double *pos,int dim) {
	int er,oldmax,newmax,emit,d;
	double *newamount,**newpos;

	newamount=NULL;
	newpos=NULL;
	er=emittersalloc(srf,face,srf->srfss->maxspecies);
	CHECK(!er);

	if(srf->nemitter[face][i]==srf->maxemitter[face][i]) {	// allocate emitter space if needed
		oldmax=srf->maxemitter[face][i];
		newmax=oldmax*2+1;

		CHECK(newamount=(double*) calloc(newmax,sizeof(double)));	// emitteramount
		for(emit=0;emit<oldmax;emit++)
			newamount[emit]=srf->emitteramount[face][i][emit];
		for(;emit<newmax;emit++) newamount[emit]=0;

		CHECK(newpos=(double**) calloc(newmax,sizeof(double*)));	// emitterpos
		for(emit=0;emit<oldmax;emit++)
			newpos[emit]=srf->emitterpos[face][i][emit];
		for(;emit<newmax;emit++)
			newpos[emit]=NULL;
		for(emit=oldmax;emit<newmax;emit++) {
			CHECK(newpos[emit]=(double*) calloc(dim,sizeof(double)));
			for(d=0;d<dim;d++) newpos[emit][d]=0; }

		free(srf->emitteramount[face][i]);						// replace old with new
		srf->emitteramount[face][i]=newamount;
		free(srf->emitterpos[face][i]);
		srf->emitterpos[face][i]=newpos;
		srf->maxemitter[face][i]=newmax; }

	emit=srf->nemitter[face][i]++;									// add new emitter
	srf->emitteramount[face][i][emit]=amount;
	for(d=0;d<dim;d++)
		srf->emitterpos[face][i][emit][d]=pos[d];

	surfsetcondition(srf->srfss,SCparams,0);

	return 0;

 failure:
	free(newamount);
	free(newpos);
	return 1; }



/* surfreadstring.  Reads and processes one line of text from the configuration
file, or some other source, for the surface indexed srfindex.  If the surface
index is not known, then set srfindex to -1.  The first word of the line should
be sent in as word and the rest sent in as line2.  If this function is
successful, it returns the surface index and it does not change the contents of
erstr; if not, it returns -1 and it writes an error message to erstr. */
int surfreadstring(simptr sim,int srfindex,char *word,char *line2,char *erstr) {
	char nm[STRCHAR],nm1[STRCHAR],nm2[STRCHAR],facenm[STRCHAR],shapenm[STRCHAR],actnm[STRCHAR],ch,*chptr;
	int d,dim,pdim,pt,i,p,p2,i1,i2,i3,col,itct,s2,er;
	unsigned int ui1,ui2;
	const int maxpnllist=32;
	panelptr pnl,pnllist[32];
	double fltv1[9],**point,*front,f1;
	surfacessptr srfss;
	surfaceptr srf,srf2;
	enum PanelShape ps,ps2;
	enum PanelFace face,face2;
	enum SrfAction act;
	enum MolecState ms1,ms2;
	enum DrawMode dm;

	dim=sim->dim;
	srfss=sim->srfss;
	if(srfindex>=0) srf=srfss->srflist[srfindex];
	else srf=NULL;

	if(!strcmp(word,"name")) {								// name
		itct=sscanf(line2,"%s",nm);
		CHECKS(itct==1,"error reading surface name");
		srfindex=stringfind(srfss->snames,srfss->nsrf,nm);
		if(srfindex<0) {
			CHECKS(srfss->nsrf<srfss->maxsrf,"more surfaces are being defined than were allocated");
			srfindex=srfss->nsrf++;
			strncpy(srfss->snames[srfindex],nm,STRCHAR-1);
			srfss->snames[srfindex][STRCHAR-1]='\0';
			srf=srfss->srflist[srfindex];
			surfsetcondition(srfss,SClists,0); }
		else
			srf=srfss->srflist[srfindex];
		CHECKS(!strnword(line2,2),"unexpected text following name"); }

	else if(!strcmp(word,"action")) {							// action
		CHECKS(srf,"need to enter surface name before action");
		CHECKS(sim->mols,"need to enter molecules before action");
		itct=sscanf(line2,"%s %s %s",facenm,nm,actnm);
		CHECKS(itct==3,"action format: face molecule action");
		face=surfstring2face(facenm);
		CHECKS(face!=PFnone,"in action, face name needs to be 'front', 'back', or 'both'");
		if(!strcmp(nm,"all")) i=-2;
		else i=stringfind(sim->mols->spname,sim->mols->nspecies,nm);
		CHECKS(i!=-1,"in action, molecule name not recognized");
		act=surfstring2act(actnm);
		CHECKS(act<=SAmult,"in action statement, action not recognized or not permitted");
		if(face==PFfront || face==PFboth) {
			if(i==-2) for(i2=0;i2<sim->mols->nspecies;i2++) srf->action[i2][MSsoln]=act;
			else srf->action[i][MSsoln]=act; }
		if(face==PFback || face==PFboth) {
			if(i==-2) for(i2=0;i2<sim->mols->nspecies;i2++) srf->action[i2][MSbsoln]=act;
			else srf->action[i][MSbsoln]=act; }
		surfsetcondition(srfss,SCparams,0);
		CHECKS(!strnword(line2,4),"unexpected text following action"); }

	else if(!strcmp(word,"rate")) {								// rate
		CHECKS(srf,"need to enter surface name before rate");
		CHECKS(sim->mols,"need to enter molecules before rate");
		itct=sscanf(line2,"%s %s %s %lg",nm,nm1,nm2,&f1);
		CHECKS(itct==4,"rate format: molecule state1 state2 value [new_species]");
		if(!strcmp(nm,"all")) i=-2;
		else i=stringfind(sim->mols->spname,sim->mols->nspecies,nm);
		CHECKS(i!=-1,"in rate, molecule name not recognized");
		ms1=molstring2ms(nm1);
		CHECKS(ms1<=MSbsoln,"in rate, state1 is not recognized or not permitted");
		ms2=molstring2ms(nm2);
		CHECKS(ms2<=MSbsoln,"in rate, state2 is not recognized or not permitted");
		CHECKS(ms1!=ms2,"in rate, it is not permitted to list the same state twice");
		CHECKS(f1>=0,"negative surface rate values are not permitted");
		line2=strnword(line2,5);
		i3=-1;
		if(line2) {
			itct=sscanf(line2,"%s",nm);
			CHECKS(itct==1,"cannot read new species name");
			i3=stringfind(sim->mols->spname,sim->mols->nspecies,nm);
			CHECKS(i3!=-1,"new species name not recognized");
			line2=strnword(line2,2); }
		if(i==-2)
			for(i2=0;i2<sim->mols->nspecies;i2++) {
				srf->srfrate[i2][ms1][ms2]=f1;
				srf->action[i2][ms1]=SAmult;
				srf->srfnewspec[i2][ms1][ms2]=i3; }
		else {
			srf->srfrate[i][ms1][ms2]=f1;
			srf->action[i][ms1]=SAmult;
			srf->srfnewspec[i][ms1][ms2]=i3; }
		surfsetcondition(srfss,SCparams,0);
		CHECKS(!line2,"unexpected text following rate"); }

	else if(!strcmp(word,"rate_internal")) {			// rate_internal
		CHECKS(srf,"need to enter surface name before rate_internal");
		CHECKS(sim->mols,"need to enter molecules before rate_internal");
		itct=sscanf(line2,"%s %s %s %lg",nm,nm1,nm2,&f1);
		CHECKS(itct==4,"rate_internal format: molecule state1 state2 value");
		if(!strcmp(nm,"all")) i=-2;
		else i=stringfind(sim->mols->spname,sim->mols->nspecies,nm);
		CHECKS(i!=-1,"in rate_internal, molecule name not recognized");
		ms1=molstring2ms(nm1);
		CHECKS(ms1<=MSbsoln,"in rate_internal, state1 is not recognized or not permitted");
		ms2=molstring2ms(nm2);
		CHECKS(ms2<=MSbsoln,"in rate_internal, state2 is not recognized or not permitted");
		CHECKS(f1>=0,"negative surface rate_internal values are not permitted");
		line2=strnword(line2,5);
		i3=-1;
		if(line2) {
			itct=sscanf(line2,"%s",nm);
			CHECKS(itct==1,"cannot read new species name");
			i3=stringfind(sim->mols->spname,sim->mols->nspecies,nm);
			CHECKS(i3!=-1,"new species name not recognized");
			line2=strnword(line2,2); }
		if(i==-2)
			for(i2=0;i2<sim->mols->nspecies;i2++) {
				srf->srfprob[i2][ms1][ms2]=f1;
				srf->action[i2][ms1]=SAmult;
				srf->srfnewspec[i2][ms1][ms2]=i3; }
		else {
			srf->srfprob[i][ms1][ms2]=f1;
			srf->action[i][ms1]=SAmult;
			srf->srfnewspec[i][ms1][ms2]=i3; }
		surfsetcondition(srfss,SCparams,0);
		CHECKS(!line2,"unexpected text following rate_internal"); }

	else if(!strcmp(word,"color") || !strcmp(word,"colour")) {		// color
		CHECKS(srf,"need to enter surface name before color");
		itct=sscanf(line2,"%s %lg %lg %lg",facenm,&fltv1[0],&fltv1[1],&fltv1[2]);
		CHECKS(itct==4,"color format: face red green blue [alpha]");
		face=surfstring2face(facenm);
		CHECKS(face!=PFnone,"in color, face name needs to be 'front', 'back', or 'both'");
		for(col=0;col<3;col++) {
			CHECKS(fltv1[col]>=0 && fltv1[col]<=1,"color values need to be between 0 and 1"); }
		if((line2=strnword(line2,5))!=NULL) {
			itct=sscanf(line2,"%lg",&fltv1[3]);
			CHECKS(itct==1,"color format: face red green blue [alpha]");
			CHECKS(fltv1[3]>=0 && fltv1[3]<=1,"alpha value needs to be between 0 and 1");
			line2=strnword(line2,2); }
		else fltv1[3]=1;
		if(face==PFfront || face==PFboth)
			for(col=0;col<4;col++) srf->fcolor[col]=fltv1[col];
		if(face==PFback || face==PFboth)
			for(col=0;col<4;col++) srf->bcolor[col]=fltv1[col];
		CHECKS(!line2,"unexpected text following color"); }

	else if(!strcmp(word,"thickness")) {				// thickness
		CHECKS(srf,"need to enter surface name before thickness");
		itct=sscanf(line2,"%lg",&(srf->edgepts));
		CHECKS(itct==1,"thickness value is missing");
		CHECKS(srf->edgepts>=0,"thickness value needs to be at least 0");
		CHECKS(!strnword(line2,2),"unexpected text following thickness"); }

	else if(!strcmp(word,"stipple")) {					// stipple
		CHECKS(srf,"need to enter surface name before stipple");
		itct=sscanf(line2,"%u %x",&ui1,&ui2);
		CHECKS(itct==2,"stipple format: factor pattern");
		CHECKS(ui1>=1,"stipple factor need to be >=1");
		CHECKS(ui2>=0 && ui2 <=0xFFFF,"stipple pattern needs to be between 0x00 and 0xFFFF");
		srf->edgestipple[0]=ui1;
		srf->edgestipple[1]=ui2;
		CHECKS(!strnword(line2,3),"unexpected text following stipple"); }

	else if(!strcmp(word,"polygon")) {					// polygon
		CHECKS(srf,"need to enter surface name before polygon");
		itct=sscanf(line2,"%s %s",facenm,nm1);
		CHECKS(itct==2,"polygon format: face drawmode");
		face=surfstring2face(facenm);
		CHECKS(face!=PFnone,"in polygon, face name needs to be 'front', 'back', or 'both'");
		dm=surfstring2dm(nm1);
		CHECKS(dm!=DMnone,"in polygon, drawing mode is not recognized");
		if(face==PFfront || face==PFboth) srf->fdrawmode=dm;
		if(face==PFback || face==PFboth) srf->bdrawmode=dm;
		CHECKS(!strnword(line2,3),"unexpected text following polygon"); }

	else if(!strcmp(word,"shininess")) {				// shininess
		CHECKS(srf,"need to enter surface name before shininess");
		itct=sscanf(line2,"%s %lg",facenm,&f1);
		CHECKS(itct==2,"shininess format: face value");
		face=surfstring2face(facenm);
		CHECKS(face!=PFnone,"face name needs to be 'front', 'back', or 'both'");
		CHECKS(f1>=0 && f1<=128,"shininess value needs to be between 0 and 128");
		if(face==PFfront || face==PFboth) srf->fshiny=f1;
		if(face==PFback || face==PFboth) srf->bshiny=f1;
		CHECKS(!strnword(line2,3),"unexpected text following shininess"); }
	
	else if(!strcmp(word,"max_panels")) {					// max_panels
		CHECKS(srf,"need to enter surface name before max_panels");
		itct=sscanf(line2,"%s %i",shapenm,&i1);
		CHECKS(itct==2,"max_panels format: shape number");
		ps=surfstring2ps(shapenm);
		CHECKS(ps<PSMAX,"in max_panels, unknown panel shape");
		CHECKS(dim!=1 || ps<=PSsph,"in max_panels, panel shape is not permitted for a 1-D system");
		CHECKS(srf->maxpanel[ps]==0,"max_panels can only be entered once per surface, for each shape");
		CHECKS(i1>=0,"max_panels number needs to be at least 0");
		if(i1>0) {
			CHECKS(panelsalloc(srf,dim,i1,ps),"memory error at max_panels statement"); }
		CHECKS(!strnword(line2,3),"unexpected text following max_panels"); }

	else if(!strcmp(word,"panel")) {							// panel
		CHECKS(srf,"need to enter surface name before panel");
		itct=sscanf(line2,"%s",shapenm);
		CHECKS(itct==1,"in panel, panel shape needs to be entered");
		ps=surfstring2ps(shapenm);
		CHECKS(ps<PSMAX,"in panel, unknown panel shape");
		CHECKS(dim!=1 || ps<=PSsph,"in panel, panel shape is not permitted for a 1-D system");
		CHECKS(srf->maxpanel[ps]>0,"need to enter max_panels before panel for this shape");
		CHECKS(srf->npanel[ps]<srf->maxpanel[ps],"more panels are specified for this shape than were allocated");
		p=srf->npanel[ps]++;
		pnl=srf->panels[ps][p];
		point=pnl->point;
		front=pnl->front;
		line2=strnword(line2,2);
		CHECKS(line2,"panel data missing");
		if(ps==PSrect) {														// panel r ...
			itct=sscanf(line2,"%c",&ch);
			CHECKS(itct==1,"error reading rectangle panel direction (should be + or -)");
			CHECKS(ch=='+' || ch=='-',"error reading rectangle panel direction (should be + or -)");
			if(ch=='+') line2=strchr(line2,'+')+1;
			else line2=strchr(line2,'-')+1;
			itct=sscanf(line2,"%i",&pdim);
			CHECKS(itct==1,"error reading rectangle panel dimension");
			CHECKS(pdim>=0 && pdim<dim,"impossible rectangle panel dimension number");
			line2=strnword(line2,2);
			CHECKS(line2,"rectangle panel is missing the initial coordinate");
			itct=strreadnd(line2,2*dim-1,fltv1,NULL);
			CHECKS(itct==2*dim-1,"rectangle panel is missing values");
			line2=strnword(line2,2*dim-1+1);
			if(dim==1) {
				point[0][0]=fltv1[0]; }
			else if(dim==2) {
				pt=1;
				if(ch=='-') pt*=-1;
				if(pdim==0) pt*=-1;
				if(fltv1[2]<0) pt*=-1;
				if(pt<0) pt=0;
				point[pt][0]=fltv1[0];
				point[pt][1]=fltv1[1];
				point[!pt][0]=fltv1[0]+(pdim==0?0:fltv1[2]);
				point[!pt][1]=fltv1[1]+(pdim==1?0:fltv1[2]);
				front[2]=(double)(!pdim); }
			else if(dim==3) {
				pt=1;
				if(ch=='-') pt*=-1;
				if(fltv1[3]*fltv1[4]<0) pt*=-1;
				if(pt<0) pt=3;
				point[0][0]=fltv1[0];
				point[0][1]=fltv1[1];
				point[0][2]=fltv1[2];
				point[pt][0]=fltv1[0]+(pdim==2?fltv1[3]:0);
				point[pt][1]=fltv1[1]+(pdim==0?fltv1[3]:0);
				point[pt][2]=fltv1[2]+(pdim==1?fltv1[4]:0);
				point[2][0]=fltv1[0]+(pdim==0?0:fltv1[3]);
				point[2][1]=fltv1[1]+(pdim==1?0:(pdim==0?fltv1[3]:fltv1[4]));
				point[2][2]=fltv1[2]+(pdim==2?0:fltv1[4]);
				point[4-pt][0]=fltv1[0]+(pdim==1?fltv1[3]:0);
				point[4-pt][1]=fltv1[1]+(pdim==2?fltv1[4]:0);
				point[4-pt][2]=fltv1[2]+(pdim==0?fltv1[4]:0);
				if(pdim==0) front[2]=(double)(pt==1?1:2);
				else if(pdim==1) front[2]=(double)(pt==1?2:0);
				else front[2]=(double)(pt==1?0:1); }
			front[0]=(double)(ch=='+'?1:-1);
			front[1]=(double)pdim; }
		else if(ps==PStri) {							// panel t ...
			itct=strreadnd(line2,dim*dim,fltv1,NULL);
			CHECKS(itct==dim*dim,"tpanel is missing values");
			line2=strnword(line2,dim*dim+1);
			if(dim==1) {
				point[0][0]=fltv1[0];
				front[0]=(double)1; }
			else if(dim==2) {
				point[0][0]=fltv1[0];
				point[0][1]=fltv1[1];
				point[1][0]=fltv1[2];
				point[1][1]=fltv1[3];
				Geo_LineNormal(point[0],point[1],front); }
			else if(dim==3) {
				point[0][0]=fltv1[0];
				point[0][1]=fltv1[1];
				point[0][2]=fltv1[2];
				point[1][0]=fltv1[3];
				point[1][1]=fltv1[4];
				point[1][2]=fltv1[5];
				point[2][0]=fltv1[6];
				point[2][1]=fltv1[7];
				point[2][2]=fltv1[8];
				Geo_TriNormal(point[0],point[1],point[2],front); }}
		else if(ps==PSsph) {							// panel s ...
			itct=strreadnd(line2,2*dim,fltv1,NULL);
			CHECKS(itct==2*dim,"sphere panel is missing values");
			line2=strnword(line2,2*dim+1);
			if(dim==1) {
				point[0][0]=fltv1[0];
				point[1][0]=fabs(fltv1[1]);
				front[0]=(double)sign(fltv1[1]); }
			else if(dim==2) {
				point[0][0]=fltv1[0];
				point[0][1]=fltv1[1];
				point[1][0]=fabs(fltv1[2]);
				point[1][1]=fltv1[3];
				CHECKS(point[1][1]>0,"drawing slices for sphere panel needs to be positive");
				front[0]=(double)sign(fltv1[2]); }
			else if(dim==3) {
				point[0][0]=fltv1[0];
				point[0][1]=fltv1[1];
				point[0][2]=fltv1[2];
				point[1][0]=fabs(fltv1[3]);
				point[1][1]=fltv1[4];
				CHECKS(point[1][1]>0,"drawing slices for sphere panel needs to be positive");
				point[1][2]=fltv1[5];
				CHECKS(point[1][2]>0,"drawing stacks for sphere panel needs to be positive");
				front[0]=(double)sign(fltv1[3]); }}
		else if(ps==PScyl) {								// panel c ...
			itct=strreadnd(line2,4*dim-3,fltv1,NULL);
			CHECKS(itct==4*dim-3,"cylinder panel is missing values");
			line2=strnword(line2,4*dim-3+1);
			if(dim==1) {
				CHECKS(0,"cylinder panels cannot be used with a 1 dimensional system"); }
			else if(dim==2) {
				point[0][0]=fltv1[0];
				point[0][1]=fltv1[1];
				point[1][0]=fltv1[2];
				point[1][1]=fltv1[3];
				CHECKS(!(point[0][0]==point[1][0] && point[0][1]==point[1][1]),"cylinder ends need to be at different locations");
				point[2][0]=fabs(fltv1[4]);
				Geo_LineNormal(point[0],point[1],front);
				front[2]=(double)sign(fltv1[4]); }
			else if(dim==3) {
				point[0][0]=fltv1[0];
				point[0][1]=fltv1[1];
				point[0][2]=fltv1[2];
				point[1][0]=fltv1[3];
				point[1][1]=fltv1[4];
				point[1][2]=fltv1[5];
				CHECKS(!(point[0][0]==point[1][0] && point[0][1]==point[1][1] && point[0][2]==point[1][2]),"cylinder ends need to be at different locations");
				point[2][0]=fabs(fltv1[6]);
				point[2][1]=fltv1[7];
				CHECKS(point[2][1]>0,"drawing slices for cylinder panel needs to be positive");
				point[2][2]=fltv1[8];
				CHECKS(point[2][2]>0,"drawing stacks for cylinder panel needs to be positive");
				front[2]=(double)sign(fltv1[6]); }}
		else if(ps==PShemi) {								// panel h ...
			itct=strreadnd(line2,3*dim,fltv1,NULL);
			CHECKS(itct==3*dim,"hemisphere panel is missing values");
			line2=strnword(line2,3*dim+1);
			if(dim==1) {
				CHECKS(0,"hemisphere panels cannot be used with a 1 dimensional system"); }
			else if(dim==2) {
				point[0][0]=fltv1[0];
				point[0][1]=fltv1[1];
				point[1][0]=fabs(fltv1[2]);
				point[1][1]=fltv1[5];
				CHECKS(point[1][1]>0,"drawing slices for hemisphere panel needs to be positive");
				point[2][0]=fltv1[3];
				point[2][1]=fltv1[4];
				CHECKS(normalizeVD(point[2],2)>0,"outward pointing vector cannot be 0 length");
				front[0]=(double)sign(fltv1[2]); }
			else if(dim==3) {
				point[0][0]=fltv1[0];
				point[0][1]=fltv1[1];
				point[0][2]=fltv1[2];
				point[1][0]=fabs(fltv1[3]);
				point[1][1]=fltv1[7];
				CHECKS(point[1][1]>0,"drawing slices for hemisphere panel needs to be positive");
				point[1][2]=fltv1[8];
				CHECKS(point[1][2]>0,"drawing stacks for hemisphere panel needs to be positive");
				point[2][0]=fltv1[4];
				point[2][1]=fltv1[5];
				point[2][2]=fltv1[6];
				CHECKS(normalizeVD(point[2],3)>0,"outward pointing vector cannot be 0 length");
				front[0]=(double)sign(fltv1[3]); }}
		else if(ps==PSdisk) {								// panel d ...
			itct=strreadnd(line2,3*dim-1,fltv1,NULL);
			CHECKS(itct==3*dim-1,"disk panel is missing values");
			line2=strnword(line2,3*dim-1+1);
			if(dim==1) {
				CHECKS(0,"disk panels cannot be used with a 1 dimensional system"); }
			else if(dim==2) {
				point[0][0]=fltv1[0];
				point[0][1]=fltv1[1];
				point[1][0]=fltv1[2];
				CHECKS(point[1][0]>0,"disk panel radius needs to be positive");
				front[0]=fltv1[3];
				front[1]=fltv1[4];
				CHECKS(normalizeVD(front,2)>0,"disk normal vector cannot be 0 length"); }
			else if(dim==3) {
				point[0][0]=fltv1[0];
				point[0][1]=fltv1[1];
				point[0][2]=fltv1[2];
				point[1][0]=fltv1[3];
				CHECKS(point[1][0]>0,"disk panel radius needs to be positive");
				point[1][1]=fltv1[7];
				CHECKS(point[1][1]>0,"drawing slices for disk panel needs to be positive");
				front[0]=fltv1[4];
				front[1]=fltv1[5];
				front[2]=fltv1[6];
				CHECKS(normalizeVD(front,3)>0,"disk normal vector cannot be 0 length"); }}
		else {
			CHECKS(0,"Error in code: impossible panel shape"); }
		if(line2) {
			itct=sscanf(line2,"%s",nm);
			CHECKS(itct==1,"Error reading panel name");
			for(ps2=(PanelShape)0;ps2<PSMAX;ps2=(PanelShape)(ps2+1)) {
				i1=srf->npanel[ps2]-(ps==ps2?1:0);
				p2=stringfind(srf->pname[ps2],i1,nm);
				if(p2!=-1) {					// panel name was used before; need to copy into prior panel
					CHECKS(ps2==ps,"Panel name was used previously for a different panel shape");
					for(pt=0;pt<pnl->npts;pt++)
						for(d=0;d<dim;d++)
							srf->panels[ps2][p2]->point[pt][d]=point[pt][d];
					for(d=0;d<dim;d++)
						srf->panels[ps2][p2]->front[d]=front[d];
					pnl=srf->panels[ps2][p2];
					p=p2;
					srf->npanel[ps]--; }}
			strcpy(srf->pname[ps][p],nm);
			line2=strnword(line2,2); }
		surfsetcondition(srfss,SClists,0);
		CHECKS(!line2,"unexpected text following panel"); }

	else if(!strcmp(word,"jump")) {								// jump
		CHECKS(srf,"need to enter surface name before jump");
		itct=sscanf(line2,"%s %s",nm,facenm);
		CHECKS(itct==2,"format for jump: panel1 face1 -> panel2 face2");
		p=ps=(PanelShape)0;
		while(ps<PSMAX && (p=stringfind(srf->pname[ps],srf->npanel[ps],nm))==-1) ps=(PanelShape)(ps+1);
		CHECKS(p>=0,"first panel name listed in jump is not recognized");
		face=surfstring2face(facenm);
		CHECKS(face<=PFback,"first face listed in jump needs to be 'front' or 'back'");
		CHECKS(line2=strnword(line2,3),"format for jump: panel1 face1 -> panel2 face2");
		itct=sscanf(line2,"%s",nm);
		CHECKS(itct==1,"format for jump: panel1 face1 -> panel2 face2");
		if(!strcmp(nm,"->")) i1=0;
		else if(!strcmp(nm,"<->")) i1=1;
		else CHECKS(0,"jump operator needs to be -> or <->");
		CHECKS(line2=strnword(line2,2),"format for jump: panel1 face1 -> panel2 face2");
		itct=sscanf(line2,"%s %s",nm,facenm);
		CHECKS(itct==2,"format for jump: panel1 face1 -> panel2 face2");
		p2=stringfind(srf->pname[ps],srf->npanel[ps],nm);
		CHECKS(p2>=0,"second panel name listed in jump is not recognized, or not same shape as first panel");
		face2=surfstring2face(facenm);
		CHECKS(face2<=PFback,"second face listed in jump needs to be 'front' or 'back'");
		srf->panels[ps][p]->jumpp[face]=srf->panels[ps][p2];
		srf->panels[ps][p]->jumpf[face]=face2;
		if(i1) {
			srf->panels[ps][p2]->jumpp[face2]=srf->panels[ps][p];
			srf->panels[ps][p2]->jumpf[face2]=face; }
		CHECKS(!strnword(line2,3),"unexpected text following jump"); }

	else if(!strcmp(word,"neighbors")) {					// neighbors
		CHECKS(srf,"need to enter surface name before neighbors");
		itct=sscanf(line2,"%s",nm);
		CHECKS(itct==1,"format for neighbors: panel neigh1 neigh2 ...");
		p=ps=(PanelShape)0;
		while(ps<PSMAX && (p=stringfind(srf->pname[ps],srf->npanel[ps],nm))==-1) ps=(PanelShape)(ps+1);
		CHECKS(p>=0,"first panel name listed in neighbors is not recognized");
		pnl=srf->panels[ps][p];
		for(i1=0;i1<maxpnllist && (line2=strnword(line2,2));i1++) {
			itct=sscanf(line2,"%s",nm);
			CHECKS(itct==1,"format for neighbors: panel neigh1 neigh2 ...");
			if(strchr(nm,':')) {
				chptr=strchr(nm,':')+1;
				*(chptr-1)='\0';
				s2=stringfind(srfss->snames,srfss->nsrf,nm);
				CHECKS(s2>=0,"surface name is not recognized");
				srf2=srfss->srflist[s2]; }
			else {
				chptr=nm;
				srf2=srf; }
			p=ps=(PanelShape)0;
			while(ps<PSMAX && (p=stringfind(srf2->pname[ps],srf2->npanel[ps],chptr))==-1) ps=(PanelShape)(ps+1);
			CHECKS(p>=0,"a neighbor panel name is not recognized");
			pnllist[i1]=srf2->panels[ps][p]; }
		CHECKS(i1<maxpnllist,"too many neighbor panels listed in one line");
		er=surfchangeneighbors(pnl,pnllist,i1,1);
		CHECKS(!er,"out of memory allocating neighbor list"); }

	else if(!strcmp(word,"unbounded_emitter")) {	// unbounded_emitter
		CHECKS(srf,"need to enter surface name before unbounded_emitter");
		itct=sscanf(line2,"%s %s %lf",facenm,nm,&f1);
		CHECKS(itct==3,"format for unbounded_emitter: face species amount position");
		face=surfstring2face(facenm);
		CHECKS(face==PFfront || face==PFback,"face must be 'front' or 'back'");
		i=stringfind(sim->mols->spname,sim->mols->nspecies,nm);
		CHECKS(i>0,"unrecognized species name");
		line2=strnword(line2,4);
		CHECKS(line2,"format for unbounded_emitter: face species amount position");
		itct=strreadnd(line2,dim,fltv1,NULL);
		CHECKS(itct==dim,"format for unbounded_emitter: face species amount position");
		er=surfaddemitter(srf,face,i,f1,fltv1,dim);
		CHECKS(er==0,"out of memory adding emitter");
		CHECKS(!strnword(line2,dim+1),"unexpected text following unbounded_emitter"); }

	else if(!strcmp(word,"action_front")) {				// action_front
		CHECKS(0,"the action_front statement has been replaced with action (remove the underscore)"); }

	else if(!strcmp(word,"action_back")) {				// action_back
		CHECKS(0,"the action_back statement has been replaced with action (remove the underscore)"); }

	else if(!strcmp(word,"action_both")) {				// action_both
		CHECKS(0,"the action_both statement has been replaced with action (remove the underscore)"); }

	else if(!strcmp(word,"polygon_front")) {			// polygon_front, got[4]
		CHECKS(0,"the polygon_front statement has been replaced with polygon (remove the underscore)"); }

	else if(!strcmp(word,"polygon_back")) {			// polygon_back, got[5]
		CHECKS(0,"the polygon_back statement has been replaced with polygon (remove the underscore)"); }

	else if(!strcmp(word,"polygon_both")) {			// polygon_both, got[4,5]
		CHECKS(0,"the polygon_both statement has been replaced with polygon (remove the underscore)"); }

	else if(!strcmp(word,"color_front")) {				// color_front, got[1]
		CHECKS(0,"the color_front statement has been replaced with color (remove the underscore)"); }

	else if(!strcmp(word,"color_back")) {					// color_back, got[2]
		CHECKS(0,"the color_back statement has been replaced with color (remove the underscore)"); }

	else if(!strcmp(word,"color") || !strcmp(word,"color_both")) {		// color, color_both, got[1,2]
		CHECKS(0,"the color_both statement has been replaced with color (remove the underscore)"); }

	else {																				// unknown word
		CHECKS(0,"syntax error within surface block: statement not recognized"); }

	return srfindex;

 failure:
	return -1; }


/* loadsurface loads a surface from an already opened disk file pointed to with
fptr.  lctrptr is a pointer to the line counter, which is updated each time a
line is read.  If successful, it returns 0 and the surface is added to the
surface superstructure in sim, which should have been already allocated.
Otherwise it returns the updated line counter along with an error message.  If a
surface with the same name (entered by the user) already exists, this function
can add more panels to it.  It can also allocate and set up a new surface.  If
this runs successfully, the complete surface structure is set up, with the
exception of box issues.  If the routine fails, any new surface structure is
freed. */
int loadsurface(simptr sim,ParseFilePtr *pfpptr,char *line2,char *erstr) {
	ParseFilePtr pfp;
	char word[STRCHAR];
	int done,pfpcode,firstline2,s;

	pfp=*pfpptr;
	CHECKS(sim->srfss,"PROGRAM BUG: surface superstructure not allocated in loadsurface");
	done=0;
	s=-1;
	firstline2=line2?1:0;

	while(!done) {
		if(pfp->lctr==0 && !strchr(sim->flags,'q'))
			printf(" Reading file: '%s'\n",pfp->fname);
		if(firstline2) {
			strcpy(word,"name");
			pfpcode=1;
			firstline2=0; }
		else
			pfpcode=Parse_ReadLine(&pfp,word,&line2,erstr);
		*pfpptr=pfp;
		CHECKS(pfpcode!=3,erstr);

		if(pfpcode==0);																// already taken care of
		else if(pfpcode==2) {													// end reading
			done=1; }
		else if(pfpcode==3) {													// error
			CHECKS(0,"SMOLDYN BUG: parsing error"); }
		else if(!strcmp(word,"end_surface")) {				// end_surface
			CHECKS(!line2,"unexpected text following end_surface");
			return 0; }
		else if(!line2) {															// just word
			CHECKS(0,"unknown word or missing parameter"); }
		else {
			s=surfreadstring(sim,s,word,line2,erstr);
			CHECKS(s>=0,erstr); }}

	CHECKS(0,"end of file encountered before end_surface statement");	// end of file

 failure:																					// failure
	return 1; }


/* setupsurfaces.  Sets up surface molecule lists, area lookup tables, and action
probabilities.  If calculated probabilities exceed 1 or add up to more than 1,
they are adjusted as needed, although this may affect simulation results.  No
warnings are returned about these possible problems, so they should be checked
elsewhere.  Returns 0 for success, 1 for inability to allocate memory, or 2 for
molecules not being sufficiently set up beforehand.  This function may be called
at setup, or later on during the simulation. */
int setupsurfaces(simptr sim) {
	surfacessptr srfss;
	surfaceptr srf;
	int i,ll,maxmollist,s,totpanel,pindex,p;
	enum MolecState ms;
	enum SMLflag *newsrfmollist;
	double totarea,*areatable,area;
	panelptr *paneltable;
	enum PanelShape ps;

	srfss=sim->srfss;
	if(!srfss) return 0;
	if(sim->mols && sim->mols->condition<=SClists) return 2;

	if(sim->mols && srfss->condition<=SClists) {
		if(sim->mols->nlist>srfss->maxmollist) {
			maxmollist=sim->mols->maxlist;								// allocate srfmollist array
			if(maxmollist>0) {
				newsrfmollist=(enum SMLflag*) calloc(maxmollist,sizeof(enum SMLflag));
				if(!newsrfmollist) return 1; }
			else newsrfmollist=NULL;
			for(ll=0;ll<maxmollist;ll++)
				newsrfmollist[ll]=SMLno;
			free(srfss->srfmollist);
			srfss->srfmollist=newsrfmollist;
			srfss->maxmollist=maxmollist; }
		srfss->nmollist=sim->mols->nlist;

		if(srfss->nmollist) {
			for(i=1;i<sim->mols->nspecies;i++)					// set srfmollist flags
				for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1)) {
					ll=sim->mols->listlookup[i][ms];
					if(sim->mols->difc[i][ms]>0)
						srfss->srfmollist[ll]=(SMLflag)(srfss->srfmollist[ll]|SMLdiffuse);
					if(rxnisprod(sim,i,ms,1))
						srfss->srfmollist[ll]=(SMLflag)(srfss->srfmollist[ll]|SMLreact);
					if(ms!=MSsoln)
						srfss->srfmollist[ll]=(SMLflag)(srfss->srfmollist[ll]|SMLsrfbound); }}

		for(s=0;s<srfss->nsrf;s++) {									// area lookup tables
			srf=srfss->srflist[s];
			totarea=surfacearea(srf,sim->dim,&totpanel);
			if(totpanel) {
				areatable=(double*)calloc(totpanel,sizeof(double));
				if(!areatable) return 1;
				paneltable=(panelptr*)calloc(totpanel,sizeof(panelptr));
				if(!paneltable) {free(areatable);return 1;}
				pindex=0;
				area=0;
				for(ps=(PanelShape)0;ps<PSMAX;ps=(PanelShape)(ps+1)) {
					for(p=0;p<srf->npanel[ps];p++) {
						area+=panelarea(srf->panels[ps][p],sim->dim);
						areatable[pindex]=area;
						paneltable[pindex]=srf->panels[ps][p];
						pindex++; }}
				srf->totarea=totarea;
				srf->totpanel=totpanel;
				free(srf->areatable);
				srf->areatable=areatable;
				free(srf->paneltable);
				srf->paneltable=paneltable; }}

		surfsetcondition(srfss,SCparams,1); }

	surfsettimestep(sim);

	return 0; }


/******************************************************************************/
/************************* core simulation functions **************************/
/******************************************************************************/

/* Returns the side of the panel pnl that point pt is on, which is either a ÔfÕ
or a ÔbÕ for front or back, respectively.  ÔbÕ is returned if the point is
exactly at the panel.  The value returned by this function defines the side that
pt is on, so should either be called or exactly copied for other functions that
care. */
enum PanelFace panelside(double* pt,panelptr pnl,int dim,double *distptr) {
	enum PanelFace face;
	double **point,*front,dist,cylnorm[3];
	int d;

	point=pnl->point;
	front=pnl->front;
	dist=0;

	if(pnl->ps==PSrect) {														// rectangle
		d=(int)front[1];
		dist=front[0]*(pt[d]-point[0][d]); }
	else if(pnl->ps==PStri || pnl->ps==PSdisk) {			// triangle, disk
		for(d=0;d<dim;d++) dist+=(pt[d]-point[0][d])*front[d]; }
	else if(pnl->ps==PSsph || pnl->ps==PShemi) {			// sphere, hemisphere
		for(d=0;d<dim;d++) dist+=(pt[d]-point[0][d])*(pt[d]-point[0][d]);
		dist=front[0]*(sqrt(dist)-point[1][0]); }
	else if(pnl->ps==PScyl) {												// cylinder
		if(dim==2) {
			for(d=0;d<dim;d++) dist+=(pt[d]-point[0][d])*front[d];
			dist=front[2]*(fabs(dist)-point[2][0]); }
		else {
			dist=Geo_LineNormal3D(point[0],point[1],pt,cylnorm);
			dist=front[2]*(dist-point[2][0]); }}
	else
		dist=0;

	face=dist>0?PFfront:PFback;
	if(distptr) *distptr=dist;
	return face; }


/* panelnormal.  Returns, in norm, the normal vector for the panel pnl, that
points outwards from the face side.  If this is a curved panel, such as a sphere
or a cylinder, then pos is the position on the surface for which the local
normal should be computed. */
void panelnormal(panelptr pnl,double *pos,enum PanelFace face,int dim,double *norm) {
	int d;
	double **point,*front;

	point=pnl->point;
	front=pnl->front;

	if(pnl->ps==PSrect) {
		for(d=0;d<dim;d++) norm[d]=0;
		norm[(int)front[1]]=((face==PFfront && front[0]==1) || (face==PFback && front[0]==-1))?1.0:-1.0; }
	else if(pnl->ps==PStri || pnl->ps==PSdisk) {
		if(face==PFfront)
			for(d=0;d<dim;d++) norm[d]=front[d];
		else
			for(d=0;d<dim;d++) norm[d]=-front[d]; }
	else if(pnl->ps==PSsph || pnl->ps==PShemi) {
		Geo_SphereNormal(point[0],pos,((face==PFfront && front[0]==1) || (face==PFback && front[0]==-1))?1:-1,dim,norm); }
	else if(pnl->ps==PScyl) {
		if(dim==2) {
			Geo_LineNormal2D(point[0],point[1],pos,norm);
			if((face==PFback && front[2]==1) || (face==PFfront && front[2]==-1))
				for(d=0;d<dim;d++) norm[d]*=-1; }
		else if(dim==3) {
			Geo_LineNormal3D(point[0],point[1],pos,norm);
			if((face==PFback && front[2]==1) || (face==PFfront && front[2]==-1))
				for(d=0;d<dim;d++) norm[d]*=-1; }}

	return; }



/* 	lineXpanel.  Determines if line from pt1 to pt2 crosses panel pnl, using a
dim dimensional system.  The panel includes all of its edges.  If it crosses, 1
is returned, the face that is on the pt1 side of the panel is returned in
faceptr, crsspt is set to the coordinates of the crossing point, and cross
points to the crossing position on the line, which is a number between 0 and 1,
inclusive.  If it does not cross, 0 is returned and the other values are
undefined.  Crossing is handled very carefully such that the exact locations of
pt1 and pt2, using tests that are identical to those in panelside, are used to
determine which sides of the panel they are on.  While crsspt will be returned
with coordinates that are very close to the panel location, it may not be
precisely at the panel, and there is no certainty about which side of the panel
it will be on; if it matters, fix it with fixpt2panel.

If the line crosses the panel more than once, which can happen for spherical or
other curved panels, the smaller of the two crossing points is returned.  For
sphere and cylinder, 0 is returned if either both points are inside or both
points are outside and the line segment does not cross the object twice.

Each portion of this routine does the same things, and usually in the same
order.  First, the potential intersection face is determined, then the crossing
value, then the crossing point, and finally it finds if intersection actually
happened.  For hemispheres and cylinders, if intersection does not happen for
the first of two possible crossing points, it is then checked for the second
point. */
int lineXpanel(double *pt1,double *pt2,panelptr pnl,int dim,double *crsspt,enum PanelFace *face1ptr,enum PanelFace *face2ptr,double *crossptr,double *cross2ptr) {
	surfaceptr srf;
	double **point,*front,dist1,dist2;
	double dot,cross,cross2,nrdist,nrpos;
	int intsct,d;
	enum PanelFace face1,face2,facein;

	srf=pnl->srf;
	point=pnl->point;
	front=pnl->front;
	face1=panelside(pt1,pnl,dim,&dist1);
	face2=panelside(pt2,pnl,dim,&dist2);
	cross=cross2=-1;

	if(pnl->ps==PSrect) {														// rectangle
		if(face1==face2) return 0;
		cross=dist1/(dist1-dist2);
		for(d=0;d<dim;d++) crsspt[d]=pt1[d]+cross*(pt2[d]-pt1[d]);
		if(dim==1) intsct=1;
		else if(dim==2) {
			d=(int)front[2];
			intsct=((point[0][d]<=crsspt[d] && crsspt[d]<=point[1][d]) || (point[1][d]<=crsspt[d] && crsspt[d]<=point[0][d])); }
		else {
			d=(int)front[2];
			intsct=((point[0][d]<=crsspt[d] && crsspt[d]<=point[1][d]) || (point[1][d]<=crsspt[d] && crsspt[d]<=point[0][d]));
			d=(d+1)%3;
			if(d==(int)front[1]) d=(d+1)%3;
			intsct=intsct && ((point[1][d]<=crsspt[d] && crsspt[d]<=point[2][d]) || (point[2][d]<=crsspt[d] && crsspt[d]<=point[1][d])); }}

	else if(pnl->ps==PStri) {												// triangle
		if(face1==face2) return 0;
		cross=dist1/(dist1-dist2);
		for(d=0;d<dim;d++) crsspt[d]=pt1[d]+cross*(pt2[d]-pt1[d]);
		if(dim==1) intsct=1;
		else if(dim==2) {
			intsct=((point[0][0]<=crsspt[0] && crsspt[0]<=point[1][0]) || (point[1][0]<=crsspt[0] && crsspt[0]<=point[0][0]));
			intsct=intsct && ((point[0][1]<=crsspt[1] && crsspt[1]<=point[1][1]) || (point[1][1]<=crsspt[1] && crsspt[1]<=point[0][1])); }
		else {
			intsct=Geo_PtInTriangle(point[0],point[1],point[2],front,crsspt); }}

	else if(pnl->ps==PSsph || pnl->ps==PShemi) {		// sphere, hemisphere
		facein=front[0]>0?PFback:PFfront;
		if(face1==facein && face1==face2) return 0;
		cross=Geo_LineXSphs(pt1,pt2,point[0],point[1][0],dim,&cross2,&nrdist,&nrpos);
		if(face1==face2 && (nrdist>point[1][0] || nrpos<0 || nrpos>1)) return 0;
		if(face1==facein)
			cross=cross2;
		for(d=0;d<dim;d++) crsspt[d]=pt1[d]+cross*(pt2[d]-pt1[d]);
		if(pnl->ps==PSsph) intsct=1;
		else {
			dot=0;
			for(d=0;d<dim;d++) dot+=(crsspt[d]-point[0][d])*point[2][d];
			intsct=(dot<=0);
			if(!intsct && face1==face2) {
				cross=cross2;
				face1=(face1==PFfront)?PFback:PFfront;
				for(d=0;d<dim;d++) crsspt[d]=pt1[d]+cross*(pt2[d]-pt1[d]);
				dot=0;
				for(d=0;d<dim;d++) dot+=(crsspt[d]-point[0][d])*point[2][d];
				intsct=(dot<=0); }}}

	else if(pnl->ps==PScyl) {									// cylinder
		facein=(int)front[2]>0?PFback:PFfront;
		if(face1==facein && face1==face2) return 0;
		if(dim==2) cross=Geo_LineXCyl2s(pt1,pt2,point[0],point[1],front,point[2][0],&cross2,&nrdist,&nrpos);
		else cross=Geo_LineXCyls(pt1,pt2,point[0],point[1],point[2][0],&cross2,&nrdist,&nrpos);
		if(face1==face2 && (nrdist>point[2][0] || nrpos<0 || nrpos>1)) return 0;
		if(face1==facein)
			cross=cross2;
		for(d=0;d<dim;d++) crsspt[d]=pt1[d]+cross*(pt2[d]-pt1[d]);
		intsct=Geo_PtInSlab(point[0],point[1],crsspt,dim);
		if(!intsct && face1==face2) {
			cross=cross2;
			face1=(face1==PFfront)?PFback:PFfront;
			for(d=0;d<dim;d++) crsspt[d]=pt1[d]+cross*(pt2[d]-pt1[d]);
			intsct=Geo_PtInSlab(point[0],point[1],crsspt,dim); }}

	else if(pnl->ps==PSdisk) {												// disk
		if(face1==face2) return 0;
		cross=dist1/(dist1-dist2);
		for(d=0;d<dim;d++) crsspt[d]=pt1[d]+cross*(pt2[d]-pt1[d]);
		dot=0;
		for(d=0;d<dim;d++) dot+=(crsspt[d]-point[0][d])*(crsspt[d]-point[0][d]);
		intsct=(dot<=point[1][0]*point[1][0]); }

	else
		intsct=0;

	if(face1ptr) *face1ptr=face1;
	if(face2ptr) *face2ptr=face2;
	if(crossptr) *crossptr=cross;
	if(cross2ptr) *cross2ptr=cross2;
	return intsct; }


/* ptinpanel.  Determines if the point pt is inside the finite panel pnl,
returning 1 if so and 0 if not.  Here, inside only means that the point is
within the volume that is swept out perpendicular to the plane of the panel,
and says nothing about the position of the point relative to the plane of the
panel. */
int ptinpanel(double *pt,panelptr pnl,int dim) {
	surfaceptr srf;
	double **point,*front;
	double len2,dot;
	int intsct,d;

	srf=pnl->srf;
	point=pnl->point;
	front=pnl->front;

	if(pnl->ps==PSrect) {														// rectangle
		if(dim==1) intsct=1;
		else if(dim==2) {
			d=(int)front[2];
			intsct=((point[0][d]<=pt[d] && pt[d]<=point[1][d]) || (point[1][d]<=pt[d] && pt[d]<=point[0][d])); }
		else {
			d=(int)front[2];
			intsct=((point[0][d]<=pt[d] && pt[d]<=point[1][d]) || (point[1][d]<=pt[d] && pt[d]<=point[0][d]));
			d=(d+1)%3;
			if(d==(int)front[1]) d=(d+1)%3;
			intsct=intsct && ((point[1][d]<=pt[d] && pt[d]<=point[2][d]) || (point[2][d]<=pt[d] && pt[d]<=point[1][d])); }}
	else if(pnl->ps==PStri) {												// triangle
		if(dim==1) intsct=1;
		else if(dim==2)
			intsct=Geo_PtInSlab(point[0],point[1],pt,2);
		else
			intsct=Geo_PtInTriangle(point[0],point[1],point[2],front,pt); }
	else if(pnl->ps==PSsph || pnl->ps==PShemi) {		// sphere, hemisphere
		if(pnl->ps==PSsph) intsct=1;
		else {
			dot=0;
			for(d=0;d<dim;d++) dot+=(pt[d]-point[0][d])*point[2][d];
			intsct=(dot<=0); }}
	else if(pnl->ps==PScyl) {									// cylinder
		intsct=Geo_PtInSlab(point[0],point[1],pt,dim); }
	else if(pnl->ps==PSdisk) {												// disk
		len2=0;
		for(d=0;d<dim;d++) len2+=(pt[d]-point[0][d])*(pt[d]-point[0][d]);
		if(len2<=point[1][0]*point[1][0]) intsct=1;
		else {
			dot=0;
			for(d=0;d<dim;d++) dot+=(pt[d]-point[0][d])*front[d];
			len2-=dot*dot;
			intsct=(len2<=point[1][0]*point[1][0]); }}
	else
		intsct=0;

	return intsct; }


/* surfaction.  Returns the surface action that should happen to a molecule of
type ident and state ms that interacts with face face of surface srf.  If the
state of ident should be changed, then the new state is returned in ms2ptr, if 
ms2ptr is not NULL. */
enum SrfAction surfaction(surfaceptr srf,enum PanelFace face,int ident,enum MolecState ms,int *i2ptr,enum MolecState *ms2ptr) {
	enum SrfAction act;
	enum MolecState ms2,ms3;
	double r,*srfcumprob;
	int i2;

	i2=-1;
	ms2=ms;
	if(ms==MSsoln && face==PFback) ms=MSbsoln;
	act=srf->action[ident][ms];

	if(act==SAmult) {
		srfcumprob=srf->srfcumprob[ident][ms];
		r=randCOD();
		ms2=MSnone;
		for(ms3=(MolecState)0;ms3<MSMAX1 && ms2==MSnone;ms3=(MolecState)(ms3+1))
			if(r<srfcumprob[ms3]) ms2=ms3;
		i2=srf->srfnewspec[ident][ms][ms2];

		if(ms==MSsoln || ms==MSbsoln) {												// from solution
			if(ms==ms2) act=SAreflect;
			else if(ms2==MSsoln || ms2==MSbsoln) act=SAtrans;
			else act=SAadsorb; }
		else if(ms==ms2) act=SAno;														// surface to same surface state
		else if(ms2==MSsoln || ms2==MSbsoln) {								// surface to solution
			if(srf->srfprob[ident][ms2][ms]>0) act=SArevdes;
			else act=SAirrevdes; }
		else act=SAflip; }																		// surface to different surface state

	if(i2ptr) *i2ptr=i2;
	if(ms2ptr) *ms2ptr=ms2;
	return act; }


/* rxnXsurface.  Returns 1 if a potential bimolecular reaction between mptr1 and
mptr2 is across a non-transparent surface, and so cannot actually happen.
Returns 0 if a reaction is allowed.  Using the diffusion coefficients of the two
molecules, this calculates the reaction location and then determines which
molecules needs to diffuse across which surfaces to get to that location.  If
the molecules can diffuse across the necessary surfaces, then the reaction is
allowed, and not otherwise.  This routine does not allow reactions to occur
across jump surfaces.  Also, it does not look for jump paths that go from mptr1
to mptr2.  Surface-bound molecules that are in their ÒupÓ or ÒdownÓ state are
assumed to be accessible from both sides of the surface, whereas those that are
in the ÒfrontÓ or ÒbackÓ states are accessible from only one side. */
int rxnXsurface(simptr sim,moleculeptr mptr1,moleculeptr mptr2) {
	int dim,p,i1,i2,result;
	double *pos1,*pos2,dc1,dc2,rxnpt,crsspt[3],cross,cross2;
	boxptr bptr;
	panelptr pnl;
	enum PanelFace face1,face2,facein;
	enum MolecState ms1,ms2;

	if(!sim->srfss) return 0;
	dim=sim->dim;
	i1=mptr1->ident;
	i2=mptr2->ident;
	ms1=mptr1->mstate;
	ms2=mptr2->mstate;
	pos1=mptr1->pos;
	pos2=mptr2->pos;
	dc1=sim->mols->difc[i1][ms1];
	dc2=sim->mols->difc[i2][ms2];
	if(dc1==0 && dc2==0) dc1=dc2=1;
	rxnpt=dc1/(dc1+dc2);

	result=0;
	for(bptr=pos2box(sim,mptr1->pos);bptr && result==0;bptr=line2nextbox(sim,pos1,pos2,bptr)) {
		for(p=0;p<bptr->npanel && result==0;p++) {
			pnl=bptr->panel[p];
			if(lineXpanel(pos1,pos2,pnl,dim,crsspt,&face1,&face2,&cross,&cross2)) {
				if((ms1==MSup || ms1==MSdown) && pnl==mptr1->pnl) result=0;
				else if((ms2==MSup || ms2==MSdown) && pnl==mptr2->pnl) result=0;
				else if(face1!=face2) {					// opposite sides of a surface
					if(rxnpt<cross || (rxnpt==cross && face1==PFback)) result=!(surfaction(pnl->srf,face2,i2,ms2,NULL,NULL)==SAtrans);
					else result=!(surfaction(pnl->srf,face1,i1,ms1,NULL,NULL)==SAtrans); }
				else {											// across a sphere, cylinder, etc.
					facein=(face1==PFfront)?PFback:PFfront;
					if(rxnpt<cross || (rxnpt==cross && face1==PFback)) result=!(surfaction(pnl->srf,face2,i2,ms2,NULL,NULL)==SAtrans && surfaction(pnl->srf,facein,i2,ms2,NULL,NULL)==SAtrans);
					else if(rxnpt<cross2 || (rxnpt==cross2 && face1==PFfront)) result=!(surfaction(pnl->srf,face1,i1,ms1,NULL,NULL)==SAtrans && surfaction(pnl->srf,face2,i2,ms2,NULL,NULL)==SAtrans);
					else result=!(surfaction(pnl->srf,face1,i1,ms1,NULL,NULL)==SAtrans && surfaction(pnl->srf,facein,i1,ms1,NULL,NULL)==SAtrans); }}}}
	return result; }



/* fixpt2panel.  Fixes the point pt onto the face face of panel pnl.  Send in
face equal to PFnone if pt should be moved as close as possible to pnl.  If it
should also be on the front or back face of the panel, as determined by
panelside, then send in face equal to PFfront or PFback, respectively.  Before
moving, if pt is already on the proper face and its distance is less than or
equal to epsilon, it is not moved; setting epsilon to 0 ensures moving.  This
function first moves pt to the panel in a direction normal to the local panel
surface and then nudges pt as required to get it to the proper side.  This only
considers the infinite plane of the panel, while ignoring its boundaries
(similarly, hemispheres are considered to be identical to spheres and cylinders
are considered to be infinitely long). */
void fixpt2panel(double *pt,panelptr pnl,int dim,enum PanelFace face,double epsilon) {
	int d,sign;
	double **point,*front,norm[3],dist,factor,dist2;
	enum PanelFace faceat;

	point=pnl->point;
	front=pnl->front;

	faceat=panelside(pt,pnl,dim,&dist);
	if((faceat==face || face==PFnone) && fabs(dist)<=epsilon) return;

	if(pnl->ps==PSrect) {
		for(d=0;d<dim;d++) norm[d]=0;
		norm[(int)front[1]]=front[0]; }
	else if(pnl->ps==PStri || pnl->ps==PSdisk) {
		for(d=0;d<dim;d++) norm[d]=front[d]; }
	else if(pnl->ps==PSsph || pnl->ps==PShemi) {
		Geo_SphereNormal(point[0],pt,(int)front[0],dim,norm); }
	else if(pnl->ps==PScyl) {
		if(dim==2) {
			dist2=0;
			for(d=0;d<dim;d++) dist2+=(pt[d]-point[0][d])*front[d];
			sign=((dist2>0 && front[2]==1) || (dist2<0 && front[2]==-1))?1:-1;
			norm[0]=sign*front[0];
			norm[1]=sign*front[1]; }
		else if(dim==3) {
			Geo_LineNormal3D(point[0],point[1],pt,norm);
			if(front[2]==-1)
				for(d=0;d<dim;d++) norm[d]*=-1; }}
	else {
		for(d=0;d<dim;d++) norm[d]=0;
		norm[0]=1; }

	for(d=0;d<dim;d++) pt[d]-=dist*norm[d];
	if(face==PFnone || face==PFboth) return;
	sign=(face==PFfront)?1:-1;
	for(factor=1.0;face!=panelside(pt,pnl,dim,NULL);factor*=2.0) {
		for(d=0;d<dim;d++) pt[d]+=sign*factor*DBL_EPSILON*norm[d]; }
	return; }


/* movept2panel.  This moves the point pt so that it is over the panel pnl.
This means that pt is not moved into the plane of the panel, which is done by
fixpt2panel, but is moved parallel to the plane of the panel. */
void movept2panel(double *pt,panelptr pnl,int dim) {
	double **point,*front;
	double lo,hi,dot;
	int d;

	point=pnl->point;
	front=pnl->front;

	if(pnl->ps==PSrect) {														// rectangle
		if(dim==1);
		else if(dim==2) {
			d=(int)front[2];
			lo=(point[0][d]<point[1][d])?point[0][d]:point[1][d];
			hi=(point[0][d]<point[1][d])?point[1][d]:point[0][d];
			if(pt[d]<lo) pt[d]=lo;
			else if(pt[d]>hi) pt[d]=hi; }
		else {
			d=(int)front[2];
			lo=(point[0][d]<point[1][d])?point[0][d]:point[1][d];
			hi=(point[0][d]<point[1][d])?point[1][d]:point[0][d];
			if(pt[d]<lo) pt[d]=lo;
			else if(pt[d]>hi) pt[d]=hi;
			d=(d+1)%3;
			if(d==(int)front[1]) d=(d+1)%3;
			lo=(point[0][d]<point[3][d])?point[0][d]:point[3][d];
			hi=(point[0][d]<point[3][d])?point[3][d]:point[0][d];
			if(pt[d]<lo) pt[d]=lo;
			else if(pt[d]>hi) pt[d]=hi; }}
	else if(pnl->ps==PStri) {												// triangle
		if(dim==1);
		else if(dim==2) Geo_NearestSlabPt(point[0],point[1],pt,pt,dim);
		else Geo_NearestTriPt(point[0],point[1],point[2],front,pt,pt); }
	else if(pnl->ps==PSsph || pnl->ps==PShemi) {		// sphere, hemisphere
		if(pnl->ps==PSsph);
		else {
			dot=0;
			for(d=0;d<dim;d++) dot+=(pt[d]-point[0][d])*point[2][d];
			if(dot>0)
				for(d=0;d<dim;d++) pt[d]-=dot*point[2][d]; }}
	else if(pnl->ps==PScyl)													// cylinder
		Geo_NearestSlabPt(point[0],point[1],pt,pt,dim);
	else if(pnl->ps==PSdisk)											// disk
		Geo_NearestCylPt(point[0],front,point[1][0],dim,pt,pt);

	return; }


/* closestpanelpt.  Finds the closest point that is on panel pnl to the test
point testpt and returns it in pnlpt.  This also returns the distance between
testpt and pnlpt, which is always positive. */
double closestpanelpt(panelptr pnl,int dim,double *testpt,double *pnlpt) {
	double dist,**point,*front;
	double lo,hi,dot,x,y;
	int d;

	point=pnl->point;
	front=pnl->front;
	dist=0;

	if(pnl->ps==PSrect) {														// rectangle
		if(dim==1) {
			pnlpt[0]=point[0][0];
			dist=fabs(testpt[0]-pnlpt[0]); }
		else if(dim==2) {
			d=(int)front[1];
			pnlpt[d]=point[0][d];
			d=(int)front[2];
			lo=(point[0][d]<point[1][d])?point[0][d]:point[1][d];
			hi=(point[0][d]<point[1][d])?point[1][d]:point[0][d];
			if(testpt[d]<lo) pnlpt[d]=lo;
			else if(testpt[d]>hi) pnlpt[d]=hi;
			else pnlpt[d]=testpt[d];
			dist=sqrt((testpt[0]-pnlpt[0])*(testpt[0]-pnlpt[0])+(testpt[1]-pnlpt[1])*(testpt[1]-pnlpt[1])); }
		else {
			d=(int)front[1];
			pnlpt[d]=point[0][d];
			d=(d+1)%3;
			lo=(point[0][d]<point[1][d])?point[0][d]:point[1][d];
			hi=(point[0][d]<point[1][d])?point[1][d]:point[0][d];
			if(testpt[d]<lo) pnlpt[d]=lo;
			else if(testpt[d]>hi) pnlpt[d]=hi;
			else pnlpt[d]=testpt[d];
			d=(d+1)%3;
			lo=(point[0][d]<point[3][d])?point[0][d]:point[3][d];
			hi=(point[0][d]<point[3][d])?point[3][d]:point[0][d];
			if(testpt[d]<lo) pnlpt[d]=lo;
			else if(testpt[d]>hi) pnlpt[d]=hi;
			else pnlpt[d]=testpt[d];
			dist=sqrt((testpt[0]-pnlpt[0])*(testpt[0]-pnlpt[0])+(testpt[1]-pnlpt[1])*(testpt[1]-pnlpt[1])+(testpt[2]-pnlpt[2])*(testpt[2]-pnlpt[2])); }}
	else if(pnl->ps==PStri) {												// triangle
		if(dim==1) {
			pnlpt[0]=point[0][0];
			dist=fabs(testpt[0]-pnlpt[0]); }
		else if(dim==2) {
			Geo_NearestLineSegPt(point[0],point[1],testpt,pnlpt,dim);
			dist=sqrt((testpt[0]-pnlpt[0])*(testpt[0]-pnlpt[0])+(testpt[1]-pnlpt[1])*(testpt[1]-pnlpt[1])); }
		else {
			Geo_NearestTrianglePt(point[0],point[1],point[2],front,testpt,pnlpt);
			dist=sqrt((testpt[0]-pnlpt[0])*(testpt[0]-pnlpt[0])+(testpt[1]-pnlpt[1])*(testpt[1]-pnlpt[1])+(testpt[2]-pnlpt[2])*(testpt[2]-pnlpt[2])); }}
	else if(pnl->ps==PSsph) {												// sphere
		if(dim==1) {
			if(testpt[0]>point[0][0])
				pnlpt[0]=point[0][0]+point[1][0];
			else
				pnlpt[0]=point[0][0]-point[1][0];
			dist=fabs(testpt[0]-pnlpt[0]); }
		else {
			dist=Geo_NearestSpherePt(point[0],point[1][0],(int)front[0],dim,testpt,pnlpt);
			dist=fabs(dist); }}
	else if(pnl->ps==PShemi) {											// hemisphere
		dot=0;
		for(d=0;d<dim;d++) dot+=(testpt[d]-point[0][d])*point[2][d];
		if(dot<0) {
			dist=Geo_NearestSpherePt(point[0],point[1][0],(int)front[0],dim,testpt,pnlpt);
			dist=fabs(dist); }
		else if(dim==2) {
			x=point[2][1];
			y=-point[2][0];
			dot=(testpt[0]-point[0][0])*x+(testpt[1]-point[0][1])*y;
			dot=(dot>0)?1:-1;
			pnlpt[0]=point[0][0]+dot*point[1][0]*x;
			pnlpt[1]=point[0][1]+dot*point[1][0]*y;
			dist=sqrt((testpt[0]-pnlpt[0])*(testpt[0]-pnlpt[0])+(testpt[1]-pnlpt[1])*(testpt[1]-pnlpt[1])); }
		else {
			Geo_NearestRingPt(point[0],point[2],point[1][0],3,testpt,pnlpt);
			dist=sqrt((testpt[0]-pnlpt[0])*(testpt[0]-pnlpt[0])+(testpt[1]-pnlpt[1])*(testpt[1]-pnlpt[1])+(testpt[2]-pnlpt[2])*(testpt[2]-pnlpt[2])); }}
	else if(pnl->ps==PScyl) {												// cylinder
		if(dim==2) {
			Geo_NearestLineSegPt(point[0],point[1],testpt,pnlpt,2);
			dot=(testpt[0]-point[0][0])*front[0]+(testpt[1]-point[0][1])*front[1];
			dot=(dot>0)?1:-1;
			pnlpt[0]+=dot*point[2][0]*front[0];
			pnlpt[1]+=dot*point[2][0]*front[1];
			dist=sqrt((testpt[0]-pnlpt[0])*(testpt[0]-pnlpt[0])+(testpt[1]-pnlpt[1])*(testpt[1]-pnlpt[1])); }
		else {
			Geo_NearestCylinderPt(point[0],point[1],point[2][0],3,testpt,pnlpt);
			dist=sqrt((testpt[0]-pnlpt[0])*(testpt[0]-pnlpt[0])+(testpt[1]-pnlpt[1])*(testpt[1]-pnlpt[1])+(testpt[2]-pnlpt[2])*(testpt[2]-pnlpt[2])); }}
	else if(pnl->ps==PSdisk) {										// disk
		Geo_NearestDiskPt(point[0],front,point[1][0],dim,testpt,pnlpt);
		if(dim==2)
			dist=sqrt((testpt[0]-pnlpt[0])*(testpt[0]-pnlpt[0])+(testpt[1]-pnlpt[1])*(testpt[1]-pnlpt[1]));
		else
			dist=sqrt((testpt[0]-pnlpt[0])*(testpt[0]-pnlpt[0])+(testpt[1]-pnlpt[1])*(testpt[1]-pnlpt[1])+(testpt[2]-pnlpt[2])*(testpt[2]-pnlpt[2])); }
	return dist; }


/* movemol2closepanel.  Checks to see if mol is within the area of the panel pnl
(i.e. over or under the panel, ignoring the position relative to the plane of
the panel).  If it isnÕt, this sees if mol is over a neighboring panel and if
so, this puts pt on the neighboring panel, and fixes it to the correct face of
the new panel. */
void movemol2closepanel(simptr sim,moleculeptr mptr,int dim,double epsilon,double neighdist) {
	int nn,p,jump;
	double dist,pt[DIMMAX],pnledgept[DIMMAX];
	panelptr pnl,newpnl;
	enum PanelFace face;
	enum MolecState ms;

	ms=mptr->mstate;
	if(ms==MSfront) face=PFfront;
	else if(ms==MSback) face=PFback;
	else face=PFnone;

	pnl=mptr->pnl;
	if(!ptinpanel(mptr->pos,pnl,dim)) {
		nn=0;
		newpnl=NULL;
		closestpanelpt(pnl,dim,mptr->pos,pnledgept);
		for(p=0;p<pnl->nneigh;p++) {
			dist=closestpanelpt(pnl->neigh[p],dim,pnledgept,pt);
			if(dist<neighdist && coinrandD(1.0/++nn)) newpnl=pnl->neigh[p]; }
		if(nn) {
			mptr->pnl=newpnl;
			if(face!=PFnone && newpnl->srf->action[mptr->ident][face==PFfront?MSsoln:MSbsoln]==SAjump) {	// jump, if required
				jump=surfacejump(mptr,newpnl,mptr->pos,face,dim);
				if(jump) {
					mptr->pnl=newpnl->jumpp[face];
					if(newpnl->jumpf[face]!=face) {
						face=newpnl->jumpf[face];
						molchangeident(sim,mptr,mptr->list,-1,mptr->ident,face==PFfront?MSfront:MSback,mptr->pnl); }}}}
		movept2panel(mptr->pos,mptr->pnl,dim); }

	fixpt2panel(mptr->pos,mptr->pnl,dim,face,epsilon);
	return; }


/* surfacereflect.  This bounces the molecule mptr off of the face side of panel
pnl.  Elastic collisions are performed, which should work properly for any shape
panel and any dimensionality.  For flat panels, elastic collisions also apply to
Brownian motion.  It is assumed that the molecule travels from some point (not
given to this function, and irrelevent) to mptr->pos, via a collision with the
panel at location crsspt, where crsspt is either exactly at the panel or is
slightly on impact side of the panel.  The molecule pos element is set to the
new, reflected, position, which will always be on the face side of the panel. */
void surfacereflect(moleculeptr mptr,panelptr pnl,double *crsspt,int dim) {
	int d,axis;
	double *pos,*front,norm[3],dot;

	pos=mptr->pos;
	front=pnl->front;

	if(pnl->ps==PSrect) {
		axis=(int)front[1];
		pos[axis]-=2.0*(pos[axis]-crsspt[axis]); }
	else if(pnl->ps==PStri || pnl->ps==PSdisk) {
		dot=0;
		for(d=0;d<dim;d++) dot+=(pos[d]-crsspt[d])*front[d];
		for(d=0;d<dim;d++) pos[d]-=2.0*front[d]*dot; }
	else if(pnl->ps==PSsph || pnl->ps==PShemi) {
		Geo_SphereNormal(pnl->point[0],crsspt,1,dim,norm);
		dot=0;
		for(d=0;d<dim;d++) dot+=(pos[d]-crsspt[d])*norm[d];
		for(d=0;d<dim;d++) pos[d]-=2.0*norm[d]*dot; }
	else if(pnl->ps==PScyl) {
		if(dim==2) {
			dot=0;
			for(d=0;d<dim;d++) dot+=(pos[d]-crsspt[d])*front[d];
			for(d=0;d<dim;d++) pos[d]-=2.0*front[d]*dot; }
		else {
			Geo_LineNormal3D(pnl->point[0],pnl->point[1],crsspt,norm);
			dot=0;
			for(d=0;d<dim;d++) dot+=(pos[d]-crsspt[d])*norm[d];
			for(d=0;d<dim;d++) pos[d]-=2.0*norm[d]*dot; }}

	return; }


/* Surfacejump.  This performs a jump for molecule mptr that hit panel pnl on
face face.  The contact location is input in crsspt, which needs to be very
close to the panel but does not have to be on the proper side.  This looks up
the jump destination and translates both the crsspt value and the molecule
position in mptr->pos to represent this jump.  On return, crsspt is on the
destination face of the destination panel.  The molecule pos element will always
be returned on the destination face side of the destination panel.  For the most
part, this function only allows jumps between panels with the same shape, the
same dimensions, and the same orientation.  Exceptions are that sphere,
hemisphere, and cylinder radii are allowed to differ between origin and
destination panels. */
int surfacejump(moleculeptr mptr,panelptr pnl,double *crsspt,enum PanelFace face,int dim) {
	double **point,**point2,*front,*front2,dot,cent[3],delta[3];
	panelptr pnl2;
	enum PanelFace face2;
	int d,dir,ps;

	pnl2=pnl->jumpp[face];
	face2=pnl->jumpf[face];
	if(!pnl2) return 0;
	if(!(face2==PFfront || face2==PFback)) return 0;

	point=pnl->point;
	front=pnl->front;
	ps=pnl->ps;
	point2=pnl2->point;
	front2=pnl2->front;

	if(ps==PSrect) {
		Geo_RectCenter(point,cent,dim);
		Geo_RectCenter(point2,delta,dim);
		for(d=0;d<dim;d++) delta[d]-=cent[d];
		dir=(front[0]==front2[0])?1:-1; }
	else if(ps==PStri) {
		Geo_TriCenter(point,cent,dim);
		Geo_TriCenter(point2,delta,dim);
		for(d=0;d<dim;d++) delta[d]-=cent[d];
		dot=0;
		for(d=0;d<dim;d++) dot+=front[d]*front2[d];
		dir=dot>0?1:-1; }
	else if(ps==PSsph || ps==PShemi) {
		for(d=0;d<dim;d++)
			delta[d]=(crsspt[d]-point[0][d])*point2[1][0]/point[1][0]+point2[0][d]-crsspt[d];
		dir=(front[0]==front2[0])?1:-1; }
	else if(ps==PScyl) {
		for(d=0;d<dim;d++)
			delta[d]=(crsspt[d]-point[0][d])*point2[2][0]/point[2][0]+point2[0][d]-crsspt[d];
		dir=(front[2]==front2[2])?1:-1; }
	else if(ps==PSdisk) {
		for(d=0;d<dim;d++) delta[d]=point2[0][d]-point[0][d];
		dot=0;
		for(d=0;d<dim;d++) dot+=front[d]*front2[d];
		dir=dot>0?1:-1; }
	else {
		for(d=0;d<dim;d++) delta[d]=0;
		dir=1; }

	for(d=0;d<dim;d++) {
		crsspt[d]+=delta[d];
		mptr->pos[d]+=delta[d];
		mptr->posoffset[d]-=delta[d]; }
	fixpt2panel(crsspt,pnl2,dim,face2,0);
	dir*=(face!=face2)?1:-1;
	if(dir==-1) surfacereflect(mptr,pnl2,crsspt,dim);
	return 1; }


/* dosurfinteract.  Performs interaction between molecule and surface for an
interaction that is known to have happened.  On return, crsspt will be on the
same side of the surface as the molecule.  Returns 1 if the molecule does not
need additional trajectory tracking (e.g. it's absorbed) and 0 if it might need
additional tracking (e.g. it's reflected).  This function does not consider
opposite-face actions.  For example, if the front of a surface is transparent
and the back is absorbing, an impact on the front will result in the molecule
being transmitted to the far side, and not being absorbed. */
int dosurfinteract(simptr sim,moleculeptr mptr,int ll,int m,panelptr pnl,enum PanelFace face,double *crsspt,double epsilon) {
	int done,dim,d,i2;
	enum PanelFace newface;
	enum MolecState ms,ms2;
	enum SrfAction act;
	double x,norm[DIMMAX];

	dim=sim->dim;
	done=0;
	ms=mptr->mstate;
																								// find action first
	if((face==PFfront || face==PFback) && pnl->emitterabsorb[face] && pnl->emitterabsorb[face][mptr->ident]>0) {
		if(randCCD()<pnl->emitterabsorb[face][mptr->ident]) act=SAabsorb;
		else act=SAreflect;
		i2=-1;
		ms2=MSsoln; }
	else
		act=surfaction(pnl->srf,face,mptr->ident,ms,&i2,&ms2);

	if(act==SAno);																// no action
	else if(act==SAtrans) {												// transmit
		newface=face==PFfront?PFback:PFfront;
		fixpt2panel(crsspt,pnl,dim,newface,epsilon);
		if(i2!=-1) molchangeident(sim,mptr,ll,m,i2,MSsoln,NULL); }
	else if(act==SAreflect) {											// reflect
		surfacereflect(mptr,pnl,crsspt,dim);
		fixpt2panel(crsspt,pnl,dim,face,epsilon);
		if(panelside(mptr->pos,pnl,dim,NULL)!=face) fixpt2panel(mptr->pos,pnl,dim,face,0);
		if(i2!=-1) molchangeident(sim,mptr,ll,m,i2,MSsoln,NULL); }
	else if(act==SAabsorb) {											// absorb
		molkill(sim,mptr,ll,m);
		done=1; }
	else if(act==SAjump)													// jump (just solution-phase molecules)
		surfacejump(mptr,pnl,crsspt,face,dim);
	else if(act==SAport) {												// port
		mptr->list=pnl->srf->port[face]->llport;
		if(m<sim->mols->sortl[ll]) sim->mols->sortl[ll]=m;
		done=1; }
	else if(act==SAadsorb) {											// adsorb
		molchangeident(sim,mptr,ll,m,i2==-1?mptr->ident:i2,ms2,pnl);
		for(d=0;d<dim;d++) mptr->pos[d]=crsspt[d];
		if(!ptinpanel(mptr->pos,mptr->pnl,dim))
			movept2panel(mptr->pos,mptr->pnl,dim);
		if(ms2==MSfront) fixpt2panel(mptr->pos,mptr->pnl,dim,PFfront,epsilon);
		else if(ms2==MSback) fixpt2panel(mptr->pos,mptr->pnl,dim,PFback,epsilon);
		done=1; }
	else if(act==SArevdes) {											// reversible desorb
		molchangeident(sim,mptr,ll,m,i2==-1?mptr->ident:i2,ms2,pnl);
		for(d=0;d<dim;d++) crsspt[d]=mptr->pos[d];
		fixpt2panel(crsspt,pnl,dim,ms2==MSsoln?PFfront:PFback,epsilon);
		x=desorbdist(sim->mols->difstep[mptr->ident][MSsoln],SPArevAds);
		panelnormal(pnl,mptr->pos,ms2==MSsoln?PFfront:PFback,dim,norm);
		for(d=0;d<dim;d++) mptr->pos[d]+=x*norm[d]; }
	else if(act==SAirrevdes) {										// irreversible desorb
		molchangeident(sim,mptr,ll,m,i2==-1?mptr->ident:i2,ms2,pnl);
		for(d=0;d<dim;d++) crsspt[d]=mptr->pos[d];
		fixpt2panel(crsspt,pnl,dim,ms2==MSsoln?PFfront:PFback,epsilon);
		x=desorbdist(sim->mols->difstep[mptr->ident][MSsoln],SPAirrDes);
		panelnormal(pnl,mptr->pos,ms2==MSsoln?PFfront:PFback,dim,norm);
		for(d=0;d<dim;d++) mptr->pos[d]+=x*norm[d]; }
	else if(act==SAflip) {												// on-surface flipping
		molchangeident(sim,mptr,ll,m,i2==-1?mptr->ident:i2,ms2,pnl);
		if(ms2==MSfront) fixpt2panel(mptr->pos,mptr->pnl,dim,PFfront,epsilon);
		else if(ms2==MSback) fixpt2panel(mptr->pos,mptr->pnl,dim,PFback,epsilon);
		done=1; }

	return done; }


/* checksurfaces_unitary. */
int checksurfaces_unitary(simptr sim,int ll,int reborn) {
	int dim,d,nmol,m,done,p,lxp,it;
	boxptr bptr1;
	moleculeptr *mlist,mptr;
	double crossmin,crssptmin[3],crsspt[3],cross,*via,*pos,epsilon;
	enum PanelFace face,facemin;
	panelptr pnl,pnlmin;

	if(!sim->srfss) return 0;
	if(!sim->mols) return 0;
	dim=sim->dim;
	epsilon=sim->srfss->epsilon;
	nmol=sim->mols->nl[ll];
	mlist=sim->mols->live[ll];

	if(!reborn) m=0;
	else m=sim->mols->topl[ll];

	for(;m<nmol;m++) {
		mptr=mlist[m];
		if(mptr->mstate!=MSsoln)									// move surface-bound molecules to closest panel
			movemol2closepanel(sim,mptr,dim,epsilon,sim->srfss->neighdist);
		else {																		// deal with solution-phase molecule-surface collisions
			via=mptr->via;
			for(d=0;d<dim;d++) via[d]=mptr->posx[d];
			pos=mptr->pos;
			done=0;
			it=0;
			while(!done) {
				if(++it>50) {
					for(d=0;d<dim;d++) pos[d]=mptr->posx[d];
					//fprintf(stderr,"SMOLDYN ERROR: surface calculation failure after 50 iterations\n");
					break; }
				crossmin=2;
				facemin=PFfront;
				pnlmin=NULL;
				for(bptr1=pos2box(sim,via);bptr1;bptr1=line2nextbox(sim,via,pos,bptr1)) {
					for(p=0;p<bptr1->npanel;p++) {
						pnl=bptr1->panel[p];
						lxp=lineXpanel(via,pos,pnl,dim,crsspt,&face,NULL,&cross,NULL);
						if(lxp && cross<=crossmin) {
							crossmin=cross;
							pnlmin=pnl;
							for(d=0;d<dim;d++) crssptmin[d]=crsspt[d];
							facemin=face; }}}
				if(crossmin<2) {											// a panel was crossed, so deal with it
					done=dosurfinteract(sim,mptr,ll,m,pnlmin,facemin,crssptmin,epsilon);
					for(d=0;d<dim;d++) via[d]=crssptmin[d];
					sim->eventcount[ETsurf]++; }
				else																	// nothing was crossed
					done=1; }}}
	return 0; }


void* check_surfaces_on_subset_mols(void* data) {//???????????? new function
//	int dim,d,nmol,m,done,p,lxp,it;
	int dim,d,m,done,p,lxp,it;
	boxptr bptr1;
	moleculeptr *mlist,mptr;
	double crossmin,crssptmin[3],crsspt[3],cross,*via,*pos,epsilon;
	enum PanelFace face,facemin;
	panelptr pnl,pnlmin;

	PARAMS_check_surfaces_on_subset_mols* theParams = (PARAMS_check_surfaces_on_subset_mols*) data;

	simptr sim = theParams->sim;
	int ll = theParams->ll;
	int first_index = theParams->first_ndx;
	int second_index = theParams->second_ndx;
	dim = sim->dim;

	mlist = sim->mols->live[ll];
	epsilon=sim->srfss->epsilon;

	for(m = first_index; m < second_index; m++) {
		mptr=mlist[m];
		if(mptr->mstate!=MSsoln)
			movemol2closepanel(sim,mptr,dim,epsilon,sim->srfss->neighdist);
	else {
		via=mptr->via;
		for(d=0;d<dim;d++) via[d]=mptr->posx[d];
		pos=mptr->pos;
		done=0;
		it=0;
		while(!done) {
			if(++it>50) {
				for(d=0;d<dim;d++) pos[d]=mptr->posx[d];
				//fprintf(stderr,"SMOLDYN ERROR: surface calculation failure after 50 iterations\n");
				break; }
			crossmin=2;
			facemin=PFfront;
			pnlmin=NULL;
			for(bptr1=pos2box(sim,via);bptr1;bptr1=line2nextbox(sim,via,pos,bptr1)) {
				for(p=0;p<bptr1->npanel;p++) {
					pnl=bptr1->panel[p];
					lxp=lineXpanel(via,pos,pnl,dim,crsspt,&face,NULL,&cross,NULL);
					if(lxp && cross<=crossmin) {
						crossmin=cross;
						pnlmin=pnl;
						for(d=0;d<dim;d++) crssptmin[d]=crsspt[d];
						facemin=face; }}}
			if(crossmin<2) {
				done=dosurfinteract(sim,mptr,ll,m,pnlmin,facemin,crssptmin,epsilon);
				for(d=0;d<dim;d++) via[d]=crssptmin[d];
				sim->eventcount[ETsurf]++; }
			else
				done=1; }}}

    return NULL; }



int checksurfaces_threaded( simptr sim, int ll, int reborn) { //????????? new function
#ifndef THREADING
	return 2;
#else

	int dim,nmol;
	moleculeptr *mlist;
	double epsilon;
	stack* current_thread_input_stack;

	if(!sim->srfss) return 0;
	if(!sim->mols) return 0;

	dim=sim->dim;
	epsilon=sim->srfss->epsilon;
	nmol=sim->mols->nl[ll];
	mlist=sim->mols->live[ll];

	int nthreads = getnumberofthreads(sim);

	PARAMS_check_surfaces_on_subset_mols theParams;
	theParams.sim = sim;
	theParams.ll = ll;

	int first_ndx = 0;
	int final_ndx = nmol;

	if(!reborn) first_ndx = 0;
	else first_ndx = sim->mols->topl[ll];

	int total_num_to_process = final_ndx - first_ndx;
	if (total_num_to_process < nthreads) return checksurfaces_unitary( sim, ll, reborn);

	int stride = (total_num_to_process + (nthreads - (total_num_to_process % nthreads))) / nthreads;  // This equals ceil( total_num_to_process / nthreads).

	int thread_ndx;
	for( thread_ndx = 0; thread_ndx != nthreads - 1; ++thread_ndx) {
		clearthreaddata( sim->threads->thread[thread_ndx] );
		current_thread_input_stack = sim->threads->thread[thread_ndx]->input_stack;

		theParams.first_ndx = first_ndx;
		theParams.second_ndx = first_ndx += stride;
		theParams.output_stack = sim->threads->thread[ thread_ndx ]->output_stack;

		push_data_onto_stack( current_thread_input_stack, &theParams, sizeof(theParams)); // this copies over the inputParams data, so the fact that it is used to seed multiple threads is no problem.
		pthread_create((pthread_t*)sim->threads->thread[thread_ndx]->thread_id, NULL, check_surfaces_on_subset_mols, (void*) current_thread_input_stack->stack_data); }
	{
	clearthreaddata( sim->threads->thread[nthreads - 1]);
	current_thread_input_stack = sim->threads->thread[nthreads-1]->input_stack;
	theParams.first_ndx = first_ndx;
	theParams.second_ndx = nmol;
	theParams.output_stack = sim->threads->thread[nthreads-1]->output_stack;

	push_data_onto_stack( current_thread_input_stack, &theParams, sizeof(theParams)); // this copies over the inputParams data, so the fact that it is used to seed multiple threads is no problem.
	pthread_create((pthread_t*)sim->threads->thread[thread_ndx]->thread_id, NULL, check_surfaces_on_subset_mols, (void*) current_thread_input_stack->stack_data);
	}

	for( thread_ndx = 0; thread_ndx != nthreads; ++thread_ndx) {
		pthread_join( *((pthread_t*) sim->threads->thread[thread_ndx]->thread_id), NULL); }

    return 0;
#endif
}



/* checksurfaces. This is the new threading version I'm working on.*/
void* checksurfaces_threaded_helper(void* voidData) {//???????? new function
	ptrsurfacefuncdata data = (ptrsurfacefuncdata) voidData;
	simptr sim = data->sim;
	int ll = data->live_list_ndx;
	int reborn = data->resurrect_only;

	int dim,d,nmol,m,done,p,lxp,it;
	boxptr bptr1;
	moleculeptr *mlist,mptr;
	double crossmin,crssptmin[3],crsspt[3],cross,*via,*pos,epsilon;
	enum PanelFace face,facemin;
	panelptr pnl,pnlmin;

	if(!sim->srfss) return NULL;
	if(!sim->mols) return NULL;

	dim = sim->dim;
	epsilon = sim->srfss->epsilon;
	nmol = sim->mols->nl[ll];
	mlist = sim->mols->live[ll];

	if(! reborn )
		m=0;
	else
		m=sim->mols->topl[ll];

	for(; m != nmol; m++) {
		mptr = mlist[m];
		if(mptr->mstate!=MSsoln)
		movemol2closepanel(sim,mptr,dim,epsilon,sim->srfss->neighdist);
		else {
			via=mptr->via;
			for(d=0;d<dim;d++) {
				via[d]=mptr->posx[d]; }
				pos = mptr->pos;
				done = 0;
				it = 0;
				while(!done) {
					if(++it>50) {
						for(d=0;d<dim;d++) {
							pos[d]=mptr->posx[d]; }
					break; }

					crossmin = 2;
					facemin = PFfront;
					pnlmin = NULL;

					for(bptr1 = pos2box(sim,via); bptr1; bptr1 = line2nextbox(sim,via,pos,bptr1)) {
						for(p=0;p<bptr1->npanel;p++) {
							pnl=bptr1->panel[p];
							lxp=lineXpanel(via,pos,pnl,dim,crsspt,&face,NULL,&cross,NULL);
							if(lxp && cross<=crossmin) {
								crossmin=cross;
								pnlmin=pnl;
								for(d=0;d<dim;d++) {
									crssptmin[d]=crsspt[d]; }
								facemin=face; }}}

					if(crossmin<2) {
						done = dosurfinteract(sim,mptr,ll,m,pnlmin,facemin,crssptmin,epsilon);
						for(d = 0; d<dim; d++) {
							via[d]=crssptmin[d]; }

						++sim->eventcount[ETsurf]; }

					else {
						done=1; }}}}
return NULL; 
}//???????????? end of new code


/* checksurfacebound. */
int checksurfacebound(simptr sim,int ll) {
	int nmol,m;
	moleculeptr mptr,*mlist;

	if(!sim->srfss) return 0;
	if(!sim->mols) return 0;

	nmol=sim->mols->nl[ll];
	mlist=sim->mols->live[ll];
	for(m=0;m<nmol;m++) {
		mptr=mlist[m];
		if(mptr->mstate!=MSsoln)
			dosurfinteract(sim,mptr,ll,m,mptr->pnl,PFnone,mptr->posx,sim->srfss->epsilon); }
	return 0; }


