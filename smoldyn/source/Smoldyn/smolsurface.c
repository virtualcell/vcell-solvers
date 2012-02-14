/* Steven Andrews, started 10/22/2001.
 This is a library of functions for the Smoldyn program.  See documentation
 called Smoldyn_doc1.pdf and Smoldyn_doc2.pdf.
 Copyright 2003-2011 by Steven Andrews.  This work is distributed under the terms
 of the Gnu General Public License (GPL). */

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
#include <sstream>

#include "smoldyn_config.h"

#ifdef THREADING
#include <pthread.h>
#endif

#define CHECK(A) if(!(A)) {printfException("Unknown solver error.");goto failure;} else (void)0
#define CHECKS(A,B) if(!(A)) {strncpy(erstr,B,STRCHAR-1);erstr[STRCHAR-1]='\0'; printfException("%s", B); goto failure;} else (void)0


/******************************************************************************/
/********************************** Surfaces **********************************/
/******************************************************************************/


/******************************************************************************/
/****************************** enumerated types ******************************/
/******************************************************************************/


/* surfstring2face */
enum PanelFace surfstring2face(char *string) {
	enum PanelFace ans;

	if(strbegin(string,"front",0)) ans=PFfront;
	else if(strbegin(string,"back",0)) ans=PFback;
	else if(strbegin(string,"all",0) || strbegin(string,"both",0)) ans=PFboth;
	else ans=PFnone;
	return ans; }


/* surfface2string */
char *surfface2string(enum PanelFace face,char *string) {
	if(face==PFfront) strcpy(string,"front");
	else if(face==PFback) strcpy(string,"back");
	else if(face==PFboth) strcpy(string,"both");
	else strcpy(string,"none");
	return string; }


/* surfstring2act */
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
	else if(strbegin(string,"adsorb",0)) ans=SAadsorb;
	else if(strbegin(string,"revdes",0)) ans=SArevdes;
	else if(strbegin(string,"irrevdes",0)) ans=SAirrevdes;
	else if(strbegin(string,"flip",0)) ans=SAflip;
	else ans=SAnone;
	return ans; }


/* surfact2string */
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


/* surfstring2ps */
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


/* surfps2string */
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


/* surfstring2dm */
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


/* surfdm2string */
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


/* panelpoints */
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


/* surfpanelparams */
int surfpanelparams(enum PanelShape ps,int dim) {
	int n;

	if(ps==PSrect) n=2*dim-1;
	else if(ps==PStri) n=dim*dim;
	else if(ps==PSsph) n=2*dim;
	else if(ps==PScyl && dim>1) n=(dim==2)?5:9;
	else if(ps==PShemi && dim>1) n=3*dim;
	else if(ps==PSdisk && dim>1) n=(dim==2)?5:8;
	else n=0;
	return n; }


/* panelmiddle */
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


/* panelarea */
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


/* surfacearea */
double surfacearea(surfaceptr srf,int dim,int *totpanelptr) {
	int ps,p,totpanel;
	double area;

	totpanel=0;
	area=0;
	for(ps=(PanelShape)0;ps<PSMAX;ps=(PanelShape)(ps+1))
		for(p=0;p<srf->npanel[ps];p++) {
			totpanel++;
			area+=panelarea(srf->panels[ps][p],dim); }
	if(totpanelptr) *totpanelptr=totpanel;
	return area; }


/* surfacearea2 */
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


/* panelrandpos */
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


/* surfrandpos */
panelptr surfrandpos(surfaceptr srf,double *pos,int dim) {
	panelptr pnl;

	if(!srf->totpanel) return NULL;
	pnl=srf->paneltable[intrandpD(srf->totpanel,srf->areatable)];
	panelrandpos(pnl,pos,dim);
	return pnl; }


/* issurfprod */
int issurfprod(simptr sim,int i,enum MolecState ms) {
	surfacessptr srfss;
	int s,i1;
	surfaceptr srf;
	enum MolecState ms1;
	enum PanelFace face;
	surfactionptr actdetails;

	srfss=sim->srfss;
	if(!srfss) return 0;

	for(s=0;s<srfss->nsrf;s++) {
		srf=srfss->srflist[s];
		i1=i;															// first try no species change at surface
		for(ms1=(MolecState)0;ms1<MSMAX;ms1=(MolecState)(ms1+1))
			for(face=(PanelFace)0;face<3;face=(PanelFace)(face+1)) {
				actdetails=srf->actdetails[i1][ms1][face];
				if(actdetails)
					if(actdetails->srfrate[ms]>0 || actdetails->srfprob[ms]>0)
						if(actdetails->srfnewspec[ms]==i)
							return 1; }
		for(i1=0;i1<srfss->maxspecies;i1++)	// failed, so try with species change at surface
			for(ms1=(MolecState)0;ms1<MSMAX;ms1=(MolecState)(ms1+1))
				for(face=(PanelFace)0;face<3;face=(PanelFace)(face+1)) {
					actdetails=srf->actdetails[i1][ms1][face];
					if(actdetails)
						if(actdetails->srfrate[ms]>0 || actdetails->srfprob[ms]>0)
							if(actdetails->srfnewspec[ms]==i)
								return 1; }}
		return 0; }


/* srfsamestate */
int srfsamestate(enum MolecState ms1,enum PanelFace face1,enum MolecState ms2,enum MolecState *ms3ptr) {
	int same;
	enum MolecState ms3;

	if(face1==PFfront && ms2==MSsoln) same=1;
	else if(face1==PFback && ms2==MSbsoln) same=1;
	else if(face1==PFnone && ms1==ms2) same=1;
	else same=0;

	if(ms3ptr) {
		if(ms1==MSsoln) {
			if(face1==PFfront) ms3=MSsoln;
			else if(face1==PFback) ms3=MSbsoln;
			else ms3=MSnone; }
		else {
			if(face1==PFfront) ms3=MSsoln;
			else if(face1==PFback) ms3=MSbsoln;
			else ms3=ms1; }
		*ms3ptr=ms3; }
	return same; }


/* srfreverseaction */
void srfreverseaction(enum MolecState ms1,enum PanelFace face1,enum MolecState ms2,enum MolecState *ms3ptr,enum PanelFace *face2ptr,enum MolecState *ms4ptr) {
	enum MolecState ms3,ms4;
	enum PanelFace face2;

	if(ms1==MSsoln && face1==PFnone) {			// soln but neither face, which is impossible
		ms3=ms4=MSnone;
		face2=PFnone; }

	else if(ms1==MSsoln) {									// soln and either face, for simple collision
		if(ms2==MSsoln || ms2==MSbsoln) {
			ms3=MSsoln;
			face2=(ms2==MSsoln)?PFfront:PFback; }
		else {
			ms3=ms2;
			face2=PFnone; }
		ms4=(face1==PFfront)?MSsoln:MSbsoln; }

	else if(face1==PFnone) {								// bound and no face, for desorb etc.
		if(ms2==MSsoln || ms2==MSbsoln) {
			ms3=MSsoln;
			face2=(ms2==MSsoln)?PFfront:PFback; }
		else {
			ms3=ms2;
			face2=PFnone; }
		ms4=ms1; }

	else {																	// bound and face, for surface-bound collision
		if(ms2==MSsoln || ms2==MSbsoln) {
			ms3=ms1;
			face2=(ms2==MSsoln)?PFfront:PFback;
			ms4=(face1==PFfront)?MSsoln:MSbsoln; }
		else {
			ms3=ms2;
			face2=PFboth;
			ms4=ms1; }}

	if(ms3ptr) *ms3ptr=ms3;
	if(face2ptr) *face2ptr=face2;
	if(ms4ptr) *ms4ptr=ms4;
	return; }


/* srftristate2index */
void srftristate2index(enum MolecState ms,enum MolecState ms1,enum MolecState ms2,enum MolecState *ms3ptr,enum PanelFace *faceptr,enum MolecState *ms4ptr) {
	enum MolecState ms3,ms4;
	enum PanelFace face;

	if(ms==MSnone) ms=MSsoln;

	if(ms==MSsoln && (ms1==MSsoln || ms1==MSbsoln)) ms3=MSsoln;
	else if(ms==MSsoln) ms3=ms1;
	else ms3=ms;

	if(ms1==MSsoln) face=PFfront;
	else if(ms1==MSbsoln) face=PFback;
	else face=PFnone;

	ms4=ms2;

	if(ms!=MSsoln && ms1!=MSsoln && ms1!=MSbsoln && ms1!=ms) {
		ms3=MSnone;
		face=PFnone;
		ms4=MSnone; }

	if(ms3ptr) *ms3ptr=ms3;
	if(faceptr) *faceptr=face;
	if(ms4ptr) *ms4ptr=ms4;
	return; }


/* srfindex2tristate */
void srfindex2tristate(enum MolecState ms3,enum PanelFace face,enum MolecState ms4,enum MolecState *msptr,enum MolecState *ms1ptr,enum MolecState *ms2ptr) {
	enum MolecState ms,ms1,ms2;

	ms=ms3;

	if(face==PFfront) ms1=MSsoln;
	else if(face==PFback) ms1=MSbsoln;
	else ms1=ms3;

	ms2=ms4;

	if(msptr) *msptr=ms;
	if(ms1ptr) *ms1ptr=ms1;
	if(ms2ptr) *ms2ptr=ms2;
	return; }


/******************************************************************************/
/****************************** memory management *****************************/
/******************************************************************************/


/* surfaceactionalloc */
surfactionptr surfaceactionalloc(int species) {
	surfactionptr actdetails;
	enum MolecState ms;

	actdetails=NULL;
	actdetails=(surfactionptr) malloc(sizeof(struct surfactionstruct));
	if(!actdetails) return NULL;
	actdetails->srfnewspec=NULL;
	actdetails->srfrate=NULL;
#ifdef VCELL_HYBRID
	actdetails->srfRateExp=NULL;
#endif
	actdetails->srfprob=NULL;
	actdetails->srfcumprob=NULL;
	actdetails->srfdatasrc=NULL;
	actdetails->srfrevprob=NULL;

	CHECK(actdetails->srfnewspec=(int*) calloc(MSMAX1,sizeof(int)));
	for(ms=(MolecState)0;ms<MSMAX1;ms=(MolecState)(ms+1)) actdetails->srfnewspec[ms]=species;

	CHECK(actdetails->srfrate=(double*) calloc(MSMAX1,sizeof(double)));
	for(ms=(MolecState)0;ms<MSMAX1;ms=(MolecState)(ms+1)) actdetails->srfrate[ms]=0;

#ifdef VCELL_HYBRID
	CHECK(actdetails->srfRateExp=(Expression**) calloc(MSMAX1,sizeof(Expression*)));
	for(ms=(MolecState)0;ms<MSMAX1;ms=(MolecState)(ms+1)) actdetails->srfRateExp[ms]=0;
#endif

	CHECK(actdetails->srfprob=(double*) calloc(MSMAX1,sizeof(double)));
	for(ms=(MolecState)0;ms<MSMAX1;ms=(MolecState)(ms+1)) actdetails->srfprob[ms]=0;

	CHECK(actdetails->srfcumprob=(double*) calloc(MSMAX1,sizeof(double)));
	for(ms=(MolecState)0;ms<MSMAX1;ms=(MolecState)(ms+1)) actdetails->srfcumprob[ms]=0;

	CHECK(actdetails->srfdatasrc=(int*) calloc(MSMAX1,sizeof(int)));
	for(ms=(MolecState)0;ms<MSMAX1;ms=(MolecState)(ms+1)) actdetails->srfdatasrc[ms]=0;

	CHECK(actdetails->srfrevprob=(double*) calloc(MSMAX1,sizeof(double)));
	for(ms=(MolecState)0;ms<MSMAX1;ms=(MolecState)(ms+1)) actdetails->srfrevprob[ms]=0;

	return actdetails;

 failure:
	surfaceactionfree(actdetails);
	return NULL; }


/* surfaceactionfree */
void surfaceactionfree(surfactionptr actdetails) {
	if(!actdetails) return;
	free(actdetails->srfrevprob);
	free(actdetails->srfdatasrc);
	free(actdetails->srfcumprob);
	free(actdetails->srfprob);
	free(actdetails->srfrate);
	free(actdetails->srfnewspec);
	free(actdetails);
	return; }


/* panelsalloc. */
int panelsalloc(surfaceptr srf,int dim,int maxpanel,int maxspecies,enum PanelShape ps) {
	panelptr *newpnls,pnl;
	int p,npts,pt,d,oldmaxpanel;
	char **newpname,string[STRCHAR];

	npts=panelpoints(ps,dim);
	newpname=NULL;
	newpnls=NULL;
	CHECK(srf);
	oldmaxpanel=srf->maxpanel[ps];

	if(maxpanel<=0 || maxpanel<oldmaxpanel) return 0;
	else if(maxpanel==oldmaxpanel) return 1;

	CHECK(newpname=(char**) calloc(maxpanel,sizeof(char*)));
	for(p=0;p<maxpanel;p++) newpname[p]=NULL;
	for(p=0;p<oldmaxpanel;p++)
		newpname[p]=srf->pname[ps][p];
	for(;p<maxpanel;p++) {
		CHECK(newpname[p]=EmptyString());
		sprintf(newpname[p],"%s%i",surfps2string(ps,string),p); }

	CHECK(newpnls=(panelptr*) calloc(maxpanel,sizeof(panelptr)));
	for(p=0;p<maxpanel;p++) newpnls[p]=NULL;
	for(p=0;p<oldmaxpanel;p++)
		newpnls[p]=srf->panels[ps][p];
	for(;p<maxpanel;p++) {
		CHECK(newpnls[p]=(panelptr) malloc(sizeof(struct panelstruct)));
		pnl=newpnls[p];
		pnl->pname=newpname[p];
		pnl->ps=ps;
		pnl->srf=srf;
		pnl->npts=npts;
		pnl->point=NULL;
		pnl->maxneigh=0;
		pnl->nneigh=0;
		pnl->neigh=NULL;
		pnl->emitterabsorb[PFfront]=NULL;
		pnl->emitterabsorb[PFback]=NULL;

		CHECK(pnl->point=(double**) calloc(npts,sizeof(double*)));
		for(pt=0;pt<npts;pt++) pnl->point[pt]=NULL;
		for(pt=0;pt<npts;pt++) {
			CHECK(pnl->point[pt]=(double*) calloc(dim,sizeof(double)));
			for(d=0;d<dim;d++) pnl->point[pt][d]=0; }
		pnl->front[0]=pnl->front[1]=pnl->front[2]=0;
		pnl->jumpp[0]=pnl->jumpp[1]=NULL;
		pnl->jumpf[0]=pnl->jumpf[1]=PFnone; }

	srf->maxpanel[ps]=maxpanel;
	free(srf->pname[ps]);
	srf->pname[ps]=newpname;
	free(srf->panels[ps]);
	srf->panels[ps]=newpnls;

	if(srf->maxemitter[PFfront]) {		// add emitter stuff to new panels
		CHECK(emittersalloc(srf,PFfront,maxspecies,maxspecies)==0); }
	if(srf->maxemitter[PFback]) {
		CHECK(emittersalloc(srf,PFback,maxspecies,maxspecies)==0); }
		
	return 1;

 failure:
	return 0; }


/* panelfree */
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


/* emittersalloc */
int emittersalloc(surfaceptr srf,enum PanelFace face,int oldmaxspecies,int maxspecies) {
	int i1,p;
	enum PanelShape ps;
	panelptr pnl;
	int *newmaxemitter,*newnemitter;
	double **newemitteramount,***newemitterpos,*newemitterabsorb;

	newmaxemitter=NULL;
	newnemitter=NULL;
	newemitteramount=NULL;
	newemitterpos=NULL;
	newemitterabsorb=NULL;

	if(!srf->maxemitter[face])				// creating emitters from scratch
		oldmaxspecies=0;

	if(maxspecies>oldmaxspecies) {		// surface structure data, for either from scratch or larger maxspecies
		CHECK(newmaxemitter=(int*) calloc(maxspecies,sizeof(int)));
		for(i1=0;i1<oldmaxspecies;i1++) newmaxemitter[i1]=srf->maxemitter[face][i1];
		for(;i1<maxspecies;i1++) newmaxemitter[i1]=0;

		CHECK(newnemitter=(int*) calloc(maxspecies,sizeof(int)));
		for(i1=0;i1<oldmaxspecies;i1++) newnemitter[i1]=srf->nemitter[face][i1];
		for(;i1<maxspecies;i1++) newnemitter[i1]=0;
		
		CHECK(newemitteramount=(double**) calloc(maxspecies,sizeof(double*)));
		for(i1=0;i1<oldmaxspecies;i1++) newemitteramount[i1]=srf->emitteramount[face][i1];
		for(;i1<maxspecies;i1++) newemitteramount[i1]=NULL;

		CHECK(newemitterpos=(double***) calloc(maxspecies,sizeof(double**)));
		for(i1=0;i1<oldmaxspecies;i1++) newemitterpos[i1]=srf->emitterpos[face][i1];
		for(;i1<maxspecies;i1++) newemitterpos[i1]=NULL;

		free(srf->maxemitter[face]);
		free(srf->nemitter[face]);
		free(srf->emitteramount[face]);
		free(srf->emitterpos[face]);
		srf->maxemitter[face]=newmaxemitter;
		srf->nemitter[face]=newnemitter;
		srf->emitteramount[face]=newemitteramount;
		srf->emitterpos[face]=newemitterpos; }

	for(ps=(PanelShape)0;ps<PSMAX;ps=(PanelShape)(ps+1))						// panel structure data
		for(p=0;p<srf->maxpanel[ps];p++) {
			pnl=srf->panels[ps][p];
			if(!pnl->emitterabsorb[face] || maxspecies>oldmaxspecies) {
				CHECK(newemitterabsorb=(double*) calloc(maxspecies,sizeof(double)));
				i1=0;
				if(maxspecies>oldmaxspecies)
					for(i1=0;i1<oldmaxspecies;i1++) newemitterabsorb[i1]=pnl->emitterabsorb[face][i1];
				for(;i1<maxspecies;i1++) newemitterabsorb[i1]=0;
				free(pnl->emitterabsorb[face]);
				pnl->emitterabsorb[face]=newemitterabsorb; }}

	return 0;

 failure:
	return 1; }


/* surfacealloc */
surfaceptr surfacealloc(surfaceptr srf,int oldmaxspecies,int maxspecies,int dim) {
	int i,freesrf;
	enum PanelShape ps;
	enum MolecState ms;
	enum SrfAction ***newaction;
	surfactionptr ***newactdetails;

	if(srf && oldmaxspecies==maxspecies) return srf;

	newaction=NULL;
	newactdetails=NULL;
	freesrf=0;
	
	if(!srf) {
		srf=(surfaceptr) malloc(sizeof(struct surfacestruct));
		if(!srf) return NULL;
		freesrf=1;
		srf->sname=NULL;
		srf->srfss=NULL;
		srf->action=NULL;
		srf->actdetails=NULL;
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
		srf->emitterpos[PFfront]=srf->emitterpos[PFback]=NULL; }
	
	if(maxspecies) {
		CHECK(newaction=(enum SrfAction***) calloc(maxspecies,sizeof(enum SrfAction**)));
		for(i=0;i<maxspecies;i++) newaction[i]=NULL;
		for(i=0;i<oldmaxspecies;i++)
			newaction[i]=srf->action[i];
		for(;i<maxspecies;i++) {
			CHECK(newaction[i]=(enum SrfAction**) calloc(MSMAX,sizeof(enum SrfAction*)));
			for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1))
				newaction[i][ms]=NULL;
			for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1)) {
				CHECK(newaction[i][ms]=(enum SrfAction*) calloc(3,sizeof(enum SrfAction)));
				newaction[i][ms][PFfront]=newaction[i][ms][PFback]=SAtrans;
				newaction[i][ms][PFnone]=SAno; }}
		
		CHECK(newactdetails=(surfactionptr***) calloc(maxspecies,sizeof(surfactionptr**)));
		for(i=0;i<maxspecies;i++) newactdetails[i]=NULL;
		for(i=0;i<oldmaxspecies;i++)
			newactdetails[i]=srf->actdetails[i];
		for(;i<maxspecies;i++) {
			CHECK(newactdetails[i]=(surfactionptr**) calloc(MSMAX,sizeof(surfactionptr*)));
			for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1))
				newactdetails[i][ms]=NULL;
			for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1)) {
				CHECK(newactdetails[i][ms]=(surfactionptr*) calloc(3,sizeof(surfactionptr)));
				newactdetails[i][ms][PFfront]=NULL;
				newactdetails[i][ms][PFback]=NULL;
				newactdetails[i][ms][PFnone]=NULL; }}

		if(srf->maxemitter[PFfront]) {	// these calls are for larger maxspecies
			CHECK(emittersalloc(srf,PFfront,oldmaxspecies,maxspecies)==0); }
		if(srf->maxemitter[PFback]) {
			CHECK(emittersalloc(srf,PFback,oldmaxspecies,maxspecies)==0); }
		
		free(srf->action);
		srf->action=newaction;
		free(srf->actdetails);
		srf->actdetails=newactdetails; }
	
	return srf;
	
failure:
	if(freesrf) surfacefree(srf,maxspecies);
	return NULL; }


/* surfacefree */
void surfacefree(surfaceptr srf,int maxspecies) {
	int p,i,emit;
	enum PanelFace face;
	enum PanelShape ps;
	enum MolecState ms;

	if(!srf) return;

	for(face=PFfront;face<=PFback;face=(PanelFace)(face+1)) {
		if(srf->emitterpos[face]) {
			for(i=0;i<maxspecies;i++)
				if(srf->emitterpos[face][i]) {
					for(emit=0;emit<srf->maxemitter[face][i];emit++)
						free(srf->emitterpos[face][i][emit]);
					free(srf->emitterpos[face][i]); }
				free(srf->emitterpos[face]); }
		if(srf->emitteramount[face]) {
			for(i=0;i<maxspecies;i++)
				free(srf->emitteramount[face][i]);
			free(srf->emitteramount[face]); }
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
	
	for(i=0;i<maxspecies;i++)
		if(srf->actdetails[i]) {
			for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1))
				if(srf->actdetails[i][ms]) {
					for(face=(PanelFace)0;face<3;face=(PanelFace)(face+1))
						surfaceactionfree(srf->actdetails[i][ms][face]);
					free(srf->actdetails[i][ms]); }
			free(srf->actdetails[i]); }
	free(srf->actdetails);
	
	for(i=0;i<maxspecies;i++)
		if(srf->action[i]) {
			for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1))
				free(srf->action[i][ms]);
			free(srf->action[i]); }
	free(srf->action);
	
	free(srf);
	return; }


/* surfacessalloc */
surfacessptr surfacessalloc(surfacessptr srfss,int maxsurface,int maxspecies,int dim) {
	int s,newsrfss;
	char **newnames;
	surfaceptr *newsrflist;

	if(maxsurface<1 || maxspecies<0) return NULL;

	newsrfss=0;
	newnames=NULL;
	newsrflist=NULL;

	if(!srfss) {												// new allocation
		srfss=(surfacessptr) malloc(sizeof(struct surfacesuperstruct));
		if(!srfss) return NULL;
		newsrfss=1;
		srfss->condition=SCinit;
		srfss->sim=NULL;
		srfss->maxspecies=maxspecies;
		srfss->maxsrf=0;
		srfss->nsrf=0;
		srfss->epsilon=100*DBL_EPSILON;
		srfss->margin=100*DBL_EPSILON;
		srfss->neighdist=-1;
		srfss->snames=NULL;
		srfss->srflist=NULL;
		srfss->maxmollist=0;
		srfss->nmollist=0;
		srfss->srfmollist=NULL; }
	else {																// checks, and update maxspecies if reallocation
		if(maxsurface<srfss->maxsrf) return NULL;
		if(maxspecies<srfss->maxspecies) return NULL;
		if(maxspecies>srfss->maxspecies) {
			for(s=0;s<srfss->maxsrf;s++)
				CHECK(surfacealloc(srfss->srflist[s],srfss->maxspecies,maxspecies,dim)!=NULL); }
		srfss->maxspecies=maxspecies; }

	if(maxsurface>srfss->maxsrf) {			// allocate any new surface names and surfaces
		CHECK(newnames=(char**) calloc(maxsurface,sizeof(char*)));		// surface names
		for(s=0;s<maxsurface;s++) newnames[s]=NULL;
		for(s=0;s<srfss->maxsrf;s++) newnames[s]=srfss->snames[s];
		for(;s<maxsurface;s++)
			CHECK(newnames[s]=EmptyString());

		CHECK(newsrflist=(surfaceptr*) calloc(maxsurface,sizeof(surfaceptr)));	// surface list
		for(s=0;s<maxsurface;s++) newsrflist[s]=NULL;
		for(s=0;s<srfss->maxsrf;s++)
			newsrflist[s]=srfss->srflist[s];
		for(;s<maxsurface;s++) {
			CHECK(newsrflist[s]=surfacealloc(NULL,0,maxspecies,dim));
			newsrflist[s]->srfss=srfss;
			newsrflist[s]->sname=newnames[s]; }

		srfss->maxsrf=maxsurface;
		free(srfss->snames);
		srfss->snames=newnames;
		free(srfss->srflist);
		srfss->srflist=newsrflist; }
	
	return srfss;

 failure:
 	if(newsrfss) surfacessfree(srfss);
 	return NULL; }


/* surfacessfree */
void surfacessfree(surfacessptr srfss) {
	int s;

	if(!srfss) return;

	free(srfss->srfmollist);
	if(srfss->srflist) {
		for(s=0;s<srfss->maxsrf;s++)
			surfacefree(srfss->srflist[s],srfss->maxspecies);
		free(srfss->srflist); }

	if(srfss->snames) {
		for(s=0;s<srfss->maxsrf;s++)
			free(srfss->snames[s]);
		free(srfss->snames); }

	free(srfss);
	return; }


/******************************************************************************/
/**************************** data structure output ***************************/
/******************************************************************************/

/* surfaceoutput */
void surfaceoutput(simptr sim) {
	int s,p,i,dim,nspecies,jumpfrnt,jumpback,ll,vflag,p2,emit,d,same,none;
	surfacessptr srfss;
	surfaceptr srf;
	double **point,*front,prob;
	char **pname,string[STRCHAR];
	panelptr pnl;
	enum SrfAction act,***action;
	enum MolecState ms,ms2,ms3,ms4,ms5;
	enum PanelFace face;
	surfactionptr actdetails;

	vflag=strchr(sim->flags,'v')?1:0;

	printf("SURFACE PARAMETERS\n");
	srfss=sim->srfss;
	dim=sim->dim;
	nspecies=sim->mols?sim->mols->nspecies:0;
	if(!srfss) {
		printf(" No internal surfaces\n\n");
		return; }
	printf(" Surface epsilon, margin, and neighbor distances: %g %g %g\n",srfss->epsilon,srfss->margin,srfss->neighdist);

	if(sim->mols) {
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
		printf("\n"); }

	printf(" Surfaces allocated: %i, surfaces defined: %i\n\n",srfss->maxsrf,srfss->nsrf);
	for(s=0;s<srfss->nsrf;s++) {
		srf=srfss->srflist[s];
		printf(" Surface: %s\n",srfss->snames[s]);
		if(srf->port[PFfront]) printf("  The front of this surface is part of port %s\n",srf->port[PFfront]->portname);
		if(srf->port[PFback]) printf("  The back of this surface is part of port %s\n",srf->port[PFback]->portname);

		if(sim->mols && srf->action) {
			printf("  actions for molecules:\n");
			action=srf->action;
			for(i=1;i<nspecies;i++) {
				for(face=(PanelFace)0;face<2;face=(PanelFace)(face+1)) {
					same=1;
					act=action[i][MSsoln][face];
					for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1))
						if(action[i][ms][face]!=act) same=0;
					if(same) {
						if(act!=SAmult)
							printf("   %s(all) at %s: %s\n",sim->mols->spname[i],face==PFfront?"front":"back",surfact2string(act,string)); }
					else {
						for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1)) {
							act=action[i][ms][face];
							if(sim->mols->exist[i][ms] && act!=SAmult) {
								printf("   %s(%s)",sim->mols->spname[i],molms2string(ms,string));
								printf(" at %s: %s\n",face==PFfront?"front":"back",surfact2string(act,string)); }}}}}

			printf("  rates for molecules:");
			none=1;
			for(i=1;i<nspecies;i++)
				for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1))
					for(face=(PanelFace)0;face<3;face=(PanelFace)(face+1))
						if(action[i][ms][face]==SAmult && srf->actdetails) {
							actdetails=srf->actdetails[i][ms][face];
							for(ms2=(MolecState)0;ms2<MSMAX1;ms2=(MolecState)(ms2+1)) {
								prob=actdetails->srfprob[ms2];
								if(prob>0 && !srfsamestate(ms,face,ms2,NULL)) {
									if(none) {
										none=0;
										printf("\n"); }
									srfindex2tristate(ms,face,ms2,&ms3,&ms4,&ms5);
									printf("    %s(%s)",sim->mols->spname[i],molms2string(ms3,string));
									printf(" %s ->",molms2string(ms4,string));
									printf(" %s",molms2string(ms5,string));
									if(actdetails->srfnewspec[ms2]!=i) printf(" (convert to %s)",sim->mols->spname[actdetails->srfnewspec[ms2]]);
									if(actdetails->srfdatasrc[ms2]==1) printf(", requested rate=%g",actdetails->srfrate[ms2]);
									else if(actdetails->srfdatasrc[ms2]==2) printf(", requested prob=%g",actdetails->srfprob[ms2]);
									printf(", actual rate=%g, prob=%g\n",srfcalcrate(sim,srf,i,ms,face,ms2),prob); }}}
			if(none) printf(" no stochastic interactions\n"); }

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
		if(srf->action) {
			for(i=1;i<nspecies;i++) {
				if(srf->action[i][MSsoln][PFfront]==SAjump) jumpfrnt=1;
				if(srf->action[i][MSsoln][PFback]==SAjump) jumpback=1; }}
		
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
				if(dim==1) printfException("   error, cylinders are not implemented in 1-D");
				else if(dim==2) printf("   %s: (%g,%g) to (%g,%g), R=%g, facing: %s, length: %g",pname[p],point[0][0],point[0][1],point[1][0],point[1][1],point[2][0],front[2]==1?"out":"in",2.0*Geo_LineLength(point[0],point[1],2));
				else printf("   %s: (%g,%g,%g) to (%g,%g,%g), R=%g, facing: %s, draw: %g %g, area: %g",pname[p],point[0][0],point[0][1],point[0][2],point[1][0],point[1][1],point[1][2],point[2][0],front[2]==1?"out":"in",point[2][1],point[2][2],2.0*PI*point[2][0]*Geo_LineLength(point[0],point[1],3));
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
				if(dim==1) printfException("   error, disks are not implemented in 1-D");
				else if(dim==2) printf("   %s: (%g,%g), R=%g, facing: (%g,%g), length: %g",pname[p],point[0][0],point[0][1],point[1][0],front[0],front[1],2.0*point[1][0]);
				else printf("   %s: (%g,%g,%g), R=%g, facing: (%g,%g,%g), draw: %g, area: %g",pname[p],point[0][0],point[0][1],point[0][2],point[1][0],front[0],front[1],front[2],point[1][1],PI*point[1][0]*point[1][0]);
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

		printf("\n"); }
	return; }


/* writesurfaces */
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
	surfactionptr actdetails;
	enum SrfAction act;

	dim=sim->dim;
	nspecies=sim->mols?sim->mols->nspecies:0;
	fprintf(fptr,"# Surface parameters\n");
	if(!sim->srfss) return;
	srfss=sim->srfss;
	fprintf(fptr,"max_surface %i\n",srfss->maxsrf);
	fprintf(fptr,"epsilon %g\n",srfss->epsilon);
	fprintf(fptr,"margin %g\n",srfss->margin);
	fprintf(fptr,"neighbor_dist %g\n",srfss->neighdist);
	fprintf(fptr,"\n");
	for(s=0;s<srfss->nsrf;s++) {
		srf=srfss->srflist[s];
		fprintf(fptr,"start_surface %s\n",srfss->snames[s]);
		if(srf->action) {
			for(i=1;i<nspecies;i++) {
				for(ms1=(MolecState)0;ms1<MSMAX;ms1=(MolecState)(ms1+1))
					for(face=(PanelFace)0;face<2;face=(PanelFace)(face+1)) {
						act=srf->action[i][ms1][face];
						if(act!=SAtrans && act!=SAmult) {
							fprintf(fptr,"action %s(%s)",sim->mols->spname[i],molms2string(ms1,string));
							fprintf(fptr," %s %s\n",face==PFfront?"front":"back",surfact2string(act,string)); }}}}

		if(srf->action && srf->actdetails) {
			for(i=1;i<nspecies;i++) {
				for(ms1=(MolecState)0;ms1<MSMAX;ms1=(MolecState)(ms1+1))
					for(face=(PanelFace)0;face<3;face=(PanelFace)(face+1))
						if(srf->action[i][ms1][face]==SAmult && srf->actdetails[i][ms1][face]) {
							actdetails=srf->actdetails[i][ms1][face];
							for(ms2=(MolecState)0;ms2<MSMAX1;ms2=(MolecState)(ms2+1)) {
								if(actdetails->srfrate[ms2]>0) {
									fprintf(fptr,"rate %s(%s)",sim->mols->spname[i],molms2string(ms1,string));
									fprintf(fptr," %s",(face==PFnone)?molms2string(ms1,string):(face==PFfront?"fsoln":"bsoln"));
									fprintf(fptr," %s %g",molms2string(ms2,string),actdetails->srfrate[ms2]);
									if(actdetails->srfnewspec[ms2]!=i)
										fprintf(fptr," %s",sim->mols->spname[actdetails->srfnewspec[ms2]]);
									fprintf(fptr,"\n"); }
								else if(actdetails->srfrate[ms2]<0 && actdetails->srfprob[ms2]>0) {
									fprintf(fptr,"rate_internal %s(%s)",sim->mols->spname[i],molms2string(ms1,string));
									fprintf(fptr," %s",(face==PFnone)?molms2string(ms1,string):(face==PFfront?"fsoln":"bsoln"));
									fprintf(fptr," %s %g",molms2string(ms2,string),actdetails->srfprob[ms2]);
									if(actdetails->srfnewspec[ms2]!=i)
										fprintf(fptr," %s",sim->mols->spname[actdetails->srfnewspec[ms2]]);
									fprintf(fptr,"\n"); }}}}}
		
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

		if(srf->action) {
			for(i=1;i<nspecies;i++)
				for(face=(PanelFace)0;face<2;face=(PanelFace)(face+1))
					if(srf->action[i][MSsoln][face]==SAjump)
						for(ps=(PanelShape)0;ps<PSMAX;ps=(PanelShape)(ps+1))
							for(p=0;p<srf->npanel[ps];p++) {
								pnl=srf->panels[ps][p];
								if(pnl->jumpp[face] && (pnl->jumpf[face]==PFfront || pnl->jumpf[face]==PFback))
									fprintf(fptr,"jump %s %s -> %s %s\n",pnl->pname,surfface2string(face,string),pnl->jumpp[face]->pname,surfface2string(pnl->jumpf[face],string)); }}

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


/* checksurfaceparams */
int checksurfaceparams(simptr sim,int *warnptr) {
	int error,warn;
	int d,c,s,i,dim,nspecies,p,pt,fjump,bjump,fport,bport;
	surfacessptr srfss;
	surfaceptr srf;
	enum MolecState ms,ms2;
	double syslen,**point,*front,lowwall[3],highwall[3],norm[3],prob;
	enum PanelShape ps;
	enum PanelFace face;
	panelptr pnl;
	char string[STRCHAR];
	surfactionptr actdetails;

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

	if(sim->mols)																			// maxspecies
		if(sim->mols->maxspecies!=sim->srfss->maxspecies) {error++;printfException(" SMOLDYN BUG: number of molecule species differ between mols and srfss\n");}

	if(sim->mols)																			// nmollist
		if(srfss->nmollist!=sim->mols->nlist) {error++;printfException(" SMOLDYN BUG: mismatch between number of molecule lists in surface and in molecule structures");}

																										// possible superstructure warnings, errors
	if(srfss->nsrf==0) {warn++;printf(" WARNING: Surface superstructure is defined, but no surfaces are defined\n");}
	if(srfss->nsrf>srfss->maxsrf) {error++;printfException(" SMOLDYN BUG: More surfaces are defined than allocated\n");}
	if(srfss->epsilon>0.01*syslen) {warn++;printf(" WARNING: surface epsilon value is large compared to system size\n");}
	if(srfss->margin>0.01*syslen) {warn++;printf(" WARNING: surface margin value is large compared to system size\n");}
	if(srfss->neighdist>0.01*syslen) {warn++;printf(" WARNING: surface neighbor distance value is large compared to system size\n");}

	for(s=0;s<srfss->nsrf;s++) {											// surface missing and incorrect elements, not panels
		if(strlen(srfss->snames[s])==0) {error++;printfException(" ERROR: surface %i is unnamed\n",s);}
		if(!srfss->srflist[s]) {error++;printfException(" SMOLDYN BUG: pointer to surface %i is NULL",s);}
		srf=srfss->srflist[s];
		if(srf->sname!=srfss->snames[s]) {error++;printfException(" SMOLDYN BUG: surface %i name pointer does not match surface superstructure pointer\n",s);}
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
		if(srf->action && nspecies>1) {
			fjump=bjump=0;
			for(i=1;i<nspecies;i++) {
				if(srf->action[i][MSsoln][PFfront]==SAjump) fjump=1;
				if(srf->action[i][MSsoln][PFback]==SAjump) bjump=1; }
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
						if(pnl->jumpp[PFback] && !(pnl->jumpf[PFback]==PFfront || pnl->jumpf[PFback]==PFback)) {error++;printfException(" SMOLDYN BUG: surface %s, panel %s jumps to an undefined panel face\n",srf->sname,pnl->pname);} }}}

	for(s=0;s<srfss->nsrf;s++) {											// make sure panel panel neighbors are stored correctly
		srf=srfss->srflist[s];
		for(ps=(PanelShape)0;ps<PSMAX;ps=(PanelShape)(ps+1))
			for(p=0;p<srf->npanel[ps];p++) {
				pnl=srf->panels[ps][p];
				if(pnl->nneigh>0 && !pnl->neigh) {error++;printfException(" SMOLDYN BUG: surface %s, panel %s has %i neighbors, but none listed\n",srf->sname,pnl->pname,pnl->nneigh);} }}

	for(s=0;s<srfss->nsrf;s++) {											// make sure porting actions are linked to ports
		srf=srfss->srflist[s];
		if(srf->action) {
			fport=bport=0;
			for(i=0;i<nspecies;i++) {
				if(srf->action[i][MSsoln][PFfront]==SAport) fport=1;
				if(srf->action[i][MSsoln][PFback]==SAport) bport=1; }
			if(fport && !srf->port[PFfront]) {
				error++;
				printfException(" ERROR: surface %s front face has porting action, but is not linked to a port\n",srf->sname);
				for(i=0;i<nspecies;i++) if(srf->action[i][MSsoln][PFfront]==SAport) srf->action[i][MSsoln][PFfront]=SAreflect; }
			if(bport && !srf->port[PFback]) {
				error++;
				printfException(" ERROR: surface %s back face has porting action, but is not linked to a port\n",srf->sname);
				for(i=0;i<nspecies;i++) if(srf->action[i][MSsoln][PFback]==SAport) srf->action[i][MSsoln][PFback]=SAreflect; }}}

	if(sim->mols)																			// check that requested surface rates can be achieved
		for(s=0;s<srfss->nsrf;s++) {
			srf=srfss->srflist[s];
			if(srf->action && srf->actdetails) {
				for(i=1;i<sim->mols->nspecies;i++)
					for(ms=(MolecState)0;ms<MSMAX;ms=(MolecState)(ms+1))
						for(face=(PanelFace)0;face<3;face=(PanelFace)(face+1))
							if(srf->action[i][ms][face]==SAmult) {
								actdetails=srf->actdetails[i][ms][face];
								if(!actdetails) {
									printfException(" SMOLDYN BUG: action is SAmult but action details not allocated");error++;}
								else {
									for(ms2=(MolecState)0;ms2<MSMAX1;ms2=(MolecState)(ms2+1)) {
										prob=actdetails->srfprob[ms2];
										if(prob<0 || prob>1) {
											error++;
											printfException(" SMOLDYN BUG: surface interaction probability is <0 or >1\n"); }
										else {
											if(actdetails->srfdatasrc[ms2]==1)
												if(fabs((actdetails->srfrate[ms2]-srfcalcrate(sim,srf,i,ms,face,ms2))/actdetails->srfrate[ms2])>0.01) {
													warn++;
													printf(" WARNING: requested surface '%s' rate for %s(%s) ->",srf->sname,sim->mols->spname[i],molms2string(ms,string));
													printf(" %s(%s) cannot be achieved\n",sim->mols->spname[i],molms2string(ms2,string)); }}}}}}}

	if(sim->mols)																			// check that don't have conflicting surface actions and emitters
		for(s=0;s<srfss->nsrf;s++) {
			srf=srfss->srflist[s];
			if(srf->action) {
				for(face=(PanelFace)0;face<2;face=(PanelFace)(face+1)) {
					for(i=1;i<sim->mols->nspecies;i++)
						if(srf->nemitter[face] && srf->nemitter[face][i])
							if(srf->action[i][MSsoln][face]!=SAreflect) {
								warn++;
								printf(" WARNING: surface '%s' %s-side action for %s",srf->sname,surfface2string(face,string),sim->mols->spname[i]);
								printf(" is listed as %s, but molecules will absorb instead because of unbounded emitters\n",surfact2string(srf->action[i][MSsoln][face],string)); }}}}

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
// check that epsilon, margin, and neighbor_dist values are reasonable

	if(warnptr) *warnptr=warn;
	return error; }


/******************************************************************************/
/****************************** structure set up ******************************/
/******************************************************************************/


/* surfenablesurfaces */
int surfenablesurfaces(simptr sim,int maxsurf) {
	surfacessptr surfss;

	if(sim->srfss)									// check for redundant function call
		if(maxsurf==-1 || sim->srfss->maxsrf==maxsurf)
			if((sim->mols && sim->srfss->maxspecies==sim->mols->maxspecies) || (!sim->mols && sim->srfss->maxspecies==0))
				return 0;
	surfss=surfacessalloc(sim->srfss,maxsurf<0?5:maxsurf,sim->mols?sim->mols->maxspecies:0,sim->dim);
	if(!surfss) return 1;
	sim->srfss=surfss;
	surfss->sim=sim;
	boxsetcondition(sim->boxs,SCparams,0);
	surfsetcondition(sim->srfss,SClists,0);
	return 0; }


/* surfaddsurface */
surfaceptr surfaddsurface(simptr sim,const char *surfname) {
	int er,s;
	surfacessptr srfss;
	surfaceptr srf;

	if(!sim->srfss) {
		er=surfenablesurfaces(sim,-1);
		if(er) return NULL; }
	srfss=sim->srfss;

	s=stringfind(srfss->snames,srfss->nsrf,surfname);
	if(s<0) {
		if(srfss->nsrf==srfss->maxsrf) {
			er=surfenablesurfaces(sim,srfss->nsrf*2+1);
			if(er) return NULL; }
		s=srfss->nsrf++;
		strncpy(srfss->snames[s],surfname,STRCHAR-1);
		srfss->snames[s][STRCHAR-1]='\0';
		srf=srfss->srflist[s];
		surfsetcondition(srfss,SClists,0); }
	else
		srf=srfss->srflist[s];

	surfsetcondition(sim->srfss,SClists,0);
	return srf; }


/* surfsetcondition. */
void surfsetcondition(surfacessptr surfss,enum StructCond cond,int upgrade) {
	if(!surfss) return;
	if(upgrade==0 && surfss->condition>cond) surfss->condition=cond;
	else if(upgrade==1 && surfss->condition<cond) surfss->condition=cond;
	else if(upgrade==2) surfss->condition=cond;
	if(surfss->condition<surfss->sim->condition) {
		cond=surfss->condition;
		simsetcondition(surfss->sim,cond==SCinit?SClists:cond,0); }
	return; }


/* surfsetepsilon */
int surfsetepsilon(simptr sim,double epsilon) {
	int er;
	
	if(!sim->srfss) {
		er=surfenablesurfaces(sim,-1);
		if(er) return 2; }
	if(epsilon<=0) return 3;
	sim->srfss->epsilon=epsilon;
	return 0; }


/* surfsetmargin */
int surfsetmargin(simptr sim,double margin) {
	int er;
	
	if(!sim->srfss) {
		er=surfenablesurfaces(sim,-1);
		if(er) return 2; }
	if(margin<0) return 3;
	sim->srfss->margin=margin;
	return 0; }


/* surfsetneighdist. */
int surfsetneighdist(simptr sim,double neighdist) {
	int er;
	
	if(!sim->srfss) {
		er=surfenablesurfaces(sim,-1);
		if(er) return 2; }
	if(neighdist<=0) return 3;
	sim->srfss->neighdist=neighdist;
	return 0; }


/* surfsetcolor */
int surfsetcolor(surfaceptr srf,enum PanelFace face,double *rgba) {
	int col;

	if(!srf) return 1;
	for(col=0;col<4;col++)
		if(rgba[col]<0 || rgba[col]>1) return 2;

	if(face==PFfront || face==PFboth)
		for(col=0;col<4;col++) srf->fcolor[col]=rgba[col];
	if(face==PFback || face==PFboth)
		for(col=0;col<4;col++) srf->bcolor[col]=rgba[col];

	return 0; }


/* surfsetedgepts */
int surfsetedgepts(surfaceptr srf,double value) {
	if(!srf) return 1;
	if(value<0) return 2;
	srf->edgepts=value;
	return 0; }


/* surfsetstipple */
int surfsetstipple(surfaceptr srf,unsigned int factor,unsigned int pattern) {
	if(!srf) return 1;
	if(factor<=0 || pattern<0 || pattern>0xFFFF) return 2;
	srf->edgestipple[0]=factor;
	srf->edgestipple[1]=pattern;
	return 0; }


/* surfsetdrawmode */
int surfsetdrawmode(surfaceptr srf,enum PanelFace face,enum DrawMode dm) {
	if(!srf) return 1;
	if(dm==DMnone) return 2;
	if(face==PFfront || face==PFboth) srf->fdrawmode=dm;
	if(face==PFback || face==PFboth) srf->bdrawmode=dm;
	return 0; }


/* surfsetshiny */
int surfsetshiny(surfaceptr srf,enum PanelFace face,double shiny) {
	if(!srf) return 1;
	if(shiny<0 || shiny>128) return 2;
	if(face==PFfront || face==PFboth) srf->fshiny=shiny;
	if(face==PFback || face==PFboth) srf->bshiny=shiny;
	return 0; }


/* surfsetaction */
int surfsetaction(surfaceptr srf,int ident,enum MolecState ms,enum PanelFace face,enum SrfAction act) {
	int i1,ilo,ihi;
	enum MolecState ms1,mslo,mshi;

	if(ident==0 || ident>=srf->srfss->maxspecies) return 1;
	else if(ident<0) {ilo=1;ihi=srf->srfss->maxspecies-1;}
	else ilo=ihi=ident;

	if(ms==MSbsoln || ms==MSnone) return 2;
	else if(ms==MSall) {mslo=(MolecState)0;mshi=(MolecState)(MSMAX-1);}
	else mslo=mshi=ms;

	if(face==PFfront || face==PFback || face==PFboth) {		// face is front, back, or both
		if(!(act==SAreflect || act==SAtrans || act==SAabsorb || act==SAjump || act==SAport || act==SAmult))
			return 3;
		for(i1=ilo;i1<=ihi;i1++)
			for(ms1=mslo;ms1<=mshi;ms1=(MolecState)(ms1+1)) {
				if(face==PFfront || face==PFboth)
					srf->action[i1][ms1][PFfront]=act;
				if(face==PFback || face==PFboth)
					srf->action[i1][ms1][PFback]=act; }}
	else {																									// face is PFnone
		if(!(act==SAmult || act==SAno)) return 3;
		for(i1=ilo;i1<=ihi;i1++)
			for(ms1=mslo;ms1<=mshi;ms1=(MolecState)(ms1+1))
				srf->action[i1][ms1][face]=act; }

	surfsetcondition(srf->srfss,SCparams,0);
	return 0; }


/* surfsetrate */
int surfsetrate(surfaceptr srf,int ident,enum MolecState ms,enum MolecState ms1,enum MolecState ms2,int newident,double value,int which) {
	int i1,ilo,ihi;
	enum MolecState ms3,ms4;
	enum PanelFace face;
	surfactionptr actdetails;

	if(ident==0 || ident>=srf->srfss->maxspecies) return 1;
	else if(ident<0) {ilo=1;ihi=srf->srfss->maxspecies-1;}
	else ilo=ihi=ident;

	if(ms==MSbsoln || ms==MSall) return 2;

	if(ms1>MSbsoln) return 3;
	else if(ms!=MSsoln && ms1!=MSsoln && ms1!=MSbsoln && ms1!=ms) return 3;

	if(ms2>MSbsoln) return 4;
	else if(ms2==ms1) return 4;

	if((newident!=-5 && newident<0) || newident>=srf->srfss->maxspecies) return 5;

	if(value<0) return 6;
	else if(which==2 && value>1) return 6;

	srftristate2index(ms,ms1,ms2,&ms3,&face,&ms4);
	for(i1=ilo;i1<=ihi;i1++) {
		if(!srf->actdetails[i1][ms3][face]) {
			actdetails=surfaceactionalloc(i1);				// allocate action details
			if(!actdetails) return -1;
			srf->actdetails[i1][ms3][face]=actdetails; }
		actdetails=srf->actdetails[i1][ms3][face];

		srf->action[i1][ms3][face]=SAmult;						// set action to mult
		if(which==1) {															// and set up action details
			actdetails->srfrate[ms4]=value;
			actdetails->srfdatasrc[ms4]=1; }
		else if(which==2) {
			actdetails->srfprob[ms4]=value;
			actdetails->srfdatasrc[ms4]=2; }
		actdetails->srfnewspec[ms4]=(newident==-5)?i1:newident; }

	surfsetcondition(srf->srfss,SCparams,0);
	return 0; }

#ifdef VCELL_HYBRID
int surfSetRateExp(surfaceptr srf,int ident,enum MolecState ms,enum MolecState ms1,enum MolecState ms2,int newident,Expression* rateExp,int which) {
	int i1,ilo,ihi;
	enum MolecState ms3,ms4;
	enum PanelFace face;
	surfactionptr actdetails;

	if(ident==0 || ident>=srf->srfss->maxspecies) return 1;
	else if(ident<0) {ilo=1;ihi=srf->srfss->maxspecies-1;}
	else ilo=ihi=ident;

	if(ms==MSbsoln || ms==MSall) return 2;

	if(ms1>MSbsoln) return 3;
	else if(ms!=MSsoln && ms1!=MSsoln && ms1!=MSbsoln && ms1!=ms) return 3;

	if(ms2>MSbsoln) return 4;
	else if(ms2==ms1) return 4;

	if((newident!=-5 && newident<0) || newident>=srf->srfss->maxspecies) return 5;

	if(rateExp==NULL) return 6;
	
	srftristate2index(ms,ms1,ms2,&ms3,&face,&ms4);
	for(i1=ilo;i1<=ihi;i1++) {
		if(!srf->actdetails[i1][ms3][face]) {
			actdetails=surfaceactionalloc(i1);				// allocate action details
			if(!actdetails) return -1;
			srf->actdetails[i1][ms3][face]=actdetails; }

		actdetails=srf->actdetails[i1][ms3][face];

		srf->action[i1][ms3][face]=SAmult;						// set action to mult
		if(which==1) {															// and set up action details
			actdetails->srfRateExp[ms4]=rateExp;
			actdetails->srfdatasrc[ms4]=1; }
		else if(which==2) {
			actdetails->srfRateExp[ms4]=rateExp;
			actdetails->srfdatasrc[ms4]=2; }
		actdetails->srfnewspec[ms4]=(newident==-5)?i1:newident; }

	surfsetcondition(srf->srfss,SCparams,0);
	return 0; }

int surfUpdateRate(simptr sim, moleculeptr mptr, enum PanelFace face, panelptr pnl) {
	int molIdent = mptr->ident;
	surfacessptr surfacess = sim -> srfss;
	int numSrfs = surfacess->nsrf;
	surfaceptr * surfacelist = surfacess->srflist;
	surfactionptr actdetails;
	enum MolecState ms,ms2;
	enum PanelFace f; 
	Expression * surfRateExp;
	for(int s=0; s<numSrfs; s++) {
		surfaceptr srf=surfacelist[s];
		if(srf->actdetails) {
			for(f=face;f<PFboth;f=(PanelFace)(f+1))
			{
				actdetails=srf->actdetails[mptr->ident][mptr->mstate][f];
				for(ms2=(MolecState)0;ms2<MSMAX1;ms2=(MolecState)(ms2+1)) {
					if(actdetails != NULL && actdetails->srfRateExp[ms2] != NULL)
					{
						surfRateExp = actdetails->srfRateExp[ms2];
						actdetails->srfrate[ms2] = evaluateRnxRate2(sim, surfRateExp, true, mptr->pos, pnl->pname);
					}
				}
			}
		}
		
	}
	return 0; }
#endif

/* surfsetmaxpanel */
int surfsetmaxpanel(surfaceptr srf,int dim,enum PanelShape ps,int maxpanel) {
	int ok;

	if(!srf) return 1;
	if(ps>=PSMAX) return 2;
	if(dim==1 && ps>PSsph) return 2;
	if(maxpanel==srf->maxpanel[ps]) return 0;
	if(maxpanel<srf->maxpanel[ps]) return 3;
	ok=panelsalloc(srf,dim,maxpanel,srf->srfss->maxspecies,ps);
	if(!ok) return -1;
	return 0; }


/* surfaddpanel */
int surfaddpanel(surfaceptr srf,int dim,enum PanelShape ps,const char *string,double *params,const char *name) {
	int p,ok,pdim,pt,d;
	panelptr pnl;
	double point[5][3],front[3];
	char ch;
	enum PanelShape ps2;
	
	if(!srf) return 1;
	if(ps>=PSMAX) return 2;
	if(dim==1 && ps>PSsph) return 2;

	if(ps==PSrect) {									// rectangle
		if(!string) return 3;
		ch=string[0];
		if(!(ch=='+' || ch=='-')) return 3;
		if(string[1]=='0' || string[1]=='x') pdim=0;
		else if(string[1]=='1' || string[1]=='y') pdim=1;
		else if(string[1]=='2' || string[1]=='z') pdim=2;
		else pdim=4;
		if(pdim>=dim) return 3;
		if(dim==1) {
			point[0][0]=params[0]; }
		else if(dim==2) {
			pt=1;
			if(ch=='-') pt*=-1;
			if(pdim==0) pt*=-1;
			if(params[2]<0) pt*=-1;
			if(pt<0) pt=0;
			point[pt][0]=params[0];
			point[pt][1]=params[1];
			point[!pt][0]=params[0]+(pdim==0?0:params[2]);
			point[!pt][1]=params[1]+(pdim==1?0:params[2]);
			front[2]=(double)(!pdim); }
		else if(dim==3) {
			pt=1;
			if(ch=='-') pt*=-1;
			if(params[3]*params[4]<0) pt*=-1;
			if(pt<0) pt=3;
			point[0][0]=params[0];
			point[0][1]=params[1];
			point[0][2]=params[2];
			point[pt][0]=params[0]+(pdim==2?params[3]:0);
			point[pt][1]=params[1]+(pdim==0?params[3]:0);
			point[pt][2]=params[2]+(pdim==1?params[4]:0);
			point[2][0]=params[0]+(pdim==0?0:params[3]);
			point[2][1]=params[1]+(pdim==1?0:(pdim==0?params[3]:params[4]));
			point[2][2]=params[2]+(pdim==2?0:params[4]);
			point[4-pt][0]=params[0]+(pdim==1?params[3]:0);
			point[4-pt][1]=params[1]+(pdim==2?params[4]:0);
			point[4-pt][2]=params[2]+(pdim==0?params[4]:0);
			if(pdim==0) front[2]=(double)(pt==1?1:2);
			else if(pdim==1) front[2]=(double)(pt==1?2:0);
			else front[2]=(double)(pt==1?0:1); }
		front[0]=(double)(ch=='+'?1:-1);
		front[1]=(double)pdim; }
	
	else if(ps==PStri) {							// triangle
		if(dim==1) {
			point[0][0]=params[0];
			front[0]=(double)1; }
		else if(dim==2) {
			point[0][0]=params[0];
			point[0][1]=params[1];
			point[1][0]=params[2];
			point[1][1]=params[3];
			Geo_LineNormal(point[0],point[1],front); }
		else if(dim==3) {
			point[0][0]=params[0];
			point[0][1]=params[1];
			point[0][2]=params[2];
			point[1][0]=params[3];
			point[1][1]=params[4];
			point[1][2]=params[5];
			point[2][0]=params[6];
			point[2][1]=params[7];
			point[2][2]=params[8];
			Geo_TriNormal(point[0],point[1],point[2],front); }}
	
	else if(ps==PSsph) {							// sphere
		if(dim==1) {
			point[0][0]=params[0];
			point[1][0]=fabs(params[1]);
			front[0]=(double)sign(params[1]); }
		else if(dim==2) {
			point[0][0]=params[0];
			point[0][1]=params[1];
			point[1][0]=fabs(params[2]);
			point[1][1]=params[3];
			if(point[1][1]<=0) return 4;
			front[0]=(double)sign(params[2]); }
		else if(dim==3) {
			point[0][0]=params[0];
			point[0][1]=params[1];
			point[0][2]=params[2];
			point[1][0]=fabs(params[3]);
			point[1][1]=params[4];
			if(point[1][1]<=0) return 4;
			point[1][2]=params[5];
			if(point[1][2]<=0) return 4;
			front[0]=(double)sign(params[3]); }}
	
	else if(ps==PScyl) {								// cylinder
		if(dim==2) {
			point[0][0]=params[0];
			point[0][1]=params[1];
			point[1][0]=params[2];
			point[1][1]=params[3];
			if(point[0][0]==point[1][0] && point[0][1]==point[1][1]) return 5;
			point[2][0]=fabs(params[4]);
			Geo_LineNormal(point[0],point[1],front);
			front[2]=(double)sign(params[4]); }
		else if(dim==3) {
			point[0][0]=params[0];
			point[0][1]=params[1];
			point[0][2]=params[2];
			point[1][0]=params[3];
			point[1][1]=params[4];
			point[1][2]=params[5];
			if(point[0][0]==point[1][0] && point[0][1]==point[1][1] && point[0][2]==point[1][2]) return 5;
			point[2][0]=fabs(params[6]);
			point[2][1]=params[7];
			if(point[2][1]<=0) return 4;
			point[2][2]=params[8];
			if(point[2][2]<=0) return 4;
			front[2]=(double)sign(params[6]); }}
	
	else if(ps==PShemi) {								// hemisphere
		if(dim==2) {
			point[0][0]=params[0];
			point[0][1]=params[1];
			point[1][0]=fabs(params[2]);
			point[1][1]=params[5];
			if(point[1][1]<=0) return 4;
			point[2][0]=params[3];
			point[2][1]=params[4];
			if(normalizeVD(point[2],2)<=0) return 6;
			front[0]=(double)sign(params[2]); }
		else if(dim==3) {
			point[0][0]=params[0];
			point[0][1]=params[1];
			point[0][2]=params[2];
			point[1][0]=fabs(params[3]);
			point[1][1]=params[7];
			if(point[1][1]<=0) return 4;
			point[1][2]=params[8];
			if(point[1][2]<=0) return 4;
			point[2][0]=params[4];
			point[2][1]=params[5];
			point[2][2]=params[6];
			if(normalizeVD(point[2],3)<=0) return 6;
			front[0]=(double)sign(params[3]); }}
	
	else if(ps==PSdisk) {								// disk
		if(dim==2) {
			point[0][0]=params[0];
			point[0][1]=params[1];
			point[1][0]=params[2];
			if(point[1][0]<=0) return 7;
			front[0]=params[3];
			front[1]=params[4];
			if(normalizeVD(front,2)<=0) return 8; }
		else if(dim==3) {
			point[0][0]=params[0];
			point[0][1]=params[1];
			point[0][2]=params[2];
			point[1][0]=params[3];
			if(point[1][0]<=0) return 7;
			point[1][1]=params[7];
			if(point[1][1]<=0) return 4;
			front[0]=params[4];
			front[1]=params[5];
			front[2]=params[6];
			if(normalizeVD(front,3)<=0) return 8; }}
	
	p=-1;
	if(name && name[0]!='\0') {
		for(ps2=(PanelShape)0;ps2<PSMAX && p==-1;ps2=(PanelShape)(ps2+1))
			p=stringfind(srf->pname[ps2],srf->npanel[ps2],name);
		if(p!=-1 && ps2!=ps) return 9; }					// panel name was used before

	if(p==-1) {																	// new panel
		if(srf->npanel[ps]==srf->maxpanel[ps]) {
			ok=panelsalloc(srf,dim,srf->maxpanel[ps]*2+1,srf->srfss->maxspecies,ps);
			if(!ok) return -1; }
		p=srf->npanel[ps]++; }

	pnl=srf->panels[ps][p];
	for(pt=0;pt<pnl->npts;pt++)
		for(d=0;d<dim;d++)
			pnl->point[pt][d]=point[pt][d];
	for(d=0;d<3;d++)
		pnl->front[d]=front[d];
	if(name && name[0]!='\0') strcpy(srf->pname[ps][p],name);

	surfsetcondition(srf->srfss,SClists,0);
	boxsetcondition(srf->srfss->sim->boxs,SCparams,0);
	compartsetcondition(srf->srfss->sim->cmptss,SCparams,0);
	
	return 0; }


/* surfsetemitterabsorption */
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


/* surfsetjumppanel */
int surfsetjumppanel(surfaceptr srf,panelptr pnl1,enum PanelFace face1,int bidirect,panelptr pnl2,enum PanelFace face2) {
	if(!srf) return 1;
	if(!pnl1) return 2;
	if(face1!=PFfront && face1!=PFback) return 3;
	if(bidirect!=0 && bidirect!=1) return 4;
	if(!pnl2 || pnl2->ps!=pnl1->ps) return 5;
	if(face2!=PFfront && face2!=PFback) return 6;

	pnl1->jumpp[face1]=pnl2;
	pnl1->jumpf[face1]=face2;
	if(bidirect) {
		pnl2->jumpp[face2]=pnl1;
		pnl2->jumpf[face2]=face1; }
	return 0; }


/* srfcalcrate */
double srfcalcrate(simptr sim,surfaceptr srf,int i,enum MolecState ms1,enum PanelFace face,enum MolecState ms2) {
	double rate,prob,probrev,sum,dt,difc;
	enum MolecState ms3,ms4,ms5;
	surfactionptr actdetails;
	enum PanelFace face2;
	
	if(ms1==MSsoln && face==PFnone) return -1;				// impossible situation
	if(srf->action[i][ms1][face]!=SAmult) return -1;	// could be computed but isn't
	actdetails=srf->actdetails[i][ms1][face];
	if(!actdetails) return -1;												// no data available
	prob=actdetails->srfprob[ms2];
	if(prob<0 || prob>1) return -2;										// impossible probability
	if(prob==0) return 0;															// rate is 0

	srfreverseaction(ms1,face,ms2,&ms3,&face2,&ms4);	// find probability of reverse action
	if(face2==PFboth) probrev=0;
	else if(srf->actdetails[i][ms3][face2])
		probrev=srf->actdetails[i][ms3][face2]->srfprob[ms4];
	else
		probrev=0;
	if(probrev<0) probrev=0;

	dt=sim->dt;
	difc=sim->mols->difc[i][MSsoln];

	sum=0;
	if(ms1!=MSsoln && face==PFnone) {							// if surface-bound, find sum of probabilities
		for(ms5=(MolecState)0;ms5<MSMAX1;ms5=(MolecState)(ms5+1)) {
			if(ms5!=ms1 && actdetails->srfprob[ms5]>=0)
				sum+=actdetails->srfprob[ms5]; }}

	if(ms1==MSsoln) {															// find rates
		if((face==PFfront && ms2==MSsoln) || (face==PFback && ms2==MSbsoln))
			rate=-3;																			// solution to solution (reflect), can't compute
		else if(ms2==MSsoln || ms2==MSbsoln) {					// solution to solution (transmit)
			if(probrev<=0) rate=surfacerate(prob,0,dt,difc,NULL,SPAirrTrans);
			else rate=surfacerate(prob,probrev,dt,difc,NULL,SPArevTrans); }
		else {																					// solution to surface (adsorb)
			if(probrev<=0) rate=surfacerate(prob,0,dt,difc,NULL,SPAirrAds);
			else rate=surfacerate(prob,probrev,dt,difc,NULL,SPArevAds); }}

	else {
		if(face==PFnone) {
			if(ms2==MSsoln || ms2==MSbsoln) {						// surface to solution (desorb)
				if(probrev<=0) rate=surfacerate(prob,sum,dt,difc,NULL,SPAirrDes);
				else rate=surfacerate(prob,probrev,dt,difc,NULL,SPArevDes); }
			else {																			// surface to surface (flip)
				if(ms1==ms2) rate=-3;											// same state
				else if(probrev<=0) rate=surfacerate(prob,sum,dt,difc,NULL,SPAirrFlip);
				else rate=surfacerate(prob,probrev,dt,difc,NULL,SPArevFlip); }}
		else {
			if((face==PFfront && ms2==MSsoln) || (face==PFback && ms2==MSbsoln))
				rate=-3;																	// surface-bound reflect, can't compute
			else if(ms2==MSsoln || ms2==MSbsoln) {			// surface-bound transmit
				if(probrev<=0) rate=surfacerate(prob,0,dt,difc,NULL,SPAirrTrans);
				else rate=surfacerate(prob,probrev,dt,difc,NULL,SPArevTrans); }
			else {																			// surface-bound hop to new surface
				rate=surfacerate(prob,0,dt,difc,NULL,SPAirrAds); }}}
	
	return rate;
	return 0; }


/* srfcalcprob */
double srfcalcprob(simptr sim,surfaceptr srf,int i,enum MolecState ms1,enum PanelFace face,enum MolecState ms2) {
	double rate,prob,raterev,sum,dt,difc;
	enum MolecState ms3,ms4,ms5;
	surfactionptr actdetails;
	enum PanelFace face2;

	if(ms1==MSsoln && face==PFnone) return 0;					// impossible situation
	if(srf->action[i][ms1][face]!=SAmult) return -1;	// could be computed but isn't
	actdetails=srf->actdetails[i][ms1][face];
	if(!actdetails) return -1;												// no data available
	if(actdetails->srfdatasrc[ms2]==2) return actdetails->srfprob[ms2];
	rate=actdetails->srfrate[ms2];
	if(rate<0) return -2;															// impossible rate
	if(rate==0) return 0;															// prob is 0

	srfreverseaction(ms1,face,ms2,&ms3,&face2,&ms4);	// find rate of reverse action
	if(face2==PFboth) raterev=0;
	else if(srf->actdetails[i][ms3][face2])
		raterev=srf->actdetails[i][ms3][face2]->srfrate[ms4];
	else
		raterev=0;
	if(raterev<0) raterev=0;

	dt=sim->dt;
	difc=sim->mols->difc[i][MSsoln];

	sum=0;
	if(ms1!=MSsoln && face==PFnone) {									// if surface-bound, find sum of rates
		for(ms5=(MolecState)0;ms5<MSMAX1;ms5=(MolecState)(ms5+1)) {
			if(ms5!=ms1 && actdetails->srfrate[ms5]>=0)
				sum+=actdetails->srfrate[ms5]; }}

	if(ms1==MSsoln) {																	// find probs
		if((face==PFfront && ms2==MSsoln) || (face==PFback && ms2==MSbsoln))
			prob=0;																				// solution to solution (reflect), can't compute
		else if(ms2==MSsoln || ms2==MSbsoln) {					// solution to solution (transmit)
			if(raterev<=0) prob=surfaceprob(rate,0,dt,difc,NULL,SPAirrTrans);
			else prob=surfaceprob(rate,raterev,dt,difc,NULL,SPArevTrans); }
		else {																					// solution to surface (adsorb)
			if(raterev<=0) prob=surfaceprob(rate,0,dt,difc,NULL,SPAirrAds);
			else prob=surfaceprob(rate,raterev,dt,difc,NULL,SPArevAds); }}

	else {
		if(face==PFnone) {
			if(ms2==MSsoln || ms2==MSbsoln) {						// surface to solution (desorb)
				if(raterev<=0) prob=surfaceprob(rate,sum,dt,difc,NULL,SPAirrDes);
				else prob=surfaceprob(rate,raterev,dt,difc,NULL,SPArevDes); }
			else {																			// surface to surface (flip)
				if(ms1==ms2) prob=0;											// same state
				else if(raterev<=0) prob=surfaceprob(rate,sum,dt,difc,NULL,SPAirrFlip);
				else prob=surfaceprob(rate,raterev,dt,difc,NULL,SPArevFlip); }}
		else {
			if((face==PFfront && ms2==MSsoln) || (face==PFback && ms2==MSbsoln))
				prob=0;																	// surface-bound reflect, can't compute
			else if(ms2==MSsoln || ms2==MSbsoln) {			// surface-bound transmit
				if(raterev<=0) prob=surfaceprob(rate,0,dt,difc,NULL,SPAirrTrans);
				else prob=surfaceprob(rate,raterev,dt,difc,NULL,SPArevTrans); }
			else {																			// surface-bound hop to new surface
				prob=surfaceprob(rate,0,dt,difc,NULL,SPAirrAds); }}}
	
	return prob; }


/* surfsetneighbors */
int surfsetneighbors(panelptr pnl,panelptr *neighlist,int nneigh,int add) {
	int newmaxneigh,p,p2;
	panelptr *newneigh;

	if(add) {
		if(pnl->nneigh+nneigh>pnl->maxneigh) { // allocate more space
			newmaxneigh=pnl->nneigh+nneigh;
			newneigh=(panelptr*) calloc(newmaxneigh,sizeof(panelptr));
			if(!newneigh) return 1;
			for(p2=0;p2<pnl->nneigh;p2++) newneigh[p2]=pnl->neigh[p2];
			for(;p2<newmaxneigh;p2++) newneigh[p2]=NULL;
			free(pnl->neigh);
			pnl->maxneigh=newmaxneigh;
			pnl->neigh=newneigh; }
		for(p=0;p<nneigh;p++) {
			for(p2=0;p2<pnl->nneigh && pnl->neigh[p2]!=neighlist[p];p2++);
			if(p2==pnl->nneigh)
				pnl->neigh[pnl->nneigh++]=neighlist[p]; }}
	else if(!neighlist) {
		pnl->nneigh=0; }
	else {
		for(p=0;p<nneigh;p++) {
			for(p2=0;p2<pnl->nneigh && pnl->neigh[p2]!=neighlist[p];p2++);
			if(p2<pnl->nneigh)
				pnl->neigh[p2]=pnl->neigh[--pnl->nneigh]; }}

	return 0; }


/* surfaddemitter */
int surfaddemitter(surfaceptr srf,enum PanelFace face,int i,double amount,double *pos,int dim) {
	int er,oldmax,newmax,emit,d;
	double *newamount,**newpos;

	newamount=NULL;
	newpos=NULL;
	if(!srf->maxemitter[face]) {
		er=emittersalloc(srf,face,srf->srfss->maxspecies,srf->srfss->maxspecies);
		CHECK(!er); }

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



/* surfreadstring */
surfaceptr surfreadstring(simptr sim,surfaceptr srf,char *word,char *line2,char *erstr) {
	char nm[STRCHAR],nm1[STRCHAR],nm2[STRCHAR],facenm[STRCHAR],shapenm[STRCHAR],actnm[STRCHAR],*chptr;
	int dim,i,p,p2,i1,i3,itct,s2,er;
	unsigned int ui1,ui2;
	const int maxpnllist=128;
	panelptr pnl,pnllist[128];
	double fltv1[9],f1;
	surfacessptr srfss;
	surfaceptr srf2;
	enum PanelShape ps;
	enum PanelFace face,face2;
	enum SrfAction act;
	enum MolecState ms,ms1,ms2;
	enum DrawMode dm;

	dim=sim->dim;
	srfss=sim->srfss;

	if(!strcmp(word,"name")) {								// name
		itct=sscanf(line2,"%s",nm);
		CHECKS(itct==1,"error reading surface name");
		srf=surfaddsurface(sim,nm);
		CHECKS(srf,"failed to add surface");
		CHECKS(!strnword(line2,2),"unexpected text following name"); }

	else if(!strcmp(word,"action")) {							// action
		CHECKS(srf,"need to enter surface name before action");
		CHECKS(srfss->maxspecies,"need to enter molecules before action");
		itct=sscanf(line2,"%s %s %s",nm1,nm2,actnm);
		CHECKS(itct==3,"action format: species[(state)] face action");
		i=readmolname(sim,nm1,&ms1);
		face=surfstring2face(nm2);
		if(face==PFnone || (i<=0 && i>-5)) {	// try old format
			face=surfstring2face(nm1);
			i=readmolname(sim,nm2,&ms1);
			if(face==PFnone && (i<=0 && i>-5)) {	// back to new format
				i=readmolname(sim,nm1,&ms1);
				face=surfstring2face(nm2);
				CHECKS(face!=PFnone,"in action, face name needs to be 'front', 'back', or 'both'");
				CHECKS(i>0 || i==-5,"in action, molecule name not recognized"); }
			CHECKS(face!=PFnone,"in action, face name needs to be 'front', 'back', or 'both'");
			CHECKS(i>0 || i==-5,"in action, molecule name not recognized"); }
		act=surfstring2act(actnm);
		CHECKS(act<=SAmult || act==SAadsorb,"in action statement, action not recognized or not permitted");
		er=surfsetaction(srf,i,ms1,face,act);
		CHECKS(!er,"BUG: error with surfsetaction statement");
		CHECKS(!strnword(line2,4),"unexpected text following action"); }

	else if(!strcmp(word,"rate") || !strcmp(word,"rate_internal")) { // rate, rate_internal
		CHECKS(srf,"need to enter surface name first");
		CHECKS(srfss->maxspecies,"need to enter molecules first");
		
#ifdef VCELL_HYBRID
		bool constRate = true;

		stringstream ss(line2);
		ss >> nm >> nm1 >> nm2;
		string rawStr;
		getline(ss, rawStr);
		size_t found = rawStr.find(";");
		string rateExpStr = rawStr.substr(0, found);
		Expression* srfRateExp = new Expression(rateExpStr);
		try {
			double rate = srfRateExp->evaluateConstant();
			f1=rate;
			constRate = true;			
		} catch (...) {
			constRate = false;
		}
#else
		itct=sscanf(line2,"%s %s %s %lg",nm,nm1,nm2,&f1);
		CHECKS(itct==4,"format: species[(state)] state1 state2 value [new_species]");
#endif
		i=readmolname(sim,nm,&ms);
		CHECKS(i>0 || i==-5,"molecule name not recognized");
		CHECKS(ms<MSbsoln,"state is not recognized or not permitted");
		ms1=molstring2ms(nm1);
		CHECKS(ms1<=MSbsoln,"state1 is not recognized or not permitted");
		ms2=molstring2ms(nm2);
		CHECKS(ms2<=MSbsoln,"state2 is not recognized or not permitted");
		if(ms==MSsoln) {
			CHECKS(ms1!=ms2,"it is not permitted for state1 to equal state2"); }
		else {
			CHECKS(ms1==ms || ms1==MSsoln || ms1==MSbsoln,"state1 does not make sense"); }
#ifdef VCELL_HYBRID
		if(constRate == true)
			CHECKS(f1>=0,"negative surface rate values are not permitted");
		string name = rawStr.substr(found+2); //after the ";" denoting the end of rate, the found move one more position(the space) to get to the end of the line, which would be the species name
		const char* anotherMolName = name.c_str();
		line2 = const_cast<char *>(anotherMolName); 
#else
		CHECKS(f1>=0,"negative surface rate values are not permitted");
		line2=strnword(line2,5);
#endif
		i3=i;
		if(line2) {
			itct=sscanf(line2,"%s",nm);
			CHECKS(itct==1,"cannot read new species name");
			i3=stringfind(sim->mols->spname,sim->mols->nspecies,nm);
			CHECKS(i3!=-1,"new species name not recognized");
			line2=strnword(line2,2); }
#ifdef VCELL_HYBRID
		if(constRate == true)
		{
			if(!strcmp(word,"rate"))
				er=surfsetrate(srf,i,ms,ms1,ms2,i3,f1,1);
			else {
				CHECKS(f1<=1,"surface interaction probabilities cannot be greater than 1");
				er=surfsetrate(srf,i,ms,ms1,ms2,i3,f1,2); }
			CHECKS(er!=-1,"out of memory");
			CHECKS(!er,"BUG: error in surfsetrate");
		}
		else //rate is a function
		{
			if(!strcmp(word,"rate"))
				er=surfSetRateExp(srf, i, ms, ms1, ms2, i3, srfRateExp,1);
			else {
				CHECKS(f1<=1,"surface interaction probabilities cannot be greater than 1");
				er=surfSetRateExp(srf, i, ms, ms1, ms2, i3, srfRateExp, 2); }
			CHECKS(er!=-1,"out of memory");
			CHECKS(!er,"BUG: error in surfsetrate");
		}
#else
		if(!strcmp(word,"rate"))
			er=surfsetrate(srf,i,ms,ms1,ms2,i3,f1,1);
		else {
			CHECKS(f1<=1,"surface interaction probabilities cannot be greater than 1");
			er=surfsetrate(srf,i,ms,ms1,ms2,i3,f1,2); }
		CHECKS(er!=-1,"out of memory");
		CHECKS(!er,"BUG: error in surfsetrate");
#endif
		CHECKS(!line2,"unexpected text at end of line");}

	else if(!strcmp(word,"color") || !strcmp(word,"colour")) {		// color
		CHECKS(srf,"need to enter surface name before color");
		itct=sscanf(line2,"%s",facenm);
		CHECKS(itct==1,"color format: face color");
		face=surfstring2face(facenm);
		CHECKS(face!=PFnone,"in color, face name needs to be 'front', 'back', or 'both'");
		line2=strnword(line2,2);
		CHECKS(line2,"color format: face color");
		er=graphicsreadcolor(&line2,fltv1);
		CHECKS(er!=3,"color values need to be between 0 and 1");
		CHECKS(er!=4,"color name not recognized");
		CHECKS(er!=6,"alpha values need to be between 0 and 1");
		CHECKS(er==0,"format is either 3 numbers or color name, and then optional alpha value");
		er=surfsetcolor(srf,face,fltv1);
		CHECKS(!er,"BUG: error in surfsetcolor");
		CHECKS(!line2,"unexpected text following color"); }

	else if(!strcmp(word,"thickness")) {				// thickness
		CHECKS(srf,"need to enter surface name before thickness");
		itct=sscanf(line2,"%lg",&f1);
		CHECKS(itct==1,"thickness value is missing");
		CHECKS(f1>=0,"thickness value needs to be at least 0");
		er=surfsetedgepts(srf,f1);
		CHECKS(!er,"BUG: error in surfsetedgepts");
		CHECKS(!strnword(line2,2),"unexpected text following thickness"); }

	else if(!strcmp(word,"stipple")) {					// stipple
		CHECKS(srf,"need to enter surface name before stipple");
		itct=sscanf(line2,"%u %x",&ui1,&ui2);
		CHECKS(itct==2,"stipple format: factor pattern");
		CHECKS(ui1>=1,"stipple factor needs to be >=1");
		CHECKS(ui2>=0 && ui2 <=0xFFFF,"stipple pattern needs to be between 0x00 and 0xFFFF");
		er=surfsetstipple(srf,ui1,ui2);
		CHECKS(!er,"BUG: error in surfsetstipple");
		CHECKS(!strnword(line2,3),"unexpected text following stipple"); }

	else if(!strcmp(word,"polygon")) {					// polygon
		CHECKS(srf,"need to enter surface name before polygon");
		itct=sscanf(line2,"%s %s",facenm,nm1);
		CHECKS(itct==2,"polygon format: face drawmode");
		face=surfstring2face(facenm);
		CHECKS(face!=PFnone,"in polygon, face name needs to be 'front', 'back', or 'both'");
		dm=surfstring2dm(nm1);
		CHECKS(dm!=DMnone,"in polygon, drawing mode is not recognized");
		er=surfsetdrawmode(srf,face,dm);
		CHECKS(!er,"BUG: error in surfsetdrawmode");
		CHECKS(!strnword(line2,3),"unexpected text following polygon"); }

	else if(!strcmp(word,"shininess")) {				// shininess
		CHECKS(srf,"need to enter surface name before shininess");
		itct=sscanf(line2,"%s %lg",facenm,&f1);
		CHECKS(itct==2,"shininess format: face value");
		face=surfstring2face(facenm);
		CHECKS(face!=PFnone,"face name needs to be 'front', 'back', or 'both'");
		CHECKS(f1>=0 && f1<=128,"shininess value needs to be between 0 and 128");
		er=surfsetshiny(srf,face,f1);
		CHECKS(!er,"BUG: error in surfsetshiny");
		CHECKS(!strnword(line2,3),"unexpected text following shininess"); }

	else if(!strcmp(word,"max_panels")) {					// max_panels
		CHECKS(srf,"need to enter surface name before max_panels");
		itct=sscanf(line2,"%s %i",shapenm,&i1);
		CHECKS(itct==2,"max_panels format: shape number");
		ps=surfstring2ps(shapenm);
		CHECKS(ps<PSMAX,"in max_panels, unknown panel shape");
		CHECKS(dim!=1 || ps<=PSsph,"in max_panels, panel shape is not permitted for a 1-D system");
		CHECKS(i1>=0,"max_panels number needs to be at least 0");
		er=surfsetmaxpanel(srf,dim,ps,i1);
		CHECKS(er!=-1,"out of memory allocating panels");
		CHECKS(!er,"BUG: error in surfsetmaxpanel");
		CHECKS(!strnword(line2,3),"unexpected text following max_panels"); }

	else if(!strcmp(word,"panel")) {							// panel
		CHECKS(srf,"need to enter surface name before panel");
		itct=sscanf(line2,"%s",shapenm);
		CHECKS(itct==1,"in panel, panel shape needs to be entered");
		ps=surfstring2ps(shapenm);
		CHECKS(ps<PSMAX,"in panel, unknown panel shape");
		CHECKS(dim!=1 || ps<=PSsph,"in panel, panel shape is not permitted for a 1-D system");
		line2=strnword(line2,2);
		CHECKS(line2,"panel data missing");
		if(ps==PSrect) {														// panel r ...
			itct=sscanf(line2,"%s",nm1);
			CHECKS(itct==1,"error reading rectangle panel direction");
			line2=strnword(line2,2);
			CHECKS(line2,"panel data missing"); }
		i1=surfpanelparams(ps,dim);
		itct=strreadnd(line2,i1,fltv1,NULL);
		CHECKS(itct==i1,"panel either has too few parameters or has unreadable parameters");
		line2=strnword(line2,i1+1);
		if(line2) {
			itct=sscanf(line2,"%s",nm);
			CHECKS(itct==1,"Error reading panel name");
			line2=strnword(line2,2); }
		else
			nm[0]='\0';
		er=surfaddpanel(srf,dim,ps,nm1,fltv1,nm);
		CHECKS(er!=-1,"out of memory adding panel");
		CHECKS(er!=1,"BUG: error in surfaddpanel");
		CHECKS(er!=2,"BUG: error in surfaddpanel");
		CHECKS(er!=3,"unable to read rectangle direction");
		CHECKS(er!=4,"drawing slices and stacks need to be positive numbers");
		CHECKS(er!=5,"cylinder ends need to be at different locations");
		CHECKS(er!=6,"outward pointing vector cannot be 0 length");
		CHECKS(er!=7,"radius needs to be positive");
		CHECKS(er!=8,"normal vector cannot be 0 length");
		CHECKS(er!=9,"panel name was used previously for a different panel shape");

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
		er=surfsetjumppanel(srf,srf->panels[ps][p],face,i1,srf->panels[ps][p2],face2);
		CHECKS(!er,"BUG: error in surfsetjumppanel");
		CHECKS(!strnword(line2,3),"unexpected text following jump"); }

	else if(!strcmp(word,"neighbors")) {					// neighbors
		CHECKS(srf,"need to enter surface name before neighbors");
		itct=sscanf(line2,"%s",nm);
		CHECKS(itct==1,"format for neighbors: panel neigh1 neigh2 ...");
		p = -1;
		ps = (PanelShape)0;
		if(strstr(nm, "tri_") == nm)//find nm starts with "tri_"
		{
			ps=PStri;
			int memIndex;
			int globalIndex;
			sscanf(nm, "tri_%d_%d_%d", &p, &globalIndex, &memIndex);

			/*int oldp = -1;
			PanelShape oldps = (PanelShape)0;
			while(oldps<PSMAX && (oldp=stringfind(srf->pname[oldps],srf->npanel[oldps],nm))==-1) oldps=(PanelShape)(oldps+1);
			if (oldp != p || oldps != ps) {
				throw "oldp != p || oldps != ps";
			}*/
		}
		if(p == -1)
		{
			while(ps<PSMAX && (p=stringfind(srf->pname[ps],srf->npanel[ps],nm))==-1) ps=(PanelShape)(ps+1);
		}
		CHECKS(p>=0,"first panel name listed in neighbors is not recognized");
		pnl=srf->panels[ps][p];
		for(i1=0;i1<maxpnllist && (line2=strnword(line2,2));i1++) {
			itct=sscanf(line2,"%s",nm);
			CHECKS(itct==1,"format for neighbors: panel neigh1 neigh2 ...");
			p = -1;
			ps = (PanelShape)0;
			if(strstr(nm, "tri_") == nm)//find nm starts with "tri_"
			{
				ps=PStri;
				srf2=srf;
				int memIndex;
				int globalIndex;
				sscanf(nm, "tri_%d_%d_%d", &p, &globalIndex, &memIndex);

				/*if(strchr(nm,':')) {
					chptr=strchr(nm,':')+1;
					*(chptr-1)='\0';
					s2=stringfind(srfss->snames,srfss->nsrf,nm);
					CHECKS(s2>=0,"surface name is not recognized");
					srf2=srfss->srflist[s2]; }
				else {
					chptr=nm;
					srf2=srf; }
				int oldp=oldps=(PanelShape)0;
				while(oldps<PSMAX && (oldp=stringfind(srf2->pname[oldps],srf2->npanel[oldps],chptr))==-1) oldps=(PanelShape)(oldps+1);
				if (oldp != p || oldps != ps) {
					throw "oldp != p || oldps != ps";
				}*/
			}
			if(p == -1)
			{
				if(strchr(nm,':')) {
					chptr=strchr(nm,':')+1;
					*(chptr-1)='\0';
					s2=stringfind(srfss->snames,srfss->nsrf,nm);
					string nmStr = nm;
					string msg = "surface name:" + nmStr + " is not recognized";
					CHECKS(s2>=0, msg.c_str());
					srf2=srfss->srflist[s2]; }
				else {
					chptr=nm;
					srf2=srf; }
				p=ps=(PanelShape)0;
				while(ps<PSMAX && (p=stringfind(srf2->pname[ps],srf2->npanel[ps],chptr))==-1) ps=(PanelShape)(ps+1);
			}
			CHECKS(p>=0,"a neighbor panel name is not recognized");
			pnllist[i1]=srf2->panels[ps][p]; }
		CHECKS(i1<maxpnllist,"too many neighbor panels listed in one line");
		er=surfsetneighbors(pnl,pnllist,i1,1);
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

	return srf;

 failure:
	return NULL; }


/* loadsurface */
int loadsurface(simptr sim,ParseFilePtr *pfpptr,char *line2,char *erstr) {
	ParseFilePtr pfp;
	char word[STRCHAR],errstring[STRCHAR];
	int done,pfpcode,firstline2;
	surfaceptr srf;

	pfp=*pfpptr;
	done=0;
	srf=NULL;
	firstline2=line2?1:0;

	while(!done) {
		if(pfp->lctr==0 && !strchr(sim->flags,'q'))
			printf(" Reading file: '%s'\n",pfp->fname);
		if(firstline2) {
			strcpy(word,"name");
			pfpcode=1;
			firstline2=0; }
		else
			pfpcode=Parse_ReadLine(&pfp,word,&line2,errstring);
		*pfpptr=pfp;
		CHECKS(pfpcode!=3,errstring);

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
			srf=surfreadstring(sim,srf,word,line2,errstring);
			CHECKS(srf!=NULL,errstring); }}

	CHECKS(0,"end of file encountered before end_surface statement");	// end of file

 failure:																					// failure
	return 1; }


/* surfupdateparams */
int surfupdateparams(simptr sim) {
	surfacessptr srfss;
	surfaceptr srf;
	int nspecies,i,s,er;
	double dt,**difc,sum,**difstep,difstepmax;
	enum MolecState ms1,ms2,ms3,ms4;
	enum PanelFace face,face2;
	surfactionptr actdetails;
	
	srfss=sim->srfss;
	
	if(sim->mols) {
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
			for(i=1;i<nspecies;i++)
				for(ms1=(MolecState)0;ms1<MSMAX;ms1=(MolecState)(ms1+1))
					for(face=(PanelFace)0;face<3;face=(PanelFace)(face+1))
						if(srf->action[i][ms1][face]==SAmult && srf->actdetails[i][ms1][face]) {
							actdetails=srf->actdetails[i][ms1][face];
							for(ms2=(MolecState)0;ms2<MSMAX1;ms2=(MolecState)(ms2+1))							// record actual probability of each event
								actdetails->srfprob[ms2]=srfcalcprob(sim,srf,i,ms1,face,ms2); }

			for(i=1;i<nspecies;i++)
				for(ms1=(MolecState)0;ms1<MSMAX;ms1=(MolecState)(ms1+1))
					for(face=(PanelFace)0;face<3;face=(PanelFace)(face+1)) {
						if(srf->action[i][ms1][face]==SAmult && srf->actdetails[i][ms1][face]) {
							actdetails=srf->actdetails[i][ms1][face];
							sum=0;
							for(ms2=(MolecState)0;ms2<MSMAX1;ms2=(MolecState)(ms2+1))							// find ms1->ms1 probability
								if(!srfsamestate(ms1,face,ms2,NULL)) sum+=actdetails->srfprob[ms2];
							if(sum>1.0) {
								actdetails->srfprob[ms1]=0;
								for(ms2=(MolecState)0;ms2<MSMAX1;ms2=(MolecState)(ms2+1))
									actdetails->srfprob[ms2]/=sum; }
							else {
								srfsamestate(ms1,face,MSsoln,&ms3);
								actdetails->srfprob[ms3]=1.0-sum; }

							for(ms2=(MolecState)0;ms2<MSMAX1;ms2=(MolecState)(ms2+1)) {						// record reverse probabilities
								srfreverseaction(ms1,face,ms2,&ms3,&face2,&ms4);
								if(face2!=PFboth && srf->actdetails[i][ms3][face2]) {
									actdetails->srfrevprob[ms2]=srf->actdetails[i][ms3][face2]->srfprob[ms4]; }}

							sum=0;																	// find cumulative probability
							for(ms2=(MolecState)0;ms2<MSMAX1;ms2=(MolecState)(ms2+1)) {
								sum+=actdetails->srfprob[ms2];
								actdetails->srfcumprob[ms2]=sum; }}}}
	
		er=surfsetemitterabsorption(sim);
		if(er)
			printf("WARNING: an unbounded emitter is at a surface panel, which will cause inaccurate operation\n"); }
	
	return 0; }


/* surfupdatelists */
int surfupdatelists(simptr sim) {
	surfacessptr srfss;
	surfaceptr srf;
	int i,ll,maxmollist,s,totpanel,pindex,p;
	enum MolecState ms;
	enum SMLflag *newsrfmollist;
	double totarea,*areatable,area;
	panelptr *paneltable;
	enum PanelShape ps;

	srfss=sim->srfss;

	if(sim->mols) {
		if(sim->mols->condition<=SClists) return 2;

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
						srfss->srfmollist[ll] = (SMLflag)(srfss->srfmollist[ll] | SMLdiffuse);
					if(rxnisprod(sim,i,ms,1))
						srfss->srfmollist[ll] = (SMLflag)(srfss->srfmollist[ll] | SMLreact);
					if(ms!=MSsoln)
						srfss->srfmollist[ll] = (SMLflag)(srfss->srfmollist[ll] | SMLsrfbound); }}

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
				srf->paneltable=paneltable; }}}

	return 0; }


/* surfupdate */
int surfupdate(simptr sim) {
	int er;
	surfacessptr srfss;

	srfss=sim->srfss;
	if(srfss) {
		if(srfss->condition<=SClists) {
			er=surfupdatelists(sim);
			if(er) return er;
			surfsetcondition(srfss,SCparams,1); }
		if(srfss->condition==SCparams) {
			er=surfupdateparams(sim);
			if(er) return er;
			surfsetcondition(srfss,SCok,1); }}
	return 0; }


/******************************************************************************/
/************************* core simulation functions **************************/
/******************************************************************************/


/* panelside */
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


/* panelnormal */
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


/* lineXpanel */
int lineXpanel(double *pt1,double *pt2,panelptr pnl,int dim,double *crsspt,enum PanelFace *face1ptr,enum PanelFace *face2ptr,double *crossptr,double *cross2ptr,int *veryclose) {
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
			//intsct=((point[0][0]<=crsspt[0] && crsspt[0]<=point[1][0]) || (point[1][0]<=crsspt[0] && crsspt[0]<=point[0][0]));
			//intsct=intsct && ((point[0][1]<=crsspt[1] && crsspt[1]<=point[1][1]) || (point[1][1]<=crsspt[1] && crsspt[1]<=point[0][1])); }
			double v1[2] = {point[1][0] - point[0][0], point[1][1] - point[0][1]};
			double v2[2] = {crsspt[0] - point[0][0], crsspt[1] - point[0][1]};
			intsct = v1[0] * v2[0] + v1[1] * v2[1] >= 0;
			double v3[2] = {crsspt[0] - point[1][0], crsspt[1] - point[1][1]};
			intsct = intsct && (v1[0] * v3[0] + v1[1] * v3[1] <= 0); 
		}
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
	if(veryclose) {
		*veryclose=0;
		if(dist1<VERYCLOSE) *veryclose+=1;
		if(dist2<VERYCLOSE) *veryclose+=2; }
	return intsct; }


/* ptinpanel */
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


/* surfaction */
enum SrfAction surfaction(surfaceptr srf,enum PanelFace face,int ident,enum MolecState ms,int *i2ptr,enum MolecState *ms2ptr) {
	enum SrfAction act;
	enum MolecState ms2,ms3;
	double r,*srfcumprob;
	int i2;
	surfactionptr actdetails;

	i2=ident;
	ms2=ms;
	act=srf->action[ident][ms][face];
	
	if(act==SAmult) {
		actdetails=srf->actdetails[ident][ms][face];
		srfcumprob=actdetails->srfcumprob;
		r=randCOD();
		ms2=MSnone;
		for(ms3=(MolecState)0;ms3<MSMAX1 && ms2==MSnone;ms3=(MolecState)(ms3+1))
			if(r<srfcumprob[ms3]) ms2=ms3;
		i2=actdetails->srfnewspec[ms2];

		if(i2==0) act=SAabsorb;
		else if(ms==MSsoln) {
			if(face==PFfront) {															// solution, front
				if(ms2==MSsoln) act=SAreflect;
				else if(ms2==MSbsoln) act=SAtrans;
				else act=SAadsorb; }
			else {																					// solution, back
				if(ms2==MSsoln) act=SAtrans;
				else if(ms2==MSbsoln) act=SAreflect;
				else act=SAadsorb; }}
		else {
			if(face==PFnone) {															// bound, none
				if(ms2==ms) act=SAno;
				else if(ms2==MSsoln || ms2==MSbsoln) {
					if(actdetails->srfrevprob[ms2]>0) act=SArevdes;
					else act=SAirrevdes; }
				else
					act=SAflip; }
			else if(face==PFfront) {												// bound, front
				if(ms2==MSsoln) act=SAreflect;
				else if(ms2==MSbsoln) act=SAtrans;
				else act=SAadsorb; }
			else {																					// bound, back
				if(ms2==MSsoln) act=SAtrans;
				else if(ms2==MSbsoln) act=SAreflect;
				else act=SAadsorb; }}}

	if(i2ptr) *i2ptr=i2;
	if(ms2ptr) *ms2ptr=ms2;
	return act; }


/* rxnXsurface */
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
			if(lineXpanel(pos1,pos2,pnl,dim,crsspt,&face1,&face2,&cross,&cross2,NULL)) {
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



/* fixpt2panel */
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


/* movept2panel */
void movept2panel(double *pt,panelptr pnl,int dim,double margin) {
	double **point,*front,inpt0[3],inpt1[3],inpt2[3];
	double lo,hi,dot;
	int d;

	point=pnl->point;
	front=pnl->front;

	if(pnl->ps==PSrect) {														// rectangle
		if(dim==1);
		else if(dim==2) {
			d=(int)front[2];
			lo=((point[0][d]<point[1][d])?point[0][d]:point[1][d])+margin;
			hi=((point[0][d]<point[1][d])?point[1][d]:point[0][d])-margin;
			if(pt[d]<lo) pt[d]=lo;
			else if(pt[d]>hi) pt[d]=hi; }
		else {
			d=(int)front[2];
			lo=((point[0][d]<point[1][d])?point[0][d]:point[1][d])+margin;
			hi=((point[0][d]<point[1][d])?point[1][d]:point[0][d])-margin;
			if(pt[d]<lo) pt[d]=lo;
			else if(pt[d]>hi) pt[d]=hi;
			d=(d+1)%3;
			if(d==(int)front[1]) d=(d+1)%3;
			lo=((point[0][d]<point[3][d])?point[0][d]:point[3][d])+margin;
			hi=((point[0][d]<point[3][d])?point[3][d]:point[0][d])-margin;
			if(pt[d]<lo) pt[d]=lo;
			else if(pt[d]>hi) pt[d]=hi; }}
	else if(pnl->ps==PStri) {												// triangle
		if(dim==1);
		else if(dim==2) {
			Geo_InsidePoints2(point[0],point[1],margin,inpt0,inpt1,dim);
			Geo_NearestSlabPt(inpt0,inpt1,pt,pt,dim); }
		else {
			Geo_InsidePoints3(point[0],point[1],point[2],margin,inpt0,inpt1,inpt2);
			Geo_NearestTriPt(inpt0,inpt1,inpt2,front,pt,pt); }}
	else if(pnl->ps==PSsph || pnl->ps==PShemi) {		// sphere, hemisphere
		if(pnl->ps==PSsph);
		else {
			dot=0;
			for(d=0;d<dim;d++) dot+=(pt[d]-point[0][d])*point[2][d];
			if(dot>0)
				for(d=0;d<dim;d++) pt[d]-=dot*point[2][d]+margin; }}
	else if(pnl->ps==PScyl) {												// cylinder
		Geo_InsidePoints2(point[0],point[1],margin,inpt0,inpt1,dim);
		Geo_NearestSlabPt(inpt0,inpt1,pt,pt,dim); }
	else if(pnl->ps==PSdisk)												// disk
		Geo_NearestCylPt(point[0],front,point[1][0]-margin,dim,pt,pt);

	return; }


/* closestpanelpt */
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


/* movemol2closepanel */
void movemol2closepanel(simptr sim,moleculeptr mptr,int dim,double epsilon,double neighdist,double margin) {
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
			if(face!=PFnone && newpnl->srf->action[mptr->ident][MSsoln][face]==SAjump) {	// jump, if required
				jump=surfacejump(mptr,newpnl,mptr->pos,face,dim);
				if(jump) {
					mptr->pnl=newpnl->jumpp[face];
					if(newpnl->jumpf[face]!=face) {
						face=newpnl->jumpf[face];
						molchangeident(sim,mptr,mptr->list,-1,mptr->ident,face==PFfront?MSfront:MSback,mptr->pnl); }}}}
		movept2panel(mptr->pos,mptr->pnl,dim,margin); }

	fixpt2panel(mptr->pos,mptr->pnl,dim,face,epsilon);
	return; }


/* surfacereflect */
void surfacereflect(moleculeptr mptr,panelptr pnl,double *crsspt,int dim,enum PanelFace face) {
	int d,axis;
	double *pos,*front,norm[3],norm2[3],dot;

	pos=mptr->pos;
	front=pnl->front;

	if(mptr->mstate==MSsoln) {
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
				for(d=0;d<dim;d++) pos[d]-=2.0*norm[d]*dot; }}}
	else {																			// reflection for surface-bound molecules
		panelnormal(pnl,crsspt,face,dim,norm);	// norm is normal for collision panel
		panelnormal(mptr->pnl,crsspt,PFfront,dim,norm2);			// norm2 is normal for surface-bound panel
		dot=0;
		for(d=0;d<dim;d++) dot+=norm[d]*norm2[d];
		for(d=0;d<dim;d++) norm[d]-=dot*norm2[d];		// make norm in plane of surface-bound panel
		dot=0;
		for(d=0;d<dim;d++) dot+=norm[d]*norm[d];
		dot=sqrt(dot);
		if(dot==0) dot=1;														// should never happen
		for(d=0;d<dim;d++) norm[d]/=dot;						// normalize norm
		dot=0;
		for(d=0;d<dim;d++) dot+=(pos[d]-crsspt[d])*norm[d];
		for(d=0;d<dim;d++) pos[d]-=2.0*norm[d]*dot; }

	return; }


/* surfacejump */
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
	if(dir==-1) surfacereflect(mptr,pnl2,crsspt,dim,face);
	return 1; }


/* dosurfinteract */
int dosurfinteract(simptr sim,moleculeptr mptr,int ll,int m,panelptr pnl,enum PanelFace face,double *crsspt) {
	int done,dim,d,i,i2,isneigh,p,jump;
	enum PanelFace newface;
	enum MolecState ms,ms2;
	enum SrfAction act;
	double x,norm[DIMMAX],epsilon,neighdist,margin;

	dim=sim->dim;
	done=0;
	i=mptr->ident;
	ms=mptr->mstate;
	epsilon=sim->srfss->epsilon;
	neighdist=sim->srfss->neighdist;
	margin=sim->srfss->margin;

	isneigh=0;																		// check for neighbor
	if(mptr->pnl && pnl->nneigh>0)
		for(p=0;p<mptr->pnl->nneigh;p++)
			if(mptr->pnl->neigh[p]==pnl) isneigh=1;

	if(isneigh) {																	// find action, new identity, and new state
		act=coinrandD(0.5)?SAadsorb:SAtrans;
		i2=i;
		ms2=ms; }
	else if((face==PFfront || face==PFback) && pnl->emitterabsorb[face] && pnl->emitterabsorb[face][i]>0) {
		if(randCCD()<pnl->emitterabsorb[face][i]) act=SAabsorb;
		else act=SAreflect;
		i2=i;
		ms2=MSsoln; }
	else
		act=surfaction(pnl->srf,face,i,ms,&i2,&ms2);
	
	if(act==SAno);																// no action

	else if(act==SAtrans) {												// transmit
		newface=(face==PFfront)?PFback:PFfront;
		fixpt2panel(crsspt,pnl,dim,newface,epsilon);
		if(i2!=i) molchangeident(sim,mptr,ll,m,i2,ms,mptr->pnl); }

	else if(act==SAreflect) {											// reflect
		surfacereflect(mptr,pnl,crsspt,dim,face);
		fixpt2panel(crsspt,pnl,dim,face,epsilon);
		if(ms!=MSsoln) movemol2closepanel(sim,mptr,dim,epsilon,neighdist,margin);
		if(panelside(mptr->pos,pnl,dim,NULL)!=face) fixpt2panel(mptr->pos,pnl,dim,face,0);
		if(i2!=i) molchangeident(sim,mptr,ll,m,i2,ms,mptr->pnl); }

	else if(act==SAabsorb) {											// absorb
		molkill(sim,mptr,ll,m);
		done=1; }

	else if(act==SAjump) {												// jump (just solution-phase molecules)
		jump=surfacejump(mptr,pnl,crsspt,face,dim);
		if(!jump) {				// transmit
			newface=(face==PFfront)?PFback:PFfront;
			fixpt2panel(crsspt,pnl,dim,newface,epsilon); }}

	else if(act==SAport) {												// port
		mptr->list=pnl->srf->port[face]->llport;
		if(m<sim->mols->sortl[ll]) sim->mols->sortl[ll]=m;
		done=1; }

	else if(act==SAadsorb) {											// adsorb
		molchangeident(sim,mptr,ll,m,i2,ms2,pnl);
		for(d=0;d<dim;d++) mptr->pos[d]=crsspt[d];
		if(!ptinpanel(mptr->pos,mptr->pnl,dim))
			movept2panel(mptr->pos,mptr->pnl,dim,margin);
		if(ms2==MSfront) fixpt2panel(mptr->pos,mptr->pnl,dim,PFfront,epsilon);
		else if(ms2==MSback) fixpt2panel(mptr->pos,mptr->pnl,dim,PFback,epsilon);
		done=1; }

	else if(act==SArevdes) {											// reversible desorb
		molchangeident(sim,mptr,ll,m,i2,ms2,pnl);
		for(d=0;d<dim;d++) crsspt[d]=mptr->pos[d];
		fixpt2panel(crsspt,pnl,dim,ms2==MSsoln?PFfront:PFback,epsilon);
		x=desorbdist(sim->mols->difstep[mptr->ident][MSsoln],SPArevAds);
		panelnormal(pnl,mptr->pos,ms2==MSsoln?PFfront:PFback,dim,norm);
		for(d=0;d<dim;d++) mptr->pos[d]+=x*norm[d];
		sim->eventcount[ETdesorb]++; }

	else if(act==SAirrevdes) {										// irreversible desorb
		molchangeident(sim,mptr,ll,m,i2,ms2,pnl);
		for(d=0;d<dim;d++) crsspt[d]=mptr->pos[d];
		fixpt2panel(crsspt,pnl,dim,ms2==MSsoln?PFfront:PFback,epsilon);
		x=desorbdist(sim->mols->difstep[mptr->ident][MSsoln],SPAirrDes);
		panelnormal(pnl,mptr->pos,ms2==MSsoln?PFfront:PFback,dim,norm);
		for(d=0;d<dim;d++) mptr->pos[d]+=x*norm[d];
		sim->eventcount[ETdesorb]++; }

	else if(act==SAflip) {												// on-surface flipping
		molchangeident(sim,mptr,ll,m,i2,ms2,pnl);
		if(ms2==MSfront) fixpt2panel(mptr->pos,mptr->pnl,dim,PFfront,epsilon);
		else if(ms2==MSback) fixpt2panel(mptr->pos,mptr->pnl,dim,PFback,epsilon);
		done=1; }

	return done; }


/* checksurfaces. */
int checksurfaces(simptr sim,int ll,int reborn) {
	int dim,d,nmol,m,done,p,lxp,it,flag;
	boxptr bptr1;
	moleculeptr *mlist,mptr;
	double crossmin,crossmin2,crssptmin[3],crsspt[3],cross,*via,*pos;
	enum PanelFace face,facemin;
	panelptr pnl,pnlmin;

	if(!sim->srfss) return 0;
	if(!sim->mols) return 0;
	dim=sim->dim;
	nmol=sim->mols->nl[ll];
	mlist=sim->mols->live[ll];

	if(!reborn) m=0;
	else m=sim->mols->topl[ll];

	for(;m<nmol;m++) {
		mptr=mlist[m];
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
			crossmin=crossmin2=2;
			facemin=PFfront;
			pnlmin=NULL;
			for(bptr1=pos2box(sim,via);bptr1;bptr1=line2nextbox(sim,via,pos,bptr1)) {
				for(p=0;p<bptr1->npanel;p++) {
					pnl=bptr1->panel[p];
					if(pnl!=mptr->pnl) {
						lxp=lineXpanel(via,pos,pnl,dim,crsspt,&face,NULL,&cross,NULL,NULL);
						if(lxp && cross<=crossmin2) {
							if(cross<=crossmin) {
								crossmin2=crossmin;
								crossmin=cross;
								pnlmin=pnl;
								for(d=0;d<dim;d++) crssptmin[d]=crsspt[d];
								facemin=face; }
							else
								crossmin2=cross; }}}}
			if(crossmin<2) {											// a panel was crossed, so deal with it
				flag=(crossmin2!=crossmin && crossmin2-crossmin<VERYCLOSE)?1:0;
				if(flag) {
					for(d=0;d<dim;d++) pos[d]=via[d];
					done=1; }
				else {
#ifdef VCELL_HYBRID
					//if we defined suface rate as function then we have to evaluate the rate every time step when surface activity happens
					surfUpdateRate(sim, mptr, facemin, pnlmin);
					//after evaluating all the rate expressions, call surfupdateparams to update probs.
					surfupdateparams(sim);
#endif
					done=dosurfinteract(sim,mptr,ll,m,pnlmin,facemin,crssptmin);
					for(d=0;d<dim;d++) via[d]=crssptmin[d];
					sim->eventcount[ETsurf]++; }}
			else																	// nothing was crossed
				done=1; }}

	return 0; }


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
			dosurfinteract(sim,mptr,ll,m,mptr->pnl,PFnone,mptr->posx); }
	return 0; }


/******************************************************************************/
/************** End of regular code, start of threading code ******************/
/******************************************************************************/

typedef struct PARAMSET_check_surfaces_on_subset_mols {
	simptr sim;
	int ll;
	int first_ndx;
	int second_ndx;
	stack* output_stack;
	} PARAMS_check_surfaces_on_subset_mols;

void* check_surfaces_on_subset_mols(void* data);

void* check_surfaces_on_subset_mols(void* data) {
#ifndef THREADING
	return NULL;
#else
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
			movemol2closepanel(sim,mptr,dim,epsilon,sim->srfss->neighdist,sim->srfss->margin);
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
					lxp=lineXpanel(via,pos,pnl,dim,crsspt,&face,NULL,&cross,NULL,NULL);
					if(lxp && cross<=crossmin) {
						crossmin=cross;
						pnlmin=pnl;
						for(d=0;d<dim;d++) crssptmin[d]=crsspt[d];
						facemin=face; }}}
			if(crossmin<2) {
				done=dosurfinteract(sim,mptr,ll,m,pnlmin,facemin,crssptmin);
				for(d=0;d<dim;d++) via[d]=crssptmin[d];
				sim->eventcount[ETsurf]++; }
			else
				done=1; }}}

    return NULL;
#endif
}


int checksurfaces_threaded( simptr sim, int ll, int reborn) {
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

	int nthreads = sim->threads->nthreads;

	PARAMS_check_surfaces_on_subset_mols theParams;
	theParams.sim = sim;
	theParams.ll = ll;

	int first_ndx = 0;
	int final_ndx = nmol;

	if(!reborn) first_ndx = 0;
	else first_ndx = sim->mols->topl[ll];

	int total_num_to_process = final_ndx - first_ndx;
	if (total_num_to_process < nthreads) return checksurfaces( sim, ll, reborn);

	int stride = (total_num_to_process + (nthreads - (total_num_to_process % nthreads))) / nthreads;  // This equals ceil( total_num_to_process / nthreads).

	int thread_ndx;
	for( thread_ndx = 0; thread_ndx != nthreads - 1; ++thread_ndx) {
		clearthreaddata( sim->threads->thread[thread_ndx] );
		current_thread_input_stack = sim->threads->thread[thread_ndx]->input_stack;

		theParams.first_ndx = first_ndx;
		theParams.second_ndx = first_ndx += stride;
		theParams.output_stack = sim->threads->thread[ thread_ndx ]->output_stack;

		push_data_onto_stack( current_thread_input_stack, &theParams, sizeof(theParams)); // this copies over the inputParams data, so the fact that it is used to seed multiple threads is no problem.
		pthread_create(sim->threads->thread[thread_ndx]->thread_id, NULL, check_surfaces_on_subset_mols, (void*) current_thread_input_stack->stack_data); }
	{
	clearthreaddata( sim->threads->thread[nthreads - 1]);
	current_thread_input_stack = sim->threads->thread[nthreads-1]->input_stack;
	theParams.first_ndx = first_ndx;
	theParams.second_ndx = nmol;
	theParams.output_stack = sim->threads->thread[nthreads-1]->output_stack;

	push_data_onto_stack( current_thread_input_stack, &theParams, sizeof(theParams)); // this copies over the inputParams data, so the fact that it is used to seed multiple threads is no problem.
	pthread_create(sim->threads->thread[thread_ndx]->thread_id, NULL, check_surfaces_on_subset_mols, (void*) current_thread_input_stack->stack_data);
	}

	for( thread_ndx = 0; thread_ndx != nthreads; ++thread_ndx) {
		pthread_join( *((pthread_t*) sim->threads->thread[thread_ndx]->thread_id), NULL); }

    return 0;
#endif
}



