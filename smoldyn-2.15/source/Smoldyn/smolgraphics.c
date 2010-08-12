/* Steven Andrews, started 10/22/01.
This is a library of functions for the Smoldyn program.  See documentation
called Smoldyn_doc1.doc and Smoldyn_doc2.doc.
Copyright 2003-2007 by Steven Andrews.  This work is distributed under the terms
of the Gnu Lesser General Public License (LGPL). */

#include <float.h>
#include <math.h>
#include <stdio.h>
#include <string.h>
#include "Geometry.h"
#include "math2.h"
#include "opengl2.h"
#include "Rn.h"
#include "smoldyn.h"
#include "string2.h"

#define CHECK(A) if(!(A)) goto failure
#define CHECKS(A,B) if(!(A)) {strncpy(erstr,B,STRCHAR-1);erstr[STRCHAR-1]='\0';goto failure;} else (void)0


/******************************************************************************/
/*********************************** Graphics *********************************/
/******************************************************************************/

/******************************************************************************/
/******************************* enumerated types *****************************/
/******************************************************************************/

/* graphicsstring2lp. */
enum LightParam graphicsstring2lp(char *string) {
	enum LightParam ans;
	
	if(strbegin(string,"ambient",0)) ans=LPambient;
	else if(strbegin(string,"diffuse",0)) ans=LPdiffuse;
	else if(strbegin(string,"specular",0)) ans=LPspecular;
	else if(strbegin(string,"position",0)) ans=LPposition;
	else if(strbegin(string,"on",0)) ans=LPon;
	else if(strbegin(string,"off",0)) ans=LPoff;
	else if(strbegin(string,"auto",0)) ans=LPauto;
	else ans=LPnone;
	return ans; }


/* graphicslp2string. */
char *graphicslp2string(enum LightParam lp,char *string) {
	if(lp==LPambient) strcpy(string,"ambient");
	else if(lp==LPdiffuse) strcpy(string,"diffuse");
	else if(lp==LPspecular) strcpy(string,"specular");
	else if(lp==LPposition) strcpy(string,"position");
	else if(lp==LPon) strcpy(string,"on");
	else if(lp==LPoff) strcpy(string,"off");
	else if(lp==LPauto) strcpy(string,"auto");
	else strcpy(string,"none");
	return string; }


/******************************************************************************/
/******************************* memory management ****************************/
/******************************************************************************/

/* graphssalloc. */
graphicsssptr graphssalloc(void) {
	graphicsssptr graphss;
	int lt;

	graphss=NULL;
	CHECK(graphss=(graphicsssptr) malloc(sizeof(struct graphicssuperstruct)));

	graphss->graphics=0;
	graphss->graphicit=1;
	graphss->graphicdelay=0;
	graphss->tiffit=0;
	graphss->framepts=2;
	graphss->gridpts=0;

	graphss->framecolor[0]=0;		// black frame
	graphss->framecolor[1]=0;
	graphss->framecolor[2]=0;
	graphss->framecolor[3]=1;

	graphss->gridcolor[0]=0;		// black grid
	graphss->gridcolor[1]=0;
	graphss->gridcolor[2]=0;
	graphss->gridcolor[3]=1;

	graphss->backcolor[0]=1;		// white background
	graphss->backcolor[1]=1;
	graphss->backcolor[2]=1;
	graphss->backcolor[3]=1;

	graphicssetlight(graphss,-1,LPauto,NULL);
	for(lt=0;lt<MAXLIGHTS;lt++) graphicssetlight(graphss,lt,LPauto,NULL);

	return graphss;
	
failure:
	graphssfree(graphss);
	return NULL; }


/* graphssfree. */
void graphssfree(graphicsssptr graphss) {
	if(!graphss) return;
	free(graphss);
	return; }


/******************************************************************************/
/***************************** data structure output **************************/
/******************************************************************************/


/* graphssoutput. */
void graphssoutput(simptr sim) {
	graphicsssptr graphss;
	char *str,string[STRCHAR];
	int i1,i2,lt;

	graphss=sim->graphss;
	printf("GRAPHICS PARAMETERS\n");
	if(!graphss || graphss->graphics==0) {
		printf(" No graphical output\n\n");
		return; }

	printf(" display: ");
	if(graphss->graphics==1) printf("OpenGL");
	else if(graphss->graphics==2) printf("OpenGL_good");
	else if(graphss->graphics==3) printf("OpenGL_better");
	printf(", every %i iterations\n",graphss->graphicit);
	if(graphss->graphicdelay>0) printf(" delay per frame: %ui ms\n",graphss->graphicdelay);

	printf(" frame thickness: %g",graphss->framepts);
	if(graphss->gridpts) printf(", grid thickness: %g",graphss->gridpts);
	printf("\n");
	if(graphss->framepts) printf(" frame color: %g,%g,%g,%g\n",graphss->framecolor[0],graphss->framecolor[1],graphss->framecolor[2],graphss->framecolor[3]);
	if(graphss->gridpts) printf(" grid color: %g,%g,%g,%g\n",graphss->gridcolor[0],graphss->gridcolor[1],graphss->gridcolor[2],graphss->gridcolor[3]);
	printf(" background color: %g,%g,%g,%g\n",graphss->backcolor[0],graphss->backcolor[1],graphss->backcolor[2],graphss->backcolor[3]);

	if(graphss->graphics>=3)
		printf(" ambient light (%s): %g %g %g %g\n",graphicslp2string(graphss->roomstate,string),graphss->ambiroom[0],graphss->ambiroom[1],graphss->ambiroom[2],graphss->ambiroom[3]);
	for(lt=0;lt<MAXLIGHTS;lt++)
		if(graphss->lightstate[lt]!=LPauto) {
			printf(" light %i: %s\n",lt,graphicslp2string(graphss->lightstate[lt],string));
			printf("  position: %g %g %g\n",graphss->lightpos[lt][0],graphss->lightpos[lt][1],graphss->lightpos[lt][2]);
			printf("  ambient: %g %g %g %g\n",graphss->ambilight[lt][0],graphss->ambilight[lt][1],graphss->ambilight[lt][2],graphss->ambilight[lt][3]);
			printf("  diffuse: %g %g %g %g\n",graphss->difflight[lt][0],graphss->difflight[lt][1],graphss->difflight[lt][2],graphss->difflight[lt][3]);
			printf("  specular: %g %g %g %g\n",graphss->speclight[lt][0],graphss->speclight[lt][1],graphss->speclight[lt][2],graphss->speclight[lt][3]); }

	str=gl2SetOptionStr("TiffName",NULL);
	gl2GetString("TiffNameDefault",string);
	i1=(int)gl2GetNumber("TiffNumber");
	i2=(int)gl2GetNumber("TiffNumMax");
	if(strcmp(str,string)) printf(" TIFF name: %s\n",str);
	if(i1!=(int)gl2GetNumber("TiffNumberDefault") || i2!=(int)gl2GetNumber("TiffNumMaxDefault"))
		printf(" TIFFs numbered from %i to %i\n",i1,i2);
	printf("\n");
	return; }


/* writegraphss. */
void writegraphss(simptr sim,FILE *fptr) {
	graphicsssptr graphss;
	char string[STRCHAR];
	int lt;

	graphss=sim->graphss;
	if(!graphss) return;

	fprintf(fptr,"# Graphics parameters\n");
	if(graphss->graphics==0) fprintf(fptr,"graphics none\n");
	else if(graphss->graphics==1) fprintf(fptr,"graphics opengl\n");
	else if(graphss->graphics==2) fprintf(fptr,"graphics opengl_good\n");
	else if(graphss->graphics==3) fprintf(fptr,"graphics opengl_better\n");
	if(graphss->graphicit>1) fprintf(fptr,"graphic_iter %i\n",graphss->graphicit);
	if(graphss->graphicdelay>0) fprintf(fptr,"graphic_delay %ui\n",graphss->graphicdelay);

	if(graphss->tiffit>0) fprintf(fptr,"tiff_iter %i\n",graphss->tiffit);
	fprintf(fptr,"tiff_name %s\n",gl2SetOptionStr("TiffName",NULL));
	fprintf(fptr,"tiff_min %i\n",gl2SetOptionInt("TiffNumber",-1));
	fprintf(fptr,"tiff_max %i\n",gl2SetOptionInt("TiffNumMax",-1));

	fprintf(fptr,"frame_thickness %g\n",graphss->framepts);
	fprintf(fptr,"frame_color %g %g %g %g\n",graphss->framecolor[0],graphss->framecolor[1],graphss->framecolor[2],graphss->framecolor[3]);
	fprintf(fptr,"grid_thickness %g\n",graphss->gridpts);
	fprintf(fptr,"grid_color %g %g %g %g\n",graphss->gridcolor[0],graphss->gridcolor[1],graphss->gridcolor[2],graphss->gridcolor[3]);
	fprintf(fptr,"background_color %g %g %g %g\n",graphss->backcolor[0],graphss->backcolor[1],graphss->backcolor[2],graphss->backcolor[3]);

	if(graphss->roomstate!=LPauto) {
		fprintf(fptr,"light global ambient %g %g %g %g\n",graphss->ambiroom[0],graphss->ambiroom[1],graphss->ambiroom[2],graphss->ambiroom[3]);
		fprintf(fptr,"light global %s\n",graphicslp2string(graphss->roomstate,string)); }
	for(lt=0;lt<MAXLIGHTS;lt++)
		if(graphss->lightstate[lt]!=LPauto) {
			fprintf(fptr,"light %i position %g %g %g\n",lt,graphss->lightpos[lt][0],graphss->lightpos[lt][1],graphss->lightpos[lt][2]);
			fprintf(fptr,"light %i ambient %g %g %g %g\n",lt,graphss->ambilight[lt][0],graphss->ambilight[lt][1],graphss->ambilight[lt][2],graphss->ambilight[lt][3]);
			fprintf(fptr,"light %i diffuse %g %g %g %g\n",lt,graphss->difflight[lt][0],graphss->difflight[lt][1],graphss->difflight[lt][2],graphss->difflight[lt][3]);
			fprintf(fptr,"light %i specular %g %g %g %g\n",lt,graphss->speclight[lt][0],graphss->speclight[lt][1],graphss->speclight[lt][2],graphss->speclight[lt][3]);
			fprintf(fptr,"light %i %s\n",lt,graphicslp2string(graphss->lightstate[lt],string)); }

	fprintf(fptr,"\n");
	return; }


/* checkgraphicsparams. */
int checkgraphicsparams(simptr sim,int *warnptr) {
	int error,warn;

	error=warn=0;
	if(warnptr) *warnptr=warn;
	return error; }


/******************************************************************************/
/******************************* structure setup ******************************/
/******************************************************************************/


/* graphicssetlight. */
void graphicssetlight(graphicsssptr graphss,int lt,enum LightParam ltparam,double *value) {
	int i;

	if(lt==-1) {										// room lights
		if(ltparam==LPambient) {
			if(graphss->roomstate==LPauto) graphss->roomstate=LPon;
			for(i=0;i<4;i++) graphss->ambiroom[i]=value[i]; }
		else if(ltparam==LPon)
			graphss->roomstate=LPon;
		else if(ltparam==LPoff)
			graphss->roomstate=LPoff;
		else if(ltparam==LPauto) {
			graphss->roomstate=LPauto;
			graphss->ambiroom[0]=0.2;		// low white ambient light
			graphss->ambiroom[1]=0.2;
			graphss->ambiroom[2]=0.2;
			graphss->ambiroom[3]=1; }}

	else if(ltparam==LPambient) {
		if(graphss->lightstate[lt]==LPauto) graphss->lightstate[lt]=LPon;
		for(i=0;i<4;i++) graphss->ambilight[lt][i]=value[i]; }
	else if(ltparam==LPdiffuse) {
		if(graphss->lightstate[lt]==LPauto) graphss->lightstate[lt]=LPon;
		for(i=0;i<4;i++) graphss->difflight[lt][i]=value[i]; }
	else if(ltparam==LPspecular) {
		if(graphss->lightstate[lt]==LPauto) graphss->lightstate[lt]=LPon;
		for(i=0;i<4;i++) graphss->speclight[lt][i]=value[i]; }
	else if(ltparam==LPposition) {
		if(graphss->lightstate[lt]==LPauto) graphss->lightstate[lt]=LPon;
		for(i=0;i<3;i++) graphss->lightpos[lt][i]=value[i]; }
	else if(ltparam==LPon)
		graphss->lightstate[lt]=LPon;
	else if(ltparam==LPoff)
		graphss->lightstate[lt]=LPoff;
	else if(ltparam==LPauto) {
		graphss->lightstate[lt]=LPauto;
		graphss->ambilight[lt][0]=0;	// no ambient lightsource light
		graphss->ambilight[lt][1]=0;
		graphss->ambilight[lt][2]=0;
		graphss->ambilight[lt][3]=1;
		graphss->difflight[lt][0]=1;	// white diffuse lightsource light
		graphss->difflight[lt][1]=1;
		graphss->difflight[lt][2]=1;
		graphss->difflight[lt][3]=1;
		graphss->speclight[lt][0]=1;	// white specular lightsource light
		graphss->speclight[lt][1]=1;
		graphss->speclight[lt][2]=1;
		graphss->speclight[lt][3]=1;
		graphss->lightpos[lt][0]=1;		// lightsource at (1,1,0)
		graphss->lightpos[lt][1]=1;
		graphss->lightpos[lt][2]=0; }
	return; }


/******************************************************************************/
/*************************** core simulation functions ************************/
/******************************************************************************/


/* RenderSurfaces.  Draws all surfaces in the simulation using OpenGL graphics. */
#ifdef __gl_h_

void RenderSurfaces(simptr sim) {
	surfacessptr srfss;
	surfaceptr srf;
	int s,p,graphics,c;
	double **point,*front;
	double xlo,xhi,ylo,yhi,xpix,ypix,ymid,zmid;
	double delta,deltax,deltay,theta,vect[3],vect2[3],axis[3],height;
	enum DrawMode fdrawmode,bdrawmode;
	GLdouble gldvect[3];
	GLfloat glfvect[4];

	srfss=sim->srfss;
	graphics=sim->graphss->graphics;
	if(!srfss) return;
	xlo=gl2GetNumber("ClipLeft");
	xhi=gl2GetNumber("ClipRight");
	ylo=gl2GetNumber("ClipBot");
	yhi=gl2GetNumber("ClipTop");
	xpix=gl2GetNumber("PixWide");
	ypix=gl2GetNumber("PixHigh");
	ymid=gl2GetNumber("ClipMidy");
	zmid=gl2GetNumber("ClipMidz");

	if(sim->dim==1) {
		for(s=0;s<srfss->nsrf;s++) {
			srf=srfss->srflist[s];
			if(srf->fdrawmode!=DMno) {
				glLineWidth((GLfloat)srf->edgepts);
				delta=srf->edgepts*(xhi-xlo)/xpix/2;
				glColor4dv(srf->fcolor);
				glBegin(GL_LINES);
				for(p=0;p<srf->npanel[0];p++) {		// 1-D rectangles front
					point=srf->panels[0][p]->point;
					front=srf->panels[0][p]->front;
					glVertex3d(point[0][0]+front[0]*delta,ymid-(yhi-ylo)/20,zmid);
					glVertex3d(point[0][0]+front[0]*delta,ymid+(yhi-ylo)/20,zmid); }
				for(p=0;p<srf->npanel[1];p++) {		// 1-D triangles front
					point=srf->panels[1][p]->point;
					front=srf->panels[1][p]->front;
					glVertex3d(point[0][0]+front[0]*delta,ymid-(yhi-ylo)/20,zmid);
					glVertex3d(point[0][0]+front[0]*delta,ymid+(yhi-ylo)/20,zmid); }
				for(p=0;p<srf->npanel[2];p++) {		// 1-D spheres front
					point=srf->panels[2][p]->point;
					front=srf->panels[2][p]->front;
					glVertex3d(point[0][0]+point[1][0]+front[0]*delta,ymid-(yhi-ylo)/20,zmid);
					glVertex3d(point[0][0]+point[1][0]+front[0]*delta,ymid+(yhi-ylo)/20,zmid);
					glVertex3d(point[0][0]-point[1][0]-front[0]*delta,ymid-(yhi-ylo)/20,zmid);
					glVertex3d(point[0][0]-point[1][0]-front[0]*delta,ymid+(yhi-ylo)/20,zmid); }
				glEnd();

				delta*=-1;
				glColor4dv(srf->bcolor);
				glBegin(GL_LINES);
				for(p=0;p<srf->npanel[0];p++) {		// 1-D rectangles back
					point=srf->panels[0][p]->point;
					front=srf->panels[0][p]->front;
					glVertex3d(point[0][0]+front[0]*delta,ymid-(yhi-ylo)/20,zmid);
					glVertex3d(point[0][0]+front[0]*delta,ymid+(yhi-ylo)/20,zmid); }
				for(p=0;p<srf->npanel[1];p++) {		// 1-D triangles back
					point=srf->panels[1][p]->point;
					front=srf->panels[1][p]->front;
					glVertex3d(point[0][0]+front[0]*delta,ymid-(yhi-ylo)/20,zmid);
					glVertex3d(point[0][0]+front[0]*delta,ymid+(yhi-ylo)/20,zmid); }
				for(p=0;p<srf->npanel[2];p++) {		// 1-D spheres back
					point=srf->panels[2][p]->point;
					front=srf->panels[2][p]->front;
					glVertex3d(point[0][0]+point[1][0]+front[0]*delta,ymid-(yhi-ylo)/20,zmid);
					glVertex3d(point[0][0]+point[1][0]+front[0]*delta,ymid+(yhi-ylo)/20,zmid);
					glVertex3d(point[0][0]-point[1][0]-front[0]*delta,ymid-(yhi-ylo)/20,zmid);
					glVertex3d(point[0][0]-point[1][0]-front[0]*delta,ymid+(yhi-ylo)/20,zmid); }
				glEnd(); }}}

	else if(sim->dim==2) {
		for(s=0;s<srfss->nsrf;s++) {
			srf=srfss->srflist[s];
			fdrawmode=srf->fdrawmode;
			bdrawmode=srf->bdrawmode;
			if(fdrawmode!=DMno) {
				glColor4dv(srf->fcolor);
				if(fdrawmode&DMedge || fdrawmode&DMface) {
					glLineWidth((GLfloat)srf->edgepts);
					deltax=srf->edgepts*(xhi-xlo)/xpix/2.5;
					deltay=srf->edgepts*(yhi-ylo)/ypix/2.5;
					glBegin(GL_LINES); }
				else {
					glPointSize((GLfloat)srf->edgepts);
					deltax=deltay=0;
					glBegin(GL_POINTS); }

				for(p=0;p<srf->npanel[0];p++) {		// 2-D rectangles front
					point=srf->panels[0][p]->point;
					front=srf->panels[0][p]->front;
					if(front[1]==0) {
						glVertex3d(point[0][0]+front[0]*deltax,point[0][1],zmid);
						glVertex3d(point[1][0]+front[0]*deltax,point[1][1],zmid); }
					else {
						glVertex3d(point[0][0],point[0][1]+front[0]*deltay,zmid);
						glVertex3d(point[1][0],point[1][1]+front[0]*deltay,zmid); }}
				for(p=0;p<srf->npanel[1];p++) {		// 2-D triangles front
					point=srf->panels[1][p]->point;
					front=srf->panels[1][p]->front;
					glVertex3d(point[0][0]+front[0]*deltax,point[0][1]+front[1]*deltay,zmid);
					glVertex3d(point[1][0]+front[0]*deltax,point[1][1]+front[1]*deltay,zmid); }
				for(p=0;p<srf->npanel[3];p++) {		// 2-D cylinders front
					point=srf->panels[3][p]->point;
					front=srf->panels[3][p]->front;
					glVertex3d(point[0][0]+front[0]*point[2][0]+front[2]*front[0]*deltax,point[0][1]+front[1]*point[2][0]+front[2]*front[1]*deltay,zmid);
					glVertex3d(point[1][0]+front[0]*point[2][0]+front[2]*front[0]*deltax,point[1][1]+front[1]*point[2][0]+front[2]*front[1]*deltay,zmid);
					glVertex3d(point[0][0]-front[0]*point[2][0]-front[2]*front[0]*deltax,point[0][1]-front[1]*point[2][0]-front[2]*front[1]*deltay,zmid);
					glVertex3d(point[1][0]-front[0]*point[2][0]-front[2]*front[0]*deltax,point[1][1]-front[1]*point[2][0]-front[2]*front[1]*deltay,zmid); }
				for(p=0;p<srf->npanel[5];p++) {		// 2-D disks front
					point=srf->panels[5][p]->point;
					front=srf->panels[5][p]->front;
					glVertex3d(point[0][0]+point[1][0]*front[1]+front[0]*deltax,point[0][1]-point[1][0]*front[0]+front[1]*deltay,zmid);
					glVertex3d(point[0][0]-point[1][0]*front[1]+front[0]*deltax,point[0][1]+point[1][0]*front[0]+front[1]*deltay,zmid); }
				glEnd();
				for(p=0;p<srf->npanel[2];p++) {		// 2-D spheres front
					point=srf->panels[2][p]->point;
					front=srf->panels[2][p]->front;
					if(fdrawmode&DMvert) gl2DrawCircleD(point[0],point[1][0],(int)point[1][1],'v',2);
					if(fdrawmode&DMface) gl2DrawCircleD(point[0],point[1][0],(int)point[1][1],'f',2);
					if(fdrawmode&DMedge) gl2DrawCircleD(point[0],point[1][0]+front[0]*deltax,(int)point[1][1],'e',2); }
				for(p=0;p<srf->npanel[4];p++) {		// 2-D hemispheres front
					point=srf->panels[4][p]->point;
					front=srf->panels[4][p]->front;
					theta=atan2(point[2][1],point[2][0])+PI/2.0;
					if(fdrawmode&DMvert) gl2DrawArcD(point[0],point[1][0],theta,theta+PI,(int)point[1][1],'v',2);
					if(fdrawmode&DMface) gl2DrawArcD(point[0],point[1][0],theta,theta+PI,(int)point[1][1],'f',2);
					if(fdrawmode&DMedge) gl2DrawArcD(point[0],point[1][0]+front[0]*deltax,theta,theta+PI,(int)point[1][1],'e',2); }

				if(fdrawmode&DMedge || fdrawmode&DMface) {
					deltax*=-1;
					deltay*=-1;
					glColor4dv(srf->bcolor);
					glBegin(GL_LINES);
					for(p=0;p<srf->npanel[0];p++) {		// 2-D rectangles back
						point=srf->panels[0][p]->point;
						front=srf->panels[0][p]->front;
						if(front[1]==0) {
							glVertex3d(point[0][0]+front[0]*deltax,point[0][1],zmid);
							glVertex3d(point[1][0]+front[0]*deltax,point[1][1],zmid); }
						else {
							glVertex3d(point[0][0],point[0][1]+front[0]*deltay,zmid);
							glVertex3d(point[1][0],point[1][1]+front[0]*deltay,zmid); }}
					for(p=0;p<srf->npanel[1];p++) {		// 2-D triangles back
						point=srf->panels[1][p]->point;
						front=srf->panels[1][p]->front;
						glVertex3d(point[0][0]+front[0]*deltax,point[0][1]+front[1]*deltay,zmid);
						glVertex3d(point[1][0]+front[0]*deltax,point[1][1]+front[1]*deltay,zmid); }
					for(p=0;p<srf->npanel[3];p++) {		// 2-D cylinders back
						point=srf->panels[3][p]->point;
						front=srf->panels[3][p]->front;
						glVertex3d(point[0][0]+front[0]*point[2][0]+front[2]*front[0]*deltax,point[0][1]+front[1]*point[2][0]+front[2]*front[1]*deltay,zmid);
						glVertex3d(point[1][0]+front[0]*point[2][0]+front[2]*front[0]*deltax,point[1][1]+front[1]*point[2][0]+front[2]*front[1]*deltay,zmid);
						glVertex3d(point[0][0]-front[0]*point[2][0]-front[2]*front[0]*deltax,point[0][1]-front[1]*point[2][0]-front[2]*front[1]*deltay,zmid);
						glVertex3d(point[1][0]-front[0]*point[2][0]-front[2]*front[0]*deltax,point[1][1]-front[1]*point[2][0]-front[2]*front[1]*deltay,zmid); }
					for(p=0;p<srf->npanel[5];p++) {		// 2-D disks back
						point=srf->panels[5][p]->point;
						front=srf->panels[5][p]->front;
						glVertex3d(point[0][0]+point[1][0]*front[1]+front[0]*deltax,point[0][1]-point[1][0]*front[0]+front[1]*deltay,zmid);
						glVertex3d(point[0][0]-point[1][0]*front[1]+front[0]*deltax,point[0][1]+point[1][0]*front[0]+front[1]*deltay,zmid); }
					glEnd();
					for(p=0;p<srf->npanel[2];p++) {		// 2-D spheres back
						point=srf->panels[2][p]->point;
						front=srf->panels[2][p]->front;
						if(bdrawmode&DMedge) gl2DrawCircleD(point[0],point[1][0]+front[0]*deltax,(int)point[1][1],'e',2); }
					for(p=0;p<srf->npanel[4];p++) {		// 2-D hemispheres back
						point=srf->panels[4][p]->point;
						front=srf->panels[4][p]->front;
						theta=atan2(point[2][1],point[2][0])+PI/2.0;
						if(bdrawmode&DMedge) gl2DrawArcD(point[0],point[1][0]+front[0]*deltax,theta,theta+PI,(int)point[1][1],'e',2); }}}}}

	else if(sim->dim==3) {
		for(s=0;s<srfss->nsrf;s++) {
			srf=srfss->srflist[s];
			fdrawmode=srf->fdrawmode;
			bdrawmode=srf->bdrawmode;

			if(fdrawmode || bdrawmode) {
				if(fdrawmode&DMface) glPolygonMode(GL_FRONT,GL_FILL);
				else if(fdrawmode&DMedge) glPolygonMode(GL_FRONT,GL_LINE);
				else if(fdrawmode&DMvert) glPolygonMode(GL_FRONT,GL_POINT);
				else glCullFace(GL_FRONT);

				if(bdrawmode&DMface) glPolygonMode(GL_BACK,GL_FILL);
				else if(bdrawmode&DMedge) glPolygonMode(GL_BACK,GL_LINE);
				else if(bdrawmode&DMvert) glPolygonMode(GL_BACK,GL_POINT);
				else glCullFace(GL_BACK);

				glColor4dv(srf->fcolor);
				glLineWidth((GLfloat)srf->edgepts);
				if(graphics>=2 && srf->edgestipple[1]!=0xFFFF) {
					glEnable(GL_LINE_STIPPLE);
					glLineStipple((GLint)srf->edgestipple[0],(GLushort)srf->edgestipple[1]); }

				if(graphics>=3) {
					for(c=0;c<4;c++) glfvect[c]=(GLfloat)srf->fcolor[c];
					glMaterialfv(GL_FRONT,GL_SPECULAR,glfvect);
					for(c=0;c<4;c++) glfvect[c]=(GLfloat)srf->bcolor[c];
					glMaterialfv(GL_BACK,GL_SPECULAR,glfvect);
					glMateriali(GL_FRONT,GL_SHININESS,(int)srf->fshiny);
					glMateriali(GL_BACK,GL_SHININESS,(int)srf->bshiny); }

				if(srf->npanel[PSrect]) {
					glBegin(GL_QUADS);									// 3-D rectangles
					for(p=0;p<srf->npanel[PSrect];p++) {
						if(graphics>=3) {
							gldvect[0]=gldvect[1]=gldvect[2]=0;
							front=srf->panels[PSrect][p]->front;
							gldvect[(int)front[1]]=(GLdouble)front[0];
							glNormal3dv(gldvect); }
						point=srf->panels[PSrect][p]->point;
						glVertex3dv(point[0]);
						glVertex3dv(point[1]);
						glVertex3dv(point[2]);
						glVertex3dv(point[3]); }
					glEnd(); }

				if(srf->npanel[PStri]) {
					glBegin(GL_TRIANGLES);							// 3-D triangles
					for(p=0;p<srf->npanel[PStri];p++) {
						if(graphics>=3) glNormal3dv(srf->panels[PStri][p]->front);
						point=srf->panels[PStri][p]->point;
						glVertex3dv(point[0]);
						glVertex3dv(point[1]);
						glVertex3dv(point[2]); }
					glEnd(); }

				for(p=0;p<srf->npanel[PSsph];p++) {		// 3-D spheres
					point=srf->panels[PSsph][p]->point;
					front=srf->panels[PSsph][p]->front;
					glMatrixMode(GL_MODELVIEW);
					glPushMatrix();
					glTranslated(point[0][0],point[0][1],point[0][2]);
					gl2DrawSphere(point[1][0],(int)point[1][1],(int)point[1][2],front[0]>0?0:1,graphics>=3?1:0);
					glPopMatrix(); }

				for(p=0;p<srf->npanel[PScyl];p++) {			// 3-D cylinders
					point=srf->panels[PScyl][p]->point;
					front=srf->panels[PScyl][p]->front;
					glMatrixMode(GL_MODELVIEW);
					glPushMatrix();
					glTranslated(point[0][0],point[0][1],point[0][2]);
					vect[0]=vect[1]=0;
					vect[2]=1;
					vect2[0]=point[1][0]-point[0][0];
					vect2[1]=point[1][1]-point[0][1];
					vect2[2]=point[1][2]-point[0][2];
					height=sqrt(vect2[0]*vect2[0]+vect2[1]*vect2[1]+vect2[2]*vect2[2]);
					normalizeVD(vect2,3);
					theta=gl2FindRotateD(vect,vect2,axis);
					glRotated(theta,axis[0],axis[1],axis[2]);
					gl2DrawCylinder(point[2][0],point[2][0],height,(int)point[2][1],(int)point[2][2],front[0]>0?0:1,graphics>=3?1:0);
					glPopMatrix(); }

				for(p=0;p<srf->npanel[PShemi];p++) {			// 3-D hemispheres
					point=srf->panels[PShemi][p]->point;
					front=srf->panels[PShemi][p]->front;
					glMatrixMode(GL_MODELVIEW);
					glPushMatrix();
					glTranslated(point[0][0],point[0][1],point[0][2]);
					vect[0]=vect[1]=0;
					vect[2]=-1;
					theta=gl2FindRotateD(vect,point[2],axis);
					glRotated(theta,axis[0],axis[1],axis[2]);
					gl2DrawHemisphere(point[1][0],(int)point[1][1],(int)point[1][2],front[0]>0?0:1,graphics>=3?1:0);
					glPopMatrix(); }

				for(p=0;p<srf->npanel[PSdisk];p++) {			// 3-D disks
					point=srf->panels[PSdisk][p]->point;
					front=srf->panels[PSdisk][p]->front;
					glMatrixMode(GL_MODELVIEW);
					glPushMatrix();
					glTranslated(point[0][0],point[0][1],point[0][2]);
					vect[0]=vect[1]=0;
					vect[2]=-1;
					theta=gl2FindRotateD(vect,front,axis);
					glRotated(theta,axis[0],axis[1],axis[2]);
					vect2[0]=vect2[1]=vect2[2]=0;
					gl2DrawCircleD(vect2,point[1][0],(int)point[1][1],'f',3);//???? 'f' isn't right
					glPopMatrix(); }

				if(glIsEnabled(GL_LINE_STIPPLE))
					glDisable(GL_LINE_STIPPLE); }}}

	return; }

#else

void RenderSurfaces(simptr sim) {
	return; }

#endif



/* RenderMolecs */
#ifdef __gl_h_

void RenderMolecs(simptr sim) {
	molssptr mols;
	moleculeptr mptr;
	int ll,m,i,dim;
	double ymid,zmid;
	enum MolecState ms;
	GLfloat whitecolor[]={1,1,1,1};

	dim=sim->dim;
	mols=sim->mols;
	if(!mols) return;
	ymid=gl2GetNumber("ClipMidy");
	zmid=gl2GetNumber("ClipMidz");

	if(sim->graphss->graphics==1) {
		for(ll=0;ll<sim->mols->nlist;ll++)
			if(sim->mols->listtype[ll]==MLTsystem)
				for(m=0;m<mols->nl[ll];m++) {
					mptr=mols->live[ll][m];
					i=mptr->ident;
					ms=mptr->mstate;
					if(mols->display[i][ms]>0) {
						glPointSize((GLfloat)mols->display[i][ms]);
						glColor3dv((GLdouble*)mols->color[i][ms]);
						glBegin(GL_POINTS);
						if(dim==1) glVertex3d((GLdouble)mptr->pos[0],ymid,zmid);
						else if(dim==2) glVertex3d((GLdouble)mptr->pos[0],(GLdouble)mptr->pos[1],zmid);
						else glVertex3dv((GLdouble*)mptr->pos);
						glEnd(); }}}

	else if(sim->graphss->graphics>=2) {
		glMatrixMode(GL_MODELVIEW);
		glPolygonMode(GL_FRONT_AND_BACK,GL_FILL);
		if(sim->graphss->graphics>=3) {
			glMaterialfv(GL_FRONT,GL_SPECULAR,whitecolor);
			glMateriali(GL_FRONT,GL_SHININESS,30); }
		for(ll=0;ll<sim->mols->nlist;ll++)
			if(sim->mols->listtype[ll]==MLTsystem)
				for(m=0;m<mols->nl[ll];m++) {
					mptr=mols->live[ll][m];
					i=mptr->ident;
					ms=mptr->mstate;
					if(mols->display[i][ms]>0) {
						glColor3dv((GLdouble*)mols->color[i][ms]);
						glPushMatrix();
						if(dim==1) glTranslated((GLdouble)mptr->pos[0],ymid,zmid);
						else if(dim==2) glTranslated((GLdouble)mptr->pos[0],(GLdouble)mptr->pos[1],zmid);
						else glTranslated((GLdouble)mptr->pos[0],(GLdouble)mptr->pos[1],(GLdouble)mptr->pos[2]);
						glutSolidSphere((GLdouble)mols->display[i][ms],15,15);
						glPopMatrix(); }}}
	
	else
		;

	return; }

#else

void RenderMolecs(simptr sim) {
	return; }

#endif



/* RenderScene is the call-back function for OpenGL that displays the graphics.
This function simply draws a box for the simulation volume, as well as points
for each molecule. */
#ifdef __gl_h_

void RenderSim(simptr sim) {
	graphicsssptr graphss;
	double pt1[DIMMAX],pt2[DIMMAX];
	int dim;
	wallptr *wlist;
	molssptr mols;

	graphss=sim->graphss;
	if(!graphss || graphss->graphics==0) return;
	dim=sim->dim;
	mols=sim->mols;
	wlist=sim->wlist;
	if(dim<3) glClear(GL_COLOR_BUFFER_BIT);
	else glClear(GL_COLOR_BUFFER_BIT|GL_DEPTH_BUFFER_BIT);

	if(dim==3) RenderMolecs(sim);

	if(graphss->framepts) {														// draw bounding box
		pt1[0]=wlist[0]->pos;
		pt2[0]=wlist[1]->pos;
		pt1[1]=dim>1?wlist[2]->pos:0;
		pt2[1]=dim>1?wlist[3]->pos:0;
		pt1[2]=dim>2?wlist[4]->pos:0;
		pt2[2]=dim>2?wlist[5]->pos:0;
		glColor4dv((GLdouble*)graphss->framecolor);
		glLineWidth((GLfloat)graphss->framepts);
		gl2DrawBoxD(pt1,pt2,dim); }

	if(graphss->gridpts) {
		pt1[0]=sim->boxs->min[0];
		pt2[0]=pt1[0]+sim->boxs->size[0]*sim->boxs->side[0];
		pt1[1]=dim>1?sim->boxs->min[1]:0;
		pt2[1]=dim>1?pt1[1]+sim->boxs->size[1]*sim->boxs->side[1]:0;
		pt1[2]=dim>2?sim->boxs->min[2]:0;
		pt2[2]=dim>2?pt1[2]+sim->boxs->size[2]*sim->boxs->side[2]:0;
		glColor4dv((GLdouble*)graphss->gridcolor);
		if(dim==1) glPointSize((GLfloat)graphss->gridpts);
		else glLineWidth((GLfloat)graphss->gridpts);
		gl2DrawGridD(pt1,pt2,sim->boxs->side,dim); }

	if(dim<3) RenderMolecs(sim);

	if(sim->srfss) RenderSurfaces(sim);

	glutSwapBuffers();
	return; }

#else

void RenderSim(simptr sim) {
	return; }

#endif








