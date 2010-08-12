/* Steven Andrews, 2/03.
OpenGL library of useful routines.
See documentation called opengl2_doc.doc.
Copyright 2003-2007 by Steven Andrews.  This work is distributed under the terms
of the Gnu Lesser General Public License (LGPL). */

#include <stdio.h>
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "opengl2.h"
#include "math2.h"
#include "random2.h"
#include "string2.h"

#define Fix2DAspectDefault 0
#define TiffNameDefault "OpenGL"
#define TiffNumberDefault 1
#define TiffNumMaxDefault 999
#define RotateAngleDefault 5.0

void ChangeSize(int w,int h);
void KeyPush(unsigned char key,int x,int y);
void SpecialKeyPush2(unsigned char key,int x,int y);
void SpecialKeyPush(int key,int x,int y);
int WriteTIFF(char *filename,char *description,int x,int y,int width,int height,int compression);

GLfloat ClipSize,ClipMidx,ClipMidy,ClipMidz;
GLfloat ClipLeft,ClipRight,ClipBot,ClipTop,ClipBack,ClipFront;
GLfloat FieldOfView,Near,Aspect,Zoom;
GLfloat PixWide,PixHigh;
int Gl2PauseState,Dimension;
GLfloat Xtrans,Ytrans;
int Fix2DAspect=Fix2DAspectDefault;
char TiffName[STRCHAR]=TiffNameDefault;
int TiffNumber=TiffNumberDefault;
int TiffNumMax=TiffNumMaxDefault;
GLfloat RotateAngle=RotateAngleDefault;
void (*FreeFunc)(void*)=NULL;
void *FreePointer=NULL;


/* ***************************************** */
/* ************* Local functions *********** */
/* ***************************************** */


/* ChangeSize */
#ifdef __gl_h_

void ChangeSize(int w,int h) {
	GLfloat clipheight,clipwidth;
	GLfloat nearold,m[16];

	PixWide=w;
	PixHigh=h;
	if(h==0) h=1;
	glViewport(0,0,w,h);
	if(Dimension<3 && Fix2DAspect) {
		if(w<=h) {
			clipheight=ClipSize/Zoom*h/w;
			clipwidth=ClipSize/Zoom; }
		else {
			clipheight=ClipSize/Zoom;
			clipwidth=ClipSize/Zoom*w/h; }
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		glOrtho(ClipLeft,ClipLeft+clipwidth,ClipBot,ClipBot+clipheight,ClipFront,ClipBack);
		glMatrixMode(GL_MODELVIEW);
		glLoadIdentity(); }
	else if(Dimension<3) {
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		glOrtho(ClipLeft,ClipRight,ClipBot,ClipTop,ClipFront,ClipBack);
		glMatrixMode(GL_MODELVIEW);
		glLoadIdentity(); }
	else {
		Aspect=1.0*w/h;
		nearold=Near;
		if(w>=h) Near=ClipSize/2.0/tan(FieldOfView*PI/180.0/2.0);
		else Near=ClipSize/2.0/tan(FieldOfView*Aspect*PI/180.0/2.0);
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		gluPerspective(FieldOfView,Aspect,Near,ClipSize+Near);
		glMatrixMode(GL_MODELVIEW);
		glGetFloatv(GL_MODELVIEW_MATRIX,m);
		glLoadIdentity();
		glTranslatef(0,0,nearold-Near);
		glMultMatrixf(m); }
	return; }

#else

void ChangeSize(int w,int h) {
	return; }

#endif



/* KeyPush */
#ifdef __gl_h_

void KeyPush(unsigned char key,int x,int y) {
	GLint viewport[4],w,h;
	char name[STRCHAR],str[STRCHAR];
	GLfloat clipheight,clipwidth;

	x=y=0;	// to avoid compiler warnings
	glMatrixMode(GL_MODELVIEW);
	if(key=='Q') {
		if(Gl2PauseState==2) {
			if(FreeFunc) (*FreeFunc)(FreePointer);
			exit(0); }
		else Gl2PauseState=2; }
	else if(key==' ' && Gl2PauseState==0) Gl2PauseState=1;
	else if(key==' ' && Gl2PauseState==1) Gl2PauseState=0;
	else if(key=='T' && TiffNumber<=TiffNumMax) {
		glGetIntegerv(GL_VIEWPORT,viewport);
		w=viewport[2];
		h=viewport[3];
		sprintf(str,"%s%%0%ii.tif",TiffName,(int)log10(TiffNumMax)+1);
		sprintf(name,str,TiffNumber);
		WriteTIFF(name,"OpenGL picture",0,0,w,h,-1);
		TiffNumber++; }
	else if(key=='z' && Dimension==3) glRotatef(RotateAngle,0,0,1);
	else if(key=='Z' && Dimension==3) glRotatef(-RotateAngle,0,0,1);
	else if(key=='x' && Dimension==3) glRotatef(RotateAngle,1,0,0);
	else if(key=='X' && Dimension==3) glRotatef(-RotateAngle,1,0,0);
	else if(key=='y' && Dimension==3) glRotatef(RotateAngle,0,1,0);
	else if(key=='Y' && Dimension==3) glRotatef(-RotateAngle,0,1,0);
	else if(Dimension<3) {
		if(key=='0') {
			Zoom=1;
			Xtrans=Ytrans=0; }
		else if(key=='=') Zoom*=1.05;
		else if(key=='-') Zoom/=1.05;
		ClipLeft=ClipMidx-Xtrans-ClipSize/2.0/Zoom;
		ClipRight=ClipMidx-Xtrans+ClipSize/2.0/Zoom;
		ClipBot=ClipMidy-Ytrans-ClipSize/2.0/Zoom;
		ClipTop=ClipMidy-Ytrans+ClipSize/2.0/Zoom;
		ClipBack=ClipMidz-ClipSize/2.0/Zoom;
		ClipFront=ClipMidz+ClipSize/2.0/Zoom;
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		if(Fix2DAspect) {
			glGetIntegerv(GL_VIEWPORT,viewport);
			w=viewport[2];
			h=viewport[3];
			if(w<=h) {
				clipheight=ClipSize/Zoom*h/w;
				clipwidth=ClipSize/Zoom; }
			else {
				clipheight=ClipSize/Zoom;
				clipwidth=ClipSize/Zoom*w/h; }
			glOrtho(ClipLeft,ClipLeft+clipwidth,ClipBot,ClipBot+clipheight,ClipFront,ClipBack); }
		else
			glOrtho(ClipLeft,ClipRight,ClipBot,ClipTop,ClipFront,ClipBack);
		glMatrixMode(GL_MODELVIEW);
		glLoadIdentity(); }

	else if(Dimension==3) {
		if(key=='0') {
			glGetIntegerv(GL_VIEWPORT,viewport);
			w=viewport[2];
			h=viewport[3];
			FieldOfView=45;
			Xtrans=Ytrans=0;
			if(w>=h) Near=ClipSize/2.0/tan(FieldOfView*PI/180.0/2.0);
			else Near=ClipSize/2.0/tan(FieldOfView*Aspect*PI/180.0/2.0);
			glMatrixMode(GL_PROJECTION);
			glLoadIdentity();
			gluPerspective(FieldOfView,Aspect,Near,ClipSize+Near);
			glMatrixMode(GL_MODELVIEW);
			glLoadIdentity();
			glTranslatef(-ClipMidx,-ClipMidy,-ClipMidz);
			glTranslatef(0,0,-ClipSize/2.0-Near); }
		else if(key=='=') {
			FieldOfView/=1.05;
			glMatrixMode(GL_PROJECTION);
			glLoadIdentity();
			gluPerspective(FieldOfView,Aspect,Near,ClipSize+Near);
			glMatrixMode(GL_MODELVIEW); }
		else if(key=='-') {
			FieldOfView*=1.05;
			if(FieldOfView>180) FieldOfView=180;
			glMatrixMode(GL_PROJECTION);
			glLoadIdentity();
			gluPerspective(FieldOfView,Aspect,Near,ClipSize+Near);
			glMatrixMode(GL_MODELVIEW); }}
	return; }

#else

void KeyPush(unsigned char key,int x,int y) {
	return; }

#endif


/* SpecialKeyPush2 */
#ifdef __gl_h_

void SpecialKeyPush2(unsigned char key,int x,int y) {
	GLfloat m[16];
	GLint viewport[4],w,h;
	GLfloat clipheight,clipwidth;

	x=y=0;		// to avoid compiler warnings
	if(Dimension<3) {
		if(key=='D') Ytrans-=(ClipRight-ClipLeft)/100;
		else if(key=='U') Ytrans+=(ClipRight-ClipLeft)/100;
		else if(key=='R') Xtrans+=(ClipRight-ClipLeft)/100;
		else if(key=='L') Xtrans-=(ClipRight-ClipLeft)/100;
		ClipLeft=ClipMidx-Xtrans-ClipSize/2.0/Zoom;
		ClipRight=ClipMidx-Xtrans+ClipSize/2.0/Zoom;
		ClipBot=ClipMidy-Ytrans-ClipSize/2.0/Zoom;
		ClipTop=ClipMidy-Ytrans+ClipSize/2.0/Zoom;
		ClipBack=ClipMidz-ClipSize/2.0/Zoom;
		ClipFront=ClipMidz+ClipSize/2.0/Zoom;
		glMatrixMode(GL_PROJECTION);
		glLoadIdentity();
		if(Fix2DAspect) {
			glGetIntegerv(GL_VIEWPORT,viewport);
			w=viewport[2];
			h=viewport[3];
			if(w<=h) {
				clipheight=ClipSize/Zoom*h/w;
				clipwidth=ClipSize/Zoom; }
			else {
				clipheight=ClipSize/Zoom;
				clipwidth=ClipSize/Zoom*w/h; }
			glOrtho(ClipLeft,ClipLeft+clipwidth,ClipBot,ClipBot+clipheight,ClipFront,ClipBack); }
		else
			glOrtho(ClipLeft,ClipRight,ClipBot,ClipTop,ClipFront,ClipBack);
		glMatrixMode(GL_MODELVIEW);
		glLoadIdentity(); }
	else if(Dimension==3) {
		glMatrixMode(GL_MODELVIEW);
		glGetFloatv(GL_MODELVIEW_MATRIX,m);
		glLoadIdentity();
		if(strchr("durl",key)) {
			glTranslatef(Xtrans,Ytrans,-(Near+ClipSize/2.0));
			if(key=='d') glRotatef(RotateAngle,1,0,0);
			else if(key=='u') glRotatef(-RotateAngle,1,0,0);
			else if(key=='r') glRotatef(RotateAngle,0,1,0);
			else if(key=='l') glRotatef(-RotateAngle,0,1,0);
			glTranslatef(-Xtrans,-Ytrans,+(Near+ClipSize/2.0)); }
		else if(strchr("DURL",key)) {
			glTranslatef(-Xtrans,-Ytrans,-(Near+ClipSize/2.0));
			if(key=='D') Ytrans-=ClipSize/100;
			else if(key=='U') Ytrans+=ClipSize/100;
			else if(key=='R') Xtrans+=ClipSize/100;
			else if(key=='L') Xtrans-=ClipSize/100;
			glTranslatef(Xtrans,Ytrans,+(Near+ClipSize/2.0)); }
		glMultMatrixf(m); }
	return; }

#else

void SpecialKeyPush2(unsigned char key,int x,int y) {
	return; }

#endif


/* SpecialKeyPush */
#ifdef __gl_h_

void SpecialKeyPush(int key,int x,int y) {
	int modify;

	modify=glutGetModifiers();
	if(!modify) {
		if(key==GLUT_KEY_DOWN) SpecialKeyPush2('d',x,y);
		else if(key==GLUT_KEY_UP) SpecialKeyPush2('u',x,y);
		else if(key==GLUT_KEY_RIGHT) SpecialKeyPush2('r',x,y);
		else if(key==GLUT_KEY_LEFT) SpecialKeyPush2('l',x,y); }
	else {
		if(key==GLUT_KEY_DOWN) SpecialKeyPush2('D',x,y);
		else if(key==GLUT_KEY_UP) SpecialKeyPush2('U',x,y);
		else if(key==GLUT_KEY_RIGHT) SpecialKeyPush2('R',x,y);
		else if(key==GLUT_KEY_LEFT) SpecialKeyPush2('L',x,y); }
	return; }

#else

void SpecialKeyPush(int key,int x,int y) {
	return; }

#endif


/* The following code was modified from a program called writetiff.c that was
written and copyrighted by Mark Kilgard, 1997.  This function requires the use
of the libtiff library that was written by Sam Leffler and can be downloaded
from www.libtiff.org. */
#if defined _TIFFIO_ && defined __gl_h_

int WriteTIFF(char *filename,char *description,int x,int y,int width,int height,int compression) {
  TIFF *file;
  GLubyte *image,*p;
  int i;

	if(compression==-1) compression=COMPRESSION_PACKBITS;
  file=TIFFOpen(filename,"w");
  if(!file) return 1;
  image=(GLubyte*)malloc(width*height*sizeof(GLubyte)*3);
  if(!image) return 1;
  glPixelStorei(GL_PACK_ALIGNMENT,1);
  glReadPixels(x,y,width,height,GL_RGB,GL_UNSIGNED_BYTE,image);
  TIFFSetField(file,TIFFTAG_IMAGEWIDTH,(unsigned int)width);
  TIFFSetField(file,TIFFTAG_IMAGELENGTH,(unsigned int)height);
  TIFFSetField(file,TIFFTAG_BITSPERSAMPLE,8);
  TIFFSetField(file,TIFFTAG_COMPRESSION,compression);
  TIFFSetField(file,TIFFTAG_PHOTOMETRIC,PHOTOMETRIC_RGB);
  TIFFSetField(file,TIFFTAG_SAMPLESPERPIXEL,3);
  TIFFSetField(file,TIFFTAG_PLANARCONFIG,PLANARCONFIG_CONTIG);
  TIFFSetField(file,TIFFTAG_ROWSPERSTRIP,1);
  TIFFSetField(file,TIFFTAG_IMAGEDESCRIPTION,description);
  p=image;
  for(i=height-1;i>=0;i--) {
    if(TIFFWriteScanline(file,p,i,0)<0) {
      free(image);
      TIFFClose(file);
      return 1; }
    p+=width*sizeof(GLubyte)*3; }
  TIFFClose(file);
  free(image);
  return 0; }

#else

int WriteTIFF(char *filename,char *description,int x,int y,int width,int height,int compression) {
	return 2; }

#endif



/* **************************************************** */
/* ********** Externally accessible routines ********** */
/* **************************************************** */

/* gl2Initialize */
#ifdef __gl_h_

void gl2Initialize(float xlo,float xhi,float ylo,float yhi,float zlo,float zhi) {
	if(ylo==yhi && zlo==zhi) Dimension=1;
	else if(zlo==zhi) Dimension=2;
	else Dimension=3;
	ClipSize=1.05*sqrt((xhi-xlo)*(xhi-xlo)+(yhi-ylo)*(yhi-ylo)+(zhi-zlo)*(zhi-zlo));
	if(ClipSize==0) ClipSize=1.0;
	ClipMidx=(xhi-xlo)/2.0+xlo;
	ClipMidy=(yhi-ylo)/2.0+ylo;
	ClipMidz=(zhi-zlo)/2.0+zlo;
	ClipLeft=ClipMidx-ClipSize/2.0;
	ClipRight=ClipMidx+ClipSize/2.0;
	ClipBot=ClipMidy-ClipSize/2.0;
	ClipTop=ClipMidy+ClipSize/2.0;
	ClipBack=ClipMidz-ClipSize/2.0;
	ClipFront=ClipMidz+ClipSize/2.0;
	if(Dimension==2 && !Fix2DAspect) {
		ClipLeft=xlo;
		ClipRight=xhi;
		ClipBot=ylo;
		ClipTop=yhi; }
	FieldOfView=45;
	Zoom=1;
	Xtrans=Ytrans=0;
	Near=-ClipSize/2.0;
	Aspect=1.0;
	Gl2PauseState=0;

	if(Dimension<3) glutInitDisplayMode(GLUT_DOUBLE|GLUT_RGB);
	else glutInitDisplayMode(GLUT_DOUBLE|GLUT_RGB|GLUT_DEPTH);
	glutCreateWindow("OpenGL");
	glutReshapeFunc(ChangeSize);
	glutKeyboardFunc(KeyPush);
	glutSpecialFunc(SpecialKeyPush);
	glClearColor(1,1,1,1);
	glColor3f(0,0,0);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();
	glTranslatef(-ClipMidx,-ClipMidy,-ClipMidz);
	if(Dimension==3) {
		glEnable(GL_DEPTH_TEST); }
	return; }

#else

void gl2Initialize(float xlo,float xhi,float ylo,float yhi,float zlo,float zhi) {
	return; }

#endif


/* gl2glutInit */
#ifdef __gl_h_

void gl2glutInit(int *argc,char **argv) {
	glutInit(argc,argv);
	return; }

#else

void gl2glutInit(int *argc,char **argv) {
	return; }

#endif


/* gl2State */
int gl2State(int state) {
	if(state>=0) Gl2PauseState=state;
	return Gl2PauseState; }


/* gl2GetNumber */
float gl2GetNumber(char *variable) {
	float value;

	if(!strcmp(variable,"ClipSize")) value=ClipSize;
	else if(!strcmp(variable,"ClipMidx")) value=ClipMidx;
	else if(!strcmp(variable,"ClipMidy")) value=ClipMidy;
	else if(!strcmp(variable,"ClipMidz")) value=ClipMidz;
	else if(!strcmp(variable,"ClipLeft")) value=ClipLeft;
	else if(!strcmp(variable,"ClipRight")) value=ClipRight;
	else if(!strcmp(variable,"ClipBot")) value=ClipBot;
	else if(!strcmp(variable,"ClipTop")) value=ClipTop;
	else if(!strcmp(variable,"ClipBack")) value=ClipBack;
	else if(!strcmp(variable,"ClipFront")) value=ClipFront;
	else if(!strcmp(variable,"FieldOfView")) value=FieldOfView;
	else if(!strcmp(variable,"Zoom")) value=Zoom;
	else if(!strcmp(variable,"Near")) value=Near;
	else if(!strcmp(variable,"Aspect")) value=Aspect;
	else if(!strcmp(variable,"PixWide")) value=PixWide;
	else if(!strcmp(variable,"PixHigh")) value=PixHigh;
	else if(!strcmp(variable,"Gl2PauseState")) value=(float) Gl2PauseState;
	else if(!strcmp(variable,"Dimension")) value=(float) Dimension;
	else if(!strcmp(variable,"Xtrans")) value=Xtrans;
	else if(!strcmp(variable,"Ytrans")) value=Ytrans;
	else if(!strcmp(variable,"Fix2DAspect")) value=(float) Fix2DAspect;
	else if(!strcmp(variable,"TiffNumber")) value=(float) TiffNumber;
	else if(!strcmp(variable,"TiffNumMax")) value=(float) TiffNumMax;
	else if(!strcmp(variable,"RotateAngle")) value=RotateAngle;
	else if(!strcmp(variable,"TiffNumberDefault")) value=(float) TiffNumberDefault;
	else if(!strcmp(variable,"TiffNumMaxDefault")) value=(float) TiffNumMaxDefault;
	else if(!strcmp(variable,"RotateAngleDefault")) value=RotateAngleDefault;
	else if(!strcmp(variable,"Fix2DAspectDefault")) value=(float) Fix2DAspectDefault;
	else value=0;
	return value; }


/* gl2GetString */
char *gl2GetString(char *option,char *string) {
	if(!strcmp(option,"TiffName"))
		strncpy(string,TiffName,STRCHAR);
	if(!strcmp(option,"TiffNameDefault"))
		strncpy(string,TiffNameDefault,STRCHAR);
	else
		strncpy(string,"error",STRCHAR);
	return string; }


/* gl2SetOptionInt */
int gl2SetOptionInt(char *option,int value) {
	if(!strcmp(option,"Fix2DAspect")) {
		if(value>=0) Fix2DAspect=value;
		else value=Fix2DAspect; }
	else if(!strcmp(option,"TiffNumber")) {
		if(value>=0) TiffNumber=value;
		else value=TiffNumber; }
	else if(!strcmp(option,"TiffNumMax")) {
		if(value>=0) TiffNumMax=value;
		else value=TiffNumMax; }
	else if(!strcmp(option,"Dimension")) {
		value=Dimension; }
	else value=-1;
	return value; }


/* gl2SetOptionFlt */
float gl2SetOptionFlt(char *option,float value) {
	if(!strcmp(option,"RotateAngle")) {
		if(value>0) RotateAngle=(GLfloat) value;
		else value=(float) RotateAngle; }
	else value=-1;
	return value; }


/* gl2SetOptionStr */
char *gl2SetOptionStr(char *option,char *value) {
	if(!strcmp(option,"TiffName")) {
		if(value) strncpy(TiffName,value,STRCHAR);
		else value=TiffName; }
	if(!strcmp(option,"TiffNameDefault")) {
		value=TiffNameDefault; }
	return value; }


/* gl2SetOptionVoid */
char *gl2SetOptionVoid(char *option,void *value) {
	if(!strcmp(option,"FreeFunc")) {
		if(value) FreeFunc=value;
		else value=FreeFunc; }
	else if(!strcmp(option,"FreePointer")) {
		if(value) FreePointer=value;
		else value=FreePointer; }
	else value=NULL;
	return value; }


/* gl2SetKeyPush */
void gl2SetKeyPush(unsigned char key) {
	if(strchr(" QT0=-xyzXYZ",key)) KeyPush(key,0,0);
	else if(strchr("durlDURL",key)) SpecialKeyPush2(key,0,0);
	return; }


/* gl2SetColor */
#ifdef __gl_h_

void gl2SetColor(char c) {
	if(c=='A' || c=='a') glColor3ub(0x70,0xdb,0x93);							/* aqua			*/
	else if(c=='B' || c=='b' || c=='6')	glColor3ub(0x00,0x00,0xff);	/* blue			*/
	else if(c=='C' || c=='c')	glColor3ub(0x00,0xff,0xff);					/* cyan			*/
	else if(c=='D' || c=='d')	glColor3ub(0x80,0x00,0x00);					/* dark red	*/
	else if(c=='E' || c=='e' || c=='8')	glColor3ub(0x80,0x80,0x80);	/* grey			*/
	else if(c=='F' || c=='f')	glColor3ub(0xff,0x6e,0xc7);					/* fuchsia	*/
	else if(c=='G' || c=='g')	glColor3ub(0x00,0xff,0x00);					/* green		*/
	else if(c=='H' || c=='h' || c=='5')	glColor3ub(0x00,0x80,0x00);	/* hunter		*/
	else if(c=='I' || c=='i')	glColor3ub(0xdb,0x70,0xdb);					/* indigo		*/
	else if(c=='J' || c=='j')	glColor3ub(0x80,0x80,0x00);					/* olive		*/
	else if(c=='K' || c=='k' || c=='0')	glColor3ub(0x00,0x00,0x00);	/* black		*/
	else if(c=='L' || c=='l')	glColor3ub(0x32,0xcd,0x32);					/* lime			*/
	else if(c=='M' || c=='m')	glColor3ub(0xff,0x00,0xff);					/* magenta	*/
	else if(c=='N' || c=='n')	glColor3ub(0x00,0x00,0x80);					/* navy			*/
	else if(c=='O' || c=='o' || c=='3')	glColor3ub(0xff,0xaf,0x00);	/* orange		*/
	else if(c=='P' || c=='p')	glColor3ub(0x80,0x00,0x80);					/* purple		*/
	else if(c=='Q' || c=='q')	glColor3ub(0xd9,0xd9,0xf3);					/* quartz		*/
	else if(c=='R' || c=='r' || c=='2')	glColor3ub(0xff,0x00,0x00);	/* red			*/
	else if(c=='S' || c=='s')	glColor3ub(0x8e,0xdb,0x23);					/* sienna		*/
	else if(c=='T' || c=='t')	glColor3ub(0x00,0x80,0x80);					/* teal			*/
	else if(c=='U' || c=='u')	glColor3ub(0x20,0x00,0x20);					/* ultraviolet */
	else if(c=='V' || c=='v' || c=='7')	glColor3ub(0xee,0x82,0xee);	/* violet		*/
	else if(c=='W' || c=='w' || c=='9')	glColor3ub(0xff,0xff,0xff);	/* white		*/
	else if(c=='X' || c=='x')	glColor3ub(intrand(0xff),intrand(0xff),intrand(0xff));	/* random	*/
	else if(c=='Y' || c=='y' || c=='4')	glColor3ub(0xff,0xff,0x00);	/* yellow		*/
	else if(c=='Z' || c=='z')	glColor3ub(intrand(2)*0xff,intrand(2)*0xff,intrand(2)*0xff);	/* random	*/
	else if(c=='1')	glColor3ub(0x40,0x40,0x20);									/* brown		*/
	else if(c=='-')	glColor3ub(0xc0,0xc0,0xc0);									/* silver		*/
	else if(c=='+')	glColor3ub(0xff,0xd7,0x20);									/* gold		*/
	else glColor3ub(0x00,0x00,0x00);														/* black		*/
	return; }

#else

void gl2SetColor(char c) {
	return; }

#endif


/* gl2FindRotate */
float gl2FindRotate(float *vect1,float *vect2,float *axis) {
	float angle,dot;

	axis[0]=vect1[1]*vect2[2]-vect1[2]*vect2[1];
	axis[1]=vect1[2]*vect2[0]-vect1[0]*vect2[2];
	axis[2]=vect1[0]*vect2[1]-vect1[1]*vect2[0];
	dot=vect1[0]*vect2[0]+vect1[1]*vect2[1]+vect1[2]*vect2[2];
	angle=acos(dot)*180/PI;
	if(angle!=0 && angle!=180) return angle;
	axis[0]=0;
	axis[1]=vect1[2];
	axis[2]=-vect1[1];
	dot=axis[1]*axis[1]+axis[2]*axis[2];
	if(dot>0) return angle;
	axis[0]=-vect1[2];
	axis[1]=0;
	axis[2]=vect1[0];
	return angle; }


/* gl2FindRotateD */
double gl2FindRotateD(double *vect1,double *vect2,double *axis) {
	double angle,dot;

	axis[0]=vect1[1]*vect2[2]-vect1[2]*vect2[1];
	axis[1]=vect1[2]*vect2[0]-vect1[0]*vect2[2];
	axis[2]=vect1[0]*vect2[1]-vect1[1]*vect2[0];
	dot=vect1[0]*vect2[0]+vect1[1]*vect2[1]+vect1[2]*vect2[2];
	angle=acos(dot)*180/PI;
	if(angle!=0 && angle!=180) return angle;
	axis[0]=0;
	axis[1]=vect1[2];
	axis[2]=-vect1[1];
	dot=axis[1]*axis[1]+axis[2]*axis[2];
	if(dot>0) return angle;
	axis[0]=-vect1[2];
	axis[1]=0;
	axis[2]=vect1[0];
	return angle; }


/* gl2DrawBox */
#ifdef __gl_h_

void gl2DrawBox(float *pt1,float *pt2,int dim) {
	if(dim==1) {
		glBegin(GL_LINES);
			glVertex3f(pt1[0],pt1[1],pt1[2]);
			glVertex3f(pt2[0],pt1[1],pt1[2]);
		glEnd(); }
	else if(dim==2) {
		glBegin(GL_LINE_LOOP);
			glVertex3f(pt1[0],pt1[1],pt1[2]);
			glVertex3f(pt2[0],pt1[1],pt1[2]);
			glVertex3f(pt2[0],pt2[1],pt1[2]);
			glVertex3f(pt1[0],pt2[1],pt1[2]);
		glEnd(); }
	else {
		glBegin(GL_LINE_STRIP);
			glVertex3f(pt1[0],pt1[1],pt1[2]);
			glVertex3f(pt1[0],pt1[1],pt2[2]);
			glVertex3f(pt1[0],pt2[1],pt2[2]);
			glVertex3f(pt1[0],pt2[1],pt1[2]);
			glVertex3f(pt1[0],pt1[1],pt1[2]);
			glVertex3f(pt2[0],pt1[1],pt1[2]);
			glVertex3f(pt2[0],pt2[1],pt1[2]);
			glVertex3f(pt2[0],pt2[1],pt2[2]);
			glVertex3f(pt2[0],pt1[1],pt2[2]);
			glVertex3f(pt2[0],pt1[1],pt1[2]);
		glEnd();
		glBegin(GL_LINES);
			glVertex3f(pt1[0],pt1[1],pt2[2]);glVertex3f(pt2[0],pt1[1],pt2[2]);
			glVertex3f(pt1[0],pt2[1],pt2[2]);glVertex3f(pt2[0],pt2[1],pt2[2]);
			glVertex3f(pt1[0],pt2[1],pt1[2]);glVertex3f(pt2[0],pt2[1],pt1[2]);
		glEnd(); }
	return; }

#else

void gl2DrawBox(float *pt1,float *pt2,int dim) {
	return; }

#endif


/* gl2DrawBoxD */
#ifdef __gl_h_

void gl2DrawBoxD(double *pt1,double *pt2,int dim) {
	if(dim==1) {
		glBegin(GL_LINES);
			glVertex3d(pt1[0],pt1[1],pt1[2]);
			glVertex3d(pt2[0],pt1[1],pt1[2]);
		glEnd(); }
	else if(dim==2) {
		glBegin(GL_LINE_LOOP);
			glVertex3d(pt1[0],pt1[1],pt1[2]);
			glVertex3d(pt2[0],pt1[1],pt1[2]);
			glVertex3d(pt2[0],pt2[1],pt1[2]);
			glVertex3d(pt1[0],pt2[1],pt1[2]);
		glEnd(); }
	else {
		glBegin(GL_LINE_STRIP);
			glVertex3d(pt1[0],pt1[1],pt1[2]);
			glVertex3d(pt1[0],pt1[1],pt2[2]);
			glVertex3d(pt1[0],pt2[1],pt2[2]);
			glVertex3d(pt1[0],pt2[1],pt1[2]);
			glVertex3d(pt1[0],pt1[1],pt1[2]);
			glVertex3d(pt2[0],pt1[1],pt1[2]);
			glVertex3d(pt2[0],pt2[1],pt1[2]);
			glVertex3d(pt2[0],pt2[1],pt2[2]);
			glVertex3d(pt2[0],pt1[1],pt2[2]);
			glVertex3d(pt2[0],pt1[1],pt1[2]);
		glEnd();
		glBegin(GL_LINES);
			glVertex3d(pt1[0],pt1[1],pt2[2]);glVertex3d(pt2[0],pt1[1],pt2[2]);
			glVertex3d(pt1[0],pt2[1],pt2[2]);glVertex3d(pt2[0],pt2[1],pt2[2]);
			glVertex3d(pt1[0],pt2[1],pt1[2]);glVertex3d(pt2[0],pt2[1],pt1[2]);
		glEnd(); }
	return; }

#else

void gl2DrawBoxD(double *pt1,double *pt2,int dim) {
	return; }

#endif


/* gl2DrawGrid */
#ifdef __gl_h_

void gl2DrawGrid(float *pt1,float *pt2,int *n,int dim) {
	float delta1,delta2;
	int i,j;

	if(dim==1) {
		glBegin(GL_POINTS);
		delta1=(pt2[0]-pt1[0])/n[0];
		for(i=0;i<=n[0];i++)
			glVertex3f(pt1[0]+i*delta1,pt1[1],pt1[2]);
		glEnd(); }
	else if(dim==2) {
		glBegin(GL_LINES);
		delta1=(pt2[1]-pt1[1])/n[1];
		for(i=0;i<=n[1];i++) {
			glVertex3f(pt1[0],pt1[1]+i*delta1,pt1[2]);
			glVertex3f(pt2[0],pt1[1]+i*delta1,pt1[2]); }
		delta1=(pt2[0]-pt1[0])/n[0];
		for(i=0;i<=n[0];i++) {
			glVertex3f(pt1[0]+i*delta1,pt1[1],pt1[2]);
			glVertex3f(pt1[0]+i*delta1,pt2[1],pt1[2]); }
		glEnd(); }
	else if(dim==3) {
		glBegin(GL_LINES);
		delta1=(pt2[1]-pt1[1])/n[1];
		delta2=(pt2[2]-pt1[2])/n[2];
		for(i=0;i<=n[1];i++)
			for(j=0;j<=n[2];j++) {
				glVertex3f(pt1[0],pt1[1]+i*delta1,pt1[2]+j*delta2);
				glVertex3f(pt2[0],pt1[1]+i*delta1,pt1[2]+j*delta2); }
		delta1=(pt2[0]-pt1[0])/n[0];
		delta2=(pt2[2]-pt1[2])/n[2];
		for(i=0;i<=n[0];i++)
			for(j=0;j<=n[2];j++) {
				glVertex3f(pt1[0]+i*delta1,pt1[1],pt1[2]+j*delta2);
				glVertex3f(pt1[0]+i*delta1,pt2[1],pt1[2]+j*delta2); }
		delta1=(pt2[0]-pt1[0])/n[0];
		delta2=(pt2[1]-pt1[1])/n[1];
		for(i=0;i<=n[0];i++)
			for(j=0;j<=n[1];j++) {
				glVertex3f(pt1[0]+i*delta1,pt1[1]+j*delta2,pt1[2]);
				glVertex3f(pt1[0]+i*delta1,pt1[1]+j*delta2,pt2[2]); }
		glEnd(); }
	return; }

#else

void gl2DrawGrid(float *pt1,float *pt2,int *n,int dim) {
	return; }

#endif


/* gl2DrawGridD */
#ifdef __gl_h_

void gl2DrawGridD(double *pt1,double *pt2,int *n,int dim) {
	double delta1,delta2;
	int i,j;

	if(dim==1) {
		glBegin(GL_POINTS);
		delta1=(pt2[0]-pt1[0])/n[0];
		for(i=0;i<=n[0];i++)
			glVertex3d(pt1[0]+i*delta1,pt1[1],pt1[2]);
		glEnd(); }
	else if(dim==2) {
		glBegin(GL_LINES);
		delta1=(pt2[1]-pt1[1])/n[1];
		for(i=0;i<=n[1];i++) {
			glVertex3d(pt1[0],pt1[1]+i*delta1,pt1[2]);
			glVertex3d(pt2[0],pt1[1]+i*delta1,pt1[2]); }
		delta1=(pt2[0]-pt1[0])/n[0];
		for(i=0;i<=n[0];i++) {
			glVertex3d(pt1[0]+i*delta1,pt1[1],pt1[2]);
			glVertex3d(pt1[0]+i*delta1,pt2[1],pt1[2]); }
		glEnd(); }
	else if(dim==3) {
		glBegin(GL_LINES);
		delta1=(pt2[1]-pt1[1])/n[1];
		delta2=(pt2[2]-pt1[2])/n[2];
		for(i=0;i<=n[1];i++)
			for(j=0;j<=n[2];j++) {
				glVertex3d(pt1[0],pt1[1]+i*delta1,pt1[2]+j*delta2);
				glVertex3d(pt2[0],pt1[1]+i*delta1,pt1[2]+j*delta2); }
		delta1=(pt2[0]-pt1[0])/n[0];
		delta2=(pt2[2]-pt1[2])/n[2];
		for(i=0;i<=n[0];i++)
			for(j=0;j<=n[2];j++) {
				glVertex3d(pt1[0]+i*delta1,pt1[1],pt1[2]+j*delta2);
				glVertex3d(pt1[0]+i*delta1,pt2[1],pt1[2]+j*delta2); }
		delta1=(pt2[0]-pt1[0])/n[0];
		delta2=(pt2[1]-pt1[1])/n[1];
		for(i=0;i<=n[0];i++)
			for(j=0;j<=n[1];j++) {
				glVertex3d(pt1[0]+i*delta1,pt1[1]+j*delta2,pt1[2]);
				glVertex3d(pt1[0]+i*delta1,pt1[1]+j*delta2,pt2[2]); }
		glEnd(); }
	return; }

#else

void gl2DrawGridD(double *pt1,double *pt2,int *n,int dim) {
	return; }

#endif


/* gl2DrawCircle */
#ifdef __gl_h_

void gl2DrawCircle(float *cent,float radius,int slices,char style,int dim) {
	int i;
	float theta,dtheta;

	if(style=='g') {
		gl2DrawCircle(cent,radius,slices,'f',dim);
		gl2DrawCircle(cent,radius,slices,'e',dim);
		return; }

	dtheta=2*PI/slices;
	if(style=='f') {
		glBegin(GL_TRIANGLE_FAN);
		if(dim==2) glVertex2fv(cent);
		else {
			glNormal3f(0,0,1);
			glVertex3fv(cent); }}
	else if(style=='e') glBegin(GL_LINE_LOOP);
	else glBegin(GL_POINTS);
	if(dim==2)
		for(i=0;i<slices;i++) {
			theta=i*dtheta;
			glVertex2f(cent[0]+radius*cos(theta),cent[1]+radius*sin(theta)); }
	else
		for(i=0;i<slices;i++) {
			theta=i*dtheta;
			glVertex3f(cent[0]+radius*cos(theta),cent[1]+radius*sin(theta),cent[2]); }
	if(style=='e' || style=='f') {
		theta=i*dtheta;
		if(dim==2) glVertex2f(cent[0]+radius*cos(theta),cent[1]+radius*sin(theta));
		else glVertex3f(cent[0]+radius*cos(theta),cent[1]+radius*sin(theta),cent[2]); }
	glEnd();

	return; }

#else

void gl2DrawCircle(float *cent,float radius,int slices,char style,int dim) {
	return; }

#endif


/* gl2DrawCircleD */
#ifdef __gl_h_

void gl2DrawCircleD(double *cent,double radius,int slices,char style,int dim) {
	int i;
	double theta,dtheta;

	if(style=='g') {
		gl2DrawCircleD(cent,radius,slices,'f',dim);
		gl2DrawCircleD(cent,radius,slices,'e',dim);
		return; }

	dtheta=2*PI/slices;
	if(style=='f') {
		glBegin(GL_TRIANGLE_FAN);
		if(dim==2) glVertex2dv(cent);
		else {
			glNormal3d(0,0,1);
			glVertex3dv(cent); }}
	else if(style=='e') glBegin(GL_LINE_LOOP);
	else glBegin(GL_POINTS);
	if(dim==2)
		for(i=0;i<slices;i++) {
			theta=i*dtheta;
			glVertex2d(cent[0]+radius*cos(theta),cent[1]+radius*sin(theta)); }
	else
		for(i=0;i<slices;i++) {
			theta=i*dtheta;
			glVertex3d(cent[0]+radius*cos(theta),cent[1]+radius*sin(theta),cent[2]); }
	if(style=='e' || style=='f') {
		theta=i*dtheta;
		if(dim==2) glVertex2d(cent[0]+radius*cos(theta),cent[1]+radius*sin(theta));
		else glVertex3d(cent[0]+radius*cos(theta),cent[1]+radius*sin(theta),cent[2]); }
	glEnd();
	return; }

#else

void gl2DrawCircleD(double *cent,double radius,int slices,char style,int dim) {
	return; }

#endif


/* gl2DrawArc */
#ifdef __gl_h_

void gl2DrawArc(float *cent,float radius,float theta1,float theta2,int slices,char style,int dim) {
	int i,imax;
	float theta,dtheta;

	dtheta=2.0*PI/slices;
	imax=(int)((theta2-theta1)/dtheta+0.5);
	dtheta=(theta2-theta1)/(float)imax;
	if(style=='f' || style=='g') {
		glBegin(GL_TRIANGLE_FAN);
		if(dim==2) glVertex2fv(cent);
		else {
			glNormal3f(0,0,1);
			glVertex3fv(cent); }}
	else if(style=='e') glBegin(GL_LINE_STRIP);
	else glBegin(GL_POINTS);
	if(dim==2)
		for(i=0;i<=imax;i++) {
			theta=theta1+i*dtheta;
			glVertex2f(cent[0]+radius*cos(theta),cent[1]+radius*sin(theta)); }
	else
		for(i=0;i<=imax;i++) {
			theta=theta1+i*dtheta;
			glVertex3f(cent[0]+radius*cos(theta),cent[1]+radius*sin(theta),cent[2]); }
	glEnd();
	return; }

#else

void gl2DrawArc(float *cent,float radius,float theta1,float theta2,int slices,char style,int dim) {
	return; }

#endif


/* gl2DrawArcD */
#ifdef __gl_h_

void gl2DrawArcD(double *cent,double radius,double theta1,double theta2,int slices,char style,int dim) {
	int i,imax;
	double theta,dtheta;

	dtheta=2.0*PI/slices;
	imax=(int)((theta2-theta1)/dtheta+0.5);
	dtheta=(theta2-theta1)/(double)imax;
	if(style=='f' || style=='g') {
		glBegin(GL_TRIANGLE_FAN);
		if(dim==2) glVertex2dv(cent);
		else {
			glNormal3d(0,0,1);
			glVertex3dv(cent); }}
	else if(style=='e') glBegin(GL_LINE_STRIP);
	else glBegin(GL_POINTS);
	if(dim==2)
		for(i=0;i<=imax;i++) {
			theta=theta1+i*dtheta;
			glVertex2d(cent[0]+radius*cos(theta),cent[1]+radius*sin(theta)); }
	else
		for(i=0;i<=imax;i++) {
			theta=theta1+i*dtheta;
			glVertex3d(cent[0]+radius*cos(theta),cent[1]+radius*sin(theta),cent[2]); }
	glEnd();
	return; }

#else

void gl2DrawArcD(double *cent,double radius,double theta1,double theta2,int slices,char style,int dim) {
	return; }

#endif


/* gl2DrawHemisphere */
#ifdef __gl_h_

void gl2DrawHemisphere(float radius,int slices,int stacks,int frontin,int normals) {
	int ilong,ilat,ilong1,ilong2,dilong;
	float dtheta,dphi,r1,r2,z1,z2,cosphi,sinphi,normfact;

	if(frontin) {
		ilong1=0;
		ilong2=slices+1;
		dilong=1; }
	else {
		ilong1=slices;
		ilong2=-1;
		dilong=-1; }

	dtheta=PI/2.0/stacks;
	dphi=2.0*PI/slices;
	r2=radius;
	z2=0;
	normfact=1.0/radius*(frontin?-1.0:1.0);

	for(ilat=1;ilat<stacks;ilat++) {
		r1=r2;												// r1 is radius of bottom edge (starts on xy plane)
		z1=z2;												// z1 is height of bottom edge
		r2=radius*cos(ilat*dtheta);		// r2 is radius of top edge
		z2=radius*sin(ilat*dtheta);		// z2 is height of top edge
		glBegin(GL_QUAD_STRIP);
		if(!normals)
			for(ilong=ilong1;ilong!=ilong2;ilong+=dilong) {
				cosphi=cos(ilong*dphi);
				sinphi=sin(ilong*dphi);
				glVertex3f(r1*cosphi,r1*sinphi,z1);
				glVertex3f(r2*cosphi,r2*sinphi,z2); }
		else
			for(ilong=ilong1;ilong!=ilong2;ilong+=dilong) {
				cosphi=cos(ilong*dphi);
				sinphi=sin(ilong*dphi);
				glNormal3f(r1*normfact*cosphi,r1*normfact*sinphi,z1*normfact);
				glVertex3f(r1*cosphi,r1*sinphi,z1);
				glNormal3f(r2*normfact*cosphi,r2*normfact*sinphi,z2*normfact);
				glVertex3f(r2*cosphi,r2*sinphi,z2); }
		glEnd(); }

	r1=r2;
	z1=z2;
	glBegin(GL_TRIANGLE_FAN);
	if(normals) {
		if(frontin)
			glNormal3f(0,0,-1);
		else
			glNormal3f(0,0,1); }
	glVertex3f(0,0,radius);
	if(!normals)
		for(ilong=ilong2;ilong!=ilong1;ilong-=dilong) {
			glVertex3f(r1*cos(ilong*dphi),r1*sin(ilong*dphi),z1); }
	else {
		for(ilong=ilong2;ilong!=ilong1;ilong-=dilong) {
			cosphi=cos(ilong*dphi);
			sinphi=sin(ilong*dphi);
			glNormal3f(r1*normfact*cosphi,r1*normfact*sinphi,z1*normfact);
			glVertex3f(r1*cosphi,r1*sinphi,z1); }}
	glEnd();

	return; }

#else

void gl2DrawHemisphere(float radius,int slices,int stacks,int frontin,int normals) {
	return; }

#endif


/* gl2DrawCylinder */
#ifdef __gl_h_

void gl2DrawCylinder(float baseRadius,float topRadius,float height,int slices,int stacks,int frontin,int normals) {
	int ilen,irad,irad1,irad2,dirad;
	float dtheta,dlen,z1,z2,r1,r2,costheta,sintheta,normxy,normz;

	if(frontin) {irad1=0;irad2=slices+1;dirad=1;}
	else {irad1=slices;irad2=-1;dirad=-1;}

	dtheta=2.0*PI/slices;
	dlen=height/stacks;
	z2=0;
	r2=baseRadius;
	if(baseRadius==topRadius) {
		normxy=frontin?-1.0:1.0;
		normz=0; }
	else {
		normxy=(frontin?-1.0:1.0)/sqrt((baseRadius-topRadius)*(baseRadius-topRadius)+height*height);
		normz=(baseRadius-topRadius)*normxy;
		normxy*=height; }

	for(ilen=1;ilen<=stacks;ilen++) {
		z1=z2;																	// z1 is bottom height, starts on xy plane
		r1=r2;																	// r1 is bottom radius
		z2=ilen*dlen;														// z2 is top height
		r2=baseRadius*(1.0-z2/height)+topRadius*z2/height;	// r2 is top radius
		glBegin(GL_QUAD_STRIP);
		if(!normals)
			for(irad=irad1;irad!=irad2;irad+=dirad) {
				costheta=cos(irad*dtheta);
				sintheta=sin(irad*dtheta);
				glVertex3f(r1*costheta,r1*sintheta,z1);
				glVertex3f(r2*costheta,r2*sintheta,z2); }
		else
			for(irad=irad1;irad!=irad2;irad+=dirad) {
				costheta=cos(irad*dtheta);
				sintheta=sin(irad*dtheta);
				glNormal3f(costheta*normxy,sintheta*normxy,normz);
				glVertex3f(r1*costheta,r1*sintheta,z1);
				glVertex3f(r2*costheta,r2*sintheta,z2); }
		glEnd(); }
	return; }

#else

void gl2DrawCylinder(float baseRadius,float topRadius,float height,int slices,int stacks,int frontin,int normals) {
	return; }

#endif


/* gl2DrawSphere */
#ifdef __gl_h_

void gl2DrawSphere(float radius,int slices,int stacks,int frontin,int normals) {
	glMatrixMode(GL_MODELVIEW);
	glPushMatrix();
	gl2DrawHemisphere(radius,slices,stacks/2,frontin,normals);
	glRotatef(180,1,0,0);
	gl2DrawHemisphere(radius,slices,stacks/2,frontin,normals);
	glPopMatrix();
	return; }

#else

void gl2DrawSphere(float radius,int slices,int stacks,int frontin,int normals) {
	return; }

#endif


/* gl2DrawEcoli */
#ifdef __gl_h_

void gl2DrawEcoli(float radius,float length,int slices,int stacks,int frontin,int normals) {
	int endstacks;

	endstacks=(int)(radius*PI/2.0/length*stacks);
	length-=2*radius;
	if(length<0) length=0;
	glMatrixMode(GL_MODELVIEW);
	glPushMatrix();
	glTranslatef(0,0,length/2);
	gl2DrawHemisphere(radius,slices,endstacks,frontin,normals);
	glTranslatef(0,0,-length);
	if(length) gl2DrawCylinder(radius,radius,length,slices,stacks-2*endstacks,frontin,normals);
	glRotatef(180,1,0,0);
	gl2DrawHemisphere(radius,slices,endstacks,frontin,normals);
	glPopMatrix();
	return; }

#else

void gl2DrawEcoli(float radius,float length,int slices,int stacks,int frontin,int normals) {
	return; }

#endif


/* gl2PlotData */
// style is 3 characters per data set: style,width,color
#ifdef __gl_h_

void gl2PlotData(float *xdata,float *ydata,int nx,int nycol,char *style) {
	int i,j;

	for(j=0;j<nycol;j++) {
		if(style[3*j]==' ');				// not drawn
		else if(style[3*j]=='-') {	// solid line
			glLineWidth(style[3*j+1]-'0');
			gl2SetColor(style[3*j+2]);
			glBegin(GL_LINE_STRIP);
			for(i=0;i<nx;i++)
				glVertex3f(xdata[i],ydata[i*nycol+j],0);
			glEnd(); }
		else if(style[3*j]=='.') {	// dots
			glPointSize(style[3*j+1]-'0');
			gl2SetColor(style[3*j+2]);
			glBegin(GL_POINTS);
			for(i=0;i<nx;i++)
				glVertex3f(xdata[i],ydata[i*nycol+j],0);
			glEnd(); }
		else if(style[3*j]=='h') {	// histogram  **** NEEDS WORK ****
			glLineWidth(style[3*j+1]-'0');
			gl2SetColor(style[3*j+2]);
			glBegin(GL_LINE_STRIP);
			glVertex3f(xdata[0],0,0);
			for(i=0;i<nx-1;i++) {
				glVertex3f(xdata[i],ydata[i*nycol+j],0);
				glVertex3f(xdata[i+1],ydata[i*nycol+j],0); }
			if(nx>1) {
				glVertex3f(xdata[i],ydata[i*nycol+j],0);
				glVertex3f(2*xdata[i]-xdata[i-1],ydata[i*nycol+j],0);
				glVertex3f(2*xdata[i]-xdata[i-1],0,0); }
			glEnd(); }}
	return; }

#else

void gl2PlotData(float *xdata,float *ydata,int nx,int nycol,char *style) {
	return; }

#endif


/* gl2PlotPts */
#ifdef __gl_h_

void gl2PlotPts(float **data,int *ser,int nser,int npts,float **color,float *size,char style) {
	int i,j;

	if(style==' ');
	else if(style=='.') {
		for(j=0;j<nser;j++) {
			if(size[j]>0) {
				glPointSize(size[j]);
				glColor3f(color[j][0],color[j][1],color[j][2]);
				glBegin(GL_POINTS);
				for(i=0;i<npts;i++)
					if(ser[i]==j) glVertex3f(data[i][0],data[i][1],data[i][2]);
				glEnd(); }}}
	else if(style=='-') {
		for(j=0;j<nser;j++) {
			if(size[j]>0) {
				glLineWidth(size[j]);
				glColor3f(color[j][0],color[j][1],color[j][2]);
				glBegin(GL_LINE_STRIP);
				for(i=0;i<npts;i++)
					if(ser[i]==j) glVertex3f(data[i][0],data[i][1],data[i][2]);
				glEnd(); }}}
	else if(style=='s') {
		glMatrixMode(GL_MODELVIEW);
		for(j=0;j<nser;j++) {
			if(size[j]>0) {
				glColor3f(color[j][0],color[j][1],color[j][2]);
				for(i=0;i<npts;i++)
					if(ser[i]==j) {
						glPushMatrix();
						glTranslatef(data[i][0],data[i][1],data[i][2]);
						glutSolidSphere(size[j],15,15);
						glPopMatrix(); }}}}
	return; }

#else

void gl2PlotPts(float **data,int *ser,int nser,int npts,float **color,float *size,char style) {
	return; }

#endif


/* gl2PlotPtsD */
#ifdef __gl_h_

void gl2PlotPtsD(double **data,int *ser,int nser,int npts,double **color,double *size,char style) {
	int i,j;

	if(style==' ');
	else if(style=='.') {
		for(j=0;j<nser;j++) {
			if(size[j]>0) {
				glPointSize(size[j]);
				glColor3d(color[j][0],color[j][1],color[j][2]);
				glBegin(GL_POINTS);
				for(i=0;i<npts;i++)
					if(ser[i]==j) glVertex3d(data[i][0],data[i][1],data[i][2]);
				glEnd(); }}}
	else if(style=='-') {
		for(j=0;j<nser;j++) {
			if(size[j]>0) {
				glLineWidth(size[j]);
				glColor3d(color[j][0],color[j][1],color[j][2]);
				glBegin(GL_LINE_STRIP);
				for(i=0;i<npts;i++)
					if(ser[i]==j) glVertex3d(data[i][0],data[i][1],data[i][2]);
				glEnd(); }}}
	else if(style=='s') {
		glMatrixMode(GL_MODELVIEW);
		for(j=0;j<nser;j++) {
			if(size[j]>0) {
				glColor3d(color[j][0],color[j][1],color[j][2]);
				for(i=0;i<npts;i++)
					if(ser[i]==j) {
						glPushMatrix();
						glTranslated(data[i][0],data[i][1],data[i][2]);
						glutSolidSphere(size[j],15,15);
						glPopMatrix(); }}}}
	return; }

#else

void gl2PlotPtsD(double **data,int *ser,int nser,int npts,double **color,double *size,char style) {
	return; }

#endif


/* gl2PlotSurf */
#ifdef __gl_h_

void gl2PlotSurf(float *xdata,float *ydata,float **zdata,int nx,int ny,int nz,char *style) {
	int ix,iy,iz,ic;
	float dxlo,dxhi,dylo,dyhi;
	float col1[64][4],col2[4];

	if(style[0]=='s') {
		if(nz>64) nz=64;
		for(iz=0;iz<nz;iz++) {
			gl2SetColor(style[iz+1]);
			glGetFloatv(GL_CURRENT_COLOR,col1[iz]); }
		for(iy=0;iy<ny;iy++) {
			if(ny>1) {
				if(iy==0) dylo=dyhi=0.5*(ydata[1]-ydata[0]);
				else if(iy==ny-1) dylo=dyhi=0.5*(ydata[ny-1]-ydata[ny-2]);
				else {
					dylo=0.5*(ydata[iy]-ydata[iy-1]);
					dyhi=0.5*(ydata[iy+1]-ydata[iy]); }}
			else dylo=dyhi=0.5*(ClipTop-ClipBot);
			for(ix=0;ix<nx;ix++) {
				if(nx>1) {
					if(ix==0) dxlo=dxhi=0.5*(xdata[1]-xdata[0]);
					else if(ix==nx-1) dxlo=dxhi=0.5*(xdata[nx-1]-xdata[nx-2]);
					else {
						dxlo=0.5*(xdata[ix]-xdata[ix-1]);
						dxhi=0.5*(xdata[ix+1]-xdata[ix]); }}
				else dxlo=dxhi=0.5*(ClipRight-ClipLeft);
				for(ic=0;ic<4;ic++) col2[ic]=0;
				for(iz=0;iz<nz;iz++)
					for(ic=0;ic<4;ic++) col2[ic]+=zdata[iy*nx+ix][iz]*col1[iz][ic];
				glColor4fv(col2);
				glRectf(xdata[ix]-dxlo,ydata[iy]-dylo,xdata[ix]+dxhi,ydata[iy]+dyhi); }}}

	return; }

#else

void gl2PlotSurf(float *xdata,float *ydata,float **zdata,int nx,int ny,int nz,char *style) {
	return; }

#endif












