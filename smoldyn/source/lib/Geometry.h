/* Steven Andrews 2/17/06
See documentation called Geometry_doc.doc.
Copyright 2006-2007 by Steven Andrews.  This work is distributed under the terms
of the Gnu Lesser General Public License (LGPL). */


#ifndef __Geometry_h
#define __Geometry_h

// Center
void Geo_LineCenter(double **point,double *cent,int dim);
void Geo_RectCenter(double **point,double *cent,int dim);
void Geo_TriCenter(double **point,double *cent,int dim);

// Normal
double Geo_LineNormal(double *pt1,double *pt2,double *ans);
double Geo_LineNormal2D(double *pt1,double *pt2,double *point,double *ans);
double Geo_LineNormal3D(double *pt1,double *pt2,double *point,double *ans);
double Geo_LineNormPos(double *pt1,double *pt2,double *point,int dim,double *distptr);
double Geo_TriNormal(double *pt1,double *pt2,double *pt3,double *ans);
double Geo_SphereNormal(double *cent,double *pt,int front,int dim,double *ans);

// Area
double Geo_LineLength(double *pt1,double *pt2,int dim);
double Geo_TriArea2(double *pt1,double *pt2,double *pt3);
double Geo_TriArea3(double *pt1,double *pt2,double *pt3,double *norm);
double Geo_QuadArea(double *pt1,double *pt2,double *pt3,double *pt4,int dim);

// Inside point
double Geo_InsidePoints2(double *pt1,double *pt2,double margin,double *ans1,double *ans2,int dim);
void Geo_InsidePoints3(double *pt1,double *pt2,double *pt3,double margin,double *ans1,double *ans2,double *ans3);

// Point in
int Geo_PtInTriangle(double *pt1,double *pt2,double *pt3,double *norm,double *test);
int Geo_PtInSlab(double *pt1,double *pt2,double *test,int dim);
int Geo_PtInSphere(double *test,double *cent,double rad,int dim);

// Nearest
void Geo_NearestSlabPt(double *pt1,double *pt2,double *point,double *ans,int dim);
void Geo_NearestLineSegPt(double *pt1,double *pt2,double *point,double*ans,int dim);
void Geo_NearestTriPt(double *pt1,double *pt2,double *pt3,double *norm,double *point,double *ans);
void Geo_NearestTrianglePt(double *pt1,double *pt2,double *pt3,double *norm,double *point,double *ans);
double Geo_NearestSpherePt(double *cent,double rad,int front,int dim,double *point,double *ans);
void Geo_NearestRingPt(double *cent,double *axis,double rad,int dim,double *point,double *ans);
void Geo_NearestCylPt(double *pt1,double *axis,double rad,int dim,double *point,double *ans);
void Geo_NearestCylinderPt(double *pt1,double *pt2,double rad,int dim,double *point,double *ans);
void Geo_NearestDiskPt(double *cent,double *axis,double rad,int dim,double *point,double *ans);

// To Rect
void Geo_Semic2Rect(double *cent,double rad,double *outvect,double *r1,double *r2,double *r3);
void Geo_Hemis2Rect(double *cent,double rad,double *outvect,double *r1,double *r2,double *r3,double *r4);
void Geo_Cyl2Rect(double *pt1,double *pt2,double rad,double *r1,double *r2,double *r3,double *r4);

// Cross
double Geo_LineXLine(double *l1p1,double *l1p2,double *l2p1,double *l2p2,double *crss2ptr);
double Geo_LineXSphs(double *pt1,double *pt2,double *cent,double rad,int dim,double *crss2ptr,double *nrdistptr,double *nrposptr);
double Geo_LineXCyl2s(double *pt1,double *pt2,double *cp1,double *cp2,double *norm,double rad,double *crss2ptr,double *nrdistptr,double *nrposptr);
double Geo_LineXCyls(double *pt1,double *pt2,double *cp1,double *cp2,double rad,double *crss2ptr,double *nrdistptr,double *nrposptr);

// Cross aabbb
int Geo_LineXaabb2(double *pt1,double *pt2,double *norm,double *bpt1,double *bpt2);
int Geo_LineXaabb(double *pt1,double *pt2,double *bpt1,double *bpt2,int dim,int infline);
int Geo_TriXaabb3(double *pt1,double *pt2,double *pt3,double *norm,double *bpt1,double *bpt2);
int Geo_RectXaabb2(double *r1,double *r2,double *r3,double *bpt1,double *bpt2);
int Geo_RectXaabb3(double *r1,double *r2,double *r3,double *r4,double *bpt1,double *bpt2);
int Geo_CircleXaabb2(double *cent,double rad,double *bpt1,double *bpt2);
int Geo_SphsXaabb3(double *cent,double rad,double *bpt1,double *bpt2);
int Geo_CylisXaabb3(double *pt1,double *pt2,double rad,double *bpt1,double *bpt2);
int Geo_DiskXaabb3(double *cent,double rad,double *norm,double *bpt1,double *bpt2);

// Approx. cross aabb
int Geo_SemicXaabb2(double *cent,double rad,double *outvect,double *bpt1,double *bpt2);
int Geo_HemisXaabb3(double *cent,double rad,double *outvect,double *bpt1,double *bpt2);
int Geo_CylsXaabb3(double *pt1,double *pt2,double rad,double *bpt1,double *bpt2);

// Volumes
double Geo_SphVolume(double rad,int dim);
double Geo_SphOLSph(double *cent1,double *cent2,double r1,double r2,int dim);

#endif
