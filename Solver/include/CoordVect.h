#ifndef _COORDVECT_H_
#define _COORDVECT_H_

#include <array>

#define DIM 2

class IndexVect;
namespace moving_boundary
{
	struct MeshElementNode;
}

class CoordVect
{
public:
  static const CoordVect Zero;
  static const CoordVect Unit;

  CoordVect ();
  CoordVect (double i, double j);
  CoordVect (const CoordVect& rhs);
  CoordVect (double* v);
  CoordVect(std::array<double, 2> p);
  CoordVect(moving_boundary::MeshElementNode* e);

  CoordVect& operator= (const CoordVect& rhs);

  const double& operator[] (int i) const;
  double& operator[] (int i);

  CoordVect operator+ (const CoordVect& p) const;
  CoordVect& operator+= (const CoordVect& p);

  CoordVect operator- (const CoordVect& p) const;
  CoordVect& operator-= (const CoordVect& p);

  CoordVect operator* (double s) const;
  CoordVect& operator*= (double s);

  CoordVect operator/ (double s) const;
  CoordVect& operator/= (double s);
  CoordVect scale (double s) const;

  CoordVect operator/ (const CoordVect& s) const;
  CoordVect operator/ (const IndexVect& s) const;

  double length() const;
  double distance2(CoordVect& p) const;
  CoordVect normalize() const;

  friend std::ostream& operator<< (std::ostream& ostr,
                                   const CoordVect& p);

private:

  double vect[DIM];

};

#endif
