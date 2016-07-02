#include <CoordVect.h>
#include <IndexVect.h>
#include <math.h>
#include <istream>
#include <World.h>
#include <MeshElementNode.h>

const CoordVect CoordVect::Unit(1.0, 1.0);
const CoordVect CoordVect::Zero(0.0, 0.0);

CoordVect::CoordVect ()
{
	for (int i = 0; i < DIM; ++ i)
	{
		vect[i] = 0.0;
	}
}

CoordVect::CoordVect (double i, double j)
{
	vect[0] = i;
	vect[1] = j;
}

CoordVect::CoordVect (double* v)
{
	vect[0] = v[0];
	vect[1] = v[1];
}

CoordVect::CoordVect(std::array<double, 2> p)
{
	vect[0] = p[0];
	vect[1] = p[1];
}

CoordVect::CoordVect(moving_boundary::MeshElementNode* e)
{
	double worldValues[2] = {(*e)(spatial::cX), (*e)(spatial::cY)};
	moving_boundary::World<moving_boundary::CoordinateType, 2>::get().toProblemDomain(worldValues, vect);
}

inline CoordVect::CoordVect (const CoordVect &iv)
{
	for (int i = 0; i < DIM; ++ i)
	{
		vect[i] = iv.vect[i];
	}
}

CoordVect& CoordVect::operator= (const CoordVect &iv)
{
	for (int i = 0; i < DIM; ++ i)
	{
		vect[i] = iv.vect[i];
	}
	return *this;
}

const double& CoordVect::operator[] (int i) const
{
  return vect[i];
}

double& CoordVect::operator[] (int i)
{
  return vect[i];
}

CoordVect CoordVect::operator+ (const CoordVect& p) const
{
	CoordVect v(vect[0] + p[0], vect[1] + p[1]);
  return *this;
}

CoordVect& CoordVect::operator+= (const CoordVect& p)
{
	for (int i = 0; i < 1; ++ i)
	{
		vect[i] += p.vect[i];
	}
  return *this;
}

CoordVect CoordVect::operator- (const CoordVect& p) const
{
	CoordVect v(vect[0] - p[0], vect[1] - p[1]);
  return v;
}

inline CoordVect& CoordVect::operator-= (const CoordVect& p)
{
	for (int i = 0; i < 1; ++ i)
	{
		vect[i] -= p.vect[i];
	}
  return *this;
}

CoordVect CoordVect::operator* (double s) const
{
  CoordVect v(vect[0] * s, vect[1] * s);
  return v;
}

CoordVect& CoordVect::operator*= (double s)
{
	for (int i = 0; i < DIM; ++ i)
	{
		vect[i] *= s;
	}
	return *this;
}

CoordVect CoordVect::operator/ (double s) const
{
  CoordVect v(vect[0] / s, vect[1] / s);
  return v ;
}

CoordVect& CoordVect::operator/= (double s)
{
	for (int i = 0; i < DIM; ++ i)
	{
		vect[i] /= s;
	}
	return *this;
}

CoordVect CoordVect::operator/ (const CoordVect& p) const
{
	CoordVect v(vect[0] / p.vect[0], vect[1] / p.vect[1]);
	return v;
}

CoordVect CoordVect::operator/ (const IndexVect& p) const
{
	CoordVect v(vect[0] / p[0], vect[1] / p[1]);
	return v;
}

CoordVect CoordVect::normalize () const
{
	double len = length();
	return (*this)/len;
}

double CoordVect::length() const
{
	return sqrt(vect[0] * vect[0] + vect[1] * vect[1]);
}

double CoordVect::distance2(CoordVect& p) const
{
	return (vect[0] - p.vect[0]) * (vect[0] - p.vect[0])
			+ (vect[1] - p.vect[1]) * (vect[1] - p.vect[1]);
}

std::ostream& operator<< (std::ostream& ostr, const CoordVect& p)
{
  ostr << "(" << p[0] << "," << p[1] << ")" ;
  return ostr;
}
