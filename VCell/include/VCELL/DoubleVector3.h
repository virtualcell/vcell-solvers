/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef DOUBLEVECTOR3_H
#define DOUBLEVECTOR3_H

#include <cmath>
#include <stdexcept>
#include <iostream>
#include <cstdlib>
#include <cassert>
using std::cout;
using std::ostream;

struct DoubleVector3 {
	double x;
	double y;
	double z;

	DoubleVector3() {
		x = 0.0;
		y = 0.0;
		z = 0.0;
	}

	DoubleVector3(double arg_x, double arg_y, double arg_z) {
		x = arg_x;
		y = arg_y;
		z = arg_z;
	}

	bool isAbsolutelyZero( ) const {
		return (x == 0) &&  (y == 0) && (z == 0);
	}

	double length() const {
		return sqrt(x * x + y * y + z * z);
	}

	double lengthSquared() const {
		return x * x + y * y + z * z;
	}

	DoubleVector3 scale(double scaler) const {
		return DoubleVector3(x * scaler, y * scaler, z * scaler);
	}

	DoubleVector3 & normalize() {
		double len = length();
		if (len > 0) {
			x /= len; 
			y /= len; 
			z /= len;
		}
		return *this;
	}

	double dotProduct(const DoubleVector3& wc)  const{
		return x * wc.x + y * wc.y + z * wc.z;
	}

	DoubleVector3 crossProduct(const DoubleVector3& wc) const { // cross product
		return DoubleVector3(
			y * wc.z - z * wc.y,
			z * wc.x - x * wc.z,
            x * wc.y - y * wc.x
		);
	}

	DoubleVector3 operator-(const DoubleVector3& wc) {
		return DoubleVector3(x - wc.x, y - wc.y, z - wc.z);
	}

	DoubleVector3 operator-() const {
		return DoubleVector3(-x, -y, -z);
	}

	void operator-=(const DoubleVector3& wc) {
		x = x - wc.x;
		y = y - wc.y;
		z = z - wc.z;
	}

	DoubleVector3 operator+(const DoubleVector3& wc) const {
		return DoubleVector3(x + wc.x, y + wc.y, z + wc.z);
	}

	void operator+=(const DoubleVector3& wc) {
		x = x + wc.x;
		y = y + wc.y;
		z = z + wc.z;
	}

	DoubleVector3 & operator/=(double divisor) {
		x /= divisor;
		y /= divisor;
		z /= divisor;
		return *this;
	}


	friend ostream& operator<<(ostream& os, const DoubleVector3& wc) {
		os << "[" << wc.x << "," << wc.y << "," << wc.z << "]";
		return os;
	}
};

inline bool operator==(const DoubleVector3 &lhs,const DoubleVector3 &rhs){
	return (lhs.x == rhs.x) && (lhs.y == rhs.y) && (lhs.z == rhs.z); 
}

inline bool operator!=(const DoubleVector3 &lhs,const DoubleVector3 &rhs){
	return !(lhs == rhs);
}

/**
* DoubleVector3 compatible vector which is always normal 
*/ 
class UnitVector3 {
	double x;
	double y;
	double z;
public:

	UnitVector3() {
		x = 1.0;
		y = 0.0;
		z = 0.0;
	}

	UnitVector3(double arg_x, double arg_y, double arg_z) {
		x = arg_x;
		y = arg_y;
		z = arg_z;
		normalize( );
	}

	UnitVector3(const UnitVector3 &in)
		:x(in.x),
		y(in.y),
		z(in.z) { }

	UnitVector3(const DoubleVector3 &in)
		:x(in.x),
		y(in.y),
		z(in.z) {
		normalize( );
	}
	UnitVector3 operator-(const UnitVector3& wc) {
		return UnitVector3(x - wc.x, y - wc.y, z - wc.z);
	}
	UnitVector3 operator-() const {
		return UnitVector3(-x, -y, -z);
	}

	double xvalue( ) const {
		return x;
	}

	double yvalue( ) const {
		return y;
	}

	double zvalue( ) const {
		return z;
	}

	operator DoubleVector3( ) const {
		return DoubleVector3(x,y,z);
	}

	double length() const {
		return sqrt(lengthSquared( ));
	}

	double lengthSquared() const {
		return x * x + y * y + z * z;
	}

	void normalize() {
		double len = length();
		if (len == 0) {
			throw std::invalid_argument("UnitVector3 can not be zero");
		}
		x /= len; 
		y /= len; 
		z /= len;
	}

	double dotProduct(const UnitVector3& wc)  const{
		return x * wc.x + y * wc.y + z * wc.z;
	}

	double dotProduct(const DoubleVector3& wc)  const{
		return x * wc.x + y * wc.y + z * wc.z;
	}

	DoubleVector3 crossProduct(const DoubleVector3& wc) const { // cross product
		return DoubleVector3(
			y * wc.z - z * wc.y,
			z * wc.x - x * wc.z,
            x * wc.y - y * wc.x
		);
	}

	friend ostream& operator<<(ostream& os, const UnitVector3& wc) {
		os << "[" << wc.x << "," << wc.y << "," << wc.z << "]";
		return os;
	}
};

/**
* wrapper to return an array -- should be replaced with std::array when compilers upgraded
* @param T type being returned
* @param N number in array
*/
template<class T, size_t N>
class ArrayHolder {
	T array[N];
public:
	/**
	* default, array default values for type T
	*/
	ArrayHolder( ) {}

	/**
	* intialize all values to initValue
	*/
	ArrayHolder(const T & initValue) {
		for (size_t i = 0; i < N; i++) {
			array[i] = initValue;
		}
	}

	T & operator[](size_t i) {
		return array[i];
	}

	//review
	size_t size( ) {
		return N;
	}
};

/**
* return pair of vectors perpendicular to in 
* @param in vector to get normals to
* in must not be zero
*/
inline ArrayHolder<UnitVector3,2> perpendicular(const DoubleVector3 & in) {
	assert(!in.isAbsolutelyZero( ));
	ArrayHolder<UnitVector3,2> rval; 

	const double epsilon = 1e-10;
	const double aX = std::abs(in.x);
	const double aY = std::abs(in.y);
	const double aZ = std::abs(in.z);
	//divide by largest component to minimize floating point issues
	if (aZ > aX && aZ > aY) {
		const double arbX = 1; 
		const double arbY = 1; 
		//solve for z that makes dot product zero
		const double zComponent = -(in.x*arbX + in.y*arbY)/in.z;
		rval[0] = UnitVector3(arbX,arbY,zComponent);
	} 
	else if (aX > aY && aX > aZ) {
		const double arbY = 1; 
		const double arbZ = 1; 
		const double xComponent = -(in.y*arbY + in.z*arbZ)/in.x;
		rval[0] = UnitVector3(xComponent,arbY,arbZ);
	} else {
		const double arbX = 1; 
		const double arbZ = 1; 
		const double yComponent = -(in.x*arbX + in.z*arbZ)/in.y;
		rval[0] = UnitVector3(arbX,yComponent,arbZ);
	}
	rval[1] = in.crossProduct(rval[0]);
	return rval;
}

#endif
