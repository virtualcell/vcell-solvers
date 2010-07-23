/*
 * (C) Copyright University of Connecticut Health Center 2001.
 * All rights reserved.
 */
#ifndef DOUBLEVECTOR3_H
#define DOUBLEVECTOR3_H

#include <math.h>
#include <iostream>
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

	double length() {
		return sqrt(x * x + y * y + z * z);
	}

	double lengthSquare() {
		return x * x + y * y + z * z;
	}

	DoubleVector3 scale(double scaler) {
		return DoubleVector3(x * scaler, y * scaler, z * scaler);
	}

	void normalize() {
		double l = length();
		if (l > 0) {
			x /= l; 
			y /= l; 
			z /= l;
		}
	}

	double dotProduct(const DoubleVector3& wc) {
		return x * wc.x + y * wc.y + z * wc.z;
	}

	DoubleVector3 crossProduct(const DoubleVector3& wc) { // cross product
		return DoubleVector3(
			y * wc.z - z * wc.y,
			z * wc.x - x * wc.z,
            x * wc.y - y * wc.x
		);
	}

	DoubleVector3 operator-(const DoubleVector3& wc) {
		return DoubleVector3(x - wc.x, y - wc.y, z - wc.z);
	}

	DoubleVector3 operator-() {
		return DoubleVector3(-x, -y, -z);
	}

	void operator-=(const DoubleVector3& wc) {
		x = x - wc.x;
		y = y - wc.y;
		z = z - wc.z;
	}

	DoubleVector3 operator+(const DoubleVector3& wc) {
		return DoubleVector3(x + wc.x, y + wc.y, z + wc.z);
	}

	void operator+=(const DoubleVector3& wc) {
		x = x + wc.x;
		y = y + wc.y;
		z = z + wc.z;
	}

	friend ostream& operator<<(ostream& os, const DoubleVector3& wc) {
		os << "[" << wc.x << "," << wc.y << "," << wc.z << "]";
		return os;
	}
};
#endif
