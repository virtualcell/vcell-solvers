#ifndef Point_h
#define Point_h
#include <cassert>
#include <iostream>
#include <array>
#include <cmath>
namespace spatial {

	/**
	* use symbols for axes to avoid confusion with array locations
	*/
	enum Axis {cX=0,cY=1,cZ=2};

	inline Axis & operator++(Axis &a) {
		a = static_cast<Axis>(a + 1);
		return a;
	}

#   pragma warning ( disable : 4351 )
	/**
	* basic point definition
	* @param T primitive type
	* @param N number of dimensions
	*/
	template<class T, int N>
	class TPoint {
	protected:
		std::array<T,N> coord; 
	public:
		//deprecated
		typedef T realType;
		typedef T value_type; 
		TPoint(const std::array<T,N> &val)  
			:coord(val) {}
		TPoint(T val)  {
			static_assert(N==1,  "N arg required");
			coord[0] = val;
		}

		TPoint(T a, T b)  {
			static_assert(N==2,  "N args required");
			coord[0] = a;
			coord[1] = b;
		}

		TPoint(T a, T b, T c) {
			static_assert(N==3,   "N args required");
			coord[0] = a;
			coord[1] = b;
			coord[2] = c;
		}

		/**
		* initialize from C style array
		* @deprecated -- std::array constructor preferred
		*/
		TPoint(const T * a) {
			for (int i = 0; i < N; i++) {
				coord[i] = a[i];
			}
		}

		/**
		* zero
		*/
		TPoint( )
			:coord(){}

		/**
		* construct scaled copy. 
		* constructed point is cast to implementing T with
		* implicit loss of precision converting from
		* type U to type T
		*/
		template <class U>
		TPoint(const TPoint<U,N> & rhs, T scale) { 
			for (int i = 0; i < N; i++ ) {
				coord[i] = static_cast<T>(rhs(static_cast<Axis>(i)) * scale);
			}
		}
		/**
		* @param x scaled long
		* @param y scaled long
		* @param scale current scale 
		*/
		/*
		TPoint(T x, T y, double scale) 
		{
		coord[cX] = x * scale;
		coord[cY] = y * scale;
		}
		*/

		TPoint(T (&values)[N] ) {
			for (int i = 0; i < N; i++) {
				coord[i] = values[i];
			}
		}

		static int numDim( ) {
			return N;
		}

		T operator( )(Axis a) const {
			return coord[a];
		}
		T &operator( )(Axis a) {
			return coord[a];
		}
		T get(Axis a) const {
			return coord[a];
		}
		/**
		* < operator to allow insertion in set
		* comparison based on coordinates in sequence
		*/
		bool operator <(const TPoint & rhs) const {
			for (int i = 0; i < N; i++) {
				if (coord[i] < rhs.coord[i]) {
					return true;
				}
				if (coord[i] > rhs.coord[i]) {
					return false;
				}
			}
			return false;
		}

	};

#pragma warning ( disable : 4351 )
	template<class T, int N>
	class GhostPoint :public TPoint<T,N> {
		typedef TPoint<T,N> base;
	protected:
		bool ghost;
	public:
		explicit GhostPoint(T val)
			:base(val),
			ghost(false)  {}

		explicit GhostPoint(T a, T b)
			:base(a,b),
			ghost(false)  {}

		explicit GhostPoint(T a, T b, T c)
			:base(a,b,c),
			ghost(false)  {}

		GhostPoint(const std::array<T,N> &in)
			:base(in),
			ghost(false) {}

		/*
		GhostPoint(const Array &a) 
		:base(a),
		ghost(false) {}
		*/

		/**
		* zero, non-ghost point
		*/
		GhostPoint( )
			:base(),
			ghost(false) {}

		GhostPoint(const TPoint<T,N> & rhs)
			:base(rhs),
			ghost(false) {}

		/**
		* construct scaled copy
		*/
		GhostPoint(const TPoint<T,N> & rhs, double scale, bool ghost_)  
			:base(rhs,scale),
			ghost(ghost_) {}

		/**
		* @param x scaled long
		* @param y scaled long
		* @param scale current scale 
		* @param ghost_ if true, mark as ghost point 
		*/
		GhostPoint(T x, T y, double scale, bool ghost_) 
			:base(x *scale, y *scale),
			ghost(ghost_) {}

		void setGhost(bool ghost) {
			this->ghost = ghost;
		}

		bool isGhost( ) const {
			return ghost;
		}
	};

	template <class T, int N>
	inline bool operator==(const spatial::TPoint<T,N> & lhs, const spatial::TPoint<T,N> & rhs ) {
		for (int i = 0; i < N ;i++) {
			const Axis a = static_cast<Axis>(i);
			if (lhs(a) != rhs(a) ) {
				return false;
			}
		}
		return true;
	}

	template <class T, int N>
	inline bool operator!=(const spatial::TPoint<T,N> & lhs, const spatial::TPoint<T,N> & rhs ) {
		return ! (lhs == rhs);
	}

	template <class T, int N>
	inline TPoint<T,N> midPoint(const spatial::TPoint<T,N> & lhs, const spatial::TPoint<T,N> & rhs ) {
		std::array<T,N> values;
		for (int i = 0; i < N ;i++) {
			const Axis a = static_cast<Axis>(i);
			values[i] = ( lhs(a) + rhs(a) )/ 2;
		}
		return  TPoint<T,N>(values);
	}

	/**
	* distance squared; runtime cheaper than distance
	*/
	template <class T, int N>
	inline T distanceSquared(const spatial::TPoint<T,N> & lhs, const spatial::TPoint<T,N> & rhs ) {
		T dSquared = 0;
		for (int i = 0; i < N ;i++) {
			const Axis a = static_cast<Axis>(i);
			const T delta = lhs(a) - rhs(a);
			dSquared += delta * delta;
		}
		return dSquared;
	}

	/**
	* distance between
	*/
	template <class T, int N>
	inline double distance(const spatial::TPoint<T,N> & lhs, const spatial::TPoint<T,N> & rhs ) {
		return sqrt(distanceSquared(lhs,rhs));
	}

	/**
	* distance between, cast (truncated) to input type
	*/
	template <class T, int N>
	inline T distanceApproximate(const spatial::TPoint<T,N> & lhs, const spatial::TPoint<T,N> & rhs ) {
		return static_cast<T>(sqrt(distanceSquared(lhs,rhs)));
	}

	template <class T, int N>
	inline std::ostream &operator<<(std::ostream & os, const spatial::TPoint<T,N> & point) {
		os << point(static_cast<Axis>(0)); 
		for (int i = 1; i < N ;i++) {
			const Axis a = static_cast<Axis>(i);
			os << " , " << point(a);
		}
		return os;
	}

	typedef TPoint<double,2> Point2D;

	template<class FROM, typename TO>
	inline TO convert(const FROM &from) {
		assert(TO::numDim( ) == FROM::numDim( ));
		TO rval;
		for (int i = 0; i < TO::numDim( ) ;i++) {
			const Axis a = static_cast<Axis>(i);
			rval(a) = from(a);
		}
		return rval;
	}
}

/*
namespace boost {
namespace polygon {
template<>
struct geometry_concept<spatial::Point2D> { typedef point_concept type; };

template<>
struct point_traits<spatial::Point2D> {
typedef double coordinate_type;
static inline coordinate_type get(const spatial::Point2D& point, orientation_2d orient) {
return (orient == HORIZONTAL) ? point.get(spatial::cX) : point.get(spatial::cY);
}
};

template<>
struct geometry_concept<spatial::GhostPoint<double,2> > { typedef point_concept type; };

template<>
struct point_traits<spatial::GhostPoint<double,2> > {
typedef double coordinate_type;
static inline coordinate_type get(const spatial::GhostPoint<double,2> & point, orientation_2d orient) {
return (orient == HORIZONTAL) ? point.get(spatial::cX) : point.get(spatial::cY);
}
};
}
}
*/
#endif
