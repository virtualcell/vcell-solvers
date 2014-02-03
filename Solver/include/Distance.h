#ifndef Distance_h
#define Distance_h
#include <cmath>
#include <TPoint.h>
#include <cassert>
#include <stdexcept>
#include <cstdint>
#include <VCellException.h>

namespace spatial {

	/**
	* default distance policy; use same types for all
	* works well with floating point types
	*/
	template <typename T>
	struct DefaultDistancePolicy{
		typedef T DistanceSquaredType;
		typedef T DistanceType;
		/**
		* allows range checking if subset of allowed types desired
		*/
		static void check(T) {}
		template <typename U>
		static DistanceType convert(U u) {
			return u;
		}
	};
	/**
	* a reasonable long policy
	*/
	template <> 
	struct DefaultDistancePolicy<long>{
		typedef long long DistanceSquaredType;
		typedef long DistanceType;
		static void check(long) {}
		template <typename U>
		static DistanceType convert(U u) {
			if (u > std::numeric_limits<long>::max( )) {
				VCELL_EXCEPTION(domain_error, u << " too big to be long" );
			}
			return static_cast<long>(u);
		}
	};
	/**
	* a int64_t policy
	*/
	template <> 
	struct DefaultDistancePolicy<int64_t>{
		typedef long long DistanceSquaredType;
		typedef long DistanceType;
		static void check(int64_t) {}
		template <typename U>
		static DistanceType convert(U u) {
			if (u > std::numeric_limits<long>::max( )) {
				VCELL_EXCEPTION(domain_error, u << " too big to be long" );
			}
			return static_cast<long>(u);
		}
	};
	/**
	* int sizes vary (more) from system to system; don't
	* provide default policy
	*/
	template <> 
	struct DefaultDistancePolicy<int>{
	};


	/**
	* class to allow different policies to be used to calculate distances 
	* @tparam T TPoint<T,n> supported (for all positive n)
	* @tparam Policy policy for types, checking, and conversion 
	*/
	template <typename T, class Policy = DefaultDistancePolicy<T> >
	struct Distance {
		typedef typename Policy::DistanceType DistanceType;
		typedef typename Policy::DistanceSquaredType DistanceSquaredType;
		/**
		* distance squared; runtime cheaper than distance
		*/
		template<int N>
		static typename Policy::DistanceSquaredType squared(const TPoint<T,N> & lhs, const TPoint<T,N> & rhs ) {
			DistanceSquaredType dSquared = 0;
			for (int i = 0; i < N ;i++) {
				const Axis a = static_cast<Axis>(i);
				Policy::check(lhs(a));
				Policy::check(rhs(a));
				const DistanceSquaredType delta = lhs(a) - rhs(a);
				dSquared += delta * delta;
			}
			return dSquared;
		}

		/**
		* distance between
		* if supported by policy
		*/
		template<int N>
		static typename Policy::DistanceType calc(const TPoint<T,N> & lhs, const TPoint<T,N> & rhs ) {
			return Policy::convert(std::sqrt(squared(lhs,rhs)));
		}

		/**
		* distance between, cast (possibly truncated) to input type
		*/
		template<int N>
		static T approximate(const TPoint<T,N> & lhs, const TPoint<T,N> & rhs ) {
			return static_cast<T>(std::sqrt(squared(lhs,rhs)));
		}
	};
}
#endif
