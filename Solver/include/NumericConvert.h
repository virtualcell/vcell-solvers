#ifndef NumericConvert_h
#define NumericConvert_h
#include <cmath>
#include <limits>
namespace vcell_util {
	/**
	* @tparam IN input type
	* @tparam OUT output type
	* return lowest valid OUT which is >= in, taking sign into consideration. 
	* e.g. ConvertUp<int>(-4.5) is -4 
	*/
	template <class OUT, class IN>
	OUT ConvertUp(IN in);

	/**
	* @tparam IN input type
	* @tparam OUT output type
	* return lowest valid OUT which is <= in, taking sign into consideration. 
	* e.g. ConverDown<int>(-4.5) is -5 
	*/
	template <class OUT, class IN>
	OUT ConvertDown(IN in);

	template <>
	inline int ConvertDown(double in) {
		if (in >= 0) {
			return static_cast<int>(in);
		}
		//implicit else 
		return static_cast<int>(floor(in)); 
	}

	template <>
	inline int ConvertUp(double in) {
		const double c = ceil(in);
		return static_cast<int>(c);
	}

	template <>
	inline long ConvertDown(double in) {
		if (in >= 0) {
			return static_cast<long>(in);
		}
		return static_cast<long>(floor(in)); 
	}

	template <>
	inline long ConvertUp(double in) {
			return static_cast<long>(ceil(in));
	}

	template <>
	inline short ConvertDown(double in) {
		if (in >= 0) {
			return static_cast<short>(in);
		}
		return static_cast<short>(floor(in)); 
	}

	template <>
	inline short ConvertUp(double in) {
			return static_cast<short>(ceil(in));
	}

	template <>
	inline double ConvertDown(double in) {
		return in;
	}

	template <>
	inline double ConvertUp(double in) {
		return in;
	}

	//template long ConvertUp<long, double>(double); 
	//template long ConvertDown<long, double>(double); 

	template <class T>
	bool validMultiply(T a, T b) {
		return std::numeric_limits<T>::max( ) / std::abs(a) >= std::abs(b);
	}
}
#endif
