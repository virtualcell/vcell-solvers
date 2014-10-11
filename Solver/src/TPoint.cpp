#include <algorithm>
#include <stdint.h>
#include <TPoint.h>
#include <persist.h>
using namespace spatial;
using namespace vcell_persist; 

template <class T, int N>
TPoint<T,N>::TPoint(std::istream &is)
	:coord( ) 
{
	Token::check<TPoint<T,N> >(is); 
	std::for_each(coord.begin( ), coord.end( ),binaryRead<T>(is) );
}

template <class T, int N>
void TPoint<T,N>::persist(std::ostream &os) {
	Token::insert<TPoint<T,N> >(os); 
	std::for_each(coord.begin( ), coord.end( ),binaryWrite<T>(os) );

}


template spatial::TPoint<char,2>::TPoint(std::istream &);
template spatial::TPoint<short,2>::TPoint(std::istream &);
template spatial::TPoint<int,2>::TPoint(std::istream &);
template spatial::TPoint<long,2>::TPoint(std::istream &);

template spatial::TPoint<double,2>::TPoint(std::istream &);

template spatial::TPoint<uint64_t,2>::TPoint(std::istream &);

template spatial::TPoint<int,3>::TPoint(std::istream &);
template spatial::TPoint<double,3>::TPoint(std::istream &);

#ifdef _MSC_BUILD
	template spatial::TPoint<int16_t,2>::TPoint(std::istream &);
	template spatial::TPoint<int64_t,2>::TPoint(std::istream &);
#endif
