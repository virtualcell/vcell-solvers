#include <TPoint.h>
#include <persist.h>
using namespace spatial;
using namespace vcell_persist; 
namespace {
	template <class T, int N>
	std::string makeToken( ) {
		std::string token("TPoint");
		token += typeid(T).name( );
		token += N;
		return token;
	}

}

template <class T, int N>
std::string TPoint<T,N>::persistToken = makeToken<T,N>( );

template <class T, int N>
TPoint<T,N>::TPoint(std::istream &is)
	:coord( ) 
{
	Token::check(is, persistToken); 
	std::for_each(coord.begin( ), coord.end( ),binaryRead<T>(is) );

}

template <class T, int N>
void TPoint<T,N>::persist(std::ostream &os) {
	Token::insert(os, persistToken); 
	std::for_each(coord.begin( ), coord.end( ),binaryWrite<T>(os) );

}

template void spatial::TPoint<double,2>::persist(std::ostream &);
template spatial::TPoint<double,2>::TPoint(std::istream &);

template void spatial::TPoint<int,3>::persist(std::ostream &);
template spatial::TPoint<int,3>::TPoint(std::istream &);