#include <sstream>
#include <algorithm>
#include <MPoint.h>
#include <persist.h>

using spatial::IndexInfo;
using spatial::MPoint;
using spatial::MeshElement;
using spatial::SurfacePosition;
using namespace vcell_persist;
namespace {

	struct Registrar {
		Registrar ( ) {
			VCELL_PERSIST_REGISTER_MACRO2(spatial::MPoint<double,2>)
			VCELL_PERSIST_REGISTER_MACRO2(spatial::MeshElement<double,2>)
		}

	};
}
template<int N>
const std::string & IndexInfo<N>::str( ) const {
	if (str_.length( ) > 0) {
		return str_;
	}
	std::ostringstream oss;
	oss << *this << std::ends;
	str_ = oss.str( );
	return str_;
}

template <class T, int N>
MPoint<T,N>::MPoint(std::istream &is) 
	:TPoint<T,N>(is)
{
	static Registrar r;
	Token::check<MPoint<T,N> >(is); 
	std::for_each(index.begin( ), index.end( ),binaryRead<size_t>(is) );
}

template <class T, int N>
void MPoint<T,N>::persist(std::ostream &os) {
	static Registrar r;
	TPoint<T,N>::persist(os);
	Token::insert<MPoint<T,N> >(os); 
	std::for_each(index.begin( ), index.end( ),binaryWrite<size_t>(os) );	
}

template <class T, int N>
MeshElement<T,N>::MeshElement(std::istream &is) 
	:MPoint<T,N>(is) 
{
	Token::check<MeshElement<T,N> >(is); 
	binaryRead<SurfacePosition> br(is);
	br(mp);
}

template <class T, int N>
void MeshElement<T,N>::persist(std::ostream &os) {
	MPoint<T,N>::persist(os);
	Token::insert<MeshElement<T,N> >(os); 
	binaryWrite<SurfacePosition> bw(os);
	bw(mp);
}

template void MPoint<double,2>::persist(std::ostream &);
template MPoint<double,2>::MPoint(std::istream &);
template void MeshElement<double,2>::persist(std::ostream &);
template MeshElement<double,2>::MeshElement(std::istream &);



template class IndexInfo<1>;
template class IndexInfo<2>;
template class IndexInfo<3>;
