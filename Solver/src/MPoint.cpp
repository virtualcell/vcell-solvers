#include <MPoint.h>
#include <sstream>

using spatial::IndexInfo;
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

template class IndexInfo<1>;
template class IndexInfo<2>;
template class IndexInfo<3>;