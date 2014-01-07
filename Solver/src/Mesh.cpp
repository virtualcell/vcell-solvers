#include <Mesh.h>

//using namespace spatial;
template <class REAL, int N> 
std::vector<REAL> spatial::MeshDef<REAL,N>::coordinateValues(spatial::Axis a) const {
	const size_t np = nPoints[a];
	const REAL step = intervals[a];
	REAL v = origin[a] + step / 2;
	std::vector<REAL> rval ( np ); 
	for (int i = 0; i < np; i++, v += step) {
		rval[i] = v;
	}
	return rval;
}



/**
* template instantiation
*/
namespace spatial {
	template struct MeshDef<double,2>; 
	template struct MeshDef<int,2>; 
}