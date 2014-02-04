#include <World.h>
using moving_boundary::Universe;
using spatial::GeoLimit;
/********************************************************************
* Universe 
********************************************************************/
template <int N>
Universe<N>::Universe( )
	:inputLimits( ),
	diagonal_( ),
	lockState(unsetUniverse) {}

template <int N>
Universe<N> & Universe<N>::get( ) {
	static Universe<N> w;
	return w;
}

template <int N>
void Universe<N>::init(std::array<GeoLimit,N> &iValues, std::array<CountType, N> numNodes, bool lock) {
	nodeNumbers = numNodes;
	typedef moving_boundary::CoordinateType CoordType;
	//validate state 
	if (lockState != unsetUniverse) {
		VCELL_EXCEPTION(logic_error, "Universe<REAL, " << N << "> already initialized");
	}
	double diagonalScratch = 0; 
	for (int i = 0; i < N; i++) {
		const GeoLimit & gl = iValues[i];
		assert(gl.low( ) <= gl.high( ));
		inputZeroPoint[i] = (gl.low( ) + gl.high( ))/2;
		const double delta = gl.span( );
		diagonalScratch += delta * delta; 
	}
	diagonal_ = sqrt(diagonalScratch);
	inputLimits = iValues;
	lockState = lock ?  lockedUniverse : set;
	WorldBase<N> *wb = worlds;
	while (wb != nullptr) {
		wb->init( );
		wb = wb->nextWorld;
	}
}

template <int N>
void Universe<N>::destroy( ) {
	if (locked( )) {
		VCELL_EXCEPTION(logic_error, "Universe<REAL, " << N << "> locked");
	}

	for (int i = 0; i < N; i++) {
		inputLimits[i] = GeoLimit(0,0);
	}
	lockState = unsetUniverse;
	WorldBase<N> *wb = worlds;
	while (wb != nullptr) {
		wb->destroy( );
		wb = wb->nextWorld;
	}
}

template <int N>
bool Universe<N>::locked( ) const {
	return lockState == lockedUniverse;
}


template struct Universe<2>;
template struct Universe<3>;
