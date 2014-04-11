#include <World.h>
#include <Logger.h>
#include <NumericConvert.h>
#include <map>
#include <list>
#include <boost/math/common_factor.hpp>

using moving_boundary::Universe;
using spatial::GeoLimit;
using moving_boundary::WorldMax;
using moving_boundary::World;
using moving_boundary::WorldTypeBase;

/********************************************************************
* WorldMax
********************************************************************/
template <typename COORD_TYPE>
WorldMax<COORD_TYPE>::WorldMax(COORD_TYPE maxValue) {
	if (WorldTypeBase<COORD_TYPE>::maxSupported > 0) { 
		WorldTypeBase<COORD_TYPE>::maxSupported = std::min(WorldTypeBase<COORD_TYPE>::maxSupported,maxValue);
	}
	else {
		WorldTypeBase<COORD_TYPE>::maxSupported = maxValue; 
	}
}

/********************************************************************
* World 
********************************************************************/
namespace {
	/**
	* use class template for implementing "init" function because they can be partially specialized
	* the base class is non-functional to ensure appropriate specializations have been created
	*/
	template <typename WORLD_COORD, int N>
	struct WorldInit {
		/**
		* use "init" as creation function instead of constructor to avoid need to define derived class constructors
		*/
		void init( );
	};

	/**
	* double requires no scaling
	*/
	template <int N>
	struct WorldInit<double,N> {
		void init(Universe<N> & universe, double &maxSupported) {
			scale = 1; 
			diagV = universe.diagonal( ); 
			limitsWorldSystem = universe.limits( );
		}

		double scale;
		double diagV;
		std::array<spatial::TGeoLimit<double>,N> limitsWorldSystem;
	};

	/**
	* common implementation for integral types
	*/
	template <typename WORLD_COORD, int N>
	struct WorldInitInteger {
		void init(Universe<N> & universe, WORLD_COORD &maxSupported) {
			typedef typename Universe<N>::CountType CountType;
			//if this has not been set by a static WorldMax object, use numerics limits for max
			if (maxSupported  == 0) {
				maxSupported = std::numeric_limits<WORLD_COORD>::max( );
			}
			scale = maxSupported / universe.diagonal( );
			WORLD_COORD iScale = static_cast<WORLD_COORD>(scale);

			//find desired divider for iScale -- we want all mesh centers and edges to be on exact integers
			WORLD_COORD divider = 1;
			const std::array<CountType,N> & numNodes = universe.numNodes( );
			for (int i = 0; i < N; i++) {
				const CountType spaces = 2*numNodes[i];
				if (spaces > 0) {
					divider = boost::math::lcm<WORLD_COORD>(divider,2*numNodes[i]);
				}
			}
			while (iScale % divider != 0 && iScale > 1) {
				iScale--;
			}
			if (iScale == 1) {
				VCELL_EXCEPTION(logic_error, "unable to find decent scale for least common multiplier of " << divider << " and scale " << scale 
					<< ", max supported is " << maxSupported);
			}

			scale = iScale;
			for (int i = 0; i < N; i++) {
				WORLD_COORD low  = vcell_util::ConvertDown<WORLD_COORD>(scale * (universe.limits( )[i].low( )  - universe.zeros( )[i]));
				WORLD_COORD high = vcell_util::ConvertUp<WORLD_COORD>  (scale * (universe.limits( )[i].high( ) - universe.zeros( )[i]));
				WORLD_COORD remainder = (high - low ) % divider;
				high += (divider - remainder);
				remainder = (high - low ) % divider;
				if (remainder != 0) {
					VCELL_EXCEPTION(logic_error, " world scale not to be evenly divisible by desired scale" << iScale);
				}
				limitsWorldSystem[i] = spatial::TGeoLimit<WORLD_COORD>(low,high);
			}

			diagV = vcell_util::ConvertUp<WORLD_COORD>(scale * universe.diagonal( ));
			static_assert(std::numeric_limits<WORLD_COORD>::is_integer,"non-integer type"); 
			VCELL_LOG(info,"World type is " <<  std::numeric_limits<WORLD_COORD>::digits << " digits, scale is " << iScale);
		}
		typename vcell::Type<WORLD_COORD>::realType scale;
		WORLD_COORD diagV;
		std::array<spatial::TGeoLimit<WORLD_COORD>,N> limitsWorldSystem;
	};

	template <int N>
	struct WorldInit<int16_t,N> : public WorldInitInteger<int16_t,N> {
	};
	template <int N>
	struct WorldInit<int32_t,N> : public WorldInitInteger<int32_t,N> {
	};
	template <int N>
	struct WorldInit<int64_t,N> : public WorldInitInteger<int64_t,N> {
	};
}

template <typename WORLD_COORD, int N>
World<WORLD_COORD,N>::World( )
	:univ(Universe<N>::get( )),
	diagV( ),
	limitsWorldSystem( ),
	scale( )
{ 
	WorldBase<N>::nextWorld = univ.worlds;
	univ.worlds = this;
	init( ); 
}

template <typename WORLD_COORD, int N>
void World<WORLD_COORD,N>::init( ) {
	WorldInit<WORLD_COORD,N> helper;
	helper.init(univ,WorldTypeBase<WORLD_COORD>::maxSupported);
	scale = helper.scale;
	diagV = helper.diagV;
	limitsWorldSystem = helper.limitsWorldSystem;
}

template <typename WORLD_COORD, int N>
void World<WORLD_COORD,N>::destroy( ) {
	diagV = 0;
	scale = 0;
}

template <typename WORLD_COORD, int N>
World<WORLD_COORD,N> & World<WORLD_COORD,N>::get( ) {
	static World<WORLD_COORD,N> w;
	return w;
}

template <typename COORD_TYPE>
COORD_TYPE WorldTypeBase<COORD_TYPE>::maxSupported;

/********************************************************************
* instantations
********************************************************************/
//template struct World<moving_boundary::CoordinateType,2>;
template struct World<double,2>;

template struct World<int16_t,2>;
template struct World<int32_t,2>;
template struct World<int64_t,2>;

template struct WorldMax<int16_t>;
template struct WorldMax<int32_t>;
template struct WorldMax<long long>;

#ifdef COMPILE_64
template struct World<int64_t,2>;
template struct WorldMax<int64_t>;
#endif