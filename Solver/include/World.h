#ifndef World_h
#define World_h
#include <MovingBoundaryTypes.h>
#include <TPoint.h>
#include <SVector.h>
#include <VCellException.h>
#include <GeoLimit.h>

namespace moving_boundary {

	//forward
	//enum UniverseLock;
	enum UniverseLock {unset, set, lockedUniverse};
	template <int NUM_DIM>
	struct Universe; 

	/**
	* if a subsytem cannot handle std::numeric_limits<COORD_TYPE>::max( ),
	* they can specify their max value here. This must be done before init.
	* This class is designed to be used as static object to
	* cause execute before main( ). 
	*/
	template <typename COORD_TYPE>
	struct WorldMax {
		WorldMax(COORD_TYPE maxValue); 
	};

	template<int N>
	struct WorldBase {
		/**
		* destroy (for testing)
		*/
		virtual void destroy( ) = 0;
		/**
		* constructor functionality in separate module
		to allow recreation for tests
		*/
		virtual void init( ) = 0;
		//avoid header dependency by doing linked list old-fashioned way
		WorldBase<N> *nextWorld;
		WorldBase( )
			:nextWorld(nullptr) {}
	};

	template <int NUM_DIM>
	struct Universe {
		/**
		* type used for number of nodes in a dimension
		*/
		typedef unsigned short CountType;
		/**
		* return singleton instance
		*/
		static Universe<NUM_DIM> & get( );

		/**
		* initialize the world. std::logic_error
		* @param limits to use
		* @param numNodes in a dimension 
		* @param lock if true, prevents destory from being used
		* @throws std::logic_error if called more than once without #destroy call
		* @throws std::domain_error if low values not less than high values
		*/
		void init(std::array<spatial::GeoLimit, NUM_DIM> & limits, std::array<CountType,NUM_DIM> numNodes= std::array<CountType,NUM_DIM>( ),
			bool lock = false);

		/**
		* return limit for specific dimension
		* @param i
		* @throws std::domain_error if i < 0 or >= NUM_DIM
		*/
		spatial::GeoLimit limitFor(unsigned int i) const {
			if (i < NUM_DIM) {
				return inputLimits[i];
			}
			VCELL_EXCEPTION(domain_error,"Invalid index " << i << " for " << NUM_DIM << " dimensional world")
		}

		/**
		* return limit for specific dimension
		* @param a 
		* @throws std::domain_error if a out of range for NUM_DIM 
		*/
		spatial::GeoLimit limitFor(spatial::Axis a) const {
			return limitFor(static_cast<unsigned int>(a));
		}

		/**
		* @return reference to complete set of limits
		*/
		const std::array<spatial::GeoLimit,NUM_DIM> & limits( ) const {
			return inputLimits;
		}
		const std::array<double,NUM_DIM> & zeros ( ) const {
			return inputZeroPoint;
		}

		/**
		* reset the world to the unset state. Intended for testing only
		* @throws std::domain_error if locked
		*/
		void destroy( );

		/**
		* @return true if Universe locked from further initialization
		*/
		bool locked( ) const;

		const std::array<CountType,NUM_DIM> & numNodes( ) const {
			return nodeNumbers; 
		}

		double diagonal( ) const {
			return diagonal_;
		}

	private:
		Universe( );
		std::array<spatial::GeoLimit,NUM_DIM> inputLimits;
		std::array<CountType,NUM_DIM> nodeNumbers;
		std::array<double,NUM_DIM> inputZeroPoint;
		double diagonal_;
		UniverseLock lockState;
		WorldBase<NUM_DIM> *worlds;
		template <typename COORD_TYPE, int> friend struct World;
	};

	/**
	* class to store maximum supported value for a type independent of number
	* of dimensions
	*/
	template <typename COORD_TYPE>
	struct WorldTypeBase {
		/**
		* maximum value for this type supported by subsystems
		*/
	protected:
		static COORD_TYPE maxSupported;
		friend WorldMax<COORD_TYPE>;
	};

	/**
	* @tparam NUM_DIM number of dimensions supported
	* @tparam COORD_TYPE type needed for modeling or API
	*/
	template <typename COORD_TYPE,int NUM_DIM>
	struct World : public WorldBase<NUM_DIM>, public WorldTypeBase<COORD_TYPE> {
		/**
		* return singleton instance
		*/
		static World & get( );

		/**
		* convert from problem domain scaling to World coordinates. 
		* integral types truncated
		*/
		spatial::TPoint<COORD_TYPE,NUM_DIM> toWorld(const spatial::TPoint<double,NUM_DIM> & problemDomainPoint) const {
			spatial::TPoint<COORD_TYPE,NUM_DIM> rval; 
			for (spatial::Axis a = spatial::axisInitial; a < NUM_DIM; ++a) {
				double v  = scale * (problemDomainPoint(a) - Universe<NUM_DIM>::get( ).inputZeroPoint[a]);
				rval(a) = static_cast<COORD_TYPE>(v);
			}
			return rval;
		}

		/**
		* convert from World coordinates to problem domain
		*/
		spatial::TPoint<double,NUM_DIM> toProblemDomain(const spatial::TPoint<COORD_TYPE,NUM_DIM> & worldPoint) const {
			spatial::TPoint<double,NUM_DIM> rval; 
			for (spatial::Axis a = spatial::axisInitial; a < NUM_DIM; ++a) {
				 rval(a) = worldPoint(a) / scale + Universe<NUM_DIM>::get( ).inputZeroPoint[a];
			}
			return rval;
		}

		/**
		* convert from World coordinates *to problem domain
		* @param source input values in World equivalent, array length #NUM_DIM
		* @param destination ouput values in problem, array length #NUM_DIM
		*/
		void toProblemDomain(const double *source, double * destination) const {
			for (spatial::Axis a = spatial::axisInitial; a < NUM_DIM; ++a) {
				 destination[a] = source[a] / scale + Universe<NUM_DIM>::get( ).inputZeroPoint[a];
			}
		}

		/**
		* convert from problem domain coordinates to World 
		* (vector conversion does not user zero offset)
		*/
		template <typename T>
		spatial::SVector<T,NUM_DIM> toWorld(const spatial::SVector<double,NUM_DIM> & pdVelocity) const {
			spatial::SVector<T,NUM_DIM> rval; 
			for (spatial::Axis a = spatial::axisInitial; a < NUM_DIM; ++a) {
				rval(a) = static_cast<T>(scale * pdVelocity(a));
			}
			return rval;
		}

		/**
		* convert from these coordinates to other world coordinates
		*/
		template <typename OTHER_TYPE>
		spatial::TPoint<OTHER_TYPE,NUM_DIM> toOtherWorld(const spatial::TPoint<COORD_TYPE,NUM_DIM> & ourPoint) const {
			static double cFactor = World<OTHER_TYPE,NUM_DIM>::get( ).scale / scale; 
			using spatial::Axis;
			spatial::TPoint<OTHER_TYPE,NUM_DIM> rval; 
			for (Axis a; a < NUM_DIM; ++a) {
				 rval(a) = cFactor * ourPoint(a); 
			}
			return rval;
		}


		/**
		* @return length of diagonal to world
		*/
		COORD_TYPE diagonal( ) const {
			return diagV;
		}

		const std::array<spatial::TGeoLimit<COORD_TYPE>,NUM_DIM> limits( ) const { 
			return limitsWorldSystem;
		}

		virtual void destroy( ); 

		double theScale( ) const {
			return scale;
		}

		const Universe<NUM_DIM> & universe( ) const {
			return univ;
		}

	private:

		World( );
		/**
		* constructor functionality in separate module
		to allow recreation for tests
		*/
		virtual void init( );
		Universe<NUM_DIM> & univ;
		COORD_TYPE diagV;
		std::array<spatial::TGeoLimit<COORD_TYPE>,NUM_DIM> limitsWorldSystem;
		/**
		* double to #COORD_TYPE factor
		*/
		double scale;
	};
}
#endif
