#ifndef VCellFront_h
#define VCellFront_h
#include <vector>
#include <TPoint.h>
#include <World.h> //GeoLimit

#ifdef _WIN32 //Visual Studio defines this by default
#ifndef WIN32
#define WIN32 //FronTier expects this for windows
#endif
#endif

#pragma warning ( disable: 4800 )
#include <boost/multi_array.hpp>
#include <FronTier.h>
#undef Coords
#undef REAL 
#undef radians
#undef degrees 
#undef Error 
#undef isnan
namespace Frontier {
	typedef ::HYPER_SURF HYPER_SURF; 
	typedef ::HYPER_SURF_ELEMENT HYPER_SURF_ELEMENT; 
	typedef ::POINTER POINTER;
	typedef ::Front Front;
	typedef ::POINT POINT;
	typedef ::F_BASIC_DATA F_BASIC_DATA;
	typedef ::LEVEL_FUNC_PACK LEVEL_FUNC_PACK;
	typedef ::VELO_FUNC_PACK VELO_FUNC_PACK;
	typedef ::BOND BOND;
	typedef ::CURVE CURVE;
	using ::PERIODIC_BOUNDARY;
	using ::FIRST_PHYSICS_WAVE_TYPE;
	using ::GENERAL_WAVE;
	using ::first_order_point_propagate;
	using ::fourth_order_point_propagate;
	/*
	typedef ::X X;
	*/
}
//#include <TPoint.h>
#include <sstream>
namespace spatial {
	using Frontier::HYPER_SURF;
	using Frontier::HYPER_SURF_ELEMENT;

	typedef double (*FronTierLevelFunction)(Frontier::POINTER, double *); 

	typedef int (*FronTierVelocityFunction)(Frontier::POINTER,
		Frontier::Front*,Frontier::POINT*,HYPER_SURF_ELEMENT*, HYPER_SURF*,double*);

	struct FronTierLevel {
		virtual double level(double *) const=0;
	};

	struct FronTierVelocity {
		virtual int velocity(Frontier::Front*,Frontier::POINT*,HYPER_SURF_ELEMENT*, HYPER_SURF*,double*) const = 0;
	};

	struct FrontProvider {
		virtual ~FrontProvider( ) {}
		/**
		* propagate to specified time
		* @return false if problem
		*/
		virtual bool propagateTo(double time) = 0;
		/**
		* get current front 
		*/
		virtual std::vector<spatial::Point2D> retrieveFront( ) = 0;
		/**
		* provide description of front
		*/
		virtual std::string describe( ) const = 0;
	};

	class VCellFront : public FrontProvider {
	public:
		VCellFront(std::vector<GeoLimit> & xlimits, int N, double tmax,
			FronTierLevelFunction levelFunction,
			FronTierVelocityFunction velocityFunction);

		VCellFront(std::vector<GeoLimit> & xlimits, int N, double tmax,
			const FronTierLevel & level,
			const FronTierVelocity &vel);

		/**
		* advance front to specified time
		* @return false if time <= current max_time
		*/
		bool propagateTo(double time);
		virtual std::string describe( ) const {
			return std::string("Frontier provided front");
		}

		template <class P>
		std::vector<P> retrieveSurf( );

		virtual std::vector<spatial::Point2D> retrieveFront( ); 

		template <class P>
		std::vector<std::vector<P> > retrieveCurves( );

		Frontier::Front* const c_ptr( ) { return &front; }

	private:
		void init(std::vector<GeoLimit> & xlimits, int N, double tmax,
			FronTierLevelFunction levelFunction,
			FronTierVelocityFunction velocityFunction,
			bool isAdapter);
		Frontier::Front front;
		Frontier::F_BASIC_DATA f_basic;
		Frontier::LEVEL_FUNC_PACK level_func_pack;
		Frontier::VELO_FUNC_PACK velo_func_pack;

		const FronTierLevel * const levelObj;
		const FronTierVelocity * const velObj;
		static double levelAdapter(Frontier::POINTER, double *); 

		static int velocityAdapter(Frontier::POINTER,Frontier::Front*,Frontier::POINT*,HYPER_SURF_ELEMENT*, HYPER_SURF*,double*);
	};

}
#endif
