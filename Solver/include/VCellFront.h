#ifndef VCellFront_h
#define VCellFront_h
#include <vector>
#include <TPoint.h>
#include <World.h> //GeoLimit

#pragma warning ( disable: 4800 )
#include <boost/multi_array.hpp>
#include <VFrontier.h>
namespace Frontier {
	typedef ::HYPER_SURF HYPER_SURF; 
	typedef ::HYPER_SURF_ELEMENT HYPER_SURF_ELEMENT; 
	typedef ::POINTER POINTER;
	typedef ::Front Front;
	typedef ::POINT POINT;
	typedef ::F_BASIC_DATA F_BASIC_DATA;
	typedef ::F_INIT_DATA F_INIT_DATA;
	typedef ::LEVEL_FUNC_PACK LEVEL_FUNC_PACK;
	typedef ::VELO_FUNC_PACK VELO_FUNC_PACK;
	typedef ::BOND BOND;
	typedef ::CURVE CURVE;
	using ::PERIODIC_BOUNDARY;
	using ::FIRST_PHYSICS_WAVE_TYPE;
	using ::GENERAL_WAVE;
	using ::VECTOR_WAVE;
	using ::GENERAL_NODE;
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

	template <typename T>
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
		virtual std::vector<spatial::TPoint<T,2> > retrieveFront( ) = 0;
		/**
		* provide description of front
		*/
		virtual std::string describe( ) const = 0;
		/**
		* register persistent type
		*/
		virtual void registerType( ) const = 0; 
		/**
		* store
		*/
		virtual void persist(std::ostream &os) const = 0; 

	};

//	template <typename T>
//	struct FrontProviderCreator {
//		/**
//		* identification token
//		*/
//		virtual const std::string & token( ) const = 0;
//		virtual FrontProvider<T> * create(std::istream &is) const = 0;
//	};
//
//	template <typename T>
//	struct FrontProviderFactory {
//		static void identify(const FrontProviderCreator<T> &);
//		const FrontProviderCreator<T> &lookup(const std::string &);  
//	};

	/**
	* @tparam FCT front coordinate type
	*/
	template <typename FCT>
	class VCellFront : public FrontProvider<FCT> {
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
		/**
		* advance front to specified time
		* @param csv stream to log intermediate outputs to
		* @return false if time <= current max_time
		*/
		bool propagateTo(double time, std::ostream  &csv);

		virtual std::string describe( ) const {
			return std::string("Frontier provided front");
		}

		std::vector<spatial::TPoint<FCT,2> > retrieveSurf( );

		virtual std::vector<spatial::TPoint<FCT,2> > retrieveFront( );
		/**
		* not implemented
		*/
		virtual void persist(std::ostream &os) const {} 
		/**
		* not implemented
		*/
		virtual void registerType( ) const {} 

		/**
		* retrieve front while logging points to csv file
		*/
		virtual std::vector<spatial::TPoint<FCT,2> > retrieveFront(std::ostream &csv );

		std::vector<std::vector<spatial::TPoint<FCT,2> > > retrieveCurves( );

		Frontier::Front* const c_ptr( ) { return &front; }

	private:
		typedef spatial::TPoint<FCT,2> VCFPointType; 
		void init(std::vector<GeoLimit> & xlimits, int N, double tmax,
			FronTierLevelFunction levelFunction,
			FronTierVelocityFunction velocityFunction,
			bool isAdapter);
		Frontier::Front front;
		Frontier::F_BASIC_DATA f_basic;
		Frontier::F_INIT_DATA f_init_data;
		Frontier::LEVEL_FUNC_PACK level_func_pack;
		Frontier::VELO_FUNC_PACK velo_func_pack;

		const FronTierLevel * const levelObj;
		const FronTierVelocity * const velObj;
		std::array<GeoLimit, 2> domainLimits;
		static double levelAdapter(Frontier::POINTER, double *); 

		static int velocityAdapter(Frontier::POINTER,Frontier::Front*,Frontier::POINT*,HYPER_SURF_ELEMENT*, HYPER_SURF*,double*);
	};
}
#endif
