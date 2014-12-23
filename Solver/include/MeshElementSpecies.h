#ifndef MeshElementSpecies_h
#define MeshElementSpecies_h
#include <MPoint.h>
#include <fstream>
#include <vector>
#include <cmath>
#include <MovingBoundaryCollections.h>
#include <Allocator.h>
#include <Volume.h>
#include <SVector.h>
#include <Segment.h>
#include <Mesh.h>
//#include <VoronoiMesh.h>
#include <VCellException.h>
#include <Logger.h>
#include <DiffuseAdvectCache.h>
#include <portability.h>
#include <math.h>
#include <persist.h>
#include <Physiology.h>
#ifdef MES_STATE_TRACK
#define setState(x) DEBUG_SET_STATE(x, __FILE__, __LINE__)
#endif

//forward definitions
//namespace spatial {
//
//	template<class COORD_TYPE, int N> struct MeshDef; 
//	template<class COORD_TYPE, int N, class TELEMENT> struct Mesh; 
//	//struct VoronoiResult;
//}

namespace moving_boundary {
	//placeholder until we convert fixed std::array to dynamic storage
	//const size_t nOfS = 1;
	struct VoronoiMesh;
	struct MeshElementSpecies;
	struct MeshElementSpeciesIdent;
	/**
	* proxy object to stream identifying information about MeshElementSpecies
	*/
	struct MeshElementSpeciesIdent {
		const MeshElementSpecies &mes;
		MeshElementSpeciesIdent(const MeshElementSpecies &m)
			:mes(m) {}
		void write(std::ostream &os) const; 
	};

	inline std::ostream & operator<<(std::ostream & os,const MeshElementSpeciesIdent& mesi) {
		mesi.write(os);
		return os;
	}

	/**
	* used by #MeshElementSpecies to store information about neighbors
	*/
	struct MeshElementNeighbor : public vcell_persist::Persistent {
		MeshElementNeighbor( )
			:element(nullptr),
			distanceTo( ),
			edgeLength( )
			//daAmount(unset)
		{}
		MeshElementNeighbor(MeshElementSpecies *e)
			:element(e),
			distanceTo( ),
			edgeLength( ) 
		{}
		MeshElementNeighbor(std::istream & is,  const MeshElementSpecies & client) ;

		bool operator==(const MeshElementNeighbor &rhs) const {
			return element == rhs.element 
				&& distanceTo == rhs.distanceTo
				&& edgeLength == rhs.edgeLength;
		}

		MeshElementSpecies *element; 
		moving_boundary::DistanceType distanceTo;
		moving_boundary::DistanceType edgeLength;

		void persist(std::ostream &os, const MeshElementSpecies &client) const; 

		static void registerType( ) {
			spatial::ElementOffset<2>::registerType( );
			vcell_persist::Registrar::reg<MeshElementNeighbor>("MeshElementNeighbor");
		}
	};

	namespace MeshElementStateful {
		//if adding state, add to std::ostream & moving_boundary::operator<<(std::ostream &os ,moving_boundary::MeshElementStateful::State state) in *cpp 
		/**
		* describes both position relative to boundary and processing status. Names indicate last step performed
		* trans -- tranisitioning
		* in -- inside boundary but not adjacent to boundary
		* Bnd -- inside and adjacent to boundary 
		* Out -- outside boundary 
		* DiffAdv -- diffusionAdvection 
		* Nbr -- neighbor 
		*/
		enum State { 
			/**
			* initial state. #setPos to #stable
			*/
			initial,
			initialInside,
			initialBoundary,
			/**
			* begin/end of move diffuse advect cycle
			*/
			//stable, 
			/**
			* source terms applied (interior nodes)
			*/
			//stableSourceApplied,
			/**
			* source terms applied (boundary nodes)
			*/
			//legacySourceApplied,
			/**
			* diffusionAdvection applied
			*/
			//stableUpdated,
			/**
			* moved outside boundary
			*/
			transBndOut, 
			/**
			* moved from interior to boundary
			*/
			transInBnd,
			/**
			* moved inside boundary (from #setPos)
			*/
			transOutBndSetBnd,
			/**
			* neighbors applied (last state #transOutBndSetBnd)
			*/
			transOutBndNbrSet,
			/**
			* mass collected (last state #transOutBndNbrSet)
			*/
			transOutBndMassCollected, 
			/**
			* front moved but neighbor edges stale, voronoi not applied 
			*/
			bndFrontMoved,
			/**
			* front moved, neighbor edges set, voronoi not applied 
			*/
			bndNbrEdgesFound,
			/**
			* diffusionAdvection applied (last state #bndNbrEdgesFound)
			*/
			bndDiffAdvDone,
			bndDiffAdvDoneMU,
			//bndNbrUpdated,
			/**
			* neighbors updated: (last state #bndDiffAdvDone, #transInBnd)
			*/
			bndFrontApplied,
			/**
			* neighbors updated and some mass has been collected: (last state #bndFrontApplied)
			*/
			/**
			* must be one more than #legacySetCollected
			* interior volume is set, from #setPos, (last state #bndDiffAdvDone)
			*/
			transOutBndSetIn,
			transBndIn,
			bndStable,
			outStable,
			outStableDeep,
			inStable,
			inDiffAdvDone,
			inDiffAdvDoneMU,
			inStableDeep,
			inStableDeepDiffAdvDone,
		};
	}


	struct MeshElementSpecies : public spatial::MPoint<moving_boundary::CoordinateType,2> , public spatial::VolumeMonitor {
		typedef spatial::MeshDef<moving_boundary::CoordinateType,2> MeshDefinition; 
		typedef spatial::Mesh<moving_boundary::CoordinateType,2,MeshElementSpecies> MeshType; 
		typedef spatial::MPoint<moving_boundary::CoordinateType,2> base;
		typedef MeshElementSpecies OurType;
		typedef MeshElementNeighbor NeighborType;
		typedef MeshElementStateful::State State;
		typedef spatial::Segment<moving_boundary::CoordinateType,2> SegmentType; 

		static spatial::DiffuseAdvectCache *createCache(const MeshDefinition & meshDef);

		MeshElementSpecies(const MeshDefinition &owner,const size_t *n, const moving_boundary::CoordinateType *values)
			:base(n,values),
			mesh(owner),
			stateVar(State::initial),
			interiorVolume(-1),
			vol(0,this),
			segments_( ),
			amtMass(),
			amtMassTransient(),
			concValue(),
			sourceTermValues(concValue),
			indexToTimeVariable(-1),
			pPhysio(nullptr), //set after creation
			interiorNeighbors( ),
			neighbors(interiorNeighbors.data( )), //default to interior, update when id'd as boundary
			boundaryNeighbors(0),
			voronoiVolume( ),
			nOutside(0),
			velocity(0,0)
		{
			if (vcell_util::Logger::get( ).enabled(vcell_util::Logger::trace)) {
				logCreation( );
			}
		}
		//RFR
		bool isInside( ) const; 
		spatial::SurfacePosition mPos( ) const; 
		bool isOutside( ) const {
			return !isInside( );
		}
		bool isDeep( ) const; 
		bool isBoundary( ) const {
			return mPos( ) == spatial::boundarySurface;
		}

		/**
		* restore from persistent storage. 
		*/
		MeshElementSpecies(const MeshDefinition &owner, std::istream &is);

		/**
		* return proxy object identifying this for streaming to an ostream
		*/
		MeshElementSpeciesIdent ident( ) const {
			return MeshElementSpeciesIdent(*this);
		};

		int numSpecies( ) const {
			return mesh.numberSpecies( ); 
		}

		/**
		* allocate species storage
		* #setPhysiology( ) must be calle first
		*/
		void allocateSpecies(); 


		/**
		* set initial concentration 
		*/
		void setConcentration(size_t i, moving_boundary::BioQuanType c) {
			using namespace MeshElementStateful;
			if (state( ) != initialInside && state( ) != initialBoundary) {
				throw std::domain_error("setConcentration");
			}
			if (vcell_portable::isNotANumber(c)) {
				throw std::invalid_argument("setConcentration not a number");
				std::cout << "nan" << std::endl;
			}
			assert (i < numSpecies( ));
			assert ( c >= 0);
			if (c > 0) {
				amtMass[i] = c * volumePD( );
				concValue[i] = c;
			}
		}

		/**
		* return previous concentration value;
		* does not necessarily equal value of last call
		* to #setConcentration
		*/
		moving_boundary::BioQuanType concentration(size_t i) const { 
			assert (i < numSpecies( ));
			if (amtMass[i] > 0) {
				return amtMass[i] / volumePD( ); 
			}
			return 0;
		}


		/**
		* see #concentration
		*/
		moving_boundary::BioQuanType mass(size_t i) const { 
			assert (i < numSpecies( ));
			return amtMass[i];
		}

		/**
		* set position. See sequence diagram and state transition table
		*/
		void setPos(spatial::SurfacePosition m) ; 
		/**
		* set initial position
		*/
		void setInitialPos(spatial::SurfacePosition m); 

		void setPhysiology(const biology::Physiology &p) {
			if (p.numberSpecies( )!= numSpecies( )) {
				throw std::domain_error("wrong number species");
			}
			pPhysio = &p;
		}

		/**
		* legacy from prior state impl
		*/
		void beginSimulation( ) {
			using namespace moving_boundary::MeshElementStateful;
			switch (state( )) {
			case initialInside:
				setState(inStable);
				break;
			case initialBoundary:
				setState(bndStable);
				break;
			case outStable:
				break;
			default:
				badState("beginSimulation");
			}
		}

		/**
		* transfer interior neighbor edge length, distance and coordinate info to doppelganger
		*/

		void setInsideNeighbor(OurType & other, moving_boundary::DistanceType dist, moving_boundary::DistanceType length) {
			//scan inside neighbor storage, find first empty
			for (int i = 0; i < 4; i++) {
				MeshElementNeighbor & iNbhr = interiorNeighbors[i]; 
				if (iNbhr.element == nullptr) {
					iNbhr.element = &other; 
					iNbhr.distanceTo = dist;
					iNbhr.edgeLength = length;
					return;
				}
			}
			assert(false);
		}

		/**
		* initial setting of list of neighbors which are present 
		* this should be a boundary element. 
		* @param vm 
		* @param bn boundary neighbors inside front
		* @param front of interest
		*/
		void setBoundaryNeighbors(
			const VoronoiMesh &vm,
			std::vector<OurType *>  & bn, 
			const FrontType & front) {
				updateBoundaryNeighbors(vm,bn);
				formBoundaryPolygon(front);
		}

		/**
		* update volume to new front. Note the new control volume may not contain
		* center point -- the node is technically "outside" the front.
		* OLD: current (#mu) concentrations are updated
		* @mesh our mesh
		* @param front moved front
		*/
		void moveFront(const FrontType & front); 

		/**
		* update volume to new front. 
		* @mesh our mesh
		* @param front moved front
		* @param interiorVolume volume if node entirely interior 
		*/
		void applyFront( const FrontType & front, moving_boundary::CoordinateProductType interiorVolume); 
		//	using namespace MeshElementStateful;
		//	switch (state( )) {
		//		/*
		//	case bndFrontApplied:
		//	case bndFrontAppliedMU:
		//		//formBoundaryPolygon(mesh,front);
		//		//setState(stableUpdated);
		//		applyFrontLegacyVoronoiSet(front);
		//		break;
		//		*/
		//	case bndDiffAdvDone:
		//		if (mPos( ) == spatial::interiorSurface) { 
		//			setState(stableUpdated);
		//			setInteriorVolume(interiorVolume);
		//			neighbors = interiorNeighbors.data( );
		//			vol.clear( );
		//		}
		//		else {
		//			assert(mPos( ) == spatial::boundarySurface);
		//			setState(bndFrontApplied);
		//			applyFrontLegacyVoronoiSet(front);
		//		}
		//		break;
		//		/*
		//	case legacyInteriorSet:
		//	case legacyInteriorSetCollected:
		//		setInteriorVolume(interiorVolume);
		//		setState(stableUpdated);
		//		break;
		//		*/
		//	case transBndOut:
		//	//case stable:
		//		assert(this->isOutside( ));
		//		break;
		//	case stableUpdated:
		//	case transOutBndMassCollected:
		//		assert(this->isInside( ));
		//		break;
		//	default:
		//		VCELL_EXCEPTION(domain_error, "apply front " << ident( )); 
		//	}
		//}

		/**
		* update list of neighbors which are present 
		* this should be a boundary element
		* not all calls result in state transition
		* @param vm 
		* @param bn boundary neighbors inside front
		*/
		void updateBoundaryNeighbors( const VoronoiMesh &vm, std::vector<OurType *>  & bn) ;
		/**
		* indicate diffusion-advection processing complete; swap concentration pointers, check interior deepness
		*/
		void endOfCycle( );  

		/**
		* volume, in problem domain units
		*/
		moving_boundary::CoordinateProductType volumePD(  ) const {
			using MeshElementStateful::State;
			switch(this->state( ) ) {
			case State::initialInside:
			case State::inStable:
			case State::inStableDeep:

			case State::inStableDeepDiffAdvDone:
			case State::inDiffAdvDone:
			case State::inDiffAdvDoneMU:
			//case State:: stableUpdated:
			//case State:: stableSourceApplied:
				switch (this->mPos( )) {
				case spatial::interiorSurface:
				case spatial::deepInteriorSurface: 
					assert(interiorVolume > 0); //should be set externally 
					return interiorVolume;
				}
				return vol.volume( ) /distanceScaledSquared; 
				break;
			case State::transBndOut: 
			case State::transInBnd:
			case State::transOutBndSetBnd:
			case State::transOutBndNbrSet: 
			case State::transOutBndMassCollected: 
			case State::initialBoundary:
			case State::bndFrontMoved:
			case State::bndNbrEdgesFound:
			case State::bndDiffAdvDone:
			case State::bndDiffAdvDoneMU:
			case State::bndFrontApplied:
			case State::bndStable:
			//case State::legacySourceApplied:
			//case State::legacyInteriorSet:
			//case State::legacyInteriorSetCollected:
				return vol.volume( ) /distanceScaledSquared; 
			case State::transOutBndSetIn:
			default:
				badState("volumePD");
			}
			return 0;
		}

		void setInteriorVolume(moving_boundary::CoordinateProductType v) {
			interiorVolume = v;
		}

		/**
		* boundary element whose voronoi doesn't intersect front
		*/
		bool isBoundaryElementWithInsideNeighbors( ) const {
			return this->mPos( ) == spatial::boundarySurface && neighbors == interiorNeighbors.data( );
		}

		/**
		* distribute mass to neighbors 
		* (implies this element is being transBndOut)
		*/
		void distributeMassToNeighbors( ); 

		/**
		* collect mass from neighbors , if applicable
		*/
		void collectMass(const FrontType & front) {
			using namespace MeshElementStateful;
			if (state( ) == transOutBndNbrSet) {
				collectMassFromNeighbors(front);
			}
		}

		/**
		* return control volume for this -- possibly "expensive" for inside polygons as
		* will cause internal polygons to be constructed (if not already done)
		* @param mesh owning mesh
		*/
		const Volume2DClass & getControlVolume( ) const;
					

		const spatial::SVector<moving_boundary::VelocityType,2> & getVelocity( ) const {
			return velocity;
		}

		void setVelocity(const spatial::SVector<moving_boundary::VelocityType,2> & rhs) {
			velocity = rhs;
		}

		/**
		* apply source terms at specified time (expressions may contain "x" and "y" ... the nodes know that already)
		*/
		void react(moving_boundary::TimeType time); 
		/**
		* perform diffusion and advection step, storing values in next generation
		* @param daCache   
		* @param coefficient "D" -- coefficient to multiply change in concentration / distance 
		* @param timeStep time step 
		* @param negativeMassError set if mass goes negative 
		*/
		void diffuseAdvect(spatial::DiffuseAdvectCache & daCache,moving_boundary::BioQuanType coefficient, moving_boundary::TimeType timeStep, bool & negativeMassError); 

		/**
		* debug dump polygon && voronoi
		* @param os destination
		* @param noPoly if true, don't write polygon out
		* @param precision std::setprecision( ), 0 uses a default
		*/
		void writeMatlab(std::ostream & os , bool noPoly = false, int precision = 0) const;

		/**
		* set scale used to convert distances from problem domain to world coordinates;
		* used in DiffuseAdvect
		* @param pToW problem to world factor 
		*/
		static void setProblemToWorldDistanceScale(moving_boundary::BioQuanType pToW) {
			 distanceScaled        = pToW;
			 distanceScaledSquared = pToW * pToW;
		}

		/**
		* debug support
		*/
		bool matches (size_t xIndex, size_t yIndex, MeshElementStateful::State  particularState = static_cast<State>(-1)) const {
			if (! (this->index[spatial::cX] == xIndex && this->index[spatial::cY] == yIndex) ) {
				return false;
			}
			if (particularState != -1 && stateVar != particularState) {
				return false;
			}
			return true;
		}

		/**
		* write coordinates of boundary in text format 
		* @param dest 
		*/
		void listBoundary(std::ostream & os) const; 

		/**
		* find neighbor edges. We don't do this until after all the nodes have received the
		* new front #FrontMoved( )
		*/
		void findNeighborEdges(); 
		/**
		* clear segments
		*/
		virtual void volumeChanged( ) {
			segments_.clear( );
		}

		/**
		* return neighbor with specified offset. See also
		* Mesh#element( ...) 
		*/
		OurType *neighbor(spatial::ElementOffset<2> & eo) const;

		static void registerType( ) {
			base::registerType( );
			Volume2DClass::registerType( );
			NeighborType::registerType( );
			spatial::SVector<moving_boundary::VelocityType,2>::registerType( );
			SegmentType::registerType( );
			vcell_persist::Registrar::reg<MeshElementSpecies>("MeshElementSpecies");
		}

		void persist(std::ostream &);


	private:
		void badState(const char * const) const;
		/**
		* log creation information to trace file
		*/
		void logCreation( ) const ;
		/*
		VCELL_LOG(trace, "creation " << this->indexInfo( ) << " (" << this->get(spatial::cX) << ',' << this->get(spatial::cY) << ')'
		*/
		/**
		* rebuild and sort #segments from volume
		*/
		void volumeToSegments();
		/**
		* access #segments_; use function in case we want to optimize in future
		*/
		std::vector<SegmentType> & segments( ) {
			if (!segments_.empty( )) {
				return segments_;
			}
			volumeToSegments( );
			return segments_;
		}
		/**
		* const access #segments_; use function in case we want to optimize in future
		*/
		const std::vector<SegmentType> & segments( ) const {
			return segments_;
		}
		/**
		* @mesh our mesh
		* @param front moved front
		*/
		void applyFrontLegacyVoronoiSet(const FrontType & front);
		/**
		* collect mass to from neighbors 
		* (implies this element is new for this generation 
		*/
		void collectMassFromNeighbors(const FrontType & front); 

#		ifdef _MSC_VER	//a define in the frontier lib interferes with MSC's macro definition 
#		define isnan _isnan 
#		endif 
		/**
		* @param store #mu or #muTransient
		* @param i index
		* @param mass value
		*/
		/*
		void setMass(std::array<moving_boundary::BioQuanType,nOfS> & store, size_t i, moving_boundary::BioQuanType mass) {
			assert (i < nOfS);
			store[i] = mass;
			if (isnan(store[i])) {
				throw std::logic_error("setMass");
			}
		}
		*/
#		undef isnan
		/**
		* set list of neighbors which are present 
		* this should be a boundary element. 
		* @param vm  
		* @param bn boundary neighbors inside front
		*/
		void processBoundaryNeighbors(const VoronoiMesh & vm, std::vector<OurType *>  & bn);
		/**
		* return amount of overlap betwen current voronoi region and
		* specified previous volume
		* @param oldVolume volume to find overlap of
		* @return volume of overlap
		*/
		moving_boundary::CoordinateProductType voronoiOverlap(const Volume2DClass& oldVolume); 

		/**
		* return number of neighbors
		*/
		size_t numNeighbors( )  const {
			if (neighbors == interiorNeighbors.data( )) {
				return NUM_INSIDE;
			}
			return boundaryNeighbors.size( );
		}
		/**
		* create inside polygon -- used for inside elements and boundary elements whose vornoi
		* doesn't cross front
		* return is stored in vol
		* @param mesh mesh of simulation
		*/
		Volume2DClass createInsidePolygon(); 

		/**
		* form polygon from intersection of voronoi result and front, and 
		* calculate distances and edge lengths to neighbors
		* @param mesh ourMesh
		* @param front the front
		*/
		void formBoundaryPolygon(const FrontType & front); 

		/**
		* find neighbor's record of us, if it exists
		*/
		const NeighborType * findUs(OurType &us) const {
			for (size_t i = 0; i < numNeighbors( ); i++) {
				if (neighbors[i].element == &us) {
					return &neighbors[i];
				}
			}
			return nullptr;
		}

		/*
		void setCollected( ) {
			//static_assert(State::legacyInteriorSetCollected == State::legacyInteriorSet + 1,"collected not set up correctly");
			static_assert(State::bndFrontAppliedMU == State::bndFrontApplied + 1,"collected not set up correctly");
			//setState(static_cast<State>(stateVar + 1) );
			assert(stateVar == State::bndFrontApplied);
			setState(State::bndFrontAppliedMU);
		}
		*/

		/**
		* return distance to neighbor, if it's been calculated; otherwise return 0
		*/
		moving_boundary::DistanceType distanceToNeighbor(OurType &us) const {
			const NeighborType * nt = findUs(us);
			if (nt != nullptr) {
				return nt->distanceTo;
			}
			return 0;
		}
#ifdef MES_STATE_TRACK
		void DEBUG_SET_STATE(MeshElementStateful::State s, const char *file, int line);
#else 
		void setState(MeshElementStateful::State s) {
			using namespace MeshElementStateful;
			stateVar = s;
			VCELL_LOG(trace, ident( ) << " new state" ); 
			assert(debugSetState( ));
		}
#endif



		/*
		const std::vector<const biology::Species> & species( ) const {
			assert(psv != nullptr);
			assert(psv->size( ) == numSpecies( )); 
			return *psv;
		}
		*/

		/*
		* hook to allow setting specific break /log conditions in *cpp 
		* @returns true
		*/
		bool debugSetState( );

		MeshElementStateful::State state( ) const {
			return stateVar;
		}

		const biology::Physiology & physiology( ) const {
			assert(pPhysio != nullptr);
			return *pPhysio;
		}

		/**
		* debug only 
		*/
		void genDebugPlot(std::ostream & dest, const Volume2DClass &ourVolume, const Volume2DClass & intersection, const OurType & nb);

		const MeshDefinition &mesh;

		MeshElementStateful::State stateVar;
		/**
		* volume if this is an inside element -- otherwise volume comes from vol object
		*/
		moving_boundary::CoordinateProductType interiorVolume;
		/**
		* our control volume
		*/
		Volume2DClass vol;
		std::vector<SegmentType> segments_;
		std::vector<moving_boundary::BioQuanType> amtMass; //amount of mass
		std::vector<moving_boundary::BioQuanType> amtMassTransient; //amount of mass, working copy
		std::vector<moving_boundary::BioQuanType> concValue; //concentration at beginning of cycle
		/**
		* pointer to species vector
		*/
		const biology::Physiology * pPhysio;
		/**
		* values needed to evaluate source term
		* for now, assume the indexing of species in the Physiology is the same as the local values
		* and use the same vector as #concValue
		* { must be declared after concValue }
		*/
		static_assert(std::is_same<moving_boundary::BioQuanType,double>::value, "update implementation");
		std::vector<double> & sourceTermValues;
		size_t indexToTimeVariable;


		const static int NUM_INSIDE = 4;
		std::array<NeighborType,NUM_INSIDE> interiorNeighbors; 
		NeighborType * neighbors;
		std::vector<NeighborType> boundaryNeighbors;
		Volume2DClass voronoiVolume;
		int nOutside;
		spatial::SVector<moving_boundary::VelocityType,2> velocity; 

		/**
		* problem domain to solution coordinates scaled
		*/
		static moving_boundary::BioQuanType distanceScaled;
		/**
		* problem domain to solution coordinates scale, squared
		*/
		static moving_boundary::BioQuanType distanceScaledSquared;

		/**
		* boundary neighbor functor; declared as part of class to give acccess to typedefs
		*/
		struct SetupBoundaryNeighbor;


	private:
		/**
		* not implemented
		*/
		MeshElementSpecies & operator=(const MeshElementSpecies &rhs);
		friend struct MeshElementSpeciesIdent;
	};

	/**
	* exception when reverse lengths don't match
	*/
	struct ReverseLengthException : public std::logic_error {
		ReverseLengthException(const std::string & msg, MeshElementSpecies &a, const MeshElementSpecies &b) 
			:std::logic_error(msg),
			aElement(a),
			bElement(b) {}
		const MeshElementSpecies &aElement;
		const MeshElementSpecies &bElement;
	};

	/**
	* when element skips boundary state
	*/
	struct SkipsBoundary : public std::exception {
		MeshElementSpecies &mes;
		const spatial::SurfacePosition newPosition;
		std::string str;
		SkipsBoundary(MeshElementSpecies &m, spatial::SurfacePosition np) 
			:mes(m),
			newPosition(np),
			str( )
		{
				std::ostringstream oss;
				oss << "Skipped Boundary " << mes.ident( ) << " to " << newPosition << std::ends;
				str = oss.str( );
		}
		virtual ~SkipsBoundary( ) throw( ) {}
		virtual const char* what() const throw() {
			return str.c_str( );
		}
	};

	std::ostream & operator<<(std::ostream &,MeshElementStateful::State);

	inline void MeshElementSpeciesIdent::write(std::ostream &os) const {
		os << mes.indexInfo( ) << ' ' << mes.state( ); 
	}
}
#endif
