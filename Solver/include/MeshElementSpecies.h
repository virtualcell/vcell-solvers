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
//#include <VoronoiMesh.h>
#include <VCellException.h>
#include <Logger.h>
#include <DiffuseAdvectCache.h>
#include <portability.h>
#include <math.h>
#include <persist.h>

//forward definitions
namespace spatial {
	template<class COORD_TYPE, int N> struct MeshDef; 
	template<class COORD_TYPE, int N, class TELEMENT> struct Mesh; 
	//struct VoronoiResult;
}

namespace moving_boundary {
	//placeholder until we convert fixed std::array to dynamic storage
	const size_t nOfS = 1;
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
		enum State { 
			/**
			* initial state. #setPos to #stable
			*/
			initial,
			/**
			* begin/end of move diffuse advect cycle
			*/
			stable, 
			/**
			* diffusionAdvection applied
			*/
			stableUpdated,
			/**
			* moved outside boundary
			*/
			lost, 
			/**
			* moved from interior to boundary
			*/
			awaitingNb,
			/**
			* moved inside boundary (from #setPos)
			*/
			gainedAwaitingNb,
			/**
			* neighbors applied (last state #gainedAwaitingNb)
			*/
			gainedEmpty,
			/**
			* mass collected (last state #gainedEmpty)
			*/
			gained, 
			/**
			* front moved but neighbor edges stale, voronoi not applied 
			*/
			legacyVolume,
			/**
			* front moved, neighbor edges set, voronoi not applied 
			*/
			legacyVolumeNeighborSet,
			/**
			* diffusionAdvection applied (last state #legacyVolumeNeighborSet)
			*/
			legacyUpdated,
			/**
			* neighbors updated: (last state #legacyUpdated, #awaitingNb)
			*/
			legacyVoronoiSet,
			/**
			* neighbors updated and some mass has been collected: (last state #legacyVoronoiSet)
			*/
			legacyVoronoiSetCollected,
			/**
			* must be one more than #legacySetCollected
			* interior volume is set, from #setPos, (last state #legacyUpdated)
			*/
			//legacyInteriorSet,
			/**
			* interior volume and some mass has been collected (last state #legacyInteriorSet)
			*/
			//legacyInteriorSetCollected,
			/** 
			* must be one more than #legacyInteriorSet
			* psuedo-state which may occur during reclassification
			*/
			transient
		};
	}

	/**
	* Allowable transitions. First transition from <i>initial</i> to <i>stable</i> omitted for simplicity.
	<table><tr><th></th><th>stable</th><th>stableUpdated</th><th>awaitingNb</th><th>lost</th><th>gainedAwaitingNb</th><th>gainedEmpty</th><th>gained</th><th>legacyVolume</th><th>legacyUpdated</th><th>legacyVoronoiSet</th><th>legacyInterior</th></tr>
	<tr><td>stable</td><td></td><td>diffuseAdvect</td><td></td><td></td><td>setPos(O->B)</td><td></td><td></td><td>moveFront</td><td></td><td></td></tr>
	<tr><td>stableUpdated</td><td>endOfCycle</td><td></td>setPos(I->B)<td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
	<tr><td>awaitingNb</td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td>updateNb</td></tr>
	<tr><td>lost</td><td>distribute</td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
	<tr><td>gainedAwaitingNb</td><td></td><td></td><td></td><td></td><td></td><td>updateNb</td><td></td><td></td><td></td><td></td></tr>
	<tr><td>gainedEmpty</td><td></td><td></td><td></td><td></td><td></td><td></td><td>collectMass</td><td></td><td></td><td></td></tr>
	<tr><td>gained</td><td>endOfCycle</td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
	<tr><td>legacyVolume</td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td>diffuseAdvect</td><td></td></tr>
	<tr><td>legacyUpdated</td><td></td><td></td><td></td><td>setPos(0)</td><td></td><td></td><td></td><td></td><td></td><td>updateNb</td><td>setPos(I)
	<tr><td>legacyVoronoiSet</td><td></td><td>applyFront</td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr>
	<tr><td>legacyInterior</td><td></td><td>setVol</td><td></td><td></td><td></td><td></td><td></td><td></td><td></td><td></td></tr></table>
	* Event sequence. See state diagram for which elements calls made on.
	* <ul>
	* <li>setPos</li>
	* <li>moveFront</li>
	* <li>diffuseAdvect</li>
	* <li>setPos</li>
	* <li>updateBoundary</li>
	* <li>collectMass</li>
	* <li>applyFront</li>
	* <li>distribute</li>
	* <li>endOfCycle</li>
	* <li>transient <i>not a real state, but needed due to reclassification algorithm</i><li>
	* </ul> 
	*/

	struct MeshElementSpecies : public spatial::MeshElement<moving_boundary::CoordinateType,2> , public spatial::VolumeMonitor {
		typedef spatial::MeshDef<moving_boundary::CoordinateType,2> MeshDefinition; 
		typedef spatial::Mesh<moving_boundary::CoordinateType,2,MeshElementSpecies> MeshType; 
		typedef spatial::MeshElement<moving_boundary::CoordinateType,2> base;
		typedef MeshElementSpecies OurType;
		typedef MeshElementNeighbor NeighborType;
		typedef MeshElementStateful::State State;
		typedef spatial::Segment<moving_boundary::CoordinateType,2> SegmentType; 

		static spatial::DiffuseAdvectCache *createCache(const MeshDefinition & meshDef);
		static const int numSpecies = nOfS; 

		MeshElementSpecies(const MeshDefinition &owner,const size_t *n, const moving_boundary::CoordinateType *values) 
			:base(n,values),
			mesh(owner),
			stateVar(State::initial),
			interiorVolume(-1),
			vol(0,this),
			segments_( ),
			amtMass( ),
			amtMassTransient( ),
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

		/**
		* set initial concentration 
		*/
		void setConcentration(size_t i, moving_boundary::BioQuanType c) {
			using namespace MeshElementStateful;
			if (state( ) != initial) {
				throw std::domain_error("setConcentration");
			}
			if (vcell_portable::isNotANumber(c)) {
				throw std::invalid_argument("setConcentration not a number");
				std::cout << "nan" << std::endl;
			}
			assert (i < nOfS);
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
			assert (i < nOfS);
			if (amtMass[i] > 0) {
				return amtMass[i] / volumePD( ); 
			}
			return 0;
		}


		/**
		* see #concentration
		*/
		moving_boundary::BioQuanType mass(size_t i) const { 
			assert (i < nOfS);
			return amtMass[i];
		}

		/**
		* set position. See sequence diagram and state transition table
		*/
		void setPos(spatial::SurfacePosition m) ; 
		/**
		* set initial position
		*/
		void setInitialPos(spatial::SurfacePosition m) {
			using namespace MeshElementStateful;
			if (state( )!=initial) {
				throw std::domain_error("setInitialPos");
			}
			base::setPos(m);
			VCELL_LOG(trace,this->indexInfo( ) << " initial position " << m)
		}

		void beginSimulation( ) {
			using namespace MeshElementStateful;
			if (state( )!=initial) {
				throw std::domain_error("beginSimulation");
			}
			setState(stable);
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
		void applyFront( const FrontType & front, moving_boundary::CoordinateProductType interiorVolume) {
			using namespace MeshElementStateful;
			switch (state( )) {
			case legacyVoronoiSet:
			case legacyVoronoiSetCollected:
				//formBoundaryPolygon(mesh,front);
				//setState(stableUpdated);
				applyFrontLegacyVoronoiSet(front);
				break;
			case legacyUpdated:
				if (mPos( ) == spatial::interiorSurface) { 
					setState(stableUpdated);
					setInteriorVolume(interiorVolume);
					neighbors = interiorNeighbors.data( );
					vol.clear( );
				}
				else {
					assert(mPos( ) == spatial::boundarySurface);
					setState(legacyVoronoiSet);
					applyFrontLegacyVoronoiSet(front);
				}
				break;
				/*
			case legacyInteriorSet:
			case legacyInteriorSetCollected:
				setInteriorVolume(interiorVolume);
				setState(stableUpdated);
				break;
				*/
			case lost:
			case stable:
				assert(this->isOutside( ));
				break;
			case stableUpdated:
			case gained:
				assert(this->isInside( ));
				break;
			default:
				VCELL_EXCEPTION(domain_error, "apply front " << ident( )); 
			}
		}

		/**
		* update list of neighbors which are present 
		* this should be a boundary element
		* @param vm 
		* @param bn boundary neighbors inside front
		* @param front of interest
		*/
		void updateBoundaryNeighbors( 
			const VoronoiMesh &vm,
			std::vector<OurType *>  & bn) ;
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
			case State::initial:
			case State::stable: 
			case State:: stableUpdated:
				switch (this->mPos( )) {
				case spatial::interiorSurface:
				case spatial::deepInteriorSurface: 
					assert(interiorVolume > 0); //should be set externally 
					return interiorVolume;
				}
				return vol.volume( ) /distanceScaledSquared; 
				break;
			case State::lost: 
			case State::awaitingNb:
			case State::gainedAwaitingNb:
			case State::gainedEmpty: 
			case State::gained: 
			case State::legacyVolume:
			case State::legacyVolumeNeighborSet:
			case State::legacyUpdated:
			case State::legacyVoronoiSet:
			case State::legacyVoronoiSetCollected:
			//case State::legacyInteriorSet:
			//case State::legacyInteriorSetCollected:
				return vol.volume( ) /distanceScaledSquared; 
			case State::transient:
			default:
				VCELL_EXCEPTION(logic_error,"volume( ) called on " << ident( ));
			}
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
		* (implies this element is being lost)
		*/
		void distributeMassToNeighbors( ); 

		/**
		* collect mass from neighbors , if applicable
		*/
		void collectMass(const FrontType & front) {
			using namespace MeshElementStateful;
			if (state( ) == gainedEmpty) {
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
		void setMass(std::array<moving_boundary::BioQuanType,nOfS> & store, size_t i, moving_boundary::BioQuanType mass) {
			assert (i < nOfS);
			store[i] = mass;
			if (isnan(store[i])) {
				throw std::logic_error("setMass");
			}
		}
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

		void setCollected( ) {
			//static_assert(State::legacyInteriorSetCollected == State::legacyInteriorSet + 1,"collected not set up correctly");
			static_assert(State::legacyVoronoiSetCollected == State::legacyVoronoiSet + 1,"collected not set up correctly");
			setState(static_cast<State>(stateVar + 1) );
		}

		void setState(MeshElementStateful::State s) {
			using namespace MeshElementStateful;
			stateVar = s;
			VCELL_LOG(trace, ident( ) << " new state" ); 
			assert(debugSetState( ));
		}

		/*
		* hook to allow setting specific break /log conditions in *cpp 
		* @returns true
		*/
		bool debugSetState( );

		MeshElementStateful::State state( ) const {
			return stateVar;
		}

		const MeshDefinition &mesh;

		struct Helper {
			void setDirty( ) {};
			void destroyed( ) {};
		};
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
		std::array<moving_boundary::BioQuanType,nOfS> amtMass; //amount of mass
		std::array<moving_boundary::BioQuanType,nOfS> amtMassTransient; //amount of mass, working copy
		std::array<moving_boundary::BioQuanType,nOfS> concValue; //concentration at beginning of cycle

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
		os << mes.indexInfo( ) << ' ' << mes.mPos( ) << ' ' << mes.state( ); 
	}
}
#endif
