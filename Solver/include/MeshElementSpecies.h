#ifndef MeshElementSpecies_h
#define MeshElementSpecies_h
#include <fstream>
#include <vector>
#include <cmath>
#include <MPoint.h>
#include <Allocator.h>
#include <Volume.h>
#include <SVector.h>
#include <VoronoiMesh.h>
#include <VCellException.h>
#include <Logger.h>
#include <DiffuseAdvectCache.h>
#include <portability.h>
#include <math.h>

namespace spatial {
	struct VoronoiResult;

	//forward definitions
	template<class REAL, int N> struct MeshDef; 
	template<class REAL, int N, class TELEMENT> struct Mesh; 
	template <class REAL, int NUM_S> struct VoronoiMesh;
	template<class REAL, int NUM_S> struct MeshElementSpecies;
	template <class REAL, int NUM_S>
	struct MeshElementSpeciesIdent;

	/**
	* used by #MeshElementSpecies to store information about neighbors
	*/
	template<class REAL, int NUM_S>
	struct MeshElementNeighbor {
		MeshElementNeighbor( )
			:element(nullptr),
			distanceTo( ),
			edgeLength( )
			//daAmount(unset)
		{}
		MeshElementNeighbor(MeshElementSpecies<REAL,NUM_S> *e)
			:element(e),
			distanceTo( ),
			edgeLength( ) 
		{}
		MeshElementSpecies<REAL,NUM_S> *element; 
		REAL distanceTo;
		REAL edgeLength;
#if PROB_NOT_USE
		/**
		* check to see if diffuse-advect amount value set
		*/
		bool daSet( ) const {
			return daAmount != unset;
		}
		/**
		* reset diffuse-advect amount to "unset"
		*/
		void clearDa( ) {
			daAmount = unset;
		}
		/**
		* set diffuse-advect amount 
		*/
		void setDa(REAL v) {
			daAmount = v;
		}
		/**
		* get diffuse-advect amount 
		* @throws std::domain_error if unset
		*/
		REAL da( ) {
			if (daAmount != unset) {
				return daAmount;
			}
			VCELL_EXCEPTION(domain_error,"attempting to access unset da( ) for " << element ? element.indexInfo( ) : "unset element")
		}
	private:
		const static int unset = -99;
		/**
		* amount to diffuse-advect with this neighbor
		*/
		REAL daAmount;
#endif
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
			* front moved but vornoi not applied 
			*/
			legacyVolume,
			/**
			* diffusionAdvection applied (last state #legacyVolume)
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
			legacyInteriorSet,
			/**
			* interior volume and some mass has been collected (last state #legacyInteriorSet)
			*/
			legacyInteriorSetCollected,
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

	template<class REAL, int NUM_S>
	struct MeshElementSpecies : public MeshElement<REAL,2> {
		typedef MeshElement<REAL,2> base;
		typedef MeshElementSpecies<REAL,NUM_S> OurType;
		typedef MeshElementNeighbor<REAL,NUM_S> NeighborType;
		typedef MeshElementStateful::State State;
		typedef TDiffuseAdvectCache<REAL,2,NUM_S> DiffuseAdvectCache; 
		static DiffuseAdvectCache *createCache(REAL smallestMeshInterval); 
		static const int numSpecies = NUM_S; 

		MeshElementSpecies(const size_t *n, const REAL *values) 
			:base(n,values),
			stateVar(State::initial),
			interiorVolume(-1),
			vol( ),
			amtMass( ),
			amtMassTransient( ),
			interiorNeighbors( ),
			neighbors(interiorNeighbors), //default to interior, update when id'd as boundary
			boundaryNeighbors(0),
			voronoiVolume( ),
			nOutside(0),
			velocity(0,0)
		{
			VCELL_LOG(trace, "creation " << this->indexInfo( ) << " (" << this->get(cX) << ',' << this->get(cY) << ')');
		}


		/**
		* return proxy object identifying this for streaming to an ostream
		*/
		MeshElementSpeciesIdent<REAL,NUM_S> ident( ) const {
			return MeshElementSpeciesIdent<REAL,NUM_S>(*this);
		};

		/**
		* set initial concentration 
		*/
		void setConcentration(size_t i, REAL c) {
			using namespace MeshElementStateful;
			if (state( ) != initial) {
				throw std::domain_error("setConcentration");
			}
			if (vcell_portable::isNotANumber(c)) {
				throw std::invalid_argument("setConcentration not a number");
				std::cout << "nan" << std::endl;
			}
			assert (i < NUM_S);
			assert ( c >= 0);
			if (c > 0) {
				amtMass[i] = c * volume( );
				concValue[i] = c;
			}
		}

		/**
		* return previous concentration value;
		* does not necessarily equal value of last call
		* to #setConcentration
		*/
		REAL concentration(size_t i) const { 
			assert (i < NUM_S);
			if (amtMass[i] > 0) {
				return amtMass[i] / volume( ); 
			}
			return 0;
		}


		/**
		* see #concentration
		*/
		REAL mass(size_t i) const { 
			assert (i < NUM_S);
			return amtMass[i];
		}

		/**
		* set position. See sequence diagram and state transition table
		*/
		void setPos(SurfacePosition m) ; 
		/**
		* set initial position
		*/
		void setInitialPos(SurfacePosition m) {
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

		void setInsideNeighbor(OurType & other, REAL dist, REAL length) {
			//scan inside neighbor storage, find first empty
			for (int i = 0; i < 4; i++) {
				MeshElementNeighbor<REAL,NUM_S> & iNbhr = interiorNeighbors[i]; 
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
			const VoronoiMesh<REAL,NUM_S> &vm,
			//const MeshDef<REAL,2> & mesh, 
			std::vector<OurType *>  & bn, 
			const std::vector<TPoint<REAL,2> > & front) {
				updateBoundaryNeighbors(vm,bn);
				formBoundaryPolygon(vm.mesh( ),front);
		}

		/**
		* update volume to new front. Note the new control volume may not contain
		* center point -- the node is technically "outside" the front.
		* OLD: current (#mu) concentrations are updated
		* @mesh our mesh
		* @param front moved front
		*/
		void moveFront( const MeshDef<REAL,2> & mesh, const std::vector<TPoint<REAL,2> > & front); 

		/**
		* update volume to new front. 
		* @mesh our mesh
		* @param front moved front
		* @param interiorVolume volume if node entirely interior 
		*/
		void applyFront( const MeshDef<REAL,2> & mesh, const std::vector<TPoint<REAL,2> > & front, REAL interiorVolume) {
			using namespace MeshElementStateful;
			switch (state( )) {
			case legacyVoronoiSet:
			case legacyVoronoiSetCollected:
				//formBoundaryPolygon(mesh,front);
				//setState(stableUpdated);
				applyFrontLegacyVoronoiSet(mesh,front);
				break;
			case legacyInteriorSet:
			case legacyInteriorSetCollected:
				setInteriorVolume(interiorVolume);
				setState(stableUpdated);
				break;
			case lost:
			case stable:
				assert(this->isOutside( ));
				break;
			case stableUpdated:
			case gained:
				assert(this->isInside( ));
				break;
			default:
				VCELL_EXCEPTION(domain_error, "apply front " << this->indexInfo( ) << ' ' << state( ) << ' ' << this->mPos( ));
				//std::cout << "apply front " << this->indexInfo( ) << ' ' << state( ) << ' ' << this->mPos( )<< std::endl;
			}
		}

		/**
		* update list of neighbors which are present 
		* this should be a boundary element
		* @param vm 
		* @param bn boundary neighbors inside front
		* @param front of interest
		*/
		void updateBoundaryNeighbors( //const MeshDef<REAL,2> & mesh, 
			const VoronoiMesh<REAL,NUM_S> &vm,
			std::vector<OurType *>  & bn) ;
		//const std::vector<TPoint<REAL,2> > & front); 
		/**
		* indicate diffusion-advection processing complete; swap concentration pointers, check interior deepness
		*/
		void endOfCycle( );  

		REAL volume(  ) const {
			using MeshElementStateful::State;
			switch(this->state( ) ) {
			case State::initial:
			case State::stable: 
			case State:: stableUpdated:
				switch (this->mPos( )) {
				case interiorSurface:
				case deepInteriorSurface: 
					assert(interiorVolume > 0); //should be set externally 
					return interiorVolume;
				}
				return vol.volume( ); 
				break;
			case State::lost: 
			case State::awaitingNb:
			case State::gainedAwaitingNb:
			case State::gainedEmpty:
			case State::gained: 
			case State::legacyVolume:
			case State::legacyUpdated:
			case State::legacyVoronoiSet:
			case State::legacyVoronoiSetCollected:
			case State::legacyInteriorSet:
			case State::legacyInteriorSetCollected:
				return vol.volume( ); 
			case State::transient:
			default:
				VCELL_EXCEPTION(domain_error,"volume( ) called on " << ident( ));
			}
		}

		void setInteriorVolume(REAL v) {
			interiorVolume = v;
		}

		/**
		* boundary element whose voronoi doesn't intersect front
		*/
		bool isBoundaryElementWithInsideNeighbors( ) const {
			return this->mPos( ) == boundarySurface && neighbors == interiorNeighbors;
		}

		/**
		* distribute mass to neighbors 
		* (implies this element is being lost)
		*/
		void distributeMassToNeighbors(const MeshDef<REAL,2> & mesh); 

		/**
		* collect mass from neighbors , if applicable
		*/
		void collectMass(const MeshDef<REAL,2> & mesh, const std::vector<TPoint<REAL,2> > & front) {
			using namespace MeshElementStateful;
			if (state( ) == gainedEmpty) {
				collectMassFromNeighbors(mesh, front);
			}
		}

		/**
		* return control volume for this -- expensive for inside polygons as
		* will cause internal polygons to be constructed (if not already done)
		*/
		const Volume<REAL,2> & getControlVolume(const MeshDef<REAL,2> & mesh) const;

		const SVector<REAL,2> & getVelocity( ) const {
			return velocity;
		}

		void setVelocity(const SVector<REAL,2> & rhs) {
			velocity = rhs;
		}

		/**
		* perform diffusion and advection step, storing values in next generation
		* @param daCache   
		* @param coefficient "D" -- coefficient to multiply change in concentration / distance 
		* @param timeStep time step 
		* @param negativeMassError set if mass goes negative 
		*/
		void diffuseAdvect(DiffuseAdvectCache & daCache,REAL coefficient, REAL timeStep, bool & negativeMassError); 

		/**
		* debug dump polygon && voronoi
		* @param os destination
		* @param noPoly if true, don't write polygon out
		* @param precision std::setprecision( ), 0 uses a default
		*/
		void writeMatlab(std::ostream & os , bool noPoly = false, int precision = 0) const;

		/**
		* debug support
		*/
		bool matches (size_t xIndex, size_t yIndex, MeshElementStateful::State  particularState = static_cast<State>(-1)) const {
			if (! (this->index[cX] == xIndex && this->index[cY] == yIndex) ) {
				return false;
			}
			if (particularState != -1 && stateVar != particularState) {
				return false;
			}
			return true;
		}

		/*
		void clearDiffuseAdvectAmounts( ) {
		for (size_t i = 0; i < numNeighbors( ); i++) {
		neighbors[i].clearDa( );
		}
		}

		static void clearDiffuseAdvectCache( );
		static void checkDiffuseAdvectCache( );
		*/


	protected:
		/**
		* @mesh our mesh
		* @param front moved front
		*/
		void applyFrontLegacyVoronoiSet(const MeshDef<REAL,2> & mesh, const std::vector<TPoint<REAL,2> > & front);
		/**
		* collect mass to from neighbors 
		* (implies this element is new for this generation 
		*/
		void collectMassFromNeighbors(const MeshDef<REAL,2> & mesh, const std::vector<TPoint<REAL,2> > & front); 

#		ifdef _MSC_VER	//a define in the frontier lib interferes with MSC's macro definition 
#		define isnan _isnan 
#		endif 
		/**
		* @param store #mu or #muTransient
		* @param i index
		* @param mass value
		*/
		void setMass(std::array<REAL,NUM_S> & store, size_t i, REAL mass) {
			assert (i < NUM_S);
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
		void processBoundaryNeighbors(const VoronoiMesh<REAL,NUM_S> & vm, std::vector<OurType *>  & bn);
		/**
		* return amount of overlap betwen current voronoi region and
		* specified previous volume
		* @param oldVolume volume to find overlap of
		* @return volume of overlap
		*/
		REAL voronoiOverlap(const Volume<REAL,2> &oldVolume); 

		/**
		* return number of neighbors
		*/
		size_t numNeighbors( )  const {
			if (neighbors == interiorNeighbors) {
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
		Volume<REAL,2> createInsidePolygon(const MeshDef<REAL,2> & mesh);

		/**
		* form polygon from intersection of voronoi result and front, and 
		* calculate distances and edge lengths to neighbors
		* @param ourMesh
		* @param front the front
		*/
		void formBoundaryPolygon( const MeshDef<REAL,2> & mesh, const std::vector<TPoint<REAL,2> > & front);

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
		double distanceToNeighbor(OurType &us) const {
			const NeighborType * nt = findUs(us);
			if (nt != nullptr) {
				return nt->distanceTo;
			}
			return 0;
			/*
			for (std::vector<NeighborType>::const_iterator iter =  boundaryNeighbors.begin( ); iter != boundaryNeighbors.end( ); ++iter) {
			if (iter->element == &us) {
			return iter->distanceTo;
			}
			}
			return 0;
			*/
		}

		void setCollected( ) {
			static_assert(State::legacyInteriorSetCollected == State::legacyInteriorSet + 1,"collected not set up correctly");
			static_assert(State::legacyVoronoiSetCollected == State::legacyVoronoiSet + 1,"collected not set up correctly");
			setState(static_cast<State>(stateVar + 1) );
		}

		void setState(MeshElementStateful::State s) {
			using namespace MeshElementStateful;
			stateVar = s;
			VCELL_LOG(trace, ident( ) << " new state" ); 
		}

		MeshElementStateful::State state( ) const {
			return stateVar;
		}


		MeshElementStateful::State stateVar;
		/**
		* volume if this is an inside element -- otherwise volume comes from vol object
		*/
		REAL interiorVolume;
		Volume<REAL,2> vol;
		//REAL mu[NUM_S]; //species concentration
		//REAL muTransient[NUM_S]; //species concentration after advection
		std::array<REAL,NUM_S> amtMass; //amount of mass
		std::array<REAL,NUM_S> amtMassTransient; //amount of mass, working copy
		std::array<REAL,NUM_S> concValue; //concentration at beginning of cycle

		const static int NUM_INSIDE = 4;
		NeighborType interiorNeighbors[NUM_INSIDE]; 
		NeighborType * neighbors;
		std::vector<NeighborType> boundaryNeighbors;
		Volume<REAL,2> voronoiVolume;
		int nOutside;
		SVector<REAL,2> velocity; 


		/**
		* boundary neighbor functor
		*/
		struct SetupBoundaryNeighbor;

	private:
		/**
		* not implemented
		*/
		MeshElementSpecies & operator=(const MeshElementSpecies &rhs);
		friend struct MeshElementSpeciesIdent<REAL,NUM_S>;
	};

	/**
	* proxy object to stream identifying information about MeshElementSpecies
	*/
	template <class REAL, int NUM_S>
	struct MeshElementSpeciesIdent {
		const MeshElementSpecies<REAL,NUM_S> &mes;
		MeshElementSpeciesIdent(const MeshElementSpecies<REAL,NUM_S> &m)
			:mes(m) {}
		void write(std::ostream &os) const {
			os << mes.indexInfo( ) << ' ' << mes.mPos( ) << ' ' << mes.state( );
		}
	};

	/**
	* exception when reverse lengths don't match
	*/
	template <class REAL, int NUM_S>
	struct ReverseLengthException : public std::logic_error {
		typedef MeshElementSpecies<REAL,NUM_S> Mes;
		ReverseLengthException(const std::string & msg, Mes &a, const Mes &b) 
			:std::logic_error(msg),
			aElement(a),
			bElement(b) {}
		const Mes &aElement;
		const Mes &bElement;
	};

	template <class REAL, int NUM_S>
	inline std::ostream & operator<<(std::ostream & os,const MeshElementSpeciesIdent<REAL,NUM_S> & mesi) {
		mesi.write(os);
		return os;
	}

	std::ostream & operator<<(std::ostream &,MeshElementStateful::State);
}
#endif
