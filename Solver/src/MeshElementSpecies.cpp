#include <MPoint.h>
#include <sstream>
#include <exception>
#include <algorithm>
#include <VCellFront.h>
#include <Mesh.h>
#include <algo.h>
#include <intersection.h>
#include <MovingBoundaryParabolicProblem.h>
#include <MeshElementSpecies.h>
#include <SegmentIterator.h>
#include <LoopIterator.h>
#include <create.h>
#include <stack_container.h>
#include <vcellutil.h>
#include <Logger.h>

#include <iomanip>
#include <ManagedArrayPtr.h> //StackPtr
#include <MBridge/FronTierAdapt.h>
#include <MBridge/Figure.h>
#include <MBridge/MatlabDebug.h>

//tolerances
namespace {
	/**
	* edge lengths calculated between cells should be this close
	*/
	const double toleranceEdgeLengthException = 1e-10;
	/**
	* diffusion to and from cells must be within this amount
	*/
	const double toleranceDiffuseAdvectException = 1e-10;
	/**
	* distance between neighbors should be within this amount
	*/
	const double toleranceNeighborDistancesAssert = 1e-10;
	/**
	* difference squared difference, relative to mesh.mininumInterval<br>
	* used to see if segments are perpendicular between point and therefore d-a edges
	*/
	const double tolerancePerpendicularSegmentDistanceSquared = 1e-3;
}


//local definitions
namespace {

	int debugAid = 0;

	/**
	* used to compare forward and backward diffuse advect values
	*/
	struct CheckValue {
		CheckValue(bool r = false, double v = 0)
			:reversed(r),
			value(v) {}
		bool reversed; 
		/**
		* mass transferred
		*/
		double value;
	};


	template<class REAL, int NUM_S>
	struct DaCache : public spatial::TDiffuseAdvectCache<REAL,2,NUM_S> { 
		typedef spatial::MeshElementSpecies<REAL,NUM_S> MES;
		typedef std::pair<const MES * ,const MES *> MesPair;
		typedef std::map<MesPair,CheckValue> Map;
		DaCache<REAL,NUM_S>(REAL minimumMeshInterval) 
			:edgeLengthTolerance(minimumMeshInterval * toleranceEdgeLengthException) {
		}

		virtual void clearDiffuseAdvectCache( ) {
			diffuseAdvectMap.clear( );
		}

		virtual void checkDiffuseAdvectCache( ) {
			typename std::map<MesPair,CheckValue>::const_iterator iter = diffuseAdvectMap.begin( ); 
			for (;iter != diffuseAdvectMap.end( ); ++iter) {
				const CheckValue & cv = iter->second;
				if (!cv.reversed && cv.value != 0) {
					const MesPair & ePair = iter->first;
#ifdef PENDING_DISCUSSION
					VCELL_EXCEPTION(logic_error,ePair.first->indexInfo( ) 
						<< " to " << ePair.second->indexInfo( ) << " not reversed" << std::endl);
#else
					VCELL_LOG(warn, ePair.first->indexInfo( ) 
						<< " to " << ePair.second->indexInfo( ) << " with mass " << cv.value
						<< " not reversed" << std::endl);
#endif
				}
			}
		}

		Map diffuseAdvectMap;
		const REAL edgeLengthTolerance; 
	};
}

using spatial::MeshElementSpecies;
using spatial::cX;
using spatial::cY;
using spatial::TPoint;
using spatial::EdgeFindResult;
using spatial::EdgeStateful;
using namespace spatial::MeshElementStateful;
using matlabBridge::MatLabDebug;
template<class REAL, int NUM_S>
spatial::TDiffuseAdvectCache<REAL,2,NUM_S> * spatial::MeshElementSpecies<REAL,NUM_S>::createCache(REAL minMeshInterval) {
	return new DaCache<REAL,NUM_S>(minMeshInterval);
}

template<class REAL, int NUM_S>
void spatial::MeshElementSpecies<REAL,NUM_S>::setPos(SurfacePosition m)  {
	if (m == this->mPos( ) || (m == spatial::interiorSurface && this->mPos( ) == spatial::deepInteriorSurface) ){
		return;
	}
	VCELL_LOG(trace,this->indexInfo( ) << " position " << m << " from " << this->mPos( ));
	VCELL_KEY_LOG(fatal,"setPos",this->indexInfo( ) << " position " << m << " from " << this->mPos( ));
	switch (state( )) {
	case initial:
		if (this->mPos( ) != unset) {
			throw std::domain_error("initial not unset");
		}
		setState(stable);
		break;
	case stableUpdated:
		switch (m) {
		case deepInteriorSurface: 
		case interiorSurface:
		case outsideSurface:
		case deepOutsideSurface:
			VCELL_EXCEPTION(domain_error,this->indexInfo( ) << " position " << m << " from " << this->mPos( ));
			break;
		case boundarySurface:
			setState(awaitingNb);
			break;
		default:
			assert(0);
		}
		break;
	case stable:
		switch (m) {
		case boundarySurface:
			throw std::domain_error("s");
		case interiorSurface:
			setState(transient);
			break;
		case deepInteriorSurface: 
			throw std::domain_error("stable -> deep interior");
		case outsideSurface:
		case deepOutsideSurface:
			throw std::domain_error("stable -> ext");
		}
		break;
	case legacyInteriorSet:
		//this occurs during reclassification B->I->B
		if (m  != boundarySurface) {
			throw std::domain_error("lis not boundary");
		}
		setState(legacyUpdated); //set back to what it was
		break;
	case legacyUpdated: 
		switch (m) {
		case interiorSurface:
			setState(legacyInteriorSet); 
			break;
		case outsideSurface:
			//not deepOutside -- too far
			setState(lost);
			break;
		default:
			assert(0);
		}
		break;
	case transient:
		switch (m) {
		case interiorSurface:
			setState(stableUpdated);
			break;
		case deepInteriorSurface: 
		case outsideSurface:
			VCELL_EXCEPTION(domain_error,this->indexInfo( ) << " position " << m << " from " << this->mPos( ));
			break;
		case boundarySurface:
			setState(gainedAwaitingNb);
			break;
		default:
			assert(0);
		}
		break;

	default:
		VCELL_EXCEPTION(domain_error,this->indexInfo( ) << " state " << state( ) 
			<< " currently " << this->mPos( ) << " being set to " << m);
	}
	base::setPos(m);
	switch (this->mPos( )) {
	case deepInteriorSurface:
	case interiorSurface:
		neighbors = interiorNeighbors; 
		break;
	case outsideSurface:
	case deepOutsideSurface:
		break; //keep neighbors pointer intact for #distributeLost
	case boundarySurface:
		//NOTE: neighbors will need to be reset if boundaryNeighbors vector resizes
		neighbors = boundaryNeighbors.data( ); 
		break;
	}
}

template<class REAL, int NUM_S>
struct spatial::MeshElementSpecies<REAL,NUM_S>::SetupBoundaryNeighbor : public std::unary_function<OurType *,NeighborType> {
	OurType &clientElement;
	SetupBoundaryNeighbor(OurType &client)
		:clientElement(client) {}

	NeighborType operator( )(OurType *nb) {
		NeighborType rval;
		rval.element = nb;

		const double existing = nb->distanceToNeighbor(clientElement);
		if (existing > 0) {
			rval.distanceTo = existing; 
			assert(spatial::nearlyEqual(existing,spatial::distance(*nb,clientElement), toleranceNeighborDistancesAssert) );
		}
		else {
			rval.distanceTo = spatial::distance(*nb,clientElement);
		}

		return rval;
	};
};

template<class REAL, int NUM_S>
void spatial::MeshElementSpecies<REAL,NUM_S>::processBoundaryNeighbors(const VoronoiMesh<REAL,NUM_S> & vm, std::vector<OurType *>  & bn) {
	VCELL_LOG(trace,this->indexInfo( ) << " processBoundaryNeighors " << state( ) << ' ' << this->mPos( ));
	//getting vornoi volume and setting up boundary neighbors in same function due to legacy reasons ...
	{ //clarity scope, building voronoiVolume
		VoronoiResult vResult;
		std::vector<GhostPoint<REAL,2> > &voronoiVertices = vResult.vertices;
		assert(voronoiVertices.empty( ));

		vm.getResult(vResult,*this);

		typedef TPoint<REAL,2> VPointType;
		typedef TPoint<REAL,2> MeshPointType;

		voronoiVolume.clear( );
		typename std::vector<TPoint<REAL,2> >::iterator fIter = voronoiVolume.fillingIterator(voronoiVertices.size());
		std::copy(voronoiVertices.begin( ),voronoiVertices.end( ),fIter);

		//check for single open line
		if (vResult.type == VoronoiResult::straightLine) { 
			assert(voronoiVertices.size( ) == 3);
			SVector<REAL,2> delta(*this,voronoiVertices[1]);
			delta *= -3;
			//go other direction to build box
			//delta.reverse( );
			VPointType add1 = spatial::displacement(voronoiVertices[2],delta);
			VPointType add2 = spatial::displacement(voronoiVertices[0],delta);
			voronoiVolume.add(add1);
			voronoiVolume.add(add2);
		}
		voronoiVolume.close( );
	}
	boundaryNeighbors.resize(bn.size( ));
	std::transform(bn.begin( ),bn.end( ),boundaryNeighbors.begin( ), SetupBoundaryNeighbor(*this));

	neighbors = boundaryNeighbors.data( );
}

template<class REAL, int NUM_S>
void spatial::MeshElementSpecies<REAL,NUM_S>::moveFront( const MeshDef<REAL,2> & mesh, const std::vector<TPoint<REAL,2> > & front) {
	using namespace MeshElementStateful;
	if (this->isInside( )) {
		if (state( ) != stable) {
			throw std::domain_error("moveFront not stable");
		}
		const REAL oldVolume = vol.volume( );
		formBoundaryPolygon(mesh,front);
		setState(legacyVolume);
	}
}


//this should be combined with moveFront, above
template<class REAL, int NUM_S>
void spatial::MeshElementSpecies<REAL,NUM_S>::applyFrontLegacyVoronoiSet( const MeshDef<REAL,2> & mesh, const std::vector<TPoint<REAL,2> > & front) {
	assert(!voronoiVolume.empty( ));
	assert(state( ) == legacyVoronoiSet || state( ) == legacyVoronoiSetCollected); 
	formBoundaryPolygon(mesh,front);
	setState(stableUpdated);
}

template<class REAL, int NUM_S>
void spatial::MeshElementSpecies<REAL,NUM_S>::formBoundaryPolygon( const MeshDef<REAL,2> & mesh, const std::vector<TPoint<REAL,2> > & front) {
	static std::ofstream ef("edgefind.m");
	MatLabDebug::setDebug(ef);
	assert(!voronoiVolume.empty( ));
	//VoronoiResult &vResult = *pVoronoiResult;
	VCELL_LOG(debug,this->indexInfo( ) << " formBoundaryPolygon old volume " << vol.volume( ));
	vol = voronoiVolume.intersection(front); 
	if (vol.empty( )) {
		matlabBridge::Polygon pFront("r",1);
		frontTierAdapt::copyVectorInto(pFront,front);
		matlabBridge::Polygons vs("-g",2);
		frontTierAdapt::copyVectorsInto(vs,voronoiVolume.points( ));
		{
			std::ofstream eDebug("debugEmpty.m");
			eDebug << pFront << vs;
		}
		throw std::logic_error("empty boundary front");
	}
	VCELL_LOG(debug,this->indexInfo( ) << " new volume " << vol.volume( )) ;
	if (MatLabDebug::on("meshvoronoi")) { 
		matlabBridge::Polygons pPolys("k",3);
		frontTierAdapt::copyVectorsInto(pPolys,vol.points( ));
		matlabBridge::Polygons vs("-r",1);
		frontTierAdapt::copyVectorsInto(vs,voronoiVolume.points( ));
		MatLabDebug::stream() << pPolys << vs << matlabBridge::pause << matlabBridge::clearFigure;
	}
	std::vector<Edge<REAL,2> > neighborEdges(boundaryNeighbors.size( ));
	for (size_t i = 0; i < boundaryNeighbors.size( ); i++) {
		//assert(boundaryNeighbors[i].edgeLength == 0);
		boundaryNeighbors[i].edgeLength = 0;
		neighborEdges[i] = Edge<REAL,2>(*this,*boundaryNeighbors[i].element);
	}

	/**
	* search for segments whose end points are equidistant between us and neighbor
	*/
	matlabBridge::Polygon trace(":+g");
	matlabBridge::Polygon pedge("-+r",2);
	bool writeTrace = matches(7,8) || matches(8,7);
	//bool writeTrace = false; 
	MatLabDebug::activate("edgefind",writeTrace);

	//REAL minimumSegSquared = mesh.minimumInterval( ) * toleranceMinimumSegmentSquared;
	typedef TPoint<REAL,2> MeshPointType;
	spatial::EdgeAccessor<REAL,2> accsr = vol.accessor( );
	for (;accsr.hasNext( );accsr.next( )) {
		debugAid++;
		Edge<REAL,2> edge = accsr.get( );
		/*
		if (edge.edgeVector( ).magnitudeSquared( ) < minimumSegSquared) {
		VCELL_LOG(debug,indexInfo( ) << " skipping short segment " << edge.origin( ) << " to " << edge.tail( )
		<< ' ' << std::setprecision(12) << edge.edgeVector( ).magnitude( ));
		continue;
		}
		*/
		REAL hDistSquare = spatial::distanceSquared(*this,edge.origin( ));
		const MeshPointType & tail = edge.tail( );
		REAL tDistSquare = spatial::distanceSquared(*this,tail);

		for (size_t i = 0; i < boundaryNeighbors.size( ); i++) {
			const OurType & nb = *boundaryNeighbors[i].element;
			debugAid++;
			REAL nbHDistSquare = spatial::distanceSquared(nb,edge.origin( ));
			if (smallRelativeDifference(hDistSquare - nbHDistSquare,mesh.minimumInterval( ),tolerancePerpendicularSegmentDistanceSquared)) {
				REAL nbTDistSquare = spatial::distanceSquared(nb,tail);
				if (smallRelativeDifference(tDistSquare - nbTDistSquare, mesh.minimumInterval( ),tolerancePerpendicularSegmentDistanceSquared)) {
					const REAL length = edge.edgeVector( ).magnitude( );
					VCELL_KEY_LOG(debug,"MeshElementSpecies.formBoundaryPolygon",this->indexInfo( ) << " between " << nb.indexInfo( ) << " vector " << edge.edgeVector( ) << " length " 
						<< std::setprecision(12) << length << " <" <<  std::setprecision(12) << edge.origin( ) << " : " 
						<< std::setprecision(12) << tail << " >");
					boundaryNeighbors[i].edgeLength = std::max(boundaryNeighbors[i].edgeLength,length);
					if (MatLabDebug::on("edgefind")) {
						frontTierAdapt::copyPointInto(pedge, edge.origin());
						frontTierAdapt::copyPointInto(pedge,tail);
						std::ostream & os = MatLabDebug::stream( );
						os << pedge;
						pedge.clear( );
					}
				}
				else {
					VCELL_KEY_LOG(verbose,"edgeTrace",this->ident( ) << " and " << nb.ident( ) << " vector " << edge.edgeVector( ) 
						<< " tail rejected, this is " << tDistSquare  
						<< " and neighbor is " << nbTDistSquare);
				}
			}
			else {
				VCELL_KEY_LOG(verbose,"edgeTrace",this->ident( ) << " and " << nb.ident( ) << " vector " << edge.edgeVector( )  
					<< " head rejected, this is " << hDistSquare  
					<< " and neighbor is " << nbHDistSquare);
			}
		}
		if (MatLabDebug::on("edgefind")) {
			frontTierAdapt::copyPointInto(trace,edge.origin( ));
		}
	}
	if (MatLabDebug::on("edgefind")) {
		matlabBridge::Scatter nbplot('b',2);
		frontTierAdapt::copyPointInto(nbplot,*this);
		for (size_t i = 0; i < boundaryNeighbors.size( ); i++) {
			const OurType & nb = *boundaryNeighbors[i].element;
			frontTierAdapt::copyPointInto(nbplot,nb);
		}
		std::ostream & os = MatLabDebug::stream( );
		std::stringstream ss;
		ss << this->index[cX] << ',' << this->index[cY];
		os << trace << nbplot << matlabBridge::Text(this->coord[cX],this->coord[cY],ss.str( ).c_str( )) 
			<< matlabBridge::pause << matlabBridge::clearFigure;
	}
}

template<class REAL, int NUM_S>
void spatial::MeshElementSpecies<REAL,NUM_S>::updateBoundaryNeighbors(const VoronoiMesh<REAL,NUM_S> & vm, std::vector<OurType *>  & bn) {
	bool dbg = matches(26,14);
	if (bn.size( ) == 0) {
		throw std::logic_error("implement isolated point");
	}

	if (this->mPos( ) != boundarySurface) {
		throw std::domain_error("updateBoundaryNeighbors");
	}
	bool processNeighbors = false;
	switch (state( )) {
	case initial:
		processNeighbors = false;
		break;
	case awaitingNb:
		setState(legacyVoronoiSet);
		processNeighbors = true;
		break;
	case gainedAwaitingNb:
		setState(gainedEmpty);
		processNeighbors = true;
		break;
	case legacyUpdated:
		setState(legacyVoronoiSet);
		break;
	default:
		throw std::logic_error("invalid state updateBoundaryNeighbors");
	}
	if (!processNeighbors) {
		bool changed =  boundaryNeighbors.size( )  != bn.size( ) ;
		if (!changed) {
			//check new and previous set of boundary neighbors to see if changed
			for (int i = 0; !changed && i < bn.size( ) ;i++) {
				NeighborType & oldNb = boundaryNeighbors[i];
				changed =  oldNb.element != bn[i];
			}
		}
		processNeighbors = changed;
	}
	if (processNeighbors) {
		boundaryNeighbors.clear( );
		processBoundaryNeighbors(vm,bn);
	}
	if (neighbors == nullptr) {
		throw std::logic_error("N");
	}
}

template <class REAL, int NUM_S>
void MeshElementSpecies<REAL,NUM_S>::writeMatlab(std::ostream  &os ) const {
	if (!vol.empty( )) {
		if (!voronoiVolume.empty( )) {
			matlabBridge::Polygons pVoro("r",1);
			frontTierAdapt::copyVectorsInto(pVoro,voronoiVolume.points( ));
			os << pVoro; 
		}
		matlabBridge::Scatter scatter('g',30,true);
		frontTierAdapt::copyPointInto(scatter,*this);
		matlabBridge::Polygon pPoly("k",3);
		typename Volume<REAL,2>::VectorOfVectors vOfV = vol.points( );
		for (typename Volume<REAL,2>::VectorOfVectors::const_iterator vvIter = vOfV.begin( ); vvIter != vOfV.end( );++vvIter) {
			matlabBridge::Polygon pPoly("k",3);
			frontTierAdapt::copyVectorInto(pPoly,*vvIter);
			os << pPoly; 
		}
		std::stringstream ss;
		ss << this->index[cX] << ',' << this->index[cY];
		os << scatter << pPoly << matlabBridge::Text(this->coord[cX],this->coord[cY],ss.str( ).c_str( ));
	}
}

template <class REAL, int NUM_S>
spatial::Volume<REAL,2> spatial::MeshElementSpecies<REAL,NUM_S>::createInsidePolygon(const MeshDef<REAL,2> & mesh) {
	Volume<REAL,2> rval(1);
	const double width = mesh.interval(cX);
	const double height = mesh.interval(cY);

	REAL x = this->coord[cX] - width / 2;
	REAL y = this->coord[cY] - height  / 2;
	std::array<REAL,2> origin = { x, y};
	rval.add(origin);
	origin[cX] += width;
	rval.add(origin);
	origin[cY] += height;
	rval.add(origin);
	origin[cX] -= width;
	rval.add(origin);
	rval.close();

	return rval; 
}

template<class REAL, int NUM_S>
const spatial::Volume<REAL,2> & spatial::MeshElementSpecies<REAL,NUM_S>::getControlVolume(const MeshDef<REAL,2> & mesh) const {
	if (!vol.empty( )) {
		return vol;
	}
	OurType & us = const_cast<OurType &>(*this);
	typedef TPoint<REAL, 2> MeshPointType;
	switch (this->mp) {
	case deepInteriorSurface: 
	case interiorSurface:
		us.vol =  us.createInsidePolygon(mesh);
		break;
	case boundarySurface:
		if (isBoundaryElementWithInsideNeighbors( )) {
			us.vol = us.createInsidePolygon(mesh);
		}
		else {
			VCELL_EXCEPTION(logic_error,this->indexInfo( ) << " " << state( ) << " " << this->mPos( ) << "has no polygon")   ;
		}
		break;
	case outsideSurface: 
	case deepOutsideSurface: 
		break;
	default:
		throw std::logic_error("unknown case");
	}
	return vol;
}

namespace {
	template <class REAL, int NUM_S>
	struct VolRecord {
		VolRecord( )
			:neighbor(nullptr),
			vol(0) {}
		spatial::MeshElementSpecies<REAL,NUM_S> *neighbor;
		REAL vol;
	};
}

template<class REAL, int NUM_S>
REAL spatial::MeshElementSpecies<REAL,NUM_S>::voronoiOverlap(const Volume<REAL,2> &oldVolume) {
	Volume<REAL,2> intersection = voronoiVolume.intersection(oldVolume);
	return intersection.volume( ); 
}

template<class REAL, int NUM_S>
void spatial::MeshElementSpecies<REAL,NUM_S>::distributeMassToNeighbors(const MeshDef<REAL,2> & meshDef) {
	if (state( ) != lost) {
		return;
	}
	const Volume<REAL,2> & ourVolume = getControlVolume(meshDef);
	VCELL_LOG(debug,this->indexInfo( ) << " dMtoN mass " << amtMassTransient[0] << " vol " << ourVolume.volume( ));

	const size_t nN = numNeighbors( );
	const REAL volumeValue = volume( );
	if (volumeValue != ourVolume.volume( )) {
		throw std::logic_error("volume !=");
	}
	bool distributed = false;
	for (int i = 0; i < nN; i++) {
		assert(neighbors[i].element != nullptr);
		OurType & nb = *neighbors[i].element;
		if (nb.state( ) == stableUpdated) {
			debugAid++;
			Volume<REAL,2> intersection = ourVolume.intersection(nb.getControlVolume(meshDef));
			REAL iVol = intersection.volume( );
			assert(iVol>=0);
			if (iVol > 0) {
				distributed = true;
				REAL nbVol = nb.volume( );
				for (size_t s = 0; s < NUM_S; s++) {
					REAL m = iVol * amtMassTransient[s] / volumeValue; 
					VCELL_COND_LOG(debug, s == 0 , " " << this->indexInfo( ) << " giving " <<nb.indexInfo( ) << " mass " << m 
						<< " has vol " << nbVol << " existing mass " << nb.amtMassTransient[0] );
					nb.amtMassTransient[s] += m; 
					VCELL_COND_LOG(debug, s == 0 , "  " << nb.indexInfo( ) << " new mass " << nb.amtMassTransient[0] );
				}
			}
		}
	}
	// mass is gone
	for (size_t s = 0; s < NUM_S; s++) {
		amtMass[s] = 0;
	}
	setState(stable);

	if (!distributed) {
		VCELL_EXCEPTION(logic_error, this->indexInfo( ) << " with " << nN << " neighbors has none to distribute to") ;
	}
}

template<class REAL, int NUM_S>
void spatial::MeshElementSpecies<REAL,NUM_S>::collectMassFromNeighbors(const MeshDef<REAL,2> & meshDef, const std::vector<TPoint<REAL,2> > & front) {
	if (state( ) != gainedEmpty) {
		throw std::domain_error("collectMassFromNeighbors");
	}
	if (amtMass[0] != 0) {
		VCELL_EXCEPTION(domain_error,"non zero new mass");
	}
	VCELL_LOG(debug,this->indexInfo( ) << "collecting Mass");
	formBoundaryPolygon(meshDef,front);
	const Volume<REAL,2> & ourVolume = getControlVolume(meshDef);
	VCELL_LOG(debug,this->indexInfo( ) << " collecting with volume " << ourVolume.volume( ));
	const size_t nN = numNeighbors( );
	REAL totalVolume = 0;
	for (int i = 0; i < nN; i++) {
		assert(neighbors[i].element != nullptr);
		OurType & nb = *neighbors[i].element;
		VCELL_COND_LOG(debug, nb.state( ) == legacyVoronoiSetCollected, nb.indexInfo ( ) << " collected again ") ;
		if (nb.state( ) == legacyVoronoiSet || nb.state( ) == legacyInteriorSet ) {
			//here we copy the transient mass to the base -- at this point in the cycle we don't need the original any more
			//this allows us to use original as basis for pre-collection concentration 
			std::copy(nb.amtMassTransient.begin ( ), nb.amtMassTransient.end( ), nb.amtMass.begin( ));
			nb.setCollected( );
		}

		const MeshElementStateful::State nbState = nb.state( );
		if (nbState == legacyVoronoiSetCollected || nbState == legacyInteriorSetCollected) {
			Volume<REAL,2> intersection = ourVolume.intersection(nb.getControlVolume(meshDef));
			if (nbState == legacyInteriorSet) {
				std::cout << "lis" << std::endl;
			}
			REAL intersectVolume = intersection.volume( );
			VCELL_LOG(debug, this->indexInfo( ) << " overlap volume with " <<nb.indexInfo( ) << " is " << intersectVolume);
			assert(intersectVolume>=0);
			if (intersectVolume > 0) {
				REAL donorVolume = nb.volume( );
				for (size_t s = 0; s < NUM_S; s++) {
					const REAL mu = nb.amtMass[s] / donorVolume; 
					VCELL_LOG(verbose,this->indexInfo( ) << ':' << nb.indexInfo( ) << " volume " << nb.volume( ) << " conc " << mu); 
					REAL m =  mu * intersectVolume; 
					amtMass[s] += m;
					nb.amtMassTransient[s] -= m;
					VCELL_COND_LOG(debug, s == 0, this->indexInfo( ) << " adding mass " <<  m  << " from " <<nb.indexInfo( ) << ' ' << nb.state( ));
				}
			}
		}
		else {
			VCELL_LOG(warn,"Not collecting from " << nb.ident( ));
		}
		//testing show some overlap with other neighbor states, but it was ~ 1 part in 10^6 of volume
	}
	VCELL_LOG(debug, this->indexInfo( ) << " final mass is " << amtMass[0] << " vol " << ourVolume.volume( )  << " conc " << concentration(0)); 
	setState(gained); 
}

template<class REAL, int NUM_S>
void spatial::MeshElementSpecies<REAL,NUM_S>::endOfCycle( ) {
	switch (state( )) {
	case stable:
		if (this->isInside( )) {
			VCELL_EXCEPTION(domain_error,this->indexInfo( ) << ' ' << state( ) << ' ' << this->mPos( ) << " end of cycle");
		}
		{
			bool deep = true;
			for (int i = 0; i < numNeighbors( ); i++) {
				if (neighbors[i].element != nullptr) {
					OurType & nb = *neighbors[i].element;
					switch (nb.mPos( )) {
					case spatial::boundarySurface:
						deep = false;
						break;
					case spatial::deepInteriorSurface: 
					case spatial::interiorSurface:
						VCELL_EXCEPTION(logic_error,ident( ) << " has neighbor " << nb.ident( )); 
						break;
					case spatial::outsideSurface:
					case spatial::deepOutsideSurface:
						break;
					}
				}
				else {
					break; //break the for loop
				}
			}
			if (deep) {
				base::setPos(spatial::deepOutsideSurface);
			}
			else {
				base::setPos(spatial::outsideSurface);
			}
		}

		break;
	case stableUpdated:
		VCELL_LOG(verbose,this->ident( ) << " eoc prior copy " << amtMassTransient[0] << " mass " << amtMass[0]);
		std::copy(amtMassTransient.begin( ), amtMassTransient.end( ), amtMass.begin( ));
		/* REVIEW -- check for negative
		for (int s = 0; s < NUM_S; s++) {
		if (amtMassTransient[s] < 0) {
		VCELL_EXCEPTION(logic_error, ident() << " has gone negative") ;
		}
		amtMass[s] = amtMassTransient[s];
		}
		*/
		setState(stable);
		switch (this->mPos( )) {
		case spatial::deepInteriorSurface: 
		case spatial::interiorSurface:
			{
				bool deep = true; //assume true until find boundary neighbor
				for (int i = 0; deep && i < numNeighbors( ); i++) {
					assert(neighbors[i].element != nullptr);
					OurType & nb = *neighbors[i].element;
					switch (nb.mPos( )) {
					case spatial::boundarySurface:
						deep = false;
					case spatial::deepInteriorSurface: 
					case spatial::interiorSurface:
						break;
					case spatial::deepOutsideSurface:
					case spatial::outsideSurface:
						VCELL_EXCEPTION(logic_error,ident( ) << " has neighbor " << nb.ident( )); 
						break;
					}
				}
				if (deep) {
					base::setPos(spatial::deepInteriorSurface);
				}
				else {
					base::setPos(spatial::interiorSurface);
				}
			}
			break;
		case spatial::outsideSurface:
		case spatial::deepOutsideSurface:
			VCELL_EXCEPTION(logic_error,ident( ) << " at eoc");
		} //sub-switch
		break;
	case gained:
		setState(stable);
		break;
	default:
		throw std::logic_error("illegal endOfCycle state");
	}
	//temporary? store concentration
	REAL vol = this->volume( );
	if (vol > 0) {
		for (int i = 0; i < NUM_S; i++) {
			concValue[i] = amtMass[i] / vol;
		}
	}
	else {
		for (int i = 0; i < NUM_S; i++) {
			concValue[i] = 0; 
		}
	}
	VCELL_LOG(verbose,this->ident( ) << " eoc end mass " << this->mass(0) << " vol " << volume(  ) << " conc " << concentration(0));
}

template<class REAL, int NUM_S>
void spatial::MeshElementSpecies<REAL,NUM_S>
	::diffuseAdvect(DiffuseAdvectCache & daCache, REAL diffusionConstant, REAL timeStep, bool & negativeMassError) {
		DaCache<REAL,NUM_S> & ourCache = static_cast<DaCache<REAL,NUM_S> &>(daCache);
		switch (state( )) {
		case stable:
		case stableUpdated:
			if (this->mPos( ) != interiorSurface && this->mPos( ) != deepInteriorSurface) {
				throw std::domain_error("diffuseAdvect");
			}
			setState(stableUpdated);
			break;
		case legacyVolume:
		case legacyUpdated:
			assert(this->mPos( ) == boundarySurface);
			setState(legacyUpdated); 
			break;
		default:
			throw std::domain_error("diffuseAvect");
		}

		if (this->isInside( )) {
			typename DaCache<REAL,NUM_S>::Map & diffuseAdvectMap = ourCache.diffuseAdvectMap; 
			std::copy(amtMass.begin( ), amtMass.end( ), amtMassTransient.begin( ));

			//turn off to dcheck
			//return;

			for (int i = 0 ; i < numNeighbors( ); i++) {
				if (neighbors[i].element == nullptr) {
					VCELL_EXCEPTION(logic_error,ident( ) << " neighbor " << i << " is null");
				}
				const OurType & nb = *neighbors[i].element;

				const REAL edgeLengthTo = neighbors[i].edgeLength; 
				if (edgeLengthTo == 0) { //if no edge, don't bother
					continue;
				}
				//find average edgelength, but check to make sure not too different
				const NeighborType * usToThem = nb.findUs(*this);
				if (usToThem == nullptr) {
					VCELL_EXCEPTION(logic_error, this->indexInfo( ) << " neighbor " << nb.indexInfo( ) << " has no record of 'this'"); 
				}
				const REAL edgeLengthFrom = usToThem->edgeLength;
				if ( this->mPos( ) == nb.mPos( ) && !spatial::differenceLessThan(edgeLengthTo,edgeLengthFrom,ourCache.edgeLengthTolerance) ) {
					std::ostringstream oss;
					oss << this->ident( ) << " to " << nb.ident( ) << " length " << edgeLengthTo 
						<< " different from reverse length " << edgeLengthFrom << " difference " 
						<< std::abs(edgeLengthTo - edgeLengthFrom) << " greater than tolerance "
						<< ourCache.edgeLengthTolerance; 
					throw  ReverseLengthException<REAL,NUM_S>(oss.str( ),*this,nb); 
				}
				const REAL edgeLength = (edgeLengthTo + edgeLengthFrom) * 0.5;

				SVector<REAL,2> averageVelocity = (getVelocity( ) +  nb.getVelocity( )) / 2; 
				NormVector<REAL,2> normalVector(nb,*this); 
				REAL advectCoeff = dot(averageVelocity,normalVector);
				for (size_t s = 0; s < NUM_S;s++) {
					//REAL cUs = concentration(s); EVAL using concentration
					//REAL cOther = nb.concentration(s);
					REAL cUs = concValue[s]; 
					REAL cOther = nb.concValue[s];
					REAL diffusionTerm = diffusionConstant * (cOther - cUs) / neighbors[i].distanceTo;
					REAL averageConcentration = (cUs + cOther) / 2;
					REAL advectTerm = advectCoeff * averageConcentration;
					REAL sum = (diffusionTerm + advectTerm); 
					//REAL transferAmount = (diffusionTerm + advectTerm) * edgeLength * timeStep;
					REAL transferAmount = sum * edgeLength * timeStep;
					amtMassTransient[s] += transferAmount; 
					VCELL_COND_LOG(debug, s == 0, this->indexInfo( ) << " da " << transferAmount << " from " << nb.indexInfo( ) 
						<< " u(i) = " << cUs << " u(j) " << cOther << " distance " << neighbors[i].distanceTo 
						<< " D = " << diffusionConstant << " u(avg) = " << averageConcentration << " advectCoeff " << advectCoeff 
						<< " advectTerm = " << advectTerm << " edge length " << edgeLength 
						<< " timeStep " << timeStep
						<< " m(i) " << amtMass[0] << " vol(i) = " << volume( ) << " m(j) " << nb.amtMass[0] << " vol(j) " << nb.volume( ) 
						); 
					std::pair<const OurType *,const OurType *> inv(&nb,this);
					if (diffuseAdvectMap.find(inv) != diffuseAdvectMap.end( )) {
						if (diffuseAdvectMap[inv].reversed) {
							VCELL_EXCEPTION(logic_error, this->indexInfo( ) << " to " << nb.indexInfo( ) << " already transferred or reversed?") ;
						}
						REAL prevAmount = diffuseAdvectMap[inv].value; 
						if ( !spatial::nearlyEqual(-1 * prevAmount,transferAmount,toleranceDiffuseAdvectException) ) {
							VCELL_EXCEPTION(logic_error, this->indexInfo( ) << " to " << nb.indexInfo( ) << " amt " << transferAmount
								<< " differs from inverse " << prevAmount << " relative diff " 
								<< spatial::relativeDifference(-1 *prevAmount,transferAmount));
							/*
							VCELL_LOG(warn, "WARNING " << indexInfo( ) << " to " << nb.indexInfo( ) << " amt " << transferAmount
							<< " differs from inverse " << prevAmount << " relative diff " 
							<< spatial::relativeDifference(-1 *prevAmount,transferAmount))
							*/
						}

						diffuseAdvectMap[inv].reversed = true;
					}
					else { //haven't done other way yet, store amount
						std::pair<const OurType *,const OurType *> pr(this,&nb);
						diffuseAdvectMap[pr] = CheckValue(false,transferAmount);
					}
				} //species loop
			} //neighbor loop
			for (size_t s = 0; s < NUM_S;s++) {
				if (amtMassTransient[s] < 0) {
					negativeMassError = true;
					return;
				}
			}
		} //isInside
}
std::ostream & spatial::operator<<(std::ostream &os ,spatial::MeshElementStateful::State state) {
#define CASE(x) case x: os << #x; break;
	switch (state) {
		CASE(initial);
		CASE(stable );
		CASE(stableUpdated);
		CASE(lost );
		CASE(awaitingNb);
		CASE(gainedAwaitingNb);
		CASE(gainedEmpty);
		CASE(gained );
		CASE(legacyVolume);
		CASE(legacyUpdated);
		CASE(legacyVoronoiSet);
		CASE(legacyVoronoiSetCollected);
		CASE(legacyInteriorSet);
		CASE(legacyInteriorSetCollected);
		CASE(transient);
	}
#undef CASE
	return os;
}

MatLabDebug MatLabDebug::instance;
template struct spatial::MeshElementSpecies<double,1>; 
