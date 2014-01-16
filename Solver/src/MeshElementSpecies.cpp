#include <MPoint.h>
#include <sstream>
#include <exception>
#include <algorithm>
#include <MeshElementSpecies.h>
#include <VCellFront.h>
#include <VoronoiResult.h>
#include <VoronoiMesh.h>
#include <Mesh.h>
#include <algo.h>
#include <intersection.h>
#include <MovingBoundaryParabolicProblem.h>
#include <SegmentIterator.h>
#include <LoopIterator.h>
#include <create.h>
#include <stack_container.h>
#include <vcellutil.h>
#include <Logger.h>
#include <Distance.h>

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
	const double toleranceEdgeLengthException = 1e-8;
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
	struct MesDistancePolicy {
		typedef long long DistanceSquaredType;
		typedef long DistanceType;
		static void check(long) {}
		template <typename U>
		static DistanceType convert(U u) {
			return u;
		}
	};
	static_assert(std::is_same<moving_boundary::CoordinateType,MesDistancePolicy::DistanceType>::value, "typedef mismatch");
	typedef spatial::Distance<moving_boundary::CoordinateType,MesDistancePolicy> MesDistance;
}

using spatial::cX;
using spatial::cY;
using spatial::TPoint;
using spatial::EdgeFindResult;
using spatial::EdgeStateful;
using namespace moving_boundary::MeshElementStateful;
using matlabBridge::MatLabDebug;
using  moving_boundary::MeshElementSpecies;
using  moving_boundary::VoronoiMesh;
using  moving_boundary::FrontType;

using spatial::SurfacePosition;
using spatial::deepInteriorSurface;
using spatial::interiorSurface;
using spatial::boundarySurface;
using spatial::outsideSurface;
using spatial::deepOutsideSurface;
using spatial::unset;

using spatial::MeshDef;


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


	struct DaCache : public spatial::DiffuseAdvectCache { 
		typedef std::pair<const MeshElementSpecies * ,const MeshElementSpecies *> MesPair;
		typedef std::map<MesPair,CheckValue> Map;
		DaCache(moving_boundary::CoordinateType minimumMeshInterval) 
			//:edgeLengthTolerance(static_cast<moving_boundary::CoordinateType>(minimumMeshInterval * toleranceEdgeLengthException) ) {
			:edgeLengthTolerance(1) {
		}

		virtual void start( ) {
			diffuseAdvectMap.clear( );
		}

		virtual void finish( ) {
			std::map<MesPair,CheckValue>::const_iterator iter = diffuseAdvectMap.begin( ); 
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
		const moving_boundary::CoordinateType edgeLengthTolerance; 
	};
}

spatial::DiffuseAdvectCache * MeshElementSpecies::createCache(moving_boundary::CoordinateType minMeshInterval) {
	return new DaCache(minMeshInterval);
}

void MeshElementSpecies::setPos(SurfacePosition m)  {
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

struct MeshElementSpecies::SetupBoundaryNeighbor : public std::unary_function<OurType *,NeighborType> {
	OurType &clientElement;
	SetupBoundaryNeighbor(OurType &client)
		:clientElement(client) {}

	NeighborType operator( )(OurType *nb) {
		NeighborType rval;
		rval.element = nb;

		const moving_boundary::DistanceType existing = nb->distanceToNeighbor(clientElement);
		if (existing > 0) {
			rval.distanceTo = existing; 
			//assert(spatial::nearlyEqual(existing,MesDistance::approximate(*nb,clientElement), toleranceNeighborDistancesAssert) );
			//
			moving_boundary::DistanceType  da = MesDistance::approximate(*nb,clientElement);
			if (!spatial::nearlyEqual(existing,da,toleranceNeighborDistancesAssert) ) {
				std::cout << "oops " << da << std::endl;
			}
			//
		}
		else {
			rval.distanceTo = MesDistance::approximate(*nb,clientElement);
		}

		return rval;
	};
};

void MeshElementSpecies::processBoundaryNeighbors(const VoronoiMesh & vm, std::vector<OurType *>  & bn) {
	VCELL_LOG(trace,this->indexInfo( ) << " processBoundaryNeighors " << state( ) << ' ' << this->mPos( ));
	//getting vornoi volume and setting up boundary neighbors in same function due to legacy reasons ...
	{ //clarity scope, building voronoiVolume
		spatial::VoronoiResult vResult;
		std::vector<spatial::VoronoiGhostPoint> &voronoiVertices = vResult.vertices;
		assert(voronoiVertices.empty( ));
		vm.getResult(vResult,*this);
		{
			std::ofstream vp("voronoiPoly.m");
			vp << "% " << vResult.type << std::endl;
			matlabBridge::Polygon p("r-+");
			frontTierAdapt::copyVectorInto(p,vResult.vertices);
			vp << p;
		}

		voronoiVolume.clear( );
		Volume2DClass::fillingIteratorType fIter = voronoiVolume.fillingIterator(voronoiVertices.size());
		//std::transform(voronoiVertices.begin( ),voronoiVertices.end( ),fIter, coordinateToVolume);
		std::copy(voronoiVertices.begin( ),voronoiVertices.end( ),fIter);

		//check for single open line
		if (vResult.type == spatial::VoronoiResult::straightLine) { 
			assert(voronoiVertices.size( ) == 3);
			spatial::SVector<moving_boundary::CoordinateType,2> delta(*this,voronoiVertices[1]);
			delta *= -3;
			//go other direction to build box
			//delta.reverse( );
			FrontPointType add1 = spatial::displacement(voronoiVertices[2],delta);
			FrontPointType add2 = spatial::displacement(voronoiVertices[0],delta);
			voronoiVolume.add(add1);
			voronoiVolume.add(add2);
		}
		voronoiVolume.close( );
	}
	boundaryNeighbors.resize(bn.size( ));
	std::transform(bn.begin( ),bn.end( ),boundaryNeighbors.begin( ), SetupBoundaryNeighbor(*this));

	neighbors = boundaryNeighbors.data( );
}

void MeshElementSpecies::moveFront( const MeshDef<moving_boundary::CoordinateType,2> & mesh, const FrontType & front) {
	using namespace MeshElementStateful;
	if (this->isInside( )) {
		if (state( ) != stable) {
			throw std::domain_error("moveFront not stable");
		}
		const moving_boundary::CoordinateProductType oldVolume = vol.volume( );
		formBoundaryPolygon(mesh,front);
		setState(legacyVolume);
	}
}


//this should be combined with moveFront, above
void MeshElementSpecies::applyFrontLegacyVoronoiSet( const MeshDef<moving_boundary::CoordinateType,2> & mesh, const FrontType & front) {
	assert(!voronoiVolume.empty( ));
	assert(state( ) == legacyVoronoiSet || state( ) == legacyVoronoiSetCollected); 
	formBoundaryPolygon(mesh,front);
	setState(stableUpdated);
}

void MeshElementSpecies::formBoundaryPolygon( const MeshDef<moving_boundary::CoordinateType,2> & mesh, const FrontType & front) {
	using spatial::Edge;
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
	if (MatLabDebug::on("edgefind")) { 
		matlabBridge::Polygons pPolys("k",3);
		frontTierAdapt::copyVectorsInto(pPolys,vol.points( ));
		matlabBridge::Polygons vs("-r",1);
		frontTierAdapt::copyVectorsInto(vs,voronoiVolume.points( ));
		MatLabDebug::stream() << pPolys << vs << matlabBridge::pause << matlabBridge::clearFigure;
	}
	std::vector<Edge<moving_boundary::CoordinateType,2> > neighborEdges(boundaryNeighbors.size( ));
	for (size_t i = 0; i < boundaryNeighbors.size( ); i++) {
		//assert(boundaryNeighbors[i].edgeLength == 0);
		boundaryNeighbors[i].edgeLength = 0;
		neighborEdges[i] = Edge<moving_boundary::CoordinateType,2>(*this,*boundaryNeighbors[i].element);
	}

	/**
	* search for segments whose end points are equidistant between us and neighbor
	*/
	matlabBridge::Polygon trace(":+g");
	matlabBridge::Polygon pedge("-+r",2);

	//REAL minimumSegSquared = mesh.minimumInterval( ) * toleranceMinimumSegmentSquared;
	typedef TPoint<moving_boundary::CoordinateType,2> MeshPointType;
	spatial::EdgeAccessor<moving_boundary::CoordinateType,moving_boundary::CoordinateProductType,2> accsr = vol.accessor( );
	for (;accsr.hasNext( );accsr.next( )) {
		debugAid++;
		Edge<moving_boundary::CoordinateType,2> edge = accsr.get( );
		/*
		if (edge.edgeVector( ).magnitudeSquared( ) < minimumSegSquared) {
		VCELL_LOG(debug,indexInfo( ) << " skipping short segment " << edge.origin( ) << " to " << edge.tail( )
		<< ' ' << std::setprecision(12) << edge.edgeVector( ).magnitude( ));
		continue;
		}
		*/
		typedef MesDistance::DistanceSquaredType SquaredType; 
		const SquaredType hDistSquare = MesDistance::squared(*this,edge.origin( ));
		const MeshPointType & tail = edge.tail( );
		const SquaredType tDistSquare = MesDistance::squared(*this,tail);

		using spatial::smallRelativeDifference;
		const SquaredType meshMin = mesh.minimumInterval( ); 
		for (size_t i = 0; i < boundaryNeighbors.size( ); i++) {
			const OurType & nb = *boundaryNeighbors[i].element;
			debugAid++;
			const SquaredType nbHDistSquare = MesDistance::squared(nb,edge.origin( ));
			if (smallRelativeDifference<SquaredType>(hDistSquare - nbHDistSquare,meshMin,tolerancePerpendicularSegmentDistanceSquared)) {
				const SquaredType nbTDistSquare = MesDistance::squared(nb,tail);
				if (smallRelativeDifference<SquaredType>(tDistSquare - nbTDistSquare, meshMin,tolerancePerpendicularSegmentDistanceSquared)) {
					const double mag = edge.edgeVector( ).magnitude( );
					const moving_boundary::DistanceType length = static_cast<moving_boundary::DistanceType>(mag);
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
					VCELL_KEY_LOG(verbose,"edgeTrace",this->ident( ) << " and " << nb.ident( ) << " edge " << edge
						<< " tail rejected, this is " << tDistSquare  
						<< " and neighbor is " << nbTDistSquare);
				}
			}
			else {
				VCELL_KEY_LOG(verbose,"edgeTrace",this->ident( ) << " and " << nb.ident( ) << " edge " <<  edge
					<< " head rejected, this is " << hDistSquare  
					<< " and neighbor is " << nbHDistSquare 
					<< " diff " << (hDistSquare - nbHDistSquare) );
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
void MeshElementSpecies::formBoundaryPolygonVM( const VoronoiMesh & vm, const FrontType & front) {
	formBoundaryPolygon(vm.mesh( ), front);
}

void MeshElementSpecies::updateBoundaryNeighbors(const VoronoiMesh & vm, std::vector<OurType *>  & bn) {
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

void MeshElementSpecies::writeMatlab(std::ostream  &os , bool noPoly, int precision) const {
	if (!vol.empty( )) {
		if (!voronoiVolume.empty( )) {
			matlabBridge::Polygons pVoro("r",1);
			if (precision != 0) {
				pVoro.setPrecision(precision);
			}
			frontTierAdapt::copyVectorsInto(pVoro,voronoiVolume.points( ));
			os << pVoro; 
		}
		matlabBridge::Scatter scatter('g',30,true);
		frontTierAdapt::copyPointInto(scatter,*this);
		std::stringstream ss;
		ss << this->index[cX] << ',' << this->index[cY];
		os << scatter << matlabBridge::Text(this->coord[cX],this->coord[cY],ss.str( ).c_str( ));
		if (!noPoly) {
			matlabBridge::Polygon pPoly("k",3);
			Volume2DClass::VectorOfVectors vOfV = vol.points( );
			for (Volume2DClass::VectorOfVectors::const_iterator vvIter = vOfV.begin( ); vvIter != vOfV.end( );++vvIter) {
				matlabBridge::Polygon pPoly("k",3);
				if (precision != 0) {
					pPoly.setPrecision(precision);
				}
				frontTierAdapt::copyVectorInto(pPoly,*vvIter);
				os << pPoly; 
			}
			os << pPoly; 
		}
	}
}

moving_boundary::Volume2DClass MeshElementSpecies::createInsidePolygon(const MeshDef<moving_boundary::CoordinateType,2> & mesh) {
	Volume2DClass rval(1);
	const moving_boundary::CoordinateType width = mesh.interval(cX);
	const moving_boundary::CoordinateType height = mesh.interval(cY);

	moving_boundary::CoordinateType x = this->coord[cX] - width / 2;
	moving_boundary::CoordinateType y = this->coord[cY] - height  / 2;
	std::array<moving_boundary::CoordinateType,2> origin = { x, y};
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

const moving_boundary::Volume2DClass & MeshElementSpecies::getControlVolume(const MeshDef<moving_boundary::CoordinateType,2> & mesh) const {
	if (!vol.empty( )) {
		return vol;
	}
	OurType & us = const_cast<OurType &>(*this);
	typedef TPoint<moving_boundary::CoordinateType, 2> MeshPointType;
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

/*
namespace {
template <class COORD_TYPE, class REAL>
struct VolRecord {
VolRecord( )
:neighbor(nullptr),
vol(0) {}
MeshElementSpecies *neighbor;
REAL vol;
};
}
*/

inline moving_boundary::CoordinateProductType MeshElementSpecies::voronoiOverlap(const moving_boundary::Volume2DClass &oldVolume) {
	Volume2DClass intersection = voronoiVolume.intersection(oldVolume);
	return intersection.volume( ); 
}

void MeshElementSpecies::distributeMassToNeighbors(const MeshDef<moving_boundary::CoordinateType,2> & meshDef) {
	if (state( ) != lost) {
		return;
	}
	const Volume2DClass & ourVolume = getControlVolume(meshDef);
	VCELL_LOG(debug,this->indexInfo( ) << " dMtoN mass " << amtMassTransient[0] << " vol " << volumePD( ));

	const size_t nN = numNeighbors( );
	const moving_boundary::CoordinateProductType volumeValue = volumePD( );
	assert(volumeValue == ourVolume.volume( ) / distanceScaledSquared);
	bool distributed = false;
	moving_boundary::BioQuanType massSentinel = amtMassTransient[0];
	for (int i = 0; i < nN; i++) {
		assert(neighbors[i].element != nullptr);
		OurType & nb = *neighbors[i].element;
		if (nb.state( ) == stableUpdated) {
			debugAid++;
			Volume2DClass intersection = ourVolume.intersection(nb.getControlVolume(meshDef));
			moving_boundary::CoordinateProductType iVol = intersection.volume( ) / distanceScaledSquared;
			assert(iVol>=0);
			if (iVol > 0) {
				distributed = true;
				moving_boundary::CoordinateProductType nbVol = nb.volumePD( );
				for (size_t s = 0; s < nOfS; s++) {
					moving_boundary::BioQuanType m = iVol * amtMassTransient[s] / volumeValue; 
					VCELL_COND_LOG(debug, s == 0 , " " << this->indexInfo( ) << " giving " <<nb.indexInfo( ) << " mass " << m 
						<< " has vol " << nbVol << " existing mass " << nb.amtMassTransient[0] );
					nb.amtMassTransient[s] += m; 
					massSentinel -= m;
					VCELL_COND_LOG(debug, s == 0 , "  " << nb.indexInfo( ) << " new mass " << nb.amtMassTransient[0] );
				}
			}
		}
	}
	// mass is gone
	if (massSentinel > 0.001) {
		std::ofstream script("massLost.m");
		script << matlabBridge::FigureName("lost mass");
		writeMatlab(script);
		for (int i = 0; i < nN; i++) {
			script << matlabBridge::pause << matlabBridge::clearFigure;
			neighbors[i].element->writeMatlab(script);
		}
	}
	for (size_t s = 0; s < nOfS; s++) {
		amtMass[s] = 0;
	}
	setState(stable);

	if (!distributed) {
		VCELL_EXCEPTION(logic_error, this->indexInfo( ) << " with " << nN << " neighbors has none to distribute to") ;
	}
}

void MeshElementSpecies::collectMassFromNeighbors(const MeshDef<moving_boundary::CoordinateType,2> & meshDef, const FrontType & front) {
	if (state( ) != gainedEmpty) {
		throw std::domain_error("collectMassFromNeighbors");
	}
	if (amtMass[0] != 0) {
		VCELL_EXCEPTION(domain_error,"non zero new mass");
	}
	VCELL_LOG(debug,this->indexInfo( ) << "collecting Mass");
	formBoundaryPolygon(meshDef,front);
	const Volume2DClass & ourVolume = getControlVolume(meshDef);
	VCELL_LOG(debug,this->indexInfo( ) << " collecting with volume " << ourVolume.volume( ));
	const size_t nN = numNeighbors( );
	moving_boundary::CoordinateProductType totalVolume = 0;
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
			Volume2DClass intersection = ourVolume.intersection(nb.getControlVolume(meshDef));
			CoordinateProductType intersectVolume = intersection.volume( );
			intersectVolume /= distanceScaledSquared;  //World -> pd conversion
			VCELL_LOG(debug, this->indexInfo( ) << " overlap volume with " <<nb.indexInfo( ) << " is " << intersectVolume);
			assert(intersectVolume>=0);
			if (intersectVolume > 0) {
				moving_boundary::CoordinateProductType donorVolume = nb.volumePD( );
				for (size_t s = 0; s < nOfS; s++) {
					const moving_boundary::BioQuanType mu = nb.amtMass[s] / donorVolume; 
					VCELL_LOG(verbose,this->indexInfo( ) << ':' << nb.indexInfo( ) << " volume " << nb.volumePD( ) << " conc " << mu); 
					moving_boundary::BioQuanType m =  mu * intersectVolume; 
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

void MeshElementSpecies::endOfCycle( ) {
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
		for (int s = 0; s < nOfS; s++) {
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
	moving_boundary::CoordinateProductType vol = this->volumePD( );
	if (vol > 0) {
		for (int i = 0; i < nOfS; i++) {
			concValue[i] = amtMass[i] / vol;
		}
	}
	else {
		for (int i = 0; i < nOfS; i++) {
			concValue[i] = 0; 
		}
	}
	VCELL_LOG(verbose,this->ident( ) << " eoc end mass " << this->mass(0) << " vol " << volumePD(  ) << " conc " << concentration(0));
}

using moving_boundary::BioQuanType;
using moving_boundary::TimeType;
void MeshElementSpecies::diffuseAdvect(spatial::DiffuseAdvectCache & daCache, BioQuanType diffusionConstant, TimeType timeStep, bool & negativeMassError) {
	DaCache & ourCache = static_cast<DaCache &>(daCache);
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
		DaCache::Map & diffuseAdvectMap = ourCache.diffuseAdvectMap; 
		std::copy(amtMass.begin( ), amtMass.end( ), amtMassTransient.begin( ));

		//turn off to dcheck
		//return;

		for (int i = 0 ; i < numNeighbors( ); i++) {
			if (neighbors[i].element == nullptr) {
				VCELL_EXCEPTION(logic_error,ident( ) << " neighbor " << i << " is null");
			}
			const OurType & nb = *neighbors[i].element;

			const moving_boundary::DistanceType edgeLengthTo = neighbors[i].edgeLength; 
			if (edgeLengthTo == 0) { //if no edge, don't bother
				continue;
			}
			//find average edgelength, but check to make sure not too different
			const NeighborType * usToThem = nb.findUs(*this);
			if (usToThem == nullptr) {
				VCELL_EXCEPTION(logic_error, this->indexInfo( ) << " neighbor " << nb.indexInfo( ) << " has no record of 'this'"); 
			}
			const moving_boundary::DistanceType edgeLengthFrom = usToThem->edgeLength;
			if ( this->mPos( ) == nb.mPos( ) && !spatial::differenceLessThan(edgeLengthTo,edgeLengthFrom,ourCache.edgeLengthTolerance) ) {
				std::ostringstream oss;
				oss << this->ident( ) << " to " << nb.ident( ) << " length " << edgeLengthTo 
					<< " different from reverse length " << edgeLengthFrom << " difference " 
					<< std::abs(edgeLengthTo - edgeLengthFrom) << " greater than tolerance "
					<< ourCache.edgeLengthTolerance; 
				throw  ReverseLengthException(oss.str( ),*this,nb); 
			}
			const moving_boundary::DistanceType edgeLength = (edgeLengthTo + edgeLengthFrom) /2;

			spatial::SVector<moving_boundary::VelocityType,2> averageVelocity = (getVelocity( ) +  nb.getVelocity( )) / 2; 
			spatial::NormVector<double,2> normalVector(spatial::Point2D(nb),spatial::Point2D(*this) ); 
			// this coefficient divided by problem domain to world scaling factor twice; one because velocity is scaled,
			// secondly because it's going to be multiplied by edgeLength (which is squared)
			BioQuanType advectCoeff = dot(averageVelocity,normalVector) /distanceScaledSquared;
			for (size_t s = 0; s < nOfS;s++) {
				//moving_boundary::BioQuanType cUs = concentration(s); EVAL using concentration
				//moving_boundary::BioQuanType cOther = nb.concentration(s);
				BioQuanType cUs = concValue[s]; 
				BioQuanType cOther = nb.concValue[s];
				BioQuanType diffusionTerm = diffusionConstant * (cOther - cUs) / neighbors[i].distanceTo;
				BioQuanType averageConcentration = (cUs + cOther) / 2;
				BioQuanType advectTerm = advectCoeff * averageConcentration;
				BioQuanType sum = (diffusionTerm + advectTerm); 
				//BioQuanType transferAmount = (diffusionTerm + advectTerm) * edgeLength * timeStep;
				BioQuanType transferAmount = sum * edgeLength * timeStep;
				amtMassTransient[s] += transferAmount; 
				VCELL_COND_LOG(debug, s == 0, this->indexInfo( ) << " da " << transferAmount << " from " << nb.indexInfo( ) 
					<< " u(i) = " << cUs << " u(j) " << cOther << " distance " << neighbors[i].distanceTo 
					<< " D = " << diffusionConstant << " u(avg) = " << averageConcentration << " advectCoeff " << advectCoeff 
					<< " advectTerm = " << advectTerm << " edge length " << edgeLength 
					<< " timeStep " << timeStep
					<< " m(i) " << amtMass[0] << " vol(i) = " << volumePD( ) << " m(j) " << nb.amtMass[0] << " vol(j) " << nb.volumePD( ) 
					); 
				std::pair<const OurType *,const OurType *> inv(&nb,this);
				if (diffuseAdvectMap.find(inv) != diffuseAdvectMap.end( )) {
					if (diffuseAdvectMap[inv].reversed) {
						VCELL_EXCEPTION(logic_error, this->indexInfo( ) << " to " << nb.indexInfo( ) << " already transferred or reversed?") ;
					}
					BioQuanType prevAmount = diffuseAdvectMap[inv].value; 
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
		for (size_t s = 0; s < nOfS;s++) {
			if (amtMassTransient[s] < 0) {
				VCELL_LOG(fatal,ident( ) << " negative mass, m(" << s << ") = " << amtMassTransient[s]);
				negativeMassError = true;
				return;
			}
		}
	} //isInside
}

void MeshElementSpecies::listBoundary(std::ostream & os,const MeshDef<moving_boundary::CoordinateType,2> & mesh) const {
	switch (mPos( )) {
	case spatial::outsideSurface:
	case spatial::deepOutsideSurface:
		return;
	case spatial::interiorSurface:
	case spatial::deepInteriorSurface:
	case spatial::boundarySurface:
		{
			const char comma = ',';
			const Volume2DClass & cVol = getControlVolume(mesh);
			typedef Volume2DClass::PointVector PointVector; 
			PointVector pv = cVol.points( ).front( ); 
			for (PointVector::const_iterator iter = pv.begin( ); iter != pv.end( ); ++iter) {
				os << index[0] << comma << index[1] << comma << mPos( ) << comma
					<< iter->get(cX) << comma << iter->get(cY) << std::endl;
			}
		}
		break;
	default:
		assert(false); //unknown state

	}
}

BioQuanType MeshElementSpecies::distanceScaledSquared = 1;

std::ostream & moving_boundary::operator<<(std::ostream &os ,moving_boundary::MeshElementStateful::State state) {
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
