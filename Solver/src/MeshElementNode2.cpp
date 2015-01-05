#include <MeshElementNode.h>
using namespace moving_boundary::MeshElementStateful;
using moving_boundary::MeshElementNode;
void MeshElementNode::setInitialPos(spatial::SurfacePosition pos) {
	if (state( ) != initial && state( ) != initialInside) {
		badState("setInitialPos"); 
	}
	switch (pos) {
	case spatial::interiorSurface:
		setState(initialInside);
		break;
	case spatial::boundarySurface:
		setState(initialBoundary);
		break;
	case spatial::outsideSurface:
		setState(outStable);
		break;
	}
}

bool MeshElementNode::isInside( ) const {
	switch (state( )) {
	case initialInside:
	case inStable:
	case inDiffAdvDone:
	case inDiffAdvDoneMU:
	case initialBoundary: 
	case bndFrontMoved:
	case bndNbrEdgesFound:
	case bndDiffAdvDone:
	case bndDiffAdvDoneMU:
	case bndFrontApplied:
	case bndStable:
	case transInBnd:
	case transOutBndSetBnd:
	case transOutBndNbrSet:
	case transOutBndMassCollected: 
	case transOutBndSetIn:
		return true;
	case outStable:
	case transBndOut: 
		return false;
		break;
	}
	badState("isInside");
	return false;
}
spatial::SurfacePosition MeshElementNode::mPos( ) const {
	switch (state( )) {
	case initialInside:
	case inStable:
	case inDiffAdvDone:
	case inDiffAdvDoneMU:
	case transOutBndSetIn: //this has to return true to get second call to setPos
		return spatial::interiorSurface;
	case initialBoundary: 
	case bndFrontMoved:
	case bndStable:
	case bndNbrEdgesFound:
	case bndDiffAdvDone:
	case bndDiffAdvDoneMU:
	case bndFrontApplied:
	case bndMassCollectedFrontApplied:
	case transInBnd:
	case transOutBndSetBnd:
	case transOutBndNbrSet:
	case transOutBndMassCollected: 
		return spatial::boundarySurface;
	case outStable:
	case transBndOut: 
		return spatial::outsideSurface;
	default:
		badState("mPos def");
	}
	return spatial::outsideSurface;
}

void MeshElementNode::setBoundaryOffsetValues( ) {
	if (!isBoundary( ) || boundaryOffset( ) != unsetOffsetValue()) {
		VCELL_EXCEPTION(domain_error, ident( ) << "setBoundaryOffsetValue with value " << boundaryOffset( ) );
	}
	bndOffset = 0;
	propagateBoundaryValue( );
}

void MeshElementNode::propagateBoundaryValue() {
	std::cout << ident( ) << " current " << static_cast<unsigned int>(boundaryOffset( )) << std::endl;
	assert(boundaryOffset( ) != unsetOffsetValue());

	//first pass, set unset neighbors and propagate call to neighbors at same offset
	size_t setIndex = 0;
	std::array<MeshElementNode *,8> nodesThatWereSet;
	const BoundaryOffsetType oneMore = boundaryOffset( ) + 1;

	for (int i = 0; i < NUM_INSIDE; i++) {
		if (interiorNeighbors[i].element != nullptr) {
			OurType & nb = *interiorNeighbors[i].element;
			if (nb.boundaryOffset( ) == unsetOffsetValue( ) || nb.boundaryOffset( ) > oneMore) {
				if (!nb.isBoundary( )) {
					nb.bndOffset = oneMore;
				}
				else {
					nb.bndOffset = 0;
				}
				nodesThatWereSet[setIndex++] = &nb;
			}
		}
	}

	//second pass, tell neighbors we just set to set their unset neighbors
	for (int i = 0; i < setIndex; i++) {
		nodesThatWereSet[i]->propagateBoundaryValue();
	}
}

void MeshElementNode::listBoundary(std::ostream & os) const {
	using spatial::cX;
	using spatial::cY;
	switch (mPos( )) {
	case spatial::outsideSurface:
		return;
	case spatial::interiorSurface:
	case spatial::boundarySurface:
		{
			const char comma = ',';
			const Volume2DClass & cVol = getControlVolume( );
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
void MeshElementNode::badState(const char * const function) const {
	std::cout <<  ident( ) << " in " << function << std::endl;
	VCELL_EXCEPTION(logic_error, ident( ) << " in " << function);
}

MeshElementNode::OurType *MeshElementNode::neighbor(spatial::ElementOffset<2> & eo) const {
	//nominally unsafe downcast
	const MeshType * m = static_cast<const MeshType *>(&mesh);
	return m->element(*this,eo);
}

namespace {
	struct NeighborBuilder {
		MeshElementNode & client;
		NeighborBuilder(MeshElementNode &c)
			:client(c) {}

		MeshElementNode::NeighborType generate(std::istream &in) const {
			return MeshElementNode::NeighborType(in,client);
		}
	};
}

/**
* see #persist
*/
MeshElementNode::MeshElementNode(const MeshDefinition &owner,std::istream &is)
	:base(is),
	mesh(owner),
	concValue( ),
	sourceTermValues(concValue),
	vol(0,this) //required to register this as VolumeMonitor of volume
{
	vcell_persist::Token::check<MeshElementNode>(is); 

	NeighborBuilder nb(*this);
	vcell_persist::readFunctor<NeighborBuilder> functor(is,nb);

	vcell_persist::binaryRead(is,stateVar);
	vcell_persist::binaryRead(is,interiorVolume);
	vol = Volume2DClass(is);
	vcell_persist::restore(is,amtMass);
	vcell_persist::restore(is,amtMassTransient);
	vcell_persist::restore(is,concValue);
	vcell_persist::restore(is,interiorNeighbors, vcell_persist::readFunctor<NeighborBuilder>(is,nb) );
	//gcw 10-20-2014 thinking vector of neighbors not used if element not boundary, so no point in storing it
	bool usingNeighborVector;
	vcell_persist::binaryRead(is, usingNeighborVector);
	if (usingNeighborVector) {
		vcell_persist::restore(is,boundaryNeighbors, vcell_persist::InsertFrom<NeighborBuilder>(nb) );
		neighbors = boundaryNeighbors.data( ); 
	}
	else {
		neighbors = interiorNeighbors.data( ); 
	}
	voronoiVolume = Volume2DClass(is);
	vcell_persist::binaryRead(is,nOutside);
	velocity = spatial::SVector<moving_boundary::VelocityType,2>(is);
}
namespace {
	struct NeighborWriter {
		MeshElementNode & client;
		NeighborWriter(MeshElementNode &c)
			:client(c) {}
		void operator( )(std::ostream &os, const MeshElementNode::NeighborType  & nb) const {
			nb.persist(os,client);
		}
	};
}

/**
* save essential state information necessary to restore from a file later
*/
void MeshElementNode::persist(std::ostream &os) {
	base::persist(os);

	NeighborWriter nw(*this);
	vcell_persist::writeFunctor<NeighborWriter> functor(os,nw);

	vcell_persist::Token::insert<MeshElementNode>(os); 
	vcell_persist::binaryWrite(os,stateVar);
	vcell_persist::binaryWrite(os,interiorVolume);
	vol.persist(os);
	//vcell_persist::save(os,segments_); //segments can be recreated on the fly
	vcell_persist::save(os,amtMass);
	vcell_persist::save(os,amtMassTransient);
	vcell_persist::save(os,concValue);
	vcell_persist::save(os,interiorNeighbors, functor);
	//vector of neighbors not used if element not boundary, so no point in storing it
	const bool usingNeighborVector = (neighbors == boundaryNeighbors.data( )); 
	vcell_persist::binaryWrite(os, usingNeighborVector);
	if (usingNeighborVector) {
		vcell_persist::save(os,boundaryNeighbors, functor); 
	}
	voronoiVolume.persist(os);
	vcell_persist::binaryWrite(os,nOutside);
	velocity.persist(os);
}

std::ostream & moving_boundary::operator<<(std::ostream &os ,moving_boundary::MeshElementStateful::State state) {
#define CASE(x) case x: os << #x; break;
	switch (state) {
		CASE(initial);
		CASE(initialInside);
		CASE(initialBoundary);
		CASE(inStable);
		CASE(inDiffAdvDone);
		CASE(inDiffAdvDoneMU);
		CASE(bndFrontMoved);
		CASE(bndNbrEdgesFound);
		CASE(bndDiffAdvDone);
		CASE(bndDiffAdvDoneMU);
		CASE(bndFrontApplied);
		CASE(bndMassCollectedFrontApplied);
		CASE(bndStable);
		CASE(outStable);
		CASE(transBndOut); 
		CASE(transInBnd);
		CASE(transOutBndSetBnd);
		CASE(transOutBndNbrSet);
		CASE(transOutBndMassCollected); 
		CASE(transOutBndSetIn);
	default:
		os << "missing state " << static_cast<int>(state); //cast required to avoid recursive call
	}
#undef CASE
	return os;
}
#ifdef MESH_ELEMENT_NODE_STATE_TRACK
std::ofstream slog("states.txt");
//void MeshElementNode::setState(moving_boundary::MeshElementStateful::State s) {
void MeshElementNode::DEBUG_SET_STATE(moving_boundary::MeshElementStateful::State s, const char *file, int line) {
	std::string full(file);
	size_t idx = full.find_last_of("/\\"); //just get file name, not path
	const char comma = ',';

	//to trace individual element calls
	//slog << ident( ) << comma << s << comma << full.substr(idx + 1) << comma << line << std::endl;

	//just to find active calls
	slog << stateVar << comma << s << comma << full.substr(idx + 1) << comma << line << std::endl;
	stateVar = s;
	VCELL_LOG(trace, ident( ) << " new state" ); 
	assert(debugSetState( ));
}
#endif
