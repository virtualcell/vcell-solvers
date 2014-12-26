#include <MeshElementSpecies.h>
using namespace moving_boundary::MeshElementStateful;
using moving_boundary::MeshElementSpecies;
void MeshElementSpecies::setInitialPos(spatial::SurfacePosition pos) {
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

bool MeshElementSpecies::isInside( ) const {
	switch (state( )) {
	case initialInside:
	case inStable:
	case inDiffAdvDone:
	case inDiffAdvDoneMU:
	case inStableDeep:
	case inStableDeepDiffAdvDone:
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
	case outStableDeep:
	case transBndOut: 
		return false;
		break;
	}
	badState("isInside");
	return false;
}
spatial::SurfacePosition MeshElementSpecies::mPos( ) const {
	switch (state( )) {
	case inStableDeep:
	case inStableDeepDiffAdvDone:
		return spatial::deepInteriorSurface;
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
	case outStableDeep:
		return spatial::deepOutsideSurface;
	default:
		badState("mPos def");
	}
		return spatial::outsideSurface;
}
bool MeshElementSpecies::isDeep( ) const  {
	switch (state( )) {
	case inDiffAdvDone:
	case inDiffAdvDoneMU:
	case inStable:
	case initial:
	case bndDiffAdvDone:
	case bndDiffAdvDoneMU:
	case bndStable:
	case bndFrontMoved:
	case bndNbrEdgesFound:
	case bndFrontApplied:
	case outStable:
	case transBndOut: 
	case transInBnd:
	case transOutBndSetBnd:
	case transOutBndNbrSet:
	case transOutBndMassCollected: 
	case transOutBndSetIn:
		return false;
	case inStableDeepDiffAdvDone:
	case outStableDeep:
	case inStableDeep:
		return true;
	default:
		badState("isDeep");
	}
	return false; 
}

void MeshElementSpecies::listBoundary(std::ostream & os) const {
	using spatial::cX;
	using spatial::cY;
	switch (mPos( )) {
	case spatial::outsideSurface:
	case spatial::deepOutsideSurface:
		return;
	case spatial::interiorSurface:
	case spatial::deepInteriorSurface:
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
void MeshElementSpecies::badState(const char * const function) const {
	std::cout <<  ident( ) << " in " << function << std::endl;
	VCELL_EXCEPTION(logic_error, ident( ) << " in " << function);
}

MeshElementSpecies::OurType *MeshElementSpecies::neighbor(spatial::ElementOffset<2> & eo) const {
	//nominally unsafe downcast
	const MeshType * m = static_cast<const MeshType *>(&mesh);
	return m->element(*this,eo);
}

namespace {
	struct NeighborBuilder {
		MeshElementSpecies & client;
		NeighborBuilder(MeshElementSpecies &c)
			:client(c) {}

		MeshElementSpecies::NeighborType generate(std::istream &in) const {
			return MeshElementSpecies::NeighborType(in,client);
		}
	};
}

/**
* see #persist
*/
MeshElementSpecies::MeshElementSpecies(const MeshDefinition &owner,std::istream &is)
	:base(is),
	mesh(owner),
	concValue( ),
	sourceTermValues(concValue),
	vol(0,this) //required to register this as VolumeMonitor of volume
{
	vcell_persist::Token::check<MeshElementSpecies>(is); 

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
		MeshElementSpecies & client;
		NeighborWriter(MeshElementSpecies &c)
			:client(c) {}
		void operator( )(std::ostream &os, const MeshElementSpecies::NeighborType  & nb) const {
			nb.persist(os,client);
		}
	};
}

/**
* save essential state information necessary to restore from a file later
*/
void MeshElementSpecies::persist(std::ostream &os) {
	base::persist(os);

	NeighborWriter nw(*this);
	vcell_persist::writeFunctor<NeighborWriter> functor(os,nw);

	vcell_persist::Token::insert<MeshElementSpecies>(os); 
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
		CASE(inStableDeep);
		CASE(inStableDeepDiffAdvDone);
		CASE(bndFrontMoved);
		CASE(bndNbrEdgesFound);
		CASE(bndDiffAdvDone);
		CASE(bndDiffAdvDoneMU);
		CASE(bndFrontApplied);
		CASE(bndMassCollectedFrontApplied);
		CASE(bndStable);
		CASE(outStable);
		CASE(outStableDeep);
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
#ifdef MES_STATE_TRACK
std::ofstream slog("states.txt");
//void MeshElementSpecies::setState(moving_boundary::MeshElementStateful::State s) {
void MeshElementSpecies::DEBUG_SET_STATE(moving_boundary::MeshElementStateful::State s, const char *file, int line) {
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
