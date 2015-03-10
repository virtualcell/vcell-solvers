#include <MPoint.h>
#include <map>
#include <functional>
#include <ExplicitSolver.h>
#include <MeshElementNode.h>
#include <Logger.h>
using moving_boundary::ExplicitSolver;
using moving_boundary::CoordinateType;
using moving_boundary::BioQuanType;
using moving_boundary::TimeType;
using moving_boundary::MeshElementNode;
using moving_boundary::CoordinateProductType;

using spatial::MeshPosition;
namespace {
	struct CoeffData {
		MeshPosition first;
		/*
		* j coefficient
		*/
		BioQuanType second;
		BioQuanType iComponent; 
		CoeffData(const MeshPosition & m, BioQuanType j, BioQuanType i = 0)
			:first(m),
			second(j),
			iComponent(i) {}
	};
	//use local for devel, refactor into class later
	TimeType timeStep;
	//typedef std::pair<MeshPosition,BioQuanType> CoeffData;  
	std::vector<CoeffData> cData;
	std::vector<BioQuanType> deltaC;
	std::map<MeshPosition,CoordinateProductType> boundaryVolumes;

	typedef spatial::Mesh<moving_boundary::CoordinateType,2,MeshElementNode> ZMesh; 
	CoordinateProductType volumeOf(const ZMesh & mesh, const MeshElementNode &e) {
		switch (e.mPos( )) {
		case spatial::interiorSurface:
			return e.volumePD( );
		case spatial::boundarySurface:
			{
				MeshPosition p = mesh.indexOf(e.indexes( ));
				if (boundaryVolumes.find(p) == boundaryVolumes.end( )) {

				}
				return boundaryVolumes[p];
			}
		case spatial::outsideSurface:
			VCELL_EXCEPTION(domain_error, e.ident( ) << " outside surface" );
		default:
			VCELL_EXCEPTION(domain_error, e.ident( ) << " position " << e.mPos( )); 
		}
	}
}

ExplicitSolver::ExplicitSolver(Mesh &m)
			:mesh(m),
			sIdx(0)
{
	deltaC.resize(m.numCells( ));
}

void ExplicitSolver::begin( ) {
	boundaryVolumes.clear( );
}

void ExplicitSolver::remember(MeshElementNode & boundaryNode) {
	MeshPosition p = mesh.indexOf(boundaryNode.indexes( )); 
	boundaryVolumes[p] = boundaryNode.volumePD( );
}

void ExplicitSolver::setStepAndSpecies(TimeType t, unsigned int s) {
	timeStep = t;
	sIdx= s;
	cData.clear( );
}
void ExplicitSolver::setCoefficent(const MeshElementNode &j, BioQuanType coeff, BioQuanType iCoeff) {
	MeshPosition p = mesh.indexOf(j.indexes( )); 
	cData.push_back(CoeffData(p,coeff,iCoeff));
}
#define DVERSION
#ifdef DVERSION
void ExplicitSolver::setSolvingFor(MeshElementNode &i, BioQuanType coeff) {
	VCELL_COND_LOG(info,sIdx == 0,i.ident( ) << " timeStep " << timeStep); 
	BioQuanType result = 0;
	//explicity find and calculate the j terms
	for (std::vector<CoeffData>::const_iterator  iter = cData.begin( ); iter != cData.end( ); ++iter) {
		MeshElementNode & node = mesh.get(iter->first);
		const BioQuanType jdelta =  node.priorConcentration(sIdx) * iter->second * timeStep; 
		const BioQuanType idelta =  i.priorConcentration(sIdx) * iter->iComponent * timeStep; 


		const BioQuanType delta = jdelta - idelta; 
		
		result += delta; 
		VCELL_COND_LOG(info,sIdx == 0,i.ident( ) << " j " << node.ident( ) << " u " << node.priorConcentration(sIdx) 
			<< " cf " << iter->second << " * t product = " << jdelta  << " i u " << i.priorConcentration(sIdx) << " cf " 
			<< iter->iComponent << " * t product = " << idelta << " delta " << delta
			<< " result " <<result); 
	}
	//const BioQuanType delta =  i.mass(sIdx) * coeff * timeStep;
	//result += delta; 
	const BioQuanType u0 = i.mass(sIdx) / i.volumePD( ); 
	const BioQuanType u0x = i.concentration(sIdx); 
	assert(u0 == u0x);
	const BioQuanType sum =  result + u0;

	MeshPosition p = mesh.indexOf(i.indexes( )); 
	deltaC[p.to<size_t>()]= sum; 
	VCELL_COND_LOG(info,sIdx == 0,i.ident( ) << " result " << result << " u0 " << u0 << " = " << sum << " in slot " << p.to<size_t>( )); 
	cData.clear( );
}
#else
void ExplicitSolver::setSolvingFor(MeshElementNode &i, BioQuanType coeff) {
	VCELL_COND_LOG(info,sIdx == 0,i.ident( ) << " timeStep " << timeStep); 
	BioQuanType result = 0;
	//explicity find and calculate the j terms
	for (std::vector<CoeffData>::const_iterator  iter = cData.begin( ); iter != cData.end( ); ++iter) {
		MeshElementNode & node = mesh.get(iter->first);
		const BioQuanType delta =  node.mass(sIdx) * iter->second * timeStep; 
		result += delta; 
		VCELL_COND_LOG(info,sIdx == 0,i.ident( ) << " j " << node.ident( ) << " mass " << node.mass(sIdx) 
			<< " cf " << iter->second << " * t product = " << delta << " result " <<result); 
	}
	const BioQuanType delta =  i.mass(sIdx) * coeff * timeStep;
	result += delta; 
	MeshPosition p = mesh.indexOf(i.indexes( )); 
	VCELL_COND_LOG(info,sIdx == 0,i.ident( ) << " i mass " << i.mass(sIdx)  << " cf " << coeff << " * t product ="
		<< delta << " result " <<result << " in slot " << p.to<size_t>( )); 
	deltaC[p.to<size_t>()]= result;
	cData.clear( );
}
#endif

void ExplicitSolver::solve( ) {
	std::for_each(mesh.begin( ),mesh.end( ),SetConc(*this));
}

void ExplicitSolver::setConcentration(MeshElementNode &e) {
	if (e.isInside( )) {
		MeshPosition p = mesh.indexOf(e.indexes( )); 
		const size_t slot = p.to<size_t>();
		const BioQuanType u = deltaC[slot];

		e.setTransConcentration(sIdx,u);
		VCELL_COND_LOG(info,sIdx == 0,e.ident( ) << " conc " << u << " from slot " << p.to<size_t>( )); 
	}
}
