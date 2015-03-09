#include <MPoint.h>
#include <functional>
#include <ExplicitSolver.h>
#include <MeshElementNode.h>
#include <Logger.h>
using moving_boundary::ExplicitSolver;
using moving_boundary::CoordinateType;
using moving_boundary::BioQuanType;
using moving_boundary::TimeType;
using moving_boundary::MeshElementNode;

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
	std::vector<BioQuanType> deltaMass;
}
ExplicitSolver::ExplicitSolver(Mesh &m)
			:mesh(m),
			sIdx(0)
{
	deltaMass.resize(m.numCells( ));
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
		const BioQuanType jdelta =  node.mass(sIdx) * iter->second * timeStep; 
		const BioQuanType idelta =  i.mass(sIdx) * iter->iComponent * timeStep; 


		const BioQuanType delta = jdelta + idelta; 
		
		result += delta; 
		VCELL_COND_LOG(info,sIdx == 0,i.ident( ) << " j " << node.ident( ) << " mass " << node.mass(sIdx) 
			<< " cf " << iter->second << " * t product = " << jdelta  << " i mass " << i.mass(sIdx) << " cf " 
			<< iter->iComponent << " * t product = " << idelta << " delta " << delta
			<< " result " <<result); 
	}
	//const BioQuanType delta =  i.mass(sIdx) * coeff * timeStep;
	//result += delta; 
	MeshPosition p = mesh.indexOf(i.indexes( )); 
	VCELL_COND_LOG(info,sIdx == 0,i.ident( ) << " result " <<result << " in slot " << p.to<size_t>( )); 
	deltaMass[p.to<size_t>()]= result;
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
	deltaMass[p.to<size_t>()]= result;
	cData.clear( );
}
#endif

void ExplicitSolver::solve( ) {
	std::for_each(mesh.begin( ),mesh.end( ),SetMass(*this));
}

void ExplicitSolver::setMass(MeshElementNode &e) {
	if (e.isInside( )) {
		MeshPosition p = mesh.indexOf(e.indexes( )); 
		const size_t slot = p.to<size_t>();
		const BioQuanType delta = deltaMass[slot];
		BioQuanType newMass = e.mass(sIdx) - delta; 
		 VCELL_COND_LOG(info,sIdx == 0,e.ident( ) << " old mass " << e.mass(sIdx) << " + delta from slot " 
			 << slot << ' '  << delta << " is " << newMass); 
		e.setMass(sIdx,newMass);
	}
}
