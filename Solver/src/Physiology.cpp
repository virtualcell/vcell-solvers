#include <algorithm>
#include <functional>
#include <Physiology.h>
#include <VCellException.h>
#include <MBConstants.h>

using moving_boundary::biology::Physiology;
using moving_boundary::biology::Species;
using namespace std::placeholders; 

const std::vector<std::string> Physiology::fixedTimeSpatialSymbols {"t", "x", "y", "z", "normalX", "normalY", "normalZ"};

namespace {
	/**
	* species to name functor
	*/
	struct SpeciesName {
		const string & operator( )(const Species* in) {
			return in->name( );
		}
	};
}

/**
* build list of names for symbol table
* includes all species names plus those passed in
*/
void Physiology::buildSymbolTable() {
	std::vector<string> names(species_.size( ) + fixedTimeSpatialSymbols.size());
	std::transform(species_.begin( ), species_.end( ),names.begin( ), SpeciesName( ));
	std::copy(fixedTimeSpatialSymbols.begin(), fixedTimeSpatialSymbols.end(), names.begin( ) + species_.size( ));
	symbolIndex_species = 0;
	symbolIndex_t = numberSpecies();
	symbolIndex_coordinate = symbolIndex_t + 1;
	symbolIndex_normal = symbolIndex_coordinate + MAX_DIM;

	const int n = static_cast<int>(names.size( ));
	string *raw = const_cast<string *>(names.data( ));
	pSymTable.reset(new SimpleSymbolTable(raw,n));
	//values.resize(n);

	using std::placeholders::_1;
	std::for_each(species_.begin( ),species_.end( ), std::bind(&Physiology::bindExpressions, this, _1) );
}

const Species* Physiology::createSpecies(const string & name, const string& initial, const string & source,const string & diffusion) {
	try {
		species_.push_back(new Species(name, initial, source,diffusion));
		return species_.back( );
	} catch (std::exception &de) {
		VCELL_EXCEPTION(domain_error, "error creating " << name << " with initial " << initial << ", source " << source << " and diffusion " << diffusion << ": " << de.what( ));
	}
}
