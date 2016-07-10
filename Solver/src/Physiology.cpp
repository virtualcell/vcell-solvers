#include <algorithm>
#include <functional>
#include <Physiology.h>
#include <VCellException.h>

using moving_boundary::biology::Physiology;
using moving_boundary::biology::Species;
using namespace std::placeholders; 
namespace {
	/**
	* species to name functor
	*/
	struct SpeciesName {
		const string & operator( )(const Species & in) {
			return in.name( );
		}
	};

	/**
	* const diffusion functor 
	*/
	struct ConstDiff {
		void operator( )(const Species & in) {
			if (!in.diffusionTerm( ).isConstant( )) {
				VCELL_EXCEPTION(domain_error,in.name( ) << " has non-constant diffusion " << in.diffusionTerm( ).infix( ));
			}
		}
	};
}

/**
* build list of names for symbol table
* includes all species names plus those passed in
*/
void Physiology::ibuild(const string * data, size_t howMany) {
	std::vector<string> names(species_.size( ) + howMany);
	std::transform(species_.begin( ), species_.end( ),names.begin( ),SpeciesName( ));
	std::copy(data,data + howMany,names.begin( ) + species_.size( ));
	const int n = static_cast<int>(names.size( ));
	string *raw = const_cast<string *>(names.data( ));
	pSymTable.reset(new SimpleSymbolTable(raw,n));
	//values.resize(n);

	using std::placeholders::_1;
	std::for_each(species_.begin( ),species_.end( ), std::bind(&Physiology::bindExpressions, this, _1) );
}

int Physiology::badName(const string &name) const {
	VCELL_EXCEPTION(domain_error, "invalid name " << name);
}

void Physiology::lock( ) {
	assert(pSymTable.get( ) != nullptr);
	locked = true;
	std::for_each(species_.begin( ),species_.end( ), ConstDiff( ) ); 
}

void Physiology::verifyUnlocked( ) const {
	if (locked)  {
		throw std::domain_error("locked Physiology");
	}
}
void Physiology::verifyLocked( ) const {
	if (!locked)  {
		throw std::domain_error("unlocked Physiology");
	}
}

const Species & Physiology::createSpecies(const string & name, const string& initial, const string & source,const string & diffusion) {
	verifyUnlocked( );
	try {
		species_.push_back(Species(name, initial, source,diffusion));
		return species_.back( );
	} catch (std::exception &de) {
		VCELL_EXCEPTION(domain_error, "error creating " << name << " with initial " << initial << ", source " << source << " and diffusion " << diffusion << ": " << de.what( ));
	}
}
