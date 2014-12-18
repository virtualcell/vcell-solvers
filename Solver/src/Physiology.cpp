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
		const std::string & operator( )(const Species & in) {
			return in.name( );
		}
	};
}

/**
* build list of names for symbol table
* includes all species names plus those passed in
*/
void Physiology::ibuild(const std::string * data, size_t howMany) {
	std::vector<std::string> names(species_.size( ) + howMany); 
	std::transform(species_.begin( ), species_.end( ),names.begin( ),SpeciesName( ));
	std::copy(data,data + howMany,names.begin( ) + species_.size( ));
	const int n = static_cast<int>(names.size( ));
	std::string *raw = const_cast<std::string *>(names.data( ));
	pSymTable.reset(new SimpleSymbolTable(raw,n));
	//values.resize(n);

	using std::placeholders::_1;
	std::for_each(species_.begin( ),species_.end( ), std::bind(&Physiology::setTable, this, _1) );
}

int Physiology::badName(const std::string &name) const {
	VCELL_EXCEPTION(domain_error, "invalid name " << name);
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
