#include <algorithm>
#include <functional>
#include <Physiology.h>
#include <VCellException.h>

using moving_boundary::biology::Physiology;
using moving_boundary::biology::Species;
using namespace std::placeholders; 
namespace {
	struct SpeciesName {
		const std::string & operator( )(const Species & in) {
			return in.name( );
		}
	};

}

/*
void Physiology::buildSymbolTable(const std::vector<std::string> &values) {
	std::vector<std::string> names(species_.size( ) + values.size( ));

	std::transform(species_.begin( ), species_.end( ),names.begin( ),SpeciesName( ));
	std::copy(values.begin( ),values.end( ),names.begin( ) + species_.size( ));
	const int n = static_cast<int>(names.size( ));
	std::string *raw = const_cast<std::string *>(names.data( ));
	pSymTable.reset(new SimpleSymbolTable(raw,n));
}
*/

void Physiology::ibuild(const std::string * data, size_t howMany) {
	std::vector<std::string> names(species_.size( ) + howMany); 
	std::transform(species_.begin( ), species_.end( ),names.begin( ),SpeciesName( ));
	std::copy(data,data + howMany,names.begin( ) + species_.size( ));
	const int n = static_cast<int>(names.size( ));
	std::string *raw = const_cast<std::string *>(names.data( ));
	pSymTable.reset(new SimpleSymbolTable(raw,n));
	values.resize(n);

	using std::placeholders::_1;
	std::for_each(species_.begin( ),species_.end( ), std::bind(&Physiology::setTable, this, _1) );


}

int Physiology::badName(const std::string &name) {
	VCELL_EXCEPTION(domain_error, "invalid name " << name);
}

