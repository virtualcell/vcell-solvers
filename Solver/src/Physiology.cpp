#include <algorithm>
#include <functional>
#include <Physiology.h>
#include <VCellException.h>
#include <MBConstants.h>

using moving_boundary::biology::Physiology;
using moving_boundary::biology::VolumeVariable;
using namespace std::placeholders; 

const std::vector<std::string> Physiology::fixedTimeSpatialSymbols {"t", "x", "y", "z", "normalX", "normalY", "normalZ"};

namespace {
	/**
	* species to name functor
	*/
	struct VariableName {
		const string & operator( )(const VolumeVariable* in) {
			return in->name( );
		}
	};
}

/**
* build list of names for symbol table
* includes all species names plus those passed in
*/
void Physiology::buildSymbolTable() {
	std::vector<string> names(volumeVariables_.size( ) + fixedTimeSpatialSymbols.size());
	std::transform(volumeVariables_.begin( ), volumeVariables_.end( ),names.begin( ), VariableName( ));
	std::copy(fixedTimeSpatialSymbols.begin(), fixedTimeSpatialSymbols.end(), names.begin( ) + volumeVariables_.size( ));
	symbolIndex_species = 0;
	symbolIndex_t = numVolumeVariables();
	symbolIndex_coordinate = symbolIndex_t + 1;
	symbolIndex_normal = symbolIndex_coordinate + MAX_DIM;

	const int n = static_cast<int>(names.size( ));
	string *raw = const_cast<string *>(names.data( ));
	pSymTable.reset(new SimpleSymbolTable(raw,n));
	//values.resize(n);

	using std::placeholders::_1;
	std::for_each(volumeVariables_.begin( ),volumeVariables_.end( ), std::bind(&Physiology::bindExpressions, this, _1) );
}

const VolumeVariable* Physiology::createVolumeVariable(VolumeVariable* s) {
	try {
		volumeVariables_.push_back(s);
		return volumeVariables_.back( );
	} catch (std::exception &de) {
		VCELL_EXCEPTION(domain_error, "error creating Species " << s->name() << ": " << de.what( ));
	}
}
