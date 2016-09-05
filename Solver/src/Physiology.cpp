#include <algorithm>
#include <functional>
#include <Physiology.h>
#include <VCellException.h>
#include <MBConstants.h>

using moving_boundary::Physiology;
using moving_boundary::VolumeVariable;
using moving_boundary::PointVariable;
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
		const string & operator( )(const PointVariable* in) {
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
	_symbolTable = new SimpleSymbolTable(raw,n);
	//values.resize(n);

	using std::placeholders::_1;
	std::for_each(volumeVariables_.begin( ),volumeVariables_.end( ), std::bind(&Physiology::bindExpressions, this, _1) );
}

void Physiology::addVariable(Variable* s) {
	try {
		if (s->getType() == vartype_volume)
		{
			volumeVariables_.push_back((VolumeVariable*)s);
		}
		else if (s->getType() == vartype_point)
		{
			_pointVariables.push_back((PointVariable*)s);
		}
	} catch (std::exception &de) {
		VCELL_EXCEPTION(domain_error, "error adding Species " << s->name() << ": " << de.what( ));
	}
}

void Physiology::addSubdomain(Subdomain* s) {
	try {
		if (s->getType() == subdomain_volume)
		{
			_volumeSubdomains.push_back((VolumeSubdomain*)s);
		}
		else if (s->getType() == subdomain_point)
		{
			_pointSubdomains.push_back((PointSubdomain*)s);
		}
	} catch (std::exception &de) {
		VCELL_EXCEPTION(domain_error, "error adding subdomain " << s->name() << ": " << de.what( ));
	}
}
