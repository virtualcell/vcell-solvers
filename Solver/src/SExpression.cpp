#include <SExpression.h>
#include <VCellException.h>
#include <vector>
#include <array>
using moving_boundary::SExpression;


void SExpression::badContainer(const type_info &ti, size_t size) const {
	VCELL_EXCEPTION(domain_error, "container of type " << ti.name( ) << " has " << size 
		<< " values, " << static_cast<unsigned int>(numSymbols) << " required");
}
void SExpression::checkSize(size_t used) const {
	if (used > std::numeric_limits<NSym>::max( )) {
		VCELL_EXCEPTION(domain_error, "sym table size " << used  << " greater than supported by "
			<< typeid(NSym).name( ) << ", max allowed " << std::numeric_limits<NSym>::max( ));
	}
}



	
