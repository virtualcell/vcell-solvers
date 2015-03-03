#include <stdexcept>
#include <Species.h>
#include <VCellException.h>
using moving_boundary::biology::Species;

void Species::setTable(const SimpleSymbolTable &symTable) {
	sourceExp.bindExpression(symTable);
	diffusionExp.bindExpression(symTable);
	size_t n = diffusionExp.numberSymbols( );
	actDiffSpecI.reserve(n);
	std::vector<std::string> symbols = diffusionExp.getSymbols( );
	for (std::string sym : symbols) {
		SymbolTableEntry * ste = symTable.getEntry(sym);
		if (ste == nullptr) {
			VCELL_EXCEPTION(domain_error,"input sym table does not contain required symbol " << sym);
		}
		actDiffSpecI.push_back(ste->getIndex( ));
	}
}
