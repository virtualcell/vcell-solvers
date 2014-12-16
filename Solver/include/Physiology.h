#ifndef Physiology_h
#define Physiology_h
#include <array>
#include <vector>
#include <memory>
#include <Species.h>
namespace moving_boundary {
	namespace biology {
		struct Physiology {
			Physiology( )
				:species_( ),
				dirtySimTable(true),
				pSymTable( ),
				values( ) {}

			void createSpecies(const char *name_, const char *expression) {
				species_.push_back(Species(name_,expression));
				dirtySimTable = true;
			}

			size_t numSpecies( )  const {
				return species_.size( );
			}

			const Species & species(size_t indx) const {
				return species_[indx];
			}

			void buildSymbolTable(const std::vector<std::string> &vec) {
				ibuild(vec.data( ), vec.size( ));
			}

			template <int N>
			void buildSymbolTable(const std::array<string,N> & arr) {
				ibuild(arr.data( ), arr.size( ));
			}

			size_t numberSymbols( ) const {
				return pSymTable->size( );
			}

			int symbolIndex(const std::string &name) {
				auto entry =  pSymTable->getEntry(name);
				if (entry != nullptr) {
					return entry->getIndex( );
				}
				return badName(name);
			}

		private:
			std::vector<Species> species_;
			bool dirtySimTable;
			std::unique_ptr<SimpleSymbolTable> pSymTable;
			std::vector<double> values;

			void setTable(Species &sp) {
				sp.setTable(*pSymTable);
			}

			void ibuild(const std::string *, size_t);
			int badName(const std::string &name);
		};
	}
}

#endif
