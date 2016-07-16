#ifndef Physiology_h
#define Physiology_h
#include <array>
#include <vector>
#include <memory>
#include <cassert>
#include <Species.h>
using std::string;

namespace moving_boundary {
	namespace biology {
		struct Physiology {
			Physiology( )
				:species_( ),
				pSymTable( )
			{}

			//************************************
			// Species access
			//************************************
			/**
			* @throws std::domain_error if not locked
			*/
			size_t numberSpecies( )  const {
				return species_.size( );
			}

			/**
			* @throws std::domain_error if not locked
			*/
			const Species* species(size_t index) const {
				return species_[index];
			}
			typedef std::vector<Species*>::const_iterator SpeciesIterator;

			SpeciesIterator beginSpecies( ) const {
				return species_.begin( );
			}
			SpeciesIterator endSpecies( ) const {
				return species_.end( );
			}

			//************************************
			// Symbol access
			//************************************
			/**
			* @throws std::domain_error if not locked
			*/
			size_t numberSymbols( ) const {
				return pSymTable->size( );
			}

			/**
			* lookup name in symbol table
			* @throws std::domain_error if not locked or name invalid
			*/
			size_t symbolIndex(const string &name) const {
				auto entry =  pSymTable->getEntry(name);
				if (entry != nullptr) {
					return entry->getIndex( );
				}
			}

			/**
			* @throws std::domain_error if locked
			*/
			const Species* createSpecies(const string & name, const string& initial, const string & sourceExpression, const string & diffusionExpression);

			/**
			* build list of names for symbol table
			* includes all species names plus those passed in
			* @throws std::domain_error if locked
			*/
			void buildSymbolTable();

			const SymbolTable& symbolTable()
			{
				return *pSymTable;
			}

			static const vector<string> fixedTimeSpatialSymbols;
			int symbolIndex_t;
			int symbolIndex_coordinate;
			int symbolIndex_normal;
			int symbolIndex_species;

		private:
			Physiology(const Physiology &); //not defined
			std::vector<Species*> species_;
			std::unique_ptr<SimpleSymbolTable> pSymTable;

			void bindExpressions(Species* sp) {
				sp->bindExpressions(*pSymTable);
			}
		};
	}
}

#endif
