#ifndef Physiology_h
#define Physiology_h
#include <array>
#include <vector>
#include <memory>
#include <cassert>
#include <VolumeVariable.h>
using std::string;

namespace moving_boundary {
	namespace biology {
		struct Physiology {
			Physiology( )
				:volumeVariables_( ),
				pSymTable( )
			{}

			//************************************
			// Species access
			//************************************
			/**
			* @throws std::domain_error if not locked
			*/
			size_t numVolumeVariables( )  const {
				return volumeVariables_.size( );
			}

			/**
			* @throws std::domain_error if not locked
			*/
			const VolumeVariable* getVolumeVariable(size_t index) const {
				return volumeVariables_[index];
			}
			typedef std::vector<VolumeVariable*>::const_iterator SpeciesIterator;

			SpeciesIterator beginVolumeVariable( ) const {
				return volumeVariables_.begin( );
			}
			SpeciesIterator endVolumeVariable( ) const {
				return volumeVariables_.end( );
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
			const VolumeVariable* createVolumeVariable(VolumeVariable* s);

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
			std::vector<VolumeVariable*> volumeVariables_;
			std::unique_ptr<SimpleSymbolTable> pSymTable;

			void bindExpressions(VolumeVariable* sp) {
				sp->bindExpressions(*pSymTable);
			}
		};
	}
}

#endif
