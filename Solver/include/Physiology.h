#ifndef Physiology_h
#define Physiology_h
#include <array>
#include <vector>
#include <memory>
#include <cassert>
#include <Species.h>
namespace moving_boundary {
	namespace biology {
		struct Physiology {
			Physiology( )
				:species_( ),
				pSymTable( ),
				locked(false)
			{}

			/**
			* @throws std::domain_error if not locked
			*/
			size_t numSpecies( )  const {
				verifyLocked( );
				return species_.size( );
			}

			/**
			* @throws std::domain_error if not locked
			*/
			const std::vector<const Species> & species( ) const {
				verifyLocked( );
				return species_;
			}


			/**
			* @throws std::domain_error if not locked
			*/
			size_t numberSymbols( ) const {
				verifyLocked( );
				return pSymTable->size( );
			}

			/**
			* lookup name in symbol table
			* @throws std::domain_error if not locked
			*/
			int symbolIndex(const std::string &name) const {
				verifyLocked( );
				auto entry =  pSymTable->getEntry(name);
				if (entry != nullptr) {
					return entry->getIndex( );
				}
				//always throws
				return badName(name);
			}

			/**
			* @throws std::domain_error if locked
			*/
			void createSpecies(const std::string & name, const std::string & expression) {
				verifyUnlocked( );
				species_.push_back(Species(name,expression));
			}

			/**
			* build list of names for symbol table
			* includes all species names plus those passed in
			* @throws std::domain_error if locked
			*/
			void buildSymbolTable(const std::vector<std::string> &vec) {
				verifyUnlocked( );
				ibuild(vec.data( ), vec.size( ));
			}

			/**
			* build list of names for symbol table
			* includes all species names plus those passed in
			* @throws std::domain_error if locked
			*/
			template <int N>
			void buildSymbolTable(const std::array<string,N> & arr) {
				verifyUnlocked( );
				ibuild(arr.data( ), arr.size( ));
			}

			/**
			* prevent further changes
			*/
			void lock( ) {
				assert(pSymTable.get( ) != nullptr);
				locked = true;
			}

		private:
			Physiology(const Physiology &); //not defined
			std::vector<const Species> species_;
			std::unique_ptr<SimpleSymbolTable> pSymTable;
			bool locked;

			void setTable(Species &sp) {
				sp.setTable(*pSymTable);
			}

			void ibuild(const std::string *, size_t);
			int badName(const std::string &name) const;
			void verifyUnlocked( ) const; 
			void verifyLocked( ) const; 
		};
	}
}

#endif
