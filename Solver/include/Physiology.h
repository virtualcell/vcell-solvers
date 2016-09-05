#ifndef Physiology_h
#define Physiology_h
#include <array>
#include <vector>
#include <memory>
#include <cassert>
#include <VolumeSubdomain.h>
#include <PointSubdomain.h>
#include <VolumeVariable.h>
#include <PointVariable.h>
using std::string;

namespace moving_boundary {
	struct SException;

	struct Physiology {
		Physiology( )
			:volumeVariables_( ),
			 _symbolTable(nullptr),
			pointSymbolTable(nullptr)
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
			return _symbolTable->size( );
		}

		/**
		* lookup name in symbol table
		* @throws std::domain_error if not locked or name invalid
		*/
		size_t symbolIndex(const string &name) const {
			auto entry =  _symbolTable->getEntry(name);
			if (entry != nullptr) {
				return entry->getIndex( );
			}
		}

		/**
		* @throws std::domain_error if locked
		*/
		void addVariable(Variable* s);
		void addSubdomain(Subdomain* s);
		int numVolumeVariables() { return volumeVariables_.size(); }
		const VolumeVariable* getVolumeVariable(int vIndex) { return volumeVariables_.at(vIndex); }

		/**
		* build list of names for symbol table
		* includes all species names plus those passed in
		* @throws std::domain_error if locked
		*/
		void buildSymbolTable();

		const SymbolTable* symbolTable()
		{
			return _symbolTable;
		}

		int symbolIndexOfT() const
		{
			return symbolIndex_t;
		}

		int symbolIndexOfCoordinate() const
		{
			return symbolIndex_coordinate;
		}

		int symbolIndexOfSpecies() const
		{
			return symbolIndex_species;
		}

		int symbolIndexOfNormal() const
		{
			return symbolIndex_normal;
		}

	private:

		static const vector<string> fixedTimeSpatialSymbols;
		int symbolIndex_t;
		int symbolIndex_coordinate;
		int symbolIndex_normal;
		int symbolIndex_species;

		Physiology(const Physiology &); //not defined
		std::vector<VolumeVariable*> volumeVariables_;
		std::vector<PointVariable*> _pointVariables;
		std::vector<VolumeSubdomain*> _volumeSubdomains;
		std::vector<PointSubdomain*> _pointSubdomains;
		SimpleSymbolTable* _symbolTable;
		SimpleSymbolTable* pointSymbolTable;

		void bindExpressions(VolumeVariable* sp) {
			sp->bindExpressions(_symbolTable);
		}

		friend struct SExpression;
	};
}

#endif
