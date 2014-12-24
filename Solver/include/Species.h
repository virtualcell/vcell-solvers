#ifndef Species_h
#define Species_h
#include <string>
#include <SExpression.h>
namespace moving_boundary {
	namespace biology {

		struct Species {
			Species(const std::string & name, const std::string & sexpression)
				:name_(name),
				sourceExp(sexpression) {}

			void setTable(const SimpleSymbolTable &symTable) {
				sourceExp.bindExpression(symTable);
			}

			/**
			* @tparam CTR std::vector, std::array, et. al.
			*/
			template <class CTR>
			double evaluate(const CTR  &values) const {
				return sourceExp.evaluate(values);
			}

			const std::string & name( ) const {
				return name_;
			}

			/**
			* does this have a constant source term?
			* @return true if does
			*/
			const SExpression & sourceTerm( ) const {
				return sourceExp;
			}

		private:
			std::string name_;
			SExpression sourceExp;

		};

	}

}

#endif
