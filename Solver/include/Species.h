#ifndef Species_h
#define Species_h
#include <string>
#include <SExpression.h>
namespace moving_boundary {
	namespace biology {

		struct Species {
			Species(const char *name, const char *expression)
				:name_(name),
				sExp(expression) {}

			void setTable(const SimpleSymbolTable &symTable) {
				sExp.bindExpression(symTable);
			}

			/**
			* @tparam CTR std::vector, std::array, et. al.
			*/
			template <class CTR>
			double evaluate(const CTR  &values) const {
				return sExp.evaluate(values);
			}

			const std::string & name( ) const {
				return name_;
			}

		private:
			std::string name_;
			SExpression sExp;

		};

	}

}

#endif
