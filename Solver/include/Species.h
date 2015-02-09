#ifndef Species_h
#define Species_h
#include <string>
#include <SExpression.h>
namespace moving_boundary {
	namespace biology {

		struct Species {
			Species(const std::string & name, const std::string & source, const std::string &diffusion)
				:name_(name),
				sourceExp(source),
				diffusionExp(diffusion) {}


			void setTable(const SimpleSymbolTable &symTable) {
				sourceExp.bindExpression(symTable);
				diffusionExp.bindExpression(symTable);
			}


			/**
			* @tparam CTR std::vector, std::array, et. al.
			*/
			/*
			template <class CTR>
			double evaluate(const CTR  &values) const {
				return sourceExp.evaluate(values);
			}
			*/

			const std::string & name( ) const {
				return name_;
			}

			/**
			* return expression
			*/
			const SExpression & sourceTerm( ) const {
				return sourceExp;
			}

			/**
			* return expression
			*/
			const SExpression & diffusionTerm( ) const {
				return diffusionExp;
			}

		private:
			std::string name_;
			SExpression sourceExp;
			SExpression diffusionExp;

		};

	}

}

#endif
