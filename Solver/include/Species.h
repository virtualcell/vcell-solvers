#ifndef Species_h
#define Species_h
#include <string>
#include <SExpression.h>
using std::string;

namespace moving_boundary {
	namespace biology {
		struct Physiology;

		struct Species {
			Species(const string & name, const string initial, const string & source, const string &diffusion)
				:name_(name),
				initialExp(initial),
				sourceExp(source),
				diffusionExp(diffusion)
			{}

			const string & name( ) const {
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
			friend Physiology;
			void bindExpressions(const SimpleSymbolTable &symTable);
			string name_;
			SExpression initialExp;
			SExpression sourceExp;
			SExpression diffusionExp;
		};
	}

}

#endif
