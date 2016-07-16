#ifndef Species_h
#define Species_h
#include <string>
#include <SExpression.h>
using std::string;

namespace moving_boundary {
	namespace biology {
		struct Physiology;

		struct Species {
			enum expr
			{
				expr_initial=0,
				expr_source,
				expr_diffusion,
				expr_size,
			};

			Species(const string & name, const string& initial, const string & source, const string &diffusion);
			~Species();

			const string & name( ) const {
				return name_;
			}

			/**
			* return expression
			*/
			const SExpression & sourceTerm( ) const {
				return expressions[expr_source];
			}

			/**
			* return expression
			*/
			const SExpression & diffusionTerm( ) const {
				return expressions[expr_diffusion];
			}

			const SExpression& initialCondition() const {
				return expressions[expr_initial];
			}

		private:
			friend Physiology;
			void bindExpressions(const SimpleSymbolTable &symTable);
			string name_;
			SExpression* expressions;
		};
	}

}

#endif
