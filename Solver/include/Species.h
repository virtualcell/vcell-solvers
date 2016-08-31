#ifndef Species_h
#define Species_h
#include <string>
#include <SExpression.h>
using std::string;

namespace moving_boundary {
	namespace biology {
		struct Physiology;

		struct Species {
			enum ExprIndex
			{
				expr_initial=0,
				expr_source,
				expr_diffusion,
				expr_advection_x,
				expr_advection_y,
				expr_size,
			};

			Species(const string & name);
			~Species();

			const string & name( ) const {
				return name_;
			}

			void setExpression(ExprIndex exprIndex, const string& expr);
			const SExpression* getExpression(ExprIndex exprIndex ) const
			{
				return expressions[exprIndex];
			}

			bool isAdvecting() const
			{
				return bAdvecting;
			}

		private:
			friend Physiology;
			bool bAdvecting;
			void bindExpressions(const SimpleSymbolTable &symTable);
			string name_;
			SExpression** expressions;
		};
	}

}

#endif
