#ifndef Variable_h
#define Variable_h
#include <string>
#include <SExpression.h>
using std::string;

namespace moving_boundary
{
  namespace biology
	{
  	struct Physiology;

		struct Variable {
  		enum VariableType
			{
  			unknown=0,
				volume=1,
				point=10,
			};
			enum ExpressionIndex
			{
				expr_initial=0,
				expr_source,
				expr_diffusion,
				expr_advection_x,
				expr_advection_y,
				expr_size,
			};

			Variable(const string & name);
			virtual ~Variable();

			const string & name( ) const {
				return name_;
			}

			void setExpression(ExpressionIndex exprIndex, const string& expr);
			const SExpression* getExpression(ExpressionIndex exprIndex ) const
			{
				return expressions[exprIndex];
			}

			bool isAdvecting() const
			{
				return bAdvecting;
			}

		protected:
			friend Physiology;
			bool bAdvecting;
			void bindExpressions(const SimpleSymbolTable &symTable);
			string name_;
			SExpression** expressions;
		};
	}
}

#endif
