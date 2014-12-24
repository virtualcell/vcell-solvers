#ifndef SExpression_h
#define SExpression_h
#include <typeinfo>
#include <Expression.h>
#include <SimpleSymbolTable.h>
namespace moving_boundary {

	struct SExpression {
		/**
		* @param exp string representation of function
		*/
		SExpression(std::string exp)
			:expression(exp),
			numSymbols(0) 
		{ }

		/**
		* @param exp string representation of function
		*/
		SExpression(const char * const exp)
			:expression(exp),
			numSymbols(0) 
		{ }

		/**
		* @param exp string representation of function
		* @param symTable symbol table -- must remain valid during
		* lifetime of object
		*/
		SExpression(std::string exp, const SimpleSymbolTable &symTable)
			:expression(exp,const_cast<SimpleSymbolTable &>(symTable)),
			numSymbols(static_cast<NSym>(symTable.size( )))
		{
			checkSize(symTable.size());
		}
		//default copy, assignment, destructor okay

		/**
		* @param symTable symbol table -- must remain valid during
		* lifetime of object
		*/
		void bindExpression(const SimpleSymbolTable &symbolTable) {
			checkSize(symbolTable.size());
			numSymbols = static_cast<NSym>(symbolTable.size( ));
			expression.bindExpression(const_cast<SimpleSymbolTable *>(&symbolTable));
		}
		/**
		* @return new generated string representation
		*/
		std::string infix( ) const {
			return expression.infix( );
		}

		/**
		* @tparam CTR std::vector, std::array, et. al.
		*/
		template <class CTR>
		double evaluate(const CTR  &values) const {
			if (values.size( ) == numSymbols) {
				return expression.evaluateVector(const_cast<double *>(values.data( )));
			}
			badContainer(typeid(CTR), values.size( ));
			return 0;
		}

		/**
		* is expression constant?
		* @return true if is
		*/
		bool isConstant( ) const {
			return expression.isConstant( );
		}

		/**
		* return constant value
		* @throws VCell::ExpressionException if #isConstant( ) != true
		*/ 
		double constantValue( ) const {
			return expression.evaluateConstant( );
		}

	private:
		typedef unsigned char NSym;
		void badContainer(const std::type_info &, size_t) const;
		void checkSize(size_t) const;

		mutable VCell::Expression expression;
		NSym numSymbols;
	};

}
#endif
