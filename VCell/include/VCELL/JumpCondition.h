#ifndef JUMPCONDITION
#define JUMPCONDITION

class Membrane;
class Expression;
class SymbolTable;
struct MembraneElement;
class SimulationExpression;

class JumpCondition
{
public:
	JumpCondition(Membrane*, Expression*);
	~JumpCondition(void);

	Expression *getExpression() {
		return expression;
	}
	Membrane* getMembrane() {
		return membrane;
	}

	void bindExpression(SymbolTable*);
	double evaluateExpression(double* values);
	double evaluateExpression(SimulationExpression*, MembraneElement*);
	void reinitConstantValues();

private:
	Membrane* membrane;
	Expression *expression;
	double* constantValue;
	bool bNeedsXYZ;

	bool isConstantExpression();
};

#endif
