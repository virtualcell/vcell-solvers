#ifndef CHOMBOREFINEMENTROI_H_
#define CHOMBOREFINEMENTROI_H_

#include <string>
using std::string;

#include <Expression.h>
#include <VCELL/ChomboGeometry.h>

class SymbolTable;
namespace VCell
{
	class Expression;
}
class ChomboRefinementRoi
{
public:
	ChomboRefinementRoi(ChomboGeometry* geometry, int level, int tagsGrowSize, string& roi);
	virtual ~ChomboRefinementRoi();

	ChomboGeometry* getChomboGeometry() {
		return chomboGeometry;
	}

	VCell::Expression* getRoi()
	{
		return roi;
	}

	int getLevel()
	{
		return level;
	}
	
	const int getTagsGrow()
	{
		return tagsGrow;
	}

	const double* getConstantValue()
	{
		return constantValue;
	}
private:
	ChomboGeometry* chomboGeometry;
	int level;
	VCell::Expression* roi;
	int tagsGrow;
	double* constantValue;
};

#endif /*CHOMBOREFINEMENTROI_H_*/
