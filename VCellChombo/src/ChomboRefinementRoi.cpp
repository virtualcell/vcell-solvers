#include <VCELL/ChomboRefinementRoi.h>
#include <SimpleSymbolTable.h>

 ChomboRefinementRoi::ChomboRefinementRoi(ChomboGeometry* geometry, int lvl, int tg, string& roiExp)
 : chomboGeometry(geometry), level(lvl), tagsGrow(tg)
 {
	string spatialSymbols[3] = {"x", "y", "z"};
	roiSymbolTable = new SimpleSymbolTable(spatialSymbols, 3, NULL);
	roi = new VCell::Expression(roiExp);
	roi->bindExpression(chomboGeometry->getGeometrySymbolTable());
 }

 ChomboRefinementRoi::~ChomboRefinementRoi()
 {
	 
 }

