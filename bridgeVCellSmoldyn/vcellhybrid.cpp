/* Steven Andrews, started 10/22/2001.
 This is the entry point for the Smoldyn program.  See documentation
 called Smoldyn_doc1.pdf and Smoldyn_doc2.pdf.
 Copyright 2003-2011 by Steven Andrews.  This work is distributed under the terms
 of the Gnu General Public License (GPL). */

#include "smoldyn.h"
#include "random2.h"
#include "smoldynfuncs.h"
#include "vcellhybrid.h"
#include <string.h>
#include <string>
using std::string;

#include <algorithm>
using namespace std;
#include <VCELL/SimTool.h>
#include <VCELL/SimulationExpression.h>
#include "VCellValueProvider.h"
#include "VCellMesh.h"
#include "VCellSmoldynOutput.h"

extern VCellSmoldynOutput* vcellSmoldynOutput;
simptr vcellhybrid::smoldynInit(SimTool* simTool, string& fileName) {
	LoggingCallback=NULL;
	ThrowThreshold=10;

	char root[STRCHAR],fname[STRCHAR],flags[STRCHAR],*cptr;

	for(int i=0;i<STRCHAR;i++) root[i]=fname[i]=flags[i]='\0';
	strcpy(root, fileName.c_str());
	cptr=strrpbrk(root,":\\/");
	if(cptr) cptr++;
	else cptr=root;
	strcpy(fname,cptr);
	*cptr='\0';

	simptr sim = NULL;
	int er;

	er=simInitAndLoad(root,fname,&sim,flags,new VCellValueProviderFactory(simTool), new VCellMesh(simTool));
	er=simUpdateAndDisplay(sim);
	er=scmdopenfiles((cmdssptr)sim->cmds,1);
	
	vcellSmoldynOutput = new VCellSmoldynOutput(sim);///check it out.
	vcellSmoldynOutput->setSimTool(simTool);

	sim->clockstt=time(NULL);
	er=simdocommands(sim);

	SimulationExpression* vcellSim = (SimulationExpression*)simTool->getSimulation();
	SymbolTable* symbolTable = vcellSim->getSymbolTable();
	char erstr[1024];
	//initialization for reaction rates (as expression)
	for(int j = 0; j < MAXORDER; j++)
	{
		rxnssptr rxnssInOrder = sim -> rxnss[j]; //loop through 0th, 1st, 2nd order rxn lists
		if (rxnssInOrder == 0) {
			continue;
		}
		for (int i = 0; i < rxnssInOrder->totrxn; i ++) {
			valueproviderptr valueProvider = rxnssInOrder->rxn[i]->rateValueProvider;
			if(valueProvider != NULL)
			{
				((VCellValueProvider*)valueProvider)->bindExpression(symbolTable);
			}
		}
	}

	//initialization for surface action(asorption, desorption, transmission) rates (as expression)
	if(sim->srfss != NULL)
	{
		surfacessptr surfacess = sim -> srfss;
		if(surfacess->nsrf > 0)
		{
			int numSrfs = surfacess->nsrf;
			surfaceptr * surfacelist = surfacess->srflist;
			surfactionptr actdetails;
			enum MolecState ms,ms2;
			enum PanelFace face;
			int nspecies=sim->mols?sim->mols->nspecies:0;
			for(int s=0; s<numSrfs; s++) {
				surfaceptr srf=surfacelist[s];
				for(int i=0; i<nspecies; i++){
					for(ms=(MolecState)0; ms<MSMAX; ms=(MolecState)(ms+1)){
						for(face=(PanelFace)0; face<3; face=(PanelFace)(face+1)){
							if(srf->actdetails != NULL && srf->actdetails[i]!=NULL && srf->actdetails[i][ms]!=NULL && srf->actdetails[i][ms][face] !=NULL) {
								actdetails=srf->actdetails[i][ms][face];
								for(ms2=(MolecState)0;ms2<MSMAX1;ms2=(MolecState)(ms2+1)) {
									if(actdetails != NULL && actdetails->srfRateValueProvider[ms2] != NULL)
									{
										valueproviderptr valueProvider = actdetails->srfRateValueProvider[ms2];
										((VCellValueProvider*)valueProvider)->bindExpression(symbolTable);
									}
								}
							}
						}
					}
				}
			}
		}
	}
	return sim;
}

void vcellhybrid::smoldynOneStep(simptr sim){
	simulatetimestep(sim);
	vcellSmoldynOutput->computeHistogram();
}

void vcellhybrid::smoldynEnd(simptr sim) {
	int er = 0;
	sim->elapsedtime+=difftime(time(NULL),sim->clockstt);
	endsimulate(sim,er);
	simfree(sim);
}

bool vcellhybrid::bHybrid = false;
int vcellhybrid::taskID = -1;
