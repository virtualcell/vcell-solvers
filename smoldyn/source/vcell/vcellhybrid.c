/* Steven Andrews, started 10/22/2001.
 This is the entry point for the Smoldyn program.  See documentation
 called Smoldyn_doc1.pdf and Smoldyn_doc2.pdf.
 Copyright 2003-2011 by Steven Andrews.  This work is distributed under the terms
 of the Gnu General Public License (GPL). */

#include "smoldyn.h"
#include "random2.h"
#include "smoldynfuncs.h"
#include <string>
using std::string;

#include <algorithm>
using namespace std;

simptr smoldynInit(SimTool* simTool, string& fileName) {
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
	er=setupsim(root,fname,&sim,flags);
	er=scmdopenfiles((cmdssptr)sim->cmds,1);
	sim->simTool = simTool;
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
			try {
				Expression* rateExpression = rxnssInOrder->rxn[i]->rateExp;
				double r = rateExpression->evaluateConstant();
				delete rxnssInOrder->rxn[i]->rateExp;
				rxnssInOrder->rxn[i]->rateExp = NULL;
				rxnssInOrder->rxn[i]->rate = r;
				
				er=rxnsetrate(sim,j,i,erstr);
			} catch (...) {
				rxnssInOrder->rxn[i]->rateExp->bindExpression(symbolTable);
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
			Expression * surfRateExp;
			for(int s=0; s<numSrfs; s++) {
				surfaceptr srf=surfacelist[s];
				for(int i=0; i<nspecies; i++){
					for(ms=(MolecState)0; ms<MSMAX; ms=(MolecState)(ms+1)){
						for(face=(PanelFace)0; face<3; face=(PanelFace)(face+1)){
							if(srf->actdetails != NULL && srf->actdetails[i]!=NULL && srf->actdetails[i][ms]!=NULL && srf->actdetails[i][ms][face] !=NULL) {
								actdetails=srf->actdetails[i][ms][face];
								for(ms2=(MolecState)0;ms2<MSMAX1;ms2=(MolecState)(ms2+1)) {
									if(actdetails != NULL && actdetails->srfRateExp[ms2] != NULL)
									{
										surfRateExp = actdetails->srfRateExp[ms2];
										try {
											surfRateExp->evaluateConstant();
											delete actdetails->srfRateExp[ms2];
											actdetails->srfRateExp[ms2] = NULL;
											//if can be evaluated to constant, set rate is done in surfreadstring of smolsurface.c
										} catch (...) {
											actdetails->srfRateExp[ms2]->bindExpression(symbolTable);
										}
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

void smoldynOneStep(simptr sim){
	simulatetimestep(sim);
}

void smoldynEnd(simptr sim) {
	int er = 0;
	sim->elapsedtime+=difftime(time(NULL),sim->clockstt);
	endsimulate(sim,er);
	simfree(sim);
}
