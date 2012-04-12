/* Steven Andrews, started 10/22/2001.
 This is the entry point for the Smoldyn program.  See documentation
 called Smoldyn_doc1.pdf and Smoldyn_doc2.pdf.
 Copyright 2003-2011 by Steven Andrews.  This work is distributed under the terms
 of the Gnu General Public License (GPL). */

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include "opengl2.h"
#include "smoldyn.h"
#include "random2.h"
#include "smoldynfuncs.h"

#ifndef VCELL_HYBRID
int main(int argc,char *argv[]);
#else
#include <string>
using std::string;
#include <VCELL/SimTool.h>
#include <VCELL/SimulationExpression.h>
#include <VCELL/Variable.h>
#include <VCELL/CartesianMesh.h>
#include <VCELL/DoubleVector3.h>
#include <VCELL/Element.h>
#endif




#include <stdarg.h>
void printfException(const char* format, ...) {
	char message[4000];
	int severity;
	va_list arguments;

	severity=2;			// total failure
	va_start(arguments, format);
	vsprintf(message, format, arguments);
	va_end(arguments);
	printf("%s", message);
#ifdef LOGGING
	if(loggingCallback)
		loggingCallback(severity,message);
#endif
	throw message;
}




/* ***************************************************************** */
/* ********************** main() segment *************************** */
/* ***************************************************************** */

/* main */
#ifndef VCELL 
// Original main
int main(int argc,char **argv) {
	int exitCode = 0;
	try {
  simptr sim;
  int i,er,pflag,qflag,wflag,tflag,Vflag,oflag;
  char root[STRCHAR],fname[STRCHAR],flags[STRCHAR],*cptr;

	LoggingCallback=NULL;
	ThrowThreshold=10;
	for(i=0;i<STRCHAR;i++) root[i]=fname[i]=flags[i]='\0';
	er=0;
	if(argc<=1) {
		fprintf(stderr,"Welcome to Smoldyn version %s.\n\n",VERSION);
		fprintf(stderr,"Enter name of configuration file: ");
		fgets(root,STRCHAR,stdin);
		if(strchr(root,'\n')) *(strchr(root,'\n'))='\0';
		fprintf(stderr,"Enter runtime flags (q=quiet, p=parameters only), or '-'=none: ");
		fgets(flags,STRCHAR,stdin);
		if(strchr(flags,'\n')) *(strchr(flags,'\n'))='\0'; }
	if(argc>1) {
		strncpy(root,argv[1],STRCHAR-1);
		root[STRCHAR-1]='\0';
		argc--;
		argv++; }
	er=Parse_CmdLineArg(&argc,argv,NULL);
	if(er) {
		if(er==1) fprintf(stderr,"Out of memory");
		else fprintf(stderr,"Follow command line '--define' options with key=replacement\n");
		return 0; }
	if(argc>1) {
		if(argv[1][0]=='-') {
			strncpy(flags,argv[1],STRCHAR-1);
			flags[STRCHAR-1]='\0';
			argc--;
			argv++; }
		else {
			fprintf(stderr,"Command line format: smoldyn [config_file] [-options] [-OpenGL_options]\n");
			return 0; }}

	cptr=strrpbrk(root,":\\/");
	if(cptr) cptr++;
	else cptr=root;
	strcpy(fname,cptr);
	*cptr='\0';

	oflag=strchr(flags,'o')?1:0;
	pflag=strchr(flags,'p')?1:0;
	qflag=strchr(flags,'q')?1:0;
	Vflag=strchr(flags,'V')?1:0;
	if(!strcmp(fname,"-V")) Vflag=1;
	wflag=strchr(flags,'w')?1:0;
	tflag=strchr(flags,'t')?1:0;

	if(Vflag) {
		printf("%s\n",VERSION);
		return 0; }
	sim=NULL;

	er=setupsim(root,fname,&sim,flags);
	if(!oflag && !pflag && !er) er=scmdopenfiles((cmdssptr) sim->cmds,wflag);
	if(pflag || er) {
    if(!qflag) printf("Simulation skipped\n"); }
  else {
		fflush(stdout);
		fflush(stderr);
		gl2glutInit(&argc,argv);
    if(tflag || !sim->graphss || sim->graphss->graphics==0) {
    	er=smolsimulate(sim);
    	endsimulate(sim,er); }
		else {
			smolsimulategl(sim); }}
	simfree(sim);
	simfuncfree();

	//cptr=getenv("OS");
	//if(cptr!=NULL && strstr(cptr,"Windows")) {			// true for a Windows system
	//	printf("Press ENTER to close window\n");
	//	getchar(); 
	//}  
	} catch (const char* errmsg) {
		fprintf(stderr, "%s\n", errmsg);
		exitCode = 1;
	} catch (...) {
		fprintf(stderr, "unknown error\n");
		exitCode = 1;
	}
	return exitCode; 
}
#else // VCELL
#ifndef VCELL_HYBRID
#include <VCELL/SimulationMessaging.h>
int taskID = -1;

int main(int argc,char *argv[]) {
	char errorMsg[2048];
	int returnCode = 0;
	
	LoggingCallback=NULL;
	try {
		SimulationMessaging::create();

  simptr sim;
  int i,er,pflag,qflag,wflag,tflag,Vflag,oflag;
  char root[STRCHAR],fname[STRCHAR],flags[STRCHAR],*cptr;

	for(i=0;i<STRCHAR;i++) root[i]=fname[i]=flags[i]='\0';
	er=0;
	if(argc<=1) {
		fprintf(stderr,"Welcome to Smoldyn version %s.\n\n",VERSION);
		fprintf(stderr,"Enter name of configuration file: ");
		fgets(root,STRCHAR,stdin);
		if(strchr(root,'\n')) *(strchr(root,'\n'))='\0';
		fprintf(stderr,"Enter runtime flags (q=quiet, p=parameters only), or '-'=none: ");
		fgets(flags,STRCHAR,stdin);
		if(strchr(flags,'\n')) *(strchr(flags,'\n'))='\0'; }
	if(argc>1) {
		strncpy(root,argv[1],STRCHAR-1);
		root[STRCHAR-1]='\0';
		argc--;
		argv++; }
	er=Parse_CmdLineArg(&argc,argv,NULL);
	if(er) {
		if(er==1) throw("Out of memory");
		else fprintf(stderr,"Follow command line '--define' options with key=replacement\n");
		return 0; }
	if(argc>1) {
		if(argv[1][0]=='-') {
			// -tid is always at the end of argument list
			// and it is used in conjunction with PBS
			// should not be used when called from command line manually
			if (strcmp(argv[1], "-tid")) {
				strncpy(flags,argv[1],STRCHAR-1);
				flags[STRCHAR-1]='\0';
				argc--;
				argv++; }
		}
		else {
			fprintf(stderr,"Command line format: smoldyn [config_file] [-options] [-OpenGL_options]\n");
			return 0; }}

	cptr=strrpbrk(root,":\\/");
	if(cptr) cptr++;
	else cptr=root;
	strcpy(fname,cptr);
	*cptr='\0';

	oflag=strchr(flags,'o')?1:0;
	pflag=strchr(flags,'p')?1:0;
	qflag=strchr(flags,'q')?1:0;
	Vflag=strchr(flags,'V')?1:0;
	if(!strcmp(fname,"-V")) Vflag=1;
	wflag=strchr(flags,'w')?1:0;
	tflag=strchr(flags,'t')?1:0;

	if(Vflag) {
		printf("%s\n",VERSION);
		return 0; }
	sim=NULL;

	if (argc > 1) {
		if (!strcmp(argv[1], "-tid")) {
			argc --;
			argv ++;

			sscanf(argv[1], "%d", &taskID);
			argc --;
			argv ++;
		}
	}

	er=setupsim(root,fname,&sim,flags);
	wflag = 1;
	if(!oflag && !pflag && !er) er=scmdopenfiles((cmdssptr)sim->cmds,wflag);
	if(pflag || er) {
    if(!qflag) printf("Simulation skipped\n"); }
  else {
		fflush(stdout);
		fflush(stderr);
    if(tflag || !sim->graphss || sim->graphss->graphics==0) {
    	er=smolsimulate(sim);
    	endsimulate(sim,er);
    	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_COMPLETED, 1.0, sim->time));	}
		else {
			gl2glutInit(&argc,argv);
			smolsimulategl(sim); }}
	simfree(sim);
	} catch (const char *exStr){
		strcpy(errorMsg, exStr);
		returnCode = 1;
	} catch (int stop) {
		// stopped by user
	}

	if (SimulationMessaging::getInstVar() == NULL) {
		if (returnCode != 0) {
			printfException("%s\n", errorMsg);
		}
	} else if (!SimulationMessaging::getInstVar()->isStopRequested()) {
		if (returnCode != 0) {
			SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_FAILURE, errorMsg));
		}
#ifdef USE_MESSAGING
		SimulationMessaging::getInstVar()->waitUntilFinished();
#endif
	}
	delete SimulationMessaging::getInstVar();
		
//	cptr=getenv("OS");
//	if(cptr!=NULL && strstr(cptr,"Windows")) {			// true for a Windows system
//		printf("Press ENTER to close window\n");
//		getchar(); }
  return returnCode; }
#else // VCELL_HYBRID
simptr smoldynInit(SimTool* simTool, string& fileName) {
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
	er=scmdopenfiles(sim->cmds,1);
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

double evaluateRnxRate(rxnptr reaction, simptr sim, int volIndex){
	int dim = sim -> dim;
	SimulationExpression* vcellSim = (SimulationExpression*)sim->simTool->getSimulation();
	WorldCoord wc = ((CartesianMesh*)vcellSim->getMesh())->getVolumeWorldCoord(volIndex);
	//vcellSim->setCurrentCoordinate(wc);
	double pos[3] = {wc.x, wc.y, wc.z};
	return evaluateRnxRate2(sim, reaction->rateExp, false, pos, NULL);
	/*int* indices = vcellSim->getIndices();
	indices[VAR_MEMBRANE_INDEX] = -1;
	indices[VAR_MEMBRANE_REGION_INDEX] = -1;
	indices[VAR_VOLUME_INDEX] = volIndex;
	indices[VAR_VOLUME_REGION_INDEX] = vcellSim->getMesh()->getVolumeElements()[volIndex].getRegionIndex();

	return rateExp->evaluateProxy();*/
}

double evaluateRnxRate2(simptr sim, Expression* rateExp, bool isMemRnx, double* pos, char* panelName)
{
	int dim = sim -> dim;
	Expression * rateExpression = rateExp;
	SimulationExpression* vcellSim = (SimulationExpression*)sim->simTool->getSimulation();
	int* indices = vcellSim->getIndices();
	WorldCoord wc(pos[0], dim>1?pos[1]:0, dim>2?pos[2]:0);
	vcellSim->setCurrentCoordinate(wc);
	
	if(isMemRnx)
	{
		if(panelName == NULL)
		{
			printfException("Unable to find membrane index(paneName == NULL). Cannot evaluate membrane reaction rate.");
		}
		//find membraneIndex
		string pName(panelName);
		size_t found = pName.find_last_of("_");
		int memIndex = atoi((pName.substr(found+1)).c_str());

		indices[VAR_MEMBRANE_INDEX] = memIndex;
		indices[VAR_MEMBRANE_REGION_INDEX] = vcellSim->getMesh()->getMembraneElements()[memIndex].getRegionIndex();
		indices[VAR_VOLUME_INDEX] = -1;
		indices[VAR_VOLUME_REGION_INDEX] = -1;
	}
	else
	{
		int volIndex = ((CartesianMesh*)vcellSim->getMesh())->getVolumeIndex(wc);
		int* indices = vcellSim->getIndices();
		indices[VAR_MEMBRANE_INDEX] = -1;
		indices[VAR_MEMBRANE_REGION_INDEX] = -1;
		indices[VAR_VOLUME_INDEX] = volIndex;
		indices[VAR_VOLUME_REGION_INDEX] = vcellSim->getMesh()->getVolumeElements()[volIndex].getRegionIndex();
	}
	return rateExpression->evaluateProxy();	
}

int randomPosInMesh(CartesianMesh* mesh, simptr sim, double* pos, int volIndex)
{
	int dim = sim -> dim;
	double deltaX, deltaY, deltaZ;
	double originX, originY, originZ;
	double domainSizeX, domainSizeY, domainSizeZ;
	WorldCoord wc = mesh -> getVolumeWorldCoord(volIndex);
	
	deltaX = mesh -> getXScale_um();
	originX = mesh -> getDomainOriginX();
	domainSizeX = mesh -> getDomainSizeX();
	pos[0]=unirandCCD(max(originX, (wc.x-0.5*deltaX)) , min((originX+domainSizeX), (wc.x + 0.5*deltaX)));
	if(dim > 1)
	{
		deltaY = mesh -> getYScale_um();
		originY = mesh -> getDomainOriginY();
		domainSizeY = mesh -> getDomainSizeY();
		pos[1]=unirandCCD(max(originY, (wc.y-0.5*deltaY)) , min((originY+domainSizeY), (wc.y + 0.5*deltaY)));
	}
	if(dim > 2)
	{
		deltaZ = mesh -> getZScale_um();
		originZ = mesh -> getDomainOriginZ();
		domainSizeZ = mesh -> getDomainSizeZ();
		pos[2]=unirandCCD(max(originZ, (wc.z-0.5*deltaZ)) , min((originZ+domainSizeZ), (wc.z + 0.5*deltaZ)));
	}

	return 0;
}


#endif
#endif
