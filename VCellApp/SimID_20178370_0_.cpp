//---------------------------------------------
//  SimID_20178370_0_.C
//---------------------------------------------

#ifdef WIN32
#include <Windows.h>
#else
#include <UnixDefs.h>
#endif

#include "SimID_20178370_0_.h"
#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include <VCELL/SimTypes.h>
#include <VCELL/Variable.h>
#include <VCELL/Feature.h>
#include <VCELL/Mesh.h>
#include <VCELL/SimTool.h>
#include <VCELL/Solver.h>
#include <VCELL/DataSet.h>
#include <VCELL/EqnBuilderReactionDiffusion.h>
#include <VCELL/EqnBuilderReactionForward.h>
#include <VCELL/MembraneEqnBuilderForward.h>
#include <VCELL/App.h>
#include <VCELL/Contour.h>
#include <VCELL/Element.h>
#include <VCELL/PdeSolverDiana.h>
#include <VCELL/Region.h>
#include <VCELL/RegionEqnBuilder.h>
#include <VCELL/EqnBuilderReactionDiffusionConvection.h>
#include <VCELL/MembraneEqnBuilderDiffusion.h>
#include <VCELL/SparseLinearSolver.h>
#include <VCELL/SparseVolumeEqnBuilder.h>
#include <VCELL/SimulationMessaging.h>

#define abs(x)      fabs(x)

//---------------------------------------------
//  main routine
//---------------------------------------------
#include <sys/stat.h>
#ifdef WIN32
#define DIRECTORY_SEPARATOR '\\'
#else
#define DIRECTORY_SEPARATOR '/'
#endif
static char* outputPath = "\\\\SAN2\\raid\\Vcell\\users\\fgao";

#ifndef VCELL_CORBA
//-------------------------------------------
//   BATCH (NON-CORBA) IMPLEMENTATION
//-------------------------------------------

#ifdef VCELL_MPI
#include <mpi.h>
#endif

int vcellExit(int returnCode, char* returnMsg) {
	if (!SimTool::bStopSimulation) {
		if (returnCode != 0) {
			SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_FAILURE, returnMsg));
		}
		SimulationMessaging::getInstVar()->waitUntilFinished();
	}
	return returnCode;
}
int main(int argc, char *argv[])
{

#ifdef VCELL_MPI
	int ierr = MPI_Init(&argc,&argv);
	assert(ierr == MPI_SUCCESS);
#endif

	int returnCode = 0;
	string returnMsg;
	try {
		jint taskID = -1;
		for (int i = 1; i < argc; i ++) {
			if (!strcmp(argv[i], "-nz")) {
				SimTool::bSimZip = false;
			} else if (!strcmp(argv[i], "-d")) {
				i ++;
				outputPath = argv[i];
			} else {
				for (int j = 0; j < strlen(argv[i]); j ++) {
					if (argv[i][j] < '0' || argv[i][j] > '9') {
						cout << "Wrong argument : " << argv[i] << endl;
						cout << "Arguments : [-d output] [-nz] [taskID]" <<  endl;
						exit(1);
					}
				}
				taskID = atoi(argv[i]);
			}
		}
		struct stat buf;
		if (stat(outputPath, &buf)) {
			cerr << "Output directory [" << outputPath <<"] doesn't exist" << endl;
			exit(1);
		}
		if (taskID == -1) { // no messaging
			SimulationMessaging::create();
		} else {
			char* broker = "tcp://code:2506";
			char *smqusername = "serverUser";
			char *password = "cbittech";
			char *qname = "workerEventDev";
			char* tname = "serviceControlDev";
			char* vcusername = "fgao";
			jint simKey = 20178370;
			jint jobIndex = 0;
			SimulationMessaging::create(broker, smqusername, password, qname, tname, vcusername, simKey, jobIndex, taskID);
		}
		SimulationMessaging::getInstVar()->start(); // start the thread
		SimTool *pSimTool = getSimTool();
		pSimTool->start();
	}catch (const char *exStr){
		returnMsg = "Exception while running : ";
		returnMsg += exStr;
		returnCode = 1;
	}catch (...){
		returnMsg = "Unknown exception while running ... ";
		returnCode = 1;
	}
#ifdef VCELL_MPI
	MPI_Finalize();
#endif
	return vcellExit(returnCode, (char*)returnMsg.c_str());
}
#else  // end not VCELL_CORBA
//-------------------------------------------
//   CORBA IMPLEMENTATION
//-------------------------------------------
#include <OB/CORBA.h>
#include <OB/Util.h>

#include <Simulation_impl.h>

#include <stdlib.h>
#include <errno.h>

#ifdef HAVE_FSTREAM
#   include <fstream>
#else
#   include <fstream.h>
#endif

int main(int argc, char* argv[], char*[])
{
    try
    {
	//
	// Create ORB and BOA
	//
	CORBA_ORB_var orb = CORBA_ORB_init(argc, argv);
	CORBA_BOA_var boa = orb -> BOA_init(argc, argv);
	
	orb->conc_model(CORBA_ORB::ConcModelThreaded);
	boa->conc_model(CORBA_BOA::ConcModelThreadPool);
	boa->conc_model_thread_pool(4);
	
	//
	// Create implementation object
	//
	mathService_Simulation_var p = new Simulation_impl(getSimTool());
	
	//
	// Save reference
	//
	CORBA_String_var s = orb -> object_to_string(p);
	
	const char* refFile = "Simulation.ref";
	ofstream out(refFile);
	if(out.fail())
	{
	    cerr << argv[0] << ": can't open `" << refFile << "': "
		 << strerror(errno) << endl;
	    return 1;
	}
	
	out << s << endl;
	out.close();
	
	//
	// Run implementation
	//
	boa -> impl_is_ready(CORBA_ImplementationDef::_nil());
    }
    catch(CORBA_SystemException& ex)
    {
	OBPrintException(ex);
	return 1;
    }

    return 0;
}

#endif // end VCELL_CORBA



SimTool *getSimTool()
{

	Simulation *sim = NULL;
	VCellModel *model = NULL;
	CartesianMesh *mesh = NULL;
	SimTool *pSimTool = new SimTool("Simulate");
	int numX = 39;
	int numY = 140;
	int numZ = 1;
	theApplication = new App();
	model = new UserVCellModel();
	assert(model);
	theApplication->setModel(model);
	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_STARTING, "initializing mesh..."));
	char tempString[1024];
	sprintf(tempString, "%s%cSimID_20178370_0_.vcg\0", outputPath, DIRECTORY_SEPARATOR);
	mesh = new CartesianMesh(tempString);
	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_STARTING, "mesh initialized"));
	assert(mesh);
	sim = new UserSimulation(mesh);
	assert(sim);
	theApplication->setSimulation(sim);

	sim->initSimulation();
	pSimTool->setSimulation(sim);
	sprintf(tempString, "%s%cSimID_20178370_0_\0", outputPath, DIRECTORY_SEPARATOR);
	pSimTool->setBaseFilename(tempString);
	pSimTool->loadFinal();   // initializes to the latest file if it exists

	pSimTool->setTimeStep(0.0010);
	pSimTool->setEndTimeSec(0.01);
	pSimTool->setKeepEvery(1);

	return pSimTool;
}

//---------------------------------------------
//  class UserSimulation
//---------------------------------------------
UserSimulation::UserSimulation(CartesianMesh *mesh)
: Simulation(mesh)
{
VolumeRegionVariable		*volumeRegionVar;
MembraneRegionVariable		*membraneRegionVar;
VolumeVariable    *volumeVar;
MembraneVariable  *membraneVar;
ContourVariable   *contourVar;
// ImplicitPDESolver *pdeSolver;
PdeSolverDiana    *pdeSolver;
ODESolver         *odeSolver;
SparseLinearSolver    *slSolver;
EqnBuilder        *builder;
SparseMatrixEqnBuilder        *smbuilder;
long sizeX = mesh->getNumVolumeX();
long sizeY = mesh->getNumVolumeY();
long sizeZ = mesh->getNumVolumeZ();
int numSolveRegions;
int *solveRegions;
int numVolumeRegions = mesh->getNumVolumeRegions();
int i;
int regionCount;

	int symmflg = 1;    // define symmflg = 0 (general) or 1 (symmetric)
   volumeVar = new VolumeVariable(sizeX,sizeY,sizeZ,"CaBPB","uM");
    // solving for all regions
    numSolveRegions = 0;  // flag specifying to solve for all regions
    solveRegions = NULL;
#ifdef USE_PDESOLVERDIANA
	symmflg = 1;    // define symmflg = 0 (general) or 1 (symmetric)
	pdeSolver = new PdeSolverDiana(volumeVar,mesh,symmflg,numSolveRegions,solveRegions,false);
	builder = new EqnBuilderReactionDiffusion(volumeVar,mesh,pdeSolver);
	pdeSolver->setEqnBuilder(builder);
	addSolver(pdeSolver);
#else
	smbuilder = new SparseVolumeEqnBuilder(volumeVar,mesh,true, numSolveRegions, solveRegions);
	slSolver = new SparseLinearSolver(volumeVar,smbuilder,false);
	addSolver(slSolver);
#endif
   addVariable(volumeVar);

   volumeVar = new VolumeVariable(sizeX,sizeY,sizeZ,"Ca","uM");
    // solving for all regions
    numSolveRegions = 0;  // flag specifying to solve for all regions
    solveRegions = NULL;
#ifdef USE_PDESOLVERDIANA
	symmflg = 1;    // define symmflg = 0 (general) or 1 (symmetric)
	pdeSolver = new PdeSolverDiana(volumeVar,mesh,symmflg,numSolveRegions,solveRegions,false);
	builder = new EqnBuilderReactionDiffusion(volumeVar,mesh,pdeSolver);
	pdeSolver->setEqnBuilder(builder);
	addSolver(pdeSolver);
#else
	smbuilder = new SparseVolumeEqnBuilder(volumeVar,mesh,true, numSolveRegions, solveRegions);
	slSolver = new SparseLinearSolver(volumeVar,smbuilder,false);
	addSolver(slSolver);
#endif
   addVariable(volumeVar);

   volumeVar = new VolumeVariable(sizeX,sizeY,sizeZ,"CaBP","uM");
    // solving for all regions
    numSolveRegions = 0;  // flag specifying to solve for all regions
    solveRegions = NULL;
#ifdef USE_PDESOLVERDIANA
	symmflg = 1;    // define symmflg = 0 (general) or 1 (symmetric)
	pdeSolver = new PdeSolverDiana(volumeVar,mesh,symmflg,numSolveRegions,solveRegions,false);
	builder = new EqnBuilderReactionDiffusion(volumeVar,mesh,pdeSolver);
	pdeSolver->setEqnBuilder(builder);
	addSolver(pdeSolver);
#else
	smbuilder = new SparseVolumeEqnBuilder(volumeVar,mesh,true, numSolveRegions, solveRegions);
	slSolver = new SparseLinearSolver(volumeVar,smbuilder,false);
	addSolver(slSolver);
#endif
   addVariable(volumeVar);

   volumeVar = new VolumeVariable(sizeX,sizeY,sizeZ,"CaB","uM");
    // solving for all regions
    numSolveRegions = 0;  // flag specifying to solve for all regions
    solveRegions = NULL;
   //odeSolver = new ODESolver(volumeVar,mesh);
   odeSolver = new ODESolver(volumeVar,mesh,numSolveRegions,solveRegions);
   builder = new EqnBuilderReactionForward(volumeVar,mesh,odeSolver);
   odeSolver->setEqnBuilder(builder);
   addSolver(odeSolver);
   addVariable(volumeVar);

}

//---------------------------------------------
//  class UserVCellModel
//---------------------------------------------
UserVCellModel::UserVCellModel()
: VCellModel()
{
   addFeature(new FeatureNucleus("Nucleus",300));
   addFeature(new FeatureCytosol("Cytosol",201));
   addFeature(new FeatureExtraCellular("ExtraCellular",102));
}

//---------------------------------------------
//  class FeatureNucleus
//---------------------------------------------
FeatureNucleus::FeatureNucleus(char *Aname, int priority)
: Feature(Aname, 0, priority)
{
   addVolumeVarContext(new VolumeVarContextNucleusCaB(this,"CaB"));
   addVolumeVarContext(new VolumeVarContextNucleusCa(this,"Ca"));
   addVolumeVarContext(new VolumeVarContextNucleusCaBP(this,"CaBP"));
   addVolumeVarContext(new VolumeVarContextNucleusCaBPB(this,"CaBPB"));
   fastSystem = new FastSystemNucleus();
}

//---------------------------------------------
//  class VolumeVarContextNucleusCaB
//---------------------------------------------
VolumeVarContextNucleusCaB::VolumeVarContextNucleusCaB(Feature *Afeature,CString AspeciesName)
: VolumeVarContext(Afeature,AspeciesName)
{
   initialValue = new double;
   *initialValue = 0.0;
   diffusionRate = NULL;

}

boolean VolumeVarContextNucleusCaB::resolveReferences(Simulation *sim)
{
   if (!VolumeVarContext::resolveReferences(sim)){
      return FALSE;
   }
   ASSERTION(sim);

   return TRUE;
}


double VolumeVarContextNucleusCaB::getReactionRate(long volumeIndex)
{
   return 0.0;
}


void VolumeVarContextNucleusCaB::getFlux(MembraneElement *element,double *inFlux, double *outFlux)
{
   *inFlux = 0.0;
   *outFlux = 0.0;
}


//---------------------------------------------
//  class VolumeVarContextNucleusCa
//---------------------------------------------
VolumeVarContextNucleusCa::VolumeVarContextNucleusCa(Feature *Afeature,CString AspeciesName)
: VolumeVarContext(Afeature,AspeciesName)
{
   initialValue = new double;
   *initialValue = 0.1;
   diffusionRate = new double;
   *diffusionRate = 300.0;

    var_Ca = NULL;
}

boolean VolumeVarContextNucleusCa::resolveReferences(Simulation *sim)
{
   if (!VolumeVarContext::resolveReferences(sim)){
      return FALSE;
   }
   ASSERTION(sim);

   var_Ca = (VolumeVariable*)sim->getVariableFromName("Ca");
   if (var_Ca==NULL){
      printf("could not resolve 'Ca'\n");
      return FALSE;
   }

   return TRUE;
}


double VolumeVarContextNucleusCa::getReactionRate(long volumeIndex)
{
   return 0.0;
}


void VolumeVarContextNucleusCa::getFlux(MembraneElement *element,double *inFlux, double *outFlux)
{
   // for this membrane, MathDescription defines inside='Nucleus', outside='Cytosol'
   // 'Nucleus' has priority=300, 'Cytosol' has priority=201
   // :-)  Priorities are consistent (insidePriority > outsidePriority)

   double Ca_OUTSIDE = mesh->getOutsideOld(var_Ca,element);
   double Ca_INSIDE = mesh->getInsideOld(var_Ca,element);
   *inFlux = (150.0 * (Ca_OUTSIDE - Ca_INSIDE));
   *outFlux =  - (150.0 * (Ca_OUTSIDE - Ca_INSIDE));
}


//---------------------------------------------
//  class VolumeVarContextNucleusCaBP
//---------------------------------------------
VolumeVarContextNucleusCaBP::VolumeVarContextNucleusCaBP(Feature *Afeature,CString AspeciesName)
: VolumeVarContext(Afeature,AspeciesName)
{
   initialValue = new double;
   *initialValue = 202.83;
   diffusionRate = new double;
   *diffusionRate = 60.0;

    var_CaBP = NULL;
}

boolean VolumeVarContextNucleusCaBP::resolveReferences(Simulation *sim)
{
   if (!VolumeVarContext::resolveReferences(sim)){
      return FALSE;
   }
   ASSERTION(sim);

   var_CaBP = (VolumeVariable*)sim->getVariableFromName("CaBP");
   if (var_CaBP==NULL){
      printf("could not resolve 'CaBP'\n");
      return FALSE;
   }

   return TRUE;
}


double VolumeVarContextNucleusCaBP::getReactionRate(long volumeIndex)
{
   return 0.0;
}


void VolumeVarContextNucleusCaBP::getFlux(MembraneElement *element,double *inFlux, double *outFlux)
{
   // for this membrane, MathDescription defines inside='Nucleus', outside='Cytosol'
   // 'Nucleus' has priority=300, 'Cytosol' has priority=201
   // :-)  Priorities are consistent (insidePriority > outsidePriority)

   double CaBP_OUTSIDE = mesh->getOutsideOld(var_CaBP,element);
   double CaBP_INSIDE = mesh->getInsideOld(var_CaBP,element);
   *inFlux = (30.0 * (CaBP_OUTSIDE - CaBP_INSIDE));
   *outFlux =  - (30.0 * (CaBP_OUTSIDE - CaBP_INSIDE));
}


//---------------------------------------------
//  class VolumeVarContextNucleusCaBPB
//---------------------------------------------
VolumeVarContextNucleusCaBPB::VolumeVarContextNucleusCaBPB(Feature *Afeature,CString AspeciesName)
: VolumeVarContext(Afeature,AspeciesName)
{
   initialValue = new double;
   *initialValue = 47.17;
   diffusionRate = new double;
   *diffusionRate = 60.0;

    var_CaBPB = NULL;
}

boolean VolumeVarContextNucleusCaBPB::resolveReferences(Simulation *sim)
{
   if (!VolumeVarContext::resolveReferences(sim)){
      return FALSE;
   }
   ASSERTION(sim);

   var_CaBPB = (VolumeVariable*)sim->getVariableFromName("CaBPB");
   if (var_CaBPB==NULL){
      printf("could not resolve 'CaBPB'\n");
      return FALSE;
   }

   return TRUE;
}


double VolumeVarContextNucleusCaBPB::getReactionRate(long volumeIndex)
{
   return 0.0;
}


void VolumeVarContextNucleusCaBPB::getFlux(MembraneElement *element,double *inFlux, double *outFlux)
{
   // for this membrane, MathDescription defines inside='Nucleus', outside='Cytosol'
   // 'Nucleus' has priority=300, 'Cytosol' has priority=201
   // :-)  Priorities are consistent (insidePriority > outsidePriority)

   double CaBPB_OUTSIDE = mesh->getOutsideOld(var_CaBPB,element);
   double CaBPB_INSIDE = mesh->getInsideOld(var_CaBPB,element);
   *inFlux = (30.0 * (CaBPB_OUTSIDE - CaBPB_INSIDE));
   *outFlux =  - (30.0 * (CaBPB_OUTSIDE - CaBPB_INSIDE));
}


//---------------------------------------------
//  class FastSystemNucleus
//---------------------------------------------
FastSystemNucleus::FastSystemNucleus()
: FastSystem(1,2)
{
   setTolerance(1e-7);
   
   pVars[0] = var_CaBPB = NULL;

   pDependentVars[0] = var_CaBP = NULL;
   pDependentVars[1] = var_Ca = NULL;

   __C2 = 0.0;
   __C3 = 0.0;
}

boolean FastSystemNucleus::resolveReferences(Simulation *sim)
{
   ASSERTION(sim);
   this->mesh = sim->getMesh();
	this->simulation = sim;

   var_CaBPB = sim->getVariableFromName("CaBPB");
   if (var_CaBPB==NULL){
      printf("could not resolve 'CaBPB'\n");
      return FALSE;
   }
   pVars[0] = var_CaBPB;

   var_CaBP = sim->getVariableFromName("CaBP");
   if (var_CaBP==NULL){
      printf("could not resolve 'CaBP'\n");
      return FALSE;
   }
   pDependentVars[0] = var_CaBP;

   var_Ca = sim->getVariableFromName("Ca");
   if (var_Ca==NULL){
      printf("could not resolve 'Ca'\n");
      return FALSE;
   }
   pDependentVars[1] = var_Ca;

   return TRUE;
}


void FastSystemNucleus::initVars()
{
   double CaBPB = var_CaBPB->getCurr(currIndex);
   setX(0,CaBPB);
   double CaBP = var_CaBP->getCurr(currIndex);
   double Ca = var_Ca->getCurr(currIndex);
   __C2 = (Ca + CaBPB);
   __C3 = (CaBP + CaBPB);
}


void FastSystemNucleus::updateDependentVars()
{
   double CaBPB = getX(0);
   var_CaBP->setCurr(currIndex,( - CaBPB + __C3));
   var_Ca->setCurr(currIndex,( - CaBPB + __C2));
}


void FastSystemNucleus::updateMatrix()
{
   double CaBPB = getX(0);
   setMatrix(0, 0, (-8.6 - (20.0 * ( - CaBPB + __C2)) - (20.0 * ( - CaBPB + __C3))));
   setMatrix(0, 1,  - ((20.0 * ( - CaBPB + __C3) * ( - CaBPB + __C2)) - (8.6 * CaBPB)));

}



//---------------------------------------------
//  class FeatureCytosol
//---------------------------------------------
FeatureCytosol::FeatureCytosol(char *Aname, int priority)
: Feature(Aname, 1, priority)
{
   addVolumeVarContext(new VolumeVarContextCytosolCaB(this,"CaB"));
   addVolumeVarContext(new VolumeVarContextCytosolCaBPB(this,"CaBPB"));
   addVolumeVarContext(new VolumeVarContextCytosolCa(this,"Ca"));
   addVolumeVarContext(new VolumeVarContextCytosolCaBP(this,"CaBP"));
   fastSystem = new FastSystemCytosol();
}

//---------------------------------------------
//  class VolumeVarContextCytosolCaB
//---------------------------------------------
VolumeVarContextCytosolCaB::VolumeVarContextCytosolCaB(Feature *Afeature,CString AspeciesName)
: VolumeVarContext(Afeature,AspeciesName)
{
   initialValue = new double;
   *initialValue = 3.96;
   diffusionRate = NULL;

}

boolean VolumeVarContextCytosolCaB::resolveReferences(Simulation *sim)
{
   if (!VolumeVarContext::resolveReferences(sim)){
      return FALSE;
   }
   ASSERTION(sim);

   return TRUE;
}


double VolumeVarContextCytosolCaB::getReactionRate(long volumeIndex)
{
   return 0.0;
}


void VolumeVarContextCytosolCaB::getFlux(MembraneElement *element,double *inFlux, double *outFlux)
{
   *inFlux = 0.0;
   *outFlux = 0.0;
}


//---------------------------------------------
//  class VolumeVarContextCytosolCaBPB
//---------------------------------------------
VolumeVarContextCytosolCaBPB::VolumeVarContextCytosolCaBPB(Feature *Afeature,CString AspeciesName)
: VolumeVarContext(Afeature,AspeciesName)
{
   initialValue = new double;
   *initialValue = 47.17;
   diffusionRate = new double;
   *diffusionRate = 60.0;

    var_CaBPB = NULL;
}

boolean VolumeVarContextCytosolCaBPB::resolveReferences(Simulation *sim)
{
   if (!VolumeVarContext::resolveReferences(sim)){
      return FALSE;
   }
   ASSERTION(sim);

   var_CaBPB = (VolumeVariable*)sim->getVariableFromName("CaBPB");
   if (var_CaBPB==NULL){
      printf("could not resolve 'CaBPB'\n");
      return FALSE;
   }

   return TRUE;
}


double VolumeVarContextCytosolCaBPB::getReactionRate(long volumeIndex)
{
   return 0.0;
}


void VolumeVarContextCytosolCaBPB::getFlux(MembraneElement *element,double *inFlux, double *outFlux)
{
   // for this membrane, MathDescription defines inside='Cytosol', outside='ExtraCellular'
   // 'Cytosol' has priority=201, 'ExtraCellular' has priority=102
   // :-)  Priorities are consistent (insidePriority > outsidePriority)

   *inFlux = 0.0;
   *outFlux = 0.0;
}


//---------------------------------------------
//  class VolumeVarContextCytosolCa
//---------------------------------------------
VolumeVarContextCytosolCa::VolumeVarContextCytosolCa(Feature *Afeature,CString AspeciesName)
: VolumeVarContext(Afeature,AspeciesName)
{
   initialValue = new double;
   *initialValue = 0.1;
   diffusionRate = new double;
   *diffusionRate = 300.0;

    var_Ca = NULL;
}

boolean VolumeVarContextCytosolCa::resolveReferences(Simulation *sim)
{
   if (!VolumeVarContext::resolveReferences(sim)){
      return FALSE;
   }
   ASSERTION(sim);

   var_Ca = (VolumeVariable*)sim->getVariableFromName("Ca");
   if (var_Ca==NULL){
      printf("could not resolve 'Ca'\n");
      return FALSE;
   }

   return TRUE;
}


double VolumeVarContextCytosolCa::getReactionRate(long volumeIndex)
{
   return 0.0;
}


void VolumeVarContextCytosolCa::getFlux(MembraneElement *element,double *inFlux, double *outFlux)
{
   // for this membrane, MathDescription defines inside='Cytosol', outside='ExtraCellular'
   // 'Cytosol' has priority=201, 'ExtraCellular' has priority=102
   // :-)  Priorities are consistent (insidePriority > outsidePriority)

   double Ca_OUTSIDE = mesh->getOutsideOld(var_Ca,element);
   double Ca_INSIDE = mesh->getInsideOld(var_Ca,element);
   WorldCoord wc = mesh->getMembraneWorldCoord(element);
   double y = wc.y;
   double x = wc.x;
   *inFlux = (((((y < 2.23) && (x > 1.34) && (x < 11.61)) ? (0.0070 * (Ca_OUTSIDE - Ca_INSIDE) / (0.5 + Ca_INSIDE)) : 0.0)) - ((((y >= 57.59) && (Ca_INSIDE > 0.1)) ? (2857.1428571428573 * (-0.1 + Ca_INSIDE) / (0.25 + Ca_INSIDE)) : 0.0)));
   *outFlux = ( - ((((y < 2.23) && (x > 1.34) && (x < 11.61)) ? (0.0070 * (Ca_OUTSIDE - Ca_INSIDE) / (0.5 + Ca_INSIDE)) : 0.0)) + ((((y >= 57.59) && (Ca_INSIDE > 0.1)) ? (2857.1428571428573 * (-0.1 + Ca_INSIDE) / (0.25 + Ca_INSIDE)) : 0.0)));
}


//---------------------------------------------
//  class VolumeVarContextCytosolCaBP
//---------------------------------------------
VolumeVarContextCytosolCaBP::VolumeVarContextCytosolCaBP(Feature *Afeature,CString AspeciesName)
: VolumeVarContext(Afeature,AspeciesName)
{
   initialValue = new double;
   *initialValue = 202.83;
   diffusionRate = new double;
   *diffusionRate = 60.0;

    var_CaBP = NULL;
}

boolean VolumeVarContextCytosolCaBP::resolveReferences(Simulation *sim)
{
   if (!VolumeVarContext::resolveReferences(sim)){
      return FALSE;
   }
   ASSERTION(sim);

   var_CaBP = (VolumeVariable*)sim->getVariableFromName("CaBP");
   if (var_CaBP==NULL){
      printf("could not resolve 'CaBP'\n");
      return FALSE;
   }

   return TRUE;
}


double VolumeVarContextCytosolCaBP::getReactionRate(long volumeIndex)
{
   return 0.0;
}


void VolumeVarContextCytosolCaBP::getFlux(MembraneElement *element,double *inFlux, double *outFlux)
{
   // for this membrane, MathDescription defines inside='Cytosol', outside='ExtraCellular'
   // 'Cytosol' has priority=201, 'ExtraCellular' has priority=102
   // :-)  Priorities are consistent (insidePriority > outsidePriority)

   *inFlux = 0.0;
   *outFlux = 0.0;
}


//---------------------------------------------
//  class FastSystemCytosol
//---------------------------------------------
FastSystemCytosol::FastSystemCytosol()
: FastSystem(2,2)
{
   setTolerance(1e-7);
   
   pVars[0] = var_CaBPB = NULL;
   pVars[1] = var_CaB = NULL;

   pDependentVars[0] = var_Ca = NULL;
   pDependentVars[1] = var_CaBP = NULL;

   __C0 = 0.0;
   __C1 = 0.0;
}

boolean FastSystemCytosol::resolveReferences(Simulation *sim)
{
   ASSERTION(sim);
   this->mesh = sim->getMesh();
	this->simulation = sim;

   var_CaBPB = sim->getVariableFromName("CaBPB");
   if (var_CaBPB==NULL){
      printf("could not resolve 'CaBPB'\n");
      return FALSE;
   }
   pVars[0] = var_CaBPB;

   var_CaB = sim->getVariableFromName("CaB");
   if (var_CaB==NULL){
      printf("could not resolve 'CaB'\n");
      return FALSE;
   }
   pVars[1] = var_CaB;

   var_Ca = sim->getVariableFromName("Ca");
   if (var_Ca==NULL){
      printf("could not resolve 'Ca'\n");
      return FALSE;
   }
   pDependentVars[0] = var_Ca;

   var_CaBP = sim->getVariableFromName("CaBP");
   if (var_CaBP==NULL){
      printf("could not resolve 'CaBP'\n");
      return FALSE;
   }
   pDependentVars[1] = var_CaBP;

   return TRUE;
}


void FastSystemCytosol::initVars()
{
   double CaBPB = var_CaBPB->getCurr(currIndex);
   setX(0,CaBPB);
   double CaB = var_CaB->getCurr(currIndex);
   setX(1,CaB);
   double Ca = var_Ca->getCurr(currIndex);
   double CaBP = var_CaBP->getCurr(currIndex);
   __C0 = (CaBPB + CaBP);
   __C1 = ( - CaBP + CaB + Ca);
}


void FastSystemCytosol::updateDependentVars()
{
   double CaBPB = getX(0);
   double CaB = getX(1);
   var_Ca->setCurr(currIndex,(__C1 + __C0 - CaBPB - CaB));
   var_CaBP->setCurr(currIndex,( - CaBPB + __C0));
}


void FastSystemCytosol::updateMatrix()
{
   double CaBPB = getX(0);
   double CaB = getX(1);
   setMatrix(0, 0, ( - (-8.6 - (20.0 * ( - CaBPB + __C0)) - (20.0 * (__C1 + __C0 - CaBPB - CaB))) + (0.1 * (400.0 - CaB))));
   setMatrix(0, 1, ((20.0 * ( - CaBPB + __C0)) - (-1.0 - (0.1 * (__C1 + __C0 - CaBPB - CaB)) - (0.1 * (400.0 - CaB)))));
   setMatrix(0, 2,  - ( - ((20.0 * (__C1 + __C0 - CaBPB - CaB) * ( - CaBPB + __C0)) - (8.6 * CaBPB)) - ((0.1 * (400.0 - CaB) * (__C1 + __C0 - CaBPB - CaB)) - CaB)));

   setMatrix(1, 0,  - (-8.6 - (20.0 * ( - CaBPB + __C0)) - (20.0 * (__C1 + __C0 - CaBPB - CaB))));
   setMatrix(1, 1, (20.0 * ( - CaBPB + __C0)));
   setMatrix(1, 2, ((20.0 * (__C1 + __C0 - CaBPB - CaB) * ( - CaBPB + __C0)) - (8.6 * CaBPB)));

}



//---------------------------------------------
//  class FeatureExtraCellular
//---------------------------------------------
FeatureExtraCellular::FeatureExtraCellular(char *Aname, int priority)
: Feature(Aname, 2, priority)
{
   addVolumeVarContext(new VolumeVarContextExtraCellularCaB(this,"CaB"));
   addVolumeVarContext(new VolumeVarContextExtraCellularCa(this,"Ca"));
   addVolumeVarContext(new VolumeVarContextExtraCellularCaBPB(this,"CaBPB"));
   addVolumeVarContext(new VolumeVarContextExtraCellularCaBP(this,"CaBP"));
}

//---------------------------------------------
//  class VolumeVarContextExtraCellularCaB
//---------------------------------------------
VolumeVarContextExtraCellularCaB::VolumeVarContextExtraCellularCaB(Feature *Afeature,CString AspeciesName)
: VolumeVarContext(Afeature,AspeciesName)
{
   initialValue = new double;
   *initialValue = 0.0;
   diffusionRate = NULL;

}

boolean VolumeVarContextExtraCellularCaB::resolveReferences(Simulation *sim)
{
   if (!VolumeVarContext::resolveReferences(sim)){
      return FALSE;
   }
   ASSERTION(sim);

   return TRUE;
}


double VolumeVarContextExtraCellularCaB::getReactionRate(long volumeIndex)
{
   return 0.0;
}


void VolumeVarContextExtraCellularCaB::getFlux(MembraneElement *element,double *inFlux, double *outFlux)
{
   *inFlux = 0.0;
   *outFlux = 0.0;
}


//---------------------------------------------
//  class VolumeVarContextExtraCellularCa
//---------------------------------------------
VolumeVarContextExtraCellularCa::VolumeVarContextExtraCellularCa(Feature *Afeature,CString AspeciesName)
: VolumeVarContext(Afeature,AspeciesName)
{
   initialValue = NULL;
   diffusionRate = NULL;

    var_Ca = NULL;
}

boolean VolumeVarContextExtraCellularCa::resolveReferences(Simulation *sim)
{
   if (!VolumeVarContext::resolveReferences(sim)){
      return FALSE;
   }
   ASSERTION(sim);

   var_Ca = (VolumeVariable*)sim->getVariableFromName("Ca");
   if (var_Ca==NULL){
      printf("could not resolve 'Ca'\n");
      return FALSE;
   }

   return TRUE;
}


double VolumeVarContextExtraCellularCa::getReactionRate(long volumeIndex)
{
   return 0.0;
}


void VolumeVarContextExtraCellularCa::getFlux(MembraneElement *element,double *inFlux, double *outFlux)
{
   *inFlux = 0.0;
   *outFlux = 0.0;
}

double VolumeVarContextExtraCellularCa::getInitialValue(long volumeIndex)
{
   WorldCoord wc = mesh->getVolumeWorldCoord(volumeIndex);
   double y = wc.y;
   return (((((y < 0.89)) ? (1000.0) : 0.0)) + ((((y >= 0.89)) ? (1000.0) : 0.0)));
}

double VolumeVarContextExtraCellularCa::getDiffusionRate(long volumeIndex)
{
   WorldCoord wc = mesh->getVolumeWorldCoord(volumeIndex);
   double y = wc.y;
   return (300.0 * ((y < 2.23) + (y > 4.0)));
}


//---------------------------------------------
//  class VolumeVarContextExtraCellularCaBPB
//---------------------------------------------
VolumeVarContextExtraCellularCaBPB::VolumeVarContextExtraCellularCaBPB(Feature *Afeature,CString AspeciesName)
: VolumeVarContext(Afeature,AspeciesName)
{
   initialValue = new double;
   *initialValue = 0.0;
   diffusionRate = new double;
   *diffusionRate = 60.0;

}

boolean VolumeVarContextExtraCellularCaBPB::resolveReferences(Simulation *sim)
{
   if (!VolumeVarContext::resolveReferences(sim)){
      return FALSE;
   }
   ASSERTION(sim);

   return TRUE;
}


double VolumeVarContextExtraCellularCaBPB::getReactionRate(long volumeIndex)
{
   return 0.0;
}


void VolumeVarContextExtraCellularCaBPB::getFlux(MembraneElement *element,double *inFlux, double *outFlux)
{
   *inFlux = 0.0;
   *outFlux = 0.0;
}


//---------------------------------------------
//  class VolumeVarContextExtraCellularCaBP
//---------------------------------------------
VolumeVarContextExtraCellularCaBP::VolumeVarContextExtraCellularCaBP(Feature *Afeature,CString AspeciesName)
: VolumeVarContext(Afeature,AspeciesName)
{
   initialValue = new double;
   *initialValue = 0.0;
   diffusionRate = new double;
   *diffusionRate = 60.0;

}

boolean VolumeVarContextExtraCellularCaBP::resolveReferences(Simulation *sim)
{
   if (!VolumeVarContext::resolveReferences(sim)){
      return FALSE;
   }
   ASSERTION(sim);

   return TRUE;
}


double VolumeVarContextExtraCellularCaBP::getReactionRate(long volumeIndex)
{
   return 0.0;
}


void VolumeVarContextExtraCellularCaBP::getFlux(MembraneElement *element,double *inFlux, double *outFlux)
{
   *inFlux = 0.0;
   *outFlux = 0.0;
}



