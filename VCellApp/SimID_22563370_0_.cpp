//---------------------------------------------
//  SimID_22563370_0_.C
//---------------------------------------------

#ifdef WIN32
#include <Windows.h>
#else
#include <UnixDefs.h>
#endif

#include "SimID_22563370_0_.h"
#include <assert.h>
#include <math.h>
#include <stdlib.h>
#include <VCELL/SimTypes.h>
#include <VCELL/VolumeVariable.h>
#include <VCELL/MembraneVariable.h>
#include <VCELL/Feature.h>
#include <VCELL/CartesianMesh.h>
#include <VCELL/SimTool.h>
#include <VCELL/ODESolver.h>
#include <VCELL/DataSet.h>
#include <VCELL/EqnBuilderReactionDiffusion.h>
#include <VCELL/EqnBuilderReactionForward.h>
#include <VCELL/MembraneEqnBuilderForward.h>
#include <VCELL/Contour.h>
#include <VCELL/Element.h>
#include <VCELL/PdeSolverDiana.h>
#include <VCELL/Region.h>
#include <VCELL/VolumeRegionEqnBuilder.h>
#include <VCELL/MembraneRegionEqnBuilder.h>
#include <VCELL/EqnBuilderReactionDiffusionConvection.h>
#include <VCELL/MembraneEqnBuilderDiffusion.h>
#include <VCELL/SparseLinearSolver.h>
#include <VCELL/SparseVolumeEqnBuilder.h>
#include <VCELL/SimulationMessaging.h>
#include <VCELL/FieldData.h>

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

SimTool* getSimTool();
int vcellExit(int returnCode, char* returnMsg) {
	if (!SimTool::getInstance()->isStopped()) {
		if (returnCode != 0) {
			SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_FAILURE, returnMsg));
		}
#ifdef USE_MESSAGING
		SimulationMessaging::getInstVar()->waitUntilFinished();
#endif
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
#ifdef USE_MESSAGING
		jint taskID = -1;
#else
		int taskID = -1;
#endif
		bool bSimZip = true;
		for (int i = 1; i < argc; i ++) {
			if (!strcmp(argv[i], "-nz")) {
				bSimZip = false;
			} else if (!strcmp(argv[i], "-d")) {
				i ++;
				outputPath = argv[i];
			} else {
				for (int j = 0; j < (int)strlen(argv[i]); j ++) {
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
#ifdef USE_MESSAGING
			char* broker = "tcp://code:2506";
			char *smqusername = "serverUser";
			char *password = "cbittech";
			char *qname = "workerEventDev";
			char* tname = "serviceControlDev";
			char* vcusername = "fgao";
			jint simKey = 22563370;
			jint jobIndex = 0;
			SimulationMessaging::create(broker, smqusername, password, qname, tname, vcusername, simKey, jobIndex, taskID);
#endif
		}
		SimulationMessaging::getInstVar()->start(); // start the thread

		SimTool *pSimTool = getSimTool();
		if (bSimZip == false) {
			SimTool::getInstance()->requestNoZip();
		}
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

	char tempString[1024];

	SimTool::create();
	sprintf(tempString, "%s%cSimID_22563370_0_\0", outputPath, DIRECTORY_SEPARATOR);
	SimTool::getInstance()->setBaseFilename(tempString);
	SimTool::getInstance()->setTimeStep(0.01);
	SimTool::getInstance()->setEndTimeSec(0.03);
	SimTool::getInstance()->setKeepEvery(1);

	VCellModel* model = new UserVCellModel();
	SimTool::getInstance()->setModel(model);

	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_STARTING, "initializing mesh..."));
	sprintf(tempString, "%s%cSimID_22563370_0_.vcg\0", outputPath, DIRECTORY_SEPARATOR);
	CartesianMesh* mesh = new CartesianMesh(tempString);
	SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_STARTING, "mesh initialized"));

	Simulation* sim = new UserSimulation(mesh);
	SimTool::getInstance()->setSimulation(sim);
	sim->initSimulation();
	SimTool::getInstance()->loadFinal();   // initializes to the latest file if it exists
	return SimTool::getInstance();
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
#ifdef USE_PDESOLVERDIANA
	PdeSolverDiana    *pdeSolver;
#endif
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
	int regionCount;

	int symmflg = 1;    // define symmflg = 0 (general) or 1 (symmetric)
   volumeVar = new VolumeVariable(sizeX,sizeY,sizeZ,string("DEX"),string("uM"));
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

}

//---------------------------------------------
//  class UserVCellModel
//---------------------------------------------
UserVCellModel::UserVCellModel()
: VCellModel()
{
	addFeature(new Featurecytosol(string("cytosol"),200));
	addFeature(new Featureextracellular(string("extracellular"),101));
}

//---------------------------------------------
//  class Featurecytosol
//---------------------------------------------
Featurecytosol::Featurecytosol(string& Aname, int priority)
: Feature(Aname, 0, priority)
{
	addVolumeVarContext(new VolumeVarContextcytosolDEX(this, string("DEX")));
}

//---------------------------------------------
//  class VolumeVarContextcytosolDEX
//---------------------------------------------
VolumeVarContextcytosolDEX::VolumeVarContextcytosolDEX(Feature *Afeature, string& AspeciesName)
: VolumeVarContext(Afeature,AspeciesName)
{
   initialValue = new double;
   *initialValue = 2.0;
   diffusionRate = new double;
   *diffusionRate = 20.0;

}

bool VolumeVarContextcytosolDEX::resolveReferences(Simulation *sim)
{
   if (!VolumeVarContext::resolveReferences(sim)){
      return false;
   }
   ASSERTION(sim);

   return true;
}


double VolumeVarContextcytosolDEX::getReactionRate(long volumeIndex)
{
   return 0.0;
}


void VolumeVarContextcytosolDEX::getFlux(MembraneElement *element,double *inFlux, double *outFlux)
{
   // for this membrane, MathDescription defines inside='cytosol', outside='extracellular'
   // 'cytosol' has priority=200, 'extracellular' has priority=101
   // :-)  Priorities are consistent (insidePriority > outsidePriority)

   *inFlux = 2.0;
   *outFlux = -2.0;
}


//---------------------------------------------
//  class Featureextracellular
//---------------------------------------------
Featureextracellular::Featureextracellular(string& Aname, int priority)
: Feature(Aname, 1, priority)
{
	addVolumeVarContext(new VolumeVarContextextracellularDEX(this, string("DEX")));
}

//---------------------------------------------
//  class VolumeVarContextextracellularDEX
//---------------------------------------------
VolumeVarContextextracellularDEX::VolumeVarContextextracellularDEX(Feature *Afeature, string& AspeciesName)
: VolumeVarContext(Afeature,AspeciesName)
{
   initialValue = new double;
   *initialValue = 1.0;
   diffusionRate = new double;
   *diffusionRate = 20.0;

}

bool VolumeVarContextextracellularDEX::resolveReferences(Simulation *sim)
{
   if (!VolumeVarContext::resolveReferences(sim)){
      return false;
   }
   ASSERTION(sim);

   return true;
}


double VolumeVarContextextracellularDEX::getReactionRate(long volumeIndex)
{
   return 0.0;
}


void VolumeVarContextextracellularDEX::getFlux(MembraneElement *element,double *inFlux, double *outFlux)
{
   *inFlux = 0.0;
   *outFlux = 0.0;
}



