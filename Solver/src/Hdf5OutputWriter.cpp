#include <Hdf5OutputWriter.h>
#include <MPoint.h>
#include <map>
#include <set>
#include <iomanip>
#include <algo.h>
#include <Mesh.h>
#include <Logger.h>
#include <Mesh.h>
#include <MeshElementNode.h>
#include <MovingBoundaryParabolicProblem.h>
#include <vcellxml.h>
#include <ReportClient.h>
#include <Physiology.h>
#include <SimulationMessaging.h>
#include <TextReportClient.h>
#include <MBridge/MatlabDebug.h>
#include <MBridge/FronTierAdapt.h>
#include <MBridge/Figure.h>
using namespace H5;

Hdf5OutputWriter::Hdf5OutputWriter(std::string& a_xml, std::string& baseFileName, int steps, double interval,
	WorldType & world_,
	const moving_boundary::MovingBoundaryParabolicProblem &mbpp)
	: ReportClient(baseFileName),
		xml(a_xml),
		outputNumSteps(steps),
		outputTimeStep(interval),
	theProblem(mbpp),
	world(world_),
	lastReportIteration(0),
	bLastIter(false),
	solution(nullptr),
	solutionSize(0),
	pointconverter(world.pointConverter( ))
{
	logFileName = baseFileName + LOG_FILE_EXT;
	h5FileName = baseFileName + H5_FILE_EXT;
	init();
}

Hdf5OutputWriter::~Hdf5OutputWriter()
{
	delete[] solution;
}

void Hdf5OutputWriter::init()
{
	remove(logFileName.c_str());
	remove(h5FileName.c_str());

	h5File = H5File(h5FileName.c_str( ), H5F_ACC_TRUNC);

	numElements = theProblem.meshDef().numCells();
	numSpecies = theProblem.physiology().numberSpecies();
	solutionSize =  numElements * numSpecies;
	solution = new double[solutionSize];

	initMeshGroup();
	initTimesDataSet();
	initSolutionGroup();
}

void Hdf5OutputWriter::initTimesDataSet()
{
	int timesRank = 1;
	hsize_t timesDims = 10;
	hsize_t maxDims = H5S_UNLIMITED;
	DataSpace timesDataSpace(timesRank, &timesDims, &maxDims);
	// enable chunking
	DSetCreatPropList cparms;
	hsize_t chunkDims = 500;
	cparms.setChunk(timesRank, &chunkDims);
	int fill_val = -1;
	cparms.setFillValue(PredType::NATIVE_INT, &fill_val);
	// create dataset
	timesDataSet = h5File.createDataSet(DataSet_Times, PredType::NATIVE_DOUBLE, timesDataSpace, cparms);
}

void Hdf5OutputWriter::initSolutionGroup()
{
	DataSpace attributeDataSpace(H5S_SCALAR);
	StrType attributeStrType(0, 64);
	// solution group
	solutionGroup = h5File.createGroup(Group_Solution);

	// size
	Attribute attribute = solutionGroup.createAttribute("size", PredType::NATIVE_INT, attributeDataSpace);
	attribute.write(PredType::NATIVE_INT, &numElements);

	// add variables as attributes
	for (int s = 0; s < theProblem.physiology().numberSpecies(); ++ s)
	{
		char attrName[64];
		char attrValue[64];

		//write name and unit
		sprintf(attrName, "Variable_%d", s);
		Attribute attribute = solutionGroup.createAttribute(attrName, attributeStrType, attributeDataSpace);
		sprintf(attrValue, "%s", theProblem.physiology().species(s)->name().c_str());
		attribute.write(attributeStrType, attrValue);
	}
}

void Hdf5OutputWriter::initMeshGroup()
{
	// mesh group
	meshGroup = h5File.createGroup(Group_Mesh);
	const int geoDimension = theProblem.meshDef().numDim();

	// dim
	{
	hsize_t hdf5Rank = 1;
	hsize_t hdf5Dims = 1;
	DataSpace dataspace(hdf5Rank, &hdf5Dims);
	DataSet dataSet = meshGroup.createDataSet("dimension", PredType::NATIVE_INT, dataspace);
	dataSet.write(&geoDimension, PredType::NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT);
	}

	// size
	{
	const int* N = theProblem.mesh().Nx().data();
	hsize_t hdf5Rank = geoDimension;
	hsize_t hdf5Dims[geoDimension] = {1, 2};
	DataSpace dataspace = DataSpace(hdf5Rank, hdf5Dims);
	DataSet dataSet = meshGroup.createDataSet("size", PredType::NATIVE_INT, dataspace);
	dataSet.write(N, PredType::NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT);
	}

	// origin
	{
	const double* origin = theProblem.mesh().geoOrigin().data();
	hsize_t hdf5Rank = 2;  // lo, hi
	hsize_t hdf5Dims[2] = {1, 2};
	DataSpace dataspace = DataSpace(hdf5Rank, hdf5Dims);
	DataSet dataSet = meshGroup.createDataSet("origin", PredType::NATIVE_DOUBLE, dataspace);
	dataSet.write(origin, PredType::NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT);
	}

	// extent
	{
	const double* extentX = theProblem.setup().extentX.data();
	const double* extentY = theProblem.setup().extentY.data();
	hsize_t hdf5Rank = 2; // lo, hi
	hsize_t hdf5Dims[2] = {1, 2};
	double data[geoDimension][2] = { extentX[1] - extentX[0], extentY[1] - extentY[0]	};
	DataSpace dataspace = DataSpace(hdf5Rank, hdf5Dims);
	DataSet dataSet = meshGroup.createDataSet("extent", PredType::NATIVE_DOUBLE, dataspace);
	dataSet.write(data, PredType::NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT);
	}
}

bool Hdf5OutputWriter::shouldReport()
{
	return currentIter == 0 || bLastIter || (outputNumSteps != -1 ? currentIter % outputNumSteps == 0 : std::abs(currentTime - timeList.back() - outputTimeStep) < 1e-8);
}

void Hdf5OutputWriter::time(double t, unsigned int numIter, bool last, const moving_boundary::GeometryInfo<moving_boundary::CoordinateType> & geometryInfo) {
		currentTime = t;
		currentIter = numIter;
		bLastIter = last;

		if (shouldReport())
		{
			int timesRank = 1;
			hsize_t timesDims = 1;
			hsize_t maxDims = H5S_UNLIMITED;
			DataSpace timesDataSpace(timesRank, &timesDims, &maxDims);

			// write current time
			hsize_t size = timeList.size() + 1;
			timesDataSet.extend(&size);
			hsize_t dim = 1;
			hsize_t offset = timeList.size();
			DataSpace fspace = timesDataSet.getSpace();
			fspace.selectHyperslab(H5S_SELECT_SET, &dim, &offset);
			timesDataSet.write(&currentTime, PredType::NATIVE_DOUBLE, timesDataSpace, fspace);

			writeSolution();
			writeLog();
			timeList.push_back(currentTime);
			SimulationMessaging::getInstVar()->setWorkerEvent(new WorkerEvent(JOB_DATA, currentTime/theProblem.endTime(), currentTime));
		}
}

void Hdf5OutputWriter::writeSolution()
{
	std::memset(solution, 0, solutionSize * sizeof(double));
	int elementIndex = 0;
	for (spatial::Mesh<moving_boundary::CoordinateType, 2, moving_boundary::MeshElementNode>::const_iterator iter = theProblem.mesh().begin();
			iter != theProblem.mesh().end( ); ++ iter, ++ elementIndex)
	{
		const moving_boundary::MeshElementNode &e = *iter;
		const double* concValues = e.priorConcentrations();
		if (concValues != nullptr)
		{
			for (int i = 0; i < numSpecies; ++ i)
			{
				solution[i * numElements + elementIndex] = concValues[i];
			}
		}
	}

	char dataSetName[128];
	sprintf(dataSetName, "time%06d", timeList.size());

	// create dataspace
	hsize_t hdf5Rank = 1;
	hsize_t hdf5Dims = solutionSize;
	DataSpace dataspace(hdf5Rank, &hdf5Dims);
	DataSet dataSet = solutionGroup.createDataSet(dataSetName, PredType::NATIVE_DOUBLE, dataspace);

	// time attribute
	DataSpace attributeDataSpace(H5S_SCALAR);
	Attribute attribute = dataSet.createAttribute("time", PredType::NATIVE_DOUBLE, attributeDataSpace);
	attribute.write(PredType::NATIVE_DOUBLE, &currentTime);

	// write dataset
	dataSet.write(solution, PredType::NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT);
}

void Hdf5OutputWriter::writeLog()
{
	std::ofstream logOf(logFileName.c_str(), ios_base::app);
	if (currentIter == 0)
	{
		logOf << MBSDATA_IDENTIFIER << std::endl;
	}
	logOf << currentIter << " " << h5FileName << " " << currentTime << std::endl;
	logOf.close();
}

void Hdf5OutputWriter::element(const moving_boundary::MeshElementNode &e) {
	// do everything in time function
}

void Hdf5OutputWriter::iterationComplete( )
{
	// do everything in time function
}

void Hdf5OutputWriter::simulationComplete( )
{
}



