#ifndef HDF5Client_h
#define HDF5Client_h
#include <TPoint.h>
#include <map>
#include <iomanip>
#include <algo.h>
#include <Mesh.h>
#include <MeshElementSpecies.h>
#include <Logger.h>
#include <Timer.h>
#include <MovingBoundaryParabolicProblem.h>
#include <vhdf5/dataset.h>
#include <vhdf5/attribute.h>
#include <vhdf5/suppressor.h>
#include <vhdf5/facade.h>
#include <vhdf5/flex.h>
#include <vhdf5/file.h>
#include <vhdf5/vlen.h>
#pragma GCC diagnostic ignored "-Winvalid-offsetof"
namespace spatial {

	/**
	* Plain old data point type
	*/
	template <class T>
	struct PODPoint {
		T x;
		T y;
		PODPoint(T x_ = 0, T y_ = 0)
			:x(x_),
			y(y_) {}
		static H5::DataType getType( ) {
			H5::CompType pointType( sizeof (PODPoint) );
			pointType.insertMember("x",HOFFSET(PODPoint,x),vcellH5::TPredType<T>::predType( ));
			pointType.insertMember("y",HOFFSET(PODPoint,y),vcellH5::TPredType<T>::predType( ));
			return pointType;
		}
	};

	struct HElementRecord {
		double volume;
		double mass;
		double concentration;
		//double lastPosition;
		double x;
		double y;
		std::vector<PODPoint<double> > controlVolume;
		HElementRecord(double x_ = 0, double y_ = 0)
			//:lastPosition(),
			:volume(),
			mass(),
			concentration(),
			x(x_),
			y(y_),
			controlVolume( ){ }
		void clear( ) {
			volume = mass = concentration = 0;
		}
	};


	/**
	* aggregates results for Hdf5 writing
	*/
	struct ResultPoint {
		ResultPoint( )
			:mass(0),
			volume(0),
			concentrationNumeric(0),
			volumePoints( )
		{ }
		void set(const spatial::TPoint<size_t,2> & idx, const HElementRecord & er, double time)
		{
			static vcellH5::VarLen<PODPoint<double> > & vtype = vectorType( );
			mass = er.mass;
			volume = er.volume;
			concentrationNumeric = er.concentration;
			volumePoints = vtype.adapt(er.controlVolume);
		}
		double mass;
		double volume;
		double concentrationNumeric;
		hvl_t  volumePoints; 
		static H5::CompType getType( ) {
			static H5::CompType ct = getType(sizeof(ResultPoint));
			return ct;
		}
		/**
		* singleton function for var len type; must be method due to static initialization
		* dependencies
		*/ 
		static vcellH5::VarLen<PODPoint<double> > & vectorType( );
	protected:
		static H5::CompType getType(size_t size) {
			using H5::PredType;
			H5::PredType dtype = vcellH5::TPredType<double>::predType( ); 
			H5::CompType resultPointType(size);
			resultPointType.insertMember("mass", offsetof(ResultPoint,mass),dtype);
			resultPointType.insertMember("volume", HOFFSET(ResultPoint,volume),dtype);
			resultPointType.insertMember("uNumeric", HOFFSET(ResultPoint,concentrationNumeric),dtype);
			resultPointType.insertMember("volumePoints", HOFFSET(ResultPoint,volumePoints),vectorType( ).getType( ));
			return resultPointType;
		}
	};

	/**
	* adds exact solution and error information; 
	*/
	template <class SOLUTION>
	struct TSolutionPoint : public ResultPoint {
		TSolutionPoint( ) 
			:ResultPoint( ),
			concentrationExact(0),
			error(0) {}

		void set(const spatial::TPoint<size_t,2> & idx, const HElementRecord & er, double time)
		{
			ResultPoint::set(idx,er,time);
			const double sol = SOLUTION::solution(er.x,er.y,time);
			if (concentrationNumeric != 0) {
				concentrationExact = sol;
				error = std::abs(concentrationNumeric - sol);
			} 
			else {
				concentrationExact = 0;
				error = 0; 
			}
		}
		double concentrationExact;
		double error;

		static H5::CompType getType( ) {
			H5::PredType dtype = vcellH5::TPredType<double>::predType( ); 
			H5::CompType solutionPointType = ResultPoint::getType(sizeof(TSolutionPoint<SOLUTION>));
			solutionPointType.insertMember("uExact", HOFFSET(TSolutionPoint<SOLUTION>,concentrationExact),dtype);
			solutionPointType.insertMember("error", HOFFSET(TSolutionPoint<SOLUTION>,error),dtype);
			return solutionPointType;
		}
	};



	struct NoSolution {
		typedef ResultPoint DataType;
		const static bool validates = false;
		static double solution(double,double,double) { return 0; }
		static std::string expression( ) { throw std::domain_error("unsupported"); } 
	};

	template <class SOLUTION = NoSolution> 
	struct NHDF5Client :public spatial::MovingBoundaryClient {
		/**
		* HDF5 spatial chunk size (x and y dimensions)
		*/
		static const size_t spatialChunkSize = 10;
		/**
		* HDF5 spatial chunk size (time dimension)
		*/
		static const size_t timeChunkSize = 50;

		/**
		* worldDim index
		*/
		static const size_t timeArrayIndex = 0; 

		/**
		* worldDim index
		*/
		static const size_t xArrayIndex = 1; 

		/**
		* worldDim index
		*/
		static const size_t yArrayIndex = 2; 

		static unsigned int calcReportStep(const spatial::MovingBoundaryParabolicProblem &mbpp, 
			double startTime,
			unsigned int numberReports) {
				unsigned int nts = mbpp.numberTimeSteps( );
				unsigned int rs = nts / numberReports;
				if (startTime >0) { //if not beginning, scale based on fraction of time reported
					const double end = mbpp.endTime( );
					unsigned int scaled = static_cast<unsigned int>(nts * (end -startTime) / end);
					rs = scaled / numberReports; 
				}
				if (rs > 0) {
					return rs;
				}
				return 1;
		}

		/**
		* @param f file to write to
		* @param mbpp the problem 
		* @param numberReports number time steps to record
		* @param datasetName name of dataset in HDF5 file if not default
		* @param startTime_ when to start recording (time 0 always recorded)
		*/
		NHDF5Client(H5::H5File &f, 
			const spatial::MovingBoundaryParabolicProblem &mbpp, 
			unsigned int numberReports,
			const char *datasetName = nullptr,
			const double startTime_ = 0) 
			:file(f),
			startTime(startTime_),
			theProblem(mbpp), //temp
			currentTime(0),
			totalStuff(0),
			oldStuff(0),
			meshDef(mbpp.meshDef( )),
			eRecords(),
			genTime(),
			timeStep(mbpp.baseTimeStep( )),
			buffer(),
			dataset( ),
			worldDim( ),
			reportStep(calcReportStep(mbpp,startTime_,numberReports)),
			reportCounter(0),
			reportBegan(startTime_ == 0), //if beginning at zero, we've "begun" at the start
			generationCounter(0),
			reportActive(true)
		{
			int numberGenerations = mbpp.numberTimeSteps( );
			timer.start( );
			using spatial::cX;
			using spatial::cY;

			std::string dsName;
			if (datasetName != nullptr) {
				dsName = datasetName;
			}
			else
				if (datasetName == nullptr) {
					std::ostringstream oss;
					oss << "result-" << numberGenerations << '-' <<  meshDef.numCells(cX)  << '-' <<meshDef.numCells(cY); 
					dsName = oss.str( );
				}

				const size_t xSize = meshDef.numCells(cX);
				const size_t ySize = meshDef.numCells(cY);
				worldDim[timeArrayIndex] = timeChunkSize;
				worldDim[xArrayIndex] = xSize; 
				worldDim[yArrayIndex] = ySize; 
				hsize_t     maxdim[3]= {H5S_UNLIMITED,xSize,ySize};
				H5::DataSpace dataspace(3,worldDim,maxdim); 

				H5::DSetCreatPropList  prop;
				hsize_t     chunkDim[3]  = {timeChunkSize,spatialChunkSize,spatialChunkSize};
				prop.setChunk(3, chunkDim);
				H5::CompType dataType = SOLUTION::DataType::getType( ); 

				{
					vcellH5::Suppressor s; //no error message if not there
					H5Ldelete( file.getLocId(), dsName.c_str( ), H5P_DEFAULT );
				}

				dataset = file.createDataSet( dsName, dataType, dataspace ,prop);
				std::cerr << "creating " << dsName << std::endl;
				{ //metadata setting via HDF attributes
					const double startx = meshDef.startCorner(spatial::cX);
					const double starty = meshDef.startCorner(spatial::cY);
					const double hx = meshDef.interval(spatial::cX);
					const double hy = meshDef.interval(spatial::cY);
					vcellH5::writeAttribute(dataset,"startX",startx);
					vcellH5::writeAttribute(dataset,"startY",starty);
					vcellH5::writeAttribute(dataset,"numX",xSize);
					vcellH5::writeAttribute(dataset,"numY",ySize);
					vcellH5::writeAttribute(dataset,"hx",hx);
					vcellH5::writeAttribute(dataset,"hy",hy);
					vcellH5::writeAttribute(dataset,"timeStep",timeStep);
					const std::string layout("time x X x Y (transposed in MATLAB)");
					vcellH5::writeAttribute(dataset,"layout",layout);
					if (SOLUTION::validates) {
						const std::string s = SOLUTION::expression( ); 
						vcellH5::writeAttribute(dataset,"expression",s);
					}
					std::vector<double> xvalues = meshDef.coordinateValues(spatial::cX);
					std::vector<double> yvalues = meshDef.coordinateValues(spatial::cY);
					vcellH5::SeqFacade<std::vector<double> > xv(xvalues); 
					vcellH5::facadeWriteAttribute(dataset,"xvalues",xv);
					vcellH5::SeqFacade<std::vector<double> > yv(yvalues); 
					vcellH5::facadeWriteAttribute(dataset,"yvalues",yv);
				}
		}

		/**
		* add information from MovingBoundarySetup; currently just the concentration string
		*/
		void addInitial(const spatial::MovingBoundarySetup & mbs) {
			const std::string s = mbs.concentrationFunctionStr; 
			vcellH5::writeAttribute(dataset,"concentrationFunction",s);
		}
		/**
		* free form annotation of data set for including notes in HDF file
		* @param attributeName
		* @param value 
		*/
		void annotate(const char *attributeName, const std::string & value) { 
			vcellH5::writeAttribute(dataset,attributeName,value);
		}

		virtual void time(double t, bool last, const GeometryInfo<double> & geometryInfo) { 
			reportActive = false;
			if (t == 0 || last|| t > startTime) {
				if (!reportBegan && t > 0) {
					std::cout << std::endl;
					reportBegan = true;
				}
				reportActive = (reportCounter%reportStep == 0) || last;
				if (reportActive) {
					currentTime = t;
					totalStuff = 0;
					std::cout << "generation " << std::setw(2) <<  generationCounter << " time " << currentTime << std::endl;
					genTime.push_back(t);
				}
				reportCounter++;
			}
			else {
				std::cout << '.';
			}
			generationCounter++;
		}

		static PODPoint<double> cf(const TPoint<double,2> &in) {
			PODPoint<double> p(in(cX),in(cY));
			return p;
		};


		/**
		* state of inside / boundary nodes
		*/
		virtual void element(const spatial::MeshElementSpecies<double,1> &e) {
			if (reportActive) {
				using spatial::cX;
				using spatial::cY;
				spatial::TPoint<size_t,2> key(e.indexOf(0),e.indexOf(1));
				if (eRecords.find(key) == eRecords.end( )) {
					HElementRecord newRecord(e(cX),e(cY));
					eRecords[key] = newRecord;
				}
				HElementRecord & er = eRecords[key];
				double m = e.mass(0);
				double c = e.concentration(0);
				double v = e.volume( );
				er.mass = m;
				er.concentration = c;
				er.volume = v;
				Volume<double,2>::VectorOfVectors vOfv = e.getControlVolume(meshDef).points( );
				if (vOfv.size( ) > 1) {
					throw std::domain_error("multi region control volumes not supported yet");
				}
				Volume<double,2>::PointVector & pVec = vOfv.front( );
				er.controlVolume.resize(pVec.size( ));
				std::transform(pVec.begin( ),pVec.end( ),er.controlVolume.begin( ),cf);
				
				totalStuff += m;
			}
		}
		/**
		* notify client they've received all elements
		*/
		virtual void iterationComplete( ) {
			if (reportActive) {
				VCELL_LOG(info,"Time " << currentTime << " total mass " << totalStuff); 
				if (oldStuff != 0 && !spatial::nearlyEqual(oldStuff,totalStuff,1e-3)) {
					VCELL_EXCEPTION(logic_error, "mass not conserved old"<< oldStuff << " , new " << totalStuff);
				}
				oldStuff = totalStuff;

				try {
					//determine size of buffer needed for current generation
					size_t minI, maxI, minJ, maxJ;
					minI = minJ = std::numeric_limits<size_t>::max( );
					maxI = maxJ = std::numeric_limits<size_t>::min( );
					for (RecordMap::const_iterator iter = eRecords.begin( ); iter != eRecords.end( ); ++iter) {
						size_t i = iter->first(spatial::cX);
						size_t j = iter->first(spatial::cY);
						minI = std::min(minI,i);
						minJ = std::min(minJ,j);
						maxI = std::max(maxI,i);
						maxJ = std::max(maxJ,j);

					}
					const size_t iSpan = maxI - minI + 1;
					const size_t jSpan = maxJ - minJ + 1;
					buffer.reindex(iSpan,jSpan);

					size_t timeIndex = genTime.size( ) - 1;

					for (RecordMap::iterator iter = eRecords.begin( ); iter != eRecords.end( ); ++iter) {
						HElementRecord & er = iter->second;
						const spatial::TPoint<size_t,2> & index = iter->first;
						hsize_t i = index(spatial::cX);
						hsize_t j = index(spatial::cY); 
						buffer[i - minI][j - minJ].set(index,er,currentTime);
						er.clear( );
					}
					const size_t singleTimeSlice = 1;
					hsize_t  bufferDim[3] = {singleTimeSlice,iSpan, jSpan}; 
					H5::DataSpace memoryspace(3,bufferDim); 

					//is dataset big enough in time dimension?
					if (timeIndex >= worldDim[timeArrayIndex]) {
						worldDim[timeArrayIndex] += timeChunkSize;
						dataset.extend(worldDim);
					}

					hsize_t offset[3] = {timeIndex ,minI,minJ};
					H5::DataSpace dataspace = dataset.getSpace( );
					dataspace.selectHyperslab(H5S_SELECT_SET,bufferDim,offset);

					H5::CompType dataType = SOLUTION::DataType::getType( ); 
					dataset.write(buffer.ptr( ),dataType,memoryspace,dataspace);
				}

				catch (H5::Exception &e) {
					std::cerr << e.getDetailMsg( ) << std::endl;
				}
				/*
				std::ofstream fa("finalareas.m");
				std::ofstream fp("finalpolygons.m");
				theProblem.plotAreas(fa);
				theProblem.plotPolygons(fp);
				*/
			}
		}

		void simulationComplete( ) {
			try {
				timer.stop( );
				const double totalTime = timer.elapsed( );
				vcellH5::writeAttribute(dataset,"endTime",currentTime, true);
				vcellH5::writeAttribute(dataset,"runTime",totalTime, true);
				unsigned int lastTimeIndex = static_cast<unsigned int>(genTime.size( ));
				vcellH5::writeAttribute(dataset,"lastTimeIndex",lastTimeIndex, true);

				vcellH5::SeqFacade<std::vector<double> > gt(genTime);
				vcellH5::facadeWriteAttribute(dataset,"generationTimes",gt);
			}
			catch (H5::Exception &e) {
				std::cerr << e.getDetailMsg( ) << std::endl;
			}
		}

	private:
		const spatial::MovingBoundaryParabolicProblem &theProblem;

		H5::H5File & file;
		double startTime;
		double currentTime; 
		double totalStuff;
		double oldStuff;
		const spatial::MeshDef<double,2> meshDef;
		typedef std::map<spatial::TPoint<size_t,2>, HElementRecord> RecordMap; 
		RecordMap eRecords;
		std::vector<double> genTime;
		const double timeStep;
		vcellH5::Flex2<typename SOLUTION::DataType> buffer;
		H5::DataSet dataset;
		hsize_t worldDim[3];
		const unsigned int reportStep;
		unsigned int reportCounter;
		bool reportBegan;
		unsigned int generationCounter;
		bool reportActive;
		vcell_util::Timer timer;
	};
}

#endif
