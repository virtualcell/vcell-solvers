#ifndef HDF5Client_h
#define HDF5Client_h
#include <MPoint.h>
#include <map>
#include <iomanip>
#include <algo.h>
#include <Mesh.h>
#include <MeshElementSpecies.h>
#include <Logger.h>
#include <Timer.h>
#include <MovingBoundaryParabolicProblem.h>
#include <World.h>
#include <vhdf5/dataset.h>
#include <vhdf5/attribute.h>
#include <vhdf5/suppressor.h>
#include <vhdf5/facade.h>
#include <vhdf5/flex.h>
#include <vhdf5/file.h>
#include <vhdf5/vlen.h>
#pragma GCC diagnostic ignored "-Winvalid-offsetof"
namespace moving_boundary {

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
		/**
		* singleton function for var len type; must be method due to static initialization
		* dependencies
		*/ 
		static vcellH5::VarLen<PODPoint<T> > & vectorType( );
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
			static vcellH5::VarLen<PODPoint<double> > & vtype = PODPoint<double>::vectorType( );
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
	protected:
		static H5::CompType getType(size_t size) {
			using H5::PredType;
			H5::PredType dtype = vcellH5::TPredType<double>::predType( ); 
			H5::CompType resultPointType(size);
			resultPointType.insertMember("mass", offsetof(ResultPoint,mass),dtype);
			resultPointType.insertMember("volume", HOFFSET(ResultPoint,volume),dtype);
			resultPointType.insertMember("uNumeric", HOFFSET(ResultPoint,concentrationNumeric),dtype);
			resultPointType.insertMember("volumePoints", HOFFSET(ResultPoint,volumePoints),PODPoint<double>::vectorType( ).getType( ));
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
	struct NHDF5Client :public moving_boundary::MovingBoundaryClient {
		typedef World<CoordinateType,2> WorldType;
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

		static unsigned int calcReportStep(const moving_boundary::MovingBoundaryParabolicProblem &mbpp, 
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
		* @param baseName name of dataset in HDF5 file if not default
		* @param startTime_ when to start recording (time 0 always recorded)
		*/
		NHDF5Client(H5::H5File &f, 
			WorldType & world,
			const moving_boundary::MovingBoundaryParabolicProblem &mbpp, 
			unsigned int numberReports,
			const char *baseName = nullptr,
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
			baseGroup( ),
			elementDataset( ),
			worldDim( ),
			reportStep(calcReportStep(mbpp,startTime_,numberReports)),
			reportCounter(0),
			reportBegan(startTime_ == 0), //if beginning at zero, we've "begun" at the start
			//generationCounter(0),
			reportActive(true),
			timer( ),
			xconverter(world),
			yconverter(world),
			pointconverter(world) 
		{
			using spatial::cX;
			using spatial::cY;
			int numberGenerations = mbpp.numberTimeSteps( );
			timer.start( );
			{ //create group

				std::string groupName;
				if (baseName != nullptr) {
					groupName = baseName;
				}
				else {
					std::ostringstream oss;
					oss << "result-" << numberGenerations << '-' <<  meshDef.numCells(cX)  << '-' <<meshDef.numCells(cY); 
					groupName = oss.str( );
				}
				{
					vcellH5::Suppressor s; //no error message if not there
					H5Ldelete( file.getLocId(), groupName.c_str( ), H5P_DEFAULT );
				}
				std::cerr << "creating " << groupName << std::endl;
				baseGroup = file.createGroup(groupName);

				vcellH5::writeAttribute(baseGroup,"timeStep",timeStep);
				if (SOLUTION::validates) {
					const std::string s = SOLUTION::expression( ); 
					vcellH5::writeAttribute(baseGroup,"expression",s);
				}
			} //create group


			{ //create element dataset
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

				elementDataset = baseGroup.createDataSet( "elements", dataType, dataspace ,prop);
				const double startx = xconverter( meshDef.startCorner(spatial::cX) );
				const double starty = yconverter( meshDef.startCorner(spatial::cY) );
				const double hx = xconverter( meshDef.interval(spatial::cX) );
				const double hy = yconverter( meshDef.interval(spatial::cY) );
				vcellH5::writeAttribute(elementDataset,"startX",startx);
				vcellH5::writeAttribute(elementDataset,"startY",starty);
				vcellH5::writeAttribute(elementDataset,"numX",xSize);
				vcellH5::writeAttribute(elementDataset,"numY",ySize);
				vcellH5::writeAttribute(elementDataset,"hx",hx);
				vcellH5::writeAttribute(elementDataset,"hy",hy);
				const std::string layout("time x X x Y (transposed in MATLAB)");
				vcellH5::writeAttribute(elementDataset,"layout",layout);

				std::vector<moving_boundary::CoordinateType> xvalues = meshDef.coordinateValues(spatial::cX);
				std::vector<double> dv(xvalues.size( ));
				std::transform(xvalues.begin( ),xvalues.end( ),dv.begin( ),xconverter);
				vcellH5::SeqFacade<std::vector<double> > axisSF(dv); 
				vcellH5::facadeWriteAttribute(elementDataset,"xvalues",axisSF);

				std::vector<moving_boundary::CoordinateType> yvalues = meshDef.coordinateValues(spatial::cY);
				dv.resize(yvalues.size( )); 
				std::transform(yvalues.begin( ),yvalues.end( ),dv.begin( ),yconverter);
				vcellH5::facadeWriteAttribute(elementDataset,"yvalues",axisSF);
			} //create element dataset

			{ //create boundary dataset
				boundaryDim[0] = timeChunkSize;
				hsize_t     maxdim[1]= {H5S_UNLIMITED};
				H5::DataSpace dataspace(1,boundaryDim,maxdim); 

				H5::DSetCreatPropList  prop;
				hsize_t     chunkDim[1]  = {timeChunkSize};
				prop.setChunk(1, chunkDim);
				vcellH5::VarLen<PODPoint<double> > & vtype = PODPoint<double>::vectorType( );

				boundaryDataset = baseGroup.createDataSet( "boundaries", vtype.getType( ), dataspace ,prop);
			} //create boundary dataset
		}

		/**
		* add information from MovingBoundarySetup; currently just the concentration string
		*/
		void addInitial(const moving_boundary::MovingBoundarySetup & mbs) {
			const std::string s = mbs.concentrationFunctionStr; 
			vcellH5::writeAttribute(baseGroup,"concentrationFunction",s);
		}
		/**
		* free form annotation of data set for including notes in HDF file
		* @param attributeName
		* @param value 
		*/
		void annotate(const char *attributeName, const std::string & value) { 
			vcellH5::writeAttribute(elementDataset,attributeName,value);
		}

		virtual void time(double t, unsigned int generationCounter, bool last, const moving_boundary::GeometryInfo<moving_boundary::CoordinateType> & geometryInfo) { 
			reportActive = false;
			if (t == 0 || last|| t > startTime) {
				if (!reportBegan && t > 0) {
					std::cout << std::endl;
					reportBegan = true;
				}
				reportActive = (reportCounter%reportStep == 0) || last;
				if (reportActive) {
					writeBoundary(genTime.size( ),geometryInfo.boundary);
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
			//generationCounter++;
		}

		void writeBoundary(hsize_t timeIndex, const std::vector<spatial::TPoint<moving_boundary::CoordinateType,2> > & boundary) {
			try {
				std::vector<PODPoint<double> > outVector(boundary.size( ));
				std::transform(boundary.begin( ),boundary.end( ),outVector.begin( ),convertFrontToPOD);
				vcellH5::VarLen<PODPoint<double> > & vtype = PODPoint<double>::vectorType( );

				hvl_t variableBoundaryData = vtype.adapt(outVector);

				const hsize_t singleTimeSlice = 1;
				hsize_t  bufferDim[1] = {singleTimeSlice};
				H5::DataSpace memoryspace(1,bufferDim); 

				//is dataset big enough in time dimension?
				if (timeIndex >= boundaryDim[0]) {
					boundaryDim[0] += timeChunkSize;
					boundaryDataset.extend(boundaryDim);
				}

				hsize_t offset[1] = {timeIndex};
				H5::DataSpace dataspace = boundaryDataset.getSpace( );
				dataspace.selectHyperslab(H5S_SELECT_SET,bufferDim,offset);

				boundaryDataset.write(&variableBoundaryData,vtype.getType( ),memoryspace,dataspace);
			} catch (H5::Exception & e) {
				std::cerr << e.getDetailMsg( ) << std::endl;
			}
		}


		static PODPoint<double> convertFrontToPOD(const spatial::TPoint<moving_boundary::CoordinateType,2> &in) {
			PODPoint<double> p(in(spatial::cX),in(spatial::cY));
			//SCALE SCALE
			return p;
		};


		/**
		* state of inside / boundary nodes
		*/
		virtual void element(const moving_boundary::MeshElementSpecies &e) {
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
				double v = e.volumePD( );
				er.mass = m;
				er.concentration = c;
				er.volume = v;
				Volume2DClass::VectorOfVectors vOfv = e.getControlVolume().points( );
				if (vOfv.size( ) > 1) {
					//throw std::domain_error("multi region control volumes not supported yet");
					std::cerr << "multi region warning" << std::endl;
				}
				Volume2DClass::PointVector & pVec = vOfv.front( );
				er.controlVolume.resize(pVec.size( ));
				std::transform(pVec.begin( ),pVec.end( ),er.controlVolume.begin( ),pointconverter);

				totalStuff += m;
			}
		}
		/**
		* notify client they've received all elements
		*/
		virtual void iterationComplete( ) {
			if (reportActive) {
				VCELL_LOG(info,"Time " << currentTime << " total mass " << totalStuff); 

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
						elementDataset.extend(worldDim);
					}

					hsize_t offset[3] = {timeIndex ,minI,minJ};
					H5::DataSpace dataspace = elementDataset.getSpace( );
					dataspace.selectHyperslab(H5S_SELECT_SET,bufferDim,offset);

					H5::CompType dataType = SOLUTION::DataType::getType( ); 
					elementDataset.write(buffer.ptr( ),dataType,memoryspace,dataspace);
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
				if (oldStuff != 0 && !spatial::nearlyEqual(oldStuff,totalStuff,1e-3)) {
					simulationComplete( ); //write out final info
					VCELL_EXCEPTION(logic_error, "mass not conserved old"<< oldStuff << " , new " << totalStuff
						<< ", gain(+)/loss(-) " << (totalStuff - oldStuff));
				}
				oldStuff = totalStuff;
			}
		}

		void simulationComplete( ) {
			try {
				timer.stop( );
				const double totalTime = timer.elapsed( );
				vcellH5::primitiveWrite(baseGroup,"endTime",currentTime);
				vcellH5::primitiveWrite(baseGroup,"runTime",totalTime);
				unsigned int lastTimeIndex = static_cast<unsigned int>(genTime.size( ));
				vcellH5::primitiveWrite(baseGroup,"lastTimeIndex",lastTimeIndex); 

				vcellH5::SeqFacade<std::vector<double> > gt(genTime);
				vcellH5::facadeWrite(baseGroup,"generationTimes",gt);
			}
			catch (H5::Exception &e) {
				std::cerr << e.getDetailMsg( ) << std::endl;
			}
		}

	private:
		const moving_boundary::MovingBoundaryParabolicProblem &theProblem;

		H5::H5File & file;
		double startTime;
		double currentTime; 
		double totalStuff;
		double oldStuff;
		const spatial::MeshDef<moving_boundary::CoordinateType,2> meshDef;
		typedef std::map<spatial::TPoint<size_t,2>, HElementRecord> RecordMap; 
		RecordMap eRecords;
		std::vector<double> genTime;
		const double timeStep;
		vcellH5::Flex2<typename SOLUTION::DataType> buffer;
		H5::Group baseGroup; 
		H5::DataSet elementDataset;
		H5::DataSet boundaryDataset;
		hsize_t worldDim[3];
		hsize_t boundaryDim[1];
		const unsigned int reportStep;
		unsigned int reportCounter;
		bool reportBegan;
		//unsigned int generationCounter;
		bool reportActive;
		vcell_util::Timer timer;
		WorldType::XConverter xconverter;
		WorldType::YConverter yconverter;
		struct WorldToPODPointConverter{
			WorldToPODPointConverter(const World<CoordinateType,2> & w)
				:world(w) {}
			PODPoint<double> operator( )(spatial::TPoint<CoordinateType,2> &pt) const { 
				using spatial::cX;
				using spatial::cY;
				double x = world.toProblemDomain(pt(cX),cX);
				double y = world.toProblemDomain(pt(cY),cY);
				return PODPoint<double>(x,y);
			}
			const World<CoordinateType,2> & world;
		} pointconverter;
	};
}

#endif
