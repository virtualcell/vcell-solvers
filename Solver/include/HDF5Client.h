#ifndef HDF5Client_h
#define HDF5Client_h
#include <MPoint.h>
#include <map>
#include <set>
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
#include <vhdf5/exception.h>
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
		PODPoint(const spatial::TPoint<double,2> & in)
			:x(in(spatial::cX)),
			y(in(spatial::cY)) {}
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

	class TimeReport {
		double startTime;
	public:
		static const unsigned int STD_PRIORITY = 10;
		TimeReport(double startTime_)
			:startTime(startTime_) {}
		double getStartTime( ) const {
			return startTime;
		}
		/**
		* allow object to expired itself
		* @return false (default)
		*/
		virtual bool expired(double time, int generation) const {
			return false;
		}
		/**
		* return relative priority for objects with same start time. Lower is higher priority
		* @returns 10
		*/
		virtual unsigned int priority( ) const {
			return STD_PRIORITY;
		}
		virtual bool needReport(unsigned int generation, unsigned int lastReportGeneration, double time, double lastReportTime) const = 0;
	};

	class TimeReportStep : public TimeReport {
		unsigned long step;
	public:
		TimeReportStep(double startTime, unsigned long step_)
			:TimeReport(startTime),
			step(step_) {}
		virtual bool needReport(unsigned int generation, unsigned int lastReportGeneration, double time, double lastReportTime) const {
			return generation - lastReportGeneration >= step;
		}
	};

	class TimeReportInterval : public TimeReport {
		double interval;
	public:
		TimeReportInterval(double startTime, double interval_)
			:TimeReport(startTime),
			interval(interval_) {}
		virtual bool needReport(unsigned int generation, unsigned int lastReportGeneration, double time, double lastReportTime) const {
			return time - lastReportTime >= interval; 
		}
	};

	/**
	* TimeReport which reports beginning generation
	*/
	struct TimeReportBegin : public TimeReport {
		TimeReportBegin( )
			:TimeReport(0) {}
		virtual bool needReport(unsigned int generation, unsigned int lastReportGeneration, double time, double lastReportTime) const {
			return generation == 0; 
		}
		/**
		* expire after first time
		*/
		virtual bool expired(double time, int generation) const {
			return time > 0.0;
		}
		/**
		* @return 0 (highest) 
		*/
		virtual unsigned int priority( ) const {
			return 0;
		}
	};

	/**
	* TimeReport which never reports; acts as placeholder
	* @param startTime
	* @param priority lower is higher priority, "standard" value is 10
	*/
	struct TimeReportQuiet : public TimeReport {

		TimeReportQuiet(long startTime, unsigned int priority = STD_PRIORITY)
			:TimeReport(startTime),
			priorityValue(priority) {}

		/**
		* never wants report
		*/
		virtual bool needReport(unsigned int generation, unsigned int lastReportGeneration, double time, double lastReportTime) const {
			return false; 
		}
		/**
		* never expires 
		*/
		virtual bool expired(double time, int generation) const {
			return false; 
		}
		/**
		* @return priorty constructed with 
		*/
		virtual unsigned int priority( ) const {
			return priorityValue; 
		}
	private:
		const unsigned int priorityValue;
	};

	/**
	* dummy placeholder so set iterator always has next element
	*/
	struct CollectionTail : public TimeReport {
		CollectionTail( ) 
			:TimeReport(std::numeric_limits<unsigned long>::max( )) {}
		virtual bool needReport(unsigned int generation, unsigned int lastReportGeneration, double time, double lastReportTime) const {
			return false; 
		}
	};

	struct TimeSorter {
		bool operator( )(const TimeReport * lhs, const TimeReport * rhs) {
			const double left =  lhs->getStartTime( );
			const double right =  rhs->getStartTime( );
			if (left < right) {
				return true;
			}
			if (right < left) {
				return false;
			}
			//if equal, use priorities
			const unsigned int lPriority = lhs->priority( );
			const unsigned int rPriority = rhs->priority( );
			if (lPriority < rPriority) {
				return true;
			}
			if (rPriority < lPriority) {
				return false;
			}
			VCELL_EXCEPTION(logic_error, "Duplicate start time " << left << " and priority " << lPriority);
		}
	};


	struct NoSolution {
		typedef ResultPoint DataType;
		const static bool validates = false;
		static double solution(double,double,double) { return 0; }
		static std::string expression( ) { throw std::domain_error("unsupported"); } 
	};

	template <class SOLUTION = NoSolution> 
	struct NHDF5Client :public moving_boundary::MovingBoundaryElementClient {
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

		/*
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
		*/

		/**
		* @param f file to write to
		* @param mbpp the problem 
		* @param baseName name of dataset in HDF5 file if not default
		*/
		template <typename R>
		NHDF5Client(H5::H5File &f, 
			WorldType & world_,
			const moving_boundary::MovingBoundaryParabolicProblem &mbpp, 
			const char *baseName,
			R &timeReports) 
			:file(f),
			theProblem(mbpp), 
			currentTime(0),
			totalStuff(0),
			oldStuff(0),
			meshDef(mbpp.meshDef( )),
			eRecords(),
			genTimes(),
			moveTimes(),
			timeStep(mbpp.baseTimeStep( )),
			buffer(),
			baseGroup( ),
			elementDataset( ),
			worldDim( ),
			//reportStep(calcReportStep(mbpp,startTime_,numberReports)),
			//reportCounter(0),
			//reportBegan(startTime_ == 0), //if beginning at zero, we've "begun" at the start
			reportActive(true),
			timer( ),
			world(world_),
			reportControllers( ),
			lastReportTime(0),
			lastReportGeneration(0),
			reportControl(nullptr),
			nextReportControlTime(-1),
			pointconverter(world.pointConverter( ))
		{
			using spatial::cX;
			using spatial::cY;
			int numberGenerations = mbpp.numberTimeSteps( );
			timer.start( );
			reportControllers.insert(new TimeReportBegin( ));
			reportControllers.insert(new TimeReportQuiet(0, std::numeric_limits<unsigned int>::max(  )) );
			for (typename R::iterator iter = timeReports.begin( );iter != timeReports.end( ); ++iter) {
				reportControllers.insert(*iter);
			}
			reportControllers.insert(new CollectionTail( ));
			determineReportControl(0,0);

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
				const double scaleFactor = world.theScale( );
				vcellH5::writeAttribute(baseGroup,"scaleFactor",scaleFactor);
				const double halfStep = world.distanceToProblemDomain(1) / 2.0;
				vcellH5::writeAttribute(baseGroup,"precision",halfStep);
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
				const double startx = world.toProblemDomain( meshDef.startCorner(spatial::cX), spatial::cX);
				const double starty = world.toProblemDomain( meshDef.startCorner(spatial::cY), spatial::cY);
				const double hx = world.distanceToProblemDomain( meshDef.interval(spatial::cX) );
				const double hy = world.distanceToProblemDomain( meshDef.interval(spatial::cY) );

				spatial::TGeoLimit<moving_boundary::CoordinateType> limit =  world.limits( )[spatial::cX];
				const double beginx = world.toProblemDomain (limit.low( ), spatial::cX); 
				const double endx = world.toProblemDomain (limit.high( ), spatial::cX); 
				assert(beginx == startx);

				limit =  world.limits( )[spatial::cY];
				const double beginy = world.toProblemDomain (limit.low( ), spatial::cY); 
				const double endy = world.toProblemDomain (limit.high( ), spatial::cY); 
				assert(beginy == starty);

				vcellH5::writeAttribute(elementDataset,"startX",startx);
				vcellH5::writeAttribute(elementDataset,"startY",starty);
				vcellH5::writeAttribute(elementDataset,"endX",endx);
				vcellH5::writeAttribute(elementDataset,"endY",endy);
				vcellH5::writeAttribute(elementDataset,"numX",xSize);
				vcellH5::writeAttribute(elementDataset,"numY",ySize);
				vcellH5::writeAttribute(elementDataset,"hx",hx);
				vcellH5::writeAttribute(elementDataset,"hy",hy);
				const std::string layout("time x X x Y (transposed in MATLAB)");
				vcellH5::writeAttribute(elementDataset,"layout",layout);
				const MovingBoundarySetup & setup = theProblem.setup( );
				if (setup.alternateFrontProvider != nullptr) {
					std::string desc = setup.alternateFrontProvider->describe( );
					vcellH5::writeAttribute(elementDataset,"alternate front",desc);
				}

				std::vector<moving_boundary::CoordinateType> xvalues = meshDef.coordinateValues(spatial::cX);
				std::vector<double> dv(xvalues.size( ));
				WorldType::XConverter xconverter(world);
				std::transform(xvalues.begin( ),xvalues.end( ),dv.begin( ),xconverter);
				vcellH5::SeqFacade<std::vector<double> > axisSF(dv); 
				vcellH5::facadeWriteAttribute(elementDataset,"xvalues",axisSF);

				std::vector<moving_boundary::CoordinateType> yvalues = meshDef.coordinateValues(spatial::cY);
				dv.resize(yvalues.size( )); 
				WorldType::YConverter yconverter(world);
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
		* delete TimeReport objects  
		*/
		~NHDF5Client( ) {
			std::for_each(reportControllers.begin( ),reportControllers.end( ),cleanup);
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

		/**
		* set #reportControl and #nextReportControlTime
		*/
		void determineReportControl(double t, unsigned int generation) {
			std::set<const TimeReport *,TimeSorter>::const_iterator iter = reportControllers.begin( );
			bool pastTime = t>=nextReportControlTime;
			//delete any expired or past time
			while (iter != reportControllers.end( )) {
				const TimeReport *tr = *iter;
				std::set<const TimeReport *,TimeSorter>::iterator eraseIter(iter); 
				++iter;
				if (tr->expired(t,generation) || (pastTime && tr->getStartTime( ) < nextReportControlTime )) {
					const TimeReport *dtr = *eraseIter;
					reportControllers.erase(eraseIter);
					delete dtr;
				}
			}
			nextReportControlTime = std::numeric_limits<double>::max( );
			for (iter = reportControllers.begin( );iter != reportControllers.end( ); ++iter) {
				double st = (*iter)->getStartTime( );
				if (st > t) {
					nextReportControlTime = st;
					break;
				}
			}
			iter = reportControllers.begin( );
			if (iter == reportControllers.end( )) {
				VCELL_EXCEPTION(logic_error,"No time reporter for time " << t << ", generation " << generation);
			}
			reportControl = *iter;
		}

		virtual void time(double t, unsigned int generationCounter, bool last, const moving_boundary::GeometryInfo<moving_boundary::CoordinateType> & geometryInfo) { 
			if (geometryInfo.nodesAdjusted) {
				moveTimes.push_back(t);
			}
			if (reportControl->expired(t,generationCounter) || t >= nextReportControlTime) {
				determineReportControl(t,generationCounter);
			}

			reportActive = last || reportControl->needReport(generationCounter,lastReportGeneration,t,lastReportTime);

			if (reportActive) {
				writeBoundary(genTimes.size( ),geometryInfo.boundary);
				currentTime = t;
				totalStuff = 0;
				std::cout << "generation " << std::setw(2) <<  generationCounter << " time " << currentTime << std::endl;
				VCELL_KEY_LOG(trace,Key::generationTime,"generation " << std::setw(2) <<  generationCounter << " time " << currentTime);
				genTimes.push_back(t);
				lastReportGeneration = generationCounter;
				lastReportTime = t;
			}
		}

		void writeBoundary(hsize_t timeIndex, const std::vector<spatial::TPoint<moving_boundary::CoordinateType,2> > & boundary) {
			try {
				moving_boundary::WorldToPDPointConverter<moving_boundary::CoordinateType,2> converter = world.pointConverter( );
				std::vector<PODPoint<double> > outVector(boundary.size( ));
				//std::transform(boundary.begin( ),boundary.end( ),outVector.begin( ),converter);
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
				throw vcellH5::Exception(e);
			}
		}


		static PODPoint<double> convertFrontToPOD(const spatial::TPoint<moving_boundary::CoordinateType,2> &in) {
			moving_boundary::World<moving_boundary::CoordinateType,2> world =  moving_boundary::World<moving_boundary::CoordinateType,2>::get( ); 
			moving_boundary::WorldToPDPointConverter<moving_boundary::CoordinateType,2> converter = world.pointConverter( ); 
			PODPoint<double> p = converter(in);
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
				if (eRecords.size( ) > 0) {

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

						size_t timeIndex = genTimes.size( ) - 1;

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
						throw vcellH5::Exception(e);
					}
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
				unsigned int lastTimeIndex = static_cast<unsigned int>(genTimes.size( ));
				vcellH5::primitiveWrite(baseGroup,"lastTimeIndex",lastTimeIndex); 

				VCELL_KEY_LOG(info,Key::generationTime,"logging " << genTimes.size( ) << " generation times"); 
				vcellH5::SeqFacade<std::vector<double> > gt(genTimes);
				vcellH5::facadeWrite(baseGroup,"generationTimes",gt);

				vcellH5::SeqFacade<std::vector<double> > mt(moveTimes);
				vcellH5::facadeWrite(baseGroup,"moveTimes",mt);
			}
			catch (H5::Exception &e) {
				throw vcellH5::Exception(e);
			}
		}

	private:
		/**
		* for_each function for destructor
		* @param tr to delete
		*/
		static void cleanup(const TimeReport *tr) {
			delete tr;
		}

		const moving_boundary::MovingBoundaryParabolicProblem &theProblem;

		H5::H5File & file;
		double currentTime; 
		double totalStuff;
		double oldStuff;
		const spatial::MeshDef<moving_boundary::CoordinateType,2> meshDef;
		typedef std::map<spatial::TPoint<size_t,2>, HElementRecord> RecordMap; 
		RecordMap eRecords;
		std::vector<double> genTimes;
		std::vector<double> moveTimes;
		const double timeStep;
		vcellH5::Flex2<typename SOLUTION::DataType> buffer;
		H5::Group baseGroup; 
		H5::DataSet elementDataset;
		H5::DataSet boundaryDataset;
		hsize_t worldDim[3];
		hsize_t boundaryDim[1];
		bool reportActive;
		vcell_util::Timer timer;
		const WorldType & world;
		std::set<const TimeReport *,TimeSorter> reportControllers;
		double lastReportTime;
		unsigned long lastReportGeneration;
		const TimeReport * reportControl;
		/**
		* start time of next TimeReport object not currently active
		*/
		double nextReportControlTime;

		WorldType::PointConverter pointconverter;
	};
}

#endif
