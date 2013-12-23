#include <MPoint.h>
#include <sys/stat.h>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <cassert>
#include <Expression.h>
#include <SimpleSymbolTable.h>

#include <vector>
#include <algorithm>
#include "gtest/gtest.h"
#include <Voronoi.h>
#include <algo.h>
#include <vcellutil.h>
#include <Mesh.h>
#include <MeshElementSpecies.h>
#include <VCellFront.h>
#include <Logger.h>
#include <Timer.h>
#include <MovingBoundaryParabolicProblem.h>
#include <HDF5Client.h>
#ifdef PLOTTING
#include <MBridge/Scatter.h>
#include <MBridge/Figure.h>
#include <MBridge/MBPolygon.h>
#include <MBridge/MBPatch.h>
#include <MBridge/MBMovie.h>
#include <MBridge/FronTierAdapt.h>
#endif
#include <MBridge/MatlabDebug.h>
#include <vhdf5/dataset.h>
#include <vhdf5/attribute.h>
#include <vhdf5/suppressor.h>
#include <vhdf5/facade.h>
#include <vhdf5/flex.h>
#include <vhdf5/file.h>
using std::cout;
using std::endl;
using spatial::NoSolution;
using vcell_util::Logger;
namespace {

	template <int N>
	struct TElementRecord {
		std::vector<double> volume;
		std::vector<double> mass;
		std::vector<double> concentration;
		spatial::SurfacePosition lastPosition;
		double x;
		double y;
		TElementRecord(double x_ = 0, double y_ = 0)
			:lastPosition(spatial::unset),
			volume(N),
			mass(N),
			concentration(N),
			x(x_),
			y(y_){
				for (int i = 0; i < N; i++) {
					assert(volume[i] == 0);
					assert(mass[i] == 0);
					assert(concentration[i] == 0);
				}
		}
	};

#ifdef PLOTTING 
	template <int N>
	struct NPlottingClient :public spatial::MovingBoundaryClient {
		const static int numGeneration = N;

		NPlottingClient(const spatial::MeshDef<double,2> & md, char *moveScript, char *ps, double pauseTime = 0)
			:script(moveScript),
			patchScriptName(ps),
			scaleScriptName(ps),
			masterPatchScript(scriptName(patchScriptName)),
			currentPatchScript(),
			patchScriptCount(1),
			log(textName(moveScript)),
			currentTime(0),
			meshDef(md),
			patch("concentration"),
			gen(-1),
			recordMovie(false),
			movie(script),
			minC(std::numeric_limits<double>::max( )),
			maxC(std::numeric_limits<double>::min( )),
			pauseCommand(pauseTime),
			needAPauseAndClear(false)
		{
			using spatial::cX;
			using spatial::cY;
			scaleScriptName += "SetScale";
			script << matlabBridge::FigureName("Moving") << std::endl;
			masterPatchScript << matlabBridge::FigureName("concentrations") << std::endl;
			masterPatchScript << "axis([" 
				<< meshDef.startCorner(cX) << ' ' << (meshDef.startCorner(cX) + meshDef.size(cX) ) << ' ' 
				<< meshDef.startCorner(cY) << ' ' << (meshDef.startCorner(cY) + meshDef.size(cY) ) 
				<< "]);" <<  std::endl;
			masterPatchScript << scaleScriptName << ';' << std::endl; 
			processPatchChunk( );
		}

		~NPlottingClient( ) {
			if (recordMovie) {
				movie.play( ); 
				movie.clear( );
			}
		}

		void processPatchChunk( ) {
			currentPatchScript.close( );
			std::string psname = nextPatchScriptName( );
			masterPatchScript << psname << ';' << std::endl; 
			currentPatchScript.open(scriptName(psname));
		}

		/**
		* one less than N to allow for recording of initial state
		*/
		static int numTimeStep( ) {
			return N - 1;
		}

		static std::string scriptName(const std::string & n) {
			std::string name(n);
			name += ".m";
			return name;
		}

		static std::string textName(const char * n) {
			std::string name(n);
			name += ".txt";
			return name;
		}

		std::string nextPatchScriptName( ) {
			std::ostringstream oss;
			oss << patchScriptName << patchScriptCount;
			return oss.str( );
		}

		virtual void time(double t, bool) {
			currentTime = t;
			gen++;
			//genTime[gen] = t;
		}
		/**
		* state of inside / boundary nodes
		*/
		virtual void element(const spatial::MeshElementSpecies<double,1> &e) {
			if (needAPauseAndClear) { //if first element in cycle
				script <<  pauseCommand << matlabBridge::clearFigure; 
				currentPatchScript << pauseCommand << matlabBridge::clearFigure;
				needAPauseAndClear = false;
			}
			using spatial::cX;
			using spatial::cY;
			const char *pSpec = "k"; 
			/* turn back on later, if desired
			const char *pSpec = nullptr;
			switch (er.lastPosition) {
			case spatial::unset:
			pSpec = "r";
			break;
			default:
			pSpec = "k";
			break;
			}
			er.lastPosition = e.mPos( );
			*/

			const spatial::Volume<double,2> & vol = e.getControlVolume(meshDef);
			matlabBridge::Polygons pgs(pSpec);
			spatial::Volume<double,2>::VectorOfVectors vv = vol.points( );
			frontTierAdapt::copyVectorsInto(pgs,vv);
			script << pgs;

			std::stringstream ss;
			ss << e.indexOf(0) << ',' << e.indexOf(1); 
			script << matlabBridge::Text(e(cX), e(cY), ss.str( ).c_str( )); 
			double c = e.concentration(0);
			minC = std::min(minC,c);
			maxC = std::max(maxC,c);

			if (!vv.empty( )) {
				patch.specifyValueAndClear(e.concentration(0));
				frontTierAdapt::copyVectorInto(patch,vv.front());
				currentPatchScript << patch;
			}
		}
		/**
		* notify client they've received all elements
		*/
		virtual void iterationComplete( ) {
			needAPauseAndClear = true; 
			//write concentration scale script -- overwrite previous
			{
				std::ofstream scaleScale(scriptName(scaleScriptName));
				scaleScale << "caxis ([" << minC << ' ' << maxC << "]);" << std::endl;
			}

			//log status
			std::stringstream s;
			s << "Time " << currentTime << std::ends;
			script << matlabBridge::Text(0,-50,s.str( ).c_str( )); 
			if (recordMovie) { //experimental buggy in-progress ...
				movie.recordFrame( );
			}
			if (matlabBridge::MatLabDebug::on("edgefind")) {
				matlabBridge::MatLabDebug::stream("edgeFind.m")  << std::endl << "% time " << currentTime << std::endl << std::endl;
			}
			if (gen > 0 && gen%20 == 0) {
				patchScriptCount++;
				processPatchChunk( );
			}
		}

		std::ofstream script;
		std::string patchScriptName; //without ".m"
		std::ofstream masterPatchScript;
		std::string scaleScriptName; //without ".m"
		int patchScriptCount;
		std::ofstream currentPatchScript;
		std::ofstream log;
		double currentTime; 
		const spatial::MeshDef<double,2> meshDef;
		matlabBridge::Patch patch;
		int gen;
		bool recordMovie;
		matlabBridge::Movie movie;
		std::array<double,N + 1> genTime;
		double minC; //min concentration
		double maxC;
		matlabBridge::PauseSeconds pauseCommand;
		bool needAPauseAndClear;
	};
#else
	//compile stub
	template <int N>
	struct NPlottingClient :public spatial::MovingBoundaryClient {
		NPlottingClient(const spatial::MeshDef<double,2> & md, const char *moveScript, const char *ps, double pauseTime = 0){}
		virtual void time(double t, bool, const spatial::GeometryInfo<double> &) { }
		virtual void element(const spatial::MeshElementSpecies<double,1> &e) {}
		virtual void iterationComplete( ) {}
		virtual void simulationComplete( ) {}
	};
#endif

	template <int N, class SOLUTION = NoSolution> 
	struct NRecordingClient :public spatial::MovingBoundaryClient {
		NRecordingClient(const spatial::MovingBoundaryParabolicProblem &mbpp) 
			:currentTime(0),
			totalStuff(0),
			oldStuff(0),
			meshDef(mbpp.meshDef( )),
			gen(-1),
			eRecords(),
			genTime( ),
			timeStep(mbpp.baseTimeStep( )),
			normsCalculated(false),
			totalMass( ),
			lNormInf( ),
			lNorm2Squared( ),
			numNonZeroConcentrations( ),
			normLinf(std::numeric_limits<double>::min( )),
			linfIndexes( ),
			normLinfNumeric( ),
			normLinfExact( ),
			dead(false)
		{
		}

		/**
		* one less than N to allow for recording of initial state
		*/
		static int numTimeStep( ) {
			return N - 1;
		}

		virtual void time(double t, bool, const spatial::GeometryInfo<double> &) { 
			if (!dead) {
				currentTime = t;
				totalStuff = 0;
				//std::cout << "generation " << std::setw(2) <<  gen << " time " << currentTime << std::endl;
				gen++;
				if (gen > N) {
					std::cerr << "overran storage; probable time step reduction" << std::endl;
					dead = true;
					return;
				}
				genTime[gen] = t;
			}
		}
		/**
		* state of inside / boundary nodes
		*/
		virtual void element(const spatial::MeshElementSpecies<double,1> &e) {
			if (!dead) {
				using spatial::cX;
				using spatial::cY;
				spatial::TPoint<size_t,2> key(e.indexOf(0),e.indexOf(1));
				if (eRecords.find(key) == eRecords.end( )) {
					ElementRecord newRecord(e(cX),e(cY));
					eRecords[key] = newRecord;
				}
				ElementRecord & er = eRecords[key];
				double m = e.mass(0);
				double c = e.concentration(0);
				double v = e.volume( );
				er.mass[gen] = m;
				er.concentration[gen] = c;
				er.volume[gen] = v;
			}
		}
		/**
		* notify client they've received all elements
		*/
		virtual void iterationComplete( ) {
			if (!dead) {
				VCELL_LOG(info,"Time " << currentTime << " total mass " << totalStuff); 
				if (oldStuff != 0 && !spatial::nearlyEqual(oldStuff,totalStuff,1e-3)) {
					VCELL_EXCEPTION(logic_error, "mass not conserved old"<< oldStuff << " , new " << totalStuff);
				}
				oldStuff = totalStuff;
			}
		}

		void summarize(std::ostream &os, bool needHeader, double time = -1) {
			if (!normsCalculated) {
				std::ofstream junk; //dummy io to nowhere
				write(junk,time, true);
			}
			using spatial::cX;
			using spatial::cY;
			const char comma = ',';
			if (needHeader) {
				os << "num X" << comma 
					<< "num Y" << comma
					<< "h X" << comma
					<< "h Y" << comma 
					<< "time step" << comma 
					<< "elapsed" << comma 
					<< "Linf" << comma 
					<< "L2" << comma
					<< "compiled" << comma
					<< "max time"  
					<< std::endl; 
			}

			double eSquared = 0;  
			int concentrationCount = 0;
			for (int i = 0; i <= gen; i++) {
				eSquared += lNorm2Squared[i];
				concentrationCount += numNonZeroConcentrations[i];
			}
			const double normL2 = sqrt (eSquared / concentrationCount);
#			ifdef NDEBUG
			const char * const compiled = "release";
#			else
			const char * const compiled = "debug";
#			endif

			os <<  meshDef.numCells(cX) << comma 
				<< meshDef.numCells(cY) << comma
				<< meshDef.interval(cX) << comma
				<<  meshDef.interval(cY) << comma
				<<  timeStep  << comma
				<<  time << comma 
				<< normLinf << comma 
				<< normL2 << comma
				<< compiled  << comma
				<< genTime[gen] << comma
				<< linfIndexes[cX] << comma <<linfIndexes[cY] << comma << normLinfExact << comma << normLinfNumeric << std::endl;
		}
		virtual void simulationComplete( ) {}

		/**
		* @param os output stream
		* @param time elapsed time of simulation
		* @param dummyWrite we're just executing to calculate norms
		*/
		void write(std::ostream &os, double time = -1, bool dummyWrite = false) {
			normsCalculated = true;
			using spatial::cX;
			using spatial::cY;
			const char comma = ',';
			os << "nX:" << comma << meshDef.numCells(cX) << comma 
				<< "nY:" << comma << meshDef.numCells(cY) << comma
				<< "hX:" << comma << meshDef.interval(cX) << comma
				<< "hY:" << comma << meshDef.interval(cY) << comma
				<< "t step:" << comma << timeStep  << comma
				<< "elapsed:" << comma << time
				<< std::endl;

			os << comma; 
			for (int i = 0; i <= gen; i++) {
				os << comma << "time" << comma << comma; 
			}
			os << std::endl;

			os << comma;
			for (int i = 0; i <= gen; i++) {
				os << comma << genTime[i] << comma << comma; 
			}
			os << std::endl;

			os << 'i' << comma << 'j';
			const char *massLabel = SOLUTION::validates ? "mass / x" : "mass";
			const char *volumeLabel = SOLUTION::validates ? "volume / y" : "volume";
			for (int i = 0; i <= gen; i++) {
				os << comma << massLabel << comma << volumeLabel << comma << "conc";
			}
			os << std::endl;
			if (!dummyWrite && !os.good( )) {
				throw std::runtime_error("bad state");
			}

			for (typename RecordMap::const_iterator iter = eRecords.begin( ); iter != eRecords.end( ); ++iter) {
				os << iter->first(spatial::cX) << comma << iter->first(spatial::cY) << comma; 
				const ElementRecord & er = iter->second;
				for (int i = 0; i <= gen; i++) {
					os << er.mass[i] << comma << er.volume[i] << comma << er.concentration[i] << comma ;
					totalMass[i] += er.mass[i];
				}
				os << std::endl;

				if (SOLUTION::validates) {
					using spatial::cX;
					using spatial::cY;
					os << iter->first(cX) << comma << iter->first(cY) << comma; 
					for (int i = 0; i <= gen; i++) {
						double sol = SOLUTION::solution(er.x,er.y,genTime[i]); 
						os << er.x << comma << er.y << comma << sol << comma ;
						if (er.concentration[i] != 0) {
							double error = abs(er.concentration[i] - sol);
							lNormInf[i] = std::max(lNormInf[i],error);
							if (error > normLinf) {
								normLinf = error;
								normLinfExact = sol;
								normLinfNumeric= er.concentration[i];
								linfIndexes[cX] =  iter->first(cX);
								linfIndexes[cY] =  iter->first(cY);
							}
							lNorm2Squared[i] += error * error;
							numNonZeroConcentrations[i]++;
						}
					}
					os << std::endl;
				}
			}
			os << comma << comma; //x, y columns
			for (int i = 0; i <= gen; i++) {
				os << totalMass[i] << comma << comma << comma; //mass (volume) (concentration)
			}
			os << std::endl;

			os << "LNorm Inf " << comma << comma; //x, y columns
			for (int i = 0; i <= gen; i++) {
				os << comma << comma << lNormInf[i] << comma; //(mass) (volume) concentration
			}
			os << std::endl;

			os << "LNorm2 " << comma << comma; //x, y columns
			for (int i = 0; i <= gen; i++) {
				os << comma << comma << sqrt(lNorm2Squared[i]/numNonZeroConcentrations[i]) << comma; //(mass) (volume) concentration
			}
			os << std::endl;
			os << "Max Linf: " << comma << linfIndexes[cX] << comma <<linfIndexes[cY] << "Exact:" << normLinfExact << comma
				<< "Numeric:" << comma << normLinfNumeric << std::endl;
		}

	private:

		double currentTime; 
		double totalStuff;
		double oldStuff;
		const spatial::MeshDef<double,2> meshDef;
		int gen;
		typedef TElementRecord<N + 1> ElementRecord; 
		typedef std::map<spatial::TPoint<size_t,2>, ElementRecord> RecordMap; 
		RecordMap eRecords;
		std::array<double,N + 1> genTime;
		const double timeStep;
		bool normsCalculated;
		std::array<double, N + 1> totalMass;
		std::array<double, N + 1> lNormInf;
		std::array<double, N + 1> lNorm2Squared;
		std::array<double, N + 1> numNonZeroConcentrations; 
		double normLinf;
		std::array<size_t,2> linfIndexes;
		double normLinfNumeric;
		double normLinfExact;
		bool dead;
	};

	/**
	* @param CTR STL style container
	*/
	template<class CTR>
	struct ClientFacade: public spatial::MovingBoundaryClient {
		ClientFacade(const CTR &ctr)
			:container(ctr) {}
		/**
		* time of simulation
		*/
		virtual void time(double t, bool last, const spatial::GeometryInfo<double> &gi) { 
			for (typename CTR::const_iterator iter = container.begin( ); iter != container.end( ); ++iter) {
				(*iter)->time(t, last,gi);
			}
		} 
		/**
		* state of inside / boundary nodes
		*/
		virtual void element(const spatial::MeshElementSpecies<double,1> &e) {
			for (typename CTR::const_iterator iter = container.begin( ); iter != container.end( ); ++iter) {
				(*iter)->element(e);
			}
		}
		/**
		* notify client they've received all elements
		*/
		virtual void iterationComplete( ) {
			for (typename CTR::const_iterator iter = container.begin( ); iter != container.end( ); ++iter) {
				(*iter)->iterationComplete( );
			}
		}
		virtual void simulationComplete( ) {
			for (typename CTR::const_iterator iter = container.begin( ); iter != container.end( ); ++iter) {
				(*iter)->simulationComplete( );
			}
		}
		const CTR & container;
	};

	struct Test1Solution {
		typedef spatial::TSolutionPoint<Test1Solution> DataType;
		const static bool validates = true;
		const static double EULERS_CONSTANT;
		const static double DIFFUSION_CONSTANT; 
		const static double X_VELOCITY;
		const static double A; //side length of box, must match test front, above 
		const static double k;
		const static double negK;
		const static double u0; 
		const static bool expressionValidate  = false;
		static std::array<string,3> symbols;
		static SimpleSymbolTable symTable;
		static VCell::Expression concExpress;

		static std::string xVelocity( ) {
			std::ostringstream xVel;
			xVel << X_VELOCITY;
			return xVel.str( );
		}

		static std::string expression( ) {
			std::ostringstream concentrationSS; 
			concentrationSS << u0 << '*' << EULERS_CONSTANT << "^(" << negK << "*(x-" << X_VELOCITY << "*t))";
			return concentrationSS.str( );
		}

		static double solution(double x,double y ,double t) { 
			const double u = u0 *pow(EULERS_CONSTANT,negK*(x - X_VELOCITY*t));
			if (expressionValidate) {
				double in[3];
				in[0] = x; in[1] = y; in[2] = t; 
				const double eU = concExpress.evaluateVector(in);
				const double diff = std::abs(u-eU);
				if (diff >1e-5) {
					VCELL_EXCEPTION(logic_error,"expressions differ");
				}
			}
			return u; 
		}
	};

	const double Test1Solution::EULERS_CONSTANT = 2.71828;
	const double Test1Solution::DIFFUSION_CONSTANT = 1;
	const double Test1Solution::X_VELOCITY = 1; 
	const double Test1Solution::A = 2; //side length of box, must match test front, above 
	const double Test1Solution::k = X_VELOCITY / DIFFUSION_CONSTANT;
	const double Test1Solution::negK = -k; 
	const double Test1Solution::u0 = (k * A /2) / sinh( k * A /2);
	std::array<string,3> Test1Solution::symbols = {"x","y","t"};
	SimpleSymbolTable Test1Solution::symTable(symbols.data( ), static_cast<int>(symbols.size( )));
	VCell::Expression Test1Solution::concExpress(expression( ),symTable);

	struct Test1Front : public spatial::FrontProvider {
		const double xVel;
		double time;
		std::vector<spatial::Point2D> baseFront; 
		Test1Front(double xVel_)
			:xVel(xVel_),
			time(0),
			baseFront( ) {
				using spatial::Point2D;
				baseFront.push_back(Point2D(-1,-1));
				baseFront.push_back(Point2D(1,-1));
				baseFront.push_back(Point2D(1,1));
				baseFront.push_back(Point2D(-1,1));
				baseFront.push_back(Point2D(-1,-1));
		}

		virtual bool propagateTo(double time_) {
			time = time_;
			return true;
		}
		/**
		* get current front 
		*/
		virtual std::vector<spatial::Point2D> retrieveFront( ) {
			std::vector<spatial::Point2D> rval(baseFront.size( ));
			std::transform(baseFront.begin( ),baseFront.end( ),rval.begin( ),*this);
			return rval;
		}

		/**
		* make self a transform operator
		*/
		spatial::Point2D operator( )(spatial::Point2D & in) {
			spatial::Point2D rval(in);
			rval(spatial::cX) += xVel *time;
			return rval;
		}

		std::string describe( ) const {
			return std::string("test1Front");
		}
	};
}

void test1(bool plotOn, bool spreadSheetOn, int numNodes = 100) {
//#define SMALL
#ifdef SMALL
	const int numberReports = 10;
	const int NUMBER_GENERATIONS = 100;
#else
	const int numberReports = 10;
	const int NUMBER_GENERATIONS = 100000;
#endif
#undef SMALL
	const double pauseTime = 0.01; //zero for manual pause
	const int stepP6 = 3000; // "step power N"
	const double timeStep = 1; 
	std::ostringstream baseNameSS;
	baseNameSS << "test1-" <<  numNodes << '-' <<  std::setfill('0') << std::setw(6) << stepP6;


	Logger::get( ).set(Logger::fatal);
	Logger::get( ).set(Logger::debug,"MeshElementSpecies.formBoundaryPolygon");
	//Logger::get( ).set(Logger::fatal,"setPos");
	vcell_util::FileDest fd(baseNameSS.str( ) + ".txt");
	Logger::get( ).setDestination(fd);

	typedef NPlottingClient<NUMBER_GENERATIONS> PlottingClient;
	typedef spatial::NHDF5Client<Test1Solution> HDF5Client;
	typedef NRecordingClient<NUMBER_GENERATIONS,Test1Solution> RecordingClient;
	//matlabBridge::MatLabDebug::activate("edgefind");
	//matlabBridge::MatLabDebug::activate("meshvoronoi");
	//matlabBridge::MatLabDebug::activate("frontmove");
	Test1Front testFront(Test1Solution::X_VELOCITY); //must match mbs.advectVelocityFunctionStrX 

	spatial::MovingBoundarySetup mbs;
	mbs.alternateFrontProvider = new Test1Front(Test1Solution::X_VELOCITY); //must match mbs.advectVelocityFunctionStrX 
	std::array<spatial::GeoLimit,2> limits;
	limits[0] = spatial::GeoLimit(-1.5,2.1);
	limits[1] = spatial::GeoLimit(-1.5,2.1);
	spatial::World<double,2>::get( ).init(limits);
	mbs.advectVelocityFunctionStrX = Test1Solution::xVelocity( );
	mbs.advectVelocityFunctionStrX = "1"; //no, there's not supposed to be advection 
	mbs.advectVelocityFunctionStrY = "0"; 
	//mbs.levelFunctionStr = "x >= 1 && x <= 3 && y >=  1 && y <= 3"; 
	//mbs.levelFunction = levelFunc;
	mbs.concentrationFunctionStr = Test1Solution::expression( );
	std::cout << "generations " << NUMBER_GENERATIONS << std::endl;
	std::cout << "exact solution " << mbs.concentrationFunctionStr << std::endl;

	mbs.numNodesX = numNodes; 
	mbs.numNodesY = numNodes; 
	mbs.frontToNodeRatio = 5;
	mbs.numberTimeSteps = RecordingClient::numTimeStep( );
	mbs.numberTimeSteps = 1; 
	mbs.diffusionConstant = 1;
	mbs.maxTime = 1; 
	PlottingClient *pc = nullptr;
	RecordingClient *rc = nullptr;
	try {
		spatial::MovingBoundaryParabolicProblem mbpp(mbs);
		const char * name = "test1data.h5";
		//H5::H5File file(name,H5F_ACC_CREAT);
		vcellH5::VH5File file(name,H5F_ACC_RDWR|H5F_ACC_TRUNC);

		typedef std::vector<spatial::MovingBoundaryClient *> ClientVector;
		ClientVector clients; 
		HDF5Client hc(file,mbpp, numberReports);
		clients.push_back(&hc);
		if (plotOn) {
			pc = new PlottingClient(mbpp.meshDef( ), "test1move.m", "test1concentration", pauseTime);
			clients.push_back(pc);
		}
		if (spreadSheetOn) {
			rc = new RecordingClient(mbpp);
			clients.push_back(rc);
		}

		double simTime = -1;
		try {
			ClientFacade<ClientVector> facade(clients);
			vcell_util::Timer timer;
			timer.start( );
			mbpp.run(facade);
			if (!plotOn) { //plotting takes forever, don't count it
				timer.stop( );
				simTime = timer.elapsed( );
			}
		} catch (std::exception e) {
			FAIL( ) << e.what( );
		}
		if (spreadSheetOn) {
			std::ofstream logfile(baseNameSS.str( ) + ".csv");
			assert(rc);
			rc->write(logfile, simTime); 
			const char * const summaryName = "test1summary.csv";
			struct stat status;
			const int rcode = stat(summaryName,&status);
			std::ofstream summary(summaryName,std::ios_base::app);
			rc->summarize(summary,rcode != 0, simTime);
		}
		/*
		H5::H5File file;
		const char * name = "test1data.h5";
		std::ifstream check(name);
		if (check.good( )) { //crude, but good enough for now
		file = H5::H5File(name,H5F_ACC_RDWR);
		}
		else {
		file = H5::H5File(name,H5F_ACC_TRUNC);
		}

		hc.hdf5Write(file);
		*/
	} catch (std::exception &e) {
		FAIL( ) << e.what( );
	}
	delete pc;
	delete rc;
}

#ifdef PLOTTING
TEST(frontier,test1p) {
	test1(true,true, 35);
}
#endif
TEST(frontier,test1) {
	test1(false,true);
}
TEST(frontier,test1s) {
	test1(false,false,35);
}
TEST(frontier,test1loop) {
	const double field = 3.5;
	const double stepPower = 1.467799268;
	const double start = 0.1;
	const double end =0.00099; //want .001, but it's floating points
	for (double h = start;h>=end;h/=stepPower) {
		int numNodes = static_cast<int>(field/h);
		std::cout << numNodes << " " << h << std::endl;
		test1(false,false,numNodes);
	}
}

TEST(frontier,test1h75) {
	test1(false,false,75);
}
