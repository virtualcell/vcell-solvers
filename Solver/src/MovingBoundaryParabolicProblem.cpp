//#pragma warning ( disable: 4996 )
#include <MPoint.h>
#include <VoronoiMesh.h>
#include <sstream>
#include <iomanip>
#include <exception>
#include <forward_list>
#include <vcellutil.h>
#include <Expression.h>
#include <SimpleSymbolTable.h>
#include <VCellFront.h>
#include <algo.h>
#include <MovingBoundaryParabolicProblem.h>
#include <MeshElementSpecies.h>
#include <VoronoiMesh.h>
#include <Logger.h>
#include <Modulo.h>

#include <MBridge/FronTierAdapt.h>
#include <MBridge/Figure.h>
#include <MBridge/MBPatch.h>
#include <MBridge/MBMovie.h>
#include <MBridge/MatlabDebug.h>

namespace {
	std::ofstream fspy("fcalls.txt"); //temp, frontier spy
	std::ofstream lspy("levelcalls.txt"); //temp, frontier spy
	//concentration function implement -- use either function pointer or expression parser
	struct ConcentrationProvider {
		virtual double conc(double x, double y) = 0;
	};

	struct ConcentrationPointer : public ConcentrationProvider {
		ConcentrationPointer(const spatial::ConcentrationFunction &p)
			:cfunc(p) {}

		virtual double conc(double x, double y) {
			return cfunc(x,y);
		}
		const spatial::ConcentrationFunction cfunc;
	};

	template<int NUMBER_SYMBOLS>
	struct ConcentrationExpression : public ConcentrationProvider {
		/**
		* runtime accessible #NUMBER_SYMBOLS
		*/
		static const int numberSymbols = NUMBER_SYMBOLS;

		ConcentrationExpression(VCell::Expression &e) 
			:exp(e) {
			VCELL_KEY_LOG(debug,"concentrationExpression", "expression: " << exp.infix( ));
		}

		virtual double conc(double x, double y) {
			double in[NUMBER_SYMBOLS] = {x,y,0};
			const double rval =  exp.evaluateVector(in);
			VCELL_KEY_LOG(debug,"concentrationExpression", "f(" << x << ", " << y << ") = " << rval);
			return rval;
		}
		VCell::Expression exp; 
	};

	struct ValidationBase: public spatial::FronTierLevel, public spatial::FronTierVelocity {

		/**
		* number of user input functions 
		*/
		const static int NUMBER_FUNCTIONS = 3;

		ValidationBase(const spatial::MovingBoundarySetup &mbs)
			:nFunctionPointers(0) { 
				std::stringstream badset;

#define CHECK(X) checkSet(badset,X, #X);  
#define CHECK2(X,Y) checkSet(badset,X, Y,#X);  

				CHECK(mbs.xLimits.high - mbs.xLimits.low);
				CHECK(mbs.yLimits.high - mbs.yLimits.low);
				CHECK(mbs.numNodesY);
				CHECK(mbs.numNodesY);
				CHECK(mbs.maxTime);
				CHECK(mbs.numberTimeSteps);
				CHECK2(reinterpret_cast<void *>(mbs.concentrationFunction),mbs.concentrationFunctionStr);
				CHECK2(reinterpret_cast<void*>(mbs.velocityFunction),mbs.advectVelocityFunctionStrX)
					if (mbs.alternateFrontProvider == 0) {
						CHECK2(reinterpret_cast<void *>(mbs.levelFunction),mbs.levelFunctionStr);
					}
#undef CHECK
#undef CHECK2
					switch (nFunctionPointers) {
					case 0:
					case NUMBER_FUNCTIONS:
						break;
					default:
						badset << "functions must be specified by pointers or strings, not both ";
						break;
					}
					if (mbs.velocityFunction == 0 && mbs.advectVelocityFunctionStrY.empty( )) {
						badset << "Unset Y velocity " << std::endl; 
					}

					validateSet(badset);
					if (mbs.frontVelocityFunctionStrX.empty( )) {
						const_cast<std::string &>(mbs.frontVelocityFunctionStrX) = mbs.advectVelocityFunctionStrX;
					}
					if (mbs.frontVelocityFunctionStrY.empty( )) {
						const_cast<std::string &>(mbs.frontVelocityFunctionStrY) = mbs.advectVelocityFunctionStrY;
					}
		}

		void checkSet(std::stringstream & badset,const double value, const char * const description) {
			if (value == 0) {
				badset << "Unset value " << description << std::endl;  
			}
		}
		void checkSet(std::stringstream & badset,void * value, const char * const description) {
			if (value == 0) {
				badset << "Unset value " << description << std::endl;  
			}
		}
		void checkSet(std::stringstream & badset, void * value, const std::string & str, const char * const description) {
			int argCount = 0;
			if (value != 0) {
				++argCount;
				++nFunctionPointers;
			}
			if (!str.empty( )) { 
				++argCount;
			}
			switch (argCount) {
			case 0:
				badset << "Unset value " << description << std::endl;  
				break;
			case 1:
				break;
			default:
				badset << description << " set more than once" << std::endl;  
			}
		}

		void validateSet(std::stringstream & badset ) {
			if (badset.str( ).length() > 0) {
				badset << " in MovingBoundarySetup ";
				std::invalid_argument ia(badset.str( ));
				throw ia; 
			}
		}
		int nFunctionPointers;
	};
}
namespace spatial {
	using VCell::Expression;
	/**
	* @param REAL floating point type 
	* @param NUM_S number species
	*/
	template <class REAL, int NUM_S>
	struct TMovingBoundaryParabolicProblem : public ValidationBase {
		typedef TMovingBoundaryParabolicProblem Outer;
		typedef VoronoiMesh<REAL,1> VrnMesh;
		typedef typename VrnMesh::MBMeshDef MBMeshDef;
		typedef typename VrnMesh::Element Element;
		typedef typename VrnMesh::MBMesh MBMesh;
		typedef typename VrnMesh::FrontType FrontType;

		TMovingBoundaryParabolicProblem(const spatial::MovingBoundarySetup &mbs) 
			:ValidationBase(mbs),
			diffusionConstant(mbs.diffusionConstant),
			symTable(buildSymTable( )),
			levelExp(mbs.levelFunctionStr,symTable), 
			advectVelocityExpX(mbs.advectVelocityFunctionStrX,symTable), 
			advectVelocityExpY(mbs.advectVelocityFunctionStrY,symTable), 
			frontVelocityExpX(mbs.frontVelocityFunctionStrX,symTable), 
			frontVelocityExpY(mbs.frontVelocityFunctionStrY,symTable), 
			concentrationExp(mbs.concentrationFunctionStr,symTable), 
			vcFront(initFront(*this,mbs)),
			meshDefinition(createMeshDef(mbs)),
			interiorVolume(meshDefinition.interval(cX) * meshDefinition.interval(cY) ),
			primaryMesh(meshDefinition),
			voronoiMesh(primaryMesh),
			//alloc( ),
			currentTime(0),
			maxTime(mbs.maxTime),
			timeStep(maxTime/mbs.numberTimeSteps),
			lostElements( ),
			gainedElements(  )
		{  
			using matlabBridge::MatLabDebug;

			//init using either function pointer or string
			if (nFunctionPointers == 0) {
				ConcentrationExpression<3> ce(concentrationExp);
				assert (ce.numberSymbols == symTable.size( )); 
				initMeshSizes(ce);
			}
			else {
				ConcentrationPointer cp(mbs.concentrationFunction);
				initMeshSizes(cp);
			}

			//check time step
			double hMin = std::min(meshDefinition.interval(spatial::cX),meshDefinition.interval(spatial::cY));
			double maxStep = hMin*hMin/(4 * diffusionConstant);
			if (maxStep < timeStep) {
				//use floor to make newNumberSteps lower and timeStep bigger to avoid very short last step
				int newNumberSteps = static_cast<int>(maxTime/maxStep);
				timeStep = (maxTime / newNumberSteps) * (1 + 1e-13); //exact factor to overcome precision lost to addition
			}

			if (MatLabDebug::on("tiling")) {
				std::ofstream master("tilingMaster.m");
				matlabBridge::Polygon pFront("b");
				frontTierAdapt::copyVectorInto(pFront,currentFront);
				int i = 0;
				for (typename MBMesh::iterator iter = primaryMesh.begin( ); iter != primaryMesh.end( ); ++iter) {
					Element &e = *iter;
					const Volume<double,2> & vol = e.getControlVolume(primaryMesh);
					if (!vol.empty( )) {
						std::stringstream command;
						command << "tilingX" << e.indexOf(spatial::cX) << 'Y' << e.indexOf(spatial::cY) << std::ends;

						std::stringstream filestr;
						filestr <<  command.str( ).c_str( ) << ".m" << std::ends;
						std::ofstream os(filestr.str( ));

						master << command.str( ) << std::endl;
						master << matlabBridge::pause << std::endl;
						os << matlabBridge::FigureName(command.str( ).c_str( ));
						os << matlabBridge::clearFigure << pFront;
						e.writeMatlab(os);
					}
				}
			}
			if (MatLabDebug::on("tilingmovie")) {
				std::ofstream master("tilingMovie.m");
				matlabBridge::Movie movie(master);
				master << matlabBridge::FigureName("Tiling, the movie");
				matlabBridge::Polygon pFront("b");
				frontTierAdapt::copyVectorInto(pFront,currentFront);
				int i = 0;
				for (typename MBMesh::iterator iter = primaryMesh.begin( ); iter != primaryMesh.end( ); ++iter) {
					Element &e = *iter;
					const Volume<double,2> & vol = e.getControlVolume(primaryMesh);
					if (!vol.empty( )) {
						master << matlabBridge::clearFigure << pFront;
						e.writeMatlab(master);
						movie.recordFrame( );
					}
				}
				movie.play( );
			}
		}
		~TMovingBoundaryParabolicProblem( ) {
			delete vcFront;
		}

		/**
		* set initial mesh sizes
		*/
		void initMeshSizes(ConcentrationProvider &provider) {
			using std::vector;
			//set inside volumes on all elements; they may become inside as front moves.
			for (typename MBMesh::iterator iter = primaryMesh.begin( ); iter != primaryMesh.end( ); ++iter) {
				iter->setInteriorVolume(interiorVolume);
			}
			currentFront = vcFront->retrieveFront( );
			if (currentFront.empty( )) {
				throw std::invalid_argument("empty front");
			}
			VCELL_LOG(warn,"initial front has " << currentFront.size( ) << " points");
			assert(currentFront.front( ) == currentFront.back( ));  //verify closed 
			voronoiMesh.setFront(currentFront);
			//spatial::Positions<Element> positions = spatial::classify2(voronoiMesh,currentFront);
			spatial::Positions<Element> positions = voronoiMesh.classify2(currentFront);
			for (typename vector<Element *>::iterator iter = positions.inside.begin( ); iter != positions.inside.end( ); ++iter) {
				Element  & insideElement = **iter;
				double mu = provider.conc(insideElement(cX),insideElement(cY));
				insideElement.setConcentration(0,mu);
			}
			for (typename vector<Element *>::iterator iter = positions.boundary.begin( ); iter != positions.boundary.end( ); ++iter) {
				Element  & boundaryElement = **iter;
				boundaryElements.push_back(&boundaryElement);
				double mu = provider.conc(boundaryElement(cX),boundaryElement(cY));
				boundaryElement.setConcentration(0,mu);
			}
			for (typename vector<Element *>::iterator iter = positions.outside.begin( ); iter != positions.outside.end( ); ++iter) {
				Element  & boundaryElement = **iter;
				boundaryElement.setConcentration(0,0);
			}

		}

		void boundaryElementSetup(Element & bElem) {
			assert (bElem.mPos( ) == spatial::boundarySurface);
		}

		/**
		* constructor support; front initialization
		*/
		static FrontProvider *initFront(const ValidationBase & base,const spatial::MovingBoundarySetup &mbs)  {
			if (mbs.alternateFrontProvider == nullptr) {
				std::vector<GeoLimit> limits;
				limits.push_back(mbs.xLimits);
				limits.push_back(mbs.yLimits);
				int mnode = std::max(mbs.numNodesX,mbs.numNodesY);
				int numFrontRegions = mnode * mbs.frontToNodeRatio;
				if (base.nFunctionPointers == 0) {
					return new VCellFront(limits,numFrontRegions,mbs.maxTime,base,base);
				}  
				/* else */
				throw std::domain_error("function pointers no longer supported");
				/*
				return new VCellFront(limits,numFrontRegions,mbs.maxTime,
				mbs.levelFunction,mbs.velocityFunction);
				*/
			}
			return mbs.alternateFrontProvider;
		}

		/**
		* constructor support; mesh initialization
		*/
		static MBMeshDef createMeshDef(const spatial::MovingBoundarySetup &mbs)  {
			using vcell_util::arrayInit;
			std::array<double,2> origin = arrayInit(mbs.xLimits.low, mbs.yLimits.low);
			std::array<double,2> c = arrayInit(mbs.xLimits.high -mbs.xLimits.low, mbs.yLimits.high -mbs.yLimits.low);
			std::array<size_t,2> p = arrayInit<size_t>( mbs.numNodesX, mbs.numNodesY);
			return MBMeshDef(origin,c,p);
		}
		/**
		* constructor support; build SymbolTable 
		*/
		static SimpleSymbolTable buildSymTable( ) {
			std::string syms[] = { "x","y","t"};
			return SimpleSymbolTable(syms, sizeof(syms)/sizeof(syms[0]));
		}

		virtual double level(double *in) const {
			lspy << in[0] << ',' << in[1] << std::endl; 
			double r = levelExp.evaluateVector(in);
			return r;
		}


		SVector<double,2> frontVelocity(double x, double y) const {
			fspy << x << ',' << y << ',' << currentTime << std::endl;
			enum {ex = 0, ey = 1, et = 2};
			double syms[3];
			syms[ex] = x; 
			syms[ey] = y; 
			syms[et] = currentTime; 
			double vX = frontVelocityExpX.evaluateVector(syms);
			double vY = frontVelocityExpY.evaluateVector(syms);
			return SVector<double,2>(vX,vY); 
		}

		SVector<double,2> advectionVelocity(double x, double y) const {
			enum {ex = 0, ey = 1, et = 2};
			double syms[3];
			syms[ex] = x; 
			syms[ey] = y; 
			syms[et] = currentTime; 
			double vX = advectVelocityExpX.evaluateVector(syms);
			double vY = advectVelocityExpY.evaluateVector(syms);
			return SVector<double,2>(vX,vY); 
		}

		virtual int velocity(Frontier::Front*,Frontier::POINT* fpoint,HYPER_SURF_ELEMENT*, HYPER_SURF*,double* out) const {
			const SVector<double,2> & v = frontVelocity(fpoint->_coords[cX],fpoint->_coords[cY]);
			out[cX] = v(cX); 
			out[cY] = v(cY); 

			return 0; //return value not used by frontier
		}


		void giveElementsToClient(MovingBoundaryClient & client) {
			GeometryInfo<double> gi(currentFront);
			client.time(currentTime, currentTime == maxTime, gi);
			for (typename MBMesh::const_iterator iter = primaryMesh.begin( ); iter != primaryMesh.end( ); ++iter) {
				if (iter->isInside( )) {
					client.element(*iter);
				}
			}
			client.iterationComplete( );
		}

		void debugDump(bool first) {
			std::ofstream dump("frontmove.m");
			if (first) {
				matlabBridge::Scatter nbplot('b',2);
				for (typename MBMesh::const_iterator iter = primaryMesh.begin( ); iter != primaryMesh.end( ); ++iter) {
					frontTierAdapt::copyPointInto(nbplot,*iter);
					double  x = iter->get(cX);
					double  y = iter->get(cY);

					std::stringstream ss;
					ss << iter->indexOf(cX) << ',' << iter->indexOf(cY); 
					dump << matlabBridge::Text(x,y, ss.str( ).c_str( ));
				}
				dump << nbplot;
			}
			matlabBridge::Polygon pPoly("k",1);
			frontTierAdapt::copyVectorInto(pPoly,currentFront);
			dump << pPoly << matlabBridge::pause;
		}

		static void commenceSimulation(Element &e) {
			e.beginSimulation( );
		}

		/**
		* Functor
		* base class for functors which need access to #Outer
		*/
		struct FunctorBase {
			FunctorBase(Outer &o)
				:outer(o) {}
			Outer & outer;
		};
		/**
		* Functor
		* move front
		*/
		struct FrontMove : public FunctorBase {
			FrontMove(Outer &o)
				:FunctorBase(o){}
			void operator( )(Element * p) {
				assert(p != nullptr);
				VCELL_LOG(trace,p->indexInfo( ) << " front move")
					if (p->mPos() == boundarySurface) {
						p->moveFront(this->outer.primaryMesh,this->outer.currentFront);
					}
					else {
						throw std::domain_error("FrontMove");
					}
			}
		};
#ifdef HOLD
#error should use front velocity, not elements
		/**
		* Functor
		* apply velocity, record max
		*/
		struct MaxVel : public FunctorBase {
			MaxVel (Outer &o)
				:FunctorBase(o),
				maxVel(0) {}
			void operator( )(Element & e) {
				VCELL_LOG(trace,e.indexInfo( ) << " max velocity")
					SVector<double,2> v = this->outer.velocity(e(cX),e(cY));
				maxVel  = std::max(maxVel, v(cX));
				maxVel  = std::max(maxVel, v(cY));
				e.setVelocity(v);
			}

			REAL maxVel;
		};
#endif

		/**
		* Functor
		* apply velocity, record max
		*/
		struct ApplyVelocity : public FunctorBase {
			ApplyVelocity (Outer &o)
				:FunctorBase(o) {}
			void operator( )(Element & e) {
				VCELL_LOG(trace,e.indexInfo( ) << " set velocity");
				SVector<double,2> v = this->outer.advectionVelocity(e(cX),e(cY));
				e.setVelocity(v);
			}
		};

		/**
		* Functor diffuseAdvect
		*/
		struct DiffuseAdvect {
			DiffuseAdvect(spatial::TDiffuseAdvectCache<double,2,1> &c, double d, double ts, bool &errFlag)
				:dac(c),
				diffusionConstant(d),
				timeStep(ts),
				errorFlag(errFlag) {}

			void operator( )(Element & e) {
				if (e.isInside( )) {
					VCELL_LOG(trace,e.ident( ) << " diffuseAdvect")
						e.diffuseAdvect(dac,diffusionConstant, timeStep, errorFlag);
				}
			}

			spatial::TDiffuseAdvectCache<double,2,1> &dac;
			const double diffusionConstant;
			const double timeStep;
			bool & errorFlag;
		};

#if 0
		/**
		* Functor ClearDiffuseAdvect
		*/
		struct ClearDiffuseAdvect {
			void operator( )(Element & e) {
				if (e.mPos( ) != outsideSurface) {
					VCELL_LOG(trace,e.ident( ) << " clearDiffuseAdvect")
						e.clearDiffuseAdvectAmounts( );
				}
			}

		};
#endif

		/**
		* Functor
		* collect mass 
		*/
		struct CollectMass : public FunctorBase {
			CollectMass (Outer &o)
				:FunctorBase(o) {}
			void operator( )(Element *p) {
				VCELL_LOG(trace,p->indexInfo( ) << " collectMass")
					p->collectMass(this->outer.primaryMesh,this->outer.currentFront);
			}
		};

		/**
		* Functor
		* apply front
		*/
		struct ApplyFront : public FunctorBase {
			ApplyFront(Outer &o)
				:FunctorBase(o) {}
			void operator( )(Element &e) {
				VCELL_LOG(trace,e.indexInfo( ) << " applyFront")
					e.applyFront(this->outer.primaryMesh, this->outer.currentFront, this->outer.interiorVolume);
			}
		};

		/**
		* Functor
		* distribute lost mass 
		*/
		struct DistributeLost : public FunctorBase {
			DistributeLost(Outer &o)
				:FunctorBase(o) {}
			void operator( )(Element &e) {
				VCELL_LOG(trace,e.indexInfo( ) << " distributeLost")
					e.distributeMassToNeighbors(this->outer.primaryMesh);
			}
		};

		/**
		* Functor
		* distribute lost mass 
		*/
		struct EndCycle : public FunctorBase {
			EndCycle(Outer &o)
				:FunctorBase(o) {}
			void operator( )(Element &e) {
				VCELL_LOG(trace,e.indexInfo( ) << " end of cycle");
				e.endOfCycle( );
			}
		};

		void run(MovingBoundaryClient & client) {
			VCELL_LOG(trace,"commence simulation");
			std::for_each(primaryMesh.begin( ),primaryMesh.end( ),commenceSimulation);
			giveElementsToClient(client);
			if (matlabBridge::MatLabDebug::on("frontmove")) {
				debugDump(true);
			}

			while (currentTime < maxTime) {
				double timeIncr = timeStep;

				currentTime += timeIncr;
				//make last step end exactly on limit to simplify comparisions betweens different simulations
				if (currentTime > maxTime) {
					currentTime = maxTime;
				}
#if PENDING_DISCUSSION_OF_WHERE_TO_CHECK
				bool goodStep = false;
				while (!goodStep) {
					ApplyVelocity mv(*this);
					std::for_each(primaryMesh.begin( ),primaryMesh.end( ),mv);
					double maxTime = meshDef( ).minimumInterval( ) / (2 * mv.maxVel);
					maxTime = meshDef( ).minimumInterval( ) / (2);
					if (timeIncr <= maxTime) {
						goodStep = true;
					}
					else {
						currentTime -= timeIncr;
						timeIncr = maxTime; 
						currentTime += timeIncr;
						VCELL_LOG(debug, "reduced time step to " << timeIncr)
					}
				}
#else
				ApplyVelocity mv(*this);
				std::for_each(primaryMesh.begin( ),primaryMesh.end( ),mv);
#endif //PENDING_DISCUSSION_OF_WHERE_TO_CHECK

				VCELL_LOG(debug, "moved from to time");
				vcFront->propagateTo(currentTime); 
				/*
				VCellFront *vcp = dynamic_cast<VCellFront *>(vcFront);
				if (vcp != nullptr) {
					Front * checkpoint = copy_front(vcp->c_ptr( ));
					FT_Save(checkpoint, "testsave.txt");
				}
				*/
				
				currentFront = vcFront->retrieveFront( );
				voronoiMesh.setFront(currentFront);

				{
					//					std::ofstream f("fullvdump.m");
					//					voronoiMesh.matlabPlot(f, &currentFront);
				}
				std::for_each(boundaryElements.begin( ),boundaryElements.end( ),FrontMove(*this));

				if (matlabBridge::MatLabDebug::on("frontmove")) {
					debugDump(false);
				}

#ifdef HOLD_FOR_POSSIBLE_IMPLEMENTATION_LATER
				double diffuseAdvectStep = timeIncr;
				double totalStepped = 0;
				int numberDASteps = 1;
				bool tooBig = false;
				do {	
					tooBig = false;
					for (int step = 0 ; step < numberDASteps; ++step ) {
						primaryMesh.diffuseAdvectCache( ).clearDiffuseAdvectCache( );
						std::for_each(primaryMesh.begin( ),primaryMesh.end( ), 
							DiffuseAdvect(primaryMesh.diffuseAdvectCache( ),diffusionConstant, diffuseAdvectStep, tooBig));
						if (tooBig) {
							diffuseAdvectStep /= 2;
							numberDASteps *= 2; 
							VCELL_LOG(debug,"Reducing da time step to " << diffuseAdvectStep <<  ", steps = " << numberDASteps);
							break;
						}
						primaryMesh.diffuseAdvectCache( ).checkDiffuseAdvectCache( );
					}
				} while (tooBig);
#endif
				primaryMesh.diffuseAdvectCache( ).clearDiffuseAdvectCache( );
				bool tooBig = false;
				std::for_each(primaryMesh.begin( ),primaryMesh.end( ), 
					DiffuseAdvect(primaryMesh.diffuseAdvectCache( ),diffusionConstant, timeIncr, tooBig));
				if (tooBig) {
					throw TimeStepTooBig("time step makes mass go negative");
				}
				primaryMesh.diffuseAdvectCache( ).checkDiffuseAdvectCache( );

				//adjustNodes calls MeshElementSpecies.updateBoundaryNeighbors
				boundaryElements.erase(boundaryElements.begin( ),boundaryElements.end( ));
				voronoiMesh.adjustNodes(boundaryElements,currentFront);
				//spatial::adjustNodes(boundaryElements,voronoiMesh,currentFront);
				//TODO: nochange

				std::for_each(boundaryElements.begin( ),boundaryElements.end( ),CollectMass(*this));

				std::for_each(primaryMesh.begin( ),primaryMesh.end( ),ApplyFront(*this));

				std::for_each(primaryMesh.begin( ),primaryMesh.end( ),DistributeLost(*this));

				std::for_each(primaryMesh.begin( ),primaryMesh.end( ),EndCycle(*this));

				//tell the client about it
				giveElementsToClient(client);
#ifdef OLD				
				lostElements.clear( );
				gainedElements.clear( );
				for (MBMesh::const_iterator iter = primaryMesh.begin( ); iter != primaryMesh.end( ); ++iter) {

					const Element &previous = *iter;
					Element &next = previous.doppelganger( );
					if (next.isInside( )) {
						next.setVelocity(velocity(next(cX),next(cY)) );
					}
					Element::Transition tns = previous.stateChange( );
					switch (tns) {
					case Element::same:
						for (size_t s = 0; s < NUM_S;s++) {
							const REAL mass = previous.mass(s);
							next.setMass(s,mass);
						}
						break;
					case Element::lost:
						assert(previous.mPos( ) == boundarySurface);
						boundaryElements.remove(const_cast<Element *>(&previous));
						lostElements.push_back(&previous);
						break;
					case Element::gained:
						assert(next.mPos( ) == boundarySurface);
						boundaryElements.emplace_front(const_cast<Element *>(&previous));
						gainedElements.push_back(&next);
						break;
					}
				}
				for (std::vector<const Element *>::const_iterator iter = lostElements.begin( );iter != lostElements.end( );++iter) {
					const Element * e = (*iter);
					size_t x = e->indexOf(0);
					size_t y = e->indexOf(1);
					e->distributeMassToNeighbors(meshDefinition);
				}
				for (std::vector<Element *>::iterator iter = gainedElements.begin( );iter != gainedElements.end( );++iter) {
					Element * e = (*iter);
					e->collectMassFromNeighbors(meshDefinition);
				}

				//tell the client about it
				giveElementsToClient(client);


#endif
			}
			client.simulationComplete( );
		}

		const MeshDef<REAL,2> & meshDef( ) const {
			return meshDefinition;
		}

		unsigned int numberTimeSteps( ) const {
			return static_cast<unsigned int>(maxTime / timeStep);
		}

		const double diffusionConstant;
		/**
		* symbol table for expressions, must be initialized prior to xxExp
		*/
		SimpleSymbolTable symTable;
		/**
		* level expression, must be created before front
		*/
		mutable Expression levelExp; //Expression doesn't currently support "const"
		mutable Expression advectVelocityExpX;
		mutable Expression advectVelocityExpY;
		mutable Expression frontVelocityExpX;
		mutable Expression frontVelocityExpY;
		mutable Expression concentrationExp;
		/**
		* FronTier integration
		*/
		FrontProvider * vcFront;
		FrontType currentFront;
		MBMeshDef meshDefinition;

		/**
		* standard volume of inside point
		*/
		double interiorVolume;
		MBMesh primaryMesh;
		VrnMesh voronoiMesh;
		//		vcell_util::ChunkAllocator<VoronoiResult,60> alloc;
		double currentTime;
		double maxTime;
		double timeStep; 
		/**
		* current boundary elements
		*/
		//typedef std::forward_list<Element *> ElementList; 
		std::vector<Element *> boundaryElements;
		/**
		* elements "lost" (moved outside of boundary) during transition between generations
		* point to previous generation 
		*/
		std::vector<const Element *> lostElements;
		/**
		* elements "gained" (moved inside of boundary) during transition between generations
		* point to next generation 
		*/
		std::vector<Element *> gainedElements;
	};

	struct MovingBoundaryParabolicProblemImpl : public TMovingBoundaryParabolicProblem <double,1> {
		MovingBoundaryParabolicProblemImpl(const spatial::MovingBoundarySetup &mbs) 
			:TMovingBoundaryParabolicProblem<double,1>(mbs) {}
		~MovingBoundaryParabolicProblemImpl( ) {}
	};


	MovingBoundaryParabolicProblem::MovingBoundaryParabolicProblem(const MovingBoundarySetup &mbs)
		:impl(new MovingBoundaryParabolicProblemImpl(mbs)) { }

	const spatial::MeshDef<double,2> & MovingBoundaryParabolicProblem::meshDef( ) const {
		return impl->meshDef( );
	}
	double MovingBoundaryParabolicProblem::baseTimeStep( ) const {
		return impl->timeStep;
	}
	unsigned int MovingBoundaryParabolicProblem::numberTimeSteps( ) const {
		return impl->numberTimeSteps( );
	}

	MovingBoundaryParabolicProblem::~MovingBoundaryParabolicProblem( ) {
		delete impl;
	}

	namespace {
		struct Evaluator {
			virtual ~Evaluator( ) {}
			virtual double evaluate(MovingBoundaryParabolicProblemImpl::Element & mes) const = 0;
			virtual const char * const label( ) const = 0;
			virtual const char * const checkFunction( ) const = 0;
			bool supportsCheck( ) const {
				return checkFunction( ) != 0; 
			}
		};

		struct Concentration : public Evaluator {
			virtual double evaluate(MovingBoundaryParabolicProblemImpl::Element & mes) const {
				return mes.concentration(0);
			}
			virtual const char * const label( ) const {
				return "concentration";
			}
			virtual const char * const checkFunction( ) const {
				return 0;
			}
		};

		struct VolumeEval : public Evaluator {
			virtual double evaluate(MovingBoundaryParabolicProblemImpl::Element & mes) const {
				double v = mes.volume( );
				return v;
			}
			virtual const char * const label( ) const {
				return "volume";
			}
			virtual const char * const checkFunction( ) const {
				return  "polyarea";
			}
		};

		void meshElementPlot(const Evaluator & ev, MovingBoundaryParabolicProblemImpl & impl, std::ostream &os) {
			char colors[] = {'y','m','c','g','b','k'};	
			vcell_util::Modulo<int,int> cIndex(0,sizeof(colors)/sizeof(colors[0]));

			MovingBoundaryParabolicProblemImpl::MBMesh::iterator iter = impl.primaryMesh.begin( );
			matlabBridge::Patch patch(ev.label( ), ev.checkFunction( ), "1e-3");
			matlabBridge::Scatter scatter('g',2,true);
			matlabBridge::Scatter empty('r',2,true);
			int seqNo = 1; //used to cause matlab variables to change
			for (;iter != impl.primaryMesh.end( ); ++iter, ++seqNo) {
				MovingBoundaryParabolicProblemImpl::Element & mes = *iter;
				/*
				if (mes.indexOf(cX) != 5) 
				continue;
				if (mes.indexOf(cY) != 11 && mes.indexOf(cY) != 11 ) {
				continue;
				}
				*/

				std::stringstream ss; ss << '[' << mes.indexOf(cX) << ',' << mes.indexOf(cY) << ']';
				std::string spec("-");
				spec += colors[cIndex];
				++cIndex;
				matlabBridge::Polygon boundingPoly("k",2, seqNo);
				const Volume<double,2> & vol = mes.getControlVolume(impl.primaryMesh);
				if (vol.empty( )) {
					empty.add(mes(cX),mes(cY));
					ss << "empty";
					os << matlabBridge::ConsoleMessage(ss.str( ).c_str( )); 
					continue;
				}
				double c =  ev.evaluate(mes);
				Volume<double,2>::VectorOfVectors vOfV = vol.points( );
				const bool checkVal = vOfV.size( ) == 1;
				patch.checkControl( ) = ev.supportsCheck( ) && checkVal;
				for (Volume<double,2>::VectorOfVectors::const_iterator vvIter = vOfV.begin( ); vvIter != vOfV.end( );++vvIter) {
					matlabBridge::Polygon pPoly("k",3);
					patch.specifyValueAndClear(c, seqNo);
					frontTierAdapt::copyVectorInto(pPoly,*vvIter);
					frontTierAdapt::copyVectorInto(patch,*vvIter);
					os << patch << pPoly;
				}
				scatter.add(mes(cX),mes(cY));
				os << matlabBridge::ConsoleMessage(ss.str( ).c_str( )); 
			}
			patch.setScale(os);
			os << scatter << empty;
		}
	}
	void MovingBoundaryParabolicProblem::run(spatial::MovingBoundaryClient & client) {
		impl->run(client);
	}

	void MovingBoundaryParabolicProblem::plotPolygons(std::ostream &os)  const {
		meshElementPlot(Concentration( ),*impl,os);
	}
	void MovingBoundaryParabolicProblem::plotAreas(std::ostream &os) const {
		meshElementPlot(VolumeEval( ),*impl,os);
	}
	double MovingBoundaryParabolicProblem::endTime( ) const {
		return impl->maxTime;
	}
}
