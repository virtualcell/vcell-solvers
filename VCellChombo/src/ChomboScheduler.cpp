#include <VCELL/ChomboScheduler.h>
#include <BRMeshRefine.H>

#include <EBISLayout.H>
#include <EBAMRPoissonOp.H>
#include <EBLevelGrid.H>
#include <GeometryShop.H>
#include <EBEllipticLoadBalance.H>
#include <EBCoarseAverage.H>
#include <EBPWLFineInterp.H>
#include <EBAMRDataOps.H>
#include <EBLevelDataOps.H>
#include <MFIndexSpace.H>
#include <ComplementIF.H>
#include <UnionIF.H>

#include <VCELL/VarContext.h>
#include <Expression.h>
#include <SimpleSymbolTable.h>
#include <VCELL/VolumeVariable.h>
#include <VCELL/DataSet.h>
#include <VCELL/VolumeRegionVariable.h>
#include <VCELL/MembraneVariable.h>
#include <VCELL/MembraneRegionVariable.h>
#include <VCELL/VCellModel.h>
#include <VCELL/Feature.h>
#include <VCELL/SimTool.h>
#include <VCELL/VarContext.h>
#include <VCELL/VolumeVarContextExpression.h>
#include <VCELL/VolumeRegionVarContextExpression.h>
#include <VCELL/MembraneVarContextExpression.h>
#include <VCELL/MembraneRegionVarContextExpression.h>
#include <VCELL/SimulationMessaging.h>
#include <VCELL/SimulationExpression.h>
#include <VCELL/Membrane.h>
#include <VCELL/ChomboIF.h>
#include <VCELL/ChomboSpec.h>
#include <VCELL/ChomboGeometry.h>
#include <VCELL/ChomboGeometryShop.h>
#include <VCELL/ConnectedComponent.h>
#include <assert.h>
#include <sstream>
#include <hdf5.h>

#ifdef HOFFSET
   #undef HOFFSET
#endif
#define HOFFSET(TYPE, MEMBER) ((size_t) &((TYPE *)0)->MEMBER)

//#define EDGE_CROSS_POINTS_FILE_EXT ".crspts"
//#define CHOMBO_MEMBRANE_METRICS_FILE_EXT ".chombo.memmetrics"
//#define MEMBRANE_SLICE_CROSS_FILE_EXT ".slccrs"
#define HDF5_FILE_EXT ".hdf5"
#define MESH_HDF5_FILE_EXT ".mesh.hdf5"

static const int blockFactor = 8;  // smallest box, 8 is recommended.
static const int nestingRadius  = 2; //ghostPhi[0];  // should be the same as ghost phi size, but Terry used 2
static const int maxCoarsen = -1;
static const int numPreCondIters = 4;
static const int relaxType = 2;
static const int numGhostEBISLayout = 4;

enum MembraneInvalidNeighborIndex
{
	MEMBRANE_NEIGHBOR_UNKNOWN = -10,
	MEMBRANE_NEIGHBOR_NEXT_TO_WALL = -1,
};

ChomboScheduler::ChomboScheduler(SimulationExpression* sim, ChomboSpec* chomboSpec) {
	simulation = sim;
	this->chomboSpec = chomboSpec;
	chomboGeometry = this->chomboSpec->getChomboGeometry();
	if (chomboSpec->getRefinementRoi().empty())
	{
		refinementRoiExp = NULL;
		refinementRoiSymbolTable = NULL;
	}
	else
	{
		refinementRoiExp = new VCell::Expression(chomboSpec->getRefinementRoi());
		string spatialSymbols[3] = {"x", "y", "z"};
		refinementRoiSymbolTable = new SimpleSymbolTable(spatialSymbols, 3, NULL);
		refinementRoiExp->bindExpression(refinementRoiSymbolTable);
	}
	numLevels = chomboSpec->getNumLevels();
	int* ratios = chomboSpec->getRefRatios();
	for (int i = 0; i < numLevels; i ++) {
		vectRefRatios.push_back(ratios[i]);
	}
	hdf5FileCount = 0;
	numGhostSoln = IntVect::Unit * 3;
	cout << "maxBoxSiz=" << chomboSpec->getMaxBoxSize() << endl;
	cout << "fillRatio=" << chomboSpec->getFillRatio() << endl;

	H5dont_atexit();
}

ChomboScheduler::~ChomboScheduler() {
	delete refinementRoiSymbolTable;
	for (int iphase = 0; iphase < NUM_PHASES; iphase ++) {
		delete geoIfs[iphase];
		for (int ivol = 0; ivol < phaseVolumeList[iphase].size(); ivol ++) {
			volSoln[iphase][ivol].clear();
			irregularPointMembraneIDs[iphase][ivol].clear();
			delete phaseVolumeList[iphase][ivol];
		}
		phaseVolumeList[iphase].clear();
		irregularPointMembraneIDs[iphase].clear();
	}
	phaseVolumeList.clear();
	irregularPointMembraneIDs.clear();
	//membranePointIndexes.clear();
	irregVolumeMembraneMap.clear();
}

bool ChomboScheduler::isInNextFinerLevel(int level, const IntVect& gridIndex) {
	if (level >= numLevels - 1) {
		return false;
	}

	int refRatio = vectRefRatios[level];
	IntVect gridIndexInNextFinerLevel = gridIndex * refRatio;

	int nextFinerLevel = level + 1;
	for (LayoutIterator lit = vectGrids[nextFinerLevel].layoutIterator(); lit.ok(); ++ lit) {
		const Box& box = vectGrids[nextFinerLevel].get(lit());
		if (box.contains(gridIndexInNextFinerLevel)) {
			return true;
		}
	}
	return false;
}

int ChomboScheduler::getChomboBoxLocalIndex(const IntVect& size, int ivar, const IntVect& ijk) {
	return getChomboBoxLocalIndex(size, ivar, D_DECL(ijk[0], ijk[1], ijk[2]));
}

int ChomboScheduler::getChomboBoxLocalIndex(const IntVect& size, int ivar, D_DECL(int i, int j, int k)) {
#if CH_SPACEDIM==2
	return ivar * size[0] * size[1] + j * size[0] + i;
#else
	return ivar * size[0] * size[1] * size[2] + k * size[0] * size[1] + j * size[0] + i;
#endif
}

double ChomboScheduler::getExpressionConstantValue(Variable* var, ExpressionIndex expIndex, Feature* feature) {
	VolumeVarContextExpression* varContextExp =	(VolumeVarContextExpression*)var->getVarContext();
	return varContextExp->evaluateConstantExpression(expIndex);
}

void ChomboScheduler::initializeGrids() {
	if (SpaceDim != chomboGeometry->getDimension()) {
		stringstream ss;
		ss << "Wrong executable (" << SpaceDim << "d) for this simulation (" << chomboGeometry->getDimension() << "d)" << endl;
		throw ss.str();
	}

	cout << endl << "ChomboScheduler:: generating grids" << endl;
	IntVect coarsestNx = chomboGeometry->getMeshSize();
	RealVect coarsestDx = chomboGeometry->getDomainSize();

	IntVect lo = IntVect::Zero;
	IntVect hi = coarsestNx;
	hi -= IntVect::Unit;
	Box crseDomBox(lo,hi);
	ProblemDomain coarsestDomain(crseDomBox);

	for (int idir = 0; idir < SpaceDim; idir ++) {
		coarsestDx[idir] /= coarsestNx[idir];
		coarsestDomain.setPeriodic(idir, false);
	}

	// setup Domains Dxes;
	vectDomains.resize(numLevels);
	vectDomains[0] = coarsestDomain;
	vectDxes.resize(numLevels);
	vectDxes[0] = coarsestDx;
	vectNxes.resize(numLevels);
	vectNxes[0] = coarsestNx;
	for(int ilev = 1; ilev < numLevels; ilev++) {
		vectDxes[ilev] = vectDxes[ilev-1];
		vectDxes[ilev] /= vectRefRatios[ilev-1];
		vectNxes[ilev] = vectNxes[ilev-1];
		vectNxes[ilev] *= vectRefRatios[ilev-1];
		vectDomains[ilev] = refine(vectDomains[ilev-1], vectRefRatios[ilev-1]);
	}

    // Define Geometry
	cout << "ChomboScheduler:: processing geometry" << endl;

	int finestLevel = numLevels - 1;
	ProblemDomain finestDomain = vectDomains[finestLevel];
	RealVect fineDx = vectDxes[finestLevel];

	Vector<GeometryService*> geometries(NUM_PHASES, NULL);
	Vector<BaseIF*> phase0List;
	for (int i = 0; i < chomboGeometry->getNumSubdomains(); ++ i) {
		ChomboIF* chomboIF = chomboGeometry->getChomboIF(i);
		if (chomboIF->getPhaseIndex() == 0) {
			phase0List.push_back(chomboIF);
		}
	}
	geoIfs[0] = new UnionIF(phase0List);
	GeometryShop *workshop0 = new GeometryShop(*(geoIfs[0]), 0, fineDx);
	workshop0->m_phase = 0;
	geometries[0] = workshop0;

	geoIfs[1] = new ComplementIF(*(geoIfs[0]));
	workshop0 = new GeometryShop(*(geoIfs[1]), 0, fineDx);
	workshop0->m_phase = 1;
	geometries[1] = workshop0;

	const RealVect& domainOrigin = chomboGeometry->getDomainOrigin();
	MFIndexSpace mfIndexSpace;
	mfIndexSpace.define(finestDomain.domainBox(), domainOrigin, fineDx[0], geometries, chomboSpec->getMaxBoxSize()/* ,maxCoarsenings */);

	phaseVolumeList.resize(NUM_PHASES);
	numConnectedComponents = 0;
	for (int iphase = 0; iphase < NUM_PHASES; iphase ++) {
		// Select one index-space
		Chombo_EBIS::alias((EBIndexSpace*) mfIndexSpace.EBIS(iphase));
		EBIndexSpace* ebisPtr = Chombo_EBIS::instance();
		Vector<RefCountedPtr<EBIndexSpace> > volumes = ebisPtr->connectedComponents();
		for (int ivol = 0; ivol < volumes.size(); ivol++) {
			++ numConnectedComponents;
			
			// find the feature for this volume
			RefCountedPtr<EBIndexSpace> volume = volumes[ivol];
			int depth = volume->getLevel(finestDomain);
			DisjointBoxLayout grids = volume->levelGrids(depth);
			EBISLayout ebisLayout;
			volume->fillEBISLayout(ebisLayout, grids, finestDomain, numGhostEBISLayout);
			Feature* featureFoundRegular = NULL;
			Feature* featureFoundIrregular = NULL;

			for(DataIterator dit = grids.dataIterator(); dit.ok() && featureFoundRegular == NULL; ++dit) {
				const Box& currBox = grids[dit()];
				const EBISBox& currEBISBox = ebisLayout[dit()];
				const IntVect& smallEnd = currBox.smallEnd();
				const IntVect& boxSize = currBox.size();
#if CH_SPACEDIM==3
				for (int k = 0; k < boxSize[2] && featureFoundRegular == NULL; ++k) {
#endif
					for (int j = 0; j < boxSize[1] && featureFoundRegular == NULL; ++j) {
						for (int i = 0; i < boxSize[0] && featureFoundRegular == NULL; ++i) {
							IntVect gridIndex(D_DECL(i + smallEnd[0], j + smallEnd[1], k + smallEnd[2]));
							if (currEBISBox.isRegular(gridIndex)) {
								RealVect a_point = EBArith::getIVLocation(gridIndex, fineDx, chomboGeometry->getDomainOrigin());
								featureFoundRegular = chomboGeometry->getFeature(a_point);
								break;
							}
						} // end for i
					} // end for j
#if CH_SPACEDIM==3
				} // end for k
#endif
				if (featureFoundRegular == NULL) {
					const EBGraph& currEBGraph = currEBISBox.getEBGraph();
					IntVectSet irregCells = currEBISBox.getIrregIVS(currBox);
					for (VoFIterator vofit(irregCells,currEBGraph); vofit.ok(); ++vofit) {
						const VolIndex& vof = vofit();

						const IntVect& gridIndex = vof.gridIndex();
						RealVect a_point = EBArith::getIVLocation(gridIndex, fineDx, chomboGeometry->getDomainOrigin());
						RealVect centroid = currEBISBox.centroid(vof);
						centroid *= fineDx;
						a_point += centroid;
						Feature* fi = chomboGeometry->getFeature(a_point);
						if (featureFoundIrregular == NULL) {
							featureFoundIrregular = fi;
						} else if (featureFoundIrregular != fi) {
							stringstream ss;
							ss << "Irregular point is in different feature than regular point in volume " << ivol << " in phase " << iphase << ". Mesh is too coarse!";
							throw ss.str();
						}
					}
				}
			} // end for DataIterator

			if (featureFoundRegular == NULL && featureFoundIrregular == NULL) {
				stringstream ss;
				ss << "Didn't find feature for volume " << ivol << " in phase " << iphase << ". Mesh is too coarse!";
				throw ss.str();
			}
			Feature* finalFeature = featureFoundRegular != NULL ? featureFoundRegular : featureFoundIrregular;
			ConnectedComponent* cc = new ConnectedComponent;
			cc->feature = finalFeature;
			cout << "Phase " << iphase << ", Volume " << ivol << ", Feature " << finalFeature->getName() << endl;
			cc->phase = iphase;
			cc->volumeIndexInPhase = ivol;
			cc->volume = volume;
			phaseVolumeList[iphase].push_back(cc);
			finalFeature->setPhase(iphase);
		} // end for comp
	}// end for phase

	// define grids for each level
  Vector<int> coarsestProcs;
	Vector<Box> coarsestBoxes;

	domainSplit(coarsestDomain, coarsestBoxes, chomboSpec->getMaxBoxSize(), blockFactor);
	mortonOrdering(coarsestBoxes);
	LoadBalance(coarsestProcs, coarsestBoxes);

	vectGrids.resize(numLevels);
	vectGrids[0] = DisjointBoxLayout(coarsestBoxes, coarsestProcs, coarsestDomain);

	vectEbis.resize(NUM_PHASES);
	for(int phase0 = 0; phase0 < NUM_PHASES; phase0 ++) {
    vectEbis[phase0].resize(phaseVolumeList[phase0].size());
		for (int ivol = 0; ivol < phaseVolumeList[phase0].size(); ivol ++) {
			vectEbis[phase0][ivol].resize(numLevels);
			phaseVolumeList[phase0][ivol]->volume->fillEBISLayout(vectEbis[phase0][ivol][0],
									  vectGrids[0],
									  coarsestDomain,
									  numGhostEBISLayout);
		}
  }

	if (numLevels > 1) {
		// If there is more than one level, the finer levels need to created by "BRMeshRefine"
	  BRMeshRefine meshRefine(coarsestDomain, vectRefRatios, chomboSpec->getFillRatio(), blockFactor, nestingRadius, chomboSpec->getMaxBoxSize());

	    //tag all irregular coarse iv's coarsest domain
		ProblemDomain secondFinestDomain = coarsestDomain;
		 // Compute the second finest domain
		for (int ilev = 0; ilev < numLevels - 2; ilev ++) {
			secondFinestDomain.refine(vectRefRatios[ilev]);
		}
		// Tags for creating the finer levels
		Vector<IntVectSet> tags(numLevels);

		if (refinementRoiExp == NULL) {
			pout() << "ChomboScheduler:: tag all irregular cells" << endl;
		} else {
			pout() << "ChomboScheduler:: tag user defined cells" << endl;
		}

		int taglevel = numLevels - 2;
		if (refinementRoiExp == NULL) {
			for (int phase0 = 0; phase0 < NUM_PHASES; phase0 ++) {
				for (int ivol = 0; ivol < phaseVolumeList[phase0].size(); ivol ++) {
					// Get the depth of the second to finest level
					int depth = phaseVolumeList[phase0][ivol]->volume->getLevel(secondFinestDomain);

					IntVectSet tagsVol = phaseVolumeList[phase0][ivol]->volume->irregCells(depth);
					tagsVol.grow(2);
					tags[taglevel] |= tagsVol;
				}
			}
		}
		else
		{
			// tag points in second finest level that satisfy ROI
			for (int phase0 = 0; phase0 < NUM_PHASES; phase0 ++)
			{
				for (int ivol = 0; ivol < phaseVolumeList[phase0].size(); ivol ++)
				{
					int depth = phaseVolumeList[phase0][ivol]->volume->getLevel(secondFinestDomain);

					IntVectSet tagsVol = phaseVolumeList[phase0][ivol]->volume->irregCells(depth);
					Vector<Box> boxes = tagsVol.boxes();
					for (int b = 0; b < boxes.size(); ++ b)
					{
						const Box& currBox = boxes[b];
						const int* loVect = currBox.loVect();
						const int* hiVect = currBox.hiVect();
#if CH_SPACEDIM == 3
						for (int k = loVect[2]; k <= hiVect[2]; ++ k)
						{
#endif
							for (int j = loVect[1]; j <= hiVect[1]; ++ j)
							{
								for (int i = loVect[0]; i <= hiVect[0]; ++ i)
								{
									IntVect gridIndex = IntVect(D_DECL(i, j, k));
									RealVect vol_point = EBArith::getIVLocation(gridIndex, vectDxes[taglevel], chomboGeometry->getDomainOrigin());
									if (refinementRoiExp->evaluateVector(vol_point.dataPtr()))
									{
										cout << "tagging point " << gridIndex << endl;
										tags[taglevel] |= gridIndex;
									}
								}
							}
#if CH_SPACEDIM == 3
						}
#endif
					} // boxes
				} // ivol
			} // iphase
		}

		if (tags[taglevel].isEmpty()) {
			MayDay::Error("No cells tagged for refinement");
		}

		Vector<Vector<Box> > oldBoxes(numLevels);
		Vector<Vector<Box> > newBoxes;
		// Need the boxes on the coarsest level and the tags on the second to
		// finest level to make all the boxes on all the levels
	 	oldBoxes[0]= coarsestBoxes;
		// Make all the boxes on all the levels
	 	meshRefine.regrid(newBoxes, tags, 0, numLevels - 1, oldBoxes);

	  ProblemDomain domainLev = coarsestDomain;
		for(int ilev = 1; ilev < numLevels; ilev++) {
			Vector<int> newProcs;
			domainLev.refine(vectRefRatios[ilev-1]);
			mortonOrdering(newBoxes[ilev]);
			LoadBalance(newProcs, newBoxes[ilev]);

			vectGrids[ilev] = DisjointBoxLayout(newBoxes[ilev], newProcs, domainLev);
		  //generate ebislayout
		  for(int phase0 = 0; phase0 < NUM_PHASES; phase0 ++) {
				for (int ivol = 0; ivol < phaseVolumeList[phase0].size(); ivol ++) {
					vectEbis[phase0][ivol].resize(numLevels);
					phaseVolumeList[phase0][ivol]->volume->fillEBISLayout(vectEbis[phase0][ivol][ilev],
											  vectGrids[ilev],
											  domainLev,
											  numGhostEBISLayout);
				}
		  }
		}
	}

	// compute membrane id = ivol * 1000 + j
	// allocate storage
	irregularPointMembraneIDs.resize(NUM_PHASES);
	for (int iphase = 0; iphase < NUM_PHASES; iphase ++) {
		int numVols = phaseVolumeList[iphase].size();
		irregularPointMembraneIDs[iphase].resize(numVols);
		for (int ivol = 0; ivol < numVols; ++ ivol)
		{
			irregularPointMembraneIDs[iphase][ivol].resize(numLevels);
			for (int ilev = 0; ilev < numLevels; ilev ++) {
				RefCountedPtr< LayoutData<IntVectSet> > irrSet = RefCountedPtr<LayoutData<IntVectSet> >(new LayoutData<IntVectSet>(vectGrids[ilev]));

				for(DataIterator dit = vectGrids[ilev].dataIterator(); dit.ok(); ++dit) {
					const EBISBox& currEBISBox = vectEbis[iphase][ivol][ilev][dit()];
					(*irrSet)[dit()] = currEBISBox.getIrregIVS(vectGrids[ilev][dit()]);
				} // end DataIterator
				BaseIVFactory<int>  bivfabFactory(vectEbis[iphase][ivol][ilev], *irrSet);
				irregularPointMembraneIDs[iphase][ivol][ilev] = RefCountedPtr<LevelData< BaseIVFAB<int> > >(new LevelData< BaseIVFAB<int> >(vectGrids[ilev], 1, IntVect::Zero, bivfabFactory));
			}
		}
	}

	// find membrane for each irregular point
	{
		// initialize all membrane IDs to -1 first
		for (int iphase = 0; iphase < NUM_PHASES; ++ iphase) {
			for (int ivol = 0; ivol < phaseVolumeList[iphase].size(); ivol ++) {
				for(int ilev = 0; ilev < numLevels; ++ ilev) {
					LevelData< BaseIVFAB<int> >& levelData = *irregularPointMembraneIDs[iphase][ivol][ilev];
					for (DataIterator dit = levelData.dataIterator(); dit.ok(); ++ dit) {
						DataIndex dataIndex = dit();
						BaseIVFAB<int>& ivfab = levelData[dataIndex];
						ivfab.setVal(-1);
					}
				}
			}
		}

		int phase0 = 0;
		int phase1 = 1;
		int totalVolumes = phaseVolumeList[phase0].size() + phaseVolumeList[phase1].size();
		bool* bAdjacent = new bool[phaseVolumeList[phase1].size()];
		numMembranePoints = 0;
		irregVolumeMembraneMap.resize(numLevels);
		for (int ivol = 0; ivol < phaseVolumeList[phase0].size(); ivol++) {
			memset(bAdjacent, 0, phaseVolumeList[phase1].size() * sizeof(bool));

			for (int ilev = 0; ilev < numLevels; ilev ++) {
				for(DataIterator dit = vectGrids[ilev].dataIterator(); dit.ok(); ++dit) {
					const Box& currBox = vectGrids[ilev][dit()];

					const EBISBox& currEBISBox = vectEbis[phase0][ivol][ilev][dit()];
					const EBGraph& currEBGraph = currEBISBox.getEBGraph();
					IntVectSet irregCells = currEBISBox.getIrregIVS(currBox);
					for (VoFIterator vofit(irregCells,currEBGraph); vofit.ok(); ++vofit) {
						const VolIndex& vof = vofit();
						const IntVect& gridIndex = vof.gridIndex();

						int membraneID = -1;
						for (int jvol = 0; jvol < phaseVolumeList[phase1].size(); jvol ++) {
							const IntVectSet& ivs = vectEbis[phase1][jvol][ilev][dit()].getIrregIVS(currBox);
							if (ivs.contains(gridIndex)) {
								membraneID = ivol * totalVolumes + jvol;
								(*irregularPointMembraneIDs[phase0][ivol][ilev])[dit()](vof, 0) = membraneID;
								(*irregularPointMembraneIDs[phase1][jvol][ilev])[dit()](vof, 0) = membraneID;
								bAdjacent[jvol] = true;
								break;
							}
						}
						if (membraneID == -1)
						{ // membrane not found
							Feature* feature = phaseVolumeList[phase0][ivol]->feature;
							RealVect vol_point = EBArith::getVofLocation(vof, vectDxes[ilev], chomboGeometry->getDomainOrigin());
							stringstream ss;
							ss << "phase " << phase0 << ":feature " << feature->getName() << ":volume " << ivol << ":level " << ilev
									<< ": no membrane id for point " << vof.gridIndex() << " at "  << vol_point << ".";
							cout << ss.str() << endl;
							(*irregularPointMembraneIDs[phase0][ivol][ilev])[dit()](vof, 0) = -1;
						}
						else
						{
							int volIndex = getChomboBoxLocalIndex(vectNxes[ilev], 0, gridIndex);
							if (isInNextFinerLevel(ilev, gridIndex))
							{
								irregVolumeMembraneMap[ilev][volIndex] = MEMBRANE_INDEX_IN_FINER_LEVEL;
							}
							else
							{
								map<int,int>::iterator iter = irregVolumeMembraneMap[ilev].find(volIndex);
								if (iter != irregVolumeMembraneMap[ilev].end())
								{
									Feature* feature = phaseVolumeList[phase0][ivol]->feature;
									RealVect vol_point = EBArith::getVofLocation(vof, vectDxes[ilev], chomboGeometry->getDomainOrigin());
									stringstream ss;
									ss << "phase " << phase0 << ":feature " << feature->getName() << ":volume " << ivol << ":level " << ilev 
											<< ", Point " << gridIndex << " at "  << vol_point << " is multi-valued point."
											<< "Mesh is too coarse to resolve. Use finer mesh or mesh refinement.";
									throw ss.str();
								}
								irregVolumeMembraneMap[ilev][volIndex] = numMembranePoints ++;
							}
						}
					}
				}
			}
			for (int jvol = 0; jvol < phaseVolumeList[phase1].size(); jvol ++) {
				if (bAdjacent[jvol]) {
					phaseVolumeList[phase0][ivol]->adjacentVolumes.push_back(phaseVolumeList[phase1][jvol]);
					phaseVolumeList[phase1][jvol]->adjacentVolumes.push_back(phaseVolumeList[phase0][ivol]);
				}
			}
		}
		delete[] bAdjacent;
	}

	// compute size of each connected component
	{
		int cfRefRatio = 1;
		for(int ilev = 0; ilev < numLevels - 1; ++ ilev)
		{
			cfRefRatio *= vectRefRatios[ilev];
		}
		double unitV = vectDxes[numLevels - 1].product();
		for (int iphase = 0; iphase < NUM_PHASES; ++ iphase)
		{
			for (int ivol = 0; ivol < phaseVolumeList[iphase].size(); ++ ivol)
			{
				double sumVolFrac = 0;
				int refratio = cfRefRatio;

				for(int ilev = 0; ilev < numLevels; ++ ilev)
				{
					int numRepeats = pow(refratio,SpaceDim);

					for(DataIterator dit = vectGrids[ilev].dataIterator(); dit.ok(); ++ dit)
					{
						const Box& currBox = vectGrids[ilev][dit()];
						const EBISBox& currEBISBox = vectEbis[iphase][ivol][ilev][dit()];

						const IntVect& bigEnd = currBox.bigEnd();
						const IntVect& smallEnd = currBox.smallEnd();

#if CH_SPACEDIM==3
						for (int k = smallEnd[2]; k <= bigEnd[2]; ++ k)
						{
#endif
							for (int j = smallEnd[1]; j <= bigEnd[1]; ++ j)
							{
								for (int i = smallEnd[0]; i <= bigEnd[0]; ++ i)
								{
									IntVect gridIndex(D_DECL(i, j, k));
									if (currEBISBox.isCovered(gridIndex) || isInNextFinerLevel(ilev, gridIndex))
									{
										continue;
									}

									double volFrac = 1.0;
									if (currEBISBox.isIrregular(gridIndex))
									{
										volFrac = currEBISBox.volFrac(VolIndex(gridIndex, 0));
									}
									volFrac *= numRepeats;
									sumVolFrac += volFrac;
								} // i
							} // j
#if CH_SPACEDIM==3
						} // k
#endif
						if (iphase == 0 && irregVolumeMembraneMap[ilev].size() > 0)
						{
							int totalVolumes = phaseVolumeList[0].size() + phaseVolumeList[1].size();
							Feature* iFeature = phaseVolumeList[iphase][ivol]->feature;
							const EBGraph& currEBGraph = currEBISBox.getEBGraph();
							IntVectSet irregCells = currEBISBox.getIrregIVS(currBox);
							Vector<ConnectedComponent*>& adjacentVolumes = phaseVolumeList[iphase][ivol]->adjacentVolumes;

							for (int j = 0; j < adjacentVolumes.size(); ++ j)
							{
								int jphase = adjacentVolumes[j]->phase;
								int jvol = adjacentVolumes[j]->volumeIndexInPhase;

								int currentMembraneID = ivol * totalVolumes + jvol;
								Feature* jFeature = phaseVolumeList[jphase][jvol]->feature;
								Membrane* membrane = SimTool::getInstance()->getModel()->getMembrane(iFeature, jFeature);
								for (VoFIterator vofit(irregCells,currEBGraph); vofit.ok(); ++vofit)
								{
									const VolIndex& vof = vofit();
									const IntVect& gridIndex = vof.gridIndex();
									int volIndex = getChomboBoxLocalIndex(vectNxes[ilev], 0, gridIndex);
									map<int,int>::iterator iter = irregVolumeMembraneMap[ilev].find(volIndex);
									if (iter == irregVolumeMembraneMap[ilev].end() || iter->second == MEMBRANE_INDEX_IN_FINER_LEVEL)
									{
										continue;
									}
									int membraneID = (*irregularPointMembraneIDs[iphase][ivol][ilev])[dit()](vof, 0);
									if (membraneID != currentMembraneID)
									{
										continue;
									}
									double areaFrac = currEBISBox.bndryArea(vof);
									membrane->addSize(areaFrac * unitV);
									membrane->addSizeFrac(areaFrac);
								}
							}
						}
					} // dit
					refratio /= vectRefRatios[ilev];
				} // level
				double s = sumVolFrac * unitV;
				phaseVolumeList[iphase][ivol]->size = s;
				phaseVolumeList[iphase][ivol]->feature->addSize(s);
				phaseVolumeList[iphase][ivol]->feature->addSizeFrac(sumVolFrac);
			} // vol
		} // phase
	} // compute size

	for(int phase0 = 0; phase0 < NUM_PHASES; phase0 ++) {
		for (int ivol = 0; ivol < phaseVolumeList[phase0].size(); ivol ++) {
			Feature* feature = phaseVolumeList[phase0][ivol]->feature;
			for(int ilev = 0; ilev < numLevels; ilev++) {
				long totalIrregLevel   = 0.0;
				long totalPtsLevel   = 0.0;

				for (DataIterator dit = vectGrids[ilev].dataIterator(); dit.ok(); ++dit) {
					const Box&  currBox      = vectGrids[ilev][dit()];

					const EBISBox& currEBISBox = vectEbis[phase0][ivol][ilev][dit()];
					IntVectSet irregCells = currEBISBox.getIrregIVS(currBox);
					pout() << "phase " << phase0 << ":feature " << feature->getName() << ":volume " << ivol << ":level " << ilev << ":" << " Box " << currBox << endl;
					totalPtsLevel += currBox.numPts();
					totalIrregLevel += irregCells.numPts();
				}

				std::ios::fmtflags origFlags = cout.flags();
				int origWidth = cout.width();
				int origPrecision = cout.precision();

				pout() << "phase " << phase0 << ":feature " << feature->getName() << ":volume " << ivol << ":level " << ilev << ":" << endl;
				pout() << setiosflags(ios::right);
				pout() << "  Total computation cells: " << setw(10) << totalPtsLevel << endl;
				pout() << "  Total irregular cells:   " << setw(10) << totalIrregLevel << endl;
				pout() << endl;

				pout().flags(origFlags);
				pout().width(origWidth);
				pout().precision(origPrecision);
			}
		}
  }
}

void ChomboScheduler::updateSolution() {
	cout << endl << "ChomboScheduler:: updateSolution" << endl;
	// reset variables
	int numVars = simulation->getNumVariables();
	for(int v = 0; v < numVars; ++ v){
		Variable* var = (Variable*)simulation->getVariable(v);
		var->reset();
	}

	int cfRefRatio = 1;
	for(int ilev = 0; ilev < numLevels - 1; ilev ++) {
		cfRefRatio *= vectRefRatios[ilev];
	}

	int finestLevel = numLevels - 1;
	double smallVolFrac = 1e-3;
	for (int iphase = 0; iphase < NUM_PHASES; iphase ++) {
		for (int ivol = 0; ivol < phaseVolumeList[iphase].size(); ivol ++) {
			Feature* feature = phaseVolumeList[iphase][ivol]->feature;
			int numDefinedVolVars = feature->getNumDefinedVariables();
			if (numDefinedVolVars == 0) {
				continue;
			}
			for (int ivar = 0; ivar < numDefinedVolVars; ivar ++) {
				Variable* var = feature->getDefinedVariable(ivar);
				double* varCurr = var->getCurr();
				double* errorCurr = 0;
				double* relErrCurr = 0;
				bool bComputeError = false;
				Variable* errorVar = var->getExactErrorVariable();
				if (errorVar != NULL)
				{
					bComputeError = true;
					errorCurr = errorVar->getCurr();
					relErrCurr = var->getRelativeErrorVariable()->getCurr();
				}

				int refratio = cfRefRatio;
				// copy phi to var, repeat values for coarse levels
				for(int ilev = 0; ilev < numLevels; ilev ++) {
					int numRepeats = pow(refratio,SpaceDim);
					for(DataIterator dit = vectGrids[ilev].dataIterator(); dit.ok(); ++dit)	{
						const EBISBox& currEBISBox = vectEbis[iphase][ivol][ilev][dit()];

						EBCellFAB& solnEBCellFAB = (*volSoln[iphase][ivol][ilev])[dit()];
						FArrayBox& solnFab = solnEBCellFAB.getFArrayBox();
						const IntVect& solnSize = solnFab.size();
						const int* solnLo = solnFab.loVect();
						Real* solnDataPtr = solnFab.dataPtr();

#if CH_SPACEDIM==3
						for (int k = numGhostSoln[2]; k < solnSize[2] - numGhostSoln[2]; k ++) {
							int koffset = k + solnLo[2];
#endif
							for (int j = numGhostSoln[1]; j < solnSize[1] - numGhostSoln[1]; j ++) {
								int joffset = j + solnLo[1];
								for (int i = numGhostSoln[0]; i < solnSize[0] - numGhostSoln[0]; i ++) {
									int ioffset = i + solnLo[0];

									IntVect gridIndex(D_DECL(ioffset, joffset, koffset));
									if (currEBISBox.isCovered(gridIndex) || isInNextFinerLevel(ilev, gridIndex)) {
										continue;
									}

									double volFrac = 1.0;
									if (currEBISBox.isIrregular(gridIndex))
									{
										volFrac = currEBISBox.volFrac(VolIndex(gridIndex, 0));
									}
									volFrac *= numRepeats;
									double sol = solnDataPtr[getChomboBoxLocalIndex(solnSize, ivar, D_DECL(i, j, k))];
									double mean = sol * volFrac;
									var->addMean(mean);
//									var->addVolFrac(volFrac);
									double error = 0;
									double relErr = 0;
									if (bComputeError)
									{
										RealVect coord = EBArith::getIVLocation(gridIndex, vectDxes[ilev], chomboGeometry->getDomainOrigin());
										memset(vectValues, 0, numSymbols * sizeof(double));
										vectValues[0] = simulation->getTime_sec();
										vectValues[1] = coord[0];
										vectValues[2] = coord[1];
										vectValues[3] = SpaceDim < 3 ? 0.5 : coord[2];
										double exact = var->getVarContext()->evaluateExpression(EXACT_EXP, vectValues);
										error = exact - sol;
										relErr = exact == 0 ? 0 : std::abs(error/exact);

										if (volFrac  > smallVolFrac)
										{
											var->updateMaxError(abs(error));
										}
										double l2 = error * error * volFrac;
										double exactl2 = exact * exact * volFrac;
										var->addL2Error(l2);
										var->addL2Exact(exactl2);
									}
#if CH_SPACEDIM==3
									for (int kk = 0; kk < refratio; kk ++) {
#endif
										for (int jj = 0; jj < refratio; jj ++) {
											for (int ii = 0; ii < refratio; ii ++) {
												IntVect finestGridIndex(D_DECL(ioffset * refratio + ii, joffset * refratio + jj, koffset * refratio + kk));
												int globalIndex = getChomboBoxLocalIndex(vectNxes[finestLevel], 0, finestGridIndex);
												varCurr[globalIndex] = sol;
												if (bComputeError)
												{
													errorCurr[globalIndex] = error;
													relErrCurr[globalIndex] = relErr;
												}
											} // end for ii
										} // end for jj
#if CH_SPACEDIM==3
									} // end for kk
#endif
								} // end for i
							} // end for j
#if CH_SPACEDIM==3
						} // end for k
#endif
					} // end for (DataIterator
					refratio /= vectRefRatios[ilev];
				} // end for ilev
			} // for (int ivar)
		} // for ivol
	} // for iphase

	for (int i = 0; i < simulation->getNumVolVariables(); ++ i)
	{
		Variable* var = simulation->getVolVariable(i);
		var->computeFinalMean();
		if (var->getExactErrorVariable() != NULL)
		{
			var->computeFinalL2Error();
		}
	}

	// membrane variables
	int totalVolumes = phaseVolumeList[0].size() + phaseVolumeList[1].size();
	int numMembraneVars = simulation->getNumMemVariables();

	int iphase = 0;
	int jphase = 1;
	for (int ivol = 0; ivol < phaseVolumeList[iphase].size(); ivol ++) {
		Feature* iFeature = phaseVolumeList[iphase][ivol]->feature;

		for (int ilev = 0; ilev < numLevels; ++ ilev)
		{
			if (irregVolumeMembraneMap[ilev].size() == 0)
			{
				continue;
			}
			DisjointBoxLayout& currGrids = vectGrids[ilev];

			for(DataIterator dit = currGrids.dataIterator(); dit.ok(); ++dit)	{
				const EBISBox& currEBISBox = vectEbis[iphase][ivol][ilev][dit()];
				const Box& currBox = vectGrids[ilev][dit()];

				const EBGraph& currEBGraph = currEBISBox.getEBGraph();
				IntVectSet irregCells = currEBISBox.getIrregIVS(currBox);

				for (VoFIterator vofit(irregCells,currEBGraph); vofit.ok(); ++vofit) {
					const VolIndex& vof = vofit();
					const IntVect& gridIndex = vof.gridIndex();
					int volIndex = getChomboBoxLocalIndex(vectNxes[ilev], 0, gridIndex);
					map<int,int>::iterator iter = irregVolumeMembraneMap[ilev].find(volIndex);
					if (iter == irregVolumeMembraneMap[ilev].end() || iter->second == MEMBRANE_INDEX_IN_FINER_LEVEL)
					{
						continue;
					}
					int memIndex = iter->second;
					int membraneID = (*irregularPointMembraneIDs[iphase][ivol][ilev])[dit()](vof, 0);
					if (membraneID < 0)
					{
						continue;
					}
					int jvol = membraneID % totalVolumes;

					double areaFrac = currEBISBox.bndryArea(vof);
					Feature* jFeature = phaseVolumeList[jphase][jvol]->feature;
					Membrane* membrane = SimTool::getInstance()->getModel()->getMembrane(iFeature, jFeature);
					for (int memVarIdx = 0; memVarIdx < numMembraneVars; ++ memVarIdx)
					{
						Variable* var = (Variable*)simulation->getMemVariable(memVarIdx);
						for (int ivar = 0; ivar < iFeature->getMemVarIndexesInAdjacentMembranes().size(); ++ ivar)
						{
							int varIndex =	iFeature->getMemVarIndexesInAdjacentMembranes()[ivar];
							if (varIndex == memVarIdx)
							{
								if (membrane->isVariableDefined(var))
								{
									//double* varCurr = var->getCurr();
									//double sol = (*memSoln[ivol][ilev])[dit()](vof, ivar);
									double sol = var->getCurr()[memIndex];
									double mean = sol * areaFrac;
//									var->addVolFrac(areaFrac);
									var->addMean(mean);

									//varCurr[memIndex] = sol;
									Variable* errorVar = var->getExactErrorVariable();
									if (errorVar != NULL)
									{
										RealVect vol_point = EBArith::getVofLocation(vof, vectDxes[ilev], chomboGeometry->getDomainOrigin());
										const RealVect& mem_centroid = currEBISBox.bndryCentroid(vof);
										RealVect coord = mem_centroid;
										coord *= vectDxes[ilev];
										coord += vol_point;
										memset(vectValues, 0, numSymbols * sizeof(double));
										vectValues[0] = simulation->getTime_sec();
										vectValues[1] = coord[0];
										vectValues[2] = coord[1];
										vectValues[3] = SpaceDim < 3 ? 0.5 : coord[2];
										double exact = var->getVarContext()->evaluateExpression(EXACT_EXP, vectValues);
										double* errorCurr = errorVar->getCurr();
										double error = sol - exact;
										errorCurr[memIndex] = error;
										Variable* relErrVar = var->getRelativeErrorVariable();
										double* relErrCurr = relErrVar->getCurr();
										relErrCurr[memIndex] = exact = 0 ? 0 : std::abs(error/exact);
										var->updateMaxError(abs(error));

										double l2 = error * error * areaFrac;
										double l2exact = exact * exact * areaFrac;
										var->addL2Error(l2);
										var->addL2Exact(l2exact);
									}
								}
								break;
							}
						}
					}
				} // for (VoFIterator vofit(irregCells,currEBGraph);
			} // end DataIter
		} // end ilev
	} // end ivol

	for (int i = 0; i < simulation->getNumMemVariables(); ++ i)
	{
		MembraneVariable* var = (MembraneVariable*)simulation->getMemVariable(i);
		var->computeFinalMean();
		if (var->getExactErrorVariable() != NULL)
		{
			// note: I assume this means sqrt(sum*finestDs)
			var->computeFinalL2Error();
		}
	}
}

void ChomboScheduler::writeData() {
	bool bWriteSim = true;
	if (bWriteSim) {
		updateSolution();
		DataSet::write(simulation);
	}
	static bool bFirstTime = false;
	bool bWriteHdf5 = false;
	// we need at least one hdf5 to show mesh in viewer.
	if (bWriteHdf5 || bFirstTime) {
		for (int iphase = 0; iphase < NUM_PHASES; iphase ++) {
			for (int ivol = 0; ivol < phaseVolumeList[iphase].size(); ivol ++) {
				Feature* feature = phaseVolumeList[iphase][ivol]->feature;
				if (feature->getNumDefinedVariables() == 0) {
					continue;
				}
				char hdf5FileName[128];
				sprintf(hdf5FileName, "%s%06d.feature_%s.vol%d%s", SimTool::getInstance()->getBaseFileName(), simulation->getCurrIteration(), feature->getName().c_str(), ivol, HDF5_FILE_EXT);
				Vector<string> names(feature->getNumDefinedVariables());
				for (int ivar = 0; ivar < feature->getNumDefinedVariables(); ivar ++) {
					Variable* var = feature->getDefinedVariable(ivar);
					char charstr[100];
					sprintf(charstr, "%s.vol%d",var->getName().c_str(), ivol);
					names[ivar] = string(charstr);
				}

				bool replaceCovered = false;
				Vector<Real> dummy;

				writeEBHDF5(string(hdf5FileName), vectGrids, volSoln[iphase][ivol], names,
					 vectDomains[0].domainBox(), vectDxes[0][0], simulation->getDT_sec(), simulation->getTime_sec(),
					 vectRefRatios, numLevels, replaceCovered, dummy);
				bFirstTime = false;
				if (!bWriteHdf5) {
					break;
				}
			}
			if (!bWriteHdf5 && !bFirstTime) {
				break;
			}
		}
		bFirstTime = false;
	}
	hdf5FileCount ++;
}

#define MESH_GROUP "/mesh"
#define MESH_ATTR_DIMENSION "dimension"
#define MESH_ATTR_ORIGIN "origin"
#define MESH_ATTR_NUM_LEVELS "numLevels"
#define MESH_ATTR_REFINE_RATIOS "refineRatios"
#define MESH_ATTR_VIEW_LEVEL "viewLevel"
#define MESH_ATTR_EXTENT "extent"
#define MESH_ATTR_NX "Nx"
#define MESH_ATTR_DX "Dx"
#define BOXES_DATASET MESH_GROUP"/boxes"
#define METRICS_DATASET MESH_GROUP"/metrics"
#define VERTICES_DATASET MESH_GROUP"/vertices"
#if CH_SPACEDIM == 2
#define SEGMENTS_DATASET MESH_GROUP"/segments"
#else
#define SURFACE_DATASET MESH_GROUP"/surface triangles"
#define SLICE_VIEW_DATASET MESH_GROUP"/slice view"
#endif

void ChomboScheduler::populateIntVectDataType(hid_t& intVectType)
{
	D_TERM(H5Tinsert(intVectType, "i", HOFFSET(IntVect, dataPtr()[0]), H5T_NATIVE_INT);,
				 H5Tinsert(intVectType, "j", HOFFSET(IntVect, dataPtr()[1]), H5T_NATIVE_INT);,
				 H5Tinsert(intVectType, "k", HOFFSET(IntVect, dataPtr()[2]), H5T_NATIVE_INT);)
}

void ChomboScheduler::populateRealVectDataType(hid_t& realVectType)
{
	D_TERM(H5Tinsert(realVectType, "x", HOFFSET(RealVect, dataPtr()[0]), H5T_NATIVE_DOUBLE);,
				 H5Tinsert(realVectType, "y", HOFFSET(RealVect, dataPtr()[1]), H5T_NATIVE_DOUBLE);,
				 H5Tinsert(realVectType, "z", HOFFSET(RealVect, dataPtr()[2]), H5T_NATIVE_DOUBLE);)
}

struct MeshBox
{
	IntVect lo, hi;
};

void ChomboScheduler::populateBoxDataType(hid_t& boxType)
{
	D_TERM(H5Tinsert(boxType, "lo_i", HOFFSET(MeshBox, lo[0]), H5T_NATIVE_INT);,
				 H5Tinsert(boxType, "lo_j", HOFFSET(MeshBox, lo[1]), H5T_NATIVE_INT);,
				 H5Tinsert(boxType, "lo_k", HOFFSET(MeshBox, lo[2]), H5T_NATIVE_INT);)
	D_TERM(H5Tinsert(boxType, "hi_i", HOFFSET(MeshBox, hi[0]), H5T_NATIVE_INT);,
				 H5Tinsert(boxType, "hi_j", HOFFSET(MeshBox, hi[1]), H5T_NATIVE_INT);,
				 H5Tinsert(boxType, "hi_k", HOFFSET(MeshBox, hi[2]), H5T_NATIVE_INT);)
}

struct MeshMetrics
{
	int index;
	int level;
	IntVect gridIndex;
	RealVect coord, normal;
	double volumeFraction,areaFraction;
};

void ChomboScheduler::populateMetricsDataType(hid_t& metricsType)
{
	H5Tinsert(metricsType, "index", HOFFSET(MeshMetrics, index), H5T_NATIVE_INT);
	H5Tinsert(metricsType, "level", HOFFSET(MeshMetrics, level), H5T_NATIVE_INT);
	D_TERM(H5Tinsert(metricsType, "i", HOFFSET(MeshMetrics, gridIndex[0]), H5T_NATIVE_INT);,
				 H5Tinsert(metricsType, "j", HOFFSET(MeshMetrics, gridIndex[1]), H5T_NATIVE_INT);,
				 H5Tinsert(metricsType, "k", HOFFSET(MeshMetrics, gridIndex[2]), H5T_NATIVE_INT);)
	D_TERM(H5Tinsert(metricsType, "x", HOFFSET(MeshMetrics, coord[0]), H5T_NATIVE_DOUBLE);,
				 H5Tinsert(metricsType, "y", HOFFSET(MeshMetrics, coord[1]), H5T_NATIVE_DOUBLE);,
				 H5Tinsert(metricsType, "z", HOFFSET(MeshMetrics, coord[2]), H5T_NATIVE_DOUBLE);)
	D_TERM(H5Tinsert(metricsType, "normalX", HOFFSET(MeshMetrics, normal[0]), H5T_NATIVE_DOUBLE);,
				 H5Tinsert(metricsType, "normalY", HOFFSET(MeshMetrics, normal[1]), H5T_NATIVE_DOUBLE);,
				 H5Tinsert(metricsType, "normalZ", HOFFSET(MeshMetrics, normal[2]), H5T_NATIVE_DOUBLE);)
	H5Tinsert(metricsType, "volumeFraction", HOFFSET(MeshMetrics, volumeFraction), H5T_NATIVE_DOUBLE);
	H5Tinsert(metricsType, "areaFraction", HOFFSET(MeshMetrics, areaFraction), H5T_NATIVE_DOUBLE);
}

struct Vertex
{
	RealVect coords;
};
void ChomboScheduler::populateVertexDataType(hid_t& vertexType)
{
	D_TERM(H5Tinsert(vertexType, "x", HOFFSET(Vertex, coords[0]), H5T_NATIVE_DOUBLE);,
				H5Tinsert(vertexType, "y", HOFFSET(Vertex, coords[1]), H5T_NATIVE_DOUBLE);,
				H5Tinsert(vertexType, "z", HOFFSET(Vertex, coords[2]), H5T_NATIVE_DOUBLE);)
}

#if CH_SPACEDIM == 2
struct Segment
{
	int index;
	int vertexIndexes[2];
	int neighborIndexes[2];
	Segment()
	{
		vertexIndexes[0] = vertexIndexes[1] = -1;
		neighborIndexes[0] = neighborIndexes[1] = MEMBRANE_NEIGHBOR_UNKNOWN;
	}
};
void ChomboScheduler::populateSegmentDataType(hid_t& segmentType)
{
	H5Tinsert(segmentType, "index", HOFFSET(Segment, index), H5T_NATIVE_INT);
	H5Tinsert(segmentType, "vertex_1", HOFFSET(Segment, vertexIndexes[0]), H5T_NATIVE_INT);
	H5Tinsert(segmentType, "vertex_2", HOFFSET(Segment, vertexIndexes[1]), H5T_NATIVE_INT);
	H5Tinsert(segmentType, "neighbor_1", HOFFSET(Segment, neighborIndexes[0]), H5T_NATIVE_INT);
	H5Tinsert(segmentType, "neighbor_2", HOFFSET(Segment, neighborIndexes[1]), H5T_NATIVE_INT);
}
#else
struct Triangle
{
	int index;
	int face;
	int neighborIndex;
	RealVect triVertices[3];
};

void ChomboScheduler::populateTriangleDataType(hid_t& triangleType)
{
	H5Tinsert(triangleType, "index", HOFFSET(Triangle, index), H5T_NATIVE_INT);
	H5Tinsert(triangleType, "face", HOFFSET(Triangle, face), H5T_NATIVE_INT);
	H5Tinsert(triangleType, "neighborIndex", HOFFSET(Triangle, neighborIndex), H5T_NATIVE_INT);
	D_TERM(H5Tinsert(triangleType, "x0", HOFFSET(Triangle, triVertices[0][0]), H5T_NATIVE_DOUBLE);,
				 H5Tinsert(triangleType, "y0", HOFFSET(Triangle, triVertices[0][1]), H5T_NATIVE_DOUBLE);,
				 H5Tinsert(triangleType, "z0", HOFFSET(Triangle, triVertices[0][2]), H5T_NATIVE_DOUBLE);)
	D_TERM(H5Tinsert(triangleType, "x1", HOFFSET(Triangle, triVertices[1][0]), H5T_NATIVE_DOUBLE);,
				 H5Tinsert(triangleType, "y1", HOFFSET(Triangle, triVertices[1][1]), H5T_NATIVE_DOUBLE);,
				 H5Tinsert(triangleType, "z1", HOFFSET(Triangle, triVertices[1][2]), H5T_NATIVE_DOUBLE);)
	D_TERM(H5Tinsert(triangleType, "x2", HOFFSET(Triangle, triVertices[2][0]), H5T_NATIVE_DOUBLE);,
				 H5Tinsert(triangleType, "y2", HOFFSET(Triangle, triVertices[2][1]), H5T_NATIVE_DOUBLE);,
				 H5Tinsert(triangleType, "z2", HOFFSET(Triangle, triVertices[2][2]), H5T_NATIVE_DOUBLE);)
}

struct SliceView
{
	int index;
	double crossPoints[3][4];
};

void ChomboScheduler::populateSliceViewDataType(hid_t& sliceViewType)
{
	H5Tinsert(sliceViewType, "index", HOFFSET(SliceView, index), H5T_NATIVE_INT);
	H5Tinsert(sliceViewType, "x_y1", HOFFSET(SliceView, crossPoints[0][0]), H5T_NATIVE_DOUBLE);
	H5Tinsert(sliceViewType, "x_z1", HOFFSET(SliceView, crossPoints[0][1]), H5T_NATIVE_DOUBLE);
	H5Tinsert(sliceViewType, "x_y2", HOFFSET(SliceView, crossPoints[0][2]), H5T_NATIVE_DOUBLE);
	H5Tinsert(sliceViewType, "x_z2", HOFFSET(SliceView, crossPoints[0][3]), H5T_NATIVE_DOUBLE);
	H5Tinsert(sliceViewType, "y_x1", HOFFSET(SliceView, crossPoints[1][0]), H5T_NATIVE_DOUBLE);
	H5Tinsert(sliceViewType, "y_z1", HOFFSET(SliceView, crossPoints[1][1]), H5T_NATIVE_DOUBLE);
	H5Tinsert(sliceViewType, "y_x2", HOFFSET(SliceView, crossPoints[1][2]), H5T_NATIVE_DOUBLE);
	H5Tinsert(sliceViewType, "y_z2", HOFFSET(SliceView, crossPoints[1][3]), H5T_NATIVE_DOUBLE);
	H5Tinsert(sliceViewType, "z_x1", HOFFSET(SliceView, crossPoints[2][0]), H5T_NATIVE_DOUBLE);
	H5Tinsert(sliceViewType, "z_y1", HOFFSET(SliceView, crossPoints[2][1]), H5T_NATIVE_DOUBLE);
	H5Tinsert(sliceViewType, "z_x2", HOFFSET(SliceView, crossPoints[2][2]), H5T_NATIVE_DOUBLE);
	H5Tinsert(sliceViewType, "z_y2", HOFFSET(SliceView, crossPoints[2][3]), H5T_NATIVE_DOUBLE);
}
#endif

#if CH_SPACEDIM == 2
int ChomboScheduler::findNeighborMembraneIndex2D(int iphase, int ilev, const IntVect& gridIndex,
				int iedge, const RealVect& cp, const RealVect& cpcoords, int& neighborEdge)
{
	bool bHasNeighbor = false;
	IntVect diff = IntVect::Zero;
	int idir = iedge / Side::NUMSIDES;
	int hilo = iedge % Side::NUMSIDES;

	bool bNextToWall = false;
	if (hilo == Side::Lo)
	{
		bNextToWall = gridIndex[idir] == 0;
		diff[idir] = -1;
	}
	else if (hilo == Side::Hi)
	{
		 bNextToWall = gridIndex[idir] == vectNxes[ilev][idir] - 1;
		 diff[idir] = +1;
	}

	static double cornerTol = 1.e-12;
	int nidx = MEMBRANE_NEIGHBOR_UNKNOWN;
	int otherDir = (idir + 1) % 2;
	if (bNextToWall)
	{
		nidx = MEMBRANE_NEIGHBOR_NEXT_TO_WALL;
	}
	else
	{
		bHasNeighbor = true;

		// cross point exactly on the corner
		if (abs(cp[otherDir]) >= (0.5-cornerTol))
		{
			double e = 0.25;
			RealVect nearByPointOffsets[4] =
			{
				RealVect(0, -e * cp[1] * vectDxes[ilev][1]),
				RealVect(0, e * cp[1] * vectDxes[ilev][1]),
				RealVect(-e * cp[0] * vectDxes[ilev][0], 0),
				RealVect(e * cp[0] * vectDxes[ilev][0], 0),
			};
			bool F[4];
			for (int i = 0; i < 4; ++ i)
			{
				F[i] = geoIfs[iphase]->value(cpcoords + nearByPointOffsets[i]) < 0;
			}
			if (F[0] == F[2])
			{
				throw "cross-point calculation: membrane is tangent to an element face. "
								"It may help if domain origin is shifted by a small number";
			}
			else if ((F[1] == F[2]) && (F[0] == F[3]))
			{
				// diagonal
				diff[idir]     = cp[idir] < 0 ? -1 : +1;
				diff[otherDir] = cp[otherDir] < 0 ? -1 : +1;
				// define a different neighborEdge here:
				if (cp[otherDir] > 0)
				{
					neighborEdge = 2 * otherDir;
				}
				else
				{
					neighborEdge = 2 * otherDir + 1;
				}
			}
			else if (F[1] != F[2])
			{
				// y direction
				diff[0] = 0;
				diff[1] = cp[1] < 0 ? -1 : +1;
				if (idir == 0)
				{
					neighborEdge = iedge;
				}
			}
			else if (F[0] != F[3])
			{
				// x direction
				diff[0] = cp[0] < 0 ? -1 : +1;
				diff[1] = 0;
				if (idir == 1)
				{
					neighborEdge = iedge;
				}
			}
			else
			{
				bHasNeighbor = false;
			}
		}
	}

	// check again if neighbor could be next to the wall
	if (bHasNeighbor)
	{
		for (int idir = 0; idir < SpaceDim; ++ idir)
		{
			if (gridIndex[idir] == 0 && diff[idir] == -1
						||	gridIndex[idir] == vectNxes[ilev][idir] - 1 && diff[idir] == +1 )
			{
				bHasNeighbor = false;
				nidx = MEMBRANE_NEIGHBOR_NEXT_TO_WALL;
				break;
			}
		}
	}
	if (bHasNeighbor)
	{
		IntVect neighborGridIndex = gridIndex + diff;
		int vi = getChomboBoxLocalIndex(vectNxes[ilev], 0, neighborGridIndex);
		map<int,int>::iterator iter = irregVolumeMembraneMap[ilev].find(vi);
		if (iter == irregVolumeMembraneMap[ilev].end())
		{
			stringstream ss;
			ss << "Volume element " << vi << " is not an irregular point";
			//cout << ss.str() << endl;
			throw ss.str();
		}
		else if (iter->second == MEMBRANE_INDEX_IN_FINER_LEVEL)
		{
			int rr = vectRefRatios[ilev];
			IntVect fineGridIndex = gridIndex * rr;
			if (diff[idir] != 0)
			{
				// most likely case
				if (hilo == Side::Hi)
				{
					fineGridIndex[idir] = fineGridIndex[idir] + rr;
				}
				else
				{
					fineGridIndex[idir] = fineGridIndex[idir] - 1;
				}
			}
			else
			{
				// unusual case when eb becomes tangent at corner and neighbor is in otherdir direction:
				if (hilo == Side::Hi)
				{
					fineGridIndex[idir] = fineGridIndex[idir] + rr - 1;
				}
				else
				{
					fineGridIndex[idir] = fineGridIndex[idir];
				}
      }
            //
			double cp_tmp = (cp[otherDir] + 0.5) * rr;
			if (diff[otherDir] == 0)
			{
				fineGridIndex[otherDir] = fineGridIndex[otherDir] + std::min<int>((rr - 1), int(cp_tmp));
			}
			else
			{
				// case when neighbor is diagonal
				if (diff[otherDir] == -1)
				{
					fineGridIndex[otherDir] = fineGridIndex[otherDir] - 1;
				}
				else if (diff[otherDir] == 1)
				{
					fineGridIndex[otherDir] = fineGridIndex[otherDir] + rr ;
				}
			}

			int nextLevel = ilev + 1;
			int fvi = getChomboBoxLocalIndex(vectNxes[nextLevel], 0, fineGridIndex);
			map<int, int>::iterator fiter = irregVolumeMembraneMap[nextLevel].find(fvi);
			if (fiter == irregVolumeMembraneMap[nextLevel].end())
			{
				stringstream ss;
				ss << " Volume element " << fvi << " in finer level is not irregular (testing neighbor)";
				if (cp_tmp - int(cp_tmp) > 0.5)
				{
					// try next element up (diagonal in refined level?)
					fineGridIndex[otherDir] = fineGridIndex[otherDir] + 1;
				}
				else
				{
					// try next element down (diagonal in refined level? )
					fineGridIndex[otherDir] = fineGridIndex[otherDir] - 1;
				}
				fvi = getChomboBoxLocalIndex(vectNxes[ilev+1], 0, fineGridIndex);
				map<int,int>::iterator fiter = irregVolumeMembraneMap[ilev+1].find(fvi);
				if (fiter == irregVolumeMembraneMap[ilev+1].end())
				{
					stringstream ss;
					ss << "warning: Volume element " << fvi << " in finer level is ALSO not irregular ?";
				}
				else
				{
					nidx = fiter->second;
				}
			}
			else
			{
				nidx = fiter->second;
			}
		}
		else
		{
			nidx = iter->second;
		}
	}
	return nidx;
}
#endif

void ChomboScheduler::writeMembraneFiles()
{
	char fileName[128];
#if (CH_SPACEDIM == 3)
	const RealVect& origin = getChomboGeometry()->getDomainOrigin();
	Real minOrigin = std::min<Real>(origin[0], origin[1]);
	minOrigin = std::min<Real>(minOrigin, origin[2]);
	Real sliceCrossPointDefaultValue = minOrigin - 1;
	SliceView* sliceViewData = new SliceView[numMembranePoints];
#endif

	int iphase = 0;
	MeshMetrics* metricsData = new MeshMetrics[numMembranePoints];
#if CH_SPACEDIM == 2
	Segment* segmentList = new Segment[numMembranePoints];
	int* edgeVertices = new int[numMembranePoints * 4];
	std::fill(edgeVertices, edgeVertices + numMembranePoints * 4, -1);

	double coeff = 0.1;
#else
	int triangleCount = 0;
	Triangle* surfaceData = new Triangle[numMembranePoints*6];
#endif
	int vertexCount = 0;
	Vertex* vertexList = new Vertex[numMembranePoints * 2];

	for (int ivol = 0; ivol < phaseVolumeList[iphase].size(); ++ ivol)
	{
		for (int ilev = 0; ilev < numLevels; ++ ilev)
		{
			if (irregVolumeMembraneMap[ilev].size() == 0)
			{
				continue;
			}
			ChomboGeometryShop chomboGeoShop(geoIfs[iphase], vectDxes[ilev]);
#if CH_SPACEDIM == 2
			RealVect edgePointOffset[4] =
			{
				RealVect(0, -vectDxes[ilev][1]*coeff),
				RealVect(0, vectDxes[ilev][1]*coeff),
				RealVect(vectDxes[ilev][0]*coeff, 0),
				RealVect(-vectDxes[ilev][0]*coeff, 0)
			};
#endif
			DisjointBoxLayout& currGrids = vectGrids[ilev];

			for(DataIterator dit = currGrids.dataIterator(); dit.ok(); ++ dit)	{
				const EBISBox& currEBISBox = vectEbis[iphase][ivol][ilev][dit()];
				const Box& currBox = vectGrids[ilev][dit()];

				const EBGraph& currEBGraph = currEBISBox.getEBGraph();
				IntVectSet irregCells = currEBISBox.getIrregIVS(currBox);
				for (VoFIterator vofit(irregCells,currEBGraph); vofit.ok(); ++ vofit)
				{
					const VolIndex& vof = vofit();
					const IntVect& gridIndex = vof.gridIndex();
					int volIndex = getChomboBoxLocalIndex(vectNxes[ilev], 0, gridIndex);
					map<int,int>::iterator iter = irregVolumeMembraneMap[ilev].find(volIndex);
					if (iter == irregVolumeMembraneMap[ilev].end() || iter->second == MEMBRANE_INDEX_IN_FINER_LEVEL)
					{
						continue;
					}
					int memIndex = iter->second;

					RealVect vol_point = EBArith::getVofLocation(vof, vectDxes[ilev], chomboGeometry->getDomainOrigin());
					const RealVect& mem_centroid = currEBISBox.bndryCentroid(vof);
					RealVect mem_point = mem_centroid;
					mem_point *= vectDxes[ilev];
					mem_point += vol_point;

					metricsData[memIndex].index = memIndex;
					metricsData[memIndex].level = ilev;
					metricsData[memIndex].gridIndex = gridIndex;
					metricsData[memIndex].coord = mem_point;
					metricsData[memIndex].normal = currEBISBox.normal(vof);
					metricsData[memIndex].areaFraction = currEBISBox.bndryArea(vof);
					metricsData[memIndex].volumeFraction = currEBISBox.volFrac(vof);

#if CH_SPACEDIM == 2
					segmentList[memIndex].index = memIndex;
					edgeMo edges[4];

					bool faceCovered;
					bool faceRegular;
					bool faceDontKnow;

					//get edgeType and intersection points
					chomboGeoShop.edgeData2D(edges,
										 faceCovered,
										 faceRegular,
										 faceDontKnow,
										 vectDxes[ilev][0],
										 vectDxes[ilev],
										 vof.gridIndex(),
										 vectDomains[ilev],
										 chomboGeometry->getDomainOrigin());
					int crossedEdgeCount = -1;
					int iedge = -1;
					for (int idir = 0; idir < SpaceDim; ++ idir)
					{
						for (int lohi = Side::Lo; lohi < Side::NUMSIDES;  ++ lohi)
						{
							++ iedge;
							if (edgeVertices[memIndex * 4 + iedge] >= 0)
							{
								++ crossedEdgeCount;
								continue;
							}
							bool irreg = edges[iedge].dontKnow();
							if (irreg)
							{
								++ crossedEdgeCount;
								if (crossedEdgeCount < 2)
								{
									RealVect cp = (edges[iedge].getIntersectLo()) ? edges[iedge].getLo() : edges[iedge].getHi();
									RealVect cross_point = cp;
									cross_point *= vectDxes[ilev];
									cross_point += vol_point;

									int neighborEdge = (iedge ^ 1);
									int nidx  = findNeighborMembraneIndex2D(iphase, ilev, gridIndex, iedge, cp, cross_point, neighborEdge);
									if (nidx == MEMBRANE_NEIGHBOR_UNKNOWN)
									{
	//									stringstream ss;
	//									ss << "failed to set a neighbor for membrane " << memIndex;
	//									throw ss.str();
										cout << "did not find one of the neighbors for membrane point (not next to wall) " << memIndex;
									}
									else
									{
										// determine if vertex is first or second
										// compute Q (as membrane enters the volume element from outside
										// find a point to the right of cross point, and compute fQ=implF(Q);
										RealVect Q = cross_point + edgePointOffset[iedge];
										Real funcQ = geoIfs[iphase]->value(Q);
										int orderV, orderN;
										if (funcQ < 0)
										{
											orderV = 0;
											orderN = 1;
										}
										else
										{
											orderV = 1;
											orderN = 0;
										 }

										// set neighbor vertex and neighbor
										// valid neighbor
										segmentList[memIndex].vertexIndexes[orderV] = vertexCount;
										segmentList[memIndex].neighborIndexes[orderV] = nidx;
										edgeVertices[memIndex * 4 + iedge] = vertexCount;
										if (nidx != MEMBRANE_NEIGHBOR_NEXT_TO_WALL)
										{
											segmentList[nidx].vertexIndexes[orderN] = vertexCount;
											segmentList[nidx].neighborIndexes[orderN] = memIndex;
											edgeVertices[nidx * 4 + neighborEdge] = vertexCount;
										}
										vertexList[vertexCount].coords = cross_point;
										++ vertexCount;
									}
								}
								else
								{
									stringstream ss;
									ss << "Point " << gridIndex << " has " << crossedEdgeCount << " cross edge points, is multi-valued point."
													<< "Mesh is too coarse to resolve. Use finer mesh or mesh refinement.";
									throw ss.str();
								}
							}
						}
					}
#else
					int faceCount = -1;
					int sliceCrossPointCount[SpaceDim];
					sliceViewData[memIndex].index = memIndex;
					for (int dir = 0; dir < SpaceDim; ++ dir)
					{
						sliceCrossPointCount[dir] = 0;
						for (int i = 0; i < 4; ++ i)
						{
							sliceViewData[memIndex].crossPoints[dir][i] = sliceCrossPointDefaultValue;
						}
					}
					for (int face = 0; face < SpaceDim; ++ face)
					{
						for (int hiLoFace = 0; hiLoFace < Side::NUMSIDES; ++ hiLoFace)
						{
							++ faceCount;
							edgeMo edges[4];
							bool faceCovered;
							bool faceRegular;
							bool faceDontKnow;

							chomboGeoShop.edgeData3D(edges,
												 faceCovered,
												 faceRegular,
												 faceDontKnow,
												 hiLoFace,
												 face,
												 vectDxes[ilev][0],
												 vectDxes[ilev],
												 vof.gridIndex(),
												 vectDomains[ilev],
												 chomboGeometry->getDomainOrigin());
							int crossedEdgeCount = 0;
							for (int iedge = 0; iedge < 4; ++ iedge)
							{
								bool irreg = edges[iedge].dontKnow();
								if (irreg)
								{
									RealVect cp = (edges[iedge].getIntersectLo()) ? edges[iedge].getLo() : edges[iedge].getHi();
									// get the real coordinate
									RealVect cross_point = cp;
									cross_point *= vectDxes[ilev];
									cross_point += vol_point;
									crossedEdgeCount ++;
									if (crossedEdgeCount < 3)
									{
										surfaceData[triangleCount].triVertices[crossedEdgeCount] = cross_point;
									}
									else
									{
										stringstream ss;
										ss << "Point " << gridIndex << " has " << crossedEdgeCount << " cross edge points, is multi-valued point."
														<< "Mesh is too coarse to resolve. Use finer mesh or mesh refinement.";
										throw ss.str();
									}
								}
							}
							surfaceData[triangleCount].index = memIndex;
							surfaceData[triangleCount].face = faceCount;
							surfaceData[triangleCount].triVertices[0] = mem_point;

							for(int dir = 0; dir < SpaceDim; ++ dir)
							{
								RealVect crossPoint;
								bool oneFaceCross = computeOneFaceCross(dir, face, hiLoFace == 0 ? -1 : 1, vectDxes[ilev],
												vol_point, surfaceData[triangleCount].triVertices[1],
												surfaceData[triangleCount].triVertices[2], crossPoint);
								if (oneFaceCross)
								{
									sliceViewData[memIndex].crossPoints[dir][sliceCrossPointCount[dir] * 2] = crossPoint[0];
									sliceViewData[memIndex].crossPoints[dir][sliceCrossPointCount[dir] * 2 + 1] = crossPoint[1];
									++ sliceCrossPointCount[dir];
								}
							}
							++ triangleCount;
						} // end for (int hiLoFace
					} // end for (int face
#endif
				} // end for vofit
			} // end for dit
		} // end for ilev
	} // end 	for ivol

	// find membrane neighbors for points having corner neighbors.
#if CH_SPACEDIM == 2
	delete[] edgeVertices;
#endif


	int finestLevel = numLevels - 1;
	// now start writing we have computed so far
	sprintf(fileName, "%s%s", SimTool::getInstance()->getBaseFileName(), MESH_HDF5_FILE_EXT);
	hid_t h5MeshFile = H5Fcreate(fileName, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
	hid_t meshGroup = H5Gcreate(h5MeshFile, MESH_GROUP, H5P_DEFAULT);

	// attribute
	{
	// dimension
	hid_t scalarDataSpace = H5Screate(H5S_SCALAR); // shared among all attributes

	hid_t attribute = H5Acreate(meshGroup, MESH_ATTR_DIMENSION, H5T_NATIVE_INT, scalarDataSpace, H5P_DEFAULT);
	int dimension = chomboGeometry->getDimension();
	H5Awrite(attribute, H5T_NATIVE_INT, &dimension);
	H5Aclose(attribute);

	// origin
	hid_t realVectType = H5Tcreate(H5T_COMPOUND, sizeof(RealVect));
	populateRealVectDataType(realVectType);
	attribute = H5Acreate(meshGroup, MESH_ATTR_ORIGIN, realVectType, scalarDataSpace, H5P_DEFAULT);
	H5Awrite(attribute, realVectType, chomboGeometry->getDomainOrigin().dataPtr());
	H5Aclose(attribute);

	// extent
	realVectType = H5Tcreate(H5T_COMPOUND, sizeof(RealVect));
	populateRealVectDataType(realVectType);
	attribute = H5Acreate(meshGroup, MESH_ATTR_EXTENT, realVectType, scalarDataSpace, H5P_DEFAULT);
	H5Awrite(attribute, realVectType, chomboGeometry->getDomainSize().dataPtr());
	H5Aclose(attribute);

	// mesh size
	hid_t intVectType = H5Tcreate(H5T_COMPOUND, sizeof(IntVect));
	populateIntVectDataType(intVectType);
	attribute = H5Acreate(meshGroup, MESH_ATTR_NX, intVectType, scalarDataSpace, H5P_DEFAULT);
	H5Awrite(attribute, intVectType, vectNxes[finestLevel].dataPtr());
	H5Aclose(attribute);

	// grid size
	attribute = H5Acreate(meshGroup, MESH_ATTR_DX, realVectType, scalarDataSpace, H5P_DEFAULT);
	H5Awrite(attribute, realVectType, vectDxes[finestLevel].dataPtr());
	H5Aclose(attribute);

	// number of levels
	attribute = H5Acreate(meshGroup, MESH_ATTR_NUM_LEVELS, H5T_NATIVE_INT, scalarDataSpace, H5P_DEFAULT);
	H5Awrite(attribute, H5T_NATIVE_INT, &numLevels);
	H5Aclose(attribute);

	// view level
	attribute = H5Acreate(meshGroup, MESH_ATTR_VIEW_LEVEL, H5T_NATIVE_INT, scalarDataSpace, H5P_DEFAULT);
	H5Awrite(attribute, H5T_NATIVE_INT, &finestLevel);
	H5Aclose(attribute);
	H5Sclose(scalarDataSpace);

	// refine ratios
	int* ratios = new int[vectRefRatios.size()];
	for (int i = 0; i < vectRefRatios.size(); ++ i)
	{
		ratios[i] = vectRefRatios[i];
	}
	int rank = 1;
	hsize_t dim[1] = {vectRefRatios.size()};
	hid_t intArrayDataSpace = H5Screate_simple(rank, dim, NULL); // shared among all attributes
	attribute = H5Acreate(meshGroup, MESH_ATTR_REFINE_RATIOS, H5T_NATIVE_INT, intArrayDataSpace, H5P_DEFAULT);
	H5Awrite(attribute, H5T_NATIVE_INT, ratios);
	H5Aclose(attribute);
	H5Sclose(intArrayDataSpace);
	delete[] ratios;
	}

	// boxes
	{
	hid_t boxType = H5Tcreate(H5T_COMPOUND, sizeof(MeshBox));
	populateBoxDataType(boxType);
	Vector<Box> vectBoxes = vectGrids[finestLevel].boxArray();
	int numBoxes = vectBoxes.size();
	MeshBox* boxData = new MeshBox[numBoxes];
	for (int i = 0; i < numBoxes; ++ i)
	{
		boxData[i].lo = vectBoxes[i].smallEnd();
		boxData[i].hi = vectBoxes[i].bigEnd();
	}
	hsize_t dim[] = {numBoxes};   /* Dataspace dimensions */
	int rank = 1;
	hid_t space = H5Screate_simple (rank, dim, NULL);
	hid_t boxDataSet = H5Dcreate (h5MeshFile, BOXES_DATASET, boxType, space, H5P_DEFAULT);
	H5Dwrite(boxDataSet, boxType, H5S_ALL, H5S_ALL, H5P_DEFAULT, boxData);
	H5Dclose(boxDataSet);
	H5Sclose(space);
	H5Tclose(boxType);
	delete[] boxData;
	}

	// metrics
	{
	hid_t metricsType = H5Tcreate(H5T_COMPOUND, sizeof(MeshMetrics));
	populateMetricsDataType(metricsType);
	hsize_t dim[] = {numMembranePoints};   /* Dataspace dimensions */
	int rank = 1;
	hid_t space = H5Screate_simple (rank, dim, NULL);
	hid_t metricsDataset = H5Dcreate (h5MeshFile, METRICS_DATASET, metricsType, space, H5P_DEFAULT);
	H5Dwrite(metricsDataset, metricsType, H5S_ALL, H5S_ALL, H5P_DEFAULT, metricsData);
	H5Dclose(metricsDataset);
	H5Sclose(space);
	H5Tclose(metricsType);
	delete[] metricsData;
	}

	// vertices
	{
	hid_t vertexType = H5Tcreate(H5T_COMPOUND, sizeof(Vertex));
	populateVertexDataType(vertexType);
	hsize_t dim[] = {vertexCount};   /* Dataspace dimensions */
	int rank = 1;
	hid_t space = H5Screate_simple(rank, dim, NULL);
	hid_t verticesDataset = H5Dcreate (h5MeshFile, VERTICES_DATASET, vertexType, space, H5P_DEFAULT);
	H5Dwrite(verticesDataset, vertexType, H5S_ALL, H5S_ALL, H5P_DEFAULT, vertexList);
	H5Dclose(verticesDataset);
	H5Sclose(space);
	H5Tclose(vertexType);
	delete[] vertexList;
	}

#if CH_SPACEDIM == 2
	// segments
	{
	hid_t segmentType = H5Tcreate(H5T_COMPOUND, sizeof(Segment));
	populateSegmentDataType(segmentType);
	hsize_t dim[] = {numMembranePoints};   /* Dataspace dimensions */
	int rank = 1;
	hid_t space = H5Screate_simple(rank, dim, NULL);
	hid_t segmentsDataset = H5Dcreate (h5MeshFile, SEGMENTS_DATASET, segmentType, space, H5P_DEFAULT);
	H5Dwrite(segmentsDataset, segmentType, H5S_ALL, H5S_ALL, H5P_DEFAULT, segmentList);
	H5Dclose(segmentsDataset);
	H5Sclose(space);
	H5Tclose(segmentType);
	delete[] segmentList;
	}

#else
	// cross points
	{
	hid_t triangleType = H5Tcreate(H5T_COMPOUND, sizeof(Triangle));
	populateTriangleDataType(triangleType);
	hsize_t dim[] = {triangleCount};   /* Dataspace dimensions */
	int rank = 1;
	hid_t space = H5Screate_simple(rank, dim, NULL);
	hid_t surfaceDataset = H5Dcreate (h5MeshFile, SURFACE_DATASET, triangleType, space, H5P_DEFAULT);
	H5Dwrite(surfaceDataset, triangleType, H5S_ALL, H5S_ALL, H5P_DEFAULT, surfaceData);
	H5Dclose(surfaceDataset);
	H5Sclose(space);
	H5Tclose(triangleType);
	delete[] surfaceData;
	}

	// slice view
	{
	hid_t sliceViewType = H5Tcreate(H5T_COMPOUND, sizeof(SliceView));
	populateSliceViewDataType(sliceViewType);
	hsize_t dim[] = {numMembranePoints};   /* Dataspace dimensions */
	int rank = 1;
	hid_t space = H5Screate_simple(rank, dim, NULL);
	hid_t sliceViewDataset = H5Dcreate(h5MeshFile, SLICE_VIEW_DATASET, sliceViewType, space, H5P_DEFAULT);
	H5Dwrite(sliceViewDataset, sliceViewType, H5S_ALL, H5S_ALL, H5P_DEFAULT, sliceViewData);
	H5Dclose(sliceViewDataset);
	H5Sclose(space);
	delete[] sliceViewData;
	}
#endif

	H5Gclose(meshGroup);
	H5Fclose(h5MeshFile);
}

bool ChomboScheduler::computeOneFaceCross(int dir, int face, int hiLoFace, RealVect& H, RealVect& v0,
				RealVect& v1, RealVect& v2, RealVect& crossPoint)
{
	if (dir == face)
	{
		return false;
	}
	static int slicedirs[3][2] = {{1, 2}, {0, 2}, {0, 1}};

	int jlohi, jcross;
	int planedir0, planedir1;
	planedir0 = dir;
	if (face == slicedirs[dir][0])    // y- or y+
	{
    planedir1 = slicedirs[dir][1];   // not y direction,
    jlohi = 0;
		jcross = 1;
	}
	else                         // z- or z+
	{
    planedir1 = slicedirs[dir][0];   // not z direction,
    jlohi = 1;
		jcross = 0;
	}

	RealVect DP = v2 - v1;
	Real thisDP0 = DP[planedir0];
	Real thisDP1 = DP[planedir1];

	if (abs(thisDP0) > 1.e-8 * abs(thisDP1))
	{
    // if slope too large, return a number out of range
    Real xc = v0[planedir0];
		Real yc = v0[planedir1];
    Real p10 = v1[planedir0];
    Real p11 = v1[planedir1];
    Real a = thisDP1 / thisDP0;
    Real value = a * (xc - p10) + p11;
    if (abs(value - yc) <= H[planedir1]/2)
		{
			crossPoint[jlohi]  = v0[face] + hiLoFace * H[face]/2 ;
			crossPoint[jcross] = value;
			return true;
    }
	}
	return false;
}

int ChomboScheduler::findLevel(const ProblemDomain& domain)
{
	if (numLevels == 1)
	{
		return 0;
	}
	for (int i = 0; i < numLevels; ++ i)
	{
		if (vectDomains[i].domainBox() == domain.domainBox())
		{
			return i;
		}
	}
	return -1;
}